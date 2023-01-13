############################################ START ###########################################################

############################################ postpred.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2021
#### Description: Draw from the posterior predictive distributions of age and sex models and create imputed datasets

##### I. Read fit objects, data, packages etc ##### 

### packages
library(tidyverse)
library(stringi)
library(brms)
library(tidybayes)
library(bayesplot)
library(tictoc)
library(data.table)

### read original dataset
load("data/dem_refvda_end2021.RData")

### read brms fit objects
m.ageonly <- readRDS("models/m.ageonly.rds")



##### II. Draw from posterior predictive distributions of m.fm and m.age ##### 

### posterior predictions for female and male counts
tic()
m.pred <- dem_longMissing %>%
  filter(missing %in% c("sexAge", "age")) %>% 
  add_predicted_draws(m.ageonly,
                      allow_new_levels = T,
                      sample_new_levels = "uncertainty",
                      ndraws = 4000,
                      seed = 2016)
toc()


##### III. Pivot posterior draws into imputation dataset in the same format as on UNHCR's refugee data finder ##### 

### pivot draws to wide format (age/sex categories as columns instead of rows) ### 
dim(m.pred)
m.pred.wide <- m.pred %>%
  ungroup() %>%
  select(-c(children, female_0_4:female_60, female, male, male_0_4:male_60, children:femaleProp, `.chain`:`.iteration`)) %>%
  pivot_wider(values_from = .prediction, 
              names_from = .category, 
              id_cols = c(.draw, 
                          year:missing, 
                          total, asylum_sdgregion:logDistance)
  ) %>% 
  rename(imputation = .draw) %>% 
  mutate(
    female = rowSums(select(., female_0_4:female_60)),
    male = rowSums(select(., male_0_4:male_60)),
    children = rowSums(select(., female_0_4:female_12_17, male_0_4:male_12_17)),
    adults = rowSums(select(., female_18_59, female_60, male_18_59, male_60))
  )

### merge type i) data with full age/sex observations to posterior draws ### 
dim(m.pred.wide)
dim(dem_longMissing %>% filter(missing %in% c("none")))
dim(m.pred.wide)[1]+dim(dem_longMissing %>% filter(missing %in% c("none")))[1]*length(unique(m.pred.wide$imputation))

imputations_longMissing <- m.pred.wide %>%
  group_by(imputation) %>% 
  group_modify(
    ~bind_rows(.x, dem_longMissing %>% # add full type 1 dataset to each draw
                 filter(missing %in% c("none"))
    ) 
  )

dim(imputations_longMissing) # OK


# check row sums

imputations_longMissing <- imputations_longMissing %>% 
  ungroup() %>%
  mutate(sexSum = rowSums(select(., female, male)),
         ageSum = rowSums(select(., female_0_4:male_60)),
         ageFemaleSum = rowSums(select(., female_0_4:female_60)),
         ageMaleSum = rowSums(select(., male_0_4:male_60))
  ) %>%
  mutate(
    totalSexDiff = total-sexSum,
    totalAgeDiff = total-ageSum,
    femaleDiff = female-ageFemaleSum,
    maleDiff = male-ageMaleSum
  )

t.imp.checkSums <- imputations_longMissing %>% 
  ungroup() %>%
  summarise(across(totalSexDiff:maleDiff,
                   list(mean=mean, sd=sd))) # OK, all 0, no NAs


# check variance over draws

t.imp.checkVar <- imputations_longMissing %>% 
  group_by(origin_iso3, asylum_iso3, popType, missing) %>% 
  summarise(across(c(total, female, male, female_0_4:male_60),
                   list(mean = mean, sd = sd))) %>% 
  arrange(missing)

t.imp.checkVarSummary <- t.imp.checkVar %>% 
  ungroup() %>% 
  group_by(missing) %>% 
  summarise(across(contains("_sd"), 
                   list(sum = sum))) # OK (varies in the correct columns by missing status)


### sum up dissolving missing variable to create refugee data finder structure ### 

imputations <- imputations_longMissing %>%
  ungroup() %>%
  group_by(imputation, year, asylum_sdgregion, origin_iso3, origin_country,
           asylum_iso3, asylum_country, popType) %>%
  summarise(across(c(female_0_4:female_60, female, 
                     male_0_4:male_60, male, total),
                   sum
  )
  )

### merge regions to imputation dataset 

## create m49hcr versions for origin and asylum country

m49hcr_asylum <- m49hcr %>%
  select(-country) %>% 
  rename_with( ~ paste0("asylum_", .))

m49hcr_origin <- m49hcr %>%
  select(-country) %>% 
  rename_with( ~ paste0("origin_", .))


## merge with demographics dataset

imputations <- imputations %>% 
  left_join(m49hcr_asylum, by = "asylum_iso3") %>% 
  left_join(m49hcr_origin, by = "origin_iso3") %>% 
  ungroup()
dim(imputations) # OK

# check whether NAs in asylum/origin variables

table(imputations$asylum_country, useNA = "ifany")
table(imputations$asylum_region, useNA = "ifany")
table(imputations$asylum_sdgregion, useNA = "ifany")
table(imputations$origin_country, useNA = "ifany")
table(imputations$origin_region, useNA = "ifany")
# all OK


##### IV. Write files ##### 

saveRDS(imputations, file =  paste0("output/imputations", length(unique(imputations$imputation)), "_", str_remove_all(as.character(Sys.Date()), "-"),".rds"))


############################################ END ###########################################################