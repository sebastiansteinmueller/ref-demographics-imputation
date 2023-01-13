############################################ START ###########################################################

############################################ model_age.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2021
#### Description: Multinomial models for sex-specific age counts

##### I. Read data, packages etc ##### 

### packages
library(tidyverse)
library(stringi)
library(brms)
library(tidybayes)
library(bayesplot)
library(piggyback)

### read dataset
load("data/dem_refvda_end2021.RData")

dem_longMissing$age  <- with(dem_longMissing, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60, 
                                                    male_0_4, male_5_11, male_12_17, male_18_59, male_60))


##### II. Define model formulas #####

f.m.ageonly <- bf(formula =  age | trials(total) ~ (1|origin_iso3/asylum_sdgregion/asylum_iso3)  + # varying intercept
                      neighbor,
               family = multinomial(link = "logit", refcat = "female_18_59"),
             center = T # intercept centered at mean of population level covariates
)

##### III. Define prior distributions ##### 

priors.m.age.empty <- get_prior(f.m.ageonly,
          data = dem_longMissing %>% 
          filter(missing %in% c("none")))


# WPP 2021 world frequencies for centres of population-level age group intercepts (0-4, 5-11, 12-17, 18-59, 60+ sex-specific distributions)
wpp_female <- c(0.082948798,
                0.117108803,
                0.094642446,
                0.555118782, # 18-59, reference cat
                0.15018117)
wpp_male <- c(0.086824115,
              0.123420862,
              0.09993278,
              0.56594869, # 18-59, reference cat
              0.123873553)

stanvars.age <- stanvar(wpp_female, name = "wpp_female") + stanvar(wpp_male, name = "wpp_male")


priors.m.ageonly <- c(
  prior(normal(0,1), class = Intercept),
  prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
  prior(normal(log(0.0412436670768394/0.276015261672875),1), class = Intercept, dpar = "mufemale04"),
  prior(normal(log(0.0582286495082683/0.276015261672875),1), class = Intercept, dpar = "mufemale511"),
  prior(normal(log(0.0470579635271494/0.276015261672875),1), class = Intercept, dpar = "mufemale1217"),
  prior(normal(log(0.0746728381151677/0.276015261672875),1), class = Intercept, dpar = "mufemale60"),
  prior(normal(log(0.0436535691629835/0.276015261672875),1), class = Intercept, dpar = "mumale04"),
  prior(normal(log(0.0620537410289167/0.276015261672875),1), class = Intercept, dpar = "mumale511"),
  prior(normal(log(0.050244364889838/0.276015261672875),1), class = Intercept, dpar = "mumale1217"),
  prior(normal(log(0.284548599385259/0.276015261672875),1), class = Intercept, dpar = "mumale1859"),
  prior(normal(log(0.0622813456327033/0.276015261672875),1), class = Intercept, dpar = "mumale60"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale04"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale04"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale04"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale511"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale511"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale511"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale1217"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale1217"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale1217"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale60"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale60"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale60"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale04"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale04"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale04"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale511"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale511"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale511"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale1217"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale1217"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale1217"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale1859"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale1859"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale1859"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale60"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale60"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale60")
)


##### IV. Fit and save model ##### 

m.ageonly <- brm(formula = f.m.ageonly,
            data = dem_longMissing %>% filter(missing %in% c("none")),
            prior = priors.m.ageonly,
         #   stanvars = stanvars.age,
            init = 0,
            sample_prior = "yes",
            iter = 2000,
            chains = 4,
            cores = 4,
            control=list(adapt_delta=0.8, 
                          max_treedepth=10),
            seed = 1805
            )
saveRDS(m.ageonly, file =  paste0("models/m.ageonly_", str_remove_all(as.character(Sys.Date()), "-"),".rds"))


##### V. Model diagnostics ##### 

# plot(m.age)
# p.m.age.mcmcacf <- mcmc_acf(m.age,pars = variables(m.age)[c(1:8)]) # acf of intercept mcmcs
# m.age.loo <- loo(m.age)
# plot(m.age.loo)


############################################ END ###########################################################