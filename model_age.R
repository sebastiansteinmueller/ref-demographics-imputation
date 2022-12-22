############################################ START ###########################################################

############################################ model_fm.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2021
#### Description: Binomial model for female/male counts

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

dem_longMissing$femaleAge  <- with(dem_longMissing, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60))
dem_longMissing$maleAge  <- with(dem_longMissing, cbind(male_0_4, male_5_11, male_12_17, male_18_59, male_60))


##### II. Define model formulas #####

f.m.femaleAge <- bf(formula =  femaleAge | trials(female) ~ (1|origin_iso3/asylum_iso3) + # varying intercept
               logGniRatio + logDistance + neighbor + # population level covariates
               (0+logGniRatio + logDistance + neighbor|origin_iso3), # varying slopes by origin
               family = multinomial(link = "logit", refcat = "female_18_59"),
             center = T # intercept centered at mean of population level covariates
)

f.m.maleAge <- bf(formula =  maleAge | trials(male) ~ (1|origin_iso3/asylum_iso3) + # varying intercept
                      logGniRatio + logDistance + neighbor + # population level covariates
                      (0+logGniRatio + logDistance + neighbor|origin_iso3), # varying slopes by origin
                  family = multinomial(link = "logit", refcat = "male_18_59"),
                    center = T # intercept centered at mean of population level covariates
)

f.m.age <- f.m.femaleAge + f.m.maleAge + set_rescor(FALSE)

##### III. Define prior distributions ##### 

priors.m.age.empty <- get_prior(f.m.age,
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

priors.m.age <- c(
  prior(normal(log(wpp_female[1]/wpp_female[4]),1), class = Intercept, dpar = "mufemale04"),
  prior(normal(log(wpp_female[2]/wpp_female[4]),1), class = Intercept, dpar = "mufemale511"),
  prior(normal(log(wpp_female[3]/wpp_female[4]),1), class = Intercept, dpar = "mufemale1217"),
  prior(normal(log(wpp_female[5]/wpp_female[4]),1), class = Intercept, dpar = "mufemale60"),
  prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_iso3"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3") # less pooling, i.e. closer to separate model for each origin
)


##### IV. Fit and save model ##### 

m.fm <- brm(formula = f.m.fm,
                    family = binomial(link = "logit"),
            data = dem_longMissing %>% filter(missing %in% c("none", "age")),
            prior = priors.m.fm,
            init = 0,
            sample_prior = "yes",
            iter = 3000,
            cores = 4,
            control=list(adapt_delta=0.9, 
                          max_treedepth=12),
            seed = 938
            )
saveRDS(m.fm, file =  paste0("models/m.fm_", str_remove_all(as.character(Sys.Date()), "-"),".rds"))


##### V. Model diagnostics ##### 

plot(m.fm)
p.m.mf.mcmcacf <- mcmc_acf(m.fm,pars = variables(m.fm)[c(1,2,3,4)])
m.fm.loo <- loo(m.fm)
plot(m.fm.loo)


############################################ END ###########################################################