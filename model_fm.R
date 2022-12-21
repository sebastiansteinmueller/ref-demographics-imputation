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

### read dataset
load("data/dem_refvda_end2021.RData")


##### II. Define model formula #####

# f.m.fm <- bf(formula =  female | trials(total) ~ (1|origin_iso3/asylum_region/asylum_iso3) + # varying intercept
#                                         logGniRatio + logDistance + neighbor + # population level covariates
#                                         (0+logGniRatio + logDistance + neighbor|origin_iso3), # varying slopes by origin
#              center = T # intercept centered at mean of population level covariates
#             )
f.m.fm <- bf(formula =  female | trials(total) ~ (1|origin_iso3/asylum_iso3) + # varying intercept
               logGniRatio + logDistance + neighbor + # population level covariates
               (0+logGniRatio + logDistance + neighbor|origin_iso3), # varying slopes by origin
             center = T # intercept centered at mean of population level covariates
)


##### III. Define prior distributions ##### 

priors.m.fm.empty <- get_prior(f.m.fm,
          family = binomial(link = "logit"),
          data = dem_longMissing %>% 
          filter(missing %in% c("none", "age")))

# priors.m.fm <- c(
#   prior(normal(0,10), class = Intercept),
#   prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
#   prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_region:asylum_iso3"),
#   prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_region"), # more pooling on region and country of asylum
#   prior(student_t(5,0,2.5), class = sd, group = "origin_iso3") # less pooling, i.e. closer to separate model for each origin
#   )
priors.m.fm <- c(
  prior(normal(0,10), class = Intercept),
  prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_iso3"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3") # less pooling, i.e. closer to separate model for each origin
)


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

plot(m.fm)
p.mf.mcmcacf <- mcmc_acf(m.fm,pars = variables(m.fm)[c(1,2,3,4)])
m.fm.loo <- loo(m.fm)
plot(m.fm.loo)

############################################ END ###########################################################