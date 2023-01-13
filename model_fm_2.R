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


##### II. Define model formula #####

f.m.fm <- bf(formula =  female | trials(total) ~ (1|origin_iso3/asylum_sdgregion/asylum_iso3) + # varying intercept
               neighbor, # neighbouring country fixed effect
             center = T # intercept centered at mean of population level covariates
)


##### III. Define prior distributions ##### 

priors.m.fm.empty <- get_prior(f.m.fm,
          family = binomial(link = "logit"),
          data = dem_longMissing %>% 
          filter(missing %in% c("none", "age")))


priors.m.fm <- c(
  prior(normal(0,1), class = Intercept),
  prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3"), # restrict within-region country variation
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3") # less pooling, i.e. closer to separate model for each origin
)


##### IV. Fit and save model ##### 

m.fm <- brm(formula = f.m.fm,
                    family = binomial(link = "logit"),
            data = dem_longMissing %>% filter(missing %in% c("none", "age")),
            prior = priors.m.fm,
            init = 0,
            sample_prior = "yes",
            iter = 2000,
            chains = 3,
            cores = 3,
            control=list(adapt_delta=0.8, 
                          max_treedepth=10),
            seed = 1742
            )
saveRDS(m.fm, file =  paste0("models/m.fm2_", str_remove_all(as.character(Sys.Date()), "-"),".rds"))


##### V. Model diagnostics ##### 

# plot(m.fm)
# p.m.mf.mcmcacf <- mcmc_acf(m.fm,pars = variables(m.fm)[c(1,2,3,4)])
# m.fm.loo <- loo(m.fm)
# plot(m.fm.loo)


############################################ END ###########################################################