############################################ START ###########################################################

############################################ preparedata.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2021
#### Description: Data preparation of ASR 2021 REF + VDA demographic data for imputation model


rm(list=ls()) # clear workspace


##### I. Read data, packages etc ##### 

### packages
library(tidyverse)
library(stringi)
library(readxl)
library(writexl)


### data from UNHCR refugee data finder public database
url <- paste( 'https://api.unhcr.org/population/v1/demographics/?limit=20&dataset=population&displayType=demographics&columns%5B%5D=refugees&columns%5B%5D=vda&yearFrom=2021&yearTo=2021&coo_all=true&coa_all=true&download=true')
destfile = "data/query_data_end2021.zip"

if (!file.exists("data/demographics.csv")) {
  download.file(url ,destfile, method = "curl", quiet = FALSE)
  unzip(destfile, "demographics.csv", exdir = "data")
  }

dem <- read_csv("data/demographics.csv", skip = 14) # end-2021 demographic table from https://www.unhcr.org/refugee-statistics/download/
m49 <- read_excel("data/UNSD — Methodology.xlsx") # m49 codes and regions from https://unstats.un.org/unsd/methodology/m49/overview/
countries <- read_excel("data/World_Bureaus.xlsx") # iso codes with UNHCR regions


##### II. Process and check data ##### 


## first check of demographic data

summary(dem) # x_unknown variables are empty, whereas x_other are populated
glimpse(dem) # confirmed by DAS team: x_other are age unknowns
              

## variable names
dem <- dem %>% 
  rename(
    year = Year,
    origin_iso3 = 'Country of origin (ISO)',
    asylum_iso3 = 'Country of asylum (ISO)',
    female_0_4 = 'Female 0 - 4',
    female_5_11 = 'Female 5 - 11',
    female_12_17 = 'Female 12 - 17',
    female_18_59 = 'Female 18 - 59',
    female_60 = 'Female 60',
    female_unknown = 'f_other',
    female = 'Female total',
    male_0_4 = 'Male 0 - 4',
    male_5_11 = 'Male 5 - 11',
    male_12_17 = 'Male 12 - 17',
    male_18_59 = 'Male 18 - 59',
    male_60 = 'Male 60',
    male_unknown = 'm_other',
    male = 'Male total',
    totalEndYear = 'Total'
  ) %>% 
  select(
    year,
    origin_iso3,
    asylum_iso3,
    female_0_4,
    female_5_11,
    female_12_17,
    female_18_59,
    female_60,
    female_unknown,
    female,
    male_0_4,
    male_5_11,
    male_12_17,
    male_18_59,
    male_60,
    male_unknown,
    male,
    totalEndYear
  )
  


## merge m49 dataset with UNHCR region codes and clean variable names

# check whether there are origin or asylum codes in demo data that are not in m49
unhcr_iso3 <- unique(c(dem$origin_iso3, dem$asylum_iso3))

unhcr_iso3_missingM49 <- unhcr_iso3[!(unhcr_iso3 %in% m49$`ISO-alpha3 Code`)] # "XXA" "TIB" (Stateless and Tibet)
unhcr_iso3_missingWR <- unhcr_iso3[!(unhcr_iso3 %in% countries$iso3)] # "XXA" "TIB", NA (Stateless, Tibet, NA)

# Stateless and Tibet: keep structure including those two codes in imputed dataset! 
# covariates and regions: use those for China for all Tibet codes. for XXA and NA, set region to own "unknown" level, use means of covariates (distance and GDP) 




## total checks



## 


