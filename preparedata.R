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


### download data from UNHCR refugee data finder public database

## Venezuelans displaced abroad (VDA)
vdaUrl <- paste('https://api.unhcr.org/population/v1/demographics/?limit=20&dataset=population&displayType=demographics&columns%5B%5D=vda&yearFrom=2021&yearTo=2021&coo_all=true&coa_all=true&download=true')
vdaTemp <- tempfile()
download.file(vdaUrl, vdaTemp,  method = "curl", mode="wb", quiet = FALSE)

demvda <- read_csv(unz(vdaTemp, "demographics.csv"), skip = 14)
unlink(vdaTemp)

demvda <- demvda %>% 
  mutate(popType = "VDA")


## Refugees
refUrl <- "https://api.unhcr.org/population/v1/demographics/?limit=20&dataset=population&displayType=demographics&columns%5B%5D=refugees&yearFrom=2021&yearTo=2021&coo_all=true&coa_all=true&download=true"
refTemp <- tempfile()
download.file(refUrl, refTemp,  method = "curl", mode="wb", quiet = FALSE)

demref <- read_csv(unz(refTemp, "demographics.csv"), skip = 14)
unlink(refTemp)

demref <- demref %>% 
  mutate(popType = "REF")


## merge VDA and REF (keep popType variable to distinguish in dataset)
dim(demref)
dim(demvda)

dem <- demref %>% 
  full_join(demvda)

dim(dem) # OK


## country and region codes and names
m49 <- read_excel("data/UNSD — Methodology.xlsx") # m49 codes and regions from https://unstats.un.org/unsd/methodology/m49/overview/
countries <- read_excel("data/World_Bureaus.xlsx") # iso codes with UNHCR regions


## remove variables and datasets not needed
rm(list=c("refTemp", "vdaTemp", "refUrl", "vdaUrl", "demref", "demvda"))



##### II. Process and check data ##### 


### first check of demographic data

summary(dem) # x_unknown variables are empty, whereas x_other are populated
glimpse(dem) # confirmed by DAS team: x_other are age unknowns
              

### clean variable names in demographic dataset
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
    popType,
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
  ) %>% 
  mutate(origin_iso3 = replace_na(origin_iso3, "NAA")) # replace missing origin iso3 codes with NAA
  


### merge m49 dataset with UNHCR region codes and clean variable names

## check whether there are origin or asylum codes in demo data that are not in m49
unhcr_iso3 <- unique(c(dem$origin_iso3, dem$asylum_iso3))

unhcr_iso3_missingM49 <- unhcr_iso3[!(unhcr_iso3 %in% m49$`ISO-alpha3 Code`)] # "XXA" "TIB", "NAA" (Stateless and Tibet, Unknown) are in unhcr demographic data, but not in m49
unhcr_iso3_missingCountries <- unhcr_iso3[!(unhcr_iso3 %in% countries$iso3)] # "XXA" "TIB", NAA (Stateless, Tibet, Unknown) are in unhcr demographic data, but not in UNHCR country file

countries_iso3_missingM49 <- countries$iso3[!(countries$iso3 %in% m49$`ISO-alpha3 Code`)] # "TWN", "XKX" (Taiwan, Kosovo) are in unhcr demographic data, but not in m49
 

# Stateless and Tibet: keep structure including those two codes in imputed dataset
# covariates and regions: use those for China for Tibet/Taiwan, Serbia for Kosovo. For XXA and NAA, set region to own "unknown" level, use means of covariates (distance and GDP) 


## add missing origins / asylum countries to m49 dataset
m49 <- m49 %>% 
  rename(
    region="Region Name",
    subregion="Sub-region Name",
    country = "Country or Area",
    m49 = "M49 Code",
    iso3 = "ISO-alpha3 Code"
  )  %>% 
  select(region, subregion, country, m49, iso3) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TIB", country = "Tibet", m49 = 156)) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TWN", country = "Taiwan", m49 = 158)) %>% 
  add_row(region = "Unknown", subregion = "Unknown", country = "Stateless", iso3 = "XXA", m49 = 997) %>% 
  add_row(region = "Unknown", subregion = "Unknown", country = "Unknown", iso3 = "NAA", m49 = 998) %>% 
  add_row(filter(., iso3 == "SRB") %>% mutate(iso3 = "XKX", country = "Kosovo", m49 = 412))

## add missing origins / asylum countries to countries dataset
countries <- countries %>% 
  select(iso3, proGres_code, main_office_short, hcr_region, hcr_subregion) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TIB",  proGres_code = "TIB")) %>% 
  add_row(main_office_short = "Unknown", hcr_region = "Unknown", hcr_subregion = "Unknown", iso3 = "XXA", proGres_code = "XXA") %>% 
  add_row(main_office_short = "Unknown", hcr_region = "Unknown", hcr_subregion = "Unknown", iso3 = "NAA", proGres_code = "NAA")


## create merge of m49 and countries files
dim(m49)
dim(countries)

m49hcr <- m49 %>% 
  left_join(countries, by = "iso3")

dim(m49hcr) # OK
sum(duplicated(m49hcr$iso3)) # OK, no duplicates



## total checks



## 


