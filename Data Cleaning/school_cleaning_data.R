library(tidyverse)
library(readr)
library(dplyr)

#BRISTOL SCHOOLS IN 2019
bsKs4Final19 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2018-2019/801_ks4final.csv")
dim(bsKs4Final19)
colnames(bsKs4Final19)

#Along with Year, certain columns are selected and bsKs4Final19 is made smaller
bsKs4Final19 = bsKs4Final19 %>% 
  mutate(Year = "2019") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

#On checking for missing values, they were seen in columns PCODE, SCHNAME and ATT8SCR
colSums(is.na(bsKs4Final19))
bsKs4Final19 = bsKs4Final19 %>% 
  drop_na(SCHNAME)
colSums(is.na(bsKs4Final19))

str(bsKs4Final19)
bsKs4Final19 = bsKs4Final19 %>% 
  drop_na(ATT8SCR)
colSums(is.na(bsKs4Final19))

#Column PCODE is renamed to POSTCODE to match with the schools data set for inner join
bsKs4Final19 = bsKs4Final19 %>% 
  rename(POSTCODE = PCODE)
colnames(bsKs4Final19)

bsSchoolInfo19 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2018-2019/801_school_information.csv")
dim(bsSchoolInfo19)
colnames(bsSchoolInfo19)
colSums(is.na(bsSchoolInfo19))

#The data set has three separate columns for each school level and their values were binary
#Those columns have been merged and the "1" binary value has been converted to its original corresponding column
#Other data sets are also chosen to minimize bsSchoolInfo19
bsSchoolInfo19 = bsSchoolInfo19 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(bsSchoolInfo19))
dim(bsSchoolInfo19)

#Data sets bsKs4Final19 and bsSchoolInfo19 are merged by inner_join by POSTCODE and SCHNAME
bsSchools19 = inner_join(bsKs4Final19, bsSchoolInfo19,
                         by = c("POSTCODE", "SCHNAME"))
dim(bsSchools19)
#The missing values of TOWN are handled here
colSums(is.na(bsSchools19))

#To find number of rows with value Bristol in TOWN
bsSchools19 %>% 
  filter(TOWN == "Bristol") %>% 
  summarise("bristol_town_schools" = n())

#The rows with missing value or Bristol in TOWN are filtered
bsSchools19 = bsSchools19 %>% 
  filter(TOWN == "Bristol" | is.na(TOWN)) %>% 
  mutate(TOWN = replace_na(TOWN, "Bristol"))
colSums(is.na(bsSchools19))

#BRISTOL SCHOOLS IN 2022
bsKs4Final22 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2021-2022/801_ks4final.csv")
dim(bsKs4Final22)
colnames(bsKs4Final22)

#Column Year created with value 2022
bsKs4Final22 = bsKs4Final22 %>% 
  mutate(Year = "2022") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

colSums(is.na(bsKs4Final22))

#Missing values of SCHNAME and ATT8SCR removed and PCODE column renamed to POSTCODE
bsKs4Final22 = bsKs4Final22 %>% 
  drop_na(SCHNAME) %>% 
  drop_na(ATT8SCR) %>% 
  rename(POSTCODE = PCODE)
colSums(is.na(bsKs4Final22))
dim(bsKs4Final22)

bsSchoolInfo22 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2021-2022/801_school_information.csv")
dim(bsSchoolInfo22)
colnames(bsSchoolInfo22)
colSums(is.na(bsSchoolInfo22))

#Different columns for school level merged to one column SCHLEVEL
bsSchoolInfo22 = bsSchoolInfo22 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(bsSchoolInfo22))
dim(bsSchoolInfo22)

#Data sets merged to create bsSchools22
bsSchools22 = inner_join(bsKs4Final22, bsSchoolInfo22,
                         by = c("POSTCODE", "SCHNAME"))
dim(bsSchools22)
colSums(is.na(bsSchools22))

bsSchools22 %>% 
  filter(TOWN == "Bristol") %>% 
  summarise("bristol_town_schools" = n())

bsSchools22 = bsSchools22 %>% 
  filter(TOWN == "Bristol" | is.na(TOWN)) %>% 
  mutate(TOWN = replace_na(TOWN, "Bristol"))
colSums(is.na(bsSchools22))

#BRISTOL SCHOOLS IN 2023
bsKs4Final23 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2022-2023/801_ks4final.csv")
dim(bsKs4Final23)
colnames(bsKs4Final23)

bsKs4Final23 = bsKs4Final23 %>% 
  mutate(Year = "2023") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

colSums(is.na(bsKs4Final23))

bsKs4Final23 = bsKs4Final23 %>% 
  drop_na(SCHNAME) %>% 
  drop_na(ATT8SCR) %>% 
  rename(POSTCODE = PCODE)
colSums(is.na(bsKs4Final23))
dim(bsKs4Final23)

bsSchoolInfo23 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/bristol/2022-2023/801_school_information.csv")
dim(bsSchoolInfo23)
colnames(bsSchoolInfo23)
colSums(is.na(bsSchoolInfo23))

bsSchoolInfo23 = bsSchoolInfo23 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(bsSchoolInfo23))
dim(bsSchoolInfo23)

bsSchools23 = inner_join(bsKs4Final23, bsSchoolInfo23,
                         by = c("POSTCODE", "SCHNAME"))
dim(bsSchools23)
colSums(is.na(bsSchools23))

bsSchools23 %>% 
  filter(TOWN == "Bristol") %>% 
  summarise("bristol_town_schools" = n())

bsSchools23 = bsSchools23 %>% 
  filter(TOWN == "Bristol" | is.na(TOWN)) %>% 
  mutate(TOWN = replace_na(TOWN, "Bristol"))
colSums(is.na(bsSchools23))

#CORNWALL SCHOOLS IN 2019
cwKs4Final19 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2018-2019/908_ks4final.csv")
dim(cwKs4Final19)
colnames(cwKs4Final19)

cwKs4Final19 = cwKs4Final19 %>% 
  mutate(Year = "2019") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

colSums(is.na(cwKs4Final19))

cwKs4Final19 = cwKs4Final19 %>% 
  drop_na(SCHNAME) %>% 
  drop_na(ATT8SCR) %>% 
  rename(POSTCODE = PCODE)
colSums(is.na(cwKs4Final19))
dim(cwKs4Final19)

cwSchoolInfo19 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2018-2019/908_school_information.csv")
dim(cwSchoolInfo19)
colnames(cwSchoolInfo19)
colSums(is.na(cwSchoolInfo19))

cwSchoolInfo19 = cwSchoolInfo19 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(cwSchoolInfo19))
dim(cwSchoolInfo19)

cwSchools19 = inner_join(cwKs4Final19, cwSchoolInfo19,
                         by = c("POSTCODE", "SCHNAME"))
dim(cwSchools19)
colSums(is.na(cwSchools19))

#Here, data set csSchools19 only contained 2 missing values in TOWN
#Missing values were imputed with actual town names 
#If there were many missing values, they would have to be removed
#because it is impractical to manually fill them all out
cwSchools19 = cwSchools19 %>%
  mutate(TOWN = if_else(is.na(TOWN) & SCHNAME == "St Joseph's School", "Launceston", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Truro School", "Truro", TOWN)) 
colSums(is.na(cwSchools19))

#CORNWALL SCHOOLS IN 2022
cwKs4Final22 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2021-2022/908_ks4final.csv")
dim(cwKs4Final22)
colnames(cwKs4Final22)

cwKs4Final22 = cwKs4Final22 %>% 
  mutate(Year = "2022") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

colSums(is.na(cwKs4Final22))

#The data set didn't have missing values in ATT8SCR like in other data sets
cwKs4Final22 = cwKs4Final22 %>% 
  drop_na(SCHNAME) %>% 
  rename(POSTCODE = PCODE)
colSums(is.na(cwKs4Final22))
dim(cwKs4Final22)

cwSchoolInfo22 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2021-2022/908_school_information.csv")
dim(cwSchoolInfo22)
colnames(cwSchoolInfo22)
colSums(is.na(cwSchoolInfo22))

cwSchoolInfo22 = cwSchoolInfo22 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(cwSchoolInfo22))
dim(cwSchoolInfo22)

cwSchools22 = inner_join(cwKs4Final22, cwSchoolInfo22,
                         by = c("POSTCODE", "SCHNAME"))
dim(cwSchools22)
colSums(is.na(cwSchools22))

#Value imputation for 4 missing values of TOWN
cwSchools22 = cwSchools22 %>%
  mutate(TOWN = if_else(is.na(TOWN) & SCHNAME == "The Lowen School", "Gunnislake", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Red Moor School", "Lanlivery", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Taliesin Education Ltd", "Liskeard", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Truro School", "Truro", TOWN))
colSums(is.na(cwSchools22))

#CORNWALL SCHOOLS IN 2023
cwKs4Final23 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2022-2023/908_ks4final.csv")
dim(cwKs4Final23)
colnames(cwKs4Final23)

cwKs4Final23 = cwKs4Final23 %>% 
  mutate(Year = "2023") %>% 
  select(PCODE,Year,SCHNAME,ATT8SCR)

colSums(is.na(cwKs4Final23))

cwKs4Final23 = cwKs4Final23 %>% 
  drop_na(SCHNAME) %>% 
  rename(POSTCODE = PCODE)
colSums(is.na(cwKs4Final23))
dim(cwKs4Final23)

cwSchoolInfo23 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/schools/cornwall/2022-2023/908_school_information.csv")
dim(cwSchoolInfo23)
colnames(cwSchoolInfo23)
colSums(is.na(cwSchoolInfo23))

cwSchoolInfo23 = cwSchoolInfo23 %>% 
  mutate(SCHLEVEL = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16')) %>% 
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)
colSums(is.na(cwSchoolInfo23))
dim(cwSchoolInfo23)

cwSchools23 = inner_join(cwKs4Final23, cwSchoolInfo23,
                         by = c("POSTCODE", "SCHNAME"))
dim(cwSchools23)
colSums(is.na(cwSchools23))

cwSchools23 = cwSchools23 %>%
  mutate(TOWN = if_else(is.na(TOWN) & SCHNAME == "The Lowen School", "Gunnislake", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Red Moor School", "Lanlivery", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Taliesin Education Ltd", "Liskeard", TOWN),
         TOWN = if_else(is.na(TOWN) & SCHNAME == "Truro School", "Truro", TOWN))
colSums(is.na(cwSchools23))

#Data sets of Bristol from above years merged by bind_rows()
bristolSchools = bind_rows(bsSchools19, bsSchools22, bsSchools23)
colSums(is.na(bristolSchools))
dim(bristolSchools)
str(bristolSchools)
#Calculation of mode of OFSTEDRATING
mode_ofstedrating = bristolSchools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

#The cleaned data set is filled with mode for missing values of OFSTEDRATING
bristolSchoolsCleaned = bristolSchools %>% 
  mutate("COUNTY" = "City of Bristol") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating, OFSTEDRATING))
colSums(is.na(bristolSchoolsCleaned))
dim(bristolSchoolsCleaned)

#Data sets of Cornwall merged to create cornwallSchools
cornwallSchools = bind_rows(cwSchools19, cwSchools22, cwSchools23)
colSums(is.na(cornwallSchools))
dim(cornwallSchools)

#Calculation of mode of OFSTEDRATING
mode_ofstedrating_cw = cornwallSchools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

#Data set cleaned by filling missing values of OFSTEDRATING by mode and adding COUNTY column
cornwallSchoolsCleaned = cornwallSchools %>% 
  mutate("COUNTY" = "Cornwall") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating_cw, OFSTEDRATING))
colSums(is.na(cornwallSchoolsCleaned))
dim(cornwallSchoolsCleaned)

write.csv(bristolSchoolsCleaned, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/bristol-schools.csv", row.names = FALSE)
write.csv(cornwallSchoolsCleaned, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/schools/cornwall-schools.csv", row.names = FALSE)
