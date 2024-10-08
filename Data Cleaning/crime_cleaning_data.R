library(tidyverse)
library(readr)
library(dplyr)

#The data sets for 2021 are loaded and analyzed via dimensions and number of missing values for each column
bsCrimeRate2105 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-05/2021-05-avon-and-somerset-street.csv")
dim(bsCrimeRate2105)                           
colSums(is.na(bsCrimeRate2105))
str(bsCrimeRate2105)

bsCrimeRate2106 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-06/2021-06-avon-and-somerset-street.csv")
dim(bsCrimeRate2106)
colSums(is.na(bsCrimeRate2106))

bsCrimeRate2107 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-07/2021-07-avon-and-somerset-street.csv")
dim(bsCrimeRate2107)
colSums(is.na(bsCrimeRate2107))

bsCrimeRate2108 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-08/2021-08-avon-and-somerset-street.csv")
dim(bsCrimeRate2108)
colSums(is.na(bsCrimeRate2108))

bsCrimeRate2109 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-09/2021-09-avon-and-somerset-street.csv")
dim(bsCrimeRate2109)
colSums(is.na(bsCrimeRate2109))

bsCrimeRate2110 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-10/2021-10-avon-and-somerset-street.csv")
dim(bsCrimeRate2110)
colSums(is.na(bsCrimeRate2110))

bsCrimeRate2111 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-11/2021-11-avon-and-somerset-street.csv")
dim(bsCrimeRate2111)
colSums(is.na(bsCrimeRate2111))

bsCrimeRate2112 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-12/2021-12-avon-and-somerset-street.csv")
dim(bsCrimeRate2112)
colSums(is.na(bsCrimeRate2112))

# Remove rows with missing 'Crime ID' from each monthly dataset and check for remaining missing values.
bsCrimeRate2105 = bsCrimeRate2105 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bsCrimeRate2105))
dim(bsCrimeRate2105)
bsCrimeRate2106 = bsCrimeRate2106 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2107 = bsCrimeRate2107 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2108 = bsCrimeRate2108 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2109 = bsCrimeRate2109 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2110 = bsCrimeRate2110 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2111 = bsCrimeRate2111 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2112 = bsCrimeRate2112 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bsCrimeRate2112))

#After removing the missing crime id rows, data set bsCrimeRate2021 is created by merging all data sets for 2021
bsCrimeRate2021 = bind_rows(bsCrimeRate2105, bsCrimeRate2106, bsCrimeRate2107, bsCrimeRate2108,
                            bsCrimeRate2109, bsCrimeRate2110, bsCrimeRate2111, bsCrimeRate2112)
dim(bsCrimeRate2021)
colSums(is.na(bsCrimeRate2021))
bsCrimeRate2021 = bsCrimeRate2021 %>% 
  select(-Context)
colSums(is.na(bsCrimeRate2021))
str(bsCrimeRate2021)
summary(bsCrimeRate2021)
sum(duplicated(bsCrimeRate2021))
duplicated_rows = duplicated(bsCrimeRate2021)
bsCrimeRate2021 = bsCrimeRate2021[!duplicated_rows, ]
dim(bsCrimeRate2021)
colSums(is.na(bsCrimeRate2021))
bsCrimeRate2021$Longitude[is.na(bsCrimeRate2021$Longitude)] = median(bsCrimeRate2021$Longitude, na.rm = TRUE)
bsCrimeRate2021$Latitude[is.na(bsCrimeRate2021$Latitude)] = median(bsCrimeRate2021$Latitude, na.rm = TRUE)
colSums(is.na(bsCrimeRate2021))
get_mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Calculation of modes of LSOA code and LSOA name
lsoaCodeMode = get_mode(bsCrimeRate2021$`LSOA code`[!is.na(bsCrimeRate2021$`LSOA code`)])
lsoaNameMode = get_mode(bsCrimeRate2021$`LSOA name`[!is.na(bsCrimeRate2021$`LSOA name`)])
bsCrimeRate2021$`LSOA code`[is.na(bsCrimeRate2021$`LSOA code`)] = lsoaCodeMode
bsCrimeRate2021$`LSOA name`[is.na(bsCrimeRate2021$`LSOA name`)] = lsoaNameMode
colSums(is.na(bsCrimeRate2021))
summary(bsCrimeRate2021)
sum(duplicated(bsCrimeRate2021))


#Data sets of crime rate in Bristol in 2022 are loaded and analyzed by dimensions and missing values
bsCrimeRate2201 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-01/2022-01-avon-and-somerset-street.csv")
dim(bsCrimeRate2201)
colSums(is.na(bsCrimeRate2201))

bsCrimeRate2202 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-02/2022-02-avon-and-somerset-street.csv")
dim(bsCrimeRate2202)
colSums(is.na(bsCrimeRate2202))

bsCrimeRate2203 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-03/2022-03-avon-and-somerset-street.csv")
dim(bsCrimeRate2203)
colSums(is.na(bsCrimeRate2203))

bsCrimeRate2204 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-04/2022-04-avon-and-somerset-street.csv")
dim(bsCrimeRate2204)
colSums(is.na(bsCrimeRate2204))

bsCrimeRate2205 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-05/2022-05-avon-and-somerset-street.csv")
dim(bsCrimeRate2205)
colSums(is.na(bsCrimeRate2205))

bsCrimeRate2206 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-06/2022-06-avon-and-somerset-street.csv")
dim(bsCrimeRate2206)
colSums(is.na(bsCrimeRate2206))

bsCrimeRate2207 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-07/2022-07-avon-and-somerset-street.csv")
dim(bsCrimeRate2207)
colSums(is.na(bsCrimeRate2207))

bsCrimeRate2208 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-08/2022-08-avon-and-somerset-street.csv")
dim(bsCrimeRate2208)
colSums(is.na(bsCrimeRate2208))

bsCrimeRate2209 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-09/2022-09-avon-and-somerset-street.csv")
dim(bsCrimeRate2209)
colSums(is.na(bsCrimeRate2209))

bsCrimeRate2210 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-10/2022-10-avon-and-somerset-street.csv")
dim(bsCrimeRate2210)
colSums(is.na(bsCrimeRate2210))

bsCrimeRate2211 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-11/2022-11-avon-and-somerset-street.csv")
dim(bsCrimeRate2211)
colSums(is.na(bsCrimeRate2211))

bsCrimeRate2212 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-12/2022-12-avon-and-somerset-street.csv")
dim(bsCrimeRate2212)
colSums(is.na(bsCrimeRate2212))

#The rows with missing crime ids are removed from each data set
bsCrimeRate2201 = bsCrimeRate2201 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bsCrimeRate2201))
dim(bsCrimeRate2201)

bsCrimeRate2202 = bsCrimeRate2202 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2203 = bsCrimeRate2203 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2204 = bsCrimeRate2204 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2205 = bsCrimeRate2205 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2206 = bsCrimeRate2206 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2207 = bsCrimeRate2207 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2208 = bsCrimeRate2208 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2209 = bsCrimeRate2209 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2210 = bsCrimeRate2210 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2211 = bsCrimeRate2211 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2212 = bsCrimeRate2212 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bsCrimeRate2212))

#Data set bsCrimeRate2022 is created by merging data sets of Bristol from 2022
bsCrimeRate2022 = bind_rows(bsCrimeRate2201, bsCrimeRate2202, bsCrimeRate2203, bsCrimeRate2204,
                            bsCrimeRate2205, bsCrimeRate2206, bsCrimeRate2207, bsCrimeRate2208, 
                            bsCrimeRate2209, bsCrimeRate2210, bsCrimeRate2211, bsCrimeRate2212)
dim(bsCrimeRate2022)

#Column "Context" is removed because it is completely empty
colSums(is.na(bsCrimeRate2022))
bsCrimeRate2022 = bsCrimeRate2022 %>% 
  select(-Context)
colSums(is.na(bsCrimeRate2022))

#The data set has duplicated rows and they are removed
sum(duplicated(bsCrimeRate2022))
duplicated_rows22 = duplicated(bsCrimeRate2022)
bsCrimeRate2022 = bsCrimeRate2022[!duplicated_rows22, ]
dim(bsCrimeRate2022)
colSums(is.na(bsCrimeRate2022))

summary(bsCrimeRate2022)

#The missing values of Longitude and Latitude are imputed by their median values
bsCrimeRate2022$Longitude[is.na(bsCrimeRate2022$Longitude)] = median(bsCrimeRate2022$Longitude, na.rm = TRUE)
bsCrimeRate2022$Latitude[is.na(bsCrimeRate2022$Latitude)] = median(bsCrimeRate2022$Latitude, na.rm = TRUE)
colSums(is.na(bsCrimeRate2022))

#Using the user-defined get_mode() function, mode of LSOA code and LSOA name are calculated and imputed
lsoaCodeMode22 = get_mode(bsCrimeRate2022$`LSOA code`[!is.na(bsCrimeRate2022$`LSOA code`)])
lsoaNameMode22 = get_mode(bsCrimeRate2022$`LSOA name`[!is.na(bsCrimeRate2022$`LSOA name`)])
bsCrimeRate2022$`LSOA code`[is.na(bsCrimeRate2022$`LSOA code`)] = lsoaCodeMode22
bsCrimeRate2022$`LSOA name`[is.na(bsCrimeRate2022$`LSOA name`)] = lsoaNameMode22
colSums(is.na(bsCrimeRate2022))
summary(bsCrimeRate2022)
sum(duplicated(bsCrimeRate2022))


#Data sets of crime rate in Bristol in 2023 are loaded and analyzed via dimensions and missing values
bsCrimeRate2301 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-01/2023-01-avon-and-somerset-street.csv")
dim(bsCrimeRate2301)
colSums(is.na(bsCrimeRate2301))

bsCrimeRate2302 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-02/2023-02-avon-and-somerset-street.csv")
dim(bsCrimeRate2302)
colSums(is.na(bsCrimeRate2302))

bsCrimeRate2303 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-03/2023-03-avon-and-somerset-street.csv")
dim(bsCrimeRate2303)
colSums(is.na(bsCrimeRate2303))

bsCrimeRate2304 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-04/2023-04-avon-and-somerset-street.csv")
dim(bsCrimeRate2304)
colSums(is.na(bsCrimeRate2304))

bsCrimeRate2305 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-05/2023-05-avon-and-somerset-street.csv")
dim(bsCrimeRate2305)
colSums(is.na(bsCrimeRate2305))

bsCrimeRate2306 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-06/2023-06-avon-and-somerset-street.csv")
dim(bsCrimeRate2306)
colSums(is.na(bsCrimeRate2306))

bsCrimeRate2307 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-07/2023-07-avon-and-somerset-street.csv")
dim(bsCrimeRate2307)
colSums(is.na(bsCrimeRate2307))

bsCrimeRate2308 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-08/2023-08-avon-and-somerset-street.csv")
dim(bsCrimeRate2308)
colSums(is.na(bsCrimeRate2308))

bsCrimeRate2309 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-09/2023-09-avon-and-somerset-street.csv")
dim(bsCrimeRate2309)
colSums(is.na(bsCrimeRate2309))

bsCrimeRate2310 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-10/2023-10-avon-and-somerset-street.csv")
dim(bsCrimeRate2310)
colSums(is.na(bsCrimeRate2310))

bsCrimeRate2311 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-11/2023-11-avon-and-somerset-street.csv")
dim(bsCrimeRate2311)
colSums(is.na(bsCrimeRate2311))

bsCrimeRate2312 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-12/2023-12-avon-and-somerset-street.csv")
dim(bsCrimeRate2312)
colSums(is.na(bsCrimeRate2312))

#Rows with missing crime ids are removed from data sets of 2023
bsCrimeRate2301 = bsCrimeRate2301 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bsCrimeRate2301))
dim(bsCrimeRate2301)

bsCrimeRate2302 = bsCrimeRate2302 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2303 = bsCrimeRate2303 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2304 = bsCrimeRate2304 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2305 = bsCrimeRate2305 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2306 = bsCrimeRate2306 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2307 = bsCrimeRate2307 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2308 = bsCrimeRate2308 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2309 = bsCrimeRate2309 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2310 = bsCrimeRate2310 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2311 = bsCrimeRate2311 %>%
  filter(!is.na(`Crime ID`))
bsCrimeRate2312 = bsCrimeRate2312 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bsCrimeRate2312))

#Data set bsCrimeRate2023 is created by merging 2023 data sets of Bristol
bsCrimeRate2023 = bind_rows(bsCrimeRate2301, bsCrimeRate2302, bsCrimeRate2303, bsCrimeRate2304,
                            bsCrimeRate2305, bsCrimeRate2306, bsCrimeRate2307, bsCrimeRate2308, 
                            bsCrimeRate2309, bsCrimeRate2310, bsCrimeRate2311, bsCrimeRate2312)
dim(bsCrimeRate2023)

#The empty column "Context" is removed
colSums(is.na(bsCrimeRate2023))
bsCrimeRate2023 = bsCrimeRate2023 %>% 
  select(-Context)
colSums(is.na(bsCrimeRate2023))

#The duplicated rows are removed
sum(duplicated(bsCrimeRate2023))
duplicated_rows23 = duplicated(bsCrimeRate2023)
bsCrimeRate2023 = bsCrimeRate2023[!duplicated_rows23, ]
dim(bsCrimeRate2023)
colSums(is.na(bsCrimeRate2023))

summary(bsCrimeRate2023)

#The missing values of Longitude and Latitude are imputed by their median values
bsCrimeRate2023$Longitude[is.na(bsCrimeRate2023$Longitude)] = median(bsCrimeRate2023$Longitude, na.rm = TRUE)
bsCrimeRate2023$Latitude[is.na(bsCrimeRate2023$Latitude)] = median(bsCrimeRate2023$Latitude, na.rm = TRUE)
colSums(is.na(bsCrimeRate2023))

#Using get_mode(), the mode of LSOA code and LSOA name are calculated and used for imputation
lsoaCodeMode23 = get_mode(bsCrimeRate2023$`LSOA code`[!is.na(bsCrimeRate2023$`LSOA code`)])
lsoaNameMode23 = get_mode(bsCrimeRate2023$`LSOA name`[!is.na(bsCrimeRate2023$`LSOA name`)])
bsCrimeRate2023$`LSOA code`[is.na(bsCrimeRate2023$`LSOA code`)] = lsoaCodeMode23
bsCrimeRate2023$`LSOA name`[is.na(bsCrimeRate2023$`LSOA name`)] = lsoaNameMode23
colSums(is.na(bsCrimeRate2023))
summary(bsCrimeRate2023)
sum(duplicated(bsCrimeRate2023))


#Data sets for 2021 loaded and analyzed
cwCrimeRate2105 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-05/2021-05-devon-and-cornwall-street.csv")
dim(cwCrimeRate2105)                           
colSums(is.na(cwCrimeRate2105))

cwCrimeRate2106 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-06/2021-06-devon-and-cornwall-street.csv")
dim(cwCrimeRate2106)
colSums(is.na(cwCrimeRate2106))

cwCrimeRate2107 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-07/2021-07-devon-and-cornwall-street.csv")
dim(cwCrimeRate2107)
colSums(is.na(cwCrimeRate2107))

cwCrimeRate2108 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-08/2021-08-devon-and-cornwall-street.csv")
dim(cwCrimeRate2108)
colSums(is.na(cwCrimeRate2108))

cwCrimeRate2109 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-09/2021-09-devon-and-cornwall-street.csv")
dim(cwCrimeRate2109)
colSums(is.na(cwCrimeRate2109))

cwCrimeRate2110 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-10/2021-10-devon-and-cornwall-street.csv")
dim(cwCrimeRate2110)
colSums(is.na(cwCrimeRate2110))

cwCrimeRate2111 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-11/2021-11-devon-and-cornwall-street.csv")
dim(cwCrimeRate2111)
colSums(is.na(cwCrimeRate2111))

cwCrimeRate2112 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2021-12/2021-12-devon-and-cornwall-street.csv")
dim(cwCrimeRate2112)
colSums(is.na(cwCrimeRate2112))

#Rows with missing crime id are removed
cwCrimeRate2105 = cwCrimeRate2105 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cwCrimeRate2105))
dim(cwCrimeRate2105)

cwCrimeRate2106 = cwCrimeRate2106 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2107 = cwCrimeRate2107 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2108 = cwCrimeRate2108 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2109 = cwCrimeRate2109 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2110 = cwCrimeRate2110 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2111 = cwCrimeRate2111 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2112 = cwCrimeRate2112 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cwCrimeRate2112))

#Data set cwCrimeRate2021 is created by merging all data sets for 2021
cwCrimeRate2021 = bind_rows(cwCrimeRate2105, cwCrimeRate2106, cwCrimeRate2107, cwCrimeRate2108,
                            cwCrimeRate2109, cwCrimeRate2110, cwCrimeRate2111, cwCrimeRate2112)
dim(cwCrimeRate2021)

colSums(is.na(cwCrimeRate2021))
#The empty column "Context" is removed
cwCrimeRate2021 = cwCrimeRate2021 %>% 
  select(-Context)
colSums(is.na(cwCrimeRate2021))

str(cwCrimeRate2021)
summary(cwCrimeRate2021)

#The duplicated rows are removed 
sum(duplicated(cwCrimeRate2021))
duplicated_rows_cw21 = duplicated(cwCrimeRate2021)
cwCrimeRate2021 = cwCrimeRate2021[!duplicated_rows_cw21, ]
dim(cwCrimeRate2021)
colSums(is.na(cwCrimeRate2021))

#Imputation of missing values: Longitude and Latitude
cwCrimeRate2021$Longitude[is.na(cwCrimeRate2021$Longitude)] = median(cwCrimeRate2021$Longitude, na.rm = TRUE)
cwCrimeRate2021$Latitude[is.na(cwCrimeRate2021$Latitude)] = median(cwCrimeRate2021$Latitude, na.rm = TRUE)
colSums(is.na(cwCrimeRate2021))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw21 = get_mode(cwCrimeRate2021$`LSOA code`[!is.na(cwCrimeRate2021$`LSOA code`)])
lsoaNameMode_cw21 = get_mode(cwCrimeRate2021$`LSOA name`[!is.na(cwCrimeRate2021$`LSOA name`)])
cwCrimeRate2021$`LSOA code`[is.na(cwCrimeRate2021$`LSOA code`)] = lsoaCodeMode_cw21
cwCrimeRate2021$`LSOA name`[is.na(cwCrimeRate2021$`LSOA name`)] = lsoaNameMode_cw21

colSums(is.na(cwCrimeRate2021))
summary(cwCrimeRate2021)
sum(duplicated(cwCrimeRate2021))


#Data sets for 2022 loaded and analyzed
cwCrimeRate2201 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-01/2022-01-devon-and-cornwall-street.csv")
dim(cwCrimeRate2201)                           
colSums(is.na(cwCrimeRate2201))

cwCrimeRate2202 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-02/2022-02-devon-and-cornwall-street.csv")
dim(cwCrimeRate2202)                           
colSums(is.na(cwCrimeRate2202))

cwCrimeRate2203 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-03/2022-03-devon-and-cornwall-street.csv")
dim(cwCrimeRate2203)                           
colSums(is.na(cwCrimeRate2203))

cwCrimeRate2204 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-04/2022-04-devon-and-cornwall-street.csv")
dim(cwCrimeRate2204)                           
colSums(is.na(cwCrimeRate2204))

cwCrimeRate2205 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-05/2022-05-devon-and-cornwall-street.csv")
dim(cwCrimeRate2205)                           
colSums(is.na(cwCrimeRate2205))

cwCrimeRate2206 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-06/2022-06-devon-and-cornwall-street.csv")
dim(cwCrimeRate2206)
colSums(is.na(cwCrimeRate2206))

cwCrimeRate2207 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-07/2022-07-devon-and-cornwall-street.csv")
dim(cwCrimeRate2207)
colSums(is.na(cwCrimeRate2207))

cwCrimeRate2208 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-08/2022-08-devon-and-cornwall-street.csv")
dim(cwCrimeRate2208)
colSums(is.na(cwCrimeRate2208))

cwCrimeRate2209 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-09/2022-09-devon-and-cornwall-street.csv")
dim(cwCrimeRate2209)
colSums(is.na(cwCrimeRate2209))

cwCrimeRate2210 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-10/2022-10-devon-and-cornwall-street.csv")
dim(cwCrimeRate2210)
colSums(is.na(cwCrimeRate2210))

cwCrimeRate2211 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-11/2022-11-devon-and-cornwall-street.csv")
dim(cwCrimeRate2211)
colSums(is.na(cwCrimeRate2211))

cwCrimeRate2212 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2022-12/2022-12-devon-and-cornwall-street.csv")
dim(cwCrimeRate2212)
colSums(is.na(cwCrimeRate2212))

#Rows with missing crime id are removed
cwCrimeRate2201 = cwCrimeRate2201 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cwCrimeRate2201))
dim(cwCrimeRate2201)

cwCrimeRate2202 = cwCrimeRate2202 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2203 = cwCrimeRate2203 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2204 = cwCrimeRate2204 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2205 = cwCrimeRate2205 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2206 = cwCrimeRate2206 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2207 = cwCrimeRate2207 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2208 = cwCrimeRate2208 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2209 = cwCrimeRate2209 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2210 = cwCrimeRate2210 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2211 = cwCrimeRate2211 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2212 = cwCrimeRate2212 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cwCrimeRate2212))

#Data set cwCrimeRate2022 is created by merging all data sets from 2022
cwCrimeRate2022 = bind_rows(cwCrimeRate2201, cwCrimeRate2202, cwCrimeRate2203, cwCrimeRate2204,
                            cwCrimeRate2205, cwCrimeRate2206, cwCrimeRate2207, cwCrimeRate2208,
                            cwCrimeRate2209, cwCrimeRate2210, cwCrimeRate2211, cwCrimeRate2212)
dim(cwCrimeRate2022)

colSums(is.na(cwCrimeRate2022))
#The empty column "Context" is removed
cwCrimeRate2022 = cwCrimeRate2022 %>% 
  select(-Context)
colSums(is.na(cwCrimeRate2022))

summary(cwCrimeRate2022)

#The duplicated rows are removed 
sum(duplicated(cwCrimeRate2022))
duplicated_rows_cw22 = duplicated(cwCrimeRate2022)
cwCrimeRate2022 = cwCrimeRate2022[!duplicated_rows_cw22, ]
dim(cwCrimeRate2022)
colSums(is.na(cwCrimeRate2022))

#Imputation of missing values: Longitude and Latitude
cwCrimeRate2022$Longitude[is.na(cwCrimeRate2022$Longitude)] = median(cwCrimeRate2022$Longitude, na.rm = TRUE)
cwCrimeRate2022$Latitude[is.na(cwCrimeRate2022$Latitude)] = median(cwCrimeRate2022$Latitude, na.rm = TRUE)
colSums(is.na(cwCrimeRate2022))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw22 = get_mode(cwCrimeRate2022$`LSOA code`[!is.na(cwCrimeRate2022$`LSOA code`)])
lsoaNameMode_cw22 = get_mode(cwCrimeRate2022$`LSOA name`[!is.na(cwCrimeRate2022$`LSOA name`)])
cwCrimeRate2022$`LSOA code`[is.na(cwCrimeRate2022$`LSOA code`)] = lsoaCodeMode_cw22
cwCrimeRate2022$`LSOA name`[is.na(cwCrimeRate2022$`LSOA name`)] = lsoaNameMode_cw22

colSums(is.na(cwCrimeRate2022))
summary(cwCrimeRate2022)
sum(duplicated(cwCrimeRate2022))


#Data sets for 2023 loaded and analyzed
cwCrimeRate2301 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-01/2023-01-devon-and-cornwall-street.csv")
dim(cwCrimeRate2301)                           
colSums(is.na(cwCrimeRate2301))

cwCrimeRate2302 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-02/2023-02-devon-and-cornwall-street.csv")
dim(cwCrimeRate2302)                           
colSums(is.na(cwCrimeRate2302))

cwCrimeRate2303 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-03/2023-03-devon-and-cornwall-street.csv")
dim(cwCrimeRate2303)                           
colSums(is.na(cwCrimeRate2303))

cwCrimeRate2304 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-04/2023-04-devon-and-cornwall-street.csv")
dim(cwCrimeRate2304)                           
colSums(is.na(cwCrimeRate2304))

cwCrimeRate2305 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-05/2023-05-devon-and-cornwall-street.csv")
dim(cwCrimeRate2305)                           
colSums(is.na(cwCrimeRate2305))

cwCrimeRate2306 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-06/2023-06-devon-and-cornwall-street.csv")
dim(cwCrimeRate2306)
colSums(is.na(cwCrimeRate2306))

cwCrimeRate2307 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-07/2023-07-devon-and-cornwall-street.csv")
dim(cwCrimeRate2307)
colSums(is.na(cwCrimeRate2307))

cwCrimeRate2308 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-08/2023-08-devon-and-cornwall-street.csv")
dim(cwCrimeRate2308)
colSums(is.na(cwCrimeRate2308))

cwCrimeRate2309 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-09/2023-09-devon-and-cornwall-street.csv")
dim(cwCrimeRate2309)
colSums(is.na(cwCrimeRate2309))

cwCrimeRate2310 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-10/2023-10-devon-and-cornwall-street.csv")
dim(cwCrimeRate2310)
colSums(is.na(cwCrimeRate2310))

cwCrimeRate2311 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-11/2023-11-devon-and-cornwall-street.csv")
dim(cwCrimeRate2311)
colSums(is.na(cwCrimeRate2311))

cwCrimeRate2312 = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/crime/2023-12/2023-12-devon-and-cornwall-street.csv")
dim(cwCrimeRate2312)
colSums(is.na(cwCrimeRate2312))

#Rows with missing crime id are removed
cwCrimeRate2301 = cwCrimeRate2301 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cwCrimeRate2301))
dim(cwCrimeRate2301)

cwCrimeRate2302 = cwCrimeRate2302 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2303 = cwCrimeRate2303 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2304 = cwCrimeRate2304 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2305 = cwCrimeRate2305 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2306 = cwCrimeRate2306 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2307 = cwCrimeRate2307 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2308 = cwCrimeRate2308 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2309 = cwCrimeRate2309 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2310 = cwCrimeRate2310 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2311 = cwCrimeRate2311 %>%
  filter(!is.na(`Crime ID`))
cwCrimeRate2312 = cwCrimeRate2312 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cwCrimeRate2312))

#Data set cwCrimeRate2023 is created by merging all data sets from 2023
cwCrimeRate2023 = bind_rows(cwCrimeRate2301, cwCrimeRate2302, cwCrimeRate2303, cwCrimeRate2304,
                            cwCrimeRate2305, cwCrimeRate2306, cwCrimeRate2307, cwCrimeRate2308,
                            cwCrimeRate2309, cwCrimeRate2310, cwCrimeRate2311, cwCrimeRate2312)
dim(cwCrimeRate2023)

colSums(is.na(cwCrimeRate2023))
#The empty column "Context" is removed
cwCrimeRate2023 = cwCrimeRate2023 %>% 
  select(-Context)
colSums(is.na(cwCrimeRate2023))

summary(cwCrimeRate2023)

#This data set has no duplicated rows 
sum(duplicated(cwCrimeRate2023))

#Imputation of missing values: Longitude and Latitude
cwCrimeRate2023$Longitude[is.na(cwCrimeRate2023$Longitude)] = median(cwCrimeRate2023$Longitude, na.rm = TRUE)
cwCrimeRate2023$Latitude[is.na(cwCrimeRate2023$Latitude)] = median(cwCrimeRate2023$Latitude, na.rm = TRUE)
colSums(is.na(cwCrimeRate2023))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw23 = get_mode(cwCrimeRate2023$`LSOA code`[!is.na(cwCrimeRate2023$`LSOA code`)])
lsoaNameMode_cw23 = get_mode(cwCrimeRate2023$`LSOA name`[!is.na(cwCrimeRate2023$`LSOA name`)])
cwCrimeRate2023$`LSOA code`[is.na(cwCrimeRate2023$`LSOA code`)] = lsoaCodeMode_cw23
cwCrimeRate2023$`LSOA name`[is.na(cwCrimeRate2023$`LSOA name`)] = lsoaNameMode_cw23

colSums(is.na(cwCrimeRate2023))
summary(cwCrimeRate2023)


#CLEANED DATA SETS
bristolCrimeRateCleaned = bind_rows(bsCrimeRate2021, bsCrimeRate2022, bsCrimeRate2023)
dim(bristolCrimeRateCleaned)
sum(duplicated(bristolCrimeRateCleaned))

cornwallCrimeRateCleaned = bind_rows(cwCrimeRate2021, cwCrimeRate2022, cwCrimeRate2023)
dim(cornwallCrimeRateCleaned)

#For adding town/city to the data set
pscdToLsoa = read_csv("C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/raw/population/Postcode to LSOA.csv")
dim(pscdToLsoa)
colnames(pscdToLsoa)

#The data set is narrowed down to necessary columns
pscdToLsoa = pscdToLsoa %>% 
  rename(postcode_space = pcds) %>% 
  rename(`LSOA code` = lsoa11cd) %>% 
  rename(city = ladnm) %>% 
  select(postcode_space, `LSOA code`, city)
dim(pscdToLsoa)
pscdToLsoa = pscdToLsoa %>% 
  group_by(`LSOA code`) %>% 
  summarize(
    postcode_space = first(postcode_space), 
    city = first(city))
dim(pscdToLsoa)

#Inner join done for bristolCrimeRateCleaned and pscdToLsoa, only retaining common rows
bristolCrimeRateCleaned = bristolCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")
dim(bristolCrimeRateCleaned)

#Data set bristolCrimeSummary records the number of different crimes that occurred in cities of Bristol, from 2021-2023
bristolCrimeSummary = bristolCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)

#cornwallCrimeRateCleaned inner joined with pscdToLsoa
cornwallCrimeRateCleaned = cornwallCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")
dim(cornwallCrimeRateCleaned)

#Number of different crimes that occurred in cities of Cornwall from 2021-2023
cornwallCrimeSummary = cornwallCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)

write.csv(bristolCrimeRateCleaned, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-rate.csv", row.names = FALSE)
write.csv(cornwallCrimeRateCleaned, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-rate.csv", row.names = FALSE)
write.csv(bristolCrimeSummary, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/bristol-crime-summary.csv", row.names = FALSE)
write.csv(cornwallCrimeSummary, "C:/Users/DELL/OneDrive/Desktop/Data Science Assignment/cleaned/crime/cornwall-crime-summary.csv", row.names = FALSE)