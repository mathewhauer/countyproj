rm(list=ls())
library(tidyverse)
library(tigris)
library(censusapi)
library(tidycensus)



key <- census_api_key("0206e3f2924a424be8722887fd0a49cea6308a7e")
key <- "0206e3f2924a424be8722887fd0a49cea6308a7e"

# Gathering a list of the available API's from CENSUSAPI
apis <- listCensusApis()

#Setting both the estimates vintage year and the date within the Census Bureau's estimates program
vintage_year = "2016" #<----------------------------------------------------------- INPUT VINTAGE ESTIMATE YEAR HERE
vintage_end = "9"     #<----------------------------------------------------------- INPUT VINTAGE DATE

#   DATE    DATE_DESC
#   1       4/1/2010 Census population
#   2       4/1/2010 population estimates base
#   3       7/1/2010 population estimate
#   4       7/1/2011 population estimate
#   5       7/1/2012 population estimate
#   6       7/1/2013 population estimate 
#   7       7/1/2014 population estimate
#   8       7/1/2015 population estimate
#   9       7/1/2016 population estimate
#   10      7/1/2017 population estimate
#   11      7/1/2018 population estimate
#   12      7/1/2019 population estimate
#
# 0 = All races
# 1 = White alone
# 2 = Black alone
# 3 = American Indian and Alaska Native alone
# 4 = Asian alone
# 5 = Native Hawaiian and Other Pacific Islander alone
# 6 = Two or more races
# 7 = White alone or in combination
# 8 = Black alone or in combination
# 9 = American Indian and Alaska Native alone or in combination
# 10 = Asian alone or in combination
# 11 = Native Hawaiian and Other Pacific Islander alone or in combination
# 
# HISP: Hispanic Origin
# 0=Both Hispanic Origins
# 1=Non-Hispanic
# 2=Hispanic

fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  rename(state = X1,
         STATEID = X2,
         CNTYID = X3,
         NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))

stateid = unlist(list(unique(fipslist$STATEID)))
stateid <- c("01", "10")
x<-"01"
getgq_2010 = function(x){
  tryCatch({
    # listing the available census variables in the population estimates agegroups data file
    list <- listCensusMetadata(name = "sf1", vintage = "2010", type ="variables")
    occupied_hhpopvars <- c('PCT013B006', 'PCT013B007', 'PCT013B008', 'PCT013B009', 'PCT013B010', 'PCT013B011', 'PCT013B012', 'PCT013B013', 'PCT013B014',
                            'PCT013B015', 'PCT013B016', 'PCT013B017', 'PCT013B018', 'PCT013B019', 'PCT013B020', 'PCT013B021', 'PCT013B022', 'PCT013B023',
                            'PCT013B024', 'PCT013B025', 'PCT013C003', 'PCT013C004', 'PCT013C005', 'PCT013C006', 'PCT013C007',
                            'PCT013C008', 'PCT013C009', 'PCT013C010', 'PCT013C011', 'PCT013C012','PCT013C013', 'PCT013C014', 'PCT013C015',
                            'PCT013C016', 'PCT013C017', 'PCT013C018', 'PCT013C019', 'PCT013C020', 'PCT013C021', 'PCT013C022', 'PCT013C023', 'PCT013C024', 'PCT013C025', 'PCT013C027',
                            'PCT013C028', 'PCT013C029', 'PCT013C030', 'PCT013C031', 'PCT013C032', 'PCT013C033', 'PCT013C034', 'PCT013C035',
                            'PCT013C036', 'PCT013C037', 'PCT013C038', 'PCT013C039', 'PCT013C040', 'PCT013C041', 'PCT013C042', 'PCT013C043',
                            'PCT013C044', 'PCT013C045', 'PCT013C046', 'PCT013C047', 'PCT013C048', 'PCT013C049',
                            'PCT013D003', 'PCT013D004', 'PCT013D005',
                            'PCT013D006', 'PCT013D007', 'PCT013D008', 'PCT013D009', 'PCT013D010', 'PCT013D011', 'PCT013D012', 'PCT013D013',
                            'PCT013D014', 'PCT013D015', 'PCT013D016', 'PCT013D017', 'PCT013D018', 'PCT013D019', 'PCT013D020', 'PCT013D021',
                            'PCT013D022', 'PCT013D023', 'PCT013D024', 'PCT013D025', 'PCT013D027', 'PCT013D028', 'PCT013D029', 'PCT013D030',
                            'PCT013D031', 'PCT013D032', 'PCT013D033', 'PCT013D034', 'PCT013D035', 'PCT013D036', 'PCT013D037', 'PCT013D038',
                            'PCT013D039', 'PCT013D040', 'PCT013D041', 'PCT013D042', 'PCT013D043', 'PCT013D044', 'PCT013D045', 'PCT013D046',
                            'PCT013D047', 'PCT013D048', 'PCT013D049', 'PCT013E003', 'PCT013E004',
                            'PCT013E005', 'PCT013E006', 'PCT013E007', 'PCT013E008', 'PCT013E009', 'PCT013E010', 'PCT013E011', 'PCT013E012',
                            'PCT013E013', 'PCT013E014', 'PCT013E015', 'PCT013E016', 'PCT013E017', 'PCT013E018', 'PCT013E019', 'PCT013E020',
                            'PCT013E021', 'PCT013E022', 'PCT013E023', 'PCT013E024', 'PCT013E025', 'PCT013E027', 'PCT013E028', 'PCT013E029',
                            'PCT013E030', 'PCT013E031', 'PCT013E032', 'PCT013E033', 'PCT013E034', 'PCT013E035', 'PCT013E036', 'PCT013E037',
                            'PCT013E038', 'PCT013E039', 'PCT013E040', 'PCT013E041', 'PCT013E042', 'PCT013E043', 'PCT013E044', 'PCT013E045',
                            'PCT013E046', 'PCT013E047', 'PCT013E048', 'PCT013E049', 'PCT013F003', 'PCT013F004', 'PCT013F005', 'PCT013F006',
                            'PCT013F007', 'PCT013F008', 'PCT013F009', 'PCT013F010', 'PCT013F011', 'PCT013F012', 'PCT013F013', 'PCT013F014',
                            'PCT013F015', 'PCT013F016', 'PCT013F017', 'PCT013F018', 'PCT013F019', 'PCT013F020', 'PCT013F021', 'PCT013F022',
                            'PCT013F023', 'PCT013F024', 'PCT013F025', 'PCT013F027', 'PCT013F028', 'PCT013F029', 'PCT013F030', 'PCT013F031',
                            'PCT013F032', 'PCT013F033', 'PCT013F034', 'PCT013F035', 'PCT013F036', 'PCT013F037', 'PCT013F038', 'PCT013F039',
                            'PCT013F040', 'PCT013F041', 'PCT013F042', 'PCT013F043', 'PCT013F044', 'PCT013F045', 'PCT013F046', 'PCT013F047',
                            'PCT013F048', 'PCT013F049', 'PCT013G003', 'PCT013G004', 'PCT013G005', 'PCT013G006', 'PCT013G007', 'PCT013G008',
                            'PCT013G009', 'PCT013G010', 'PCT013G011', 'PCT013G012', 'PCT013G013', 'PCT013G014', 'PCT013G015', 'PCT013G016',
                            'PCT013G017', 'PCT013G018', 'PCT013G019', 'PCT013G020', 'PCT013G021', 'PCT013G022', 'PCT013G023', 'PCT013G024',
                            'PCT013G025', 'PCT013G027', 'PCT013G028', 'PCT013G029', 'PCT013G030', 'PCT013G031', 'PCT013G032', 'PCT013G033',
                            'PCT013G034', 'PCT013G035', 'PCT013G036', 'PCT013G037', 'PCT013G038', 'PCT013G039', 'PCT013G040', 'PCT013G041',
                            'PCT013G042', 'PCT013G043', 'PCT013G044', 'PCT013G045', 'PCT013G046', 'PCT013G047', 'PCT013G048', 'PCT013G049',
                            'PCT013H003', 'PCT013H004', 'PCT013H005', 'PCT013H006', 'PCT013H007', 'PCT013H008', 'PCT013H009', 'PCT013H010',
                            'PCT013H011', 'PCT013H012', 'PCT013H013', 'PCT013H014', 'PCT013H015', 'PCT013H016', 'PCT013H017', 'PCT013H018',
                            'PCT013H019', 'PCT013H020', 'PCT013H021', 'PCT013H022', 'PCT013H023', 'PCT013H024', 'PCT013H025', 'PCT013H027',
                            'PCT013H028', 'PCT013H029', 'PCT013H030', 'PCT013H031', 'PCT013H032', 'PCT013H033', 'PCT013H034', 'PCT013H035',
                            'PCT013H036', 'PCT013H037', 'PCT013H038', 'PCT013H039', 'PCT013H040', 'PCT013H041', 'PCT013H042', 'PCT013H043',
                            'PCT013H044', 'PCT013H045', 'PCT013H046', 'PCT013H047', 'PCT013H048', 'PCT013H049', 'PCT013I003', 'PCT013I004',
                            'PCT013I005', 'PCT013I006', 'PCT013I007', 'PCT013I008', 'PCT013I009', 'PCT013I010', 'PCT013I011', 'PCT013I012',
                            'PCT013I013', 'PCT013I014', 'PCT013I015', 'PCT013I016', 'PCT013I017', 'PCT013I018', 'PCT013I019', 'PCT013I020',
                            'PCT013I021', 'PCT013I022', 'PCT013I023', 'PCT013I024', 'PCT013I025', 'PCT013I027', 'PCT013I028', 'PCT013I029',
                            'PCT013I030', 'PCT013I031', 'PCT013I032', 'PCT013I033', 'PCT013I034', 'PCT013I035', 'PCT013I036', 'PCT013I037',
                            'PCT013I038', 'PCT013I039', 'PCT013I040', 'PCT013I041', 'PCT013I042', 'PCT013I043', 'PCT013I044', 'PCT013I045',
                            'PCT013I046', 'PCT013I047', 'PCT013I048', 'PCT013I049',
                            'PCT013B003', 'PCT013B004', 'PCT013B005', 'PCT013B027', 'PCT013B028', 'PCT013B029',
                            'PCT013B030', 'PCT013B031', 'PCT013B032', 'PCT013B033', 'PCT013B034', 'PCT013B035', 'PCT013B036', 'PCT013B037',
                            'PCT013B038', 'PCT013B039', 'PCT013B040', 'PCT013B041', 'PCT013B042', 'PCT013B043', 'PCT013B044', 'PCT013B045',
                            'PCT013B046', 'PCT013B047', 'PCT013B048', 'PCT013B049', "COUNTY", "NAME"
                            
    )
    
    totpopvars <- c(
      'P012B003',  'P012B004',  'P012B005',  'P012B006',  'P012B007',  'P012B008',  'P012B009',  'P012B010',
      'P012B011',  'P012B012',  'P012B013',  'P012B014',  'P012B015',  'P012B016',  'P012B017',  'P012B018',
      'P012B019',  'P012B020',  'P012B021',  'P012B022',  'P012B023',  'P012B024',  'P012B025',  
      'P012B027',  'P012B028',  'P012B029',  'P012B030',  'P012B031',  'P012B032',  'P012B033',  'P012B034',
      'P012B035',  'P012B036',  'P012B037',  'P012B038',  'P012B039',  'P012B040',  'P012B041',  'P012B042',
      'P012B043',  'P012B044',  'P012B045',  'P012B046',  'P012B047',  'P012B048',  'P012B049',
      'P012C003',  'P012C004',  'P012C005',  'P012C006',  'P012C007',  'P012C008',  'P012C009',  'P012C010',
      'P012C011',  'P012C012',  'P012C013',  'P012C014',  'P012C015',  'P012C016',  'P012C017',  'P012C018',
      'P012C019',  'P012C020',  'P012C021',  'P012C022',  'P012C023',  'P012C024',  'P012C025',  
      'P012C027',  'P012C028',  'P012C029',  'P012C030',  'P012C031',  'P012C032',  'P012C033',  'P012C034',
      'P012C035',  'P012C036',  'P012C037',  'P012C038',  'P012C039',  'P012C040',  'P012C041',  'P012C042',
      'P012C043',  'P012C044',  'P012C045',  'P012C046',  'P012C047',  'P012C048',  'P012C049',  'P012D003',
      'P012D004',  'P012D005',  'P012D006',  'P012D007',  'P012D008',  'P012D009',  'P012D010',  'P012D011',
      'P012D012',  'P012D013',  'P012D014',  'P012D015',  'P012D016',  'P012D017',  'P012D018',  'P012D019',
      'P012D020',  'P012D021',  'P012D022',  'P012D023',  'P012D024',  'P012D025',    'P012D027',
      'P012D028',  'P012D029',  'P012D030',  'P012D031',  'P012D032',  'P012D033',  'P012D034',  'P012D035',
      'P012D036',  'P012D037',  'P012D038',  'P012D039',  'P012D040',  'P012D041',  'P012D042',  'P012D043',
      'P012D044',  'P012D045',  'P012D046',  'P012D047',  'P012D048',  'P012D049',  'P012E003',  'P012E004',
      'P012E005',  'P012E006',  'P012E007',  'P012E008',  'P012E009',  'P012E010',  'P012E011',  'P012E012',
      'P012E013',  'P012E014',  'P012E015',  'P012E016',  'P012E017',  'P012E018',  'P012E019',  'P012E020',
      'P012E021',  'P012E022',  'P012E023',  'P012E024',  'P012E025',    'P012E027',  'P012E028',
      'P012E029',  'P012E030',  'P012E031',  'P012E032',  'P012E033',  'P012E034',  'P012E035',  'P012E036',
      'P012E037',  'P012E038',  'P012E039',  'P012E040',  'P012E041',  'P012E042',  'P012E043',  'P012E044',
      'P012E045',  'P012E046',  'P012E047',  'P012E048',  'P012E049',  'P012F003',  'P012F004',  'P012F005',
      'P012F006',  'P012F007',  'P012F008',  'P012F009',  'P012F010',  'P012F011',  'P012F012',  'P012F013',
      'P012F014',  'P012F015',  'P012F016',  'P012F017',  'P012F018',  'P012F019',  'P012F020',  'P012F021',
      'P012F022',  'P012F023',  'P012F024',  'P012F025',    'P012F027',  'P012F028',  'P012F029',
      'P012F030',  'P012F031',  'P012F032',  'P012F033',  'P012F034',  'P012F035',  'P012F036',  'P012F037',
      'P012F038',  'P012F039',  'P012F040',  'P012F041',  'P012F042',  'P012F043',  'P012F044',  'P012F045',
      'P012F046',  'P012F047',  'P012F048',  'P012F049',  'P012G003',  'P012G004',  'P012G005',  'P012G006',
      'P012G007',  'P012G008',  'P012G009',  'P012G010',  'P012G011',  'P012G012',  'P012G013',  'P012G014',
      'P012G015',  'P012G016',  'P012G017',  'P012G018',  'P012G019',  'P012G020',  'P012G021',  'P012G022',
      'P012G023',  'P012G024',  'P012G025',    'P012G027',  'P012G028',  'P012G029',  'P012G030',
      'P012G031',  'P012G032',  'P012G033',  'P012G034',  'P012G035',  'P012G036',  'P012G037',  'P012G038',
      'P012G039',  'P012G040',  'P012G041',  'P012G042',  'P012G043',  'P012G044',  'P012G045',  'P012G046',
      'P012G047',  'P012G048',  'P012G049',  'P012H003',  'P012H004',  'P012H005',  'P012H006',  'P012H007',
      'P012H008',  'P012H009',  'P012H010',  'P012H011',  'P012H012',  'P012H013',  'P012H014',  'P012H015',
      'P012H016',  'P012H017',  'P012H018',  'P012H019',  'P012H020',  'P012H021',  'P012H022',  'P012H023',
      'P012H024',  'P012H025',    'P012H027',  'P012H028',  'P012H029',  'P012H030',  'P012H031',
      'P012H032',  'P012H033',  'P012H034',  'P012H035',  'P012H036',  'P012H037',  'P012H038',  'P012H039',
      'P012H040',  'P012H041',  'P012H042',  'P012H043',  'P012H044',  'P012H045',  'P012H046',  'P012H047',
      'P012H048',  'P012H049',  'P012I003',  'P012I004',  'P012I005',  'P012I006',  'P012I007',  'P012I008',
      'P012I009',  'P012I010',  'P012I011',  'P012I012',  'P012I013',  'P012I014',  'P012I015',  'P012I016',
      'P012I017',  'P012I018',  'P012I019',  'P012I020',  'P012I021',  'P012I022',  'P012I023',  'P012I024',
      'P012I025',  'P012I027',  'P012I028',  'P012I029',  'P012I030',  'P012I031',  'P012I032',
      'P012I033',  'P012I034',  'P012I035',  'P012I036',  'P012I037',  'P012I038',  'P012I039',  'P012I040',
      'P012I041',  'P012I042',  'P012I043',  'P012I044',  'P012I045',  'P012I046',  'P012I047',  'P012I048',
      'P012I049',"COUNTY", "NAME"
    )
    
    
    # Getting the census data from the API
    totpop <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = "2010", # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = totpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x))  %>%
      gather(name, TOTAL, P012B003:P012I049) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 5, 5)) %>%
      separate(label, c("Sex", "Other"), sep = ":") %>%
      separate(Other, c("drop", "pAge"), sep = "! ") %>%
      separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ "0",
        Age == "18 and 19 years" ~ "15",
        Age == "20 years" ~ "20",
        Age == "21 years" ~ "20",
        Age == "22" ~ "20",
        Age == "60 and 61 years" ~ "60",
        Age == "62" ~ "60",
        Age == "65 and 66 years" ~ "65",
        Age == "67" ~ "65",
        Age == "85 years and over" ~ "85",
        TRUE ~ as.character(Age)),
        Race = case_when(
          Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("C", "D", "E", "F", "G") ~ "OTHER, NH",
          Racecode == "H" ~ "HISPANIC",
          Racecode == "I" ~ "WHITE, NH"
        ))  %>%
      select(-drop, -Drop, -concept, -Racecode, -name)  %>%
      group_by(state, county, NAME, Sex, Race, Age) %>%
      dplyr::summarise(TOTAL = sum(TOTAL))
    
    hhpopp <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = "2010", # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = occupied_hhpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x)) %>%
      gather(name, HHPOP, PCT013B006:PCT013B049) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 7, 7)) %>%
      separate(label, c("Sex", "Other"), sep = ":") %>%
      separate(Other, c("drop", "pAge"), sep = "! ") %>%
      separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ "0",
        Age == "18 and 19 years" ~ "15",
        Age == "20 years" ~ "20",
        Age == "21 years" ~ "20",
        Age == "22" ~ "20",
        Age == "60 and 61 years" ~ "60",
        Age == "62" ~ "60",
        Age == "65 and 66 years" ~ "65",
        Age == "67" ~ "65",
        Age == "85 years and over" ~ "85",
        TRUE ~ as.character(Age)),
        Race = case_when(
          Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("C", "D", "E", "F", "G") ~ "OTHER, NH",
          Racecode == "H" ~ "HISPANIC",
          Racecode == "I" ~ "WHITE, NH"
        ))  %>%
      select(-drop, -Drop, -concept, -Racecode, -name) %>%
      group_by(state, county, NAME, Sex, Race, Age) %>%
      dplyr::summarise(HHPOP = sum(HHPOP))
    
    joined <- left_join(totpop, hhpopp) %>%
      ungroup() %>%
      mutate(GQ = TOTAL - HHPOP,
             Age = as.numeric(Age),
             YEAR = 2010,
             STATE = as.numeric(state),
             COUNTY = as.numeric(county),
             SEX = case_when(
               Sex == "Female" ~ "FEMALE",
               Sex == "Male" ~ "MALE"
             ),
             AGEGRP = case_when(
               Age == 0 ~ 1,
               Age == 5 ~ 2,
               Age == 10 ~ 3,
               Age == 15~ 4,
               Age == 20 ~ 5,
               Age == 25 ~ 6,
               Age == 30 ~ 7,
               Age == 35 ~ 8,
               Age == 40 ~ 9,
               Age == 45 ~ 10,
               Age == 50 ~ 11,
               Age == 55 ~ 12,
               Age == 60 ~ 13,
               Age == 65 ~ 14,
               Age == 70 ~ 15,
               Age == 75 ~ 16,
               Age == 80 ~ 17,
               Age == 85 ~ 18
             )) %>%
      separate(NAME, "County", sep = " County") %>%
      rename(RACE = Race) %>%
      select(-state, -county, -County, -Sex, -Age, -TOTAL, -HHPOP)
    
    joined2 <- rbind(joined, mutate(joined, YEAR = 2011)) %>%
      rbind(., mutate(joined, YEAR = 2012)) %>%
      rbind(., mutate(joined, YEAR = 2013)) %>%
      rbind(., mutate(joined, YEAR = 2014)) %>%
      rbind(., mutate(joined, YEAR = 2015)) %>%
      rbind(., mutate(joined, YEAR = 2016))
    
    return(joined2)
  }
  
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
baseyear = 2000
list <- listCensusMetadata(name = "sf1", vintage = baseyear, type ="variables")
getgq_2000 = function(x){
  tryCatch({
    
    # listing the available census variables in the population estimates agegroups data file
    
    occupied_hhpopvars <- c('PCT013B006', 'PCT013B007', 'PCT013B008', 'PCT013B009', 'PCT013B010', 'PCT013B011', 'PCT013B012', 'PCT013B013', 'PCT013B014',
                            'PCT013B015', 'PCT013B016', 'PCT013B017', 'PCT013B018', 'PCT013B019', 'PCT013B020', 'PCT013B021', 'PCT013B022', 'PCT013B023',
                            'PCT013B024', 'PCT013B025', 'PCT013C003', 'PCT013C004', 'PCT013C005', 'PCT013C006', 'PCT013C007',
                            'PCT013C008', 'PCT013C009', 'PCT013C010', 'PCT013C011', 'PCT013C012','PCT013C013', 'PCT013C014', 'PCT013C015',
                            'PCT013C016', 'PCT013C017', 'PCT013C018', 'PCT013C019', 'PCT013C020', 'PCT013C021', 'PCT013C022', 'PCT013C023', 'PCT013C024', 'PCT013C025', 'PCT013C027',
                            'PCT013C028', 'PCT013C029', 'PCT013C030', 'PCT013C031', 'PCT013C032', 'PCT013C033', 'PCT013C034', 'PCT013C035',
                            'PCT013C036', 'PCT013C037', 'PCT013C038', 'PCT013C039', 'PCT013C040', 'PCT013C041', 'PCT013C042', 'PCT013C043',
                            'PCT013C044', 'PCT013C045', 'PCT013C046', 'PCT013C047', 'PCT013C048', 'PCT013C049',
                            'PCT013D003', 'PCT013D004', 'PCT013D005',
                            'PCT013D006', 'PCT013D007', 'PCT013D008', 'PCT013D009', 'PCT013D010', 'PCT013D011', 'PCT013D012', 'PCT013D013',
                            'PCT013D014', 'PCT013D015', 'PCT013D016', 'PCT013D017', 'PCT013D018', 'PCT013D019', 'PCT013D020', 'PCT013D021',
                            'PCT013D022', 'PCT013D023', 'PCT013D024', 'PCT013D025', 'PCT013D027', 'PCT013D028', 'PCT013D029', 'PCT013D030',
                            'PCT013D031', 'PCT013D032', 'PCT013D033', 'PCT013D034', 'PCT013D035', 'PCT013D036', 'PCT013D037', 'PCT013D038',
                            'PCT013D039', 'PCT013D040', 'PCT013D041', 'PCT013D042', 'PCT013D043', 'PCT013D044', 'PCT013D045', 'PCT013D046',
                            'PCT013D047', 'PCT013D048', 'PCT013D049', 'PCT013E003', 'PCT013E004',
                            'PCT013E005', 'PCT013E006', 'PCT013E007', 'PCT013E008', 'PCT013E009', 'PCT013E010', 'PCT013E011', 'PCT013E012',
                            'PCT013E013', 'PCT013E014', 'PCT013E015', 'PCT013E016', 'PCT013E017', 'PCT013E018', 'PCT013E019', 'PCT013E020',
                            'PCT013E021', 'PCT013E022', 'PCT013E023', 'PCT013E024', 'PCT013E025', 'PCT013E027', 'PCT013E028', 'PCT013E029',
                            'PCT013E030', 'PCT013E031', 'PCT013E032', 'PCT013E033', 'PCT013E034', 'PCT013E035', 'PCT013E036', 'PCT013E037',
                            'PCT013E038', 'PCT013E039', 'PCT013E040', 'PCT013E041', 'PCT013E042', 'PCT013E043', 'PCT013E044', 'PCT013E045',
                            'PCT013E046', 'PCT013E047', 'PCT013E048', 'PCT013E049', 'PCT013F003', 'PCT013F004', 'PCT013F005', 'PCT013F006',
                            'PCT013F007', 'PCT013F008', 'PCT013F009', 'PCT013F010', 'PCT013F011', 'PCT013F012', 'PCT013F013', 'PCT013F014',
                            'PCT013F015', 'PCT013F016', 'PCT013F017', 'PCT013F018', 'PCT013F019', 'PCT013F020', 'PCT013F021', 'PCT013F022',
                            'PCT013F023', 'PCT013F024', 'PCT013F025', 'PCT013F027', 'PCT013F028', 'PCT013F029', 'PCT013F030', 'PCT013F031',
                            'PCT013F032', 'PCT013F033', 'PCT013F034', 'PCT013F035', 'PCT013F036', 'PCT013F037', 'PCT013F038', 'PCT013F039',
                            'PCT013F040', 'PCT013F041', 'PCT013F042', 'PCT013F043', 'PCT013F044', 'PCT013F045', 'PCT013F046', 'PCT013F047',
                            'PCT013F048', 'PCT013F049', 'PCT013G003', 'PCT013G004', 'PCT013G005', 'PCT013G006', 'PCT013G007', 'PCT013G008',
                            'PCT013G009', 'PCT013G010', 'PCT013G011', 'PCT013G012', 'PCT013G013', 'PCT013G014', 'PCT013G015', 'PCT013G016',
                            'PCT013G017', 'PCT013G018', 'PCT013G019', 'PCT013G020', 'PCT013G021', 'PCT013G022', 'PCT013G023', 'PCT013G024',
                            'PCT013G025', 'PCT013G027', 'PCT013G028', 'PCT013G029', 'PCT013G030', 'PCT013G031', 'PCT013G032', 'PCT013G033',
                            'PCT013G034', 'PCT013G035', 'PCT013G036', 'PCT013G037', 'PCT013G038', 'PCT013G039', 'PCT013G040', 'PCT013G041',
                            'PCT013G042', 'PCT013G043', 'PCT013G044', 'PCT013G045', 'PCT013G046', 'PCT013G047', 'PCT013G048', 'PCT013G049',
                            'PCT013H003', 'PCT013H004', 'PCT013H005', 'PCT013H006', 'PCT013H007', 'PCT013H008', 'PCT013H009', 'PCT013H010',
                            'PCT013H011', 'PCT013H012', 'PCT013H013', 'PCT013H014', 'PCT013H015', 'PCT013H016', 'PCT013H017', 'PCT013H018',
                            'PCT013H019', 'PCT013H020', 'PCT013H021', 'PCT013H022', 'PCT013H023', 'PCT013H024', 'PCT013H025', 'PCT013H027',
                            'PCT013H028', 'PCT013H029', 'PCT013H030', 'PCT013H031', 'PCT013H032', 'PCT013H033', 'PCT013H034', 'PCT013H035',
                            'PCT013H036', 'PCT013H037', 'PCT013H038', 'PCT013H039', 'PCT013H040', 'PCT013H041', 'PCT013H042', 'PCT013H043',
                            'PCT013H044', 'PCT013H045', 'PCT013H046', 'PCT013H047', 'PCT013H048', 'PCT013H049', 'PCT013I003', 'PCT013I004',
                            'PCT013I005', 'PCT013I006', 'PCT013I007', 'PCT013I008', 'PCT013I009', 'PCT013I010', 'PCT013I011', 'PCT013I012',
                            'PCT013I013', 'PCT013I014', 'PCT013I015', 'PCT013I016', 'PCT013I017', 'PCT013I018', 'PCT013I019', 'PCT013I020',
                            'PCT013I021', 'PCT013I022', 'PCT013I023', 'PCT013I024', 'PCT013I025', 'PCT013I027', 'PCT013I028', 'PCT013I029',
                            'PCT013I030', 'PCT013I031', 'PCT013I032', 'PCT013I033', 'PCT013I034', 'PCT013I035', 'PCT013I036', 'PCT013I037',
                            'PCT013I038', 'PCT013I039', 'PCT013I040', 'PCT013I041', 'PCT013I042', 'PCT013I043', 'PCT013I044', 'PCT013I045',
                            'PCT013I046', 'PCT013I047', 'PCT013I048', 'PCT013I049',
                            'PCT013B003', 'PCT013B004', 'PCT013B005', 'PCT013B027', 'PCT013B028', 'PCT013B029',
                            'PCT013B030', 'PCT013B031', 'PCT013B032', 'PCT013B033', 'PCT013B034', 'PCT013B035', 'PCT013B036', 'PCT013B037',
                            'PCT013B038', 'PCT013B039', 'PCT013B040', 'PCT013B041', 'PCT013B042', 'PCT013B043', 'PCT013B044', 'PCT013B045',
                            'PCT013B046', 'PCT013B047', 'PCT013B048', 'PCT013B049', "COUNTY", "NAME"
                            
    )
    
    totpopvars <- c(
      'P012B003',  'P012B004',  'P012B005',  'P012B006',  'P012B007',  'P012B008',  'P012B009',  'P012B010',
      'P012B011',  'P012B012',  'P012B013',  'P012B014',  'P012B015',  'P012B016',  'P012B017',  'P012B018',
      'P012B019',  'P012B020',  'P012B021',  'P012B022',  'P012B023',  'P012B024',  'P012B025',  
      'P012B027',  'P012B028',  'P012B029',  'P012B030',  'P012B031',  'P012B032',  'P012B033',  'P012B034',
      'P012B035',  'P012B036',  'P012B037',  'P012B038',  'P012B039',  'P012B040',  'P012B041',  'P012B042',
      'P012B043',  'P012B044',  'P012B045',  'P012B046',  'P012B047',  'P012B048',  'P012B049',
      'P012C003',  'P012C004',  'P012C005',  'P012C006',  'P012C007',  'P012C008',  'P012C009',  'P012C010',
      'P012C011',  'P012C012',  'P012C013',  'P012C014',  'P012C015',  'P012C016',  'P012C017',  'P012C018',
      'P012C019',  'P012C020',  'P012C021',  'P012C022',  'P012C023',  'P012C024',  'P012C025',  
      'P012C027',  'P012C028',  'P012C029',  'P012C030',  'P012C031',  'P012C032',  'P012C033',  'P012C034',
      'P012C035',  'P012C036',  'P012C037',  'P012C038',  'P012C039',  'P012C040',  'P012C041',  'P012C042',
      'P012C043',  'P012C044',  'P012C045',  'P012C046',  'P012C047',  'P012C048',  'P012C049',  'P012D003',
      'P012D004',  'P012D005',  'P012D006',  'P012D007',  'P012D008',  'P012D009',  'P012D010',  'P012D011',
      'P012D012',  'P012D013',  'P012D014',  'P012D015',  'P012D016',  'P012D017',  'P012D018',  'P012D019',
      'P012D020',  'P012D021',  'P012D022',  'P012D023',  'P012D024',  'P012D025',    'P012D027',
      'P012D028',  'P012D029',  'P012D030',  'P012D031',  'P012D032',  'P012D033',  'P012D034',  'P012D035',
      'P012D036',  'P012D037',  'P012D038',  'P012D039',  'P012D040',  'P012D041',  'P012D042',  'P012D043',
      'P012D044',  'P012D045',  'P012D046',  'P012D047',  'P012D048',  'P012D049',  'P012E003',  'P012E004',
      'P012E005',  'P012E006',  'P012E007',  'P012E008',  'P012E009',  'P012E010',  'P012E011',  'P012E012',
      'P012E013',  'P012E014',  'P012E015',  'P012E016',  'P012E017',  'P012E018',  'P012E019',  'P012E020',
      'P012E021',  'P012E022',  'P012E023',  'P012E024',  'P012E025',    'P012E027',  'P012E028',
      'P012E029',  'P012E030',  'P012E031',  'P012E032',  'P012E033',  'P012E034',  'P012E035',  'P012E036',
      'P012E037',  'P012E038',  'P012E039',  'P012E040',  'P012E041',  'P012E042',  'P012E043',  'P012E044',
      'P012E045',  'P012E046',  'P012E047',  'P012E048',  'P012E049',  'P012F003',  'P012F004',  'P012F005',
      'P012F006',  'P012F007',  'P012F008',  'P012F009',  'P012F010',  'P012F011',  'P012F012',  'P012F013',
      'P012F014',  'P012F015',  'P012F016',  'P012F017',  'P012F018',  'P012F019',  'P012F020',  'P012F021',
      'P012F022',  'P012F023',  'P012F024',  'P012F025',    'P012F027',  'P012F028',  'P012F029',
      'P012F030',  'P012F031',  'P012F032',  'P012F033',  'P012F034',  'P012F035',  'P012F036',  'P012F037',
      'P012F038',  'P012F039',  'P012F040',  'P012F041',  'P012F042',  'P012F043',  'P012F044',  'P012F045',
      'P012F046',  'P012F047',  'P012F048',  'P012F049',  'P012G003',  'P012G004',  'P012G005',  'P012G006',
      'P012G007',  'P012G008',  'P012G009',  'P012G010',  'P012G011',  'P012G012',  'P012G013',  'P012G014',
      'P012G015',  'P012G016',  'P012G017',  'P012G018',  'P012G019',  'P012G020',  'P012G021',  'P012G022',
      'P012G023',  'P012G024',  'P012G025',    'P012G027',  'P012G028',  'P012G029',  'P012G030',
      'P012G031',  'P012G032',  'P012G033',  'P012G034',  'P012G035',  'P012G036',  'P012G037',  'P012G038',
      'P012G039',  'P012G040',  'P012G041',  'P012G042',  'P012G043',  'P012G044',  'P012G045',  'P012G046',
      'P012G047',  'P012G048',  'P012G049',  'P012H003',  'P012H004',  'P012H005',  'P012H006',  'P012H007',
      'P012H008',  'P012H009',  'P012H010',  'P012H011',  'P012H012',  'P012H013',  'P012H014',  'P012H015',
      'P012H016',  'P012H017',  'P012H018',  'P012H019',  'P012H020',  'P012H021',  'P012H022',  'P012H023',
      'P012H024',  'P012H025',    'P012H027',  'P012H028',  'P012H029',  'P012H030',  'P012H031',
      'P012H032',  'P012H033',  'P012H034',  'P012H035',  'P012H036',  'P012H037',  'P012H038',  'P012H039',
      'P012H040',  'P012H041',  'P012H042',  'P012H043',  'P012H044',  'P012H045',  'P012H046',  'P012H047',
      'P012H048',  'P012H049',  'P012I003',  'P012I004',  'P012I005',  'P012I006',  'P012I007',  'P012I008',
      'P012I009',  'P012I010',  'P012I011',  'P012I012',  'P012I013',  'P012I014',  'P012I015',  'P012I016',
      'P012I017',  'P012I018',  'P012I019',  'P012I020',  'P012I021',  'P012I022',  'P012I023',  'P012I024',
      'P012I025',  'P012I027',  'P012I028',  'P012I029',  'P012I030',  'P012I031',  'P012I032',
      'P012I033',  'P012I034',  'P012I035',  'P012I036',  'P012I037',  'P012I038',  'P012I039',  'P012I040',
      'P012I041',  'P012I042',  'P012I043',  'P012I044',  'P012I045',  'P012I046',  'P012I047',  'P012I048',
      'P012I049',"COUNTY", "NAME"
    )
    
    
    # Getting the census data from the API
    totpop <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = paste0(baseyear), # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = totpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x))  %>%
      gather(name, TOTAL, P012B003:P012I049) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 5, 5),
             label= as.character(label)) %>%
      separate(label, c("Sex", "Other", "Age"), sep = ":") %>%
      #separate(Other, c("drop", "pAge"), sep = "! ") %>%
      #separate(Age, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "'<'5" ~ 1,
        Age == "5 to 9" ~ 2,
        Age == "10 to 14" ~ 3,
        Age == "15 to 17" ~ 4,
        Age == "18'&'19" ~ 4,
        Age == "20" ~ 5,
        Age == "21" ~ 5,
        Age == "22 to 24" ~ 5,
        Age == "25 to 29" ~ 6,
        Age == "30 to 34" ~ 7,
        Age == "35 to 39" ~ 8,
        Age == "40 to 44" ~ 9,
        Age == "45 to 49" ~ 10,
        Age == "50 to 54" ~ 11,
        Age == "55 to 59" ~ 12,
        Age == "60'&'61" ~ 13,
        Age == "62 to 64" ~ 13,
        Age == "65'&'66" ~ 14,
        Age == "67 to 69" ~ 14,
        Age == "70 to 74" ~ 15,
        Age == "" ~ 15,
        Age == "75 to 79" ~ 16,
        Age == "80 to 84" ~ 17,
        Age == "85 yrs'&'over" ~ 18),
        Race = case_when(
          Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("C", "D", "E", "F", "G") ~ "OTHER, NH",
          Racecode == "H" ~ "HISPANIC",
          Racecode == "I" ~ "WHITE, NH"
        ),
        SEX = case_when(
          Other == "Male" ~ "MALE",
          Other == "Female" ~ "FEMALE",
          Other == "Femalee" ~ "FEMALE"
        ))  %>%
      select(-concept, -Racecode, -name)  %>%
      group_by(state, county, NAME, SEX, Race, Age) %>%
      dplyr::summarise(TOTAL = sum(TOTAL))
    
    totpop[is.na(totpop)] <-18
    
    hhpopp <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = paste0(baseyear), # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = occupied_hhpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x)) %>%
      gather(name, HHPOP, PCT013B006:PCT013B049) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 7, 7)) %>%
      separate(label, c("Sex", "Other", "Age"), sep = ":") %>%
      # separate(Other, c("drop", "pAge"), sep = "! ") %>%
      # separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ 1,
        Age == "5 to 9 years" ~ 2,
        Age == "10 to 14 years" ~ 3,
        Age == "15 to 17 years" ~ 4,
        Age == "18 and 19 years" ~ 4,
        Age == "20 years" ~ 5,
        Age == "21 years" ~ 5,
        Age == "22 to 24 years" ~ 5,
        Age == "25 to 29 years" ~ 6,
        Age == "30 to 34 years" ~ 7,
        Age == "35 to 39 years" ~ 8,
        Age == "40 to 44 years" ~ 9,
        Age == "45 to 49 years" ~ 10,
        Age == "50 to 54 years" ~ 11,
        Age == "55 to 59 years" ~ 12,
        Age == "60 and 61 years" ~ 13,
        Age == "62 to 64 years" ~ 13,
        Age == "65 and 66 years" ~ 14,
        Age == "67 to 69 years" ~ 14,
        Age == "70 to 74 years" ~ 15,
        Age == "75 to 79 years" ~ 16,
        Age == "80 to 84 years" ~ 17,
        Age == "85 years and over" ~ 18),
        Race = case_when(
          Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("C", "D", "E", "F", "G") ~ "OTHER, NH",
          Racecode == "H" ~ "HISPANIC",
          Racecode == "I" ~ "WHITE, NH"
        ),
        SEX = case_when(
          Other == "Male" ~ "MALE",
          Other == "Female" ~ "FEMALE"
        ))  %>%
      select(-concept, -Racecode, -name) %>%
      group_by(state, county, NAME, SEX, Race, Age) %>%
      dplyr::summarise(HHPOP = sum(HHPOP))
    
    joined <- left_join(totpop, hhpopp) %>%
      ungroup() %>%
      mutate(GQ = TOTAL - HHPOP,
             Age = as.numeric(Age),
             YEAR = 2000,
             STATE = as.numeric(state),
             COUNTY = as.numeric(county)
      ) %>%
      separate(NAME, "County", sep = " County") %>%
      rename(RACE = Race,
             AGEGRP = Age) %>%
      select(-state, -county, -County, -TOTAL, -HHPOP)
    
    joined2 <- rbind(joined, mutate(joined, YEAR = baseyear)) %>%
      rbind(., mutate(joined, YEAR = baseyear+1)) %>%
      rbind(., mutate(joined, YEAR = baseyear+2)) %>%
      rbind(., mutate(joined, YEAR = baseyear+3)) %>%
      rbind(., mutate(joined, YEAR = baseyear+4)) %>%
      rbind(., mutate(joined, YEAR = baseyear+5)) %>%
      rbind(., mutate(joined, YEAR = baseyear+6)) %>%
      rbind(., mutate(joined, YEAR = baseyear+7)) %>%
      rbind(., mutate(joined, YEAR = baseyear+8)) %>%
      rbind(., mutate(joined, YEAR = baseyear+9))
    return(joined)
  }
  
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


getgq_1990 = function(x){
  tryCatch({
    baseyear = 1990
    # listing the available census variables in the population estimates agegroups data file
    list <- listCensusMetadata(name = "sf3", vintage = baseyear, type ="variables")
    occupied_hhpopvars <- c('PCT013B006', 'PCT013B007', 'PCT013B008', 'PCT013B009', 'PCT013B010', 'PCT013B011', 'PCT013B012', 'PCT013B013', 'PCT013B014',
                            'PCT013B015', 'PCT013B016', 'PCT013B017', 'PCT013B018', 'PCT013B019', 'PCT013B020', 'PCT013B021', 'PCT013B022', 'PCT013B023',
                            'PCT013B024', 'PCT013B025', 'PCT013C003', 'PCT013C004', 'PCT013C005', 'PCT013C006', 'PCT013C007',
                            'PCT013C008', 'PCT013C009', 'PCT013C010', 'PCT013C011', 'PCT013C012','PCT013C013', 'PCT013C014', 'PCT013C015',
                            'PCT013C016', 'PCT013C017', 'PCT013C018', 'PCT013C019', 'PCT013C020', 'PCT013C021', 'PCT013C022', 'PCT013C023', 'PCT013C024', 'PCT013C025', 'PCT013C027',
                            'PCT013C028', 'PCT013C029', 'PCT013C030', 'PCT013C031', 'PCT013C032', 'PCT013C033', 'PCT013C034', 'PCT013C035',
                            'PCT013C036', 'PCT013C037', 'PCT013C038', 'PCT013C039', 'PCT013C040', 'PCT013C041', 'PCT013C042', 'PCT013C043',
                            'PCT013C044', 'PCT013C045', 'PCT013C046', 'PCT013C047', 'PCT013C048', 'PCT013C049',
                            'PCT013D003', 'PCT013D004', 'PCT013D005',
                            'PCT013D006', 'PCT013D007', 'PCT013D008', 'PCT013D009', 'PCT013D010', 'PCT013D011', 'PCT013D012', 'PCT013D013',
                            'PCT013D014', 'PCT013D015', 'PCT013D016', 'PCT013D017', 'PCT013D018', 'PCT013D019', 'PCT013D020', 'PCT013D021',
                            'PCT013D022', 'PCT013D023', 'PCT013D024', 'PCT013D025', 'PCT013D027', 'PCT013D028', 'PCT013D029', 'PCT013D030',
                            'PCT013D031', 'PCT013D032', 'PCT013D033', 'PCT013D034', 'PCT013D035', 'PCT013D036', 'PCT013D037', 'PCT013D038',
                            'PCT013D039', 'PCT013D040', 'PCT013D041', 'PCT013D042', 'PCT013D043', 'PCT013D044', 'PCT013D045', 'PCT013D046',
                            'PCT013D047', 'PCT013D048', 'PCT013D049', 'PCT013E003', 'PCT013E004',
                            'PCT013E005', 'PCT013E006', 'PCT013E007', 'PCT013E008', 'PCT013E009', 'PCT013E010', 'PCT013E011', 'PCT013E012',
                            'PCT013E013', 'PCT013E014', 'PCT013E015', 'PCT013E016', 'PCT013E017', 'PCT013E018', 'PCT013E019', 'PCT013E020',
                            'PCT013E021', 'PCT013E022', 'PCT013E023', 'PCT013E024', 'PCT013E025', 'PCT013E027', 'PCT013E028', 'PCT013E029',
                            'PCT013E030', 'PCT013E031', 'PCT013E032', 'PCT013E033', 'PCT013E034', 'PCT013E035', 'PCT013E036', 'PCT013E037',
                            'PCT013E038', 'PCT013E039', 'PCT013E040', 'PCT013E041', 'PCT013E042', 'PCT013E043', 'PCT013E044', 'PCT013E045',
                            'PCT013E046', 'PCT013E047', 'PCT013E048', 'PCT013E049', 'PCT013F003', 'PCT013F004', 'PCT013F005', 'PCT013F006',
                            'PCT013F007', 'PCT013F008', 'PCT013F009', 'PCT013F010', 'PCT013F011', 'PCT013F012', 'PCT013F013', 'PCT013F014',
                            'PCT013F015', 'PCT013F016', 'PCT013F017', 'PCT013F018', 'PCT013F019', 'PCT013F020', 'PCT013F021', 'PCT013F022',
                            'PCT013F023', 'PCT013F024', 'PCT013F025', 'PCT013F027', 'PCT013F028', 'PCT013F029', 'PCT013F030', 'PCT013F031',
                            'PCT013F032', 'PCT013F033', 'PCT013F034', 'PCT013F035', 'PCT013F036', 'PCT013F037', 'PCT013F038', 'PCT013F039',
                            'PCT013F040', 'PCT013F041', 'PCT013F042', 'PCT013F043', 'PCT013F044', 'PCT013F045', 'PCT013F046', 'PCT013F047',
                            'PCT013F048', 'PCT013F049', 'PCT013G003', 'PCT013G004', 'PCT013G005', 'PCT013G006', 'PCT013G007', 'PCT013G008',
                            'PCT013G009', 'PCT013G010', 'PCT013G011', 'PCT013G012', 'PCT013G013', 'PCT013G014', 'PCT013G015', 'PCT013G016',
                            'PCT013G017', 'PCT013G018', 'PCT013G019', 'PCT013G020', 'PCT013G021', 'PCT013G022', 'PCT013G023', 'PCT013G024',
                            'PCT013G025', 'PCT013G027', 'PCT013G028', 'PCT013G029', 'PCT013G030', 'PCT013G031', 'PCT013G032', 'PCT013G033',
                            'PCT013G034', 'PCT013G035', 'PCT013G036', 'PCT013G037', 'PCT013G038', 'PCT013G039', 'PCT013G040', 'PCT013G041',
                            'PCT013G042', 'PCT013G043', 'PCT013G044', 'PCT013G045', 'PCT013G046', 'PCT013G047', 'PCT013G048', 'PCT013G049',
                            'PCT013H003', 'PCT013H004', 'PCT013H005', 'PCT013H006', 'PCT013H007', 'PCT013H008', 'PCT013H009', 'PCT013H010',
                            'PCT013H011', 'PCT013H012', 'PCT013H013', 'PCT013H014', 'PCT013H015', 'PCT013H016', 'PCT013H017', 'PCT013H018',
                            'PCT013H019', 'PCT013H020', 'PCT013H021', 'PCT013H022', 'PCT013H023', 'PCT013H024', 'PCT013H025', 'PCT013H027',
                            'PCT013H028', 'PCT013H029', 'PCT013H030', 'PCT013H031', 'PCT013H032', 'PCT013H033', 'PCT013H034', 'PCT013H035',
                            'PCT013H036', 'PCT013H037', 'PCT013H038', 'PCT013H039', 'PCT013H040', 'PCT013H041', 'PCT013H042', 'PCT013H043',
                            'PCT013H044', 'PCT013H045', 'PCT013H046', 'PCT013H047', 'PCT013H048', 'PCT013H049', 'PCT013I003', 'PCT013I004',
                            'PCT013I005', 'PCT013I006', 'PCT013I007', 'PCT013I008', 'PCT013I009', 'PCT013I010', 'PCT013I011', 'PCT013I012',
                            'PCT013I013', 'PCT013I014', 'PCT013I015', 'PCT013I016', 'PCT013I017', 'PCT013I018', 'PCT013I019', 'PCT013I020',
                            'PCT013I021', 'PCT013I022', 'PCT013I023', 'PCT013I024', 'PCT013I025', 'PCT013I027', 'PCT013I028', 'PCT013I029',
                            'PCT013I030', 'PCT013I031', 'PCT013I032', 'PCT013I033', 'PCT013I034', 'PCT013I035', 'PCT013I036', 'PCT013I037',
                            'PCT013I038', 'PCT013I039', 'PCT013I040', 'PCT013I041', 'PCT013I042', 'PCT013I043', 'PCT013I044', 'PCT013I045',
                            'PCT013I046', 'PCT013I047', 'PCT013I048', 'PCT013I049',
                            'PCT013B003', 'PCT013B004', 'PCT013B005', 'PCT013B027', 'PCT013B028', 'PCT013B029',
                            'PCT013B030', 'PCT013B031', 'PCT013B032', 'PCT013B033', 'PCT013B034', 'PCT013B035', 'PCT013B036', 'PCT013B037',
                            'PCT013B038', 'PCT013B039', 'PCT013B040', 'PCT013B041', 'PCT013B042', 'PCT013B043', 'PCT013B044', 'PCT013B045',
                            'PCT013B046', 'PCT013B047', 'PCT013B048', 'PCT013B049', "COUNTY", "NAME"
                            
    )
    
    totpopvars <- c(
      'P0120002', 	'P0120003', 	'P0120004', 	'P0120005', 	'P0120006', 	'P0120007', 	'P0120008', 	'P0120009', 	'P0120010', 	'P0120011', 
      'P0120012', 	'P0120013', 	'P0120014', 	'P0120015', 	'P0120016', 	'P0120017', 	'P0120018', 	'P0120019', 	'P0120020', 	'P0120021', 
      'P0120022', 	'P0120023', 	'P0120024', 	'P0120025', 	'P0120026', 	'P0120027', 	'P0120028', 	'P0120029', 	'P0120030', 	'P0120031', 
      'P0120033', 	'P0120034', 	'P0120035', 	'P0120036', 	'P0120037', 	'P0120038', 	'P0120039', 	'P0120040', 	'P0120041', 	'P0120042', 
      'P0120043', 	'P0120044', 	'P0120045', 	'P0120046', 	'P0120047', 	'P0120048', 	'P0120049', 	'P0120050', 	'P0120051', 	'P0120052', 
      'P0120053', 	'P0120054', 	'P0120055', 	'P0120056', 	'P0120057', 	'P0120058', 	'P0120059', 	'P0120060', 	'P0120061', 	'P0120062', 
      'P0120064', 	'P0120065', 	'P0120066', 	'P0120067', 	'P0120068', 	'P0120069', 	'P0120070', 	'P0120071', 	'P0120072', 	'P0120073', 
      'P0120074', 	'P0120075', 	'P0120076', 	'P0120077', 	'P0120078', 	'P0120079', 	'P0120080', 	'P0120081', 	'P0120082', 	'P0120083', 
      'P0120084', 	'P0120085', 	'P0120086', 	'P0120087', 	'P0120088', 	'P0120089', 	'P0120090', 	'P0120091', 	'P0120092', 	'P0120093', 
      'P0120095', 	'P0120096', 	'P0120097', 	'P0120098', 	'P0120099', 	'P0120100', 	'P0120101', 	'P0120102', 	'P0120103', 	'P0120104', 
      'P0120105', 	'P0120106', 	'P0120107', 	'P0120108', 	'P0120109', 	'P0120110', 	'P0120111', 	'P0120112', 	'P0120113', 	'P0120114', 
      'P0120115', 	'P0120116', 	'P0120117', 	'P0120118', 	'P0120119', 	'P0120120', 	'P0120121', 	'P0120122', 	'P0120123', 	'P0120124', 
      'P0120126', 	'P0120127', 	'P0120128', 	'P0120129', 	'P0120130', 	'P0120131', 	'P0120132', 	'P0120133', 	'P0120134', 	'P0120135', 
      'P0120136', 	'P0120137', 	'P0120138', 	'P0120139', 	'P0120140', 	'P0120141', 	'P0120142', 	'P0120143', 	'P0120144', 	'P0120145', 
      'P0120146', 	'P0120147', 	'P0120148', 	'P0120149', 	'P0120150', 	'P0120151', 	'P0120152', 	'P0120153', 	'P0120154', 	'P0120155', 
      'P0120157', 	'P0120158', 	'P0120159', 	'P0120160', 	'P0120161', 	'P0120162', 	'P0120163', 	'P0120164', 	'P0120165', 	'P0120166', 
      'P0120167', 	'P0120168', 	'P0120169', 	'P0120170', 	'P0120171', 	'P0120172', 	'P0120173', 	'P0120174', 	'P0120175', 	'P0120176', 
      'P0120177', 	'P0120178', 	'P0120179', 	'P0120180', 	'P0120181', 	'P0120182', 	'P0120183', 	'P0120184', 	'P0120185', 	'P0120186', 
      'P0120188', 	'P0120189', 	'P0120190', 	'P0120191', 	'P0120192', 	'P0120193', 	'P0120194', 	'P0120195', 	'P0120196', 	'P0120197', 
      'P0120198', 	'P0120199', 	'P0120200', 	'P0120201', 	'P0120202', 	'P0120203', 	'P0120204', 	'P0120205', 	'P0120206', 	'P0120207', 
      'P0120208', 	'P0120209', 	'P0120210', 	'P0120211', 	'P0120212', 	'P0120213', 	'P0120214', 	'P0120215', 	'P0120216', 	'P0120217', 
      'P0120219', 	'P0120220', 	'P0120221', 	'P0120222', 	'P0120223', 	'P0120224', 	'P0120225', 	'P0120226', 	'P0120227', 	'P0120228', 
      'P0120229', 	'P0120230', 	'P0120231', 	'P0120232', 	'P0120233', 	'P0120234', 	'P0120235', 	'P0120236', 	'P0120237', 	'P0120238', 
      'P0120239', 	'P0120240', 	'P0120241', 	'P0120242', 	'P0120243', 	'P0120244', 	'P0120245', 	'P0120246', 	'P0120247', 	'P0120248', 
      'P0120250', 	'P0120251', 	'P0120252', 	'P0120253', 	'P0120254', 	'P0120255', 	'P0120256', 	'P0120257', 	'P0120258', 	'P0120259', 
      'P0120260', 	'P0120261', 	'P0120262', 	'P0120263', 	'P0120264', 	'P0120265', 	'P0120266', 	'P0120267', 	'P0120268', 	'P0120269', 
      'P0120270', 	'P0120271', 	'P0120272', 	'P0120273', 	'P0120274', 	'P0120275', 	'P0120276', 	'P0120277', 	'P0120278', 	'P0120279', 
      'P0120281', 	'P0120282', 	'P0120283', 	'P0120284', 	'P0120285', 	'P0120286', 	'P0120287', 	'P0120288', 	'P0120289', 	'P0120290', 
      'P0120291', 	'P0120292', 	'P0120293', 	'P0120294', 	'P0120295', 	'P0120296', 	'P0120297', 	'P0120298', 	'P0120299', 	'P0120300', 
      'P0120301', 	'P0120302', 	'P0120303', 	'P0120304', 	'P0120305', 	'P0120306', 	'P0120307', 	'P0120308', 	'P0120309', 	'P0120310', 
      'P0130002', 	'P0130003', 	'P0130004', 	'P0130005', 	'P0130006', 	'P0130007', 	'P0130008', 	'P0130009', 	'P0130010', 	'P0130011', 
      'P0130012', 	'P0130013', 	'P0130014', 	'P0130015', 	'P0130016', 	'P0130017', 	'P0130018', 	'P0130019', 	'P0130020', 	'P0130021', 
      'P0130022', 	'P0130023', 	'P0130024', 	'P0130025', 	'P0130026', 	'P0130027', 	'P0130028', 	'P0130029', 	'P0130030', 	'P0130031', 
      'P0130033', 	'P0130034', 	'P0130035', 	'P0130036', 	'P0130037', 	'P0130038', 	'P0130039', 	'P0130040', 	'P0130041', 	'P0130042', 
      'P0130043', 	'P0130044', 	'P0130045', 	'P0130046', 	'P0130047', 	'P0130048', 	'P0130049', 	'P0130050', 	'P0130051', 	'P0130052', 
      'P0130053', 	'P0130054', 	'P0130055', 	'P0130056', 	'P0130057', 	'P0130058', 	'P0130059', 	'P0130060', 	'P0130061', 	'P0130062', 
      "CNTY", "ANPSADPI"
    )
    
    
    # Getting the census data from the API
    totpop <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = paste0(baseyear), # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = totpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x))  %>%
      gather(name, TOTAL, P0120002:P0130062) %>%
      left_join(., list) %>%
      mutate(SEX = case_when(
        grepl("Females", label) ~ "FEMALE",
        grepl("Males", label) ~ "MALE"
      ),
      RACE = case_when(
        grepl("White", label) ~ "WHITE, NH",
        grepl("Black", label) ~ "BLACK, NH",
        grepl("Hispanic", label) ~ "HISPANIC",
        !grepl("White|Black|Hispanic", label) ~ "OTHER, NH"
      ),
      Age = case_when(
        grepl("1-2|3-4", label) ~ 1,
        grepl("5 yrs|6 yrs|7-9", label) ~ 2,
        grepl("10-11|12-13|14", label) ~ 3,
        grepl("15|16|17|18|19", label) ~ 4,
        grepl("20|21|22-24", label) ~ 5,
        grepl("25-29", label) ~ 6,
        grepl("30-34", label) ~ 7,
        grepl("35-39", label) ~ 8,
        grepl("40-44", label) ~ 9,
        grepl("45-49", label) ~ 10,
        grepl("50-54", label) ~ 11,
        grepl("55-59", label) ~ 12,
        grepl("60-61|62-64", label) ~ 13,
        grepl("65-69", label) ~ 14,
        grepl("70-74", label) ~ 15,
        grepl("75-79", label) ~ 16,
        grepl("80-84", label) ~ 17,
        grepl("85+", label) ~ 18
      )) %>%
      #select(-concept, -Racecode, -name)  %>%
      group_by(state, county, SEX, RACE, Age) %>%
      dplyr::summarise(TOTAL = sum(TOTAL))
    
    hhpopp <- getCensus(name="sf1", # This is the Estimates datafile
                        vintage = paste0(baseyear), # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        vars = occupied_hhpopvars, # gathering these variables
                        region="COUNTY:*",
                        regionin=paste0("state:", x)) %>%
      gather(name, HHPOP, PCT013B006:PCT013B049) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 7, 7)) %>%
      separate(label, c("Sex", "Other", "Age"), sep = ":") %>%
      # separate(Other, c("drop", "pAge"), sep = "! ") %>%
      # separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ 1,
        Age == "5 to 9 years" ~ 2,
        Age == "10 to 14 years" ~ 3,
        Age == "15 to 17 years" ~ 4,
        Age == "18 and 19 years" ~ 4,
        Age == "20 years" ~ 5,
        Age == "21 years" ~ 5,
        Age == "22 to 24 years" ~ 5,
        Age == "25 to 29 years" ~ 6,
        Age == "30 to 34 years" ~ 7,
        Age == "35 to 39 years" ~ 8,
        Age == "40 to 44 years" ~ 9,
        Age == "45 to 49 years" ~ 10,
        Age == "50 to 54 years" ~ 11,
        Age == "55 to 59 years" ~ 12,
        Age == "60 and 61 years" ~ 13,
        Age == "62 to 64 years" ~ 13,
        Age == "65 and 66 years" ~ 14,
        Age == "67 to 69 years" ~ 14,
        Age == "70 to 74 years" ~ 15,
        Age == "75 to 79 years" ~ 16,
        Age == "80 to 84 years" ~ 17,
        Age == "85 years and over" ~ 18),
        Race = case_when(
          Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("C", "D", "E", "F", "G") ~ "OTHER, NH",
          Racecode == "H" ~ "HISPANIC",
          Racecode == "I" ~ "WHITE, NH"
        ),
        SEX = case_when(
          Other == "Male" ~ "MALE",
          Other == "Female" ~ "FEMALE"
        ))  %>%
      select(-concept, -Racecode, -name) %>%
      group_by(state, county, NAME, SEX, Race, Age) %>%
      dplyr::summarise(HHPOP = sum(HHPOP))
    
    joined <- left_join(totpop, hhpopp) %>%
      ungroup() %>%
      mutate(GQ = TOTAL - HHPOP,
             Age = as.numeric(Age),
             YEAR = 2000,
             STATE = as.numeric(state),
             COUNTY = as.numeric(county)
      ) %>%
      separate(NAME, "County", sep = " County") %>%
      rename(RACE = Race,
             AGEGRP = Age) %>%
      select(-state, -county, -County, -TOTAL, -HHPOP)
    
    joined2 <- rbind(joined, mutate(joined, YEAR = baseyear)) %>%
      rbind(., mutate(joined, YEAR = baseyear+1)) %>%
      rbind(., mutate(joined, YEAR = baseyear+2)) %>%
      rbind(., mutate(joined, YEAR = baseyear+3)) %>%
      rbind(., mutate(joined, YEAR = baseyear+4)) %>%
      rbind(., mutate(joined, YEAR = baseyear+5)) %>%
      rbind(., mutate(joined, YEAR = baseyear+6)) %>%
      rbind(., mutate(joined, YEAR = baseyear+7)) %>%
      rbind(., mutate(joined, YEAR = baseyear+8)) %>%
      rbind(., mutate(joined, YEAR = baseyear+9))
    return(joined2)
  }
  
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


dat <- pbmclapply(stateid, getgq_2010)
GQ2010 <- rbindlist(dat)
dat <- pblapply(stateid, getgq_2000)
GQ2000 <- rbindlist(dat)

write_csv(GQ2010, "DATA/gq_2010.csv")
write_csv(GQ2000, "DATA/gq_2000.csv")

z<- left_join(cendat, joined)