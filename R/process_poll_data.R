########################################################
##
## process CSV prior to analysis
## to be called from an Rmd file
## not for running solo
##
## simon jackman
## simon.jackman@sydney.edu.au
##
## 2022-05-18 21:58:54
########################################################


d2022 <- read_csv(file="~/Downloads/PollingData-5.csv")

## ----warning=FALSE------------------------------------------------------------
d2022 <- d2022 %>%
  mutate(SampleSize = ifelse(SampleSize=="30/04/1905",
                             1905,
                             as.numeric(SampleSize)))  %>%
  mutate(ALP_2PP = ifelse(ALP_2PP<1,ALP_2PP*100,ALP_2PP),
         LNP_2PP = ifelse(LNP_2PP<1,LNP_2PP*100,LNP_2PP)) %>%
  mutate(Pollster = str_squish(Pollster))


## -----------function to swap month and day if date is in format m/d/y---------------
fixDateTransposed <- function(z){
  require(lubridate)
  m <- month(z)
  y <- year(z)
  d <- day(z)
  badDates <- (m>5 & y==2022) | (z > Sys.Date() + 1) 
  badDates <- replace_na(badDates,FALSE)
  
  if(any(badDates)){
    ## assume badDates have month and day transposed
    print(z[badDates])
    month(z[badDates]) <- d[badDates]
    day(z[badDates]) <- m[badDates]
  }
  
  return(z)
}


d2022 <- d2022 %>%
  mutate(PublicationDate = as.Date(PublicationDate,format="%d/%m/%Y"),
         start_date=as.Date(FieldedStart,format="%d/%m/%Y"),
         end_date=as.Date(FieldedEnd,format="%d/%m/%Y"))

## fix weird/impossible dates
d2022 <- d2022 %>%
  mutate(PublicationDate = fixDateTransposed(PublicationDate),
         start_date = fixDateTransposed(start_date),
         end_date = fixDateTransposed(end_date)) %>%
  mutate(mid_date = as.Date(floor((as.numeric(start_date)+as.numeric(end_date))/2),
                            origin="1970/01/01")) %>%
  mutate(ALP_2PP = ifelse(ALP_2PP<1,ALP_2PP*100,ALP_2PP),
         LNP_2PP = ifelse(LNP_2PP<1,LNP_2PP*100,LNP_2PP)) 


## ----no fielddate-------------------------------------------------------------
d2022 <- d2022 %>%
  mutate(mid_date = ifelse(is.na(mid_date) & !is.na(PublicationDate),
                           PublicationDate - 3,
                           mid_date),
         mid_date = as.Date(mid_date,origin="1970/01/01"))

## ----normalise primaries where they don't sum to 100--------------------------
d2022 <- d2022 %>% 
  mutate(PHON = ifelse(is.na(PHON),0,PHON),
         UAP = ifelse(is.na(UAP),0,UAP),
         OTHER = ifelse(is.na(OTHER),0,OTHER)) %>%
  mutate(LNP = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,LNP/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,LNP),
         ALP = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,ALP/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,ALP),
         GRN = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,GRN/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,GRN),
         PHON = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,PHON/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,PHON),
         UAP = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,UAP/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,UAP),
         OTHER = ifelse(LNP+ALP+GRN+PHON+UAP+OTHER<100,OTHER/(LNP+ALP+GRN+PHON+UAP+OTHER)*100,OTHER))

## -----  compute non major party primary support
d2022 <- d2022 %>% mutate(NonMajor = 100-LNP-ALP)


## ----convert-2pp+-to-2pp------------------------------------------------------

d2022 <- d2022 %>%
  mutate(ALP_2PP = ifelse(is.na(ALP_2PP) & !is.na(ALP_2PPplus_Essential),
                          ALP_2PPplus_Essential/(ALP_2PPplus_Essential+LNP_2PPplus_Essential)*100,
                          ALP_2PP),
         LNP_2PP = ifelse(is.na(LNP_2PP) & !is.na(LNP_2PPplus_Essential),
                          100-ALP_2PP,
                          LNP_2PP))

## ----estimate 2pp from first preferences where no published 2pp---------------

d2022 <- d2022 %>%
  mutate(ALP_2PP = ifelse(is.na(ALP_2PP),
                          ALP + 
                            .822*ifelse(is.na(GRN),0,GRN) + 
                            .349*ifelse(is.na(UAP),0,UAP) + 
                            .348*ifelse(is.na(PHON),0,PHON) + 
                            .507*ifelse(is.na(OTHER),0,OTHER),
                          ALP_2PP),
         LNP_2PP = ifelse(is.na(LNP_2PP),
                          100-ALP_2PP,
                          LNP_2PP))






## ----fix UND------------------------------------------------------------------
d2022 <- d2022 %>%   
  mutate(ALL=ALP + LNP + GRN + 
           ifelse(is.na(PHON),0,PHON) +
           ifelse(is.na(UAP),0,UAP) +
           ifelse(is.na(OTHER),0,OTHER)
  ) %>%
  mutate(renorm_needed = ALL<99.5 & !is.na(UND)) %>%
  mutate(ALP = ifelse(renorm_needed,ALP/ALL*100,ALP),
         LNP = ifelse(renorm_needed,LNP/ALL*100,LNP),
         GRN = ifelse(renorm_needed,GRN/ALL*100,GRN),
         PHON = ifelse(renorm_needed,PHON/ALL*100,PHON),
         UAP = ifelse(renorm_needed,UAP/ALL*100,UAP),
         OTHER = ifelse(renorm_needed,OTHER/ALL*100,OTHER)) %>%
  mutate(ALL=ALP + LNP + GRN + 
           ifelse(is.na(PHON),0,PHON) +
           ifelse(is.na(UAP),0,UAP) +
           ifelse(is.na(OTHER),0,OTHER)
  ) 

## ----missing-sample-size------------------------------------------------------
avg_samp_size <- d2022 %>% 
  group_by(Pollster) %>% 
  summarise(n=mean(SampleSize,na.rm=TRUE)) %>%
  ungroup()

d2022 <- left_join(d2022,
                   avg_samp_size,
                   by="Pollster") %>%
  mutate(SampleSize = ifelse(is.na(SampleSize),n,SampleSize)) %>%
  select(-n)


## ----effective-sample-size----------------------------------------------------
avg_deflator <- d2022 %>% 
  summarise(avg_deflator = mean(EffectiveSampleSize/SampleSize,na.rm=TRUE)) %>% 
  pull(avg_deflator)

d2022 <- d2022 %>%
  mutate(EffectiveSampleSize = ifelse(is.na(EffectiveSampleSize),
                                      avg_deflator*SampleSize,
                                      EffectiveSampleSize))

d2022 <- d2022 %>%
  mutate(pollster_abbr = factor(Pollster,
                                levels=c("Essential","Ipsos","Resolve",
                                         "Roy Morgan","YouGov Galaxy"),
                                labels=c("ESS","IPS","RES","RM","YG")
                                )
         )
