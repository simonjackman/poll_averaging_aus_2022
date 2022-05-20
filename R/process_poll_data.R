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

library(tidyverse)
library(stringr)
library(lubridate)

d2022 <- readxl::read_xlsx("~/Downloads/PollingData-6.xlsx")

## ----warning=FALSE------------------------------------------------------------
d2022 <- d2022 %>%
  mutate(SampleSize = ifelse(SampleSize=="30/04/1905",
                             1905,
                             as.numeric(SampleSize)))  %>%
  mutate(ALP_2PP = ifelse(ALP_2PP<1,ALP_2PP*100,ALP_2PP),
         LNP_2PP = ifelse(LNP_2PP<1,LNP_2PP*100,LNP_2PP)) %>%
  mutate(Pollster = str_squish(Pollster))


## parseDateString
parseDateString <- function(str){
  require(stringr)
  
  out <- NULL
  if(is.na(str) | str==""){
    return(NA_Date_)
  }
  
  print(str)
  if(!grepl(str,pattern="/",fixed=TRUE)){
    str <- strftime(
        as.Date(as.numeric(str),
                origin=as.Date("1899-12-30")
                ),
        format="%m/%d/%Y"
        )
    print(str)
  }
  z <- as.numeric(str_split(str,pattern="/",simplify = TRUE))
  print(z)
  if(z[1]>12){
    ## this is d/m/y
    out <- as.Date(str,format="%d/%m/%Y")
  }
  if(z[2]>12){
    ## this is m/d/y
    out <- as.Date(str,format="%m/%d/%Y")
  }
  
  ## ambiguous, try this, hope for the best
  if(is.null(out)){
    out <- as.Date(str,format="%m/%d/%Y")
  }
  
  return(out)
}


d2022 <- d2022 %>%
  rowwise() %>%
  mutate(PublicationDate = parseDateString(PublicationDate),
         start_date=parseDateString(FieldedStart),
         end_date=parseDateString(FieldedEnd))

## absurdly long polls almost always a case of transposed dates
d2022 <- d2022 %>% 
  mutate(flag=difftime(end_date,start_date)>14 | 
           end_date>Sys.Date() | 
           start_date>Sys.Date() | 
           start_date>end_date)

tab <- d2022 %>%
  select(Pollster,start_date,end_date,flag) %>% 
  arrange(desc(flag))


date_transpose <- function(z){
  m <- month(z)
  d <- day(z)
  
  if(d<13){
    month(z) <- d
    day(z) <- m
    z_try <- as.Date(z,origin=as.Date("1970/01/01"))
    z <- ifelse(z_try<Sys.Date(),z_try,z)
  }
  
  if(class(z)!="Date"){
    z <- as.Date(z,origin=as.Date("1970/01/01"))
  }
  return(z)
}
  
## -----------function to swap month and day if date is in format m/d/y---------------
fixDateTransposed <- function(start_date,end_date){
  ## work through some cases
  if(start_date>end_date & start_date>Sys.Date()){
    ## transpose start_date
    start_date <- date_transpose(start_date)
  }
  
  if(start_date>Sys.Date() & end_date>Sys.Date()){
    start_date <- date_transpose(start_date)
    end_date <- date_transpose(end_date)
  }
  
  if(end_date>Sys.Date()){
    end_date <- date_transpose(end_date)
  }
  
  if(difftime(end_date,start_date)>14 | start_date>end_date){
    ## try to figure out which of the two dates is transposed
    if(day(start_date)>12 & day(end_date)<13){
      end_date <- date_transpose(end_date)
    } else
      if(day(start_date)<13 & day(end_date)>12){
        start_date <- date_transpose(start_date)
      }
  }
    
  # transposing which one makes sense?
  if(difftime(end_date,start_date)>14 | start_date>end_date){
    start_tmp <- date_transpose(start_date)
    end_tmp <- date_transpose(end_date)
    if(difftime(end_tmp,start_date)<14 & start_date<end_tmp){
      end_date <- end_tmp
    } 
    else
      if(difftime(end_date,start_tmp)<14 & start_tmp<end_date){
        start_date <- start_tmp
      }
    else 
      if(difftime(end_tmp,start_tmp)<14 & start_tmp<end_tmp){
        start_date <- start_tmp
        end_date <- end_tmp
      }
  }
  
  out <- tibble(start_date_2=start_date,
                end_date_2=end_date)
  return(out)
}


## fix weird/impossible dates
d2022 <- d2022 %>%
  mutate(m = map2(.x=start_date,.y=end_date,~fixDateTransposed(.x,.y))) %>%
  unnest(m)

tab <- d2022 %>% 
  mutate(flag=difftime(end_date_2,start_date_2)>14 | 
           end_date_2>Sys.Date() | 
           start_date_2>Sys.Date() | 
           start_date_2>end_date_2) %>%
  select(Pollster,start_date,end_date,start_date_2,end_date_2) %>% 
  arrange(desc(start_date_2))

print(tab)

d2022 <- d2022 %>%
  mutate(mid_date = as.Date(floor((as.numeric(start_date_2)+as.numeric(end_date_2))/2),
                            origin="1970/01/01")) %>%
  mutate(ALP_2PP = if_else(ALP_2PP<1,ALP_2PP*100,ALP_2PP),
         LNP_2PP = if_else(LNP_2PP<1,LNP_2PP*100,LNP_2PP)) 

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
