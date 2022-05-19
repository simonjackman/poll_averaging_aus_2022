########################################################
## state-space model, setup for stan
##
## simon jackman
## simon.jackman@sydney.edu.au
## ussc, univ of sydney
## 2019-06-16 22:49:43
########################################################

########################################################
## With some modifications on he state-space model, setup for stan
##
## Luke Mansillo
## luke.mansillo@sydney.edu.au
## ussc, univ of sydney
## 2019-08-22 1:39:53
########################################################

library(tidyverse)
library(here)
library(ussc)


# polls_16_19 <- read.csv("../../data/polls_2016_2019_good_v2.csv")
# 
# polls_13_16 <- read.csv("../../data/polls_2013_2016_good_v2.csv")
# 
# polls_04_13 <- read.csv("../../data/polls_2004_2013_good_v2.csv")
# 
# require(dplyr)
# 
# polls_16_19$sample[polls_16_19$pollster=="Essential"] <- polls_16_19$sample[polls_16_19$pollster=="Essential"]/2
# 
# polls_16_19$responses <- 9 - rowSums(is.na(polls_16_19[c( "Coalition.total.primary",
#                                                           "Labor.primary","Green.primary",
#                                                           "OneNation.primary","Other.primary",
#                                                           "UAP.PUP.primary","KattersAustraliaParty.primary",
#                                                           "XenophonCentreAlliance.primary","CON")]))
# 
# p.16_19 <- polls_16_19 %>% select(fielding.start,fielding.end,pollster,sample,
#                                   Coalition.total.primary,Labor.primary,Green.primary,OneNation.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
#                                   DKrefused.combined,responses,
#                                   Coalition.TPP.reported)
# 
# 
# polls_13_16$responses <- 7 - rowSums(is.na(polls_13_16[c( "Coalition.total.primary",
#                                                           "Labor.primary","Green.primary",
#                                                           "Other.primary",
#                                                           "UAP.PUP.primary","KattersAustraliaParty.primary",
#                                                           "XenophonCentreAlliance.primary")]))
# 
# p.13_16 <- polls_13_16 %>% select(fielding.start,fielding.end,pollster,sample,
#                                   Coalition.total.primary,Labor.primary,Green.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
#                                   DKrefused.combined,responses,
#                                   Coalition.TPP.reported)
# 
# 
# polls_04_13$responses <- 12 - rowSums(is.na(polls_04_13[c( "Coalition.total.primary","Liberal.primary","National.primary",
#                                                            "FamilyFirst.primary","Democrats.primary","OneNation.primary",
#                                                            "Labor.primary","Green.primary",
#                                                            "Other.primary",
#                                                            "UAP.PUP.primary","KattersAustraliaParty.primary", 
#                                                            "Independent.primary",      
#                                                            "XenophonCentreAlliance.primary")]))
# 
# 
# p.04_13 <- polls_04_13 %>% select(fielding.start,fielding.end,pollster,sample,
#                                   Coalition.total.primary,Labor.primary,Green.primary,
#                                   Other.primary,
#                                   UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,
#                                   DKrefused.combined,responses,
#                                   Coalition.TPP.reported)
# 
# comb <- bind_rows(p.16_19,p.13_16,p.04_13)
# 
# comb$LNP <- comb$Coalition.total.primary
# comb$ALP <- comb$Labor.primary
# comb$GRN <- comb$Green.primary
# comb$OTH <- 100 - comb$Labor.primary- comb$Green.primary - comb$Coalition.total.primary
# comb$LNP2PP <- comb$Coalition.TPP.reported
# 
# comb$fielding.start <- as.Date(comb$fielding.start,format="%d/%m/%y")
# comb$fielding.end <- as.Date(comb$fielding.end,format="%d/%m/%y")
# 
# #comb <- comb %>% mutate(OTH=sum(Other.primary,
# #                                UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,na.rm=TRUE))
# 
# 
# election.data <- read.csv("../../data/electionresults_jumps_long.csv")
# election.data$election <- as.factor(election.data$election)
# 
# require(tidyverse)
# #library(here)
# #library(ussc)
# 
# comb.s <- comb %>% select(fielding.start,fielding.end,pollster,sample,LNP,ALP,GRN,OTH,LNP2PP) %>% gather(`LNP`,`ALP`,`GRN`,`OTH`,`LNP2PP`,key="what",value="y")
# comb.s$pollster <- as.factor(comb.s$pollster)
# require(dplyr)
# 
# #source("common.R")
# 
# makeStanData <- function(theParty){
#   
#   tmp <- polls %>% 
#     filter(what==theParty) %>% 
#     filter(start>electionDay_2016) %>% 
#     filter(!is.na(y))
#   tmp <- pollster_recode(tmp)
#   
#   ## break polls into dates
#   n <- dim(tmp)[1]
#   out <- list()
#   for(i in 1:n){
#     m <- tmp$ndays[i]
#     out[[i]] <- tmp %>% 
#       slice(i) %>% 
#       slice(rep(row_number(), m)) %>%
#       mutate(n=sample/ndays) %>%
#       mutate(fieldDay=seq.Date(from=start[1],to=end[1],by="day")) %>%
#       mutate(day=as.numeric(difftime(fieldDay,electionDay_2016,units="days"))) %>%
#       mutate(sigma = sqrt(y*(1-y)/n))
#   }
#   forStan <- bind_rows(out) %>%
#     select(pollster,j,y,day,fieldDay,sigma)
#   forStan <- as.list(forStan)
#   
#   ## reported precision of result
#   forStan$halfwidth <- ifelse(forStan$y%%.01==0,.005,.0005)
#   forStan$NDAYS <- length(seq.Date(from=electionDay_2016,to=electionDay_2019,by="day"))
#   
#   forStan$theBreaks <- match(theBreaks,seq.Date(from=electionDay_2016,to=electionDay_2019,by="day"))
#   forStan$discontinuity <- rep(0,forStan$NDAYS)
#   forStan$discontinuity[forStan$theBreaks] <- 1
#   
#   forStan$election_result_2016 <- endPoints[[theParty]]$y[1]/100
#   forStan$election_result_2019 <- endPoints[[theParty]]$y[2]/100
#   
#   forStan$NPOLLS <- length(forStan$y)
#   forStan$NHOUSES <- max(forStan$j)
#   forStan$tau_upper <- .003
#   forStan$sigma_delta_upper <- .04
#   
#   return(as.list(forStan))
# }
# 
# ##forStan <- makeStanData("ALP")
# 
# init_function <- function(chain_id=1,data=forStan){
#   tau_candidate <- .0008187*2
#   xi <- seq(from=data$election_result_2016,
#             to=data$election_result_2019,
#             length=data$NDAYS)
#   wn <- c(arima.sim(n=data$NDAYS-21,
#                     list(ar=.99),
#                     sd=tau_candidate),
#           rep(0,20))
#   xi <- c(xi[1],xi[-1]+wn)
#   e <- diff(xi)
#   tau <- sd(e)
#   omega <- e/tau
#   
#   ## compute house effects for each house, conditional on xi
#   tmpData <- as_tibble(data.frame(y=data$y,
#                                   day=data$day,
#                                   j=data$j)) %>%
#     left_join(data.frame(xi=xi,day=1:length(xi))) %>% 
#     group_by(j) %>% 
#     summarise(delta=mean(y-xi,na.rm=TRUE)) %>%
#     ungroup()
#   
#   
#   list(xi=xi[-data$NDAYS],
#        delta=tmpData$delta,
#        omega=omega,
#        tau=tau_candidate,
#        gamma=0)
# } 
# 
# 
# run_stan <- function(party="ALP"){
#   forStan <- makeStanData(party)
#   
#   require(rstan)
#   if(exists("m")){
#     rm(m)
#   }
#   
#   if(system("whoami",intern = TRUE)=="jackman"){
#     system("cp ~/.R/Makevars.stan ~/.R/Makevars")
#   }
#   
#   m <- stan(file="../stan/standard.stan",
#             data = forStan,
#             pars=c("omega"),
#             include=FALSE,
#             init=if(nchains>1){
#               lapply(1:nchains,
#                      function(id){
#                        init_function(chain_id=id,data=forStan)
#                      })
#             } else {
#               init_function
#             },
#             iter=10E3,
#             warmup = 4E3,
#             thin=5,
#             chains = nchains,
#             cores=nchains,
#             control=list(adapt_delta=0.99,
#                          max_treedepth=20),
#             verbose = TRUE)
#   
#   if(system("whoami",intern = TRUE)=="jackman"){
#     system("cp ~/.R/Makevars.apple ~/.R/Makevars")
#   }
#   
#   out <- as.data.frame(m)
#   return(out)
# }
# 
# run_wrapper <- function(theParty="ALP"){
#   out <- run_stan(party=theParty)
#   theOutFile <- paste("../stan/",theParty,"_standard.rds",sep="")
#   saveRDS(out,file=theOutFile)
#   return(invisible(NULL))
# }
# 
# run_wrapper("ALP")
# run_wrapper("LNP")
# run_wrapper("GRN")
# run_wrapper("OTH")
# run_wrapper("Coalition2PP")
# 


#####



polls_16_19 <- read.csv("../../data/polls_2016_2019_good_v2.csv")

polls_13_16 <- read.csv("../../data/polls_2013_2016_good_v2.csv")

polls_04_13 <- read.csv("../../data/polls_2004_2013_good_v2.csv")

library(dplyr)

polls_16_19$sample[polls_16_19$pollster=="Essential"] <- polls_16_19$sample[polls_16_19$pollster=="Essential"]/2

polls_16_19$responses <- 9 - rowSums(is.na(polls_16_19[c( "Coalition.total.primary",
                                                          "Labor.primary","Green.primary",
                                                          "OneNation.primary","Other.primary",
                                                          "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                          "XenophonCentreAlliance.primary","CON")]))

p.16_19 <- polls_16_19 %>% select(fielding.start,fielding.end,pollster,sample,
                                  Coalition.total.primary,Labor.primary,Green.primary,OneNation.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                  DKrefused.combined,responses,
                                  Coalition.TPP.reported)


polls_13_16$responses <- 7 - rowSums(is.na(polls_13_16[c( "Coalition.total.primary",
                                                          "Labor.primary","Green.primary",
                                                          "Other.primary",
                                                          "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                          "XenophonCentreAlliance.primary")]))

p.13_16 <- polls_13_16 %>% select(fielding.start,fielding.end,pollster,sample,
                                  Coalition.total.primary,Labor.primary,Green.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                  DKrefused.combined,responses,
                                  Coalition.TPP.reported)


polls_04_13$responses <- 12 - rowSums(is.na(polls_04_13[c( "Coalition.total.primary","Liberal.primary","National.primary",
                                                           "FamilyFirst.primary","Democrats.primary","OneNation.primary",
                                                           "Labor.primary","Green.primary",
                                                           "Other.primary",
                                                           "UAP.PUP.primary","KattersAustraliaParty.primary", 
                                                           "Independent.primary",      
                                                           "XenophonCentreAlliance.primary")]))


p.04_13 <- polls_04_13 %>% select(fielding.start,fielding.end,pollster,sample,
                                  Coalition.total.primary,Labor.primary,Green.primary,
                                  Other.primary,
                                  UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,
                                  DKrefused.combined,responses,
                                  Coalition.TPP.reported)

comb <- bind_rows(p.16_19,p.13_16,p.04_13)

comb$LNP <- comb$Coalition.total.primary
comb$ALP <- comb$Labor.primary
comb$GRN <- comb$Green.primary
comb$OTH <- 100 - comb$Labor.primary- comb$Green.primary - comb$Coalition.total.primary
comb$LNP2PP <- comb$Coalition.TPP.reported

comb$fielding.start <- as.Date(comb$fielding.start,format="%d/%m/%y")
comb$fielding.end <- as.Date(comb$fielding.end,format="%d/%m/%y")

comb$pollster <- recode(comb$pollster,
                        "Galaxy MM"="Newspoll (online & robo)",
                        "Essential (one-week)"="Essential")

#comb <- comb %>% mutate(OTH=sum(Other.primary,
#                                UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,na.rm=TRUE))


election.data <- read.csv("../../data/electionresults_jumps_long.csv")
election.data$election <- as.factor(election.data$election)

library(tidyverse)
#library(here)
#library(ussc)

comb.s <- comb %>% select(fielding.start,fielding.end,pollster,sample,LNP,ALP,GRN,OTH,LNP2PP) %>% gather(`LNP`,`ALP`,`GRN`,`OTH`,`LNP2PP`,key="what",value="y")
comb.s$pollster <- as.factor(comb.s$pollster)
#####


#source("common.R")


pollster_recode <- function(obj){
  zzz <- obj$pollster
  if(is.factor(zzz)){
    zzz <- as.character(zzz)
  }
  j <- rep(NA,dim(obj)[1])
  j[grepl("Essential",zzz)] <- "Essential"
  j[grepl("YouGov",zzz)] <- "YouGov"
  j[is.na(j)] <- zzz[is.na(j)]
  obj$pollster_simon <- j
  j <- match(j,sort(unique(j)))
  obj$j <- j 
  return(obj)
}

makeStanData <- function(theParty,theYear#,theFilter
                         ){
  
  polls_16_19 <- read.csv("../../data/polls_2016_2019_good_v2.csv")
  
  polls_13_16 <- read.csv("../../data/polls_2013_2016_good_v2.csv")
  
  polls_04_13 <- read.csv("../../data/polls_2004_2013_good_v2.csv")
  
  require(dplyr)
  
  polls_16_19$sample[polls_16_19$pollster=="Essential"] <- polls_16_19$sample[polls_16_19$pollster=="Essential"]/2
  
  polls_16_19$responses <- 9 - rowSums(is.na(polls_16_19[c( "Coalition.total.primary",
                                                            "Labor.primary","Green.primary",
                                                            "OneNation.primary","Other.primary",
                                                            "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                            "XenophonCentreAlliance.primary","CON")]))
  
  p.16_19 <- polls_16_19 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,OneNation.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  
  polls_13_16$responses <- 7 - rowSums(is.na(polls_13_16[c( "Coalition.total.primary",
                                                            "Labor.primary","Green.primary",
                                                            "Other.primary",
                                                            "UAP.PUP.primary","KattersAustraliaParty.primary",
                                                            "XenophonCentreAlliance.primary")]))
  
  p.13_16 <- polls_13_16 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,Other.primary,UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  
  polls_04_13$responses <- 12 - rowSums(is.na(polls_04_13[c( "Coalition.total.primary","Liberal.primary","National.primary",
                                                             "FamilyFirst.primary","Democrats.primary","OneNation.primary",
                                                             "Labor.primary","Green.primary",
                                                             "Other.primary",
                                                             "UAP.PUP.primary","KattersAustraliaParty.primary", 
                                                             "Independent.primary",      
                                                             "XenophonCentreAlliance.primary")]))
  
  
  p.04_13 <- polls_04_13 %>% select(fielding.start,fielding.end,pollster,sample,
                                    Coalition.total.primary,Labor.primary,Green.primary,
                                    Other.primary,
                                    UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,
                                    DKrefused.combined,responses,
                                    Coalition.TPP.reported)
  
  comb <- bind_rows(p.16_19,p.13_16,p.04_13)
  
  comb$LNP <- comb$Coalition.total.primary
  comb$ALP <- comb$Labor.primary
  comb$GRN <- comb$Green.primary
  comb$OTH <- 100 - comb$Labor.primary- comb$Green.primary - comb$Coalition.total.primary
  comb$LNP2PP <- comb$Coalition.TPP.reported
  
  comb$fielding.start <- as.Date(comb$fielding.start,format="%d/%m/%y")
  comb$fielding.end <- as.Date(comb$fielding.end,format="%d/%m/%y")
  
  
  comb$pollster <- recode(comb$pollster,
                          "Galaxy MM"="Newspoll (online & robo)",
                          "Essential (one-week)"="Essential")
  
  
  #comb <- comb %>% mutate(OTH=sum(Other.primary,
  #                                UAP.PUP.primary,KattersAustraliaParty.primary,XenophonCentreAlliance.primary,FamilyFirst.primary,Democrats.primary,OneNation.primary,Independent.primary,na.rm=TRUE))
  
  
  election.data <- read.csv("../../data/electionresults_jumps_long.csv")
  election.data$election <- as.factor(election.data$election)
  
  require(tidyverse)
  #library(here)
  #library(ussc)
  
  comb.s <- comb %>% select(fielding.start,fielding.end,pollster,sample,LNP,ALP,GRN,OTH,LNP2PP) %>% gather(`LNP`,`ALP`,`GRN`,`OTH`,`LNP2PP`,key="what",value="y")
  comb.s$pollster <- as.factor(comb.s$pollster)
  require(dplyr)
  election.data <- election.data %>% filter(what==theParty) %>% filter(election==theYear)
  
  prevElectionDay <- as.Date(election.data$start.election,format="%d/%m/%y")
  ElectionDay <- as.Date(election.data$end.election,format="%d/%m/%y")
  jumpDay <- as.Date(election.data$jump.date,format="%d/%m/%y")
  
  tmp <- comb.s %>%
    filter(what==theParty) %>%
    filter(fielding.start>prevElectionDay) %>%
    filter(fielding.end<ElectionDay) %>%
    filter(!is.na(y)) %>%
    mutate(y=y/100) %>% 
    mutate(ndays=as.numeric(difftime(fielding.end,fielding.start,units="days"))+1)
  
  tmp$pollster <- as.character(tmp$pollster)
  tmp <- pollster_recode(tmp)
  
  ## break polls into dates
  n <- dim(tmp)[1]
  out <- list()
  for(i in 1:n){
    m <- tmp$ndays[i]
    out[[i]] <- tmp %>%
      slice(i) %>%
      slice(rep(row_number(), m)) %>%
      mutate(n=sample/ndays) %>%
      mutate(fieldDay=seq.Date(from=fielding.start[1],to=fielding.end[1],by="day")) %>%
      mutate(day=as.numeric(difftime(fieldDay,prevElectionDay,units="days"))) %>%
      mutate(daytoelection=as.numeric(difftime(ElectionDay,fieldDay,units="days"))) %>%
      mutate(sigma = sqrt(y*(1-y)/n)) # %>%
      #filter(daytoelection<=theFilter)
  }
  forStan <- bind_rows(out) %>%
    select(pollster,j,y,day,fieldDay,sigma) 
  forStan <- as.list(forStan)
  
  ## reported precision of result
  forStan$halfwidth <- ifelse(forStan$y%%.01==0,.005,.0005)
  forStan$NDAYS <- length(seq.Date(from=prevElectionDay,to=ElectionDay,by="day"))
  
  theBreaks <- jumpDay
  
  forStan$theBreaks <- match(theBreaks,seq.Date(from=prevElectionDay,to=ElectionDay,by="day"))
  forStan$discontinuity <- rep(0,forStan$NDAYS)
  forStan$discontinuity[forStan$theBreaks] <- 1
  
  forStan$prevElectionResult <- election.data$start.result/100
  forStan$ElectionResult <- election.data$end.result/100
  
  forStan$prevElectionDay <- prevElectionDay
  forStan$ElectionDay <- ElectionDay
  
  forStan$j <- as.integer(as.factor(forStan$j))
  
  forStan$NPOLLS <- length(forStan$y)
  forStan$NHOUSES <- length(unique(forStan$j))
  forStan$tau_upper <- .003
  forStan$sigma_delta_upper <- .04
  
  return(as.list(forStan))
}

# forStan <- makeStanData(theParty="GRN",theYear="2010",theFilter = 2000)


init_function <- function(chain_id=1,data=forStan){
  tau_candidate <- .0008187*2
  xi <- seq(from=as.numeric(data$prevElectionResult),
            to=as.numeric(data$ElectionResult),
            length=data$NDAYS)
  wn <- c(arima.sim(n=data$NDAYS-21,
                    list(ar=.99),
                    sd=tau_candidate),
          rep(0,20))
  xi <- c(xi[1],xi[-1]+wn)
  e <- diff(xi)
  tau <- sd(e)
  omega <- (e/tau)#/100
  
  ## compute house effects for each house, conditional on xi
  tmpData <- as_tibble(data.frame(y=data$y,
                                  day=data$day,
                                  j=data$j)) %>%
    left_join(data.frame(xi=xi,day=1:length(xi))) %>%
    group_by(j) %>%
    summarise(delta=mean(y-xi,na.rm=TRUE)) %>%
    ungroup()
  
  
  list(xi=xi[-data$NDAYS],
       delta=tmpData$delta,
       omega=omega,
       tau=tau_candidate,
       gamma=0)
}


run_stan <- function(theParty,theYear#,theFilter
                     ){
  forStan <- makeStanData(theParty,theYear#,theFilter
                          )
  
  require(rstan)
  if(exists("m")){
    rm(m)
  }
  m <- stan(file="../stan/standard4.stan",
            data = forStan,
            pars=c("omega"),
            include=FALSE,
            init=if(nchains>1){
              lapply(1:nchains,
                     function(id){
                       init_function(chain_id=id,data=forStan)
                     })
            } else {
              init_function
            },
            iter=10E3,
            warmup = 4E3,
            thin=5,
            chains = nchains,
            cores=nchains,
            control=list(adapt_delta=0.99,
                         max_treedepth=20),
            verbose = TRUE)
  
  #out <- as.data.frame(m)
  return(m)
}

run_wrapper <- function(theParty,theYear#,theFilter
                        ){
  m <- run_stan(theParty,theYear#,theFilter
                )
  out <- as.data.frame(m)
  theOutFile <- paste("../stan/",theParty,"_",theYear,#"_",theFilter,
                      "_standard.rds",sep="")
  theModelOutFile <- paste("../stan/model/",theParty,"_",theYear,#"_",theFilter,
                           "_standard.rds",sep="")
  saveRDS(out,file=theOutFile)
  saveRDS(m,file=theModelOutFile)
  return(invisible(NULL))
}

nchains <- 4
party <-c("LNP2PP","ALP","LNP","GRN","OTH")
year <-rev(c("2007","2010","2013","2016","2019"))
#filter <-c(seq(30,1170,by=30))
#filteryear <- rev(c(39,34,38,35,36))




library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl,20)

library(foreach)


foreach(n=1:length(year),.verbose=TRUE) %:% 
  foreach(j=1:length(party),.verbose=TRUE)  %dopar% 
  #foreach(k=1:length(filter[1:filteryear[n]]),.verbose=TRUE) %:% 
  #when(!file.exists(paste0("../stan/filtered4/model/",party[j],"_",year[n],"_standard.rds")) ) 
  run_wrapper(theParty=party[j],
              theYear=year[n]#,
              #theFilter=filter[k]#,#theFilterYear=filteryear[n]
  )



