########################################################
## state-space model, setup for stan
##
## simon jackman
## simon.jackman@sydney.edu.au
## ussc, univ of sydney
## 2019-06-16 22:49:43
########################################################

library(tidyverse)
library(here)
library(ussc)

source("common.R")

makeStanData <- function(theParty){
  
  tmp <- polls %>% 
    filter(what==theParty) %>% 
    filter(start>electionDay_2016) %>% 
    filter(!is.na(y))
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
      mutate(fieldDay=seq.Date(from=start[1],to=end[1],by="day")) %>%
      mutate(day=as.numeric(difftime(fieldDay,electionDay_2016,units="days"))) %>%
      mutate(sigma = sqrt(y*(1-y)/n))
  }
  forStan <- bind_rows(out) %>%
    select(pollster,j,y,day,fieldDay,sigma)
  forStan <- as.list(forStan)
  
  ## reported precision of result
  forStan$halfwidth <- ifelse(forStan$y%%.01==0,.005,.0005)
  forStan$NDAYS <- length(seq.Date(from=electionDay_2016,
                                   to=electionDay_2019,
                                   by="day"))
  
  forStan$theBreaks <- match(theBreaks,
                             seq.Date(from=electionDay_2016,
                                      to=electionDay_2019,
                                      by="day"))
  forStan$discontinuity <- rep(0,forStan$NDAYS)
  forStan$discontinuity[forStan$theBreaks] <- 1
  
  forStan$election_result_2016 <- endPoints[[theParty]]$y[1]/100
  forStan$election_result_2019 <- endPoints[[theParty]]$y[2]/100
  
  forStan$NPOLLS <- length(forStan$y)
  forStan$NHOUSES <- max(forStan$j)
  forStan$tau_upper <- .003
  forStan$sigma_delta_upper <- .04

  return(as.list(forStan))
}

##forStan <- makeStanData("ALP")

init_function <- function(chain_id=1,data=forStan){
  tau_candidate <- .0008187*2
  xi <- seq(from=data$election_result_2016,
         to=data$election_result_2019,
         length=data$NDAYS)
  wn <- c(arima.sim(n=data$NDAYS-21,
                    list(ar=.99),
                    sd=tau_candidate),
          rep(0,20))
  xi <- c(xi[1],xi[-1]+wn)
  e <- diff(xi)
  tau <- sd(e)
  omega <- e/tau
  
  ## compute house effects for each house, conditional on xi
  tmpData <- as_tibble(data.frame(y=data$y,
                                  day=data$day,
                                  j=data$j)) %>%
    left_join(data.frame(xi=xi,day=1:length(xi))) %>% 
    group_by(j) %>% 
    summarise(delta=mean(y-xi,na.rm=TRUE)) %>%
    ungroup()
  
  
  list(xi=xi[-data$NDAYS],
       delta_raw=tmpData$delta,
       omega=omega,
       tau=tau_candidate,
       gamma=0)
} 


run_stan <- function(party="ALP"){
  forStan <- makeStanData(party)
  
  require(rstan)
  if(exists("m")){
    rm(m)
  }
  
  if(system("whoami",intern = TRUE)=="jackman" & 
     grepl(pattern="sydney.edu.au",
     system("uname -a",intern = TRUE))){
    system("cp ~/.R/Makevars.stan ~/.R/Makevars")
  }

  m <- stan(file="../stan/sum_to_zero.stan",
            data = forStan,
            pars=c("xi","delta","omega"),
            init=if(nchains>1){
               lapply(1:nchains,
                      function(id){
                        init_function(chain_id=id,data=forStan)
                      })
             } else {
               init_function
             },
            iter=7.5E3,
            warmup = 2.5E3,
            thin=5,
            chains = nchains,
            cores=nchains,
            control=list(adapt_delta=0.99,
                         max_treedepth=20),
            verbose = TRUE)
  
  if(system("whoami",intern = TRUE)=="jackman" & 
     grepl(pattern="sydney.edu.au",
           system("uname -a",intern = TRUE))){
    system("cp ~/.R/Makevars.apple ~/.R/Makevars")
  }
  
  out <- as.data.frame(m)
  return(out)
}

run_wrapper <- function(theParty="ALP"){
  out <- run_stan(party=theParty)
  theOutFile <- paste("../stan/",theParty,"_sum_to_zero.rds",sep="")
  saveRDS(out,file=theOutFile)
  return(invisible(NULL))
}

run_wrapper("ALP")
run_wrapper("GRN")
run_wrapper("Coalition2PP")
run_wrapper("LNP")
run_wrapper("OTH")







                  