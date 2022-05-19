########################################################
## functions etc for running sum-to-zero model for 
## user-specified party
## user-specified year \in (2007, ..., 2016)
##
## simon jackman
## simon.jackman@sydney.edu.au
## ussc, univ of sydney
## 2022-04-03 18:59:41
########################################################
library(tidyverse)
library(here)

niter <- 6500
burnin <- 1500
nchains <- 4

makeCombined <- function(){
  polls_16_19 <- read.csv(here("data/2019/polls_2016_2019_good_v2.csv"))
  polls_13_16 <- read.csv(here("data/2016/polls_2013_2016_good_v2.csv"))
  polls_04_13 <- read.csv(here("data/2013/polls_2004_2013_good_v2.csv"))
  
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
  
  comb <- comb %>%
    mutate(mid_date = as.Date(floor((as.numeric(fielding.start)+as.numeric(fielding.end))/2),
                            origin="1970/01/01")) 
  
  
  comb$pollster <- recode(comb$pollster,
                          "Galaxy MM"="Newspoll (online & robo)",
                          "Essential (one-week)"="Essential")
  
  comb.s <- comb %>% 
    select(fielding.start,fielding.end,mid_date,pollster,sample,LNP,ALP,GRN,OTH,LNP2PP) %>% 
    gather(`LNP`,`ALP`,`GRN`,`OTH`,`LNP2PP`,key="what",value="y")
  comb.s$pollster <- as.factor(comb.s$pollster)
  
  return(as_tibble(comb.s))
  
}

comb.s <- makeCombined()
  
getElectionData <- function(theParty,theYear){
  election.data <- read.csv(here("data/historic/electionresults_jumps_long.csv"))
  election.data$election <- as.factor(election.data$election)
  election.data <- election.data %>% filter(what==theParty) %>% filter(election==theYear)
  return(election.data)
}

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

makeStanData <- function(theParty,year){
  cat(paste("running makeStanData_test for",theParty,"for the",year,"election\n"))
  
  election.data <- getElectionData(theParty,year)
  prevElectionDay <- as.Date(election.data$start.election,format="%d/%m/%y")
  ElectionDay <- as.Date(election.data$end.election,format="%d/%m/%y")
  jumpDay <- as.Date(election.data$jump.date,format="%d/%m/%y")
  
  dateSeq <- seq.Date(from=prevElectionDay,to=ElectionDay,by="day")
  
  tmp <- comb.s %>% 
    filter(what == theParty) %>%
    filter(fielding.start>prevElectionDay) %>%
    filter(fielding.end<ElectionDay) %>%
    filter(!is.na(y)) 
  
  tmp$pollster <- as.character(tmp$pollster)
  
  ## write historic polls to data frame
  save(tmp,
       dateSeq,
       election.data,
       file = here(paste("data",year,
                          paste(theParty,"polls.RData",sep="_"),
                          sep="/")))
  
  tmp <- pollster_recode(tmp)
  
  forStan <- as.list(tmp %>%
                       mutate(y=y/100,
                              sigma=sqrt(y*(1-y)/sample)) %>%
                       mutate(day=match(mid_date,dateSeq)))
  
  
  forStan$NDAYS <- length(dateSeq)
  
  theBreaks <- jumpDay
  
  forStan$theBreaks <- match(theBreaks,dateSeq)
  forStan$discontinuity <- rep(0,forStan$NDAYS)
  forStan$discontinuity[forStan$theBreaks] <- 1:length(theBreaks)
  forStan$NBREAKS <- max(forStan$discontinuity)
  
  forStan$prevElectionResult <- election.data$start.result/100
  ##forStan$ElectionResult <- election.data$end.result/100
  
  ##forStan$prevElectionDay <- prevElectionDay
  ##forStan$ElectionDay <- ElectionDay
  
  forStan$j <- as.integer(as.factor(forStan$j))
  
  forStan$NPOLLS <- length(forStan$y)
  forStan$NHOUSES <- length(unique(forStan$j))
  forStan$tau_upper <- .003
  forStan$sigma_delta_upper <- .04
  
  return(list(forStan=forStan,dateSeq=dateSeq))
}


## ----initial-values-----------------------------------------------------------
init_function <- function(chain_id=1,data=forStan){
  tau_candidate <- .0008187*2
  xi <- seq(from=data$prevElectionResult,
            to=mean(data$y[data$day==max(data$day)]),
            length=data$NDAYS)
  wn <- c(arima.sim(n=data$NDAYS-45,
                    list(ar=.99),
                    sd=tau_candidate),
          rep(0,44))
  xi <- c(xi[1],xi[-1]+wn)
  e <- diff(xi)
  tau <- sd(e)
  omega <- e/tau
  omega <- omega
  
  ## compute house effects for each house, conditional on xi
  tmpData <- as_tibble(data.frame(y=data$y,
                                  day=data$day,
                                  j=data$j)) %>%
    left_join(data.frame(xi=xi,day=1:length(xi))) %>% 
    group_by(j) %>% 
    summarise(delta=mean(y-xi,na.rm=TRUE)) %>%
    ungroup()
  
  
  list(xi=xi,
       delta_raw=tmpData$delta,
       omega=omega,
       tau=tau_candidate)
} 



## ----run-stan-----------------------------------------------------------------
run_stan <- function(party="ALP",year=2019,debug=FALSE){
  msd <- makeStanData(party,year)
  forStan <- msd$forStan
  dateSeq <- msd$dateSeq
  rm(msd)
  
  if(debug){
    cat(paste("forStan object for party",forStan,"\n"))
    print(forStan)
  }
  
  require(rstan)
  if(exists("m",inherits = FALSE)){
    rm(m)
  }
  
  # if(system("whoami",intern = TRUE)=="jackman" & 
  #    grepl(pattern="sydney.edu.au",
  #    system("uname -a",intern = TRUE))){
  #   system("cp ~/.R/Makevars.stan ~/.R/Makevars")
  # }
  
  ncores <- max(2,parallel::detectCores()-2)
  
  m <- stan(file=here("R/sum_to_zero_historic.stan"),
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
            iter=niter,
            warmup = burnin,
            thin=5,
            chains = 4,
            cores=ncores,
            control=list(adapt_delta=0.99,
                         max_treedepth=20),
            verbose = debug)
  
  # if(system("whoami",intern = TRUE)=="jackman" & 
  #    grepl(pattern="sydney.edu.au",
  #          system("uname -a",intern = TRUE))){
  #   system("cp ~/.R/Makevars.apple ~/.R/Makevars")
  #}
  
  ##out <- as.data.frame(m)
  return(list(m=m,dateSeq=dateSeq))
}

extractSummary <- function(m){
  s <- summary(m$m,pars="xi")
  xi <- s$summary %>% as_tibble() 
  xi <- xi %>% 
    mutate(date=m$dateSeq) %>% 
    rename(lo=`2.5%`,up=`97.5%`)
  return(xi)
}

run_wrapper <- function(theParty="ALP",year=2019,debug=FALSE){
  require(rstan)
  m <- run_stan(party=theParty,year=year,debug=debug)
  xi <- extractSummary(m)
  
  theOutFile <- here(
    paste("output",
          year,
          paste("xi",theParty,
                "sum_to_zero.rds",sep="_"),
          sep="/")
  )
  
  saveRDS(xi,file=theOutFile)
  return(invisible(NULL))
}


inspect <- function(theParty,year,type="sum_to_zero"){
  
  ## load polls
  load(file=here(paste("data",year,
                       paste(theParty,"polls.RData",sep="_"),
                       sep="/")))
  thePolls <- tmp
  rm(tmp)
  
  prevElectionDay <- as.Date(election.data$start.election,format="%d/%m/%y")
  thisElectionDay <- as.Date(election.data$end.election,format="%d/%m/%y")
  dateSeq <- seq.Date(prevElectionDay,thisElectionDay,by="day")
  
  pastElectionResult <- election.data$start.result
  thisElectionResult <- election.data$end.result
  
  colors <- tribble(
    ~color, ~Party,
    "#ed1b35", "ALP",
    "#009de3", "LNP",
    "#00A800", "GRN",
    "#ee4627", "OTH",
    "#ed1b35", "ALP_2PP",
    "#009de3", "LNP_2PP",
    "#ed1b35", "ALP2PP",
    "#009de3", "LNP2PP"
  )
  
  thisCol <- colors %>% filter(Party==theParty) %>% pull(color)
  pastElectionResult <- election.data$start.result
  thisElectionResult <- election.data$end.result
  
  ## sum to zero results
  xi <- readRDS(file=here(
    paste("output",
          year,
          paste("xi",theParty,
                "sum_to_zero.rds",sep="_"),
          sep="/"))) %>%
    mutate(type="Poll average") %>%
    mutate(type=factor(type))
  
  error <- 100*(xi$mean[length(xi$mean)]) - thisElectionResult
  
  ## standard results, if user asks for them
  if(type!="sum_to_zero"){
    xi_standard <- readRDS(file=here(paste("data/historic/standard/",
                                           theParty,"_",year,
                                           "_standard.rds",sep=""))) %>%
      as_tibble() %>%
      select(starts_with("xi")) %>%
      pivot_longer(cols=everything(),names_to = "xi",values_to = "x") %>%
      mutate(xi = str_extract(xi,pattern="[0-9]{1,}")) %>%
      mutate(xi = as.numeric(xi)) %>%
      group_by(xi) %>%
      summarise(mean=mean(x),
                lo=quantile(x,.025),
                up=quantile(x,.975)) %>%
      ungroup() %>%
      bind_rows(tibble(xi=length(dateSeq),
                       mean=thisElectionResult/100,
                       lo=thisElectionResult/100,
                       up=thisElectionResult/100)) %>%
      mutate(date=dateSeq) %>%
      select(-xi) %>%
      mutate(type="Backcasted poll average")
    
    xi <- bind_rows(xi,xi_standard) %>%
      mutate(type=factor(type,levels=c("Poll average","Backcasted poll average")))
    
    thisCol <- c(thisCol,paste0(thisCol,"50"))
  }
  
  colVals <- thisCol
  names(colVals) <- levels(xi$type)
  
  require(ggplot2)
  g <- ggplot(xi,
              aes(x=date,y=mean*100,
                  ymin=lo*100,ymax=up*100,
                  group=type,color=type)) +
    geom_ribbon(data = xi %>% filter(type=="Poll average"),
                color="transparent",
                fill=gray(.85),alpha=.5) + 
    geom_line(size=3) + 
    scale_color_manual("",values=colVals) +
    geom_point(data=thePolls,
               inherit.aes=FALSE,
               aes(x=mid_date,y=y)) + 
    geom_hline(yintercept = pastElectionResult) +
    geom_hline(yintercept = thisElectionResult) + 
    annotate("text",x=min(xi$date),
             y=pastElectionResult+.1,
             label="Previous Result",
             vjust=0,hjust=0) +
    annotate("text",x=max(xi$date),
             y=thisElectionResult+.1,
             label=paste(year,"Result"),
             vjust=0,hjust=1) + 
    scale_y_continuous("Voting intention",minor_breaks = NULL) + 
    scale_x_date("",date_breaks = "3 months",
                 expand=c(0,0),
                 date_labels = "%b %y",
                 minor_breaks = NULL) + 
    theme_minimal() + 
    theme(legend.position=c(1,1),
          legend.justification = "right",
          legend.direction = "horizontal") +
    labs(title=paste(theParty,year,paste0("election cycle",","),
                     "error of poll average:",
                     round(error,1),
                     "percentage points"))
 
  forCasey <- xi %>% select(type,date,xi=mean,lo,up) %>% mutate(xi=xi*100,lo=lo*100,up=up*100)
  
  save(g,
       forCasey,
       thePolls,
       pastElectionResult,
       thisElectionResult,
       file=here(paste0("output/",year,"/",theParty,"_","summary.RData")))
  
  return(g)
}

## update sum_to-zero in past years
for(y in c(2019,2016,2013,2010,2007)){
  for(p in c("ALP","LNP","GRN","OTH","LNP2PP")){
    theOutFile <- here(
      paste("output",
            y,
            paste("xi",p,
                  "sum_to_zero.rds",sep="_"),
            sep="/")
    )
    if(!file.exists(theOutFile)){
      cat(paste("running for",p,y,"\n",sep=" "))
      run_wrapper(p,y)
    }

    ## graphics and things
    theOutFile <- here(paste0("output/",y,"/",p,"_","summary.RData"))
    if(!file.exists(theOutFile)){
      inspect(p,y,type="standard")
    }
    
  }
}
