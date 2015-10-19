# PREP for analysis

# load packages
library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
#library(RGtk2)
library(caret)
library(reshape2)
library(rpart)
#library(pROC)
#library(Hmisc)
#library(gbm)
library(zoo)
library(rattle)
#library(glmulti)
#library(leaps)
library(shiny)
#library(shinyapps)
library(shinydashboard)
library(devtools)
library(rCharts)
library(qcc)
library(googleVis)
library(ggvis)
library(IRanges)
library(dygraphs)
library(rpart.plot)
library(randomForest)
library(party)


#read data
RUNSTATS<-data.table(read.csv("/home/andy/srv/shiny-server/FILTER_MANAGEMENT/DATA/RUNSTATS.csv")[,-1])
#format run data

RUNSTATS[,STRT:=ymd_hms(STRT)]
RUNSTATS[,END:=ymd_hms(END)]
RUNSTATS[,FILTER_ID:=factor(paste("STREAM",STREAM, "FILTER",FILTER))]
RUNSTATS[,RUN_HL :=(TERM_HL_NORM - CBHL_NORM)]
RUNSTATS[,RIPEN_HIGH:= ifelse(MAX_TURB_TIME < 3, 1,0)]
RUNSTATS[,TURB_DIFF :=(TURB_MEAN - STREAM_TURB_MEAN)]



RUNSTATS[,V1:=NULL]
#RUNSTATS[,END:=NULL]
#RUNSTATS[,RUN_TIME_HRS:=NULL]
RUNSTATS[,MAX_INT:=NULL]
RUNSTATS[,TURB_VAR:=NULL]
RUNSTATS[,TURB_ZEROS:=NULL]

# id failing runs
RUNSTATS$RUN_FAIL<- ifelse(RUNSTATS$T99 > 0.1  | RUNSTATS$TURB_MEAN > RUNSTATS$HIGH_LIMIT, 1, 0)

# Subset failing runs
FAIL<- RUNSTATS[RUNSTATS$RUN_FAIL == 1]

# get interval lookup of failing runs
RUN_INTERVALS<-IRanges(start = as.numeric(RUNSTATS$STRT),end = as.numeric(RUNSTATS$END), names = RUNSTATS$STREAM)
FAILURE_INTERVALS<-IRanges(start = as.numeric(FAIL$STRT),end = as.numeric(FAIL$END), names = FAIL$STREAM)


RUNSTATS[, CONCURRENT_FAILS:= countOverlaps(
          IRanges(start = as.numeric(STRT),end = as.numeric(END), names = STREAM),FAILURE_INTERVALS)]

RUNSTATS[, CONCURRENT_FAILS_STREAM:= countOverlaps(
          IRanges(start = as.numeric(STRT),end = as.numeric(END), names = STREAM),FAILURE_INTERVALS[names(FAILURE_INTERVALS)==STREAM]), by = STREAM]

# high level failure types
RUNSTATS$HIGH_PERIOD<-ifelse(RUNSTATS$MAX_TURB_TIME < 3,"EARLY", ifelse(RUNSTATS$MAX_TURB_TIME/RUNSTATS$RUN_TIME_HRS >0.7,"END", "MID"))

RUNSTATS$FILTERS_IMPACTED<-ifelse(RUNSTATS$CONCURRENT_FAILS == 1, "SINGLE FILTER", ifelse(RUNSTATS$CONCURRENT_FAILS == RUNSTATS$CONCURRENT_FAILS_STREAM, "SINGLE STREAM", "MULTI STREAM"))

RUNSTATS$FAILTYPEA<-ifelse(RUNSTATS$RUN_FAIL == 1 , paste( RUNSTATS$HIGH_PERIOD), "OK")
RUNSTATS$FAILTYPEB<-ifelse(RUNSTATS$RUN_FAIL == 1 , paste( RUNSTATS$FILTERS_IMPACTED,RUNSTATS$HIGH_PERIOD), "OK")

# get long df for control charting
RUNSTATS_MEL<- data.table(melt(RUNSTATS, id=c("STREAM", "FILTER", "FILTER_ID", "RUN","STRT", "END", "HIGH_PERIOD", "FILTERS_IMPACTED","FAILTYPEA","FAILTYPEB", "RUN_FAIL")))

str(RUNSTATS)
#RUNSTATS_MEL[, cut_val := cut(value, breaks = 7), by = variable]

RUNSTATS_MEL_FAIL<- data.table(RUNSTATS_MEL[RUNSTATS_MEL$RUN_FAIL ==1])

# set random number generation
set.seed(31)




# val = sample(1: nrow ( TRY_FILTER_TEST ) , nrow ( TRY_FILTER_TEST ) * .5)
# TRY_FILTER_VAL<-TRY_FILTER_TEST[val,]
# TRY_FILTER_TEST<-TRY_FILTER_TEST[-val,]

# TRAIN_MELT<-melt(TRY_FILTER_TRAIN, id = c("STREAM","FILTER","FILTER_ID","RUN_FAIL","RIPEN_HIGH","CONCURRENT_FAILS", "CONCURRENT_FAILS_STREAM", "HIGH_PERIOD", "FILTERS_IMPACTED","FAILTYPEA","FAILTYPEB"))
#TRAIN_MELT_HL<-melt(TRY_FILTER_TRAIN, id = c("STREAM","FILTER","FILTER_ID","RUN_FAIL", "RUN_HL"))


zoowrap<- function(x){ zoobit<- zoo(x$value, x$STRT)
return(zoobit)
}

zoowrap_cusum<- function(x){ zoobit<- zoo(x$cusum_day, x$STRT)
return(zoobit)
}

# Subset failing runs
FAILURES<- RUNSTATS[RUNSTATS$RUN_FAIL == 1]


# rad inference tab

INF_TAB<- read.csv("home/andy/srv/shiny-server/FILTER_MANAGEMENT/DATA/INFERENCE_TAB.csv")
INF_TAB<- melt(INF_TAB, id=1)

FAULT_LAB<- read.csv("home/andy/srv/shiny-server/FILTER_MANAGEMENT/DATA/FAULT_LABS.csv", stringsAsFactors = FALSE)
