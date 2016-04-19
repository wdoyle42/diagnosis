################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> loans.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> Jan 6 2016
##
################################################################################

## PURPOSE

## The purpose of this file is to generate a file that has the total amount of
## loans in each state, divided by FTE, for each sector and the state as a whole

## CODE

## clear memory
rm(list=ls())

## libraries
require(dplyr)

## data dirs
cddir <- '../../data/acs/'
mddir <- '../../data/misc/'
addir <- '../../data/analysis/'
rddir<-'../../data/ipeds/'
    

source('functions.r')

## Pull IPEDS header data
## IPEDS institutional characteristics (using HD files)

years<-c(2008,2013)

filenames=paste0("HD",c(2008,2013),".zip")
var <- c('unitid','instnm','city','stabbr','control','sector','carnegie', 'ccipug')
attr.data <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=years)

## Pull EFIA to generate fte
## IPEDS enrollments (using EFIA files)

filenames <- paste0('EFIA',c(2009,2014),".zip")
var <- c('unitid','fteug')
enroll.data <- build.dataset.ipeds(filenames=filenames,datadir = rddir, vars = var,years=years)

##SFA for loans


## IPEDS student netprice (using SFA files)

filenames<-paste0(c("SFA0809",
                    "SFA1314"),
                  ".zip"
                  )

var<-c('unitid','ufloant')

loan.data<-build.dataset.ipeds(filenames=filenames,datadir = rddir, vars = var,years=years)

## Merge Datasets
pattern <- '*\\.data\\b'; byvar <- c('unitid', 'year')
inst <- merge.ipeds(pattern = pattern, byvar = byvar)

## REPORT CATEGORIES -----------------------------------------------------------
##
## 1 = Public 2 year (>= 90% of degrees at Associate’s or certificate level)
## 2 = Public Non-Doctoral (all other 4-years)
## 3= Public Doctoral (includes extensive and intensive) 4-year
## 4 = Private Non-Doctoral (as above)
## 5 = Private Doctoral (as above)
## 
## 
## -----------------------------------------------------------------------------

## Initialize grouping
inst$group <- NA

## All public four years
inst$group[inst$sector==1] <- 2

#Reassign public doctoral
inst$group[inst$group==2 & inst$carnegie %in% c(15:16)]<-3

## All private four years
inst$group[inst$sector==2]<-4

## Reassign private doctoral              

inst$group[inst$group==4 & inst$carnegie %in% c(15:16)]<-5

## Public 2 years

inst$group[inst$sector==4]<-1

## Reassign any asscoiate dominant 2 years from 4 years

inst$group[inst$group==2 & inst$ccipug==2]<-1

inst<-filter(inst,is.na(group)==FALSE)

##Keep only select states
statelist<-c("AK", "AL" ,"AR", "AZ", "CA", "CO", "CT",  "DE", "FL",
"GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
"ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ",
"NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
"TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

inst<-filter(inst,stabbr%in%statelist)

## aggregate at state lavel
inst1<-inst %>%
    group_by(stabbr,year) %>%
        summarize(state_total_loan=sum(ufloant,na.rm=TRUE),
               state_total_ug=sum(fteug,na.rm=TRUE)
               )

inst1$loan_fte<-inst1$state_total_loan/inst1$state_total_ug

## Calculate loans/fte

write.csv(inst1,file=paste0(addir,"loans_overall.csv"))

inst<-ungroup(inst)

## aggregate by sector
inst2<-inst %>%
    group_by(year,stabbr, group) %>%
        summarize(sector_total_loan=sum(ufloant,na.rm=TRUE),
                  sector_total_ug=sum(fteug,na.rm=TRUE)
                  )

## Calculate loans/fte
inst2$loan_fte<-inst2$sector_total_loan/inst2$sector_total_ug
   
write.csv(inst2,file=paste0(addir,"loans_sector.csv"))

