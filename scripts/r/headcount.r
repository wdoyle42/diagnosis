################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> headcount.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 13 February 2016
##
################################################################################

## PURPOSE

## The purpose of this file is to generate a file that has the number and percent
## of fte in each sector in each state.


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

## Pull EF<year>A to generate fte
## IPEDS enrollments (using EF<year>A files)

filenames <- paste0('EF',c(2008,2013),"A.zip")
cond <- 'efalevel == 3'
var <- c('unitid','eftotlt')
enroll.data <- build.dataset.ipeds(filenames=filenames,datadir=rddir,conditions=cond,vars=var,years=years)

## Merge Datasets
pattern <- '*\\.data\\b'; byvar <- c('unitid', 'year')
inst <- merge.ipeds(pattern = pattern, byvar = byvar)


## REPORT CATEGORIES -----------------------------------------------------------
## 1 = Public Doctoral (includes extensive and intensive) 4-year
## 2 = Public Non-Doctoral (all other 4-years)
## 3 = Private Doctoral (as above)
## 4 = Private Non-Doctoral (as above)
## 5 = Public 2 year (Carnegie Associate's dominant)
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

## aggregate at state level
inst<-inst %>%
    group_by(stabbr,year) %>%
        mutate(state_total_ug=sum(eftotlt,na.rm=TRUE)
               )

inst<-inst %>%
    group_by(stabbr,year,group) %>%
        summarize(sector_total_ug=sum(eftotlt,na.rm=TRUE),
                  state_total_ug=max(state_total_ug,na.rm=TRUE)
                  )
                  

inst$pct_headcount<-inst$sector_total_ug/inst$state_total_ug

write.csv(inst,paste0(addir,"headcount.csv"))
