################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> fte.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> Jan 6 2016
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

years<-c(2004,2007,2013)

filenames=paste0("HD",c(2004,2007,2013),".zip")
var <- c('unitid','instnm','stabbr','sector')
attr.data <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=years)

## Pull EFIA to generate fte
## IPEDS enrollments (using EFIA files)

filenames <- paste0('EFIA',c(2005,2008,2014),".zip")
var <- c('unitid','fteug')
enroll.data <- build.dataset.ipeds(filenames=filenames,datadir = rddir, vars = var,years=years)

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

## All public=1
inst$group[inst$sector%in%c(1,4,7)] <- 1

## All Private=2
inst$group[inst$sector%in%c(2,5,8)] <- 2

inst<-filter(inst,is.na(group)==FALSE)

## aggregate at state lavel
inst<-inst %>%
    group_by(stabbr,year) %>%
        mutate(state_total_ug=sum(fteug,na.rm=TRUE)
               )

inst<-inst %>%
    group_by(stabbr,year,group) %>%
        summarize(sector_total_ug=sum(fteug,na.rm=TRUE),
                  state_total_ug=max(state_total_ug,na.rm=TRUE)
                  )

inst$pct_fte<-inst$sector_total_ug/inst$state_total_ug

write.csv(inst,file=paste0(addir,"fte_two_sector.csv"),row.names=FALSE)


