################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> analysis.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 1 8 2016
##
################################################################################

## PURPOSE

## This file compares net price with family income in each state within each sector
## and outputs the results. It also create a data file with the aggregate levels
## of net price for each group in each state. This is just for three states with technical college systems: TN, OK, GA

## CODE

rm(list=ls())

##Libraries 
library(dplyr) 
library(tidyr)

##options
options(stringsAsFactors=FALSE)

## data dirs
cddir <- '../../data/acs/'
rddir <- '../../data/ipeds/'
mddir <- '../../data/misc/'
addir <- '../../data/analysis/'

##constants

my.labels=c("*****","****","***", "**" , "*")


statelist<-c("GA","OK","TN")


## open institution data
inst <- read.csv('../../data/analysis/institutions.csv', stringsAsFactors = F)

inst<-dplyr::filter(inst,stabbr%in%(statelist))

## Match with SREB data

sreb<-read.csv("../../data/misc/srebsectoripeds.csv")

sreb$sreb_group<-0

sreb$sreb_group[sreb$sector%in%c(61:69)]<-1

sreb<-dplyr::select(sreb,unitid,sreb_group)

inst<-left_join(inst,sreb,by="unitid")

inst$group[inst$sreb_group==1]<-6

## aggregate
inst <- inst %>% 
    group_by(year, stabbr, group, faminccat) %>%
        summarize(avecost = round(weighted.mean(x=netprice, w = fteug, na.rm = TRUE)))

## merge with acs

acs<-read.csv(paste0(addir,"states.csv"))

afford<-left_join(inst,acs,by=c("stabbr","year","faminccat"))

## calculate percentages
afford$percent<-(afford$avecost/afford$aveinc)*100

## need faminccat levels
levels <- c('< 30k','30k to 48k','48k to 75k','75k to 110k','> 110k')

## reorder so that < 30k is first
afford$faminccat <- factor(afford$faminccat, levels = levels)

## Star Ratings
afford<-afford %>%
    group_by(year,group,faminccat)%>%
        mutate(quant=cut(percent,
                   breaks=quantile(percent,probs=seq(0,1,by=.2),na.rm=TRUE),
                   labels=my.labels,
                   include.lowest=TRUE
                         )
               )
## Output results
write.csv(afford,file=paste0(addir,"afford_tech.csv"),row.names=FALSE)

## Create weighted average based on headcount
headcount<-read.csv(paste0(addir,"headcount.csv"))

afford_total<-left_join(afford,headcount,by=c("stabbr","group","year"))

## Weighted Net Price by Income level
afford_total<-afford_total%>%
    group_by(year,stabbr,faminccat) %>%
        summarize(net_price_ave=weighted.mean(x=avecost,w=sector_total_ug,na.rm=TRUE),
                  income=max(aveinc),
                  inc_pct_pop=max(inc_pct_pop)
                  )

## As percent of income
afford_total$percent<-(afford_total$net_price_ave/afford_total$income)*100

## Output

write.csv(afford_total, paste0(addir,"afford_total_data_tech.csv"),row.names=FALSE)

