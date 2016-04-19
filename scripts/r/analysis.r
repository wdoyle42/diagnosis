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
## of net price for each group in each state. 

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


statelist<-c("AK", "AL" ,"AR", "AZ", "CA", "CO", "CT",  "DE", "FL",
"GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
"ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ",
"NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
"TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")


## open institution data
inst <- read.csv('../../data/analysis/institutions.csv', stringsAsFactors = F)

inst<-dplyr::filter(inst,stabbr%in%(statelist))

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
#write.csv(afford,file=paste0(addir,"afford.csv"),row.names=FALSE)

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

#write.csv(afford_total, "../../output/reports/xlsx/afford_total_data.csv",row.names=FALSE)

# #Calculate single number by state, weighted by pop in that income group
afford_total_summary<-afford_total %>%
    group_by(year,stabbr) %>%
        summarize(ave_percent=weighted.mean(x=percent,w=inc_pct_pop,na.rm=TRUE))%>%
            mutate(rank=rank(ave_percent))

afford_total_summary$ave_percent=round(afford_total_summary$ave_percent,1)

afford_total_summary13<-filter(afford_total_summary,year==2013)

afford_total_summary13<-afford_total_summary13[order(afford_total_summary13$ave_percent),]

write.csv(afford_total_summary13,file="../../output/reports/xlsx/afford_total_summary13.csv",row.names=FALSE)

afford_total_summary08<-filter(afford_total_summary,year==2008)

afford_total_summary08<-afford_total_summary08[order(afford_total_summary08$rank),]

write.csv(afford_total_summary08,file="../../output/reports/xlsx/afford_total_summary08.csv",row.names=FALSE)

afford8<-afford_total_summary%>%
    filter(year==2008)

afford13<-afford_total_summary%>%
    filter(year==2013)

ac<-data.frame(afford8$stabbr,afford8$ave_percent,afford13$ave_percent)
names(ac)<-c("stabbr","afford08","afford13")
ac$change<-ac$afford08-ac$afford13
length(ac$change[ac$change<0])

## Collapse income categories: simple mean across groups

afford2<-afford %>%
    group_by(year,stabbr,group) %>%
        summarize(ave_percent=(mean(percent,na.rm=TRUE)))%>%
            group_by(year,group) %>%            
            mutate(quant=cut(ave_percent,
                   breaks=quantile(ave_percent,probs=seq(0,1,by=.2),na.rm=TRUE),
                   labels=my.labels,
                   include.lowest=TRUE
                             ),
                   rank=rank(ave_percent, na.last=FALSE)
               )

write.csv(afford2,file=paste0(addir,"afford_overall.csv"),row.names=FALSE)






