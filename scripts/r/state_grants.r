################################################################################
## State Grants
## Getting state grants files in shape
## Will Doyle
## 2/26/16
################################################################################

rm(list=ls())


library(dplyr)
library(readxl)
library(tidyr)

## Constants

statelist<-c("AK", "AL" ,"AR", "AZ", "CA", "CO", "CT",  "DE", "FL",
"GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
"ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ",
"NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
"TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")


names<-c("statename",
         "need_only",
         "need_merit",
         "merit",
         "special",
         "uncategorized"
         )

changestates<-c("Alabama",
                "California",
                "Iowa",
                "Maryland",
                "Michigan",
                "Texas",
                "West Virginia"
                )

## read in statename data
statename<-read.csv("../../data/misc/statename.csv")
##statename<-select(statename,-c)

pub_aid<-NULL
## set year, later loop
for (year in c(2004,2007,2013)) {

    rm(public_aid)

    public_aid<-read_excel(path="../../data/nassgap/nassgap_public.xlsx",
                        sheet=paste0(year),skip=0)

    names(public_aid)[1:6]<-names

    public_aid<-select(public_aid,one_of(names))

    public_aid<-left_join(public_aid,statename,by="statename")

    public_aid<-filter(public_aid,stabbr%in%statelist)

    ## Add need and need-merit for change states
    public_aid$need_only[public_aid$statename%in%changestates]<-
        public_aid$need_only[public_aid$statename%in%changestates]+
            public_aid$need_merit[public_aid$statename%in%changestates]

    ## Change need-merit to 0 for those change states
    public_aid$need_merit[public_aid$statename%in%changestates]<-0
    
    ## Create new other category
    public_aid$other<-public_aid$need_merit+
        public_aid$merit+
            public_aid$special+
                public_aid$uncategorized


    ## Define year
    public_aid$year<-year

    ## Select only relevant columns

    public_aid<-public_aid%>%
        select(stabbr,statename,year,need_only,other)

    ##Tidy

    public_aid<-public_aid%>%
        gather(key=aidtype,value=aid,-statename,-year,-stabbr)

    pub_aid<-rbind(pub_aid,public_aid)

    
}


## Merge with FTE

fte2<-read.csv("../../data/analysis/fte_two_sector.csv")

fte2<-filter(fte2,group==1&stabbr%in%statelist)

pub_aid<-left_join(pub_aid,fte2,by=c("stabbr","year"))

## Create Key Indicator
pub_aid$aid_fte<-pub_aid$aid/pub_aid$sector_total_ug

## Create National Average

pub_aid<-pub_aid%>%
    group_by(year,aidtype)%>%
        mutate(nat_sum=sum(aid,na.rm=TRUE),
               all_ug=sum(sector_total_ug,na.rm=TRUE)
               )

pub_aid$avg_fte<-pub_aid$nat_sum/pub_aid$all_ug

#Ranking

pub_aid<-
    pub_aid%>%
        group_by(year,aidtype) %>%
            mutate(ranking=rank(-aid_fte))

##Output File
write.csv(pub_aid,file="../../data/analysis/pub_aid.csv",row.names=FALSE)

##Privates

priv_aid<-NULL
## set year, later loop
for (year in c(2004,2007,2013)) {

    rm(private_aid)

    private_aid<-read_excel(path="../../data/nassgap/nassgap_private.xlsx",
                            sheet=paste0(year),skip=1)

    names(private_aid)[1:6]<-names

    private_aid<-select(private_aid,one_of(names))

    private_aid<-left_join(private_aid,statename,by="statename")

    private_aid<-filter(private_aid,stabbr%in%statelist)

    ## Add need and need-merit for change states
    private_aid$need_only[private_aid$statename%in%changestates]<-
        private_aid$need_only[private_aid$statename%in%changestates]+
            private_aid$need_merit[private_aid$statename%in%changestates]

    ## Change need-merit to 0 for those change states
    private_aid$need_merit[private_aid$statename%in%changestates]<-0
    
    ## Create new other category
    private_aid$other<-private_aid$need_merit+
        private_aid$merit+
            private_aid$special+
                private_aid$uncategorized


    ## Define year
    private_aid$year<-year

    ## Select only relevant columns

    private_aid<-private_aid%>%
        select(stabbr,statename,year,need_only,other)

    ##Tidy

    private_aid<-private_aid%>%
        gather(key=aidtype,value=aid,-statename,-year,-stabbr)

    priv_aid<-rbind(priv_aid,private_aid)

    
}

## Merge with FTE

fte2<-read.csv("../../data/analysis/fte_two_sector.csv")

fte2<-filter(fte2,group==2&stabbr%in%statelist)

priv_aid<-left_join(priv_aid,fte2,by=c("stabbr","year"))

## Create Key Indicator
priv_aid$aid_fte<-priv_aid$aid/priv_aid$sector_total_ug

## Create National Average

priv_aid<-priv_aid%>%
    group_by(year,aidtype)%>%
        mutate(nat_sum=sum(aid,na.rm=TRUE),
               all_ug=sum(sector_total_ug,na.rm=TRUE)
               )

priv_aid$avg_fte<-priv_aid$nat_sum/priv_aid$all_ug

## Create ranking

priv_aid<-
    priv_aid%>%
        group_by(year,aidtype) %>%
            mutate(ranking=rank(-aid_fte))


write.csv(priv_aid,file="../../data/analysis/priv_aid.csv")
