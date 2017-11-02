library(acs)
library(tidyverse)
library(mapview)
library(sf)
library(rmapshaper)



#############################
# Import census income data #
#############################


#Income
#############


income15<-read.acs("Data/Tract Data/Income/ACS_15_5YR_B19001_with_ann.csv",endyear=2015)
income14<-read.acs("Data/Tract Data/Income/ACS_14_5YR_B19001_with_ann.csv",endyear=2014)
income13<-read.acs("Data/Tract Data/Income/ACS_13_5YR_B19001_with_ann.csv",endyear=2013)
income12<-read.acs("Data/Tract Data/Income/ACS_12_5YR_B19001_with_ann.csv",endyear=2012)
income11<-read.acs("Data/Tract Data/Income/ACS_11_5YR_B19001_with_ann.csv",endyear=2011)
income10<-read.acs("Data/Tract Data/Income/ACS_10_5YR_B19001_with_ann.csv",endyear=2010)

tract_geog<-geography(income15)[,1:2]
names(tract_geog)<-c("rownames","tractid")

#Import df, add year, and just take columns for $100K or more
income15_est<-data.frame(estimate(income15))[,c(1,14:17)] %>% mutate(year="Y2013",tractid=tract_geog$tractid)
income14_est<-data.frame(estimate(income14))[,c(1,14:17)] %>% mutate(year="Y2012",tractid=tract_geog$tractid)
income13_est<-data.frame(estimate(income13))[,c(1,14:17)] %>% mutate(year="Y2011",tractid=tract_geog$tractid)
income12_est<-data.frame(estimate(income12))[,c(1,14:17)] %>% mutate(year="Y2010",tractid=tract_geog$tractid)
income11_est<-data.frame(estimate(income11))[,c(1,14:17)] %>% mutate(year="Y2009",tractid=tract_geog$tractid)
income10_est<-data.frame(estimate(income10))[,c(1,14:17)] %>% mutate(year="Y2008",tractid=tract_geog$tractid)

#Combine datasets and columns
income_est<-rbind(income15_est,income14_est,income13_est,income12_est,income11_est,income10_est)
income_est$totalhh<-income_est$HD01_VD01.Estimate..Total.
income_est$hh100k_up<-rowSums(income_est[,2:5])
income_est$hh150k_up<-rowSums(income_est[,4:5])
income_est[,1:5]<-NULL

#Hispanic/African American
##########################

race15<-read.acs("Data/Tract Data/Race Hispanic/ACS_15_5YR_B03002_with_ann.csv",endyear=2015)
race14<-read.acs("Data/Tract Data/Race Hispanic/ACS_14_5YR_B03002_with_ann.csv",endyear=2014)
race13<-read.acs("Data/Tract Data/Race Hispanic/ACS_13_5YR_B03002_with_ann.csv",endyear=2013)
race12<-read.acs("Data/Tract Data/Race Hispanic/ACS_12_5YR_B03002_with_ann.csv",endyear=2012)
race11<-read.acs("Data/Tract Data/Race Hispanic/ACS_11_5YR_B03002_with_ann.csv",endyear=2011)
race10<-read.acs("Data/Tract Data/Race Hispanic/ACS_10_5YR_B03002_with_ann.csv",endyear=2010)

#Collect columns
race15_est<-data.frame(estimate(race15))[,c(1,4,6,12)] %>% mutate(year="Y2013",tractid=tract_geog$tractid)
race14_est<-data.frame(estimate(race14))[,c(1,4,6,12)] %>% mutate(year="Y2012",tractid=tract_geog$tractid)
race13_est<-data.frame(estimate(race13))[,c(1,4,6,12)] %>% mutate(year="Y2011",tractid=tract_geog$tractid)
race12_est<-data.frame(estimate(race12))[,c(1,4,6,12)] %>% mutate(year="Y2010",tractid=tract_geog$tractid)
race11_est<-data.frame(estimate(race11))[,c(1,4,6,12)] %>% mutate(year="Y2009",tractid=tract_geog$tractid)
race10_est<-data.frame(estimate(race10))[,c(1,4,6,12)] %>% mutate(year="Y2008",tractid=tract_geog$tractid)

#Combine datasets and columns
race_est<-rbind(race15_est,race14_est,race13_est,race12_est,race11_est,race10_est)
names(race_est)<-c("tractpop","afam_pop","asn_pop","hisp_pop","year","tractid")

#Poverty
################

poverty15<-read.acs("Data/Tract Data/Poverty/ACS_15_5YR_C17002_with_ann.csv", endyear=2015)
poverty14<-read.acs("Data/Tract Data/Poverty/ACS_14_5YR_C17002_with_ann.csv", endyear=2014)
poverty13<-read.acs("Data/Tract Data/Poverty/ACS_13_5YR_C17002_with_ann.csv", endyear=2013)
poverty12<-read.acs("Data/Tract Data/Poverty/ACS_12_5YR_C17002_with_ann.csv", endyear=2012)
poverty11<-read.acs("Data/Tract Data/Poverty/ACS_11_5YR_C17002_with_ann.csv", endyear=2011)
poverty10<-read.acs("Data/Tract Data/Poverty/ACS_10_5YR_C17002_with_ann.csv", endyear=2010)


poverty15_est<-data.frame(estimate(poverty15))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2013",tractid=tract_geog$tractid)
poverty14_est<-data.frame(estimate(poverty14))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2012",tractid=tract_geog$tractid)
poverty13_est<-data.frame(estimate(poverty13))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2011",tractid=tract_geog$tractid)
poverty12_est<-data.frame(estimate(poverty12))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2010",tractid=tract_geog$tractid)
poverty11_est<-data.frame(estimate(poverty11))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2009",tractid=tract_geog$tractid)
poverty10_est<-data.frame(estimate(poverty10))[,c(1,2,3,4,5,6)] %>% mutate(year="Y2008",tractid=tract_geog$tractid)

poverty_est<-rbind(poverty15_est,poverty14_est,poverty13_est,poverty12_est,poverty11_est,poverty10_est)
poverty_est$totpop_pov<-poverty_est$HD01_VD01.Estimate..Total.
poverty_est$povpop<-rowSums(poverty_est[,2:3])
poverty_est$povpop185<-rowSums(poverty_est[,2:6])
poverty_est[,1:6]<-NULL

# Age
############

age15<-read.acs("Data/Tract Data/Age groups/ACS_15_5YR_S0101_with_ann.csv", endyear=2015)
age14<-read.acs("Data/Tract Data/Age groups/ACS_14_5YR_S0101_with_ann.csv", endyear=2014)
age13<-read.acs("Data/Tract Data/Age groups/ACS_13_5YR_S0101_with_ann.csv", endyear=2013)
age12<-read.acs("Data/Tract Data/Age groups/ACS_12_5YR_S0101_with_ann.csv", endyear=2012)
age11<-read.acs("Data/Tract Data/Age groups/ACS_11_5YR_S0101_with_ann.csv", endyear=2011)
age10<-read.acs("Data/Tract Data/Age groups/ACS_10_5YR_S0101_with_ann.csv", endyear=2010)

age15_est<-data.frame(estimate(age15))[,c(1,58,61,84)] %>% mutate(year="Y2013",tractid=tract_geog$tractid)
age14_est<-data.frame(estimate(age14))[,c(1,58,61,84)] %>% mutate(year="Y2012",tractid=tract_geog$tractid)
age13_est<-data.frame(estimate(age13))[,c(1,58,61,84)] %>% mutate(year="Y2011",tractid=tract_geog$tractid)
age12_est<-data.frame(estimate(age12))[,c(1,58,61,84)] %>% mutate(year="Y2010",tractid=tract_geog$tractid)
age11_est<-data.frame(estimate(age11))[,c(1,58,61,84)] %>% mutate(year="Y2009",tractid=tract_geog$tractid)
age10_est<-data.frame(estimate(age10))[,c(1,58,61,84)] %>% mutate(year="Y2008",tractid=tract_geog$tractid)

age_est<-rbind(age15_est,age14_est,age13_est,age12_est,age11_est,age10_est)
age_est$totpop_age<-age_est$HC01_EST_VC01.Total..Estimate..Total.population
age_est$under18<-round(((age_est$HC01_EST_VC23.Total..Estimate..SELECTED.AGE.CATEGORIES...5.to.14.years+
                           age_est$HC01_EST_VC24.Total..Estimate..SELECTED.AGE.CATEGORIES...15.to.17.years)/100)*
                         age_est$HC01_EST_VC01.Total..Estimate..Total.population,0)
age_est$over64<-round((age_est$HC03_EST_VC31.Female..Estimate..SELECTED.AGE.CATEGORIES...65.years.and.over/100) * age_est$totpop_age,0)
age_est[,1:4]<-NULL

# Combine tractdata & create percentages
########################################

tract_est<-left_join(poverty_est,income_est) %>%
  left_join(.,race_est) %>%
  left_join(.,age_est)

tract_est_pct<-tract_est %>%
  mutate(
    povpop_pct=povpop/totpop_pov*100,
    povpop185_pct=povpop185/totpop_pov*100,
    hh100k_pct=hh100k_up/totalhh*100,
    hh150k_pct=hh150k_up/totalhh*100,
    afam_pct=afam_pop/tractpop*100,
    asn_pct=asn_pop/tractpop*100,
    hisp_pct=hisp_pop/tractpop*100,
    under18_pct=under18/totpop_age*100,
    over64_pct=over64/totpop_age*100
  )

tract_est_pct$gisjn_tct<-paste("G",tract_est_pct$tractid,sep="")

# Add store data
#####################
stores<-read_csv("GA_SNAPstores_2008_2016_GeoID_dummy.csv")

storecnt_tract<-stores[,c(1:7,16,32:78,30)] %>%
  gather(Y2008,Y2009,Y2010,Y2011,Y2012,Y2013,key="year",value="count") %>%
  gather(walmart:big.lots,key="chain",value="store_count") %>%
  filter(count==1&store_count==1) %>%
  group_by(GISJN_TCT,year,chain) %>%
  summarise(st_count=sum(store_count)) %>%
  spread(chain,st_count)

storecnt_tract[is.na(storecnt_tract)]<-0
storecnt_tract$gisjn_tct<-storecnt_tract$GISJN_TCT
storecnt_tract$GISJN_TCT<-NULL
  
###Join to demographic data
tracts_stcount<-left_join(tract_est_pct,storecnt_tract)
tracts_stcount[is.na(tracts_stcount)]<-0

write.csv(tracts_stcount,"GAtracts_stcount.csv",row.names=FALSE)
