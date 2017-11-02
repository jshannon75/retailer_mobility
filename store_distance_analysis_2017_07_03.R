library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(plm)
library(stargazer)
library(spdep)
library(car)
library(Hmisc)

###########################
## Set up data for models 
###########################

chain_select<-c("walmart","target","kroger","publix","ingles.mar","dollar.gen","family.dol","shell.food","chevron.fo","cvs.pharma")

modeldata<-read_csv("Data/storedist_tct_data_2017_07_14.csv") 

tracts_ua<-st_read("Data/Tract_UA_Atlanta_individual.shp")[,c(1,2,4)]

modeldata<-left_join(tracts_ua,modeldata) #Select just tracts in the Atlanta urban area
st_geometry(modeldata)<-NULL

#Calculate mean values of variables by chain and tract
modeldata_mean <- modeldata %>%
  gather(dist:snap_pct,key="var",value="value") %>%
  group_by(tractid,chain_name,var) %>%
  summarise(mean=mean(value)) %>%
  spread(var,mean)

modeldata_mean_wide<-modeldata_mean %>%
  dplyr::select(-dist) %>%
  spread(chain_name,dist_1k) 

#Read in tract data with demographic clusters
tracts_sp<-readOGR(".","tractdata_clusters")[,c(1,2,4)]

modeldata_wide_sp<-merge(tracts_sp,modeldata_mean_wide)
modeldata_wide_sp<-subset(modeldata_wide_sp,pop1k>0)


############################
## Fixed effects Models
############################

modeldata<-subset(modeldata,pop1k>0)

#Set up function for fixed effects models
model_fe<-function(chain123) {
  plm(lag(log(dist_1k),1)~afam_pct+asn_pct+hisp_pct+
        povpop_pct+hh150k_pct+snap_pct+popden1k,
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_result.htm")

#Testing just ACS variables
model_fe_acs<-function(chain123) {
  plm(lag(log(dist_1k),1)~afam_pct+asn_pct+hisp_pct+
        povpop_pct+hh150k_pct,
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe_acs)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_acs_result.htm")


###Examine residuals

hist(modeldata$popden1k)

hist(models[[1]]$residuals)
hist(models[[2]]$residuals)
hist(models[[3]]$residuals)
hist(models[[4]]$residuals)
hist(models[[5]]$residuals)
hist(models[[6]]$residuals)
hist(models[[7]]$residuals)
hist(models[[8]]$residuals)
hist(models[[9]]$residuals)
hist(models[[10]]$residuals)




###########################
#Correlation for mean values of model variables
##########################

modeldata_wide<-data.frame(modeldata_wide_sp) %>%
  gather(chevron.fo:walmart,key="chain_name",value="dist1k") %>%
  dplyr::select(c(20,21,4,6,9,13,8,16,12))

#Create functions for extracting coefficients from a correlation matrix
#Function below from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

correl_chain<-function(chain123){
  subdata<-subset(modeldata_wide,chain_name==chain123) %>% 
    dplyr::select(-chain_name)
  
  res2<-rcorr(as.matrix(subdata),type="spearman")
  matrix<-flattenCorrMatrix(res2$r, res2$P)
    round(cor(subdata),2)
  matrix$chain_name<-chain123
  matrix
}

#Apply the function and bind results
correl_result_list<-lapply(chain_select,correl_chain)
correl_result<-do.call("rbind", correl_result_list)

correl_result$chain_name<-factor(correl_result$chain_name,levels=chain_select)

#Add stars to show significance
correl_result_select<-correl_result %>%
  filter(row=="dist1k" & column!="dist1k") %>%
  mutate(cor=round(cor,2),
         p=round(p,2)) %>%
  dplyr::select(-row) %>%
  mutate(sig=ifelse(p<.005,"***",ifelse(p<.01,"**",ifelse(p<.05,"*",""))),
         cor_sig=paste(cor,sig,sep="")) %>%
  dplyr::select(-p,-cor,-sig)%>%
  spread(chain_name,cor_sig)
 
write_csv(correl_result_select,"dist1k_correlations_22_07_2017.csv")

#Check for multicollinearity
#While some coefficients are high, model results show little sensitivity to the inclusion of all variables
#Check on poverty & SNAP
cor(modeldata_wide$povpop_pct,modeldata_wide$snap_pct) #0.66
cor(modeldata_wide$afam_pct,modeldata_wide$snap_pct) #0.75
cor(modeldata_wide$povpop_pct,modeldata_wide$snap_pct) #0.66
cor(modeldata_wide$hisp_pct,modeldata_wide$snap_pct) #-0.11
cor(modeldata_wide$hisp_pct,modeldata_wide$povpop_pct) #0.26
cor(modeldata_wide$afam_pct,modeldata_wide$hh150k_pct) #-0.56