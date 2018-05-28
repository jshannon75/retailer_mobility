library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(plm)
library(stargazer)
library(spdep)
library(car)
library(Hmisc)
library(ggbeeswarm)

##############################
## Set up data for models ####


storedist_modeldata<-read_csv("storedist_modeldata_2018_03_10.csv") 
chain_select<-read_csv("atl_stlist_30more_2018_03_03.csv") %>%
  filter(STTYPE!="Category") %>%
  dplyr::select(st_name)
chain_select<-chain_select$st_name
chain_type<-storedist_modeldata %>%
  select(store,st_name,STTYPE,sttype2) %>%
  distinct()

cpal<-c("#d7191c", "#d8b365", "#2b83ba")

# modeldata_mean <- modeldata %>%
#   gather(dist:snap_pct,key="var",value="value") %>%
#   group_by(tractid,chain_name,var) %>%
#   summarise(mean=mean(value)) %>%
#   spread(var,mean)
# 
# modeldata_mean_wide<-modeldata_mean %>%
#   dplyr::select(-dist) %>%
#   spread(chain_name,dist_1k) 
# 
# tracts_sp<-readOGR(".","tractdata_clusters")[,c(1,2,4)]
# #tracts_sp<-subset(tracts_sp,Atl_Core==1)
# 
# modeldata_wide_sp<-merge(tracts_sp,modeldata_mean_wide)
# modeldata_wide_sp<-subset(modeldata_wide_sp,pop1k>0)





############################
## Fixed effects Models ####

model_fe_D1<-function(chain123,dv) {
  plm(log(D1)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(snap_pct,1)+lag(popden1k,1),
      data=storedist_modeldata[storedist_modeldata$st_name==chain123,],
      index=c("tract_id","year"))
}

models.d1<-lapply(chain_select,model_fe_D1)
models.d1_broom<-lapply(models.d1,broom::glance)
models.d1_broom_df<-bind_rows(models.d1_broom) %>%
  mutate(st_name=chain_select,
         model="D1")

#stargazer(models,title="Fixed effects models_D1",column.labels=chain_select,type="html",out="femodels_D1_2018_03_03.htm")

model_fe_D2<-function(chain123,dv) {
  plm(log(D2)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(snap_pct,1)+lag(popden1k,1),
      data=storedist_modeldata[storedist_modeldata$st_name==chain123,],
      index=c("tract_id","year"))
}

models.d2<-lapply(chain_select,model_fe_D2)
models.d2_broom<-lapply(models.d2,broom::glance)
models.d2_broom_df<-bind_rows(models.d2_broom) %>%
  mutate(st_name=chain_select,
         model="D2")

model_fe_D3<-function(chain123,dv) {
  plm(log(D3)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(snap_pct,1)+lag(popden1k,1),
      data=storedist_modeldata[storedist_modeldata$st_name==chain123,],
      index=c("tract_id","year"))
}

models.d3<-lapply(chain_select,model_fe_D3)
models.d3_broom<-lapply(models.d3,broom::glance)
models.d3_broom_df<-bind_rows(models.d3_broom) %>%
  mutate(st_name=chain_select,
         model="D3")

#stargazer(models,title="Fixed effects models_D3",column.labels=chain_select,type="html",out="femodels_D3_2018_03_03.htm")

model_fe_D4<-function(chain123,dv) {
  plm(log(D4)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(snap_pct,1)+lag(popden1k,1),
      data=storedist_modeldata[storedist_modeldata$st_name==chain123,],
      index=c("tract_id","year"))
}

models.d4<-lapply(chain_select,model_fe_D4)
models.d4_broom<-lapply(models.d4,broom::glance)
models.d4_broom_df<-bind_rows(models.d4_broom) %>%
  mutate(st_name=chain_select,
         model="D4")

model_fe_D5<-function(chain123,dv) {
  plm(log(D5)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(snap_pct,1)+lag(popden1k,1),
      data=storedist_modeldata[storedist_modeldata$st_name==chain123,],
      index=c("tract_id","year"))
}

models.d5<-lapply(chain_select,model_fe_D5)
models.d5_broom<-lapply(models.d5,broom::glance)
models.d5_broom_df<-bind_rows(models.d5_broom) %>%
  mutate(st_name=chain_select,
         model="D5")

models_all<-models.d1_broom_df %>%
  bind_rows(models.d2_broom_df) %>%
  bind_rows(models.d3_broom_df) %>%
  bind_rows(models.d4_broom_df) %>%
  bind_rows(models.d5_broom_df) %>%
  left_join(chain_type) %>%
  mutate(sttype2=factor(sttype2,
                        levels=c("Large retailer","Combination","Convenience store",
                                 "Category"))) 
models_all_graph<-models_all %>%
  filter(sttype2!="Category")

##Visualize global diagnostics
ggplot(models_all_graph,aes(x=model,y=r.squared,group=st_name,color=sttype2)) +
  geom_point() + geom_line() +
  geom_text(aes(label=if_else(model=="D5",as.character(st_name),'')),
            hjust=0.2,vjust=-0.3,color="black")+
  theme_minimal()+
  scale_colour_manual(values=cpal)+
  theme(legend.position="none")+
  labs(x="",y="R2 value")+
  facet_grid(sttype2~.,switch="y")

##Summarise all stores
models_all_mean<-models_all %>%
  filter(sttype2=="Category") %>%
  group_by(st_name) %>%
  summarise(meanr2=mean(r.squared))

###########################
## Model coefficients
###########################

models.d1_tidy<-lapply(models.d1,broom::tidy)
chain_select_d1<-paste(chain_select,"_D1",sep="")
names(models.d1_tidy)<-chain_select_d1
models.d1_tidy_df<-bind_rows(models.d1_tidy,.id="store")

models.d2_tidy<-lapply(models.d2,broom::tidy)
chain_select_d2<-paste(chain_select,"_D2",sep="")
names(models.d2_tidy)<-chain_select_d2
models.d2_tidy_df<-bind_rows(models.d2_tidy,.id="store")

models.d3_tidy<-lapply(models.d3,broom::tidy)
chain_select_d3<-paste(chain_select,"_D3",sep="")
names(models.d3_tidy)<-chain_select_d3
models.d3_tidy_df<-bind_rows(models.d3_tidy,.id="store")

models.d4_tidy<-lapply(models.d4,broom::tidy)
chain_select_d4<-paste(chain_select,"_D4",sep="")
names(models.d4_tidy)<-chain_select_d4
models.d4_tidy_df<-bind_rows(models.d4_tidy,.id="store") 

models.d5_tidy<-lapply(models.d5,broom::tidy)
chain_select_d5<-paste(chain_select,"_D5",sep="")
names(models.d5_tidy)<-chain_select_d5
models.d5_tidy_df<-bind_rows(models.d5_tidy,.id="store") 

var_labels<-unique(models.d5_tidy_df$term)[1:6]
var_labels2<-c("% African-American","% Asian-American","% Hispanic","% HH in poverty",
               "% HH w/$150k income","% w/SNAP")
labels<-data.frame(var_labels,var_labels2) %>%
  rename("term"=var_labels)

models_tidy<-models.d1_tidy_df %>%
  bind_rows(models.d2_tidy_df) %>%
  bind_rows(models.d3_tidy_df) %>%
  bind_rows(models.d4_tidy_df) %>%
  bind_rows(models.d5_tidy_df) %>%
  separate(store,c("st_name","var"),sep="_") %>%
  left_join(chain_type) %>%
  filter(p.value<0.05 & term!="lag(popden1k, 1)") %>%
  left_join(labels) %>%
  mutate(sttype2=factor(sttype2,
                        levels=c("Large retailer","Combination","Convenience store","Category")))

models_tidy_graph<-models_tidy %>%
  filter(sttype2!="Category")

# ggplot(models_tidy_graph,aes(x=var,y=estimate,color=sttype2)) +
#   geom_quasirandom(width=0.02,dodge.width=0.5)+
#   facet_wrap(~var_labels2) + 
#   theme_minimal() +
#   scale_colour_manual(values=cpal)

ggplot(models_tidy_graph,aes(x=var,y=estimate,group=store,color=sttype2)) +
  geom_point()+geom_line()+
  theme_minimal() +
  scale_colour_manual(values=cpal)+
  facet_wrap(~var_labels2) 

#Bar graph just for D3
atl_stlist <- read_csv("atl_stlist_30more_2018_03_03.csv") %>%
  filter(sttype2!="Category") %>%
  arrange(sttype2,desc(st_name)) #Order by store type and store name
chain_select<-unique(atl_stlist$st_name)

models_tidy_graph_D3<-models_tidy %>%
  filter(var=="D3" & sttype2!="Category") %>% #Subset the models
  mutate(st_name=factor(st_name,levels=chain_select),
         ci_low=estimate-2*std.error, #Can use more complicated t score in the futrue if need be
         ci_high=estimate+2*std.error) %>%
  dplyr::select(-statistic,-p.value,-std.error) %>%
  gather(estimate,ci_low,ci_high,key="pointtype",value="value") 

ggplot(models_tidy_graph_D3,aes(y=value,x=reorder(st_name,sttype2),color=sttype2)) +
  geom_point(data=models_tidy_graph_D3[models_tidy_graph_D3$pointtype=="estimate",],
             size=1.8)+
  geom_line(size=0.7)+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_hline(yintercept=0,color="black")+
  scale_colour_manual(values=cpal)+
  ylab("Model coefficient and confidence interval")+xlab("")+
  facet_wrap(~var_labels2,scales="free_y")

#Create table for average coefficient by store
models_tidy_table<-models_tidy %>%
  group_by(st_name,var_labels2,sttype2) %>%
  summarise(var_mean=round(mean(estimate),3)) %>%
  spread(var_labels2,var_mean) %>%
  arrange(sttype2)

write_csv(models_tidy_table,"Models_coeftable_2018_03_05.csv")





#Correlation for mean values of model variables ####
modeldata_wide<-data.frame(storedist_modeldata) %>%
  dplyr::select(gisjn_tct,st_name,D3,afam_pct,asn_pct,hisp_pct,povpop_pct,hh150k_pct,snap_pct,popden1k) %>%
  gather(D3:popden1k,key="var",value="value") %>%
  group_by(gisjn_tct,st_name,var) %>%
  summarise(mean_value=mean(value)) %>%
  filter(is.na(mean_value)==FALSE) %>%
  spread(var,mean_value) 
modeldata_wide$gisjn_tct<-NULL

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
  subdata<-subset(modeldata_wide,st_name==chain123) %>% 
    dplyr::select(-st_name)
  
  res2<-rcorr(as.matrix(subdata),type="spearman")
  matrix<-flattenCorrMatrix(res2$r, res2$P)
  round(cor(subdata),2)
  matrix$chain_name<-chain123
  matrix
}

correl_result_list<-lapply(chain_select,correl_chain)
correl_result<-do.call("rbind", correl_result_list)

correl_result$chain_name<-factor(correl_result$chain_name,levels=chain_select)

correl_result_select<-correl_result %>%
  filter(row=="D3" | column=="D3") %>%
  mutate(cor=round(cor,2),
         p=round(p,2),
         column=as.character(column),
         row=as.character(row),
         variable=if_else(row=="D3",column,row)) %>%
  dplyr::select(-row,-column) %>%
  mutate(sig=ifelse(p<.005,"***",ifelse(p<.01,"**",ifelse(p<.05,"*",""))),
         cor_sig=paste(cor,sig,sep="")) %>%
  dplyr::select(-p,-cor,-sig)%>%
  spread(chain_name,cor_sig)

write_csv(correl_result_select,"D3_correlations_2018_03_30.csv")


##Attempt at heat map approach
ggplot(correl_result_wide,aes(chain_name,column))+
  geom_tile(aes(fill=Y1))+
  scale_fill_brewer(palette="RdYlGn")+
  theme_minimal()












##########################
##Old models

model_fe_inc<-function(chain123) {
  plm(log(dist_1k)~lag(povpop_pct,1)+lag(popden1k,1),
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe_inc)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_inc_2017_10_19.htm")


#Break models out by race/class/SNAP
model_fe<-function(chain123) {
  plm(log(dist_1k)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        +lag(popden1k,1),
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_race_2017_07_24.htm")

model_fe<-function(chain123) {
  plm(log(dist_1k)~lag(afam_pct,1)+lag(asn_pct,1)+lag(hisp_pct,1)+
        lag(povpop_pct,1)+lag(hh150k_pct,1),
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_acs_2017_07_27.htm")


model_fe<-function(chain123) {
  plm(log(dist_1k)~lag(povpop_pct,1)+lag(hh150k_pct,1)+lag(popden1k,1),
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_income_2017_07_24.htm")


model_fe<-function(chain123) {
  plm(log(dist_1k)~lag(snap_pct,1)+lag(popden1k,1),
      data=modeldata[modeldata$chain_name==chain123,],
      index=c("tractid","year"))
}

models<-lapply(chain_select,model_fe)

stargazer(models,title="Fixed effects models",column.labels=chain_select,type="html",out="femodels_snap_2017_07_24.htm")

###Interpret coefficents as 1/100 of the rate of increase/decrease in distance per unit change.

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



#####################
# Spatial regression
#####################

#writeOGR(modeldata_wide_sp,".","tractdata_ua_distmean_2017_07_12",driver="ESRI Shapefile")

#Read weights
q4_wt<-read.gal("tractdata_ua_distmean_2017_07_12_q4wt.gal",region.id=modeldata_wide_sp$gisjn_tct)
q4<-nb2listw(q4_wt)

#Look at residuals
model<-lm(publix~afam_pct+asn_pct+povpop_pct+hh150k_pct+snap1k+pop1k+popden1k,
              data=modeldata_wide_sp)

modeldata_wide_sp$residuals<-residuals(model)
moran.mc(modeldata_wide_sp$residuals,q4,99)


#Model
lm.LMtests(model, q4, test="all")
model_lag<-lagsarlm(shell.food~afam_pct+asn_pct+povpop_pct+hh150k_pct+snap_pct+popden1k,modeldata_wide_sp,q4)
summary(model_lag)
bptest.sarlm(model_lag)


model_err<-errorsarlm(publix~afam_pct+asn_pct+hisp_pct+povpop_pct+hh150k_pct+popden1k+snap1k,modeldata_wide_sp,q4)
summary(model_err)
bptest.sarlm(model_lag)


##Apply error model to list
chain_select<-c("walmart","target","kroger","publix","ingles.mar","dollar.gen","family.dol","shell.food","chevron.fo","cvs.pharma")

model_err<-function(chain123){
  var<-subset(modeldata_wide_sp,select=c("gisjn_tct",chain123))
  names(var)<-c("gisjn_tct","dist1k")
  var<-data.frame(var)
  modeldata_wide_sp<-merge(modeldata_wide_sp,var,by="gisjn_tct")
  errorsarlm(log(dist1k)~afam_pct+asn_pct+hisp_pct+povpop_pct+hh150k_pct+snap_pct+popden1k,modeldata_wide_sp,q4)
}

errormodels<-lapply(chain_select,model_err)

stargazer(errormodels,title="Spatial error models",column.labels=chain_select,type="html",out="errormodels_ua_2017_07_21.htm")


##With interaction terms
model_err_int<-function(chain123){
  var<-subset(modeldata_wide_sp,select=c("gisjn_tct",chain123))
  names(var)<-c("gisjn_tct","dist1k")
  var<-data.frame(var)
  modeldata_wide_sp<-merge(modeldata_wide_sp,var,by="gisjn_tct")
  errorsarlm(dist1k~afam_pct*povpop_pct+asn_pct*povpop_pct+povpop_pct+hh150k_pct+snap_pct+log(popden1k)+pop1k,modeldata_wide_sp,q4)
}

summary(model_err_int("publix"))

errormodels<-lapply(chain_select,model_err_int)

stargazer(errormodels,title="Spatial error models",column.labels=chain_select,type="html",out="errormodels_interact_ua_2017_07_14.htm")


###########################
##Cross sectional models (Now using spatial error model instead...)
###########################

###Check variable correlation
modeldata_mean <- modeldata %>%
  gather(dist:snap_pct,key="var",value="value") %>%
  group_by(tractid,chain_name,var) %>%
  summarise(mean=mean(value)) %>%
  spread(var,mean)


modelvar<-modeldata_mean[,c(6,3,4,9,12,8,11,15)] %>% filter(pop1k>0)
modeldata_cor<-data.frame(cor(modelvar)) %>%
  mutate(var2=row.names(.)) %>%
  gather(dist_1k:snap1k,key="var1",value="value")
ggplot(modeldata_cor,aes(var1,var2))+
  geom_tile(aes(fill=value))+
  scale_fill_distiller(palette = "Spectral")+
  geom_text(aes(label=round(value,2)))


###Modeling

model_lm<-function(chain123) {
  lm(log(dist_1k)~afam_pct*povpop_pct+asn_pct*povpop_pct+hisp_pct*povpop_pct+hh150k_pct+pop1k+snap1k,
     data=modeldata_mean[modeldata_mean$chain_name==chain123,])}

summary(model_lm("publix"))

modelresult_lm<-lapply(chain_select,model_lm)
stargazer(modelresult_lm,title="Linear regresison models",column.labels=chain_select,type="html",out="lmmodels_2017_07_11.htm")

model_all<-lm(log(dist_1k)~afam_pct+asn_pct+povpop_pct+hh150k_pct+snap1k+pop1k,
              data=modeldata_mean)

sqrt(vif(model_all))>2
hist(model_all$residuals)

hist(modelresult_lm[[1]]$residuals)
hist(modelresult_lm[[2]]$residuals)
hist(modelresult_lm[[3]]$residuals)
hist(modelresult_lm[[4]]$residuals)
hist(modelresult_lm[[5]]$residuals)
hist(modelresult_lm[[6]]$residuals)
hist(modelresult_lm[[7]]$residuals)
hist(modelresult_lm[[8]]$residuals)
hist(modelresult_lm[[9]]$residuals)
hist(modelresult_lm[[10]]$residuals)


ggplot(modeldata_mean,aes(x=afam_pct,y=dist_1k)) + geom_point() + facet_wrap(~chain_name)
ggplot(modeldata_mean,aes(x=povpop_pct,y=dist_1k)) + geom_point() + facet_wrap(~chain_name)
ggplot(modeldata_mean,aes(x=snap1k,y=dist_1k)) + geom_point() + facet_wrap(~chain_name)

########################
# Calculate change in variables by tract
########################

modeldata_change <- modeldata %>%
  filter(year=="Y2008" | year=="Y2013") %>%
  select(gisjn_tct,year,afam_pct,asn_pct,hisp_pct,povpop_pct,hh150k_pct,snap_pct,popden1k) %>%
  gather(afam_pct,asn_pct,hisp_pct,povpop_pct,hh150k_pct,snap_pct,popden1k,
         key="var",value="value") %>%
  unique() %>%
  spread(year,value) %>%
  mutate(var_chg=Y2013-Y2008) %>%
  select(-Y2013,-Y2008) %>%
  spread(var,var_chg)












#####################
## OLD CODE for setup
#####################

############################
# Subset stores
############################

stores<-st_read("Data/GA_SNAPstores.shp") %>% st_transform(32616)
names<-read_csv("chain_temp.csv")

tractdata<-read_csv("GAtracts_stcount_atl.csv")[,c(1:4,17:25,72)] %>%
  mutate(pop1k=totpop_pov/1000,
         snap1k=snap_enroll/1000)

tracts<-st_read("tractdata_clusters.shp")[,c(1,4)]

chain_select<-c("walmart","target","kroger","publix","ingles.mar","dollar.gen","family.dol","shell.food","chevron.fo","cvs.pharma")

stores_select<-stores %>%
  gather(2:7,key="year",value="value") %>%
  filter(value==1) %>%
  gather(walmart:big.lots,key="chain",value="value1") %>%
  filter(value1==1 & chain %in% chain_select)
stores_select$value<-NULL
stores_select$value1<-NULL

stores_subset<-function(store,year){
  store_name<-store
  year_name<-year
  store_subset<-subset(stores_select,chain==store_name&year_name==year)
  store_subset
}

stores_subset("walmart","Y2008")

stores_subset_all<-function(chainname){
  storename<-chainname
  store1<-stores_subset(storename,"Y2008")
  store2<-stores_subset(storename,"Y2009")
  store3<-stores_subset(storename,"Y2010")
  store4<-stores_subset(storename,"Y2011")
  store5<-stores_subset(storename,"Y2012")
  store6<-stores_subset(storename,"Y2013")
  st_write(store1,paste(storename,"_Y2008.shp",sep=""))
  st_write(store2,paste(storename,"_Y2009.shp",sep=""))
  st_write(store3,paste(storename,"_Y2010.shp",sep=""))
  st_write(store4,paste(storename,"_Y2011.shp",sep=""))
  st_write(store5,paste(storename,"_Y2012.shp",sep=""))
  st_write(store6,paste(storename,"_Y2013.shp",sep=""))
  
}

lapply(chain_select,stores_subset_all)


########################################
# Distances
#######################################

blocks<-readOGR(".","atl_blocks")
blocks<-spTransform(blocks,CRS("+init=epsg:32616 +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

storeFiles_names <- list.files(path='dist_raster/storepoints2/',pattern='.shp')
storeFiles_names<-substr(storeFiles_names,1,nchar(storeFiles_names)-4)

storeDist<-function(filename){
  storepoints<-readOGR("dist_raster/storepoints2",filename)
  storepoints<-spTransform(storepoints,CRS("+init=epsg:32616 +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  knn<-gDistance(blocks,storepoints,byid=TRUE)
  knn_short<-apply(knn, 2, min)
  knn_short
}

storeDist("chevron.fo_Y2008")


testfiles<-lapply(storeFiles_names,storeDist)
testfiles_df<-as.data.frame(testfiles)
names(testfiles_df)<-storeFiles_names
testfiles_df<-data.frame(data.frame(cbind(blocks[,c(6,13,11,12,14)]),testfiles_df))
write_csv(testfiles_df,"storedist_2017_07_03.csv")

##########
# Summarise to tracts
##########

storedist<-read_csv("storedist_2017_07_03.csv")

storedist_tct<-storedist %>%
  gather(chevron.fo_Y2008:walmart_Y2013,key="id",value="distance") %>%
  mutate(weight=Pop2010*distance) %>%
  group_by(tract_id,id) %>%
  summarise(weightmean=sum(weight)/sum(Pop2010)) %>%
  spread(id,weightmean) %>%
  rename(tractid=tract_id)

write_csv(storedist_tct,"storedist_tct_2017_07_03.csv")

###########
# Join tract data
###########

storedist_tct<-read_csv("storedist_tct_2017_07_03.csv") %>%  
  gather(chevron.fo_Y2008:walmart_Y2013,key="store",value="dist") %>%
  separate(store,sep="_",c("chain_name","year"))

storedist_tct_all<-left_join(storedist_tct,tractdata)
write_csv(storedist_tct_all,"storedist_tct_data_2017_07_10.csv")

storedist_tct_spread<-storedist_tct %>%
  gather(chevron.fo:walmart,key="store",value="dist") %>%
  mutate(store_yr=paste(substr(store,1,6),substr(year,4,5),sep="_")) %>%
  dplyr::select(c("tractid","store_yr","dist")) %>%
  spread(store_yr,dist) 

storedist_tct_spread_all<-left_join(tracts,storedist_tct_spread)
st_write(storedist_tct_spread_all,"storedist_tct_data_shp_2017_07_10.shp")

#Calculate pop density
x <- shapefile("C:/Users/jshannon/Dropbox/Jschool/GIS data/Census/Urban areas_2013/Tract_UA_Atlanta_individual.shp")
crs(x)
x$area_sqkm <- area(x) / 1000000
mapview(x,zcol="area_sqkm")

modeldata<-read_csv("storedist_tct_data_2017_07_10.csv") %>%
  mutate(dist_1k=dist/1000) %>%
  filter(totpop_pov>5)

modeldata<-merge(modeldata,x[,c(1,5)])
modeldata$popden1k<-modeldata$totpop_pov/modeldata$area_sqkm/1000

#Calculate SNAP rate
modeldata$snap_pct<-modeldata$snap_enroll/(modeldata$totpop_pov)*100

write_csv(modeldata,"storedist_tct_data_2017_07_14.csv")
