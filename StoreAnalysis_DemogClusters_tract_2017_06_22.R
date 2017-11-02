library(sf)
library(mapview)
library(MASS)
library(stargazer)
library(broom)
library(spdep)
library(rgdal)
library(tidyverse)

###############
###Prepare data
###############

tractdata<-read_csv("Data/GAtracts_stcount_atl.csv")
chaincode<-read_csv("Data/chains_stcode.csv")
modeldata<-read_csv("Data/storedist_tct_data_2017_07_14.csv")

chain_select<-c("walmart","target","kroger","publix","ingles.mar","dollar.gen","family.dol","shell.food","chevron.fo","cvs.pharma")

#Calculate mean values of variables by chain and tract
modeldata_mean <- modeldata %>%
  gather(dist:snap_pct,key="var",value="value") %>%
  group_by(tractid,chain_name,var) %>%
  summarise(mean=mean(value)) %>%
  spread(var,mean)

modeldata_mean_wide<-modeldata_mean %>%
  dplyr::select(-dist) %>%
  spread(chain_name,dist_1k) 

#Add geographic tract data and join to the census variables
tracts<-st_read("Data/Tract Data/GA_tracts_puma_simplify.shp")
tracts<-subset(tracts,Atl_MSA==1)
#tracts_wgs<-st_transform(tracts, 4326)

tracts$tractid<-tracts$TRACTID
tracts$gisjn_tct<-tracts$GISJN_TCT
tracts<-tracts[,c(13,9,10,12)]

tract_countdata<-unique(tractdata[,c(1,2,4,17:23)]) %>% 
  gather(variable,value,-(gisjn_tct:year)) %>%
  unite(temp,variable,year,sep="  ") %>%
  spread(temp,value) 

tract_data<-left_join(tracts,tract_countdata)
tract_data_df<-tract_data
st_geometry(tract_data_df)<-NULL

#Set parameters for later functions, particularly plotting
cluster_color<-c("1"="#C8AF8A","2"="#658385","3"="#F6BE41","4"="#A3739E","5"="#B04838","6"="#1f78b4")
variable_names<-c(
  'afam_pct'="% African-American",
  'asn_pct'="% Asian-American",
  'hisp_pct'="% Hispanic",
  'hh150k_pct'="% hh w/income >$150k",
  'povpop_pct'="% hh in poverty",
  'snap_pct'="% enrolled in SNAP",
  'totpop_pov'="Total population",
  'popden1k'="Pop. density (1,000 per sq. mile)"
)

var_labeller <- function(variable,value){
  return(variable_names[value])
}

chain_names<-c(
'walmart'="Walmart",
'target'="Target",
'kroger'="Kroger",
'publix'="Publix",
'ingles.markets'="Ingles",
'dollar.general'="Dollar General",
'family.dollar.store'="Family Dollar",
'chevron.food.mart'="Chevron",
'shell.food.mart'="Shell",
'cvs.Pharmacy'="CVS Pharmacy")
var_labeller2 <- function(variable,value){
  return(chain_names[value])
}

chain_names1<-c(
  'walmart'="Walmart",
  'target'="Target",
  'kroger'="Kroger",
  'publix'="Publix",
  'ingles.mar'="Ingles",
  'dollar.gen'="Dollar General",
  'family.dol'="Family Dollar",
  'chevron.fo'="Chevron",
  'shell.food'="Shell",
  'cvs.pharma'="CVS Pharmacy")
var_labeller3 <- function(variable,value){
  return(chain_names1[value])
}

####################
# Create mean per capita counts for tracts
###################

tract_countdata<-tractdata[,c(1,2,4,26:71)] %>% 
  gather(variable,value,-(gisjn_tct:totpop_pov)) %>%
  mutate(percap=value/totpop_pov*1000) %>%
  group_by(gisjn_tct,variable) %>%
  summarise(mean=mean(percap)) %>% 
  spread(variable,mean)

##Select just UA
tract_countdata<-left_join(tracts_ua,tract_countdata)

###########################
#Summarize mean values by tract for demographics and cluster
###########################

tractdata_means<-left_join(tracts_ua,modeldata_mean_wide) #Add variable means across years
st_geometry(tractdata_means)<-NULL
tractdata_means<-subset(tractdata_means,afam_pct>-1) #Select just populated tracts

#Subset to just the variables used for clustering
tract.x<-tractdata_means[,c(4,6,9,8,13)]

#Create distance matrix
tract.dist<-dist(tract.x)
tract.dist[tract.dist==0]<-1

#Create dendrogram and use hca to identify clusters
hc<-hclust(tract.dist, method="ward.D")
plot(hc)
group_num<-6
rect.hclust(hc,k=group_num,border="red")

hc.groups<-data.frame(cutree(hc,k=group_num))
names(hc.groups)<-"cluster"
hc.groups$cluster<-as.factor(hc.groups$cluster)

#Add the cluster ID back to the tract variables
hc.groups$gisjn_tct<-tractdata_means$gisjn_tct
tractdata_means_cluster<-left_join(tractdata_means,hc.groups)
#ggplot(hc.groups,aes(x,y,colour=cluster)) + geom_point()
tractclusters_shp<-left_join(tracts_ua,tractdata_means_cluster) %>%
  filter(afam_pct>-1)
st_write(tractclusters_shp,"Data/tractdata_clusters_ua.shp")
write_csv(tractdata_means_cluster,"tract_clusters.csv")

###############
#Map the clusters
##############
tract_data<-st_read("Data/tractdata_clusters_ua.shp") #Read in the file created in l. 141 to save time
tract_data<-tractclusters_shp

#Summarise count and locations of clusters
table(tract_data$cluster)
mapview(tract_data,zcol="cluster")

##Add clusters to the yearly variables
tractdata_means_cluster<-read_csv("Data/tract_clusters.csv") #If not running analysis from l. 142
tractdata_cluster_mean<-left_join(modeldata,tractdata_means_cluster[,c(3,30)])
tractdata_cluster_mean$cluster<-as.factor(tractdata_cluster_mean$cluster)

#Prepare values for plotting means by year by cluster
tract_clusters_split<-tractdata_cluster_mean %>%
  dplyr::select(afam_pct,asn_pct,hisp_pct,povpop_pct,hh150k_pct,snap_pct,totpop_pov,popden1k,cluster,year) %>%
  gather(key="variable",value="value",-cluster,-year) 

tract_clusters_split<-tract_clusters_split %>%
  group_by(cluster,variable,year) %>%
  summarise(mean=mean(value)) %>%
  mutate(facet=factor(variable,levels=c("afam_pct","asn_pct","hisp_pct","hh150k_pct","povpop_pct","snap_pct","totpop_pov","popden1k")),
         year_num=substr(year,2,5))

ggplot(tract_clusters_split, aes(year_num,mean,group=cluster,colour=cluster)) +
  geom_line(size=1) +
  scale_colour_manual(values=cluster_color,name="Clusters")+
  theme_minimal()+guides(fill=FALSE)+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=11),legend.position="NONE",
        axis.title.x=element_blank(),axis.text.y=element_text(size=11),strip.text = element_text(size=16))+ 
  ylab(NULL)+ 
  facet_wrap(~facet,scales="free_y",ncol=3,labeller=var_labeller) 
ggsave("Maps figures/cluster_demog_ua.pdf")

##########
# Look at select stores by cluster (not in publication)
##########

chaindata_means<-tractdata_cluster_mean %>%
  group_by(chain_name,cluster,year) %>%
  summarise(meandist1k=mean(dist)/1000) %>%
  mutate(cluster_f=as.factor(cluster)) %>%
  dplyr::filter(meandist1k>-0.1)

chaindata_means$year_num<-substr(chaindata_means$year,2,5)
chaindata_means$chain_name<-factor(chaindata_means$chain_name,levels=chain_select)

ggplot(chaindata_means, aes(year_num,meandist1k,group=cluster,color=cluster_f)) +
           geom_line(size=1) +
           scale_colour_manual(values=cluster_color,name="Clusters")+
           theme_minimal()+
           theme(axis.text.x=element_text(angle=45,hjust=1,size=11),
                 axis.text.y=element_text(size=11),
                 axis.title.y=element_text(size=12),
                 strip.text = element_text(size=14))+ 
           ylab("Mean tract distance in km")+
           xlab(NULL)+
           facet_wrap(~chain_name,ncol=5,scale="free_y",labeller=var_labeller3)
ggsave("Maps figures/chaintrends_ua.pdf")


############################
#Calculating change at tract level
###########################
tractdata_means_cluster<-read_csv("Data/tract_clusters.csv") #If not running analysis from l. 142
modeldata_demog<-left_join(modeldata,tractdata_means_cluster[,c(3,30)])
modeldata_demog<-subset(modeldata_demog,dist_1k>0)
modeldata_demog$cluster<-as.factor(modeldata_demog$cluster)

modeldata_tctcnt<-modeldata_demog %>%
  group_by(tractid,cluster) %>%
  summarise() %>%
  group_by(cluster) %>%
  summarise(tct_cnt=n())

####Calcualte number of tracts with a change
modeldata_demog_sd<-modeldata_demog %>%
  group_by(tractid,cluster,chain_name) %>%
  summarise(stdev=sd(dist)) 

#Create dummy variable for tracts where distance changed
modeldata_demog_sd$chg<-ifelse(modeldata_demog_sd$stdev==0,0,1)

#Calculate % of tracts that had a change by cluster
modeldata_demog_sd_table<-modeldata_demog_sd %>%
  group_by(cluster,chain_name) %>%
  summarise(pct_change=sum(chg)/n()*100,
            count=n())

##Calculating coefficients/trend lines for tracts with a change
modeldata_demog<-left_join(modeldata_demog,modeldata_demog_sd) #Add chg variable to the yearly data

modeldata_demog$year_num<-as.numeric(substr(modeldata_demog$year,2,5)) #create numeric year variable

#Nest those tracts where store distance changed for modeling
cluster_counts<-modeldata_demog %>%
  filter(chg==1) %>%
  group_by(chain_name,tractid,cluster) %>%
  nest()

#Create model for trendline
year_lm<-function(df){
  lm(dist_1k~year_num,data=df)
}

#Run models and extract coefficient
cluster_counts<-cluster_counts %>%
  mutate(fit=map(data,year_lm),
         tidy=map(fit, broom::tidy)) %>%
  unnest(tidy)

#Create dummy variable for increase/decrease
cluster_counts1<-cluster_counts %>%
  filter(term=="year_num") %>% 
  mutate(sign=ifelse(estimate>0,"increase",ifelse(estimate<0,"decrease","zero")))%>%
  group_by(chain_name,cluster,sign) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  spread(sign,count)

cluster_counts1[is.na(cluster_counts1)]<-0
cluster_counts1<-left_join(cluster_counts1,modeldata_tctcnt)

#Calculate % of tracts with increase/decrease and plot
cluster_counts2<-cluster_counts1 %>%
  mutate(decrease=decrease*-1)%>%
  gather(decrease:increase,key="sign",value="value") %>%
  mutate(pct=value/tct_cnt*100) 

cluster_counts2$chain_name<-factor(cluster_counts2$chain_name,levels=chain_select)
cluster_counts2$sign<-factor(cluster_counts2$sign,levels=c("increase","decrease"))

ggplot(cluster_counts2,aes(x=cluster,y=pct, fill=sign)) + 
  geom_bar(stat="identity",position="identity") + 
  scale_fill_manual(values=c("#d7191c","#1a9641"))+
  facet_wrap(~chain_name,ncol=5,labeller=var_labeller2)+
  labs(x="Clusters",y="Pct. of tracts with increase/decrease in store distance",colour=NULL)+
  theme_minimal()+
  theme(axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.y=element_text(size=12),
        strip.text = element_text(size=16))

ggsave("Maps figures/chain_tctchange_ua.pdf")


#####################################
### Preparing tract lm data for map (done in ArcMap)
#####################################

cluster_counts$chain_name<-gsub("[.]","_",cluster_counts$chain_name)

cluster_counts_wide<-cluster_counts %>%
  filter(term=="year_num") %>%
  dplyr::select(chain_name,tractid,estimate) %>%
  spread(chain_name,estimate) %>% ##Fill in missing zeros
  mutate_all(funs(replace(., is.na(.), 0))) %>% #code from https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
  gather(-tractid,key="chain_name",value="estimate") %>%
  mutate(sign=ifelse(estimate<0,"neg",ifelse(estimate>0,"pos","none"))) %>% #Create sign
  mutate(estimate=abs(estimate),
         chain_name=substr(chain_name,1,6)) %>% #Turn all values to positive and rename chains
  gather(estimate:sign,key="var",value="value") %>%
  mutate(var=substr(var,1,3)) %>%
  unite(var_chain,var,chain_name) %>%
  spread(var_chain,value)

tract_shape<-st_read("Data/tractdata_lmcoef_getisord_2017_07_18.shp")
tract_shape<-left_join(tract_shape,cluster_counts_wide)
#mapview(tract_shape,zcol="cvs.pharma_estimate")
st_write(tract_shape,"Data/tractdata_lmcoef_estimates_2017_07_18.shp")


################
# Distance boxplots
################

tractdata_cluster_mean<-read_csv("Data/tract_clusters.csv")

tractdata_mean_wide<-modeldata %>%
  mutate(dist1k=dist/1000) %>%
  left_join(tractdata_cluster_mean[,c(1,30)])

tractdata_mean_wide$chain_name<-factor(tractdata_mean_wide$chain_name,levels=chain_select)
tractdata_mean_wide$cluster<-factor(tractdata_mean_wide$cluster,levels=c("1","2","3","4","5","6"))

tractdata_medians<-tractdata_mean_wide %>%
  group_by(chain_name,cluster) %>%
  summarise(median=median(dist1k))

ggplot(tractdata_mean_wide,aes(x=cluster,y=dist1k,fill=cluster)) +
  geom_boxplot(outlier.shape=20) +
  theme_minimal()+
  theme(axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        axis.title.y=element_text(size=12),
        strip.text = element_text(size=14))+ 
  coord_cartesian(ylim = c(0, 22)) +
  stat_summary(fun.y=median, aes(label=sprintf("%1.1f", ..y..)), geom = "text",
               position = position_dodge(width = .75))+
  ylab("Mean tract distance ")+
  scale_fill_manual(values=cluster_color,name="Clusters")+
  facet_wrap(~chain_name,ncol=5,scale="free_y",labeller=var_labeller3)

ggsave("Maps figures/meandist_tract_boxplot.pdf")

#######################################
###Kruskal Wallis of distance by tract by store
########################################

library(FSA)
library(dunn.test)

kruskal<-function(chain123){
  data<-tractdata_cluster_mean %>%
    gather(chevron.fo:walmart,key="chain_name",value="meandist1k") %>%
    filter(chain_name==chain123)
  kruskal.test(data$meandist1k,data$cluster)
  #dunnTest(meandist1k~cluster,data=data,method="bh")
  #pairwise.wilcox.test(data$meandist1k,data$cluster,p.adjust.method="fdr")
}

##All Kruskal tests are significant. 

kruskal("walmart")
kruskal("target")
kruskal("kroger")
kruskal("publix")
kruskal("ingles.mar")
kruskal("dollar.gen")
kruskal("family.dol")
kruskal("shell.food")
kruskal("chevron.fo")
kruskal("cvs.pharma")

dunnTest(meandist1k~cluster,chaindata_means_tct)

install.packages("FSA")
library(FSA)


################
# Calculating change thresholds for demog variables for table of tract change on dependent variables
################

modeldata<-read_csv("Data/storedist_tct_data_2017_07_14.csv") %>%
  select(-chain_name,-dist,-povpop185_pct,-hh100k_pct,-under18_pct,-totpop_pov,-over64_pct,-c(snap_enroll:popden1k)) %>%
  filter(year=="Y2008" | year=="Y2013")

modeldata<-unique(modeldata)
  
modeldata_wide <- modeldata %>%
  gather(povpop_pct:snap_pct,key="var",value="value") %>%
  spread(year,value) %>%
  mutate(value_chg=Y2013-Y2008) %>%
  select(-Y2013,-Y2008) %>%
  mutate(value_chg_cat=ifelse(value_chg<=-25,"decline25",
                              ifelse(value_chg<=-10,"decline10",
                                     ifelse(value_chg>=10&value_chg<25,"increase10",
                                            ifelse(value_chg>=25,"increase25","midchange")))))

pct_table<-data.frame(table(modeldata_wide$var,modeldata_wide$value_chg_cat)) %>%
  spread(Var2,Freq)
write_csv(pct_table,"modelvar_tractcount_2017_10_10.csv")
