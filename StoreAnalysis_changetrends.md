Identifying changes in proximty across the study period
================

``` r
library(tidyverse)
library(broom)
```

As part of our analysis, we identify tracts where proximity to SNAP authorized retailers changed during our study period. To do so, we smooth the yearly data using OLS regression to identify trends over time.

Reading data
------------

First, we read in previously identified tract clusters based on demographics and our model data. We join the tract IDs to our model data. We then select just the D3 variable (distance to 3rd closest store) and change the cluster number format from character to vector.

``` r
tractdata_means_cluster<-read_csv("Data/tract_clusters.csv") 
storedist_modeldata<-read_csv("Data/storedist_modeldata_2018_03_10.csv") %>%
  mutate(gisjn_tct=paste("G",tract_id,sep=""))
modeldata_demog<-left_join(storedist_modeldata,tractdata_means_cluster[,c(1,30)]) %>%
    dplyr::select(-D1,-D2,-D4,-D5)
modeldata_demog$cluster<-as.factor(modeldata_demog$cluster)
```

| store | year  |    tract\_id|        D3| st\_name | STTYPE      | sttype2        | gisjn\_tct   |  totpop\_pov|  povpop\_pct|  povpop185\_pct|  hh100k\_pct|  hh150k\_pct|  afam\_pct|   asn\_pct|  hisp\_pct|  under18\_pct|  over64\_pct|  snap\_enroll|  pop1k|     snap1k|  area\_sqkm|   popden1k|  snap\_pct| cluster |
|:------|:------|------------:|---------:|:---------|:------------|:---------------|:-------------|------------:|------------:|---------------:|------------:|------------:|----------:|----------:|----------:|-------------:|------------:|-------------:|------:|----------:|-----------:|----------:|----------:|:--------|
| aldi  | Y2008 |  13015960801|  22.56937| Aldi     | Supermarket | Large retailer | G13015960801 |         3130|    11.341853|        20.89457|    17.491166|     3.091873|  12.610340|  0.3783102|   2.522068|      19.79823|     8.890290|      216.4419|  3.130|  0.2164419|    23.81528|  0.1314282|   6.915078| 1       |
| aldi  | Y2008 |  13015960802|  23.47469| Aldi     | Supermarket | Large retailer | G13015960802 |         2891|    21.722587|        53.96057|     5.000000|     1.886793|   1.833276|  0.0000000|  12.798340|      18.40194|    16.395711|      217.2537|  2.891|  0.2172537|    18.11431|  0.1595976|   7.514828| 5       |
| aldi  | Y2008 |  13015960803|  27.32393| Aldi     | Supermarket | Large retailer | G13015960803 |         5159|    15.313045|        30.54856|    14.712269|     6.188925|   2.615385|  0.0000000|  10.230769|      18.69231|     9.807692|      544.7538|  5.159|  0.5447538|    82.45413|  0.0625681|  10.559291| 1       |
| aldi  | Y2008 |  13035150100|  41.77607| Aldi     | Supermarket | Large retailer | G13035150100 |         7557|     8.918883|        24.22919|    11.676973|     1.091306|  19.838689|  0.0000000|   1.535059|      18.09549|    14.296865|     1167.1509|  7.557|  1.1671509|   232.45675|  0.0325093|  15.444632| 1       |
| aldi  | Y2008 |  13045910101|  41.70067| Aldi     | Supermarket | Large retailer | G13045910101 |         5972|    22.689216|        44.00536|     4.649123|     1.052632|  31.339554|  3.9986671|   5.231590|      18.59380|    11.696101|      933.7671|  5.972|  0.9337671|    16.56105|  0.3606051|  15.635751| 2       |
| aldi  | Y2008 |  13045910103|  46.36072| Aldi     | Supermarket | Large retailer | G13045910103 |         4618|    10.025985|        19.55392|    18.531468|     6.526807|  11.802575|  0.0000000|   7.231760|      14.29185|     9.206009|      557.8731|  4.618|  0.5578731|    56.30526|  0.0820172|  12.080406| 1       |

Calculating rate of change
--------------------------

Next, we count the total number of tracts in each cluster, which will be our denominator in calculating the rate of change.

``` r
modeldata_tctcnt<-modeldata_demog %>%
  group_by(gisjn_tct,cluster) %>%
  summarise() %>%
  group_by(cluster) %>%
  summarise(tct_cnt=n())
```

Next, we use standard deviation to flag tracts with changing proximity over time, stratified by store name. We create a dummy variable that indicates a change.

``` r
modeldata_demog_sd<-modeldata_demog %>%
  group_by(gisjn_tct,cluster,st_name) %>%
  summarise(stdev=sd(D3)) 

modeldata_demog_sd$chg<-if_else(modeldata_demog_sd$stdev==0,0,1)
```

We then join the change dummy variable back to our main dataset. We use the nest function from the tidyverse to collect all tracts within each cluster for each store. We also convert the year variable from character to numeric in order to use it in our models.

``` r
modeldata_demog<-left_join(modeldata_demog,modeldata_demog_sd)
modeldata_demog$year_num<-as.numeric(substr(modeldata_demog$year,2,5)) #create numeric year variable

cluster_counts<-modeldata_demog %>%
  filter(chg==1) %>%
  group_by(st_name,gisjn_tct,cluster) %>%
  nest()
```

We then create a function with a simple linear model using years and distance to retailer. We convert the output to columns in our data frame using fit and tidy from the broom package.

``` r
year_lm<-function(df){
  lm(D3~year_num,data=df)
}

#Run models and extract coefficient
cluster_counts<-cluster_counts %>%
  mutate(fit=map(data,year_lm),
         tidy=map(fit, broom::tidy)) %>%
  unnest(tidy)
```

| st\_name | gisjn\_tct   | cluster | term        |      estimate|    std.error|  statistic|    p.value|
|:---------|:-------------|:--------|:------------|-------------:|------------:|----------:|----------:|
| Aldi     | G13015960801 | 1       | (Intercept) |  1658.6458473|  554.2167105|   2.992775|  0.0402277|
| Aldi     | G13015960801 | 1       | year\_num   |    -0.8143567|    0.2756610|  -2.954196|  0.0417947|
| Aldi     | G13015960802 | 5       | (Intercept) |  1169.8073357|  388.3172456|   3.012504|  0.0394530|
| Aldi     | G13015960802 | 5       | year\_num   |    -0.5705868|    0.1931445|  -2.954196|  0.0417947|
| Aldi     | G13015960803 | 1       | (Intercept) |  1277.5970060|  423.5267996|   3.016567|  0.0392956|
| Aldi     | G13015960803 | 1       | year\_num   |    -0.6223231|    0.2106574|  -2.954196|  0.0417947|

Based on the output of these models, we then create a categorical variable for change across the study period using the coefficient (the "estimate" variable) from our models. We then count the number of tracts with increases and decreases in store proximity, create columns for each, and join in the total tract count for each cluster.

``` r
#Create dummy variable for increase/decrease
cluster_counts1<-cluster_counts %>%
  filter(term=="year_num") %>% 
  mutate(sign=if_else(estimate>0,"Increase",if_else(estimate<0,"Decrease","zero")))%>%
  group_by(st_name,cluster,sign) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  spread(sign,count)

cluster_counts1[is.na(cluster_counts1)]<-0
cluster_counts1<-left_join(cluster_counts1,modeldata_tctcnt)
```

| st\_name | cluster |  Decrease|  Increase|  tct\_cnt|
|:---------|:--------|---------:|---------:|---------:|
| Aldi     | 1       |       165|         0|       218|
| Aldi     | 2       |       120|         0|       173|
| Aldi     | 3       |        87|         0|       100|
| Aldi     | 4       |        40|         0|       112|
| Aldi     | 5       |        61|         0|       138|
| Aldi     | 6       |        14|         0|       114|

Lastly, we calculate the percentage of tracts in each cluster that have increased or decreased distances to stores across the study period. We also filter the store list to remove broad categories (e.g., convenience store).

``` r
#Calculate % of tracts with increase/decrease and plot
storelist<-read_csv("Data/atl_stlist_30more_2018_03_03.csv") %>%
  arrange(desc(sttype2)) %>%
  mutate(st_name=factor(st_name,levels=st_name))
```

    ## Parsed with column specification:
    ## cols(
    ##   store = col_character(),
    ##   st_name = col_character(),
    ##   STTYPE = col_character(),
    ##   sttype2 = col_character()
    ## )

``` r
cluster_counts2<-cluster_counts1 %>%
  mutate(Decrease=Decrease*-1)%>%
  gather(Decrease:Increase,key="Change",value="value") %>%
  mutate(pct=round(value/tct_cnt*100,2)) %>%
  left_join(storelist) %>%
  filter(sttype2!="Category") %>%
  dplyr::select(-value) %>%
  spread(Change,pct)
```

    ## Joining, by = "st_name"

    ## Warning: Column `st_name` joining character vector and factor, coercing
    ## into character vector

| st\_name | cluster |  tct\_cnt| store | STTYPE      | sttype2        |  Decrease|  Increase|
|:---------|:--------|---------:|:------|:------------|:---------------|---------:|---------:|
| Aldi     | 1       |       218| aldi  | Supermarket | Large retailer |    -75.69|         0|
| Aldi     | 2       |       173| aldi  | Supermarket | Large retailer |    -69.36|         0|
| Aldi     | 3       |       100| aldi  | Supermarket | Large retailer |    -87.00|         0|
| Aldi     | 4       |       112| aldi  | Supermarket | Large retailer |    -35.71|         0|
| Aldi     | 5       |       138| aldi  | Supermarket | Large retailer |    -44.20|         0|
| Aldi     | 6       |       114| aldi  | Supermarket | Large retailer |    -12.28|         0|
