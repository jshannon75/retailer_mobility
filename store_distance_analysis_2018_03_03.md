Fixed effects models
================

``` r
library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(plm)
library(stargazer)
library(spdep)
library(car)
library(Hmisc)
```

In our analysis, we used fixed effects models to identify variables associated with changing proximity to authorized SNAP retailers.

Reading in the data
-------------------

First, we read in the data from our models and the list of all chains included in our analysis.

``` r
storedist_modeldata<-read_csv("Data/storedist_modeldata_2018_03_10.csv") 
```

| store | year  |    tract\_id|         D1|        D2|        D3|        D4|        D5| st\_name | STTYPE      | sttype2        | gisjn\_tct   |  totpop\_pov|  povpop\_pct|  povpop185\_pct|  hh100k\_pct|  hh150k\_pct|  afam\_pct|   asn\_pct|  hisp\_pct|  under18\_pct|  over64\_pct|  snap\_enroll|  pop1k|     snap1k|  area\_sqkm|   popden1k|  snap\_pct|
|:------|:------|------------:|----------:|---------:|---------:|---------:|---------:|:---------|:------------|:---------------|:-------------|------------:|------------:|---------------:|------------:|------------:|----------:|----------:|----------:|-------------:|------------:|-------------:|------:|----------:|-----------:|----------:|----------:|
| aldi  | Y2008 |  13015960801|   8.868863|  19.00656|  22.56937|  22.92868|  30.69741| Aldi     | Supermarket | Large retailer | G13015960801 |         3130|    11.341853|        20.89457|    17.491166|     3.091873|  12.610340|  0.3783102|   2.522068|      19.79823|     8.890290|      216.4419|  3.130|  0.2164419|    23.81528|  0.1314282|   6.915078|
| aldi  | Y2008 |  13015960802|   9.625737|  20.97837|  23.47469|  24.41901|  31.97999| Aldi     | Supermarket | Large retailer | G13015960802 |         2891|    21.722587|        53.96057|     5.000000|     1.886793|   1.833276|  0.0000000|  12.798340|      18.40194|    16.395711|      217.2537|  2.891|  0.2172537|    18.11431|  0.1595976|   7.514828|
| aldi  | Y2008 |  13015960803|  13.217224|  25.90866|  27.32393|  29.13067|  34.90976| Aldi     | Supermarket | Large retailer | G13015960803 |         5159|    15.313045|        30.54856|    14.712269|     6.188925|   2.615385|  0.0000000|  10.230769|      18.69231|     9.807692|      544.7538|  5.159|  0.5447538|    82.45413|  0.0625681|  10.559291|
| aldi  | Y2008 |  13035150100|  35.064164|  40.36195|  41.77607|  47.24550|  48.58465| Aldi     | Supermarket | Large retailer | G13035150100 |         7557|     8.918883|        24.22919|    11.676973|     1.091306|  19.838689|  0.0000000|   1.535059|      18.09549|    14.296865|     1167.1509|  7.557|  1.1671509|   232.45675|  0.0325093|  15.444632|
| aldi  | Y2008 |  13045910101|  34.133346|  37.05227|  41.70067|  44.71284|  47.56594| Aldi     | Supermarket | Large retailer | G13045910101 |         5972|    22.689216|        44.00536|     4.649123|     1.052632|  31.339554|  3.9986671|   5.231590|      18.59380|    11.696101|      933.7671|  5.972|  0.9337671|    16.56105|  0.3606051|  15.635751|
| aldi  | Y2008 |  13045910103|  38.141789|  41.52183|  46.36072|  48.75723|  50.16716| Aldi     | Supermarket | Large retailer | G13045910103 |         4618|    10.025985|        19.55392|    18.531468|     6.526807|  11.802575|  0.0000000|   7.231760|      14.29185|     9.206009|      557.8731|  4.618|  0.5578731|    56.30526|  0.0820172|  12.080406|

``` r
chain_select<-read_csv("Data/atl_stlist_30more_2018_03_03.csv") %>%
  filter(STTYPE!="Category") %>%
  dplyr::select(st_name)
chain_select<-chain_select$st_name
```

| x              |
|:---------------|
| CVS            |
| Dollar General |
| Dollar Tree    |
| Family Dollar  |
| RiteAid        |
| Walgreens      |
| Chevron        |
| Citgo          |
| Exxon          |
| Quick Stop     |
| QuickTrip      |
| Shell          |
| Texaco         |
| Kroger         |
| Target         |
| Walmart        |
| Aldi           |
| Food Depot     |
| Ingles         |
| Publix         |

``` r
chain_type<-storedist_modeldata %>%
  select(store,st_name,STTYPE,sttype2) %>%
  distinct()
```

| store             | st\_name          | STTYPE                    | sttype2           |
|:------------------|:------------------|:--------------------------|:------------------|
| aldi              | Aldi              | Supermarket               | Large retailer    |
| chevron.food.mart | Chevron           | Convenience Store         | Convenience store |
| citgo.food.mart   | Citgo             | Convenience Store         | Convenience store |
| Combination       | Combination       | Category                  | Category          |
| Convenience store | Convenience store | Category                  | Category          |
| cvs.pharmacy      | CVS               | Combination Grocery/Other | Combination       |

Running the models
------------------

We run separate models for each store chain included in our analysis. To do so, we define a function that can be used to apply our model to data subsetted for each chain. We use the plm package for the fixed effects models, lagging all dependent variables by one year. Our dependent variable is also logged.

We use lapply to apply the model function to each chain on our chain list and then use broom's glance function to extract global diagnostics. The bind\_rows function from the tidyverse collapses the output into a single data frame.

``` r
model_fe_D3<-function(chain123) {
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
```

``` r
models.d3_broom_df %>% head() %>% kable()
```

|  r.squared|  adj.r.squared|   statistic|  p.value|   deviance|  df.residual| st\_name       | model |
|----------:|--------------:|-----------:|--------:|----------:|------------:|:---------------|:------|
|  0.2035512|      0.0013232|  123.661037|        0|  118.52006|         3387| CVS            | D3    |
|  0.2166885|      0.0177963|  133.850037|        0|   43.90327|         3387| Dollar General | D3    |
|  0.1975847|     -0.0061582|  119.143732|        0|   63.04957|         3387| Dollar Tree    | D3    |
|  0.2193653|      0.0211527|  135.968147|        0|   32.99223|         3387| Family Dollar  | D3    |
|  0.0170589|     -0.2325217|    8.397298|        0|   29.38973|         3387| RiteAid        | D3    |
|  0.1050962|     -0.1221306|   56.823470|        0|   48.92391|         3387| Walgreens      | D3    |

We can use the tidy function from broom to extract model coefficients for each chain.

``` r
models.d3_tidy<-lapply(models.d3,broom::tidy)
chain_select_d3<-paste(chain_select) #Add an ID variable
names(models.d3_tidy)<-chain_select_d3
models.d3_tidy_df<-bind_rows(models.d3_tidy,.id="store") #Collapse to a single data frame
```

``` r
models.d3_tidy_df %>% head() %>% kable()
```

| store | term                |    estimate|  std.error|    statistic|    p.value|
|:------|:--------------------|-----------:|----------:|------------:|----------:|
| CVS   | lag(afam\_pct, 1)   |  -0.0081146|  0.0010955|   -7.4070617|  0.0000000|
| CVS   | lag(asn\_pct, 1)    |  -0.0090906|  0.0019344|   -4.6994332|  0.0000027|
| CVS   | lag(hisp\_pct, 1)   |  -0.0068565|  0.0013697|   -5.0059817|  0.0000006|
| CVS   | lag(povpop\_pct, 1) |  -0.0008046|  0.0008902|   -0.9038605|  0.3661337|
| CVS   | lag(hh150k\_pct, 1) |  -0.0070067|  0.0014850|   -4.7182789|  0.0000025|
| CVS   | lag(snap\_pct, 1)   |  -0.0171720|  0.0007112|  -24.1452001|  0.0000000|

Because our models have so many variables, we visualize model coefficients with the standard error. We set up labels for the independent variables and then prepare the data: filtering for only significant variables and calculating a rough estimate of standard error. We then use ggplot to visualize the variables.

``` r
var_labels<-unique(models.d3_tidy_df$term)[1:6]
var_labels2<-c("% African-American","% Asian-American","% Hispanic","% HH in poverty",
               "% HH w/$150k income","% w/SNAP")
labels<-data.frame(var_labels,var_labels2) %>%
  rename("term"=var_labels)

models_tidy_graph_D3<-models.d3_tidy_df %>%
  filter(p.value<=0.05)%>%
  mutate(st_name=factor(store,levels=chain_select),
         ci_low=estimate-2*std.error, #Can use more complicated t score in the futrue if need be
         ci_high=estimate+2*std.error) %>%
  dplyr::select(-statistic,-p.value,-std.error,-store) %>%
  gather(estimate,ci_low,ci_high,key="pointtype",value="value") %>% 
  filter(term!="lag(popden1k, 1)") %>%
  left_join(labels) %>%
  left_join(chain_type)

cpal<-c("#d7191c", "#d8b365", "#2b83ba") #Colors for store categories

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
```

![](store_distance_analysis_2018_03_03_files/figure-markdown_github/unnamed-chunk-6-1.png)
