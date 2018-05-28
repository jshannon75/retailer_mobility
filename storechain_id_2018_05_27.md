Identifying store chains
================

``` r
library(tidyverse)
library(sf)
```

Loading data
------------

We start by loading tracts, particularly a dummy variable that shows whether a tract is in the Atlanta urbanized area.

``` r
tracts_ua<-st_read("Data/Tract_UA_Atlanta_individual.shp", quiet=TRUE)[,c(1,2,4)] %>%
  mutate(GISJN_TCT=paste("G",tractid,sep=""))
st_geometry(tracts_ua)<-NULL
```

Then we also load our store data and join the tract data

``` r
storedata<-read_csv("Data/GA_SNAPstores_2008_2016_GeoID_dummy.csv") %>%
  right_join(tracts_ua) 
```

Working with the data
---------------------

We convert the store data to long format and just select stores present in each year.

``` r
storedata_long<-storedata %>%
  gather(Y2008:Y2016,key="year",value="st_value") %>%
  gather(walmart:big.lots,key="store",value="value") %>%
  filter(value==1,st_value==1) %>%
  select(-value,-st_value)
```

| STOREID | NAME                     | STREET                     | CITY           | ST  | STTYPE      | STCODE |   ZIP5|  ZIP6|       LONG|       LAT| LOCTYPE |  TRACTID|   PUMAID|  CTYID| GISJN\_CTY | GISJN\_TCT   | GISJN\_PUMA | gisjn\_tct   |  Atl\_Core|      tractid| year  | store   |
|:--------|:-------------------------|:---------------------------|:---------------|:----|:------------|:-------|------:|-----:|----------:|---------:|:--------|--------:|--------:|------:|:-----------|:-------------|:------------|:-------------|----------:|------------:|:------|:--------|
| R16252  | walmart s/c 2732         | 600 highway 61             | villa rica     | GA  | Super Store | A      |  30180|  4969|  -84.93530|  33.71940| NA      |        0|  1302300|  13045| G1300450   | G13045910101 | G1302300    | G13045910101 |          0|  13045910101| Y2008 | walmart |
| R16261  | walmart s/c 3461         | 2717 highway 54            | peachtree city | GA  | Super Store | A      |  30269|  1031|  -84.59902|  33.39695| NA      |        0|  1302400|  13113| G1301130   | G13113140208 | G1302400    | G13113140208 |          0|  13113140208| Y2008 | walmart |
| R16277  | walmart s/c 594          | 125 pavilion pkwy          | fayetteville   | GA  | Super Store | A      |  30214|  4098|  -84.44222|  33.47962| NA      |        0|  1302400|  13113| G1301130   | G13113140102 | G1302400    | G13113140102 |          0|  13113140102| Y2008 | walmart |
| R16247  | walmart s/c 2475         | 1436 dogwood drive         | conyers        | GA  | Super Store | A      |  30012|     0|  -83.99806|  33.65035| NA      |        0|  1304300|  13247| G1302470   | G13247060305 | G1304300    | G13247060305 |          0|  13247060305| Y2008 | walmart |
| R16322  | walmart supercenter 1766 | 3100 johnson ferry rd      | marietta       | GA  | Super Store | A      |  30062|  5657|  -84.42580|  34.02305| NA      |        0|  1303005|  13067| G1300670   | G13067030328 | G1303005    | G13067030328 |          1|  13067030328| Y2008 | walmart |
| R16312  | walmart supercenter 937  | 2795 chastain meadows pkwy | marietta       | GA  | Super Store | A      |  30066|  3361|  -84.55283|  34.02105| NA      |        0|  1303002|  13067| G1300670   | G13067030228 | G1303002    | G13067030228 |          1|  13067030228| Y2008 | walmart |

Some SNAP retailers are classified in different ways across years. We identify chain type by the most common classification as shown below.

``` r
storedata_type<-storedata_long %>%
  group_by(store,STTYPE) %>%
  summarise(count=n()) %>%
  ungroup()%>%
  group_by(store) %>%
  filter(count==max(count)) %>%
  select(-count)
```

| store             | STTYPE              |
|:------------------|:--------------------|
| aldi              | Supermarket         |
| bell.s            | Large Grocery Store |
| big.bear          | Super Store         |
| big.lots          | Convenience Store   |
| bp.food.mart      | Convenience Store   |
| chevron.food.mart | Convenience Store   |

Lastly, we create a count of stores in each year and look at the minimum values. We select just those store with a minimum of 20 or more.

``` r
atl_stcount<-storedata_long %>%
  group_by(store,year) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  group_by(store) %>%
  summarise(min=min(count)) %>%
  filter(min>19) %>%
  left_join(storedata_type) %>%
  arrange(STTYPE,desc(min))
```

    ## Joining, by = "store"

| store                |  min| STTYPE                    |
|:---------------------|----:|:--------------------------|
| dollar.general       |  103| Combination Grocery/Other |
| cvs.pharmacy         |   89| Combination Grocery/Other |
| rite.aid..rite.aid.. |   85| Combination Grocery/Other |
| family.dollar.store  |   83| Combination Grocery/Other |
| walgreen.s.          |   58| Combination Grocery/Other |
| race.trac            |   35| Combination Grocery/Other |
| dollar.tree          |   24| Combination Grocery/Other |
| chevron.food.mart    |   44| Convenience Store         |
| quiktrip             |   38| Convenience Store         |
| citgo.food.mart      |   31| Convenience Store         |
| shell.food.mart      |   25| Convenience Store         |
| texaco.food.mart     |   22| Convenience Store         |
| kroger               |  123| Super Store               |
| walmart              |   56| Super Store               |
| target               |   36| Super Store               |
| publix               |  137| Supermarket               |
| ingles.markets       |   28| Supermarket               |
| food.depot           |   22| Supermarket               |
| aldi                 |   21| Supermarket               |
