# Analysis of SNAP chain retailers in Atlanta during the Great Recession

This repo contains data and analysis scripts for an analysis of changing access to chain SNAP retailers in the metro Atlanta area during the Great Recession. Our article on this research appears in Social Science and Medicine [link](https://www.sciencedirect.com/science/article/pii/S0277953618302892). The final accepted version of this manuscript is available [on Jerry Shannon's website](http://jerry.shannons.us/publications.html). This analysis was done using R and associated packages. Scripts and data from the R analysis are included here. 

There are two main data files that may be useful for general use:
* A list of all SNAP retailers in Georgia from 2007 through 2016 [link to csv](https://github.com/jshannon75/retailer_mobility/raw/master/Data/GA_SNAPstores_2008_2016_GeoID_dummy.csv). We used just stores in the Atlanta metro from 2007-2013.
* The data used in our statistical models [link to xlsx](https://github.com/jshannon75/retailer_mobility/raw/master/Data/storedist_modeldata_2018_03_10.xlsx). This Excel spreadsheet has a tab with the data and a tab with variable descriptions.

We have several files that walk through steps of the analysis.
* [How we chose the store chains we focused on.](https://github.com/jshannon75/retailer_mobility/blob/master/storechain_id_2018_05_27.md)
* [How we determined the demographic clusters for our descriptive analysis](https://github.com/jshannon75/retailer_mobility/blob/master/StoreAnalysis_DemogClusters_tract_2018_03_10.md)
* [How we calculated weighted mean distance to closest retailers.](https://github.com/jshannon75/retailer_mobility/blob/master/StoreDistanceCalc_2018_03_03.md)
* [How we identified tracts with positive or negative trends in retailer proximity by cluster](https://github.com/jshannon75/retailer_mobility/blob/master/StoreAnalysis_changetrends.md)
* [Our process for running fixed effects models](https://github.com/jshannon75/retailer_mobility/blob/master/store_distance_analysis_2018_03_03.md)

For any questions on this project, contact Jerry Shannon at jshannon (at) uga.edu.