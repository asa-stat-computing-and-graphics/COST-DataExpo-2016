# BayesApproachSelectionBias

This repository contains the data and code that generate the figures in the article: A Hierarchical Bayes Approach to Adjust for Selection Bias in Before-After Analyses of Vision Zero Policies. Some general comments about the data and code are necessary. 

The data are contained in the directory `/data`. Some datasets, such as shapefiles, are too large to be uploaded to github. (See https://help.github.com/en/articles/what-is-my-disk-quota for details.) Commented code links to the location of these datasets. To download the data, simply use the `download.file` and `unzip` functions.

For example, the comments state that the New York street shapefile can be found at http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=932. To download this data, run

```{r shapefile, eval = FALSE}
url <- "http://gis.ny.gov/gisdata/fileserver/?DSID=932&file=streets_shp.zip"
download.file(url, destfile = "data/Vision_Zero/streets_shp.zip")
unzip("data/Vision_Zero/streets_shp.zip", exdir = "data/Vision_Zero")
file.remove("data/Vision_Zero/streets_shp.zip")
streets <- rgdal::readOGR("data/Vision_Zero/Streets_shp/", "StreetSegment")
```

The code is contained in one script `bayes_approach_selection_bias.R` and divided into 12 steps. 
It was written in 2017 using R 3.3 and RStan 2.14.

The code uses the ggmap function `get_map`, which as of 2018 requires an API key from Google. Be sure to install ggmap using the `devtools` package: `devtools::install_github("dkahle/ggmap")` in Step 1: Setup. (See https://www.r-bloggers.com/geocoding-with-ggmap-and-the-google-api/ for details.)

For example,

```{r key, eval = FALSE}
devtools::install_github("dkahle/ggmap")
library("ggmap")
api_key <- "your key here"
ggmap::register_google(key = api_key)
ggmap::has_google_key()
```
