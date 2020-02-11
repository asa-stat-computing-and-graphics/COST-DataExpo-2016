# TaxonomyOfBusCrashes-
This repo holds code and data to reproduce results and graphics in A Cluster-Based Taxonomy of Bus Crashes in the US.

To reproduce the graphs and the data for the tables, follow these steps:
1. Clone this repo:
```bash
git clone https://github.com/COSTDataChallenge2016/TaxonomyOfBusCrashes-.git
```
2. Download the GES data: [ftp://ftp.nhtsa.dot.gov/GES/](ftp://ftp.nhtsa.dot.gov/GES/) (You only need the data from 2005-2015)
3. Install the following R packages as you usually do: data.table, dplyr, bit64, sas7bdat, devtools, magrittr, FactoMineR, dbscan, cluster, cclust, WeightedCluster, fmsb
4. [Download v2.0.19 of R package *kohonen* from CRAN](https://cran.r-project.org/src/contrib/Archive/kohonen/) and 
install from source:
```r
install.packages("~/Downloads/kohonen_2.0.19.tar.gz", repos = NULL, type = "source")
```
5. In *wrapper.R*, change `base_dir` to wherever you've cloned the repo, and change `data_base_dir` to wherever you've downloaded the GES data to. Run this file. The program will put all generated output in `base_dir/out`.

In the repo, you can find an "author_intermediate" directory that contains our intermediate output, and an "author_out" directory that contains the figures and tables from the paper, so that you can compare your outputs with ours.
