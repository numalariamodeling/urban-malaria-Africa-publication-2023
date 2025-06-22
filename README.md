# urban-malaria-Africa-publication-2023
Trends in urban malaria and ITN use, access, and cost from 2010- 2021.
The scripts in this repo can be used to analyze the DHS/ MIS to calculate P. falciparum parasite prevalence estimates by cluster for each country in sub-Saharan Africa which collected this data between the years 2010- 2021. Scripts for our research on the factors associated with malaria prevalence in urban areas in sub-Saharan Africa are also included. We also include code for the analysis of the hypothetical scenario where if nets for urban areas were reserved for persons living in urban slums and funds from any excess nets could instead be used to purchase RDTs and ACTs. All analyses were done in R version 4.2.3. 
  
### Repo Contents
**Trends in malaria prevalence, net use and access:** 
* Includes R code for Desciptive analysis of P. falciparum prevalence urban areas, ITN access and use
* Multivariate modeling of urban malaria (ZIP Poisson)
  
**Redistribution of nets:**
* Includes R code of analysis of urban ITN redistribution scenarios
  
**Environmental data:**
* Includes R code to extract precipitation, relative humidity, and enhanced vegetation index (EVI) by geospatial location for all of Africa
* Extract environmental data per cluster by month/year of survey
  
**Functions:**
* Custom R functions for creating the plots

### System Requirements
* R software can be run on Windows, Linux, and macOS operating systems. R is and open-access software licensed under GNU General Public License version 3.0. 
* R version 4.2.3 was used in RStudio (2023-12.0).
* All packages required are listed in the R scripts.

### Installation Guide 
* Installation guide for R can be found here: https://www.r-project.org/ 
* typical install time is about 2 minutes.
#### Packages required
Users should install the following R packages prior to running all the code.
```
install.packages(c('readxl', 'writexl', 'plyr', 'dplyr', 'tidyverse', 'ggplot2', 'janitor', 'haven', 'survey', 'skimr', 'labelled', 'glmmTMB', 'ggeffects', 'ggrepel', 'splines', 'scales', 'expss', 'DHARMa', 'viridis', 'zoo', 'ggpubr', 'sf', 'listr'))
# list of packages for plots
list.of.packages <- c("ggplot2", "reshape2", "ggpubr", "wesanderson", "haven", "tidyr", "dplyr",
                      "questionr", "data.table", "plyr", "purrr", "forcats", "survey", "stringr", 
                      "patchwork", "viridis", "zoo", "rdhs", "scales", "Kendall", "lemon", "sp",
                      "rgeos", "sf", "tmap", "blscrapeR")
install.packages(list.of.packages)
```

### Demo 
#### Instructions on how to run the sample data.
 Sample data is available in the following folder:
 1. 
