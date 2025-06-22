# urban-malaria-Africa-publication-2023
Trends in urban malaria and ITN use, access, and cost from 2010- 2021.
The scripts in this repo can be used to analyze the DHS/ MIS to calculate P. falciparum parasite prevalence estimates by cluster for each country in sub-Saharan Africa which collected this data between the years 2010- 2021. Scripts for our research on the factors associated with malaria prevalence in urban areas in sub-Saharan Africa are also included. We also include code for the analysis of the hypothetical scenario where if nets for urban areas were reserved for persons living in urban slums and funds from any excess nets could instead be used to purchase RDTs and ACTs. All analyses were done in R version 4.2.3. 

### System Requirements
* R software can be run on Windows, Linux, and macOS operating systems. R is and open-access software licensed under GNU General Public License version 3.0. 
* R version 4.2.3 was used in RStudio (2023-12.0).
* All packages required are listed in the R scripts.

#### Installation guide for R can be found here: https://www.r-project.org/ 
* typical install time is 5 min.
  
### Folders
**Trends in malaria prevalence, net use and access:** 
* Desciptive analysis of P. falciparum prevalence urban areas, ITN access and use
* Multivariate modeling of urban malaria (ZIP Poisson)
  
**Redistribution of nets:**
* Includes analysis of urban ITN redistribution scenarios
  
**Environmental data:**
* Includes code to extract precipitation, relative humidity, and enhanced vegetation index (EVI) by geospatial location for all of Africa
* Extract environmental data per cluster by month/year of survey
  
**Functions:**
* Functions for creating plots

