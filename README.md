# Examining the Relationship of Race and Social Determinants of Health on Incidence, Severity and Antimicrobial Resistance of Shigella
*using Foodborne Diseases Active Surveillance Network Data*

This repository contains code for the preparation, quality assurance, and analysis of FoodNet surveillance data on shigella cases diagnosed between 2004 and 2019. FoodNet data was merged to PulseNet and the National Antimicrobial Resistance Monitoring System for Enteric Bacteria (NARMS) data. 

[View interactive descriptive statistics in the here.]('https://s5rockhill.github.io/shigella-svi-analysis/')

## Contents

### aggregate population data
This folder contains code to aggregate population data from the U.S. Census Bureau and the National Center for Health Statistics. Data are aggregated by census tract level (Census) and by county (census and NCHS). The population data is used for analysis of shigella incidence. 

### archive
This folder contains older versions of code that are retained for archive purposes only. 

### analysis
This folder contains code for analysis of county-level incidence and person-level risk of severe shigella and antimicrobial resistant shigella. 

### data cleaning
This folder contains code to clean and prepare the FoodNet-PulseNet-NARMS linked dataset for analysis.

- census tract crosswalk and imputation: imputes census tract for cases that are missing data on the census tract of residence closest to their year of diagnosis. 

- cases for follow up: outputs list of cases with non-Shigella genus or species

- reconcile-missing-census-tracts: descriptive statistics on census tract missingness and imputation

- merged_dataset_cleaning: final preparation of FoodNet-PulseNet-NARMS linked dataset. Calculates variables for analysis and calls census tract crosswalk and imputation. 

- ct-incidence-regression-data: aggregates shigella case data and combines with census population data for analysis of incidence at the census tract level.

- county-incidence-regression-data: aggregates shigella case data and combines with NCHS population data for analysis of incidence at the county level.

- narms_data_prep.R: prepares NARMS data for merge with FoodNet data including classifying antibiotic resistance categories

### helper functions
Includes functions to support analysis. 

### model selection
This folder contains code used to develop regression models and 

### table shells
This folder contains code used to populate original table shells. 

### visualizations
Contains code for charts and maps

