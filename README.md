README: Simulation-code

## Table of Contents
1. General information
2. Data & File Overview
3. Technologies
4. Running the R script:

### 1. General Info
***

This a simulation study to look at the impact of different methods for handling missing data on a prediction model. 

Methodology:
- A complete "true" dataset is simulated
- Missingness is introduced under to create an incomplete dataset (one dataset under MCAR mechanism and two datasets under MAR mechanism)  
- The different methods of handling missing data are applied to the incomplete dataset to create multiple complete datasets alongside the complete "true" dataset.
- A prediction model is fitted to all complete datasets and prediction measures are computed to assess model performance
- Model performance is compared between all complete datasets and the complete "true" dataset to assess the impact of the different methods of handling missing data

### 2. Data & File Overview
***
Directory of Files:

All datasets used within the project are generated using the code provided. 

### 3. Technologies
***
A list of technologies used within the project:
* R: Version 4.1.0 (2021-05-18)
* RStudio: Version 1.4.1717

### 4. Running the R script:
***

Run all code in Rstudio

1. Run file '0 libraries.R': contains libraries required for all code
2. Run all files starting with "1" (e.g. 1.1, 1.2, etc): contains all functions required for simulation
3. Run all files starting with "2" (e.g. 2.1, 2.2, etc): contains all simulations for different scenario & result is complete datasets for each method of handling the missing data as well as the simulated full data
4. Run all files starting with "3" (e.g. 3.1, 3.2, etc): contains code for plots of all results
