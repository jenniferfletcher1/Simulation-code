README: Simulation-code

## Table of Contents
1. General information
2. Data & File Overview
3. Technologies
4. Running the R script:

### 1. General Info
***

This a simulation study to look at the impact of different different methods of handling missing data on a prediction model. A complete "true" dataset is simulated, missingness is introduced under MCAR and MAR mechanisms to create an incomplete  dataset and subsequently different methods of handling missing data are applied to the incomplete dataset to generate multiple complete datasets alongside the "true" dataset. A prediction model is fitted to all datasets, and each are compared to the "true" dataset.

### 2. Data & File Overview
***
Directory of Files:
	A. Filename: 
	   Short description: 
	B. Filename: 
	   Short description: 

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
