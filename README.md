README: Simulation-code

## Table of Contents
1. General information
2. Data & File Overview
3. Technologies
4. Running the R code

### 1. General Info
***

This a simulation study to look at the impact of different methods for handling missing data on a prediction model. 

Methodology:
- A complete "true" dataset is simulated
- Missingness is introduced under to create an incomplete dataset (one dataset under MCAR mechanism and two datasets under MAR mechanism)  
- The different methods of handling missing data are applied to the incomplete dataset to create multiple complete datasets alongside the complete "true" dataset.
- A prediction model is fitted to all complete datasets and prediction measures are computed to assess model performance
- Model performance is compared between all complete datasets and the complete "true" dataset to assess the impact of the different methods of handling missing data with the baseline ("true" dataset)

### 2. Data & File Overview
***
Directory of Files:

note: scripts 1 denote scripts begining with 1, e.g. script 1.1, 1.2 etc. 

* Scripts 0 (starting with 0): 
Contain libraries required for running of script.

* Scripts 1 (starting with 1): 
Contain all functions required for running simulation (simulations are found in scripts 2).

* Scripts 2 (starting with 2): 
Each script contains code to run the simulation for a specific scenario. At the end of the scripts the datasets generated from the simulation are exported to the working directory. 

* Scripts 3 (starting with 3): 
Contains all functions required for presenting and manipulating results results.

* Scripts 4 (starting with 4): 
Scripts to generate results for each scenario. 

### 3. Technologies
***
A list of technologies used within the project:
* R: Version 4.1.0 (2021-05-18)
* RStudio: Version 1.4.1717

### 4. Running the R script
***

Run all code in Rstudio:

1. Run file '0 libraries.R': contains libraries required for all code
2. Run all files starting with "1": runs all functions required for simulation - each whole script can be run in one go.
3. Run all files starting with "2" : there is three files, where each runs the simulation for one scenario. Complete datasets can be exported at the end of the script.
4. Run all files starting with "3": runs all functions required for generation of results - each whole script can be run in one go.
5. 4. Run all files starting with "4": there is three files, where each generates results for one scenario.
