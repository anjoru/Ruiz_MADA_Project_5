Last updated: 2024-04-05
Overview

These folders contain the files needed to reproduce the project outlined in the manuscript. 

Most of the work associated with Part 3 is in the R/eda-code folder. it is still under development, as is the manuscript. 


assets: Contains the figures and tables generated for the project.
data: Contains the data used in the project. The raw-data folder contains the original data files. The processed-data folder contains the data files that have been cleaned and processed for analysis.
products: Contains the manuscript and any other products generated from the project.It will also contain any supplemental materials. Currently, there are not supplemental materials
R: Contains the R scripts used in the project. The scripts are organized by the phase of the project in which they were used. The scripts are still evolving.
renv: Contains the R environment files needed to reproduce the project. When you use renv in a project, it creates a private R library just for that project. This means the project uses specific versions of packages, independent of what might be installed in the system-wide or user-specific R library. This isolation prevents conflicts between package versions needed by different projects.
results: Contains the tables and figures generated in the project. These files are the final products of the project. They are the files that are used in the manuscript and other products. 
dataanalysis-template.Rproj: This is the R project file. It is the file you open in RStudio to work on the project. It contains the settings for the project, such as the working directory and the R environment.