Last updated: 2024-04-29
# Overview
## This README file provides an overview of the project and instructions for reproducing the project. It will serve as the main source of information for the project. Some, but not all, folders may contain their own README files with additional information.
### Below is a list and description of the folders and their content in the project directory.

1. **assets:** Contains the bibliography files.
2. **data:** Contains the data used in the project. 
   - The raw-data folder contains the original data files.
      - Notes in the raw data qmd files provide information about the data, such as the source.
   - The processed-data folder contains the data files that have been cleaned and processed for analysis.
      - Also contains a folder of RDA files that are used to store the data in a compressed format.
3. **data_dictionary:** Contains a searchable data dictionary tool that can be open by running the file [Data Dictionary Search Tool](data_dictionary/data_dict_search_tool.qmd)
   - The searchable tool is only for those files in the **_processed-data_** files in the project. 
   - It does not contain descriptions of the variables in the raw files since many of those are removed before anaylsis.
   - The data dictionary provides information about the variables in the data files, such as their names, descriptions, and units of measurement.
4. **products:** Contains the manuscript and any other products generated from the project.It will also contain any supplemental materials. Currently, there are not supplemental materials
5. **R:** Contains the R scripts used in the project. The scripts are organized by the phase of the project in which they were used. The order of the script should be as follows: processing-code  →  eda-code  → analysis-code.
   - analysis-code: Contains the scripts used to analyze the data and generate the results.
   - eda-code: Contains the scripts used to explore the data and generate the tables and figures used in the exploratory data analysis.
   - processing-code: Contains the scripts used to clean and process the data.
   - *Refer to the comments in the scripts for more information on their purpose and use.*
6. **renv:** Contains the R environment files needed to reproduce the project. When you use renv in a project, it creates a private R library just for that project. This means the project uses specific versions of packages, independent of what might be installed in the system-wide or user-specific R library. This isolation prevents conflicts between package versions needed by different projects.
7. **results:** Contains the tables and figures generated in the project. These files are the final products of the project. They are the files that are used in the manuscript and other products. 
8. **dataanalysis-template.Rproj:** This is the R project file. It is the file you open in RStudio to work on the project. It contains the settings for the project, such as the working directory and the R environment.
