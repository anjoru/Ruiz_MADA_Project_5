FINAL

This folder contains the project's R environment as managed by the renv package. It ensures reproducibility by capturing the exact state of all R packages used in this project. Here you'll find:

renv.lock: A lockfile storing the versions and sources of R packages required for this project.
settings.dcf: Configuration settings for the renv environment.
library/: A directory where the specific versions of packages are stored.
Usage: To restore the project environment on a new machine or a different setup, run renv::restore() in R. This will install all necessary packages as specified in the renv.lock file, ensuring that the project can be run with the same package environment as when it was last worked on.