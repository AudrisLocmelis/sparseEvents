# From @stevenworthington on GitHub

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# # usage
# Load packages (and install if necessary)
# source("../mm_common/ipak.R")
# packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
# ipak(packages)
