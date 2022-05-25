### Visualising the mobility NPI results

# Results from script 1_Mobility_NPIs.R

# Date started: 25th May 2022


# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)



# Boxplots for beta values ------------------------------------------------

regression_lockdown1<-read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/NPIs_mobility/Outputs/Regional_workplace_lockdown1_regression_results.csv")

re