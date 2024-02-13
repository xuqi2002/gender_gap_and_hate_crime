#### Preamble ####
# Purpose: Converting and cleaning survey.dta file to Fig_2.csv file for Figure 2 
# Author: Yixuan Yao
# Date: 12 February 2024
# Contact: 
# License: MIT
# Pre-requisites: none

rm(list=ls())

library(haven)

dat <- read.dta13(file = "inputs/data/survey.dta")


dat_use <- dat[dat$wave == 4, ]
write.csv(dat_use, "inputs/data/Fig_2.csv")

