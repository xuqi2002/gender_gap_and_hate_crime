#### Preamble ####
# Purpose: Converting and cleaning crime.dta and context.dta file to Fig_1.csv file for Figure 1 
# Author: Xu Qi
# Date: 10 February 2024
# Contact: xu.qi@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

#### Workspace setup ####
library(haven)
library(readr)


#### Data Cleaning (crime) ####
data <- read_dta("inputs/data/crime.dta")
data_top5 <- head(data, 5)
data_top5 <- data_top5[,-1]

df <- data_top5
temp1 <- df[1, "violence_num_cases"]
temp2 <- df[2, "violence_num_cases"]
temp3 <- df[3, "violence_num_cases"]
temp4 <- df[4, "violence_num_cases"]
temp5 <- df[5, "violence_num_cases"]
df[1, "violence_num_cases"] <- temp2
df[2, "violence_num_cases"] <- temp4
df[3, "violence_num_cases"] <- temp5
df[4, "violence_num_cases"] <- temp3
df[5, "violence_num_cases"] <- temp1

folder_path <- file.path("inputs", "data")
file_path <- file.path(folder_path, "crime.csv")
write.csv(df, file_path, row.names = FALSE)

#### Data Cleaning (gendergap) ####
data1 <- read_dta("inputs/data/context.dta")
data_top5 <- head(data1, 5)
subset_data <- data_top5[c("ags_county", "pop_25_44_muni_gendergap_2015")]

folder_path1 <- file.path("inputs", "data")
file_path1 <- file.path(folder_path1, "gendergap.csv")
write_csv(subset_data, file_path1)

#### Data Merging ####

source_df <- read_csv("inputs/data/gendergap.csv", show_col_types = FALSE)

target_df <- read_csv("inputs/data/crime.csv", show_col_types = FALSE)

# Check if both data frames have the same number of rows
if(nrow(source_df) == nrow(target_df)) {
  # Add the column to the target data frame
  target_df$pop_25_44_muni_gendergap_2015 <- source_df$pop_25_44_muni_gendergap_2015
  
  # Save the updated target data frame to a new CSV file
  write_csv(target_df, "inputs/data/Fig_1.csv")
} else {
  cat("The data frames have different numbers of rows. Please align them before adding the column.")
}

