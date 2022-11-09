# WEAI Bangladesh 
# Occelli M. & Tufan H.
# Script for assembling WEAI dataset after extracting the data with 01_weai_creation_indices file and the Stata dataprep code
# For inquiries on the code, contact Martina Occelli (mo386@cornell.edu)

# Set working directory
dir <- "/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/Input WEAI BIHS" # Change path according to your directory
setwd(dir)

# Load library
library(haven)
library(dplyr)

# Load WEAI modules extracted from BIHS

# Input from core modules for the three waves
input_main_18M <- read_dta("WEAI input_Round3/processed data (mid step)/input_rai_credit_group_leisure_18M.dta")
input_main_18F <- read_dta("WEAI input_Round3/processed data (mid step)/input_rai_credit_group_leisure_18F.dta")
input_main_15M <- read_dta("WEAI_input Round 2/Processed data (mid step)/input_rai_credit_group_leisure_15M.dta")
input_main_15F <- read_dta("WEAI_input Round 2/Processed data (mid step)/input_rai_credit_group_leisure_15F.dta")
input_main_11M <- read_dta("WEAI_input_Round1/Processed data (mid step)/input_rai_credit_group_leisure_11M.dta")
input_main_11F <- read_dta("WEAI_input_Round1/Processed data (mid step)/input_rai_credit_group_leisure_11F.dta")

# Input from the time modules for the three waves (missing 11M due to corrupted file in raw data)
input_time_18M <- read_dta("WEAI_time_module/input_time_18M.dta")
input_time_18F <- read_dta("WEAI_time_module/input_time_18F.dta")
input_time_15M <- read_dta("WEAI_time_module/input_time_15M.dta")
input_time_15F <- read_dta("WEAI_time_module/input_time_15F.dta")
input_time_11F <- read_dta("WEAI_time_module/input_time_11F.dta")

# Input from resource modules for the three waves
var_resource_18M <- read_dta("WEAI_resource_module/var_resource_18M.dta")
var_resource_18F <- read_dta("WEAI_resource_module/var_resource_18F.dta")
var_resource_15M <- read_dta("WEAI_resource_module/var_resource_15M.dta")
var_resource_15F <- read_dta("WEAI_resource_module/var_resource_15F.dta")
var_resource_11M <- read_dta("WEAI_resource_module/var_resource_11M.dta")
var_resource_11F <- read_dta("WEAI_resource_module/var_resource_11F.dta")

# Rename columns in resource modules 
names(var_resource_18M)[1] <- "hhid"
names(var_resource_18F)[1] <- "hhid"
names(var_resource_15M)[1] <- "hhid"
names(var_resource_15F)[1] <- "hhid"
names(var_resource_11M)[1] <- "hhid"
names(var_resource_11F)[1] <- "hhid"

# Assemble each WEAI file for each gender in each year
weai_f_18M <- merge(input_main_18M, input_time_18M, by = "hhid")
weai_file_18M <- merge(weai_f_18M, var_resource_18M, by= "hhid")

weai_f_18F <- merge(input_main_18F, input_time_18F, by = "hhid")
weai_file_18F <- merge(weai_f_18F, var_resource_18F, by= "hhid")

weai_f_15M <- merge(input_main_15M, input_time_15M, by = "hhid")
weai_file_15M <- merge(weai_f_15M, var_resource_15M, by= "hhid")

weai_f_15F <- merge(input_main_15F, input_time_15F, by = "hhid")
weai_file_15F <- merge(weai_f_15F, var_resource_15F, by= "hhid")

weai_f_11M <- merge(input_main_11M, input_time_11M, by = "hhid") # this string is not doable until we do not recover time_hour_11M.dta
weai_file_11M <- merge(input_main_11M, var_resource_11M, by= "hhid")

weai_f_11F <- merge(input_main_11F, input_time_11F, by = "hhid")
weai_file_11F <- merge(weai_f_11F, var_resource_11F, by= "hhid")

# Add in each dataset the year of reference
weai_file_18M$year <- 2018
weai_file_18F$year <- 2018

weai_file_15M$year <- 2015
weai_file_15F$year <- 2015

weai_file_11M$year <- 2011
weai_file_11F$year <- 2011

# Add in each dataset region and district

region_18F <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_BIHS_2018/BIHSRound3/Female/092_bihs_r3_female_mod_a.dta")
region_18M <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_BIHS_2018/BIHSRound3/Male/009_bihs_r3_male_mod_a.dta")

reg_distr_18F <- region_18F[,c(1, 9:16)]
reg_distr_18M <- region_18M[,c(1, 9:16)]

names(reg_distr_18F)[1] <- "hhid"
names(reg_distr_18M)[1] <- "hhid"

weai_data_18F <- merge(weai_file_18F, reg_distr_18F, by = "hhid")
weai_data_18M <- merge(weai_file_18M, reg_distr_18M, by = "hhid")

region_15F <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_B_2015/002_r2_mod_a_female.dta")
region_15M <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_B_2015/001_r2_mod_a_male.dta")

reg_distr_15F <- region_15F[,(1:13)]
reg_distr_15M <- region_15M[,(1:13)]

names(reg_distr_15F)[1] <- "hhid"
names(reg_distr_15M)[1] <- "hhid"

weai_data_15F <- merge(weai_file_15F, reg_distr_15F, by = "hhid")
weai_data_15M <- merge(weai_file_15M, reg_distr_15M, by = "hhid")

region_11F <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_BIHS_2011/Round1/002_mod_a_female.dta")
region_11M <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw data)/dataverse_BIHS_2011/Round1/001_mod_a_male.dta")

reg_distr_11F <- region_11F[,(1:13)]
reg_distr_11M <- region_11M[,(1:13)]


names(reg_distr_11F)[1] <- "hhid"
names(reg_distr_11M)[1] <- "hhid"

weai_data_11F <- merge(weai_file_11F, reg_distr_11F, by = "hhid")
weai_data_11M <- merge(weai_file_11M, reg_distr_11M, by = "hhid")


# Merge data for male and female in each year

names(weai_data_18M) <- tolower(names(weai_data_18M))
names(weai_data_18F) <- tolower(names(weai_data_18F))
weai_final_18 <- rbind(weai_data_18F, weai_data_18M)

names(weai_data_15M) <- tolower(names(weai_data_15M))
names(weai_data_15F) <- tolower(names(weai_data_15F))
weai_final_15 <- rbind(weai_data_15F, weai_data_15M)
weai_final_15 <- weai_final_15[,c(1:17, 20:29)]
names(weai_final_15)[2] <- "hh_type"

weai_reduced_11F <- weai_data_11F[, c(1:11, 16:30)]   #need to create a reduced form as there is not time module for 11M
names(weai_data_11M) <- tolower(names(weai_data_11M))
weai_final_11 <- rbind(weai_reduced_11F, weai_data_11M)

# export final dataset

write_dta(weai_final_11, "weai_final_11.dta")
write_dta(weai_final_15, "weai_final_15.dta")
write_dta(weai_final_18, "weai_final_18.dta")
