# WEAI & BIHS Bangladesh
# Occelli M. & Tufan H.
# BIHS  & WEAI MODULES - econometric analysis part II
# For inquiries on the code, contact Martina Occelli (mo386@cornell.edu)

# Load packages
library(haven)
library(xlsx)
library(foreign)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(ggplot2)
library(gghighlight)
library(sf)
library(raster)
library(tmap) 
library(ncdf4)
library(waffle)
library(hrbrthemes)
library(RColorBrewer)
library(pglm)
library(mclogit)
library(survival)
library(bife)
library(alpaca)

# Set working directory
setwd("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/Data_final_crop")

# Retrieve final dataset
final_cereal <- read_dta("final_cereal.dta")
final_fiber <- read_dta("final_fiber.dta")
final_pulses <- read_dta("final_pulses.dta")
final_oil <- read_dta("final_oil.dta")
final_spices <- read_dta("final_spices.dta")
final_vegetables <- read_dta("final_vegetables.dta")
final_fruits <- read_dta("final_fruits.dta")
final_other <- read_dta("final_other.dta")

# Now we want to add the module on gender role in crop labor - module H5
crop_labor_a_15 <- 
  read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)/dataverse_2015/019_r2_mod_h5_male.dta")
crop_labor_a_18 <- 
  read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)/dataverse_2018/BIHSRound3/Male/025_bihs_r3_male_mod_h5.dta")

crop_labor_b_15 <- 
  read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)/dataverse_2015/020_r2_mod_h6_male.dta")
crop_labor_b_18 <- 
  read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)/dataverse_2018/BIHSRound3/Male/026_bihs_r3_male_mod_h6.dta")


# I need to calculate for each household's plot, when the number of hours worked by women exceed those worked by men
crop_labor_a_15 <- crop_labor_a_15[, c(1, 5, 7, 8, 9, 11, 13, 14, 15, 17, 19, 20, 21, 23, 25, 26, 27, 29, 31, 32, 33, 35, 
                                    37, 38, 39, 41, 43, 44, 45, 47, 49, 50, 51, 53)]
crop_labor_a_18 <- crop_labor_a_18[, c(1, 4, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 26, 27, 28, 30, 32, 33, 34, 36, 
                                    38, 39, 40, 42, 44, 45, 46, 48)]

crop_labor_b_15 <- crop_labor_b_15[, c(1,4, 5, 6, 8, 14, 15, 16, 18, 20,21, 22, 24, 26, 27, 28, 30, 32, 33, 34, 36)]
crop_labor_b_18 <- crop_labor_b_18[, c(1,4, 5, 6, 8, 14, 15, 16, 18, 20, 21, 22, 24, 26, 27, 28, 30, 32, 33, 34, 36)]

crop_labor_15 <- merge(crop_labor_a_15, crop_labor_b_15, by = "a01") # first family labor male
crop_labor_18 <- merge(crop_labor_a_18, crop_labor_b_18, by = "a01")

crop_labor_15 <- mutate(crop_labor_15,
                crop_group = case_when(
                  crop_a >= 10 & crop_a <=30  ~ "cereal", 
                  crop_a >= 41 & crop_a <= 45  ~ "fiber",
                  crop_a >= 51 & crop_a <= 59  ~ "pulses",
                  crop_a >= 61 & crop_a <= 67  ~ "oil",
                  crop_a >= 71 & crop_a <= 77  ~ "spices", 
                  crop_a >= 101 & crop_a <= 213  ~ "vegetables",
                  crop_a >= 301 & crop_a <= 326  ~ "fruits",
                  crop_a >= 411 & crop_a <= 710 ~ "other"
                )) # explicit crop groups, 2015

crop_labor_18 <- mutate(crop_labor_18,
                        crop_group = case_when(
                          crop_a_h5 >= 10 & crop_a_h5 <=30  ~ "cereal", 
                          crop_a_h5 >= 41 & crop_a_h5 <= 45  ~ "fiber",
                          crop_a_h5 >= 51 & crop_a_h5 <= 59  ~ "pulses",
                          crop_a_h5 >= 61 & crop_a_h5 <= 67  ~ "oil",
                          crop_a_h5 >= 71 & crop_a_h5 <= 77  ~ "spices", 
                          crop_a_h5 >= 101 & crop_a_h5 <= 213  ~ "vegetables",
                          crop_a_h5 >= 301 & crop_a_h5 <= 326  ~ "fruits",
                          crop_a_h5 >= 411 & crop_a_h5 <= 710 ~ "other"
                        )) # explicit crop groups, 2018

crop_labor_15$sum_male_hour = rowSums(crop_labor_15[,c(3,5,7,9,11,13,15,17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53)])
crop_labor_15$sum_female_hour = rowSums(crop_labor_15[,c(4,6,8,10,12,14,16,18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54)])

crop_labor_15$hired_male_hour = rowSums(crop_labor_15[,c(5,9,13,17,21,25,29,33,37, 41, 45, 49,53)])
crop_labor_15$hired_female_hour = rowSums(crop_labor_15[,c(6,10,14,18,22,26,30,34,38,42,46,50,54)])

crop_labor_18$sum_male_hour = rowSums(crop_labor_18[,c(3,5,7,9,11,13,15,17, 19, 21, 23, 25, 27, 29, 32, 33, 35, 37, 39, 41, 43, 45, 47, 49)])
crop_labor_18$sum_female_hour = rowSums(crop_labor_18[,c(4,6,8,10,12,14,16,18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50)])

crop_labor_18$hired_male_hour = rowSums(crop_labor_18[,c(5,9,13,17,21,25, 29, 33, 37, 41, 45, 49)])
crop_labor_18$hired_female_hour = rowSums(crop_labor_18[,c(6,10,14,18,22,26, 30, 34, 38, 42, 46, 50)])

# We define a plot managed by a women if the number of hours spent on a crop by a woman are more than the one of men
crop_labor_15$women_managed <- ifelse(crop_labor_15$sum_female_hour > crop_labor_15$sum_male_hour, 1, 0)
crop_labor_18$women_managed <- ifelse(crop_labor_18$sum_female_hour > crop_labor_18$sum_male_hour, 1, 0)

# Extract dataset for usage in script 08
crop_labor_15 <- crop_labor_15[, c(1, 55:60)]
crop_labor_18 <- crop_labor_18[, c(1, 51:56)]

crop_labor_15$year <- 2015
crop_labor_18$year <- 2018

crop_labor <- rbind(crop_labor_15, crop_labor_18)

write_dta(crop_labor, "crop_labor.dta")

# Finalize the dataset for the two years
crop_15 <- crop_labor_15[,c(1,29,32)]
crop_15$year <- 2015
crop_18 <- crop_labor_18[,c(1,27,30)]
crop_18$year <- 2018

crop_labor <- rbind(crop_15, crop_18)
crop_labor <- crop_labor[order(crop_labor$a01),]

# Again, we create a dataset for each crop group
labor_cereal <- crop_labor%>%
  filter(crop_group == "cereal")

labor_fiber <- crop_labor%>%
  filter(crop_group == "fiber")

labor_pulses <- crop_labor%>%
  filter(crop_group == "pulses")

labor_oil <- crop_labor%>%
  filter(crop_group == "oil")

labor_spices <- crop_labor%>%
  filter(crop_group == "spices")

labor_vegetables <- crop_labor%>%
  filter(crop_group == "vegetables")

labor_fruits <- crop_labor%>%
  filter(crop_group == "fruits")

labor_other <- crop_labor%>%
  filter(crop_group == "other")

# For some households, plots of the same crop group are managed predominantly by both men and women 
# In this cases, we calculated what is the most frequent predominant labor in each crop group
# We call this c

Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
} # function which takes the highest frequent value in every row

labor_cereal <- labor_cereal %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_fiber <- labor_fiber %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_pulses <- labor_pulses %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_oil <- labor_oil %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_spices <- labor_spices %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_vegetables <- labor_vegetables %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_fruits <- labor_fruits %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))
labor_other <- labor_other %>% group_by(a01, year) %>% mutate(c=Mode(women_managed))

labor_cereal <- labor_cereal %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_cereal_wide <- labor_cereal %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_cereal_wide <- labor_cereal_wide[,c(1:4)]

labor_fiber <- labor_fiber %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_fiber_wide <- labor_fiber %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_fiber_wide <- labor_fiber_wide[,c(1:4)]

labor_pulses <- labor_pulses %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_pulses_wide <- labor_pulses %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_pulses_wide <- labor_pulses_wide[,c(1:4)]

labor_oil <- labor_oil %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_oil_wide <- labor_oil %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_oil_wide <- labor_oil_wide[,c(1:4)]

labor_spices <- labor_spices %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_spices_wide <- labor_spices %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_spices_wide <- labor_spices_wide[,c(1:4)]

labor_vegetables <- labor_vegetables %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_vegetables_wide <- labor_vegetables %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_vegetables_wide <- labor_vegetables_wide[,c(1:4)]

labor_fruits <- labor_fruits %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_fruits_wide <- labor_fruits %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_fruits_wide <- labor_fruits_wide[,c(1:4)]

labor_other <- labor_other %>% group_by(a01, year) %>% mutate(n = row_number()) # how many observations for hhid and year
labor_other_wide <- labor_other %>%
  pivot_wider(names_from = n, values_from = c(women_managed)) # reshape in wide format the labor division
labor_other_wide <- labor_other_wide[,c(1:4)]


# We are now left with the task of merging the dataset
new_cereal <- merge(final_cereal, labor_cereal_wide, by = c("a01", "year"))
new_fiber <- merge(final_fiber, labor_fiber_wide, by = c("a01", "year"))
new_pulses <- merge(final_pulses, labor_pulses_wide, by = c("a01", "year"))
new_oil <- merge(final_oil, labor_oil_wide, by = c("a01", "year"))
new_spices <- merge(final_spices, labor_spices_wide, by = c("a01", "year"))
new_vegetables <- merge(final_vegetables, labor_vegetables_wide, by = c("a01", "year"))
new_fruits <- merge(final_fruits, labor_fruits_wide, by = c("a01", "year"))
new_other <- merge(final_other, labor_other_wide, by = c("a01", "year"))

# We clean the dataset
new_cereal <- new_cereal[,c(1:16, 19)]
new_fiber <- new_fiber[,c(1:16, 19)]
new_pulses <- new_pulses[,c(1:16, 19)]
new_oil <- new_oil[,c(1:16, 19)]
new_spices <- new_spices[,c(1:16, 19)]
new_vegetables <- new_vegetables[,c(1:16, 19)]
new_fruits <- new_fruits[,c(1:16, 19)]
new_other <- new_other[,c(1:16, 19)]

##### Econometric exercise (part II) ####

new_cereal <- new_cereal %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

a <- new_cereal %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 1,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 0
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, a, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_fiber <- new_fiber %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

b <- new_fiber %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, b, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_pulses <- new_pulses %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

c <- new_pulses %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, c, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_oil <- new_oil %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

d <- new_oil %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 1,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 0
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, d, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_spices <- new_spices %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

e <- new_spices %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, e, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_vegetables <- new_vegetables %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

f <- new_vegetables %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 1,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 0
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, f, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_fruits <- new_fruits %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

g <- new_fruits %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, g, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

new_other <- new_other %>%
  mutate( trait_type_10 = case_when(
    hf_h1_10 == 1 ~ "Agronomic",
    hf_h1_10 >= 4 & hf_h1_10 <= 7 ~ "Abiotic stress",
    hf_h1_10 == 3 ~ "Biotic stress",
    hf_h1_10 == 2 |  hf_h1_10 == 13  ~ "Morphological",
    hf_h1_10 == 14 | hf_h1_10 >= 8 & hf_h1_10 <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    hf_h1_11 == 1 ~ "Agronomic",
    hf_h1_11 >= 4 & hf_h1_11 <= 7 ~ "Abiotic stress",
    hf_h1_11 == 3 ~ "Biotic stress",
    hf_h1_11 == 2 |  hf_h1_11 == 13  ~ "Morphological",
    hf_h1_11 == 14 | hf_h1_11 >= 8 & hf_h1_11 <= 12 ~ "Quality"
  )) 

h <- new_other %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 1,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 0
  ))

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(c)| district + year, h, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level


# Plot the difference in terms of barplots
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(stat,  sort.est = TRUE)
