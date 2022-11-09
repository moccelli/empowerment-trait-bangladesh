# WEAI & BIHS Bangladesh
# Occelli M. & Tufan H.
# BIHS  & WEAI MODULES - econometric analysis part I
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
setwd("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)")

#### Assemble final dataset ####

# HOUSEHOLD LEVEL DATA

mod_a_11 <- read_dta("dataverse_2011/Round1/001_mod_a_male.dta") # household data, 2011
names(mod_a_11) <- tolower(names(mod_a_11))
mod_a_11$year <- 2011

mod_a_15 <- read_dta("dataverse_2015/001_r2_mod_a_male.dta") # household data, 2015
names(mod_a_15) <- tolower(names(mod_a_15))
mod_a_15$year <- 2015

mod_a_18 <- read_dta("dataverse_2018/BIHSRound3/Male/009_bihs_r3_male_mod_a.dta") # household data, 2018
names(mod_a_18) <- tolower(names(mod_a_18))
mod_a_18$year <- 2018

a_11 <- mod_a_11[, c(1:3, 7, 9, 16, 40)]
a_15 <- mod_a_15[, c(1,4,6, 7, 9, 19, 22, 29)]
a_18 <- mod_a_18[, c(1, 9, 11, 12, 16, 18, 60)]

d <- merge(a_11, a_15, by="a01") # assemble household data in a wide format
da <- merge(a_15, a_18, by="a01")
da <- da[, c(1, 4, 5, 6, 7, 8, 13, 14)]
names(da) <- c("a01", "dis_code", "district", "village_code","a10_r2", "year_r2", "a10_r3", "year_r3")

da$a10_r12 <- (da$a10_r1 - da$a10_r2) # to calculate when the main respondent was different from r1 to r2 (82% equal)
table(da$a10_r12)

da$a10_r23 <- (da$a10_r2 - da$a10_r3) # to calculate when the main respondent was different from r1 to r2 (74% equal)
table(da$a10_r23)

da$resp_diff_r12 <- ifelse(da$a10_r12 == 0, 0, 1) # create a dummy to account for these changes in respondents
da$resp_diff_r23 <- ifelse(da$a10_r23 == 0, 0, 1)

da <- da[, c(1:9, 12, 13)] # keep the dataset clean from useless variables (first dataset)

write_dta(da, "da.dta") #save to be further utilized in script 08 - extra step

mod_b1_11 <- read_dta("dataverse_2011/Round1/003_mod_b1_male.dta") # household composition module, 2011
mod_b1_15 <- read_dta("dataverse_2015/003_r2_male_mod_b1.dta") # household composition module, 2015
mod_b1_18 <- read_dta("dataverse_2018/BIHSRound3/Male/010_bihs_r3_male_mod_b1.dta") # household composition module, 2018

mod_b1_11 <- mod_b1_11[, c(1:5, 9:10)] # select only variable of interest for the demographic summary
names(mod_b1_11) <- c("a01", "a10_r1", "sex_r1", "age_r1", "relation_r1", "literacy_r1", "education_r1")
mod_b1_15 <- mod_b1_15[, c(1,2,4:6, 12,13)]
names(mod_b1_15) <- c("a01", "a10_r2", "sex_r2", "age_r2", "relation_r2", "literacy_r2", "education_r2")
mod_b1_18 <- mod_b1_18[, c(1,6, 8,9,15, 23, 24)]
names(mod_b1_18) <- c("a01", "a10_r3", "sex_r3", "age_r3", "relation_r3", "literacy_r3", "education_r3")

dat <- merge(da, mod_b1_11, by = c("a01", "a10_r1"))
data <- merge(dat, mod_b1_15, by = c("a01", "a10_r2"))
data_1 <- merge(data, mod_b1_18, by = c("a01", "a10_r3")) # household data and composition (second dataset)

write_dta(data_1, "data_1.dta") #save to be further utilized in script 08 - extra step

# Information contained in the data_1 dataset are: hhid, respondent characteristics for each of the three rounds (r1, r2, r3)

# PLOT LEVEL DATA - we focus firstly on our main outcomes

mod_h1_11 <- read_dta("dataverse_2011/Round1/011_mod_h1_male.dta") # agriculture, 2011 - here we do not have crop information but we have the WEAI
mod_h1_15 <- read_dta("dataverse_2015/015_r2_mod_h1_male.dta") # agriculture module, 2015
mod_h1_18 <- read_dta("dataverse_2018/BIHSRound3/Male/021_bihs_r3_male_mod_h1.dta") # agriculture module, 2018

table(mod_h1_15$h1_10) # first trait preferred - variable of interest, 2015
table(mod_h1_15$h1_11) # second trait preferred - variable of interest, 2015

table(mod_h1_18$h1_10) # first trait preferred - variable of interest, 2018
table(mod_h1_18$h1_11) # second trait preferred - variable of interest, 2018

h1_18 <- mod_h1_18[,c(1, 6:37, 39)]
h1_18_2 <- h1_18[,c(1:5,7,9:22, 24,26,28,30,32,34)]
h1_18_3 <- h1_18_2[,c(1:17, 19:26)]

h1_18 <- h1_18_3 %>% relocate(hh_type, .after = a01) %>% relocate(varity_name, .after = crop_b_h1)

h1_15 <- mod_h1_15
h1_18 <-  rename(h1_18, plotid = plotid_h1) 
h1_18 <-  rename(h1_18, crop_a = crop_a_h1) 
h1_18 <-  rename(h1_18, crop_b = crop_b_h1)

h1_18 <- h1_18[, c(1, 5, 6, 7, 11, 20, 21)] # isolate only the outcomes of interest, 2015
h1_15 <- h1_15[,c(1, 5, 6, 7, 11, 20, 21)] # isolate only the outcomes of interest, 2018

h1_18$crop_group <- NA
h1_15$crop_group <- NA

h1_15 <- mutate(h1_15,
                crop_group = case_when(
                  crop_a >= 10 & crop_a <=30 | crop_b >= 10 & crop_b<=30  ~ "cereal", 
                  crop_a >= 41 & crop_a <= 45 | crop_b >= 41 & crop_b<= 45 ~ "fiber",
                  crop_a >= 51 & crop_a <= 59 | crop_b >= 51 & crop_b<= 59 ~ "pulses",
                  crop_a >= 61 & crop_a <= 67 | crop_b >= 61 & crop_b<= 67 ~ "oil",
                  crop_a >= 71 & crop_a <= 77 | crop_b >= 71 & crop_b<= 77 ~ "spices", 
                  crop_a >= 101 & crop_a <= 213 | crop_b >= 101 & crop_b<= 213 ~ "vegetables",
                  crop_a >= 301 & crop_a <= 326 | crop_b >= 301 & crop_b<= 326 ~ "fruits",
                  crop_a >= 411 & crop_a <= 710 | crop_b >= 411 & crop_b<= 710 ~ "other"
                )) # explicit crop groups, 2015

h1_18 <- mutate(h1_18,
                crop_group = case_when(
                  crop_a >= 10 & crop_a <=30 | crop_b >= 10 & crop_b<=30  ~ "cereal", 
                  crop_a >= 41 & crop_a <= 45 | crop_b >= 41 & crop_b<= 45 ~ "fiber",
                  crop_a >= 51 & crop_a <= 59 | crop_b >= 51 & crop_b<= 59 ~ "pulses",
                  crop_a >= 61 & crop_a <= 67 | crop_b >= 61 & crop_b<= 67 ~ "oil",
                  crop_a >= 71 & crop_a <= 77 | crop_b >= 71 & crop_b<= 77 ~ "spices", 
                  crop_a >= 101 & crop_a <= 213 | crop_b >= 101 & crop_b<= 213 ~ "vegetables",
                  crop_a >= 301 & crop_a <= 326 | crop_b >= 301 & crop_b<= 326 ~ "fruits",
                  crop_a >= 411 & crop_a <= 710 | crop_b >= 411 & crop_b<= 710 ~ "other"
                )) # explicit crop groups, 2018

h1_15 <- h1_15[,c(1,2, 5:7)]
h1_18 <- h1_18[,c(1,2, 5:7)]

write_dta(h1_15, "crop_info_15.dta") #save to be further utilized in script 08 - extra step
write_dta(h1_18, "crop_info_18.dta") #save to be further utilized in script 08 - extra step

# I am interested to have one observation per household for each crop group

# Cereal 2015
y <- h1_15 %>%
  filter(crop_group == "cereal") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

# Now we have an issue, because in some households preferences for the same crop do not match in the same year 
# This is probably due to crop specific requirements, therefore we calculate the trait most frequent

y_15_wide$freq_h1_10 <- NA
y_15_wide$freq_h1_11 <- NA

Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
} # function which takes the highest frequent value in every row

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:34], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 35:63], 1, Mode) # calculate it for h1_11, 2015

# We clean the data and we add it to the main dataset
ce_15 <- y_15_wide[, c(1:5, 93,94)]
names(ce_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
ce_15$crop_group  <- "cereal"
ce_15$year <- 2015

# Now we repeat the same reasoning for 2018 and for all the crops group
# cereal 2018
y <- h1_18 %>%
  filter(crop_group == "cereal") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- NA
y_18_wide$freq_h1_11 <- NA

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:28], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 29:51], 1, Mode) # calculate it for h1_11, 2015

# We clean the data and we add it to the main dataset
ce_18 <- y_18_wide[, c(1:5, 75, 76)]
names(ce_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
ce_18$crop_group  <- "cereal"
ce_18$year <- 2018

# fiber, 2015
y <- h1_15 %>%
  filter(crop_group == "fiber") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- NA
y_15_wide$freq_h1_11 <- NA

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:18], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 19:31], 1, Mode) # calculate it for h1_11, 2015

# We clean the data and we add it to the main dataset
fi_15 <- y_15_wide[, c(1:5, 45, 46)]
names(fi_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
fi_15$crop_group  <- "fiber"
fi_15$year <- 2015

# fiber, 2018
y <- h1_18 %>%
  filter(crop_group == "fiber") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:17], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 18:29], 1, Mode) # calculate it for h1_11, 2015

# We clean the data and we add it to the main dataset
fi_18 <- y_18_wide[, c(1:5, 42, 43)]
names(fi_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
fi_18$crop_group  <- "fiber"
fi_18$year <- 2018

# pulses, 2015
y <- h1_15 %>%
  filter(crop_group == "pulses") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:33], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 34:61], 1, Mode) # calculate it for h1_11, 2015

pu_15 <- y_15_wide[, c(1:5, 90,91)]
names(pu_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
pu_15$crop_group  <- "pulses"
pu_15$year <- 2015

# pulses, 2018
y <- h1_18 %>%
  filter(crop_group == "pulses") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:18], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 19:31], 1, Mode) # calculate it for h1_11, 2015

pu_18 <- y_18_wide[, c(1:5,45, 46)]
names(pu_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
pu_18$crop_group  <- "pulses"
pu_18$year <- 2018

# oil, 2015
y <- h1_15 %>%
  filter(crop_group == "oil") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:18], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 19:31], 1, Mode) # calculate it for h1_11, 2015

oil_15 <- y_15_wide[, c(1:5, 45, 46)]
names(oil_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
oil_15$crop_group  <- "oil"
oil_15$year <- 2015

# oil, 2018
y <- h1_18 %>%
  filter(crop_group == "oil") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:15], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 16:25], 1, Mode) # calculate it for h1_11, 2015

oil_18 <- y_18_wide[, c(1:5,36, 37)]
names(oil_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
oil_18$crop_group  <- "oil"
oil_18$year <- 2018

# spices, 2015
y <- h1_15 %>%
  filter(crop_group == "spices") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:17], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 18:29], 1, Mode) # calculate it for h1_11, 2015

spices_15 <- y_15_wide[, c(1:5, 42,43)]
names(spices_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
spices_15$crop_group  <- "spices"
spices_15$year <- 2015

# spices, 2018
y <- h1_18 %>%
  filter(crop_group == "spices") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:16], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 15:27], 1, Mode) # calculate it for h1_11, 2015

spices_18 <- y_18_wide[, c(1:5,39, 40)]
names(spices_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
spices_18$crop_group  <- "spices"
spices_18$year <- 2018

# vegetables, 2015
y <- h1_15 %>%
  filter(crop_group == "vegetables") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:13], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 14:21], 1, Mode) # calculate it for h1_11, 2015

vegetables_15 <- y_15_wide[, c(1:5, 30, 31)]
names(vegetables_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
vegetables_15$crop_group  <- "vegetables"
vegetables_15$year <- 2015

# vegetables, 2018
y <- h1_18 %>%
  filter(crop_group == "vegetables") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:13], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 14:21], 1, Mode) # calculate it for h1_11, 2015

vegetables_18 <- y_18_wide[, c(1:5, 30,31)]
names(vegetables_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
vegetables_18$crop_group  <- "vegetables"
vegetables_18$year <- 2018

# fruits, 2015
y <- h1_15 %>%
  filter(crop_group == "fruits") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:9], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 10:13], 1, Mode) # calculate it for h1_11, 2015

fruits_15 <- y_15_wide[, c(1:5, 18, 19)]
names(fruits_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
fruits_15$crop_group  <- "fruits"
fruits_15$year <- 2015

# fruits, 2018
y <- h1_18 %>%
  filter(crop_group == "fruits") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:10], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 11:15], 1, Mode) # calculate it for h1_11, 2015

fruits_18 <- y_18_wide[, c(1:5,21,22)]
names(fruits_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
fruits_18$crop_group  <- "fruits"
fruits_18$year <- 2018

# other, 2015
y <- h1_15 %>%
  filter(crop_group == "other") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_15_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_15_wide$freq_h1_10 <- apply(y_15_wide[, 6:20], 1, Mode) # calculate it for h1_10, 2015
y_15_wide$freq_h1_11 <- apply(y_15_wide[, 21:35], 1, Mode) # calculate it for h1_11, 2015

other_15 <- y_15_wide[, c(1:5, 51, 52)]
names(other_15) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
other_15$crop_group  <- "other"
other_15$year <- 2015

# other, 2018
y <- h1_18 %>%
  filter(crop_group == "other") 
y <- y %>% group_by(a01, h1_season) %>% mutate(n = row_number()) # how many observations for hhid and for season

y_18_wide <- y %>%
  pivot_wider(names_from = n, values_from = c(h1_10, h1_11, crop_group)) # reshape in wide format the trait preferences, 2015

y_18_wide$freq_h1_10 <- apply(y_18_wide[, 6:17], 1, Mode) # calculate it for h1_10, 2015
y_18_wide$freq_h1_11 <- apply(y_18_wide[, 18:29], 1, Mode) # calculate it for h1_11, 2015

other_18 <- y_18_wide[, c(1:5,42, 43)]
names(other_18) <- c("a01", "season_r2", "crop_a", "crop_b", "area_planted", "h1_10_f_r2", "h1_11_f_r2")
other_18$crop_group  <- "other"
other_18$year <- 2018

# Now we elaborate the information per crop group

cereal <- rbind(ce_15, ce_18)
cereal <- cereal[order(cereal$a01),] 
names(cereal) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")

fiber <- rbind(fi_15, fi_18)
fiber <- fiber[order(fiber$a01),] 
names(fiber) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")

pulses <- rbind(pu_15, pu_18)
pulses <- pulses[order(pulses$a01),] 
names(pulses) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")

oil <- rbind(oil_15, oil_18)
oil <- oil[order(oil$a01),] 
names(oil) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")

spices <- rbind(spices_15, spices_18)
spices <- spices[order(spices$a01),] 
names(spices) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")
  
vegetables <- rbind(vegetables_15, vegetables_18)
vegetables <- vegetables[order(vegetables$a01),] 
names(vegetables) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")
  
fruits <- rbind(fruits_15, fruits_18)
fruits <- fruits[order(fruits$a01),] 
names(fruits) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")
  
other <- rbind(other_15, other_18)
other <- other[order(other$a01),] 
names(other) <- c("a01", "harvest_season", "crop_a", "crop_b", "area_planted","hf_h1_10", "hf_h1_11", "crop_group", "year")

a <- rbind(cereal, fiber)
al <- rbind(a, pulses)
all <- rbind(al, oil)
all_t <- rbind(all, spices)
all_tr <- rbind(all_t, vegetables)
all_tra <- rbind(all_tr, fruits)
all_traits <- rbind(all_tra, other)

all_traits <- all_traits[order(all_traits$a01),] 

# Now let's merge the two datasets for having our first clean data
# all_traits is in long format, while data_1 is in wide format
# change data_1 in long format, as we will use a time series model

data_1 <- data_1[order(data_1$a01),]

w <- data_1[, c(1, 4:7, 14:18)]
names(w) <- c("a01", "a10", "dis_code", "district", "year", "sex", "age", "relation", "literacy", "education")

wi <- data_1[,c(1,3,5,6,8, 19:23)]
names(wi) <- c("a01", "a10", "dis_code", "district", "year", "sex", "age", "relation", "literacy", "education")

wid <- data_1[,c(1, 2, 5, 6, 9, 24:28)]
names(wid) <- c("a01", "a10", "dis_code", "district", "year", "sex", "age", "relation", "literacy", "education")

wide <- rbind(w, wi)
data_wide <- rbind(wide, wid)

data_wide <- data_wide[order(data_wide$a01),]

# We merge the two datasets - merged is the final dataset containing household and agronomic data for 2015 and 2018

merged <- merge(data_wide, all_traits, by=c("a01", "year"))
merged <- merged[order(merged$a01),]

write_dta(merged, "merged.dta")

u <- merged %>% 
  group_by(year, harvest_season, crop_group) %>%
  summarise(n=n()) # trait preferences, per year, harvesting season and crop group

# To complete the dataset, we need the WEAI data we calculated before
# first we need to load the WEAI datasets

# Empowerment dimensions
weai_final_15 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_final_input_stata/weai_final_15.dta")
weai_final_18 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_final_input_stata/weai_final_18.dta")

D5_district_disaggregated_15 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/5D_gender_district_disaggregated_15.dta")
D5_gender_district_disaggregated_18 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/5D_gender_district_disaggregated_18.dta")

#5D - I cannot use this data because I am missing the district of reference



# GPI
GPI_15 <- read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_15.dta")
GPI_18 <- read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_18.dta")

GPI_15 <- GPI_15[,c(1,3, 17, 76, 78)]
GPI_18 <- GPI_18[,c(1,2, 19, 76, 78)]

GPI_15 <- GPI_15 %>%
  rename(a01 = hhid)

GPI_18 <- GPI_18 %>%
  rename(a01 = hhid)

GPI_total <- rbind(GPI_15, GPI_18) # create a unique GPI dataset
GPI_total <- GPI_total[order(GPI_total$a01),]

GPI_data <- GPI_total %>%
  filter(sex == 2) # we keep only one sex as the GPI is identical by construction for the two sexes

f <- merge(GPI_data, merged, by = c("a01", "year"))
f <- f[order(f$a01),]
f <- f[,c(1,2, 5:20)] # this is the final dataset with WEAI and crop data merged 
f <- f %>%
  rename(sex = sex.y)

write_dta(f, "final.dta") 

# Now let's create a dataset for each crop group
f_cereal <- f%>%
  filter(crop_group == "cereal")

f_fiber <- f%>%
  filter(crop_group == "fiber")

f_pulses <- f%>%
  filter(crop_group == "pulses")

f_oil <- f%>%
  filter(crop_group == "oil")

f_spices <- f%>%
  filter(crop_group == "spices")

f_vegetables <- f%>%
  filter(crop_group == "vegetables")

f_fruits <- f%>%
  filter(crop_group == "fruits")

f_other <- f%>%
  filter(crop_group == "other")

# Save the final dataset - so that they can be recalled in other files
write_dta(f_cereal, "final_cereal.dta")
write_dta(f_fiber, "final_fiber.dta")
write_dta(f_pulses, "final_pulses.dta")
write_dta(f_oil, "final_oil.dta")
write_dta(f_spices, "final_spices.dta")
write_dta(f_vegetables, "final_vegetables.dta")
write_dta(f_fruits, "final_fruits.dta")
write_dta(f_other, "final_other.dta")


####### Econometric analysis ####### 

# Model (pglm package) for trait frequency (1st preferred trait)

m_f_cereal <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_cereal, index = "year", effect = "individual")
summary(m_f_cereal)

m_f_fiber <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_fiber, index = "year", effect = "individual")
summary(m_f_fiber)

m_f_pulses <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_pulses, index = "year", effect = "individual")
summary(m_f_pulses)

m_f_oil <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_oil, index = "year", effect = "individual")
summary(m_f_oil)

m_f_spices <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_spices, index = "year", effect = "individual")
summary(m_f_spices)

m_f_vegetables <- pglm(hf_h1_10 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_vegetables, index = "year", effect = "individual")
summary(m_f_vegetables)

m_f_fruits <- pglm(hf_h1_10 ~ GPI +  age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_fruits, index = "year", effect = "individual")
summary(m_f_fruits)

m_f_other <- pglm(hf_h1_10 ~ GPI +  age  + as_factor(literacy), family = "poisson", model = "within", data = f_other, index = "year", effect = "individual")
summary(m_f_other)

# Model (pglm package) for trait frequency (2nd preferred trait)

m_f_cereal <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_cereal, index = "year", effect = "individual")
summary(m_f_cereal)

m_f_fiber <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_fiber, index = "year", effect = "individual")
summary(m_f_fiber)

m_f_pulses <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_pulses, index = "year", effect = "individual")
summary(m_f_pulses)

m_f_oil <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_oil, index = "year", effect = "individual")
summary(m_f_oil)

m_f_spices <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_spices, index = "year", effect = "individual")
summary(m_f_spices)

m_f_vegetables <- pglm(hf_h1_11 ~ GPI + as_factor(sex) + age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_vegetables, index = "year", effect = "individual")
summary(m_f_vegetables)

m_f_fruits <- pglm(hf_h1_11 ~ GPI +  age + as_factor(literacy) + district, family = "poisson", model = "within", data = f_fruits, index = "year", effect = "individual")
summary(m_f_fruits)

m_f_other <- pglm(hf_h1_11 ~ GPI +  age  + as_factor(literacy) + district, family = "poisson", model = "within", data = f_other, index = "year", effect = "individual")
summary(m_f_other)

# Cluster the different trait types to reduce the categories from 15 to fewer, following crop ontology classification

# [1] Agronomic: 1
# [2] Abiotic stress: 4,5,6,7
# [3] Biotic stress: 3
# [4] Morphological: 2,13
# [5] Quality: 8,9,10,11,12,14


# cereal
f_cereal <- f_cereal %>%
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

a <- f_cereal %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  )) 

# Utilizing the bife package
a_mod <- bife(trait_11_type1 ~ GPI + as_factor(a01) | year, a, "probit") # utilizing bife package
summary(a_mod)
a_stat <- get_APEs(a_mod)
summary(a_stat) # average marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(sex)| district + year, a, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

# fiber
f_fiber <- f_fiber %>%
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

b <- f_fiber %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 1,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 0
  )) 


# Utilizing the bife package
b_mod <- bife(trait_11_type1 ~ GPI +  sex + as_factor(district) | year, b, "probit") # utilizing bife package
summary(b_mod)
b_stat <- get_APEs(b_mod)
summary(b_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(sex)| district + year, b, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

# pulses
f_pulses <- f_pulses %>%
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

c <- f_pulses %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

# Utilizing the bife package
c_mod <- bife(trait_10_type1 ~ GPI +   as_factor(sex) + as_factor(district) | year, c, "probit") # utilizing bife package
summary(c_mod)
c_stat <- get_APEs(c_mod)
summary(c_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(sex)| district + year, c, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level


# oil
f_oil <- f_oil %>%
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

d <- f_oil %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

# Utilizing the bife package
d_mod <- bife(trait_10_type1 ~ GPI + as_factor(sex) + as_factor(district) | year, d, "probit") # utilizing bife package
summary(d_mod)
d_stat <- get_APEs(d_mod)
summary(d_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(sex)| district + year, d, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level


# spices
f_spices <- f_spices %>%
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

e <- f_spices %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

# Utilizing the bife package
e_mod <- bife(trait_10_type1 ~ GPI +   as_factor(sex) + as_factor(district) | year, e, "probit") # utilizing bife package
summary(e_mod)
e_stat <- get_APEs(e_mod)
summary(e_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~GPI*as_factor(sex)| district + year, e, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level


# vegetables
f_vegetables <- f_vegetables %>%
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

g <- f_vegetables %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

# Utilizing the bife package
g_mod <- bife(trait_11_type1 ~ GPI + as_factor(sex) +age + as_factor(district) | year, g, "probit") # utilizing bife package
summary(g_mod)
g_stat <- get_APEs(g_mod)
summary(g_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_11_type1 ~ GPI*as_factor(sex)| district + year, g, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

# fruits
f_fruits <- f_fruits %>%
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

i <- f_fruits %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  )) 

# Utilizing the bife package
i_mod <- bife(trait_10_type1 ~ GPI + as_factor(district) | year, i, "probit") # utilizing bife package
summary(i_mod)
i_stat <- get_APEs(i_mod)
summary(i_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_10_type1 ~ GPI*as_factor(sex)| district + year, i, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level

# other
f_other <- f_other %>%
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

l <- f_other %>%
  mutate(trait_10_type1 = case_when(
    trait_type_10 == "Agronomic" ~ 0,
    trait_type_10 == "Abiotic stress" ~ 0,
    trait_type_10 == "Biotic stress" ~ 0,
    trait_type_10 == "Morphological" ~ 1,
    trait_type_10 == "Quality" ~ 0
  )) 

# Utilizing the bife package
l_mod <- bife(trait_11_type1 ~ GPI + as_factor(a01) | year, l, "probit") # utilizing bife package
summary(l_mod)
l_stat <- get_APEs(l_mod)
summary(l_stat) # marginal effects

# Utilizing the alpaca package
stat <- feglm(trait_10_type1 ~ GPI | district + year, l, binomial("probit"))
stat.bc <- biasCorr(stat) # coefficients corrected for IPP by Neyman and Scott (1948)
summary(stat.bc)
apes.stat.bc <- getAPEs(stat.bc)
summary(apes.stat.bc, vcov=vcovCL(apes.stat.bc, factor(a$district))) # standard errors clusterd at the village level


##### Validation of some parts of the model in STATA ########
# Extract the datasets to validate the results in STATA w/ femlogit code (Pforr, 2014)
getwd()

write_dta(f_cereal, "cereal.dta")
write_dta(f_fiber, "fiber.dta")
write_dta(f_pulses, "pulses.dta")
write_dta(f_oil, "oil.dta")
write_dta(f_spices, "spices.dta")
write_dta(f_vegetables, "vegetables.dta")
write_dta(f_fruits, "fruits.dta")
write_dta(f_other, "other.dta")

# This is just a trial to see if the command https://www.stata.com/manuals/cmcmxtmixlogit.pdf work in STATA
f_cereal_new <- f_cereal

# firstly, we need to create a column "alt" listing for every a01 the trait alternatives (1:15)
# cereal
m <- f_cereal_new %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_cereal_new <- left_join(f_cereal, m, by = c("a01", "year")) # we merge it with the main dataset

f_cereal_new <- f_cereal_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

#fiber
m <- f_fiber %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_fiber_new <- left_join(f_fiber, m, by = c("a01", "year")) # we merge it with the main dataset

f_fiber_new <- f_fiber_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# pulses 
m <- f_pulses %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_pulses_new <- left_join(f_pulses, m, by = c("a01", "year")) # we merge it with the main dataset

f_pulses_new <- f_pulses_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# oil 
m <- f_oil %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_oil_new <- left_join(f_oil, m, by = c("a01", "year")) # we merge it with the main dataset

f_oil_new <- f_oil_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# spices
m <- f_spices %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_spices_new <- left_join(f_spices, m, by = c("a01", "year")) # we merge it with the main dataset

f_spices_new <- f_spices_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%

  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# vegetables
m <- f_vegetables %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_vegetables_new <- left_join(f_vegetables, m, by = c("a01", "year")) # we merge it with the main dataset

f_vegetables_new <- f_vegetables_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# fruits
m <- f_fruits %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_fruits_new <- left_join(f_fruits, m, by = c("a01", "year")) # we merge it with the main dataset

f_fruits_new <- f_fruits_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# other
m <- f_other %>%
  group_by(a01, year) %>%
  summarise(alt = 1:15) 

f_other_new <- left_join(f_other, m, by = c("a01", "year")) # we merge it with the main dataset

f_other_new <- f_other_new %>%
  group_by(a01, year) %>%
  mutate(choice_h1_10 = case_when(
    hf_h1_10 == alt ~ 1,
    hf_h1_10 != alt ~ 0
  )) %>%
  mutate(choice_h1_11 = case_when(
    hf_h1_11 == alt ~ 1,
    hf_h1_11 != alt ~ 0
  ))

# Extract the datasets to validate the results in STATA w/ the command https://www.stata.com/manuals/cmcmxtmixlogit.pdf
getwd()

write_dta(f_cereal_new, "cereal_new.dta")
write_dta(f_fiber_new, "fiber_new.dta")
write_dta(f_pulses_new, "pulses_new.dta")
write_dta(f_oil_new, "oil_new.dta")
write_dta(f_spices_new, "spices_new.dta")
write_dta(f_vegetables_new, "vegetables_new.dta")
write_dta(f_fruits_new, "fruits_new.dta")
write_dta(f_other_new, "other_new.dta")


