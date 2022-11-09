# WEAI & BIHS Bangladesh
# Occelli M. & Tufan H.
# BIHS  & WEAI MODULES - descriptive analysis
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

# Set working directory
setwd("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)")

# Household location - we consider only the years 2015 and 2018, as in 2011 there was no module on varietal preferences
mod_a_15 <- read_dta("dataverse_2015/001_r2_mod_a_male.dta") # household, 2015
names(mod_a_15) <- tolower(names(mod_a_15))
mod_a_15$year <- 2015

mod_a_18 <- read_dta("dataverse_2018/BIHSRound3/Male/009_bihs_r3_male_mod_a.dta") # household, 2018
names(mod_a_18) <- tolower(names(mod_a_18))
mod_a_18$year <- 2018

# Agriculture (module H1) in 2015 and 2018

mod_h1_15 <- read_dta("dataverse_2015/015_r2_mod_h1_male.dta") # household,2015
table(mod_h1_15$h1_10) 
table(mod_h1_15$h1_11)

mod_h1_18 <- read_dta("dataverse_2018/BIHSRound3/Male/021_bihs_r3_male_mod_h1.dta") # household,2018
table(mod_h1_18$h1_10) 
table(mod_h1_18$h1_11)

h1_18 <- mod_h1_18[,c(1, 6:37, 39)]
h1_18_2 <- h1_18[,c(1:5,7,9:22, 24,26,28,30,32,34)]
h1_18_3 <- h1_18_2[,c(1:17, 19:26)]

h1_18 <- h1_18_3 %>% relocate(hh_type, .after = a01) %>% relocate(varity_name, .after = crop_b_h1)

h1_15 <- mod_h1_15
h1_18 <-  rename(h1_18, plotid = plotid_h1) 
h1_18 <-  rename(h1_18, crop_a = crop_a_h1) 
h1_18 <-  rename(h1_18, crop_b = crop_b_h1)

h1_15$year <- 2015
h1_18$year <- 2018

val_labels(h1_15) <- NULL # Pay attention to the different label of the variables in the two years! 
val_labels(h1_18) <- NULL

h1 <- rbind(h1_15, h1_18) # combine the two dataset for the years
h1 <- h1[order(h1$a01),] # order them for hhid

y <- left_join(h1, mod_a_15, by = "a01") # add here information on district names

h1 <- y[,c(1,6:7, 20:21,26,34)] # isolate only variables of interest
names(h1) <- c("a01", "crop_1", "crop_b", "h1_10", "h1_11", "year", "district_name") # rename columns

h1$crop_group <- NA

h1 <- mutate(h1,
                  crop_group = case_when(
                    crop_1 >= 10 & crop_1<=30 | crop_b >= 10 & crop_b<=30  ~ "cereal", 
                    crop_1 >= 41 & crop_1<= 45 | crop_b >= 41 & crop_b<= 45 ~ "fiber",
                    crop_1 >= 51 & crop_1<= 59 | crop_b >= 51 & crop_b<= 59 ~ "pulses",
                    crop_1 >= 61 & crop_1<= 67 | crop_b >= 61 & crop_b<= 67 ~ "oil",
                    crop_1 >= 71 & crop_1<= 77 | crop_b >= 71 & crop_b<= 77 ~ "spices", 
                    crop_1 >= 101 & crop_1<= 213 | crop_b >= 101 & crop_b<= 213 ~ "vegetables",
                    crop_1 >= 301 & crop_1<= 326 | crop_b >= 301 & crop_b<= 326 ~ "fruits",
                    crop_1 >= 411 & crop_1<= 710 | crop_b >= 411 & crop_b<= 710 ~ "other"
                  ))

p <- h1 %>%
  group_by(year) %>%
  count(crop_group) # we utilize this formula to calculate the % of plots allocated to each crop group

# Represent change in seed preferences across crop groups and years

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

h1 %>%
  drop_na(h1_10) %>% 
  mutate (h1_10 = factor (h1_10,
                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                  labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                             "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                             "market demand", "taste", "color", "animal feed", "other")
                  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=h1_10)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( ~ year, scales = "free") +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> first_trait

first_trait

h1 %>%
  drop_na(h1_11) %>% 
  mutate (h1_11 = factor (h1_11,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=h1_11)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( ~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> second_trait

second_trait

##### Matching preferences with gender data #####

mod_b1_2015 <- read_dta("dataverse_2015/003_r2_male_mod_b1.dta") # load file which contains gender info, 2015
mod_b1_2015 <- mod_b1_2015[, c(1,2, 4:6)]
names(mod_b1_2015) <- c("a01", "a10", "gender", "age", "relation") # rename columns

data_gender <- data_frame(mod_a_15$a01, mod_a_15$a10) # merge to attach each respondent with the proper gender
names(data_gender) <- c("a01", "a10") 
data_gender_15 <- merge(data_gender, mod_b1_2015, by = c("a01", "a10"))
table(data_gender_15$gender) # women are around 20% of the sample

h1_15 <- merge(data_gender_15, h1_15, by = "a01") #final dataset for 2015

mod_b1_2018 <- read_dta("dataverse_2018/BIHSRound3/Male/010_bihs_r3_male_mod_b1.dta") # load file which contains gender info, 2018
mod_b1_2018 <- mod_b1_2018[, c(1,6,8,9,15)]
names(mod_b1_2018) <- c("a01", "a10", "gender", "age", "relation") # rename columns

data_gender_2 <- data_frame(mod_a_18$a01, mod_a_18$a10) # merge to attach each respondent with the proper gender
names(data_gender_2) <- c("a01", "a10") 
data_gender_18 <- merge(data_gender_2, mod_b1_2018, by = c("a01", "a10"))
table(data_gender_18$gender) # women are around 20% of the sample

h1_18 <- merge(data_gender_18, h1_18, by = "a01") #final dataset for 2018

h1_g <- rbind(h1_15, h1_18) # combine the two dataset for the years + gender
h1_g <- h1_g[order(h1_g$a01),] # order them for hhid

h1_g <- h1_g[,c(1:5,10:11, 24:25, 30)] # isolate only variables of interest, no GPS specified this time
names(h1_g) <- c("a01", "a10", "gender", "age", "relation", "crop_1", "crop_b", "h1_10", "h1_11", "year") # rename columns

h1_g$crop_group <- NA

h1_g <- mutate(h1_g,
             crop_group = case_when(
               crop_1 >= 10 & crop_1<=30 | crop_b >= 10 & crop_b<=30  ~ "cereal", 
               crop_1 >= 41 & crop_1<= 45 | crop_b >= 41 & crop_b<= 45 ~ "fiber",
               crop_1 >= 51 & crop_1<= 59 | crop_b >= 51 & crop_b<= 59 ~ "pulses",
               crop_1 >= 61 & crop_1<= 67 | crop_b >= 61 & crop_b<= 67 ~ "oil",
               crop_1 >= 71 & crop_1<= 77 | crop_b >= 71 & crop_b<= 77 ~ "spices", 
               crop_1 >= 101 & crop_1<= 213 | crop_b >= 101 & crop_b<= 213 ~ "vegetables",
               crop_1 >= 301 & crop_1<= 326 | crop_b >= 301 & crop_b<= 326 ~ "fruits",
               crop_1 >= 411 & crop_1<= 710 | crop_b >= 411 & crop_b<= 710 ~ "other"
             ))

# represent again the data, this time by gender

h1_g %>%
  drop_na(h1_10) %>% 
  drop_na(gender) %>%
  mutate (h1_10 = factor (h1_10,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  mutate(gender = factor(gender,
                         levels = c("1", "2"),
                         labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=h1_10)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( gender ~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> first_trait_gender

h1_g %>%
  drop_na(h1_11) %>% 
  drop_na(gender) %>%
  mutate (h1_11 = factor (h1_11,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  mutate(gender = factor(gender,
                         levels = c("1", "2"),
                         labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=h1_11)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( gender ~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> second_trait_gender

second_trait_gender

# Number of households in each wave
round1 <- read_dta("dataverse_2011/Round1/001_mod_a_male.dta")
round2 <- read_dta("dataverse_2015/001_r2_mod_a_male.dta")
round3 <- read_dta("dataverse_2018/BIHSRound3/Male/009_bihs_r3_male_mod_a.dta")

data1 <- merge(round1, round2, by = "a01")
data2 <- merge(data1, round3, by = "a01")
table(data2$a01)

# Represent preference data by trait class
# I use the dataset F from the script 06-bihs_weai_econometric analysis
nb.cols <- 5
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

# Create the trait classes
f <- f %>%
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

level_order <- c("Cereal", "Fiber", "Pulses", "Oil", "Spices", "Vegetables", "Fruits", "Other")

f %>%
  drop_na(trait_type_10) %>% 
  drop_na(sex) %>%
  filter(crop_group != "fruits") %>%
  mutate (trait_type_10 = factor (trait_type_10,
                          levels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality"),
                          labels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality")
  )) %>%
  mutate(sex = factor(sex,
                         levels = c("1", "2"),
                         labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=trait_type_10)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( sex ~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> first_class_gender

first_class_gender

f %>%
  drop_na(trait_type_11) %>% 
  drop_na(sex) %>%
  filter(crop_group != "fruits") %>%
  mutate (trait_type_11 = factor (trait_type_11,
                                  levels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality"),
                                  labels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality")
  )) %>%
  mutate(sex = factor(sex,
                      levels = c("1", "2"),
                      labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=trait_type_11)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid( sex ~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> second_class_gender

second_class_gender

f %>%
  drop_na(trait_type_10) %>% 
  drop_na(sex) %>%
  #filter(crop_group != "fruits") %>%
  mutate (trait_type_10 = factor (trait_type_10,
                                  levels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality"),
                                  labels = c("Agronomic", "Abiotic stress", "Biotic stress", "Morphological", "Quality")
  )) %>%
  mutate(sex = factor(sex,
                      levels = c("1", "2"),
                      labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), fill=trait_type_10)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors) +
  facet_grid(~ year) +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers") -> overall_class_gender

overall_class_gender
