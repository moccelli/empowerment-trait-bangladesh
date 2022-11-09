# WEAI Bangladesh 
# Occelli M. & Tufan H.
# Script for descriptive graphs, specifically on WEAI modules
# For inquiries on the code, contact Martina Occelli (mo386@cornell.edu)

# Set working directory
dir <- "/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/Input WEAI BIHS" # Change path according to your directory
setwd(dir)

# Load library
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(raster)
library(tmap) # necessary to have package terra to launch it
library(ncdf4)
library(waffle)
library(hrbrthemes)

# Load dataset (WEAI indicators)
weai_final_11 <- read_dta("WEAI_final_input_stata/weai_final_11.dta")
weai_final_15 <- read_dta("WEAI_final_input_stata/weai_final_15.dta")
weai_final_18 <- read_dta("WEAI_final_input_stata/weai_final_18.dta")

# Load dataset (5D & GPI)

gender_district_dis_11 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_gender_district_disaggregated_11.dta")
gender_district_dis_15 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_gender_district_disaggregated_15.dta")
gender_district_dis_18 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_gender_district_disaggregated_18.dta")

individual_5D_ind_11 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_individual_indices_gender_11.dta")
individual_5D_ind_15 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_individual_indices_gender_15.dta")
individual_5D_ind_18 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/5D_individual_indices_gender_18.dta")

GPI_district_dis_11 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_11.dta")
GPI_district_dis_15 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_15.dta")
GPI_district_dis_18 <- read_dta("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_18.dta")


# Load shapefiles for Bangladesh

banAdm2 <- readRDS("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/Bangladesh map shapefile/GADM/gadm36_BGD_2_sf(1).rds") #adm2 district
banAdm3 <- readRDS("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/Bangladesh map shapefile/GADM/gadm36_BGD_3_sf.rds") # adm3 upazilla
banAdm4 <- readRDS("/Volumes/GoogleDrive/My Drive/!!CORNELL/BT eggplant/R script and input data/Bangladesh map shapefile/GADM/gadm36_BGD_4_sf.rds") #adm4 union

# Check that the names coincide - we have 64 administrative districts in Bangladesh

table(banAdm2$NAME_2)
table(GPI_district_dis_11$district_name) 
table(GPI_district_dis_15$district_name)
table(GPI_district_dis_18$district) # for 2018 we have no district names but code, so we need to assemble them 

z <- GPI_district_dis_18 %>%
  left_join(GPI_district_dis_11, by = "hhid")
table(z$district_name)
z_2 <- z[,c(1, 99)]

GPI_district_dis_18 <- merge(GPI_district_dis_18, z_2, by = "hhid")
table(GPI_district_dis_18$district_name)

# GPI (Gender Parity Index)

# Plot average GPI per district in all the three rounds

GPI_agg_11 <- with(GPI_district_dis_11, aggregate(GPI, by = list(district_name),
                                                  FUN = "mean")) # create a new dataset which contains means of GPI by gender in each district
names(GPI_agg_11) <- c("district_name", "GPI_mean") # rename columns

GPI_agg_15 <- with(GPI_district_dis_15, aggregate(GPI, by = list(district_name),
                                                  FUN = "mean")) 
names(GPI_agg_15) <- c("district_name", "GPI_mean") 

GPI_agg_18 <- with(GPI_district_dis_18, aggregate(GPI, by = list(district_name),
                                                  FUN = "mean"))  # here we must update the code of the district with the name
names(GPI_agg_18) <- c("district_name", "GPI_mean") 

# Merge with map to create a map - data dataset

banAdm2_11 <- 
  banAdm2 %>% 
  left_join(GPI_agg_11, by=c("NAME_2"="district_name")) 

banAdm2_15 <- 
  banAdm2 %>% 
  left_join(GPI_agg_15, by=c("NAME_2"="district_name")) 

banAdm2_18 <- 
  banAdm2 %>% 
  left_join(GPI_agg_18, by=c("NAME_2"="district_name"))

# Plot GPI maps of Bangladesh for each year 

mapGPI2011 <- 
  tm_shape(banAdm2_11) + 
  tm_polygons(col = "GPI_mean", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Blues")[1:11]), 
              style = "fixed",
              breaks = c(.4, .6, .7, .75, .8, .85, .9, .95), 
              title = "GPI (mean) 2011") 
mapGPI2011

mapGPI2015 <- 
  tm_shape(banAdm2_15) + 
  tm_polygons(col = "GPI_mean", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Blues")[1:11]), 
              style = "fixed",
              breaks = c(.4, .6, .7, .75, .8, .85, .9, .95, 1), 
              title = "GPI (mean) 2015")
mapGPI2015

mapGPI2018 <- 
  tm_shape(banAdm2_18) + 
  tm_polygons(col = "GPI_mean", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Blues")[1:11]), 
              style = "fixed",
              breaks = c(.4, .6, .7, .75, .8, .85, .9, .95, 1), 
              title = "GPI (mean) 2018")
mapGPI2018

tmap_arrange(mapGPI2011, mapGPI2015, mapGPI2018, ncol = 2) # assemble the maps in one group

# 5D (5 domain of empowerment in agriculture)

# Not super clear which variable to use for synthesize 5D - so we are going to start by using Population (i.e., gender) Subgroup Contribution to DAI

gender_EAI_2011 <- data_frame(gender_district_dis_11$district, gender_district_dis_11$M0_20p, gender_district_dis_11$gender)
names(gender_EAI_2011) <- c("district", "M0_20p", "gender")
gender_EAI_2015 <- data.frame(gender_district_dis_15$M0_20p) # missing district - double check if the order has been kept
gender_EAI_2018 <- data.frame(gender_district_dis_18$M0_20p) # missing district - double check if the order has been kept

district_code <- data.frame(GPI_district_dis_11$district, GPI_district_dis_11$district_name)
names(district_code) <- c("district", "district_name")

y <- district_code %>%
  left_join(gender_EAI_2011, by = "district")

y_2 <- with(y, aggregate(M0_20p, by = list(district_name, gender),
                                                  FUN = "mean")) # compress into one value per district
names(y_2) <- c("district_name", "gender", "M0_20p")
gender_EAI_2011 <- y_2

gender_EAI_2015 <- data.frame(gender_EAI_2015$gender_district_dis_15.M0_20p, gender_EAI_2011$district_name, gender_EAI_2011$gender)
names(gender_EAI_2015) <- c("M0_20p", "district_name", "gender")

gender_EAI_2018 <- data.frame(gender_EAI_2018$gender_district_dis_18.M0_20p, gender_EAI_2011$district_name, gender_EAI_2011$gender)
names(gender_EAI_2018) <- c("M0_20p", "district_name", "gender")

# Merge with map to create a map - data dataset

banAdm2_11 <- 
  banAdm2 %>% 
  left_join(gender_EAI_2011, by=c("NAME_2"="district_name")) 

banAdm2_15 <- 
  banAdm2 %>% 
  left_join(gender_EAI_2015, by=c("NAME_2"="district_name")) 

banAdm2_18 <- 
  banAdm2 %>% 
  left_join(gender_EAI_2018, by=c("NAME_2"="district_name"))

# Plot GPI maps of Bangladesh for each year 

banAdm2_11$gender <- as_factor(banAdm2_11$gender)
levels(banAdm2_11$gender)
levels(banAdm2_11$gender) <- c("Men", "Women")

banAdm2_15$gender <- as_factor(banAdm2_15$gender)
levels(banAdm2_15$gender)
levels(banAdm2_15$gender) <- c("Men", "Women")

banAdm2_18$gender <- as_factor(banAdm2_18$gender)
levels(banAdm2_18$gender)
levels(banAdm2_18$gender) <- c("Men", "Women")

mapM02011 <- 
  tm_shape(banAdm2_11) + 
  tm_polygons(col = "M0_20p", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Reds")[1:11]), 
              style = "fixed",
              breaks = c(.05, .1, .2, .3, .4, .5, .6), 
              title = "Disemp. Index (mean) 2011") +
  tm_facets(by = "gender", showNA = F) +
  mutate(outline = case_when(
    district_name == "Rangpur", "Bogra", "Rajshahi", "Jessore","Tangail" ~ "yellow",
    TRUE ~ "white"),)

mapM02011

mapM02015 <- 
  tm_shape(banAdm2_15) + 
  tm_polygons(col = "M0_20p", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Reds")[1:11]), 
              style = "fixed",
              breaks = c(.05, .1, .2, .3, .4, .5, .6), 
              title = "Disemp. Index (mean) 2015")+
  tm_facets(by = "gender", showNA = F)

mapM02015

mapM02018 <- 
  tm_shape(banAdm2_18) + 
  tm_polygons(col = "M0_20p", border.alpha = 0.7, textNA = "No data available",  
              palette = rev(hcl.colors(11, palette = "Reds")[1:11]), 
              style = "fixed",
              breaks = c(.05, .1, .2, .3, .4, .5, .6), 
              title = "Disemp. Index  (mean) 2018") +
  tm_facets(by = "gender", showNA = F)

mapM02018

tmap_arrange(mapM02011, mapM02015, mapM02018, ncol = 2) # assemble the maps in one group


# Calculate the percentage of men and women in each region for each indicator
z <- weai_final_18 %>%
  left_join(weai_final_11, by = "hhid")
table(z$district_name)
z_2 <- z[,c(1, 48)]

weai_final_18_2 <- merge(weai_final_18, z_2, by = "hhid")
table(weai_final_18_2$district_name)

# Interviewed alone
alone_11 <- weai_final_11 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(int_alone==1)/n())

alone_18_2 <- weai_final_18_2 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(int_alone==1)/n())

# Feel input decision agriculture
feelinputdecagr_11 <- weai_final_11 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(feelinputdecagr==1, na.rm=T)/n())
feelinputdecagr_11$year <- 2011

feelinputdecagr_15 <- weai_final_15 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(feelinputdecagr==1, na.rm=T)/n())
feelinputdecagr_15$year <- 2015

feelinputdecagr_18_2 <- weai_final_18_2 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(feelinputdecagr==1, na.rm=T)/n())
feelinputdecagr_18_2$year <- 2018

feelinputdecagr_final <- rbind(feelinputdecagr_11, feelinputdecagr_15, feelinputdecagr_18_2)

# income decision
incdec_11 <- weai_final_11 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(incdec_count==1, na.rm=T)/n())
incdec_11$year <- 2011

incdec_15 <- weai_final_15 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(incdec_count==1, na.rm=T)/n())
incdec_15$year <- 2015

incdec_18_2 <- weai_final_18_2 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(incdec_count==1, na.rm=T)/n())
incdec_18_2$year <- 2018

incdec_final <- rbind(incdec_11, incdec_15, incdec_18_2)

# speak public
speak_11 <- weai_final_11 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(speakpublic_any==1)/n())
speak_11$year <- 2011

speak_15 <- weai_final_15 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(speakpublic_any==1)/n())
speak_15$year <- 2015

speak_18_2 <- weai_final_18_2 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(speakpublic_any==1)/n())
speak_18_2$year <- 2018

speak_final <- rbind(speak_11, speak_15, speak_18_2)

#leisure time
leisure_11 <- weai_final_11 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(leisuretime==1)/n())
leisure_11$year <- 2011

leisure_15 <- weai_final_15 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(leisuretime==1)/n())
leisure_15$year <- 2015

leisure_18_2 <- weai_final_18_2 %>%
  group_by(district_name, sex) %>%
  summarize(NumRes=n(),prop=sum(leisuretime==1)/n())
leisure_18_2$year <- 2018

leisure_final <- rbind(leisure_11, leisure_15, leisure_18_2)

# Boxplot representation

leisure_final %>%
  mutate(gender = factor(sex,
    levels = c(1,2),
    labels = c("Male", "Female")
  )) %>%
  filter(year != 2015) %>%
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_boxplot() +
  facet_wrap( ~ year) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") +
  xlab("") +
  ylab("Proportion of respondents")  -> f1
 
speak_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year != 2015) %>%
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_boxplot() +
  facet_wrap( ~ year) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") +
  xlab("") +
  ylab("")  -> f2

incdec_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year != 2015) %>%
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_boxplot() +
  facet_wrap( ~ year) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") +
  xlab("Gender") +
  ylab("Proportion of respondents")  -> f3

feelinputdecagr_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year != 2015) %>%
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_boxplot() +
  facet_wrap( ~ year) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") +
  xlab("Gender") +
  ylab("")  -> f4

cowplot::plot_grid(f1, f2, f3, f4, nrow = 2, labels = c('a','b','c', 'd')) -> final 
final

bt_district <- c("Rangpur", "Bogra", "Rajshahi", "Jessore", "Tangail")

incdec_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2011) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

incdec_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2018) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)



feelinputdecagr_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2011) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

feelinputdecagr_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2018) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

speak_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2011) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

speak_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2018) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

leisure_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2011) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)

leisure_final %>%
  mutate(gender = factor(sex,
                         levels = c(1,2),
                         labels = c("Male", "Female")
  )) %>%
  filter(year == 2018) %>%
  filter(district_name %in% bt_district) %>%
  ggplot(aes(x= "", y = prop, fill = gender)) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap( ~ district_name) +
  scale_fill_manual(values = c("Male" = "#67a9cf","Female" = "#f1a340")) +
  theme_classic() +
  theme(legend.position = "") -> bp

bp + coord_polar("y", start=0)
