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
library(Matching)
library(sandwich)
library(lmtest)
library(mfx)
library(erer)
library(miceadds)
library(ivtools)
library(ivreg)
library(AER)
library(plm)

# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)")

# Load necessary datasets
da <- read_dta("da.dta") # respondents' location
data_1 <- read_dta("data_1.dta") # respondents' demographics
crop_info_15 <- read_dta("crop_info_15.dta") # respondents' crop info 2015
crop_info_18 <- read_dta("crop_info_18.dta") # respondents' crop info 2018
merged <- read_dta("merged.dta") # respondents' crop and plot data, both years combined
final <- read_dta("final.dta") # respondents' final dataset, both years combined

# Include village information for clustering

da <- da[, c(1,4)]
merged <- merge(merged, da, by = "a01")

table(merged$sex, merged$year)

female_hh_data <- subset(merged, sex == 2)

# Multivariate matching technique 
merged$Tr <- ifelse(merged$sex == 2, 1, 0)

X <- cbind(merged$area_planted, merged$harvest_season, as.factor(merged$crop_group), merged$age, merged$literacy, merged$dis_code, merged$year)
colnames(X)<-c("area","season", "crop group", "age", "literacy", "district", "year")
Tr <- merged$Tr

5/sd(merged$age) #to allow a margin of error in the measurement of age, while all other variables must be equal

rr <- Match(Tr=Tr, X=X, ties = F, caliper = 0.4035)
summary(rr)
r<-data.frame(rr$index.treated,rr$index.control)
colnames(r)<-c("cases","controls")
merged$rowID<-as.numeric(rownames(merged))

# Assemble the matching dataset - r

r$Casea01 <- merged$a01[match(r$cases, merged$rowID)]
r$Controla01 <- merged$a01[match(r$controls, merged$rowID)]

r$Casearea <- merged$area_planted[match(r$cases, merged$rowID)]
r$Controlarea <- merged$area_planted[match(r$controls, merged$rowID)]

r$Caseseason <- merged$harvest_season[match(r$cases, merged$rowID)]
r$Controlseason <- merged$harvest_season[match(r$controls, merged$rowID)]

r$Casegroup <- merged$crop_group[match(r$cases, merged$rowID)]
r$Controlgroup <- merged$crop_group[match(r$controls, merged$rowID)]

r$CaseAge <- merged$age[match(r$cases, merged$rowID)]
r$ControlAge<-merged$age[match(r$controls, merged$rowID)]

r$CaseLiteracy <- merged$literacy[match(r$cases, merged$rowID)]
r$ControlLiteracy <-merged$literacy[match(r$controls, merged$rowID)]

r$Casedistrict <- merged$dis_code[match(r$cases, merged$rowID)]
r$Controldistrict<-merged$dis_code[match(r$controls, merged$rowID)]

r$Caseyear <- merged$year[match(r$cases, merged$rowID)]
r$Controlyear<-merged$year[match(r$controls, merged$rowID)]

r$Casesex <- merged$sex[match(r$cases, merged$rowID)]
r$Controlsex <- merged$sex[match(r$controls, merged$rowID)]

r$Case_hf_h1_10 <- merged$hf_h1_10[match(r$cases, merged$rowID)]
r$Control_hf_h1_10 <- merged$hf_h1_10[match(r$controls, merged$rowID)]

r$Case_hf_h1_11 <- merged$hf_h1_11[match(r$cases, merged$rowID)]
r$Control_hf_h1_11 <- merged$hf_h1_11[match(r$controls, merged$rowID)]

r$Casea10 <- merged$a10[match(r$cases, merged$rowID)]
r$Controla10 <- merged$a10[match(r$controls, merged$rowID)]

r$Casevillage <- merged$village_code[match(r$cases, merged$rowID)]
r$Controlvillage<-merged$village_code[match(r$controls, merged$rowID)]

# Create a dataset which is longitudinal

case_data <- r[, c(3,5,7,9,11, 13, 15, 17, 19, 21, 23, 25, 27)]
control_data <- r[,c(4,6,8,10,12,14,16,18,20,22,24, 26, 28)]

colnames(case_data)<-c("a01", "area_plot", "season_harvested", "crop_group", "age", "literary", "district", "year", "sex", "hf_h1_10", "hf_h1_11", "a10", "village")
colnames(control_data)<-c("a01", "area_plot", "season_harvested", "crop_group", "age", "literary", "district", "year", "sex", "hf_h1_10", "hf_h1_11", "a10", "village")

matched <- rbind(case_data, control_data)
table(matched$sex)

# Introducing labor hours in each plot ( to isolate the role of women-managed plots)

crop_labor <- read_dta("crop_labor.dta") # dataset containing info on hours worked on each plot for each crop group

matched_time <- merge(matched, crop_labor, by=c("a01","crop_group", "year"), all.x = T)
table(matched_time$sex)

hour <- matched_time %>% 
  group_by(a01, crop_group, year) %>% 
  summarise(across(sum_female_hour, mean, na.rm = TRUE))

matched_time <- merge(matched_time, hour, by=c("a01","crop_group", "year"), all.x = T)

names(matched_time)[names(matched_time) == 'sum_female_hour.y'] <- 'average_fem_hours'
names(matched_time)[names(matched_time) == 'sum_female_hour.x'] <- 'sum_female_hour'

final <- merge(matched_time, GPI_data, by = c("a01", "year"), all.x = T)
table(final$sex.x)

# Final cleaning of the dataset

final <- final[, c(1:13, 18,19,21,22)]

final <- final%>%
  distinct(crop_group, a01, year, .keep_all = TRUE)
table(final$sex.x)

names(final)[names(final) == "season_harvested"] <- "crop_season"
names(final)[names(final) == 'sex.x'] <- "sex"
names(final)[names(final) == "hf_h1_10"] <- "best_trait"
names(final)[names(final) == "hf_h1_11"] <- "second_best_trait"


y <- final %>%
  filter(crop_group == "cereal" | crop_group == "pulses" | crop_group == "vegetables") 

t <- merge(y, GPI_11, by = c("a01"), all.x = T)
t <- t %>%
  distinct(crop_group, a01, year.x, .keep_all = TRUE)

final_data <- t[,c(1:17, 20, 21)]

names(final_data)[names(final_data) == "year.x"] <- "year"
names(final_data)[names(final_data) == 'sex.x'] <- "sex"
names(final_data)[names(final_data) == "H_GPI.x"] <- "H_GPI"
names(final_data)[names(final_data) == "GPI.x"] <- "GPI"
names(final_data)[names(final_data) == "GPI.y"] <- "GPI_2011"
names(final_data)[names(final_data) == "H_GPI.y"] <- "H_GPI_2011"

write_dta(final_data, "final_data.dta")
final_data <- read_dta("final_data.dta")

# Logistic model

y <- y %>%
  mutate( trait_type_10 = case_when(
    best_trait == 1 ~ "Agronomic",
    best_trait >= 4 &  best_trait <= 7 ~ "Abiotic stress",
    best_trait == 3 ~ "Biotic stress",
    best_trait == 2 |   best_trait == 13  ~ "Morphological",
    best_trait == 14 |  best_trait >= 8 &  best_trait <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    second_best_trait == 1 ~ "Agronomic",
    second_best_trait >= 4 & second_best_trait <= 7 ~ "Abiotic stress",
    second_best_trait == 3 ~ "Biotic stress",
    second_best_trait == 2 |  second_best_trait == 13  ~ "Morphological",
    second_best_trait == 14 | second_best_trait >= 8 & second_best_trait <= 12 ~ "Quality"
  )) 


# Regression (change the trait_type_10 = 1 to replicate the result for the different trait class)

y <- y %>%
  mutate(trait_10_type1 = case_when(
    trait_type_10 == "Agronomic" ~ 0,
    trait_type_10 == "Abiotic stress" ~ 0,
    trait_type_10 == "Biotic stress" ~ 0,
    trait_type_10 == "Morphological" ~ 1,
    trait_type_10 == "Quality" ~ 0
  )) 

table(y$vegetables_grown)

# Create a dummy for each crop group

y$cereal_grown <- ifelse(y$crop_group == "cereal", 1, 0)
y$pulses_grown <- ifelse(y$crop_group == "pulses", 1, 0)
y$vegetables_grown <- ifelse(y$crop_group == "vegetables", 1, 0)

final_data$cereal_grown <- ifelse(final_data$crop_group == "cereal", 1, 0)
final_data$pulses_grown <- ifelse(final_data$crop_group == "pulses", 1, 0)
final_data$vegetables_grown <- ifelse(final_data$crop_group == "vegetables", 1, 0)

# First model 

final_data$best_trait <- as_factor(final_data$best_trait)
final_data$best_trait <- relevel(final_data$best_trait, ref = 1)

min(final_data$best_trait)

require(foreign)
require(nnet)
test <- multinom(best_trait ~ GPI, data = final_data)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

mod1 <- glm.cluster(data = y, formula = trait_10_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
                      GPI*vegetables_grown + year, 
                              family = "binomial", cluster="village")
mod1 <- glm(data = y, formula = trait_10_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
                      GPI*vegetables_grown + year, 
                    family = "binomial")
summary(mod1)
logitmfx(formula = trait_10_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
           GPI*vegetables_grown + year, data = y, robust = T, clustervar1 = "village")


# Second model 

y <- y %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  )) 

mod2 <- glm.cluster(data = y, formula = trait_10_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
                      GPI*vegetables_grown + year, 
                    family = "binomial", cluster="village")
mod2 <- glm(data = y, formula = trait_11_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
              GPI*vegetables_grown + year, 
            family = "binomial")
summary(mod2)
logitmfx(formula = trait_11_type1 ~ GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
           GPI*vegetables_grown + year, data = y, robust = T, clustervar1 = "village")

# Instrumental variable model (with glm binomial model)

final_data$cereal_grown <- ifelse(final_data$crop_group == "cereal", 1, 0)
final_data$pulses_grown <- ifelse(final_data$crop_group == "pulses", 1, 0)
final_data$vegetables_grown <- ifelse(final_data$crop_group == "vegetables", 1, 0)

final_data <- final_data %>%
  mutate( trait_type_10 = case_when(
    best_trait == 1 ~ "Agronomic",
    best_trait >= 4 &  best_trait <= 7 ~ "Abiotic stress",
    best_trait == 3 ~ "Biotic stress",
    best_trait == 2 |   best_trait == 13  ~ "Morphological",
    best_trait == 14 |  best_trait >= 8 &  best_trait <= 12 ~ "Quality"
  )) %>%
  mutate( trait_type_11 = case_when(
    second_best_trait == 1 ~ "Agronomic",
    second_best_trait >= 4 & second_best_trait <= 7 ~ "Abiotic stress",
    second_best_trait == 3 ~ "Biotic stress",
    second_best_trait == 2 |  second_best_trait == 13  ~ "Morphological",
    second_best_trait == 14 | second_best_trait >= 8 & second_best_trait <= 12 ~ "Quality"
  )) 

final_data <- final_data %>%
  mutate(trait_10_type1 = case_when(
    trait_type_10 == "Agronomic" ~ 0,
    trait_type_10 == "Abiotic stress" ~ 0,
    trait_type_10 == "Biotic stress" ~ 0,
    trait_type_10 == "Morphological" ~ 1,
    trait_type_10 == "Quality" ~ 0
  )) 

final_data <- final_data %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  )) 


fitX.LZ <- lm(formula = GPI ~ GPI_2011, data=final_data)
summary(fitX.LZ)

fitY.LX <- glm(formula=trait_10_type1 ~ GPI + GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown +
                 year, family="binomial", data=final_data)
summary(fitY.LX)

i_glm <- ivglm(estmethod="ts", fitX.LZ=fitX.LZ, fitY.LX=fitY.LX, data=final_data, ctrl=TRUE) 
summary(i_glm)

iv_1 <- ivreg(trait_10_type1 ~ GPI + sex + average_fem_hours + cereal_grown + pulses_grown | 
                GPI_2011 + sex + average_fem_hours + cereal_grown + pulses_grown, 
              data = final_data)
summary(iv_1)
summary(iv_1, diagnostics = TRUE, cluster = final_data$village)

iv_2 <- ivreg(trait_11_type1 ~ GPI + sex + average_fem_hours + cereal_grown + pulses_grown + GPI*sex + GPI*average_fem_hours + GPI*cereal_grown + GPI*pulses_grown | 
                GPI_2011 + GPI_2011*sex + GPI_2011*average_fem_hours + GPI_2011*cereal_grown + GPI_2011*pulses_grown, 
              data = final_data)
summary(iv_2)
summary(iv_2, diagnostics = TRUE, cluster = final_data$village)

iv_3 <- ivreg(trait_11_type1 ~ GPI + sex + average_fem_hours + cereal_grown + pulses_grown  | 
                .-GPI + GPI_2011, 
              data = final_data)
summary(iv_3)
summary(iv_3, diagnostics = TRUE, cluster = final_data$village)
