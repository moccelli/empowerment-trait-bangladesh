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


# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/WEAI Bangladesh (raw BIHS data)")

# Load necessary datasets
da <- read_dta("da.dta") # respondents' location
data_1 <- read_dta("data_1.dta") # respondents' demographics
crop_info_15 <- read_dta("crop_info_15.dta") # respondents' crop info 2015
crop_info_18 <- read_dta("crop_info_18.dta") # respondents' crop info 2018
merged <- read_dta("merged.dta") # respondents' crop and plot data, both years combined
final <- read_dta("final.dta") # respondents' final dataset, both years combined

# GPI
GPI_15 <- read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_15.dta")
GPI_18 <- read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_18.dta")
GPI_11 <- read_dta("~/Library/CloudStorage/GoogleDrive-martina.occelli.91@gmail.com/My Drive/!!CORNELL/BT eggplant/!BIHS - WEAI (1st study)/R script and input data/WEAI_5D_GPI/GPI_district_disaggregated_11.dta")

GPI_11 <- GPI_11[,c(1,2, 14, 75, 77)]
GPI_15 <- GPI_15[,c(1,3, 17, 76, 78)]
GPI_18 <- GPI_18[,c(1,2, 19, 76, 78)]

GPI_11 <- GPI_11 %>%
  rename(a01 = hhid)

GPI_15 <- GPI_15 %>%
  rename(a01 = hhid)

GPI_18 <- GPI_18 %>%
  rename(a01 = hhid)

GPI_total <- rbind(GPI_15, GPI_18, GPI_11) # create a unique GPI dataset
GPI_total <- GPI_total[order(GPI_total$a01),]

GPI_data <- GPI_total %>%
  filter(sex == 2) # we keep only one sex as the GPI is identical by construction for the two sexes

# Trait preferences for female headed households
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

# Summary statistics and t-test

table(r$Casedistrict)
table(r$Controldistrict)

library(tidyverse)
library(ggpubr)
library(rstatix)

r %>%
  filter(Casegroup == "cereal") %>%
  get_summary_stats(Case_hf_h1_11, type = "mean_sd")
r %>%
  filter(Casegroup == "cereal") %>%
  get_summary_stats(Control_hf_h1_11, type = "mean_sd")

j <- r %>%
  filter(Casegroup == "pulses")
res <- t.test(j$Case_hf_h1_10, j$Control_hf_h1_10)
res

# Create a dataset which is longitudinal

case_data <- r[, c(3,5,7,9,11, 13, 15, 17, 19, 21, 23, 25)]
control_data <- r[,c(4,6,8,10,12,14,16,18,20,22,24, 26)]

colnames(case_data)<-c("a01", "area_plot", "season_harvested", "crop_group", "age", "literary", "district", "year", "sex", "hf_h1_10", "hf_h1_11", "a10")
colnames(control_data)<-c("a01", "area_plot", "season_harvested", "crop_group", "age", "literary", "district", "year", "sex", "hf_h1_10", "hf_h1_11", "a10")

matched <- rbind(case_data, control_data)

## This dataset is a pooled cross section ##

# Descriptive statistics

nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

r %>%
  drop_na(Case_hf_h1_10) %>% 
  mutate (h1_10 = factor (Case_hf_h1_10,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  ggplot(aes(x=as_factor(Casegroup), fill=h1_10)) +
  geom_bar(position="fill", size=4) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = mycolors, drop = F) +
  facet_grid( ~ Caseyear, scales = "free") +
  theme_classic()+
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Portion of farmers")  # First preferred trait in each crop group (2015 – 2018)

matched %>%
  drop_na(hf_h1_10) %>% 
  drop_na(sex) %>%
  mutate (h1_10 = factor (hf_h1_10,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  mutate(gender = factor(sex,
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
  ylab("Portion of farmers") # First preferred trait in each crop group, for male and female ag. primary decision makers (2015 – 2018)

matched %>%
  drop_na(hf_h1_11) %>% 
  drop_na(sex) %>%
  mutate (h1_11 = factor (hf_h1_11,
                          levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"),
                          labels = c("yield", "size", "insect/disease resistant", "flood tolerant", "saline tolerant",
                                     "drought tolerant", "zinc enriched", "low labour", "low input", "ease of processing",
                                     "market demand", "taste", "color", "animal feed", "other")
  )) %>%
  mutate(gender = factor(sex,
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
  ylab("Portion of farmers") # Second preferred trait in each crop group, for male and female ag. primary decision makers (2015 – 2018)

# Introducing labor hours in each plot ( to isolate the role of women-managed plots)

crop_labor <- read_dta("crop_labor.dta") # dataset containing info on hours worked on each plot for each crop group

matched_time <- merge(matched, crop_labor, by=c("a01","crop_group", "year"))

# Descriptive statistics

matched_time %>%
  group_by(sex, year) %>%
  get_summary_stats(hired_female_hour, type = "mean_sd")

u <- matched_time %>%
  filter(sex == 1) %>%
  filter(year == 2018)

i <- matched_time %>%
  filter(sex == 2) %>%
  filter(year == 2018)

t.test(u$hired_male_hour, i$hired_male_hour)

j <- r %>%
  filter(Casegroup == "pulses")
res <- t.test(j$Case_hf_h1_10, j$Control_hf_h1_10)
res

matched_time %>%
  filter(crop_group == "cereal" | crop_group == "pulses" | crop_group == "vegetables") %>%
  drop_na(sum_male_hour) %>% 
  drop_na(sum_female_hour) %>% 
  drop_na(sex) %>%
  mutate(gender = factor(sex,
                         levels = c("1", "2"),
                         labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), y= sum_male_hour, fill=gender)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 200)) +
  facet_grid(gender ~ year) +
  theme_classic()+
  theme(legend.position="",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Hours worked by men")

matched_time %>%
  filter(crop_group == "cereal" | crop_group == "pulses" | crop_group == "vegetables") %>%
  drop_na(sum_male_hour) %>% 
  drop_na(sum_female_hour) %>% 
  drop_na(sex) %>%
  mutate(gender = factor(sex,
                         levels = c("1", "2"),
                         labels = c("Men", "Women")
  )) %>%
  ggplot(aes(x=as_factor(crop_group), y= sum_female_hour, fill=gender)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 200)) +
  facet_grid(gender ~ year) +
  theme_classic()+
  theme(legend.position="",
        legend.title = element_blank()) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=14),
        axis.text.y= element_text(size = 14)) +
  xlab("Crop group") +
  ylab("Hours worked by women")



# We merge the GPI data

final <- merge(matched_time, GPI_data, by = c("a01", "year"))

# Exploratory regressions with pooled cross sections
# I am interested to have one observation per household for each crop group

# Cereal

cer <- final%>%
  distinct(crop_group, a01, year, .keep_all = TRUE)

y <- cer %>%
  filter(crop_group == "cereal")

# Create dependent variable of interest
y <- y %>%
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


# Regression (change the trait_type_10 = 1 to replicate the result for the different trait class)

y <- y %>%
  mutate(trait_10_type1 = case_when(
    trait_type_10 == "Agronomic" ~ 0,
    trait_type_10 == "Abiotic stress" ~ 0,
    trait_type_10 == "Biotic stress" ~ 0,
    trait_type_10 == "Morphological" ~ 1,
    trait_type_10 == "Quality" ~ 0
  )) 

# Regression (change the trait_type_11 = 1 to replicate the result for the different trait class)

y <- y %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 0,
    trait_type_11 == "Quality" ~ 1
  )) 

y$dummy_2018 <- ifelse(y$year == 2018, 1, 0)
table(y$trait_11_type1)

reg1 <- glm(trait_10_type1 ~ dummy_2018 + GPI + GPI*dummy_2018, family = "poisson", data = y)
reg1 <- glm(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, family = binomial(link = "logit"), data = y)
reg1 <- lm(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, data = y)
summary(reg1)

coeftest(reg1,vcov=vcovHC(reg1,type="HC0",cluster="group")) # robust standard errors at the group level

# Marginal effects

logitmfx(formula =  trait_11_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, data = y, robust = T)
logitmfx(formula =  trait_11_type1 ~ GPI + GPI*dummy_2018 + dummy_2018 + sex.x + sex.x*dummy_2018 + 
           age + sum_female_hour + sum_female_hour*dummy_2018 + literary + season_harvested, data = y, robust = T)

# Pulses

pul <- final%>%
  distinct(crop_group, a01, year,  .keep_all = TRUE)

z <- pul %>%
  filter(crop_group == "pulses" | crop_group == "fiber")

z <- z %>%
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

z <- z %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

z$dummy_2018 <- ifelse(z$year == 2018, 1, 0)
table(z$trait_11_type1)

reg1 <- glm(trait_10_type1 ~ dummy_2018 + GPI + GPI*dummy_2018, family = "poisson", data = z)
reg1 <- glm(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, family = binomial(link = "logit"), data = y)
reg1 <- lm(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, data = y)
summary(reg1)

coeftest(reg1,vcov=vcovHC(reg1,type="HC0",cluster="group")) # robust standard errors at the group level

# Marginal effects

logitmfx(formula =  trait_11_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, data = z, robust = T)
logitmfx(formula =  trait_11_type1 ~ GPI + GPI*sex.x*dummy_2018 + dummy_2018 + sex.x + sex.x*dummy_2018 + 
           age + sum_female_hour + sum_female_hour*dummy_2018 + literary + season_harvested, data = z, robust = T)


# Bootstrapping the coefficients and ggplot representation

mfxboot <- function(modform,dist,data,boot=1000,digits=3){
  x <- glm(modform, family=binomial(link=dist),data)
  # get marginal effects
  pdf <- ifelse(dist=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  set.seed(1111)
  for(i in 1:boot){
    samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
    x1 <- glm(modform, family=binomial(link=dist),samp1)
    pdf1 <- ifelse(dist=="probit",
                   mean(dnorm(predict(x, type = "link"))),
                   mean(dlogis(predict(x, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
  } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
  }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
} #function for bootstrapping the coefficients

mfx1 <- mfxboot(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018 + sex.x + age + sum_female_hour + sum_female_hour*dummy_2018 + literary + season_harvested,"probit",y)
mfx2 <- mfxboot(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018 + sex.x + age + sum_female_hour + sum_female_hour*dummy_2018 + literary + season_harvested,"logit",y)
mfx3 <- mfxboot(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018 + sex.x + age + sum_female_hour + sum_female_hour*dummy_2018 + literary + season_harvested,"probit",y,boot=100,digits=4)
mfxdat <- data.frame(cbind(rownames(mfx1),mfx1))
mfxdat$me <- as.numeric(as.character(mfxdat$marginal.effect))
mfxdat$se <- as.numeric(as.character(mfxdat$standard.error))

ggplot(mfxdat, aes(V1, marginal.effect,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete('Variable') +
  scale_y_continuous('Marginal Effect',limits=c(-0.5,1)) +
  theme_bw() + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me)) +
  geom_hline(yintercept=0) + 
  coord_flip() # Marginal Effects with 95% Confidence Intervals


reg3a <- lm(trait_10_type1 ~ GPI + GPI*dummy_2018 + dummy_2018, data = y)
summary(reg3a)
coeftest(reg3a,vcov=vcovHC(reg3a,type="HC0",cluster="group")) # robust standard errors at the group level

reg3b <- lm(trait_10_type1 ~ GPI, data = y)
summary(reg3b)
coeftest(reg3b,vcov=vcovHC(reg3b,type="HC0",cluster="group")) # robust standard errors at the group level

waldtest(reg3a, reg3b) # to test if the effect changed over time - yes, it does!

# Fixed effects

fi <- plm(trait_10_type1 ~ GPI + sex.x + sum_female_hour, data = y,  model = "within", effect="individual")
summary(fi)

# let's see if we can have an event study approach
table(y$GPI)
mean(y$GPI, na.rm = T)


table(y$trait_type_11)
table(z$trait_type_10)

#### New econometric exercise after talking with CSCU - time does not matter ####

final_v2 <- final %>%
  filter(crop_group == "pulses" | crop_group == "vegetables")

pul <- final_v2%>%
  distinct(crop_group, a01, year,  .keep_all = TRUE)

final_v2 <- final_v2 %>%
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

final_v2 <- final_v2 %>%
  mutate(trait_11_type1 = case_when(
    trait_type_11 == "Agronomic" ~ 0,
    trait_type_11 == "Abiotic stress" ~ 0,
    trait_type_11 == "Biotic stress" ~ 0,
    trait_type_11 == "Morphological" ~ 1,
    trait_type_11 == "Quality" ~ 0
  )) 

logitmfx(formula = trait_11_type1 ~ GPI + GPI*sex.x + GPI*women_managed + year, data = final_v2, robust = T, clustervar1 = "district")
reg1 <- glm(trait_11_type1 ~ GPI + GPI*sex.x + GPI*sum_female_hour, family = binomial(link = "logit"), data = pul)
summary(reg1)
coeftest(reg1,vcov=vcovHC(reg1,type="HC0",cluster="group")) # robust standard errors at the group level


