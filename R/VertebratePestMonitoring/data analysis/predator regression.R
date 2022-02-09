##binary detection

# libraries ---------------------------------------------------------------
library(tidyverse)
library(lme4)
library(boot)

# read in data ------------------------------------------------------------

setwd("C:/Users/mcsorleya/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/")
df <- read.csv("3.1 Detection tables/Long_allSites_allSpecies.csv" )[,-1]
StationData <- read.csv("3.2 Formated logs/start_end_dates.csv")

# filter data to only dog cats and foxes ----------------------------------
unique(df_merge$Species)
species_of_interest <- c('dog',
                         'Felis catus',
                         'fox')

unique(StationData$Subsite)
#Sites without dogs and foxes
sites_wo_dog <- c(
  "Badja",
  "Schlink Pass",
  "Barrington Swamp",
  "Kellets Creek",
  "Koukandowie",
  "Square Top",
  "Uringery",
  "Kellets Creek"
  )

#predator dataframe. Filter for species
pred_df <- df %>% filter(Species %in% species_of_interest)

# summarize data to time intervals? ----------------------------------------

# cast df into variables for species ---------------------------------------
pred_w_df <-  pred_df %>% pivot_wider(names_from = Species , values_from = detection)

#merge site data with detection, filtered out non dog sites
df_merge <- pred_w_df %>% left_join(. , StationData %>% select(stationID , SamplingMethod , Subsite)) %>% 
  filter(!Subsite %in% sites_wo_dog)

#rename felis catis to cat
colnames(df_merge)[3] <- "cat"

df_merge$dog <- factor(df_merge$dog)

#split data between trail and Targeted sites
targeted_df <- df_merge %>% filter(SamplingMethod == "Targeted")
trail_df <- df_merge %>% filter(SamplingMethod == "Trail")

# binomial glm -----------------------------------------------------------------
#basic model with not acounting for sites
Trail_model1 <- glm(fox~-1+dog, data = trail_df, family = "binomial")
sum_m1 <- summary(Trail_model1)
inv.logit(sum_m1$coefficients[1,1]);inv.logit(sum_m1$coefficients[1,1]+sum_m1$coefficients[2,1]) 

#model with subsite as random factor
Trail_model2 <- glmer(fox~dog + (1|Subsite), data = trail_df, family = "binomial")
sum_m2 <- summary(Trail_model2)
inv.logit(sum_m2$coefficients[,1]) 

#targeted sampling models
Targeted_model1 <- glm(fox~dog, data = targeted_df, family = "binomial")
sum_m1 <- summary(Targeted_model1)
inv.logit(sum_m1$coefficients[2,1]) 

#model with subsite as random factor
Targeted_model2 <- glmer(fox~dog + (1|Subsite), data = targeted_df, family = "binomial")
sum_m3 <- summary(Targeted_model2)
inv.logit(sum_m3$coefficients[,1]) 

#model show about twice as likly to detect a fox on the same night as a dog opposed to a night with no dog detected.
#this might mean that they're both active when the weather is good.

#model testing....

# chi suared tests --------------------------------------------------------------
#targets
targeted_chi <- chisq.test(table(targeted_df %>% select(fox,dog)))
targeted_chi
targeted_chi$expected
targeted_chi$observed

#trail
trail_chi <- chisq.test(table(trail_df %>% select(fox,dog)))
trail_chi
trail_chi$expected
trail_chi$observed

#same results from chi squared as GLMs

# results -----------------------------------------------------------------


# plot --------------------------------------------------------------------


