# Code 2.1: MLM Hybrid Models witn egp class

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, sjPlot, summarytools,
               effectsize, lme4, easystats, stargazer, 
               influence.ME, performance, ggrepel, ggpubr,
               broom, broomExtra, sjlabelled, RColorBrewer, 
               texreg, car, flexplot, ggeffects, misty, optimx)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load("output/db-proc.RData")

names(db)
sapply(db, class)

db <- db %>% 
  mutate(egp = factor(str_squish(as.character(egp))),
         egp = case_when(egp %in% c("I Service class I", 
                                    "II Service class II") ~ "Service class (I+II)",
                         egp %in% c("III.a Routine non-manual, higher grade",
                                    "III.b Routine non-manual, lower grade",
                                    "IV.a Self-employed with employees",
                                    "IV.b Self-employed with no empoyees",
                                    "IV.c Self-employed Farmers etc") ~ "Intermediate class (III+IV)",
                         egp %in% c("V Manual supervisors/Lower grade technicians",
                                    "VI Skilled workers",
                                    "VII.a Unskilled workers",
                                    "VII.b Farm labours") ~ "Working class (V+VI+VII)"),
         egp = factor(egp, levels = c("Working class (V+VI+VII)",
                                      "Intermediate class (III+IV)",
                                      "Service class (I+II)")))

# 3. Analysis -------------------------------------------------------------

# 3.1. Multilevel hybrid models ----

# Null model
model_0 <- lmer(psci ~ 1 + (1 | country_wave) + (1 | country), 
                data = db, weights = factor, REML = T)

performance::icc(model_0, by_group = T)
## ICC Country = 0.17
## ICC Country wave = 0.06

# Influence test
inf_m0 <- influence(model_0, group = "country")

# D cook
cooks.distance(inf_m0, parameters = 1, sort = T) # cut point is 4/33 = 0.1212121

plot(inf_m0, which="cook",
     cutoff=0.121, sort=TRUE,
     xlab="Distancia de Cook",
     ylab="País", width=60, height=40)

# Influential countries: South Korea and Hungary. 
# This are the two countries with the highest average PSCi in the sample.

# Model 1: Only Class
model_1 <- lmer(psci ~ 1 + egp + 
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 2: Class + Union
model_2 <- lmer(psci ~ 1 + egp + union +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 3: Class + Union + Individual controls
model_3 <- lmer(psci ~ 1 + egp + union + 
                  sex + (c_age)^2 + ideology +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 4: Class + Union + Individual controls + Top10(BE|WE) + CorpAll + WAVE
model_4 <- lmer(psci ~ 1 + egp + union + 
                  sex + (c_age)^2 + ideology + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 5: Class + Union + Individual controls + Top10(BE|WE) + CorpAll + WAVE + GDP (BE|WE)
model_5 <- lmer(psci ~ 1 + egp + union + 
                  sex + (c_age)^2 + ideology + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 6: Random slope class
model_6 <- lmer(psci ~ 1 + egp + union + 
                  sex + (c_age)^2 + ideology + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 + egp| country_wave) +
                  (1 + egp| country), data = db, weights = factor, REML = T)

# Model 7: Cross level interactions 
model_7 <- lmer(psci ~ 1 + egp + union + 
                  sex + (c_age)^2 + ideology + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  top10_be*egp + top10_we*egp +
                  (1 + egp| country_wave) +
                  (1 + egp| country), data = db, weights = factor, REML = T)

screenreg(model_7)
