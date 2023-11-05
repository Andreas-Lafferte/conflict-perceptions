# Code 2: MLM Hybrid Models

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
model_1 <- lmer(psci ~ 1 + class + 
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 2: Class + Union
model_2 <- lmer(psci ~ 1 + class + union +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 3: Class + Union + Top10(BE|WE) + CorpAll + WAVE
model_3 <- lmer(psci ~ 1 + class + union + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 4: Class + Union + Top10(BE|WE) + CorpAll + WAVE + Controls N1 & N2
model_4 <- lmer(psci ~ 1 + class + union + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  gdp_be + gdp_we +
                  #mean_socexpend + 
                  (1 | country_wave) +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 5: Random slope class
model_5 <- lmer(psci ~ 1 + class + union + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  gdp_be + gdp_we +
                  mean_socexpend + 
                  (1 + class| country_wave) +
                  (1 + class| country), data = db, weights = factor, REML = T)

# Model 6: Cross level interactions 
model_6 <- lmer(psci ~ 1 + class + union + 
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  gdp_be + gdp_we +
                  #mean_socexpend + 
                  top10_be*class + top10_we*class +
                  (1 + class| country_wave) +
                  (1 + class| country), data = db, weights = factor, REML = T)




model_r <- lmer(psci ~ 1 + class + union + 
                  datawizard::centre(top10) +
                  datawizard::centre(corp_all) +
                  wave + (c_age)^2 + sex + ideology +
                  datawizard::centre(soc_expend) +
                  datawizard::centre(log(gdp)) + 
                  (1 | country), data = db, weights = factor, REML = T)



model_cl <- lmer(psci ~ 1 + class + union + 
                  datawizard::centre(top10) +
                  datawizard::centre(corp_all) +
                  wave + (c_age)^2 + sex + ideology +
                  datawizard::centre(soc_expend) +
                  datawizard::centre(log(gdp)) + 
                   datawizard::centre(top10)*class +
                  (1 + class | country), data = db, weights = factor, REML = T)
