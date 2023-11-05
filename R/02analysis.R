# Code 2: MLM  Models

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, sjPlot, summarytools,
               effectsize, lme4, easystats, stargazer, 
               influence.ME, performance, ggrepel, ggpubr,
               broom, sjlabelled, RColorBrewer, 
               texreg, car, ggeffects, misty, optimx)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load("output/db-proc.RData")

names(db)
sapply(db, class)

# 3. Analysis -------------------------------------------------------------

# 3.1. Multilevel  models ----

# Null model
model_0 <- lmer(psci ~ 1 + (1 | country), 
                data = db, weights = factor, REML = T)

performance::icc(model_0, by_group = T)
## ICC Country = 0.22

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
                  (1 | country), data = db, weights = factor, REML = T)

# Model 2: Class + Union
model_2 <- lmer(psci ~ 1 + class + union +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 3: Class + Union + Z_Top10 + Z_CorpAll + WAVE
model_3 <- lmer(psci ~ 1 + class + union + 
                  z_top10 + corp_all + wave +
                  (1 | country), data = db, weights = factor, REML = T)

# Model 4: Class + Union + Z_Top10 + Z_CorpAll + WAVE + Controls N1 & N2
model_4 <- lmer(psci ~ 1 + class + union + 
                  z_top10 + corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  z_soc_expend + 
                  (1 | country), data = db, weights = factor, REML = T)

# check_collinearity(model_4) %>% plot()

# Model 5: Random slope class
model_5 <- lmer(psci ~ 1 + class + union + 
                  z_top10 + corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  z_soc_expend + 
                  (1 + class| country), data = db, weights = factor, REML = T)

# Model 6: Cross level interactions 
model_6 <- lmer(psci ~ 1 + class + union + 
                  z_top10 + corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  z_soc_expend + 
                  z_top10*class +
                  (1 + class| country), data = db, weights = factor, REML = T)

model_6_c <- lmer(psci ~ 1 + class + union + 
                  c_top10 + corp_all + wave +
                  (c_age)^2 + sex + ideology +
                  c_soc_expend + 
                  c_top10*class +
                  (1 + class| country), data = db, weights = factor, REML = T)


# Save models

save(model_0, model_1, model_2, model_3, model_4, model_6, model_6_c, file = "output/all_models.RData")

stargazer(model_6, type = "text", star.cutoffs = c(0.05, 0.01, 0.001))

screenreg(l = list(model_6))
