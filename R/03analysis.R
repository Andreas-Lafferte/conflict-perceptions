# Code 2: MLM Hybrid Models

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, sjPlot, summarytools,
               effectsize, lme4, easystats, stargazer, 
               influence.ME, performance, ggrepel, ggpubr,
               broom, broom.mixed, sjlabelled, RColorBrewer, 
               texreg, car, ggeffects, misty, optimx, marginaleffects)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load("output/db-proc.RData")

names(db)
glimpse(db)


#db <- db %>% 
#  mutate(class = case_when(
#    class %in% c("1.Capitalistas", 
#                 "2.Pequeños empleadores") ~ "I. Clases capitalistas",
#    class %in% c("3.Pequeña burguesia") ~ "II. Pequeña burguesia",
#    class %in% c("4.Expertos directivos", 
#                 "5.Expertos sin autoridad", 
#                 "6.Supervisores calificados") ~ "III. Clases intermedias",
#    class %in% c("7.Supervisores no calificados", 
#                 "8.Trabajadores calificados", 
#                 "9.Trabajadores no calificados") ~ "IV. Clases trabajadoras"))
#

# 3. Analysis -------------------------------------------------------------

# 3.1. Multilevel hybrid models ----

# Null model
model_0 <- lmer(psci ~ 1 + (1 | country_wave) + (1 | country), 
                data = db, REML = T)

performance::icc(model_0, by_group = T)
## ICC Country = 0.16
## ICC Country wave = 0.06

screenreg(model_0)

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

sjPlot::plot_model(model_0, 
                   type = "re", 
                   vline.color = "green",
                   grid = F, 
                   sort.est = "sort.all", 
                   ci.lvl = .95, 
                   colors = "#800080")


# Model 1: Only Class
model_1 <- lmer(psci ~ 1 + class + 
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 2: Class + N1 controls
model_2 <- lmer(psci ~ 1 + class + union + sex + (age)^2 +
                  degree + ideology +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 3: Class + N1 controls + Top10(BE|WE) + CorpAll + WAVE
model_3 <- lmer(psci ~ 1 + class +  union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 4: Class + Top10(BE|WE) + CorpAll + WAVE + Controls N1 & N2
model_4 <- lmer(psci ~ 1 + class + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 5: Random slope class
model_5 <- lmer(psci ~ 1 + class + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 + class| country_wave) +
                  (1 + class| country), data = db, REML = T)

# Model 6: Cross level interactions 
model_6 <- lmer(psci ~ 1 + class + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  top10_be*class + top10_we*class +
                  (1 + class| country_wave) +
                  (1 + class| country), data = db, REML = T)




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
