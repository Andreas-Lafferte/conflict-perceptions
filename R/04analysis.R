# Code 2.1: MLM Hybrid Models witn egp class

# 1. Packages -------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               sjmisc, 
               sjPlot, 
               lme4, 
               easystats, 
               influence.ME, 
               broom.mixed, 
               here,
               texreg, 
               ggeffects,
               marginaleffects,
               naniar,
               ggdist,
               Polychrome,
               misty,
               kableExtra,
               sjlabelled)

options(scipen=999)
rm(list = ls())

theme_set(theme_ggdist())
# 2. Data -----------------------------------------------------------------

load("output/db-proc.RData")

names(db)
sapply(db, class)

sjmisc::descr(db$psci)

db <- db %>% 
  mutate(egp = factor(str_squish(as.character(egp))),
         egp = case_when(egp %in% c("I Service class I", 
                                    "II Service class II") ~ "Clase servicios (I+II)",
                         egp %in% c("III.a Routine non-manual, higher grade",
                                    "III.b Routine non-manual, lower grade",
                                    "IV.a Self-employed with employees",
                                    "IV.b Self-employed with no empoyees",
                                    "IV.c Self-employed Farmers etc") ~ "Clase intermedia (III+IV)",
                         egp %in% c("V Manual supervisors/Lower grade technicians",
                                    "VI Skilled workers",
                                    "VII.a Unskilled workers",
                                    "VII.b Farm labours") ~ "Clase trabajadora (V+VI+VII)"),
         egp = factor(egp, levels = c("Clase servicios (I+II)",
                                      "Clase intermedia (III+IV)",
                                      "Clase trabajadora (V+VI+VII)")))


sum(is.na(db))
colSums(is.na(db))

db <- db %>% 
  select(1:11, egp, psci, top10_be, top10_we, mean_corp_all,
         gdp_be, gdp_we, starts_with("conflict")) %>% 
  na.omit()

# 3. Analysis -------------------------------------------------------------

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

# 3.1. Multilevel hybrid models ----

# Null model
model_0 <- lmer(psci ~ 1 + (1 | country_wave) + (1 | country), 
                data = db, REML = T)

performance::icc(model_0, by_group = T)
## ICC Country = 0.16
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

sjPlot::plot_model(model_0, 
                   type = "re", 
                   vline.color = "green",
                   grid = F, 
                   sort.est = "sort.all", 
                   ci.lvl = .95, 
                   colors = "#800080")


# Model 1: Only Class
model_1 <- lmer(psci ~ 1 + egp + 
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 2: Class + N1 controls
model_2 <- lmer(psci ~ 1 + egp + union + sex + (age)^2 +
                  degree + ideology +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 3: Class + N1 controls + Top10(BE|WE) + CorpAll + WAVE
model_3 <- lmer(psci ~ 1 + egp +  union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 4: Class + Top10(BE|WE) + CorpAll + WAVE + Controls N1 & N2
model_4 <- lmer(psci ~ 1 + egp + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 | country_wave) +
                  (1 | country), data = db, REML = T)

# Model 5: Random slope class
model_5 <- lmer(psci ~ 1 + egp + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  (1 + egp| country_wave) +
                  (1 + egp| country), data = db, REML = T)

anova(model_4, model_5)

# Model 6: Cross level interactions 
model_6 <- lmer(psci ~ 1 + egp + union + sex + (age)^2 +
                  degree + ideology +
                  top10_be + top10_we +
                  mean_corp_all + wave +
                  gdp_be + gdp_we +
                  top10_be*egp + top10_we*egp +
                  (1 + egp| country_wave) +
                  (1 + egp| country), data = db, REML = T)

res_fit1 <- anova(model_4, model_5)

save(
  model_0, model_1, model_2, model_3, model_4, model_5, model_6, res_fit1,
  file = here("output/hybrid_models.RData")
)

