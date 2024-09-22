# Code 1: Data preparation

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               sjPlot,
               haven,
               stargazer,
               psych, 
               datawizard,
               here)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(file = here("input/ELSOC_Long.RData"))

db <- elsoc_long_2016_2022 %>% as_tibble()

names(db)
glimpse(db)
attach(db)

# 3. Processing -----------------------------------------------------------

# select ----

db <- db %>% 
  select(1:30, c18_09, c18_10, d02_01, d02_02, d02_03)

db[db ==-999] <- NA
db[db ==-888] <- NA
db[db ==-777] <- NA
db[db ==-666] <- NA



load(url("https://github.com/justicia-distributiva/merit-edjust/raw/main/input/data-proc/df_study_t6_ind_comunas.RData"))


df_study1 <- 
  df_study_long_t6_comunas %>%
  select(idencuesta,ola,comuna, ponderador_long_total,just_educ, 
         just_pension, just_salud, merit_effort, merit_talent) %>% 
  na.omit() %>% 
  mutate(ola = as.factor(ola),
         ola_num = as.numeric(ola),
         ola_2=as.numeric(ola)^2
  )


df_study1 <- df_study1 %>% 
  rowwise() %>% 
  mutate(market_justice = mean(just_salud, just_pension, just_educ)) %>% 
  ungroup()

colSums(is.na(df_study1))

library(plm)

plm(formula = market_justice ~ merit_effort + merit_talent, 
    data = df_study1,
    model = "within") %>% summary()

plm(formula = market_justice ~ merit_effort + merit_talent, 
    data = df_study1,
    model = "random") %>% summary()


# filter ----

db <- db %>% 
  filter(tipo_atricion == 1)

db %>% 
  filter(ola_w01 == 1 & ola_w02 == 1 & ola_w03 == 1 & ola_w04 == 1 & ola_w05 == 1 
         & ola_w06 == 1)


