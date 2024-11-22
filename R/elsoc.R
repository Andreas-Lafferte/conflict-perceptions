#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for research paper
# Author: Andreas Laffert            
# Overview: Preparation of the ELSOC DATA     
# Date: 21-10-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               sjPlot,
               here,
               naniar,
               car)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("input/ELSOC_Long.RData"))

db <- elsoc_long_2016_2022 %>% as_tibble()

names(db)
glimpse(db)

# 3. Processing -----------------------------------------------------------

db <- db %>% 
  select(1:30, c18_09, c18_10, d01_01, d01_02, d01_03, d02_01, d02_02, d02_03, -tipo_caso)

# Meritocracy
frq(db$c18_09)
frq(db$c18_10)

db <- db %>% 
  mutate(across(.cols = c(c18_09, c18_10), .fns = ~ sjlabelled::set_na(., na = c(-666,-777,-888,-999))))

# Subjective social mobility
frq(db$d01_01)
frq(db$d01_02)
frq(db$d01_03)

db <- db %>% 
  mutate(across(.cols = c(d01_01, d01_02, d01_03), .fns = ~ sjlabelled::set_na(., na = c(-666,-777,-888,-999))))

db$mobility_intra <- db$d01_01 - db$d01_02

frq(db$mobility_intra)

db$mobility_inter <- db$d01_01 - db$d01_03

frq(db$mobility_inter)

# Market justice preferences
frq(db$d02_01)
frq(db$d02_02)
frq(db$d02_03)

db <- db %>% 
  mutate(across(.cols = c(d02_01, d02_02, d02_03), .fns = ~ sjlabelled::set_na(., na = c(-666,-777,-888,-999))))

db <- cbind(db, "mjp_i" = rowMeans(db %>% select(d02_01, d02_02, d02_03), na.rm=TRUE))

sjmisc::descr(db$mjp_i)

# Wave

frq(db$ola)

df <- db %>% 
  select(idencuesta, ola, c18_09, c18_10, mobility_intra, mobility_inter, mjp_i) %>% as_tibble()

df %>% 
  group_by(ola) %>% 
  summarise(t = sum(is.na(mjp_i)))


df <- df %>% 
  filter(ola != 5)

df <- df %>% 
  na.omit()



library(plm)


plm(formula = mjp_i ~  mobility_intra + mobility_inter + c18_09 + c18_10, 
    data = df,
    model = "within") %>% summary()


lmer(formula = mjp_i ~ mobility_inter + mobility_intra + c18_09 + c18_10 +
       (1 | idencuesta),
     data = df) %>% 
  summary()



df %>% 
  filter(ola == 6) %>% 
  lm( mjp_i ~  mobility_intra + mobility_inter + c18_09 + c18_10, data = .) %>% summary()
