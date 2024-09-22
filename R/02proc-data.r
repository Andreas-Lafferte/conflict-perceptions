# Code 1.2: Data preparation ISSP 2019

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               sjPlot,
               haven,
               psych, 
               summarytools,
               countrycode,
               datawizard,
               here, 
               janitor,
               occupar,
               SciViews)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

issp19 <- read_dta("input/data/ZA7600_v3-0-0.dta")
glimpse(issp19)
names(issp19)

# 3. Processing -----------------------------------------------------------

# ***********************
#
# Individual level data
#
# ***********************

issp19 <- issp19 %>% select(country,
                            v36,
                            v37,
                            v38,
                            v61,
                            SEX,
                            AGE,
                            DEGREE,
                            WORK,
                            EMPREL,
                            WRKSUP,
                            NSUP,
                            ISCO08,
                            UNION,
                            WEIGHT) %>%  # bg, sr y t1 son los unicos paises de los cuales se puede recodificar el prty, los demas no estan disponibles
  janitor::clean_names()


issp19 %>% group_by(country) %>% tally() %>% print(n = 40)

# 3.1 YEAR ----

issp19$year <- 2019
issp19$year <- as.numeric(issp19$year) 
issp19$year <- sjlabelled::set_label(issp19$year, label = c("Wave/Year"))

# 3.2 FACTOR ----

issp19 <- rename_variables(issp19, weight = "factor")
issp19$factor <- sjlabelled::set_label(issp19$factor, label = c("Weight"))

# 3.3 COUNTRY ----

frq(issp19$country)
issp19$country <- countrycode(issp19$country, 'iso3n', 'country.name')
issp19$country <- sjlabelled::set_label(issp19$country, label = c("Country"))


# 3.4 SEX ----
frq(issp19$sex)
issp19$sex <- as.numeric(issp19$sex)
issp19$sex <- car::recode(issp19$sex, recodes = c("-9 = NA; 1 = 'Male'; 2 = 'Female'"), as.factor = T)
issp19$sex <- sjlabelled::set_label(issp19$sex, label = c("Sex"))

# 3.5 AGE ----
frq(issp19$age)
issp19$age <- set_na(issp19$age, na = -9, drop.levels = T, as.tag = F)
issp19$age <- as.numeric(issp19$age)
issp19$age <- sjlabelled::set_label(issp19$age, label = c("Age"))

# 3.6 UNION ----
frq(issp19$union)
issp19$union <- as.numeric(issp19$union)
issp19$union <- car::recode(issp19$union, recodes = c("c(-9,-7,-4) = NA; 1 = 'Yes'; 2 = 'Yes'; 3 = 'No'"), as.factor = T)
issp19$union <- sjlabelled::set_label(issp19$union, label = c("Union membership"))


# 3.7 SUBJECTIVE SOCIAL CLASS ----
frq(issp19$v61)
issp19 <- issp19 %>% mutate(subjec_class = case_when(v61 == 1 ~ "6.Lower class",
                                                     v61 == 2 ~ "5.Working class",
                                                     v61 == 3 ~ "4.Lower middle class",
                                                     v61 == 4 ~ "3.Middle class",
                                                     v61 == 5 ~ "2.Upper middle class",
                                                     v61 == 6 ~ "1.Upper class",
                                                     TRUE ~ NA_character_))

issp19$subjec_class <- as.factor(issp19$subjec_class)
issp19$subjec_class <- sjlabelled::set_label(issp19$subjec_class, label = c("Class identity"))

# 3.9 EDUCATION ----
frq(issp19$degree) # 5 & 6 

# For control var
issp19 <- issp19 %>% mutate(degree_1 = case_when(degree == 0 ~ "1.Incomplete primary school or less",
                                                 degree == 1 ~ "2.Complete primary school",
                                                 degree == 2 ~ "3.Incomplete secondary school",
                                                 degree == 3 ~ "4.Complete secondary school",
                                                 degree == 4 ~ "5.Incomplete university",
                                                 degree %in% c(5,6) ~ "6.Complete university",
                                                 TRUE ~ NA_character_))

issp19$degree_1 <- as.factor(issp19$degree_1)
issp19$degree_1 <- sjlabelled::set_label(issp19$degree_1, label = c("Educational level"))

# For control skills in class var
issp19 <- issp19 %>% mutate(educ = case_when(degree %in% c(0:4) ~ 'No',
                                             degree %in% c(5:6) ~ 'Yes',
                                             TRUE ~ NA_character_))
issp19$educ <- as.factor(issp19$educ)
issp19$educ <- sjlabelled::set_label(issp19$educ, label = c("Completed tertiary level of education"))
table(issp19$degree_1, useNA = "ifany")
table(issp19$educ, useNA = "ifany")

# 3.10 CLASS ESCHEME EGP ----
frq(issp19$emprel)
frq(issp19$isco08)

issp19 <- issp19 %>% 
  mutate(isco08_or = occupar::isco08to88(isco08),
         selfemp_egp = if_else(emprel %in% c(2:5), 1, 0),
         nemploy_egp = case_when(emprel %in% c(1,2) ~ 0,
                                 emprel == 3 ~ 9,
                                 emprel == 4  ~ 11,
                                 emprel == 5 ~ 1,
                                 TRUE ~ NA_real_))

issp19 <- issp19 %>% 
  mutate(egp = occupar::isco88toEGP(isco88 = isco08_or, 
                                    n.employees = nemploy_egp, 
                                    self.employed = selfemp_egp,
                                    n.classes = 11))

issp19 <- issp19 %>% 
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
         egp = factor(egp, levels = c("Service class (I+II)",
                                      "Intermediate class (III+IV)",
                                      "Working class (V+VI+VII)")))

# 3.11 PERCEIVED SOCIAL CONFLICT INDEX ----

## Rich and poor
frq(issp19$v36)
issp19 <- issp19 %>% mutate(conflict_rp = case_when(v36 == 1 ~ 3,
                                                    v36 == 2 ~ 2,
                                                    v36 == 3 ~ 1,
                                                    v36 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp19$conflict_rp)
sjPlot::plot_frq(na.omit(issp19$conflict_rp), type = "histogram", show.mean = TRUE)
issp19$conflict_rp <- sjlabelled::set_label(issp19$conflict_rp, label = c("Conflicts: rich people - poor people"))

## Working class and middle class
frq(issp19$v37)
issp19 <- issp19 %>% mutate(conflict_wcmc = case_when(v37 == 1 ~ 3,
                                                      v37 == 2 ~ 2,
                                                      v37 == 3 ~ 1,
                                                      v37 == 4 ~ 0,
                                                      TRUE ~ NA_real_))
frq(issp19$conflict_wcmc)
sjPlot::plot_frq(na.omit(issp19$conflict_wcmc), type = "histogram", show.mean = TRUE)
issp19$conflict_wcmc <- sjlabelled::set_label(issp19$conflict_wcmc, label = c("Conflicts: working class - middle class"))

## Management and workers 
frq(issp19$v38)
issp19 <- issp19 %>% mutate(conflict_wm = case_when(v38 == 1 ~ 3,
                                                    v38 == 2 ~ 2,
                                                    v38 == 3 ~ 1,
                                                    v38 == 4 ~ 0,
                                                    TRUE ~ NA_real_))
frq(issp19$conflict_wm)
sjPlot::plot_frq(na.omit(issp19$conflict_wm), type = "histogram", show.mean = TRUE)
issp19$conflict_wm <- sjlabelled::set_label(issp19$conflict_wm, label = c("Conflicts: management - workers"))

## Dependent variable: PSCi

issp19 <- issp19 %>% 
  rowwise() %>%
  mutate(psci = sum(conflict_rp, conflict_wcmc, conflict_wm, na.rm = F)) %>% 
  ungroup()

issp19$psci <- sjlabelled::set_label(issp19$psci, label = c("Perceived Social Conflict Index"))
frq(issp19$psci)
sjPlot::plot_frq(na.omit(issp19$psci), type = "histogram", show.mean = TRUE) # Follows a normal distribution. Careful with the NA

## Cronbach's alpha
matriz <- issp19 %>% select(conflict_rp, conflict_wcmc, conflict_wm)
psych::alpha(matriz) # coef = 0.78

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.84


issp19 %>% filter(!is.na(psci)) %>% count(psci) %>% mutate(prop = prop.table(n))


# ***********************
#
# Contextual level data
#
# ***********************

library(readxl)

wid <- read_excel(path = "input/data/WID_Data.xlsx",
                  range = paste0("A", 2, ":AA", 178),
                  col_names = c("country", "percentile", seq(1997,2021,1))) %>% 
  select(-percentile) %>% 
  pivot_longer(., cols = -1,
               names_to = "year",
               values_to = "value")

country <- issp19 %>% group_by(country) %>% tally() %>% select(-n)

c_vec <- unique(country$country)

frq(wid$country)

wid <- wid %>% 
  mutate(country = if_else(country == "USA", "United States",
                           if_else(country == "Czech Republic", "Czechia",
                                   if_else(country == "Russian Federation", "Russia", country))))

wid_r <- wid %>% 
  filter(country %in% c_vec, year == 2019) %>% 
  rename(top_10 = value) %>% 
  mutate(c_top10 = datawizard::centre(top_10),
         z_top10 = datawizard::standardise(top_10, center = T)) 

db <- left_join(issp19, wid_r[,-2], by = "country")

db <- db %>% group_by(country) %>% 
  mutate(mean.age = mean(age, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(c_age = age - mean.age)  
  
  