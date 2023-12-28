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

issp19 <- read_dta("input/ZA7600_v3-0-0.dta")
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
                            v12,
                            v13,
                            v14,
                            v17, 
                            v18,
                            v19, 
                            v50,
                            v53,
                            v54, 
                            v55,
                            SEX,
                            AGE,
                            DEGREE,
                            WORK,
                            EMPREL,
                            WRKSUP,
                            NSUP,
                            ISCO08,
                            UNION,
                            WEIGHT,
                            PARTY_LR,
                            BG_PRTY,
                            SR_PRTY,
                            TW_PRTY) %>%  # bg, sr y t1 son los unicos paises de los cuales se puede recodificar el prty, los demas no estan disponibles
  janitor::clean_names()


issp19 %>% group_by(country) %>% tally() %>% print(n = 40)


# paises nuevos en comparacion a version
# que use para la tesis:
# Australia, Austria, Francia, Noruega, Suecia, USA, Venezuela = 7

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

# 3.7 POLICIAL IDENTIFICATION ----
frq(issp19$party_lr)
frq(issp19$bg_prty)
frq(issp19$sr_prty)
frq(issp19$tw_prty)

issp19 <- issp19 %>% mutate(ideology = case_when(party_lr %in% c(1,2) ~ "Left",
                                                 party_lr == 3 ~ "Center",
                                                 party_lr %in% c(4,5) ~ "Right",
                                                 party_lr %in% c(-4,6) ~ "No identification",
                                                 bg_prty %in% c(2,9) ~ "Left",
                                                 bg_prty %in% c(4,6,7) ~ "Center",
                                                 bg_prty %in% c(1,3,5,8,12,13) ~ "Right",
                                                 bg_prty == -4 ~ "No identification",
                                                 sr_prty %in% c(1,4) ~ "Left",
                                                 sr_prty %in% c(2,3,5) ~ "Center",
                                                 sr_prty %in% c(-4,95) ~ "No identification",
                                                 tw_prty %in% c(2) ~ "Left",
                                                 tw_prty %in% c(1,3) ~ "Right",
                                                 tw_prty == -4 ~ "No identification",
                                                 TRUE ~ NA_character_))


issp19$ideology <- as.factor(issp19$ideology)
issp19$ideology <- sjlabelled::set_label(issp19$ideology, label = c("Political identity"))

# 3.8 SUBJECTIVE SOCIAL CLASS ----
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

# 3.10 EDUCATION ----
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

# 3.11 CLASS ESCHEME E.O WRIGHT ----

## Employment relation
issp19 <- issp19 %>% filter(work != 3)
frq(issp19$emprel)

issp19$emprel <- as.numeric(issp19$emprel)
issp19 <- issp19 %>% mutate(prop_salaried = case_when(emprel == 1 ~ "Salaried",
                                                      emprel == 2 ~ "3.Petty bourgeoisie",
                                                      emprel == 5 ~ "3.Petty bourgeoisie",
                                                      emprel == 3 ~ "2.Small employers",
                                                      emprel == 4 ~ "1.Capitalist",
                                                      TRUE ~ NA_character_))

issp19 %>% count(prop_salaried) %>% mutate(prop = prop.table(n))
issp19 %>% count(emprel) %>% mutate(prop = prop.table(n))

## Control
frq(issp19$wrksup)
issp19$wrksup <- as.numeric(issp19$wrksup)
issp19 <- issp19 %>% mutate(control = case_when(wrksup == 1 ~ "Control",
                                                wrksup == 2 ~ "No control",
                                                TRUE ~ NA_character_))

issp19 %>% count(wrksup) %>% mutate(prop = prop.table(n))
issp19 %>% count(control) %>% mutate(prop = prop.table(n))

## Skills
frq(issp19$isco08)
issp19 <- issp19 %>% filter(isco08 != -9, isco08 != -8, isco08 != 110, isco08 != 210, isco08 != 310) # eliminated FA & don't labor force people
issp19$isco08_2 <- as.numeric(issp19$isco08)


issp19$isco08_or <- issp19$isco08_2
issp19$isco08_2 <- substr(issp19$isco08_2, start = 1, stop = 2)
frq(issp19$isco08)
frq(issp19$isco08_2)

### Skills variable
issp19 <- issp19 %>% mutate(skills = case_when(isco08_2 %in% c(10:26) ~ 'Expert',
                                               isco08_2 %in% c(30, 31, 32, 33, 34, 35, 60, 61, 72) ~ 'Skilled',
                                               isco08_2 %in% c(40, 41, 42, 43, 44, 50, 51, 52, 53, 54, 62, 63, 70, 71, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96) ~ 'Nonskilled',
                                               TRUE ~ NA_character_))

issp19 %>% count(skills) %>% mutate(prop = prop.table(n))

## Education control effect
issp19 <- issp19 %>% mutate(skillsA = if_else(skills == "Expert" & educ == "Yes", "Expert",
                                              if_else(skills == "Expert" & educ == "No", "Skilled", skills))) %>% 
  mutate(skillsA = if_else(is.na(skillsA), skills, skillsA))

issp19 %>% count(skills) %>% mutate(prop = prop.table(n))
issp19 %>% count(skillsA) %>% mutate(prop = prop.table(n)) # Skilled up

ctable(issp19$prop_salaried, issp19$skillsA)

## FINAL VARIABLE CLASS 

issp19$class <- NA
issp19$class <- with(issp19, ifelse(prop_salaried=="1.Capitalist", 1, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="2.Small employers", 2, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="3.Petty bourgeoisie", 3, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="Control" & skillsA=="Expert", 4, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="No control" & skillsA=="Expert", 5, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="Control" & skillsA=="Skilled", 6, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="Control" & skillsA=="Nonskilled", 7, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="No control" & skillsA=="Skilled", 8, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & control=="No control" & skillsA=="Nonskilled", 9, class))

issp19$class <- factor(issp19$class,levels = c(1:9),
                       labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                  "4.Expert managers","5.Nonmanagerial expert",
                                  "6.Skilled supervisors","7.Unskilled supervisors",
                                  "8.Skilled workers","9.Unskilled workers"))

issp19 %>% filter(!is.na(class)) %>% count(class) %>% mutate(prop = prop.table(n)) 
issp19$class <- sjlabelled::set_label(issp19$class, label = c("Class position EOW"))

# 3.12 CLASS ESCHEME EGP ----
frq(issp19$emprel)
frq(issp19$isco08_or)

issp19 <- issp19 %>% 
  mutate(isco08_or = occupar::isco08to88(isco08 = isco08_or),
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

# 3.13 International Socio-Economic Index of occupational status ISEI ----

issp19 <- issp19 %>% 
  mutate(isei = occupar::isco08toISEI08(isco08_or))


# 3.14 PERCEIVED SOCIAL CONFLICT INDEX ----

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

# 3.15 DISTRIBUTIVE JUSTICE ----

# Justice gap

issp19 <- issp19 %>% 
  mutate(
    across(
      .cols = c(v12, v14, v17, v19),
      .fns = ~ set_na(., na = c(-8,-9))
    )) %>% 
  rowwise() %>% 
  mutate(eval_just_chairman = ln(v12/v17), 
         eval_just_worker = ln(v14/v19)) %>% 
  ungroup()

summary(issp19$eval_just_chairman)
summary(issp19$eval_just_worker)

# convert -Inf & NaN in NA

issp19$eval_just_chairman[is.infinite(issp19$eval_just_chairman) | is.nan(issp19$eval_just_chairman)] <- NA
summary(issp19$eval_just_chairman)

issp19$eval_just_worker[is.infinite(issp19$eval_just_worker) | is.nan(issp19$eval_just_worker)] <- NA
summary(issp19$eval_just_worker)

issp19$justice_gap <- issp19$eval_just_chairman - issp19$eval_just_worker


# Just income distribution 

issp19 <- issp19 %>% 
  mutate(v50 = as.numeric(v50),
         dist_incom_fair = set_na(v50, na = c(-8,-9)),
         dist_incom_fair = if_else(dist_incom_fair %in% c(1,2), "Fair", "Unfair")) 


# ***********************
#
# Contextual level data
#
# ***********************

