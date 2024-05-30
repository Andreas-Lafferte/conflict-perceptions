#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Code to process ISSP 2009 for ISUC classes
# Author: Andreas Laffert            
# Overview: ISSP 2009 processing of all variables only level 1 Chile           
# Date: 12-04-2024            
#
#******************************************************************************************************************

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

issp <- read_dta(file = here("input","ISSP2009.dta"))
glimpse(issp)
names(issp)

# 3. Processing -----------------------------------------------------------

issp <- issp %>% select(V3, 
                        V5,
                        SEX,
                        AGE,
                        DEGREE,
                        UNION,
                        WEIGHT,
                        V40,
                        V41,
                        V42,
                        V43,
                        V64,
                        V66,
                        ISCO88,
                        NEMPLOY,
                        WRKSUP,
                        V23,
                        V25,
                        V28,
                        V30,
                        TOPBOT,
                        CL_RINC,
                        EDUCYRS,
                        CL_PRTY) %>%  
  janitor::clean_names()


issp %>% group_by(v5) %>% tally() %>% print(n = 40)

# paises nuevos en comparacion a version
# que use para la tesis:
# Australia, Austria, Francia, Noruega, Suecia, USA, Venezuela = 7

or_issp <- issp

issp <- issp %>% 
  filter(v5 == 152) # only Chile

# 3.1 YEAR ----

issp$year <- 2009
issp$year <- as.numeric(issp$year) 
issp$year <- sjlabelled::set_label(issp$year, label = c("Wave/Year"))

# 3.2 FACTOR ----

issp <- rename_variables(issp, weight = "factor")
issp$factor <- sjlabelled::set_label(issp$factor, label = c("Weight"))

# 3.3 SEX ----
frq(issp$sex)
issp$sex <- as.numeric(issp$sex)
issp$sex <- car::recode(issp$sex, 
                          recodes = c("-9 = NA; 1 = 1; 2 = 2"),
                          levels = 1:2,
                          as.factor = T)
issp$sex <- sjlabelled::set_label(issp$sex, label = c("Sex"))

# 3.4 AGE ----
frq(issp$age)
issp$age <- set_na(issp$age, na = -9, drop.levels = T, as.tag = F)
issp$age <- as.numeric(issp$age)
issp$age <- sjlabelled::set_label(issp$age, label = c("Age"))

# 3.5 UNION ----
frq(issp$union)
issp$union <- as.numeric(issp$union)
issp$union <- car::recode(issp$union, 
                            recodes = c("c(-9,-7,-4) = NA; 1 = 1; 2 = 1; 3 = '0'"),
                            levels = c(0,1),
                            as.factor = T)
issp$union <- sjlabelled::set_label(issp$union, label = c("Union membership"))


# 3.6 EDUCATION ----
frq(issp$educyrs)

issp <- issp %>% 
  mutate(educyrs = if_else(educyrs == 0, NA, educyrs),
         educyrs = as.numeric(educyrs))

issp$educyrs <- set_na(issp$educyrs, na = c(95:99), drop.levels = F)

# 3.7 INCOME ----
frq(issp$cl_rinc)

issp <- issp %>% 
  mutate(cl_rinc = as.numeric(cl_rinc),
         cl_rinc = car::recode(cl_rinc, "999990 = NA; 999997 = NA; 999998 = NA; 999999 = NA; 
                                              9999990 = NA; 9999997 = NA; 9999998 = NA; 9999999 = NA;
                                              99999990 = NA; 99999997 = NA; 99999998 = NA; 99999999 = NA"))
 
# ntile function without NAs
ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}

issp <- issp %>%  
  mutate(income = ntile_na(cl_rinc, 10))
  
frq(issp$income)

issp$income <- sjlabelled::set_label(issp$income, label = c("Decil ingreso"))

# 3.8 Ideology
frq(issp$cl_prty)

issp <- issp %>% mutate(ideology = case_when(cl_prty %in% c(2,4,9,10) ~ "Derecha",
                                             cl_prty %in% c(1,6,7) ~ "Centro",
                                             cl_prty %in% c(3,5,8) ~ "Izquierda",
                                             cl_prty %in% c(95,96) ~ "Sin identificación",
                                             TRUE ~ NA_character_))




# 3.17 Final data ----

db <- issp %>% 
  select(sex, union, topbot, income, educyrs, ideology, v40, v41, v42, v43)

db <- tibble::rowid_to_column(db, "id_subject")
db <- db %>% as_tibble(.)

# 4. Save ----
db <- db %>% sjlabelled::remove_all_labels()
haven::write_dta(db, path = here("output", "db_isuc2.dta"))
