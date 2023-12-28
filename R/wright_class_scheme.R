# 3.11 CLASS ESCHEME E.O WRIGHT ----
# Con ISSP 2019


## VERSION COLAPSADA 9 CLASES ----


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


## VERSION COMPLETA 12 CLASES ----

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


frq(issp19$nsup)
issp19$nsup <- as.numeric(issp19$nsup)
issp19$nsup[issp19$nsup == -9] <- NA
issp19 <- issp19 %>% mutate(num_sup = case_when(nsup <= 1 ~ "Nonmanagement",
                                                nsup >= 2 & nsup <= 9 ~ "Supervisor",
                                                nsup >= 10 ~ "Manager",
                                                TRUE ~ NA_character_))

issp19 %>% count(nsup) %>% mutate(prop = prop.table(n))
issp19 %>% count(num_sup) %>% mutate(prop = prop.table(n))

issp19 <- issp19 %>% 
  mutate(authority = case_when(control == "No control" & num_sup == "Nonmanagement" ~ "Nonmanagement",
                               control == "Control" & num_sup == "Supervisor" ~ "Supervisor",
                               control == "Control" & num_sup == "Manager" ~ "Manager"))

issp19 %>% count(authority) %>% mutate(prop = prop.table(n))

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

issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Manager" & skillsA=="Expert", 4, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Supervisor" & skillsA=="Expert", 5, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Nonmanagement" & skillsA=="Expert", 6, class))

issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Manager" & skillsA=="Skilled", 7, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Supervisor" & skillsA=="Skilled", 8, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Nonmanagement" & skillsA=="Skilled", 9, class))


issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Manager" & skillsA=="Nonskilled", 10, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Supervisor" & skillsA=="Nonskilled", 11, class))
issp19$class <- with(issp19, ifelse(prop_salaried=="Salaried" & authority=="Nonmanagement" & skillsA=="Nonskilled", 12, class))

issp19$class <- factor(issp19$class,levels = c(1:12),
                       labels = c("1.Capitalists","2.Small employers","3.Petite bourgeoisie",
                                  "4.Expert managers","5.Expert supervisors",
                                  "6.Expert nonmanagers","7.Skilled managers",
                                  "8.Skilled supervisor","9.Skilled workers",
                                  "10.Nonskilled managers","11.Nonskilled supervisor",
                                  "12.Nonskilled workers"))

issp19 %>% filter(!is.na(class)) %>% count(class) %>% mutate(prop = prop.table(n)) 
issp19$class <- sjlabelled::set_label(issp19$class, label = c("Class position EOW"))
