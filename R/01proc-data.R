# Code 1: Data preparation

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               dplyr,
               stringr, 
               sjlabelled, 
               sjmisc, 
               kableExtra,
               sjPlot,
               haven,
               stargazer,
               magrittr,
               psych, 
               gtsummary,
               datawizard)
options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

#cargamoss las bases de datos desde internet
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/issp99.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/issp09.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/issp19.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/ictwss.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/oecd_euro.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/wid.RData"))
load(url("https://github.com/Andreas-Lafferte/tesis/raw/main/output/data/wiid.RData"))


# 3. Manipulation ---------------------------------------------------------

# 3.1 ISSP ----

## Join ISSP
issp99 <- issp99 %>% sjlabelled::remove_all_labels() %>% as_tibble()
issp09 <- issp09 %>% sjlabelled::remove_all_labels()%>% as_tibble()
issp19 <- issp19 %>% sjlabelled::remove_all_labels()%>% as_tibble()

db <- rbind(issp99,issp09)
db <- rbind(db,issp19)

### Re-level ideology

frq(db$IDEOLOGY)
levels(db$IDEOLOGY)

db$IDEOLOGY <- car::recode(db$IDEOLOGY, recodes = c("'Derecha' = 'Derecha'; 
                                                'Centro' = 'Centro';
                                                'Izquierda' = 'Izquierda';
                                                'Sin identificación' = 'Sin identificación'")) %>% 
  factor(., levels = c("Derecha", "Centro", "Izquierda", "Sin identificación"),
         labels = c("Derecha", "Centro", "Izquierda", "Sin identificación"))


## Dependent variable: PSCi

db <- db %>% 
  rowwise() %>%
  mutate(PSCi = sum(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW, na.rm = F)) 

frq(db$PSCi)
sjPlot::plot_frq(na.omit(db$PSCi), type = "histogram", show.mean = TRUE) 

## Cronbach's alpha
matriz <- db %>% select(CONFLICT_RP, CONFLICT_WCMC, CONFLICT_MW)
psych::alpha(matriz) # coef = 0.78

## Polychoric alpha ordinal (Likert scale)
matriz_poly <- polychoric(matriz) 
psych::alpha(matriz_poly$rho) # coef = 0.83

db <- as.data.frame(db) # remove Rowwise type

db %>% filter(!is.na(PSCi)) %>% count(PSCi) %>% mutate(prop = prop.table(n))

# 3.2 Contextual level variables (WIID, WID, ICTWSS, OECD) ----
names(wid)
names(ictwss)
names(oecd)
names(wiid)

df <- full_join(wiid, ictwss, by = c("COUNTRY", "YEAR"))
df <- full_join(df, oecd, by = c("COUNTRY", "YEAR"))
df <- full_join(df, wid, by = c("COUNTRY", "YEAR"))

country <- db %>% group_by(COUNTRY, YEAR) %>% tally() %>% select(-n)

df <- inner_join(country, df)

df <- df %>% select(COUNTRY, YEAR, CorpAll, GDP, SOC_EXPEND, TOP10) %>% ungroup()

# 3.2.1 Cross-sectional and Longitudinal effects ----

## Center and Z core (only 2 level data)

df <- df %>% 
  mutate(C_TOP10 = datawizard::centre(TOP10),
         Z_TOP10 = datawizard::standardise(TOP10, center = T),
         C_GDP = datawizard::centre(log(GDP)),
         Z_GDP = datawizard::standardise(log(GDP), center = T),
         C_SOC_EXPEND = datawizard::centre(SOC_EXPEND),
         Z_SOC_EXPEND = datawizard::standardise(SOC_EXPEND, center = T)) 

## Cross-sectional effects

# Country mean
df <- df %>% 
  group_by(COUNTRY) %>% 
  mutate(MEAN_TOP10 = mean(TOP10, na.rm = T), # MEAN TOP10
         MEAN_CorpAll = mean(CorpAll, na.rm = T), # MEAN CorpAll
         MEAN_GDP = mean(GDP, na.rm = T),
         MEAN_SOCEXPEND = mean(SOC_EXPEND, na.rm = T)) %>% # MEAN GDP
  ungroup()

df %>% group_by(COUNTRY) %>% count(MEAN_TOP10) %>% print(n = nrow(.)) # View


## Longitudinal effects 

# Mean centered

df <- df %>% mutate(LAG_TOP10 = (TOP10 - MEAN_TOP10), # LAG RATIO
                    LAG_GDP = (GDP - MEAN_GDP)) # LAG GDP

df %>% group_by(COUNTRY) %>% count(LAG_TOP10) %>% print(n = nrow(.)) # View

df[sapply(df, is.nan)] <- NA

# 3.3 Final data and transforming variables ----

db <- full_join(db, df, by = c("COUNTRY", "YEAR"))

# ID subject
db <- tibble::rowid_to_column(db, "ID_SUBJECT")

db_original <- db %>% as_tibble(.) #original

db <- db %>% select(everything(), -DEGREE, -SUBJEC_CLASS, -INCOME, -CONFLICT_TB, -EGP, -ISEI)

db <- db %>% na.omit()

db <- left_join(db, db_original[,c(1,12,13)], by = "ID_SUBJECT")

df_original <- df

df <- df %>% na.omit()
df <- df %>% filter(!COUNTRY %in% c("Japon", "Canada", "Irlanda"))
df <- df %>% as_tibble(.)


# 3.4 Transforming variables ----

## AGE center CWC (group mean)

db <- db %>% group_by(COUNTRY) %>% 
  mutate(mean.age = mean(AGE, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(COUNTRY) %>% 
  mutate(C_AGE = AGE - mean.age)

# 3.5 Identification variables ----

# ISO code
db <- db %>% mutate(ISO_COUNTRY = case_when(COUNTRY == "Alemania" ~ "DEU",
                                            COUNTRY == "Argentina" ~ "ARG",
                                            COUNTRY == "Australia" ~ "AUS",
                                            COUNTRY == "Austria" ~ "AUT",
                                            COUNTRY == "Belgica" ~ "BEL",
                                            COUNTRY == "Bulgaria" ~ "BGR",
                                            COUNTRY == "Canada" ~ "CAN",
                                            COUNTRY == "Chile" ~ "CHL",
                                            COUNTRY == "China" ~ "CHN",
                                            COUNTRY == "Chipre" ~ "CYP",
                                            COUNTRY == "Corea del Sur" ~ "KOR",
                                            COUNTRY == "Croacia" ~ "HRV",
                                            COUNTRY == "Dinamarca" ~ "DNK",
                                            COUNTRY == "Eslovaquia" ~ "SVK",
                                            COUNTRY == "Eslovenia" ~ "SVN",
                                            COUNTRY == "España" ~ "ESP",
                                            COUNTRY == "Estonia" ~ "EST",
                                            COUNTRY == "Filipinas" ~ "PHL",
                                            COUNTRY == "Finlandia" ~ "FIN",
                                            COUNTRY == "Francia" ~ "FRA",
                                            COUNTRY == "Gran Bretaña" ~ "GBR",
                                            COUNTRY == "Hungria" ~ "HUN",
                                            COUNTRY == "Irlanda" ~ "IRL",
                                            COUNTRY == "Islandia" ~ "ISL",
                                            COUNTRY == "Israel" ~ "ISR",
                                            COUNTRY == "Italia" ~ "ITA",
                                            COUNTRY == "Japon" ~ "JPN",
                                            COUNTRY == "Letonia" ~ "LVA",
                                            COUNTRY == "Lituania" ~ "LTU",
                                            COUNTRY == "Noruega" ~ "NOR",
                                            COUNTRY == "Nueva Zelanda" ~ "NZL",
                                            COUNTRY == "Polonia" ~ "POL",
                                            COUNTRY == "Portugal" ~ "PRT",
                                            COUNTRY == "Rep Checa" ~ "CZE",
                                            COUNTRY == "Rusia" ~ "RUS",
                                            COUNTRY == "Sudafrica" ~ "ZAF",
                                            COUNTRY == "Suecia" ~ "SWE",
                                            COUNTRY == "Suiza" ~ "CHE",
                                            COUNTRY == "Surinam" ~ "SUR",
                                            COUNTRY == "Tailandia" ~ "THA",
                                            COUNTRY == "Taiwan" ~ "TWN",
                                            COUNTRY == "Turquia" ~ "TUR",
                                            COUNTRY == "Ucrania" ~ "UKR",
                                            COUNTRY == "USA" ~ "USA",
                                            COUNTRY == "Venezuela" ~ "VEN",
                                            TRUE ~ NA_character_))

## Wave
db$WAVE <- as.factor(db$YEAR)
db$COUNTRY_WAVE <- do.call(paste, c(db[c("ISO_COUNTRY", "WAVE")], sep = "_"))

# 3.6 Final data ----

db <- db %>% select(YEAR, COUNTRY, ISO_COUNTRY, WAVE, COUNTRY_WAVE, SEX, AGE,
                    IDEOLOGY, UNION, CLASS, EGP, ISEI, PSCi, TOP10, CorpAll, GDP, 
                    SOC_EXPEND, C_TOP10, Z_TOP10, TOP10_BE=MEAN_TOP10, TOP10_WE=LAG_TOP10, 
                    C_GDP, Z_GDP, GDP_BE=MEAN_GDP, GDP_WE=LAG_GDP, MEAN_CorpAll, MEAN_SOCEXPEND,
                    C_SOC_EXPEND, Z_SOC_EXPEND, 
                    C_AGE, FACTOR, starts_with("CONFLICT"))

db <- tibble::rowid_to_column(db, "ID_SUBJECT")
db <- db %>% as_tibble(.)

## Labels

db$YEAR <- sjlabelled::set_label(db$YEAR, label = c("Año"))
db$SEX <- sjlabelled::set_label(db$SEX, label = c("Sexo"))
db$AGE <- sjlabelled::set_label(db$AGE, label = c("Edad"))
db$UNION <- sjlabelled::set_label(db$UNION, label = c("Afiliación sindical"))
db$IDEOLOGY <- sjlabelled::set_label(db$IDEOLOGY, label = c("Identificación política"))
db$CLASS <- sjlabelled::set_label(db$CLASS, label = c("Posición de clase EOW"))
db$EGP <- sjlabelled::set_label(db$EGP, label = c("Posición de clase EGP"))
db$ISEI <- sjlabelled::set_label(db$ISEI, label = c("International Socio-Economic Index of occupational status"))
db$ISO_COUNTRY <- sjlabelled::set_label(db$ISO_COUNTRY, label = c("Código ISO país"))
db$WAVE <- sjlabelled::set_label(db$WAVE, label = c("Ola"))
db$COUNTRY_WAVE <- sjlabelled::set_label(db$COUNTRY_WAVE, label = c("País-ola"))
db$TOP10_BE <- sjlabelled::set_label(db$TOP10_BE, label = c("Top 10% income share [BE]"))
db$TOP10_WE <- sjlabelled::set_label(db$TOP10_WE, label = c("Top 10% income share [WE]"))
db$GDP_BE <- sjlabelled::set_label(db$GDP_BE, label = c("GDP Per capita [BE]"))
db$GDP_WE <- sjlabelled::set_label(db$GDP_WE, label = c("GDP Per capita [WE]"))
db$MEAN_CorpAll <- sjlabelled::set_label(db$MEAN_CorpAll, label = c("Indice corporativismo [CG]"))
db$CorpAll <- sjlabelled::set_label(db$CorpAll, label = c("Indice corporativismo [Z score]"))
db$MEAN_SOCEXPEND <- sjlabelled::set_label(db$MEAN_SOCEXPEND, label = c("Gasto social %GDP [GC]"))
db$SOC_EXPEND <- sjlabelled::set_label(db$SOC_EXPEND, label = c("Gasto social (%GDP)"))
db$C_AGE <- sjlabelled::set_label(db$C_AGE, label = c("Edad [CWC]"))
db$FACTOR <- sjlabelled::set_label(db$FACTOR, label = c("Factor expansión"))
db$COUNTRY <- sjlabelled::set_label(db$COUNTRY, label = c("País"))
db$PSCi <- sjlabelled::set_label(db$PSCi, label = c("Perceived Social Conflict Index"))
db$TOP10 <- sjlabelled::set_label(db$TOP10, label = c("Top 10% income share"))
db$GDP <- sjlabelled::set_label(db$GDP, label = c("GDP per capita USD"))
db$CONFLICT_RP <- sjlabelled::set_label(db$CONFLICT_RP, label = c("Conflictos: ricos - pobres"))
db$CONFLICT_WCMC <- sjlabelled::set_label(db$CONFLICT_WCMC, label = c("Conflictos: clase obrera - clase media"))
db$CONFLICT_MW <- sjlabelled::set_label(db$CONFLICT_MW, label = c("Conflictos: directivos - trabajadores"))
db$ID_SUBJECT <- sjlabelled::set_label(db$ID_SUBJECT, label = c("Id persona"))
db$C_TOP10 <- sjlabelled::set_label(db$C_TOP10, label = c("Top 10% income share [GMC]"))
db$Z_TOP10 <- sjlabelled::set_label(db$Z_TOP10, label = c("Top 10% income share [Z score]"))
db$C_GDP<- sjlabelled::set_label(db$C_GDP, label = c("GDP Per capita [GMC]"))
db$Z_GDP <- sjlabelled::set_label(db$Z_GDP, label = c("GDP Per capita [Z score]"))
db$C_SOC_EXPEND<- sjlabelled::set_label(db$C_SOC_EXPEND, label = c("Gasto social (%GDP) [GMC]"))
db$Z_SOC_EXPEND <- sjlabelled::set_label(db$Z_SOC_EXPEND, label = c("Gasto social (%GDP) [Z score]"))

# Names

db <- db %>% janitor::clean_names() %>% rename(psci = ps_ci)

# 4. Save ----

save(db, file = "output/db-proc.RData")
save(df, file = "output/df2-proc.RData")
