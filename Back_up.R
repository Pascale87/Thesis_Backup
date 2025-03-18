# 1. Install packages ------------------------------------------------------
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("feather")
library(feather)
# library(ggplot2)
# install.packages("padr")
library(padr)
library(feather)
# library(ggplot2)
# install.packages("lubridate")
library(lubridate)
# install.packages("janitor")
library(janitor)

# 2. Import data sets --------------------------------------------------------
# 2.a Data sets
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr2.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Diagnose_red2_corr.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/DRG_red1.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Move_stat2f.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Pat_info.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Procedure_red2_corr.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/gestational_age_2024-10-01.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/LOS_newborns_2025-03-06.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/parity_2024-06-19.RData")

# 2.b Pseudonymised data sets: Consent 
Consent <- read_feather("I:/Verwaltung/MaNtiS/02_Pseudonymisierte_Daten/Consent_pseud.feather")


# 3. Birth information ----------------------------------------------------
# 3.1 Birth_corr2 ------------------------------------
summary(Birth_corr2)

## Time frame check (Inclusion criteria 1)
summary(Birth_corr2$CBIS_BIRTH_DATE_TS)
# Min.                       1st Qu.                    Median                     Mean                       3rd Qu.                    Max. 
# "2019-01-01 01:26:00.0000" "2019-12-22 02:57:00.0000" "2021-01-06 00:12:30.0000" "2020-12-31 05:41:56.7116" "2021-12-25 17:35:00.0000" "2022-12-31 16:17:00.0000" 
# Conclusion: no exclusions

## patient_id_child
sum(is.na(Birth_corr2$patient_id_child)) # there are 2 NAs
## case_id_child
sum(is.na(Birth_corr2$case_id_child)) # there are 2 NAs
na_case <- Birth_corr2 %>%
  filter(is.na(case_id_child)) # stillbirths 

Birth_corr2 <- Birth_corr2 %>%
  filter(!is.na(case_id_child)) # excluded cases without case_id, 10776 (-2)

## CBIS_BIRTH_DATE_TS (Birth date and time)
summary(Birth_corr2$CBIS_BIRTH_DATE_TS)
#Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
#"2019-01-01 01:26:00.0000" "2019-12-21 18:54:45.0000" "2021-01-05 15:17:00.0000" "2020-12-30 15:21:19.3272" "2021-12-24 07:42:30.0000" "2022-12-31 16:17:00.0000" 
# from 2019-01-01 01:26:00. to 2022-12-31 16:17:00
sum(is.na(Birth_corr2$CBIS_BIRTH_DATE_TS)) # no NAs

## CBIS_MULTIPLE_BIRTH_FLAG (multiple birth (several children), 1 if yes, otherwise 0)
summary(as.factor(Birth_corr2$CBIS_MULTIPLE_BIRTH_FLAG))
# 0      1 
# 10143   633 
# there are no NAs

## Stillbirths
table(Birth_corr2$CBIS_STILLBIRTH_FLAG) # 77

## CBIS_CONGENITAL_MALFORMATION (congenital malformation (1 = yes, 0 = no, (-1) = unknown))
table(Birth_corr2$CBIS_CONGENITAL_MALFORMATION)
# -1     0     1 
# 73 10083   620 
# there are no NAs

## CBIS_MODE (codes in (1,7) - exact decryption still unclear))
table(Birth_corr2$CBIS_MODE) 
# 1    2    3    4    6 n.a. 
# 5409  841 1066  103   48 3309 
## not relevant

## CBIS_WEIGHT
summary(Birth_corr2$CBIS_WEIGHT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 135    2930    3300    3219    3650    5900      44 

# hist(Birth_corr2$CBIS_WEIGHT, main = "Histogram Weight Newborns (2019-2022)", 
#      xlab = "weight (g)", ylab = "Freq")
# # all newborns are here included, also the premature
# (Summ_weight <- Birth_corr2 %>%
#     summarize(mean_weight = mean(CBIS_WEIGHT, na.rm = TRUE),
#               min_weight = min(CBIS_WEIGHT, na.rm = TRUE),
#               max_weight = max(CBIS_WEIGHT, na.rm = TRUE)))
# #         mean_weight min_weight max_weight
# #           <dbl>      <dbl>      <dbl>
# #  1       3219.        135       5900

## CBIS_BODY_SIZE
summary(Birth_corr2$CBIS_BODY_SIZE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 19.00   48.00   50.00   49.64   52.00   63.00      45 

# hist(Birth_corr2$CBIS_BODY_SIZE, main = "Histogram Body Size Newborns (2019-2022)", 
#      xlab = "size (cm)", ylab = "Freq")
# (Summ_size_body <- Birth_corr2 %>%
#     summarize(mean_size_body = mean(CBIS_BODY_SIZE, na.rm = TRUE),
#               min_size_body = min(CBIS_BODY_SIZE, na.rm = TRUE),
#               max_size_body = max(CBIS_BODY_SIZE, na.rm = TRUE)))
# #       mean_size min_weight max_weight
# #         <dbl>      <dbl>      <dbl>
# #  1      49.6         19         63

## CBIS_HEAD_SIZE
summary(Birth_corr2$CBIS_HEAD_SIZE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 12.00   33.00   34.00   34.02   35.00   53.00      57 

# (Summ_size_head <- Birth_corr2 %>%
#     summarize(mean_size_head = mean(CBIS_HEAD_SIZE, na.rm = TRUE),
#               min_size_head = min(CBIS_HEAD_SIZE, na.rm = TRUE),
#               max_size_head = max(CBIS_HEAD_SIZE, na.rm = TRUE)))
# #             mean_size_head min_weight_head max_weight_head
# #               <dbl>           <dbl>           <dbl>
# #  1           34.0              12              53

## CBIS_APGAR_SCORE_0Min
summary(Birth_corr2$CBIS_APGAR_SCORE_0MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   8.000   9.000   8.005   9.000  10.000    2597 

## CBIS_APGAR_SCORE_5Min
summary(Birth_corr2$CBIS_APGAR_SCORE_5MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   9.000   9.000   9.077  10.000  10.000    2605 

## CBIS_APGAR_SCORE_10Min
summary(Birth_corr2$CBIS_APGAR_SCORE_10MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000  10.000  10.000   9.599  10.000  10.000    2605 

## CBIS_COMMENT
summary(as.factor(Birth_corr2$CBIS_COMMENT)) 
# various specified comments, but not always -> variable will be deleted as not necessary and there are also person related information

## CBIS_CREATE_DATE (Date of creation of record) -> not needed, will be deleted
## CBIS_CREATE_DAY_BK (Date of creation of record in format YYYYMMDD) -> not needed, will be deleted
## CBIS_UPDATE_DATE (Date of update of record) -> not needed, will be deleted
## CBIS_UPDATE_DAY_BK (Date of update of record in format YYYYMMDD) -> not needed, will be deleted

# Check for id /cases
Check_id <- Birth_corr2 %>% 
  select(patient_id_child, case_id_child) %>% 
  distinct
# 10776 obs

Check_pat_id <- Birth_corr2 %>% 
  select(patient_id_child) %>% 
  distinct

Check_case_id <- Birth_corr2 %>% 
  select(case_id_child) %>% 
  distinct

# Check for duplicates
sum(duplicated(Birth_corr2)) # no duplicates

# 3.2 Gestational age data set (for inclusion criteria 3)------------------------------------------
# 10348 obs of 7 variables
sum(is.na(gestational_age_final))  # no NAs
summary(gestational_age_final$Weeks_LPM)
# Min.   1st Qu.  Median  Mean   3rd Qu.  Max. 
# 18.00   38.00   39.00   38.38   40.00   43.00 
summary(gestational_age_final$gestational_age_total_days)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 128.0   268.0   276.0   271.6   283.0   303.0

# 3.3 Consent denial cases (Lookup1) ----------------------------------------------------
# Exclude cases with consent denial (mother & child)
summary(as.factor(Consent$CON_VALUE))
# Ja    n.a.  Nein 
# 5188   36   1364 

# Consent-checks mother and child
Consent_check <- left_join(Birth_corr2, Consent, by = c("patient_id_mother" = "patient_id"))
Consent_check2 <- left_join(Birth_corr2, Consent, by = c("patient_id_child" = "patient_id"))

Consent_exclude_mother <- Consent_check %>%
  filter(CON_VALUE %in% "Nein") %>% 
  select(patient_id_mother, case_id_mother) # 1381

Consent_exclude_child <- Consent_check2 %>%
  filter(CON_VALUE %in% "Nein") %>% 
  select(patient_id_child, case_id_child) # 82 

Lookup1 <- Birth_corr2 %>%
  filter(!case_id_mother %in% Consent_exclude_mother$case_id_mother) %>%
  filter(!case_id_child %in% Consent_exclude_child$case_id_child)  # 9342

## Only child
# Consent_check <- left_join(Birth_corr2, Consent, by = c("patient_id_child" = "patient_id")) # 10776
# summary(as.factor(Consent_check$CON_VALUE))
# #   Ja  Nein  NA's 
# #   376   82  10318 
# 
# ## All newborn cases with consent "Nein" must be excluded
# Lookup1 <- Consent_check %>% 
#   filter(!CON_VALUE %in% "Nein") %>% 
#   select(!c(CON_VALID_FROM_DATE, CON_VALID_TO_DATE))  # 10694 (-82 obs)
# 
# summary(as.factor(Lookup1$CON_VALUE))
# # Ja  NA's 
# # 376 10318
# 
# # Check id
# Lookup1_case_id <- Lookup1 %>% 
#   select(case_id_child) %>% 
#   distinct() # 10694

# 3.4 Sample selection ------------------------------------------
# To bring together the GA data set with Birth_corr2 data (Lookup1)
Birth_corr2_m <- left_join(Lookup1, gestational_age_final, by = c("patient_id_mother", "case_id_mother", "patient_id_child", "case_id_child"), relationship = "many-to-many")
# 9342
summary(Birth_corr2_m) # overview: Gestational age there are now 392 NAs -> most of them amb./tagesklinik
Birth_corr2_m <- Birth_corr2_m %>% 
  filter(!is.na(gestational_age_total_days))
table(duplicated(Birth_corr2_m))  # check for duplicates

# 3.4.1 >= 37 GA ---------------------------------------------------------------
# Inclusion criteria 3
# Select only variable of interest
Birth_corr2_m <- Birth_corr2_m %>%
  select(patient_id_mother, case_id_mother, patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, CBIS_WEIGHT, CBIS_WEIGHT_UNIT,
         CBIS_STILLBIRTH_FLAG, CBIS_CONGENITAL_MALFORMATION, Weeks_LPM, Days_LPM, gestational_age_total_days, admission_neo, admission_neo_n) #- CON_VALUE

# Lookup2 <- Birth_corr2_m %>%
#   select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, CBIS_BIRTH_DAY_BK, CBIS_BIRTH_TIM_BK, CBIS_WEIGHT, CBIS_WEIGHT_UNIT, CBIS_STILLBIRTH_FLAG, CBIS_CONGENITAL_MALFORMATION,
#          CBIS_BODY_SIZE, CBIS_BODY_SIZE_UNIT, CBIS_HEAD_SIZE, CBIS_HEAD_SIZE_UNIT, admission_neo, admission_neo_n, Weeks_LPM, Days_LPM, gestational_age_total_days, CON_VALUE) 

# Create data set with newborns >= 37 GA (259 weeks)
Sample1 <- Birth_corr2_m %>% 
  filter(gestational_age_total_days >= "259") # 7802 

# 3.4.1.1 Birth weight ---------------------------------------------------------
## To exclude newborns which have a birth weight < 2500g 
summary(Sample1$CBIS_WEIGHT) # there are cases with less than 2500 g
Check_weight <- Sample1 %>% 
  filter(CBIS_WEIGHT < 2500) # 228: these cases are to be excluded
Sample1 <- Sample1 %>% 
  filter(CBIS_WEIGHT >= 2500) %>% 
  select(- CBIS_WEIGHT_UNIT) %>% 
  rename(Birth_weight_g = CBIS_WEIGHT) # 7574

# 3.4.2 Stillbirth and Malformations --------------------------------------
table(Sample1$CBIS_STILLBIRTH_FLAG) 
# 0      1 
# 7568   6

## CBIS_CONGENITAL_MALFORMATION (congenital malformation (1 = yes, 0 = no, (-1) = unknown))
table(Sample1$CBIS_CONGENITAL_MALFORMATION)
# -1    0    1 
# 18 7152  404 

Check_stillbirth <- Sample1 %>% 
  filter(CBIS_STILLBIRTH_FLAG == 1, CBIS_CONGENITAL_MALFORMATION == 1) # 0 cases with both conditions

Sample1 <- Sample1 %>% 
  filter(CBIS_STILLBIRTH_FLAG == 0, CBIS_CONGENITAL_MALFORMATION == 0) 
# Sample1 <- Sample1 %>% 
#   filter(CBIS_STILLBIRTH_FLAG == 0, CBIS_CONGENITAL_MALFORMATION == 0 | CBIS_CONGENITAL_MALFORMATION == -1)
# excluded conditions stillbirths and malformation
# -> with malformation "unknown" n= 7164
# -> without malformation "unknown" n= 7574

# Percentages
Birth_corr2_m %>%
  summarise(total = n(),
            GA_week37 = sum(gestational_age_total_days >= 259),
            percent = (GA_week37 / total) * 100)
# total GA_week37 percent
# <int>     <int>   <dbl>
#1  8950      7802    87.2

# Admission neo (just a between step for control, inclusion criteria 2 is actually missing)
Adm_nicu <- Sample1 %>% 
  filter(admission_neo %in% "Yes") # 351
Sample1 %>%
  summarise(total = n(), 
            admitted = sum(admission_neo %in% "Yes"), 
            percent = (admitted / total) * 100)
# total admitted percent
# <int>    <int>   <dbl>
# 7146      351    4.91

rm(Lookup1, Lookup1_case_id, Check_case_id, Check_id, Check_weight, Consent_check, na_case, Consent_check, Consent_check2, Consent_exclude_child, Consent_exclude_mother)

### Next steps:
# - Merge Sample1 with Movement data set (LOS data) 
# - (Merging with icds)
# - Merging with temp data, Meona data

# 3.4.3 Newborn transfer -----------------------------------------------------
# LOS data (extracted from Movement data, only newborns admitted to the Muki and discharged from Muki)
## Overview
summary(LOS_newborns) # 9268
table(is.na(LOS_newborns$patient_id_child)) # no NAs
table(is.na(LOS_newborns$case_id_child)) # no NAs
table(is.na(LOS_newborns$LOS_neonatal)) # no NAs

# Merging with Sample1, with variables selection and arrangement and renaming variables
Sample2 <- left_join(Sample1, LOS_newborns, by = c("patient_id_child", "case_id_child")) %>% 
  select(- CBIS_STILLBIRTH_FLAG, - CBIS_CONGENITAL_MALFORMATION) %>% 
  relocate(admission_postnatal_neonatal, discharge_postnatal_neonatal, admission_neo, admission_neo_n, .after = last_col()) %>% # .after = Destination of columns selected by
  rename(admission_MUKI = admission_postnatal_neonatal, discharge_MUKI = discharge_postnatal_neonatal, LOS = LOS_neonatal) 

summary(Sample2)

# Check admission_Muki
table(is.na(Sample2$admission_MUKI))
# FALSE  TRUE 
# 6903   243 

# Check cases not transferred directly from the labour ward to the postnatal unit
Sample2_isna_transfer <- Sample2 %>% 
  filter(is.na(admission_MUKI)) 

Move_newborn <- left_join(Sample2_isna_transfer, Move_stat2f, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, admission_neo, admission_neo_n, MOV_START_DATE_TS, MOV_END_DATE_TS, CAS_TYPE, MOV_KIND, MOV_TYPE,
         MOV_REASON1, MOV_REASON2, unit_id, ORG)
## Reasons: n= 3 Home, n= 19 Birth centre, n= 3 inpatient births, n= 196 adm. NICU, n= 2 transfer other unit not postnatal unit (SS, Chirurgie) --> missing 20 cases?

Labour_dis <- Move_newborn %>%
  filter(unit_id %in% "00002030", MOV_TYPE == "Entlassung") # 237
Labour_dis_nicu <- Labour_dis %>% 
  filter(admission_neo %in% "Yes") # 196 --> HERE I NEED HELP

# Dataset cleaned without no transfer to neonatal unit
Sample2 <- Sample2 %>% 
  filter(!is.na(admission_MUKI)) # 6903

table(Sample2$admission_neo)
# No    Yes 
# 6748  155  

# attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
# Sample2 <- Sample2 %>% 
#   mutate(LOS = LOS/60)
# attr(Sample2$LOS, "units") <- "h"

# Check ids
Check_id_sample2_child <- Sample2 %>% 
  select(patient_id_child) %>% 
  distinct # 6903
Check_id_sample2_child2 <- Sample2 %>% 
  select(case_id_child) %>% 
  distinct # 6903

Check_id_sample2_mo <- Sample2 %>% 
  select(patient_id_mother) %>% 
  distinct # 6297
Check_id_sample2_mo2 <- Sample2 %>% 
  select(case_id_mother) %>% 
  distinct # 6849

# sum(is.na(Sample2$patient_id_mother)) # 0
# sum(is.na(Sample2$case_id_mother)) # 0

rm(Check_id_sample2_child, Check_id_sample2_child2, Check_id_sample2_mo, Check_id_sample2_mo2, Sample1, Labour_dis, Labour_dis_nicu)


# 3.4.4 Neonatal diagnoses -------------------------------------------------
# 3.4.4.1 Hypoglycaemia categories ----------------------------------------

# source("/Home/Thesis/Code/01_03_02_Code_Newborn_Meona.R")

## Next step: merging Hypoglyc_data_cat2 with Sample2 (attention: without variable birth date! It doesn't work with these variables, 
## pat_geb_dat is also a little different than CBIS_BIRTH_DATE_TS)
# Sample2b <- left_join(Sample2, Hypoglyc_data_cat2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# 
# table(Sample2b$Hypoglycaemia_cat)
# # Mild       Moderate     No_measure Normoglycaemic         Severe 
# # 177             28           4312           2289             97 
# round(prop.table(table(as.factor(Sample2b$Hypoglycaemia_cat))) * 100, 1)
# # Mild       Moderate     No_measure     Normoglycaemic     Severe 
# # 2.6            0.4           62.5           33.2            1.4 

# 3.4.4.2 Hyperbilirubinaemia categories ----------------------------------------


# 3.4.4.1 Hypothermia categories ----------------------------------------



# Join with Diagnose_red2_corr data set
Sample3 <- left_join(Sample2, Diagnose_red2_corr, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(- DIG_DATE_TS, - DIG_RANK, - DIG_TARGET_SITE, - DIA_BK, - ICD_groups)

table(Sample3$ICD_labels)

## Diagnoses
# 1. Hypoglycaemia: P70.4, E16.2
# 2. Hyperbilirubinaemia: P58.1, P58.8, P59.0, P59.8., P59.9
# 3. Hypothermia: P80.8, P80.9, P81.8, P81.9

# ICD-10 codes
Hypoglycaemia_codes <- c("P70.4", "E16.2")
Hyperbilirubinaemia_codes <- c("P58.1", "P58.8", "P59.0", "P59.8", "P59.9")
Hypothermia_codes <- c("P80.8", "P80.9", "P81.8", "P81.9")


# Create with the icd-10 codes new variable per H to check
Sample3_dia <- Sample3 %>%
  mutate(Dia_hypoglyc = DIA_NK %in% Hypoglycaemia_codes,
  Dia_hyperbili = DIA_NK %in% Hyperbilirubinaemia_codes,
  Dia_hypotherm = DIA_NK %in% Hypothermia_codes)

# Bring together the diagnoses information per patient
Sample4 <- Sample3_dia %>%
  group_by(patient_id_child, case_id_child) %>% # because of multiple rows
  summarise(Dia_hypoglyc = any(Dia_hypoglyc), # check if at least one of the rows contains the value TRUE for the related diagnosis
            Dia_hyperbili = any(Dia_hyperbili),
            Dia_hypotherm = any(Dia_hypotherm)) %>%
              mutate(Hypoglyc_yn = if_else(Dia_hypoglyc, "Yes", "No"),
                     Hyperbili_yn = if_else(Dia_hyperbili, "Yes", "No"),
                     Hypotherm_yn = if_else(Dia_hypotherm, "Yes", "No"),
                      Diag_count = Dia_hypoglyc + Dia_hyperbili + Dia_hypotherm,
                      Diagnoses_HHH = case_when(Diag_count == 0 ~ "None",
                      Diag_count == 1 & Dia_hypoglyc == TRUE ~ "Hypoglyc_only",
                      Diag_count == 1 & Dia_hyperbili == TRUE ~ "Hyperbili_only",
                      Diag_count == 1 & Dia_hypotherm == TRUE ~ "Hypotherm_only",
                      Diag_count == 2 & Dia_hypoglyc == TRUE & Dia_hyperbili == TRUE ~ "Hypoglyc_Hyperbili",
                      Diag_count == 2 & Dia_hypoglyc == TRUE & Dia_hypotherm == TRUE ~ "Hypoglyc_Hypothermia",
                      Diag_count == 2 & Dia_hyperbili == TRUE & Dia_hypotherm == TRUE ~ "Hyperbili_Hypothermia",
                      Diag_count == 3 ~ "All_diagnoses", TRUE ~ "Unknown")) # "Unknown" to cover all cases- should not be the case here, but is good practice for case_when

table(Sample4$Diagnoses_HHH)
# Hyperbili_Hypothermia   Hyperbili_only  Hypoglyc_Hyperbili  Hypoglyc_Hypothermia  Hypoglyc_only   Hypotherm_only       None
# 2                       42              1                    29                   184             164                  6481

# Bring together Sample3_dia and Sample3
Sample4 <- left_join(Sample3, Sample3_dia, by = c("patient_id_child", "case_id_child")) %>%
  select(- Dia_hypoglyc, - Dia_hyperbili, - Dia_hypotherm, - Hypoglyc_yn, - Hyperbili_yn, - Hypotherm_yn, - Diag_count) # %>%
  # distinct(patient_id_child, .keep_all = TRUE) -> all other diagnoses are no more visible, not as this stage
# 12338

# 3.4.5 Risk factors HHH ------------------------------------------------------

# 1. Hypoglycaemia: P70.0, P70.1, P70.8, Z83.3, birth weight > 4500g, hypothermia, Parity
# 2. Hyperbilirubinaemia: P55.0, P55.1, D55.0, (race/ethnicity), (maternal age), GA, GDM/DM, macrosomic infant (= birth weight >4500g) of a diabetic mother
# 3. Hypothermia: hypoglycaemia, Race/ethnicity, maternal age, parity

#  3.4.5.1 Birth weight >4500g --------------------------------
Sample4 <- Sample3 %>% 
  mutate(RF_weight = if_else(CBIS_WEIGHT >= 4500, "Yes", "No"))

# 3.4.5.2 Parity ----------------------------------------------------------

Parity <- parity1 %>% 
  mutate(RF_parity = if_else(Anzahl_vorausg_LebGeb == 0, "Primi", "Multi"))

## Overview
# summary(parity1)
# table(parity1$Anzahl_vorausg_SS) # 0-12
# table(parity1$Anzahl_vorausg_LebGeb) # 0-7
# table(parity1$Anzahl_fehl_Geb) # 0-12
# table(parity1$Anzahl_Interruptio) # 0-10

Sample5 <- left_join(Sample4, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)

# 3.4.5.3 Maternal age ----------------------------------------------------
Mother_data <- left_join(Sample5, Pat_info, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>% 
  select(patient_id_mother, case_id_mother, PAT_BIRTH_DATE, CBIS_BIRTH_DATE_TS, PAT_CITIZENSHIP_COUNTRY) # %>% 
  # distinct(patient_id_mother, .keep_all = TRUE)

Mother_data <- Mother_data %>%
  distinct(patient_id_mother, case_id_mother, .keep_all = TRUE) # remove duplicates, 6849

Mother_data <- Mother_data %>% 
  mutate(Birth_date = as.Date(CBIS_BIRTH_DATE_TS)) %>%
  mutate(RF_Maternal_age = interval(start = PAT_BIRTH_DATE, end = Birth_date) / duration(n = 1, unit = "years"))
summary(Mother_data$RF_Maternal_age)
# Min. 1st Qu.  Median    Mean   3rd Qu.  Max. 
# 14.55   30.48   33.70   33.50   36.86   52.37

Underage <- Mother_data %>% 
  filter(RF_Maternal_age < 18) # 10 cases

Sample5b <- left_join(Sample5, Mother_data, by = c("patient_id_mother", "case_id_mother", "CBIS_BIRTH_DATE_TS")) %>% 
  select(- Birth_date, - PAT_BIRTH_DATE)


# Mother_data <- Mother_data %>%
#   distinct(patient_id_mother, case_id_mother, .keep_all = TRUE)

# Sample3 <- left_join(Sample2, Mother_age, by = c("patient_id_mother", "case_id_mother", "CBIS_BIRTH_DATE_TS")) %>% 
#   select(- Birth_date, - PAT_BIRTH_DATE)
# summary(Sample3$Maternal_age) # NAs= 606 
# 
# Sample4 <- left_join(Sample2, Mother_age, by = c("case_id_mother", "CBIS_BIRTH_DATE_TS")) %>% 
#   select(- Birth_date, - PAT_BIRTH_DATE)
# summary(Sample4$Maternal_age) # NAs= 606
# 
# Sample5 <- left_join(Sample2, Mother_age, by = c("CBIS_BIRTH_DATE_TS")) %>% 
#   select(- Birth_date, - PAT_BIRTH_DATE)
# summary(Sample5$Maternal_age) # NAs= 603


rm(Sample2, Sample2_isna_transfer, Sample3, Sample4, Underage, Parity)

# 3.4.5.4 Origin --------------------------------------------------
## From Luisa to start
Countries_FOS <- readxl::read_excel( "I:/Verwaltung/MaNtiS/01_Rohdaten/FOS_countries_categories.xlsx")
Countries_FOS <- janitor::clean_names(Countries_FOS) ## to clean column names
## Import adapated original data with correct country names for merging with FOS data
Countries1 <- read_csv( "I:/Verwaltung/MaNtiS/01_Rohdaten/Countries_categories.csv", show_col_types = F) # set `show_col_types = FALSE` to quiet the message

Countries_FOS1 <- Countries_FOS %>% 
  select(landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, iso2, iso3, de_kurzform, kontinent_continent_continente, region_region_regione)

Countries1 <- Countries1 %>% 
  select(PAT_CITIZENSHIP_COUNTRY, pat_citizenship_country_new, )

## Merge country names from data with country names from FOS
Countries2 <- left_join(Countries1, Countries_FOS1, by = c( "pat_citizenship_country_new" = "de_kurzform"))

## Build counties categories 
Countries2b <- Countries2 %>% 
  mutate(Country = case_when(
    pat_citizenship_country_new == "Schweiz" ~ "Switzerland",
    kontinent_continent_continente == 1 ~ "Europe",
    kontinent_continent_continente == 2 ~ "Africa",
    kontinent_continent_continente == 3 ~ "America",
    kontinent_continent_continente == 4 ~ "Asia",
    kontinent_continent_continente == 5 ~ "Oceania",
    TRUE ~ "Unknown")) # when NA then unknown

# Combine with Sample5b
Sample6 <- left_join(Sample5b, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) 

Sample6b <- left_join(Sample5b, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>%
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust,
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) %>%
  distinct(patient_id_child, .keep_all = TRUE)

table(Sample6b$Country)
# Africa     America      Asia      Europe     Oceania   Switzerland     Unknown 
# 262         187         503        2938          17        2955           17 

round(prop.table(table(as.factor(Sample6b$Country))) * 100, 1)
# Africa     America     Asia      Europe     Oceania   Switzerland   Unknown 
# 3.8         2.7         7.3       42.9         0.2        42.7         0.2

table(Sample6$ICD_labels) # overview ICD-codes to generate a list


# 3.4.5.5 RF Hypoglycaemia ---------------------------------------
# Diabetes_codes <- c("P70.0", "P70.1", "P70.8", "Z83.3")
# 
# Sample6_rf_hypoglyc <- Sample6 %>% 
#   mutate(RF_Hypoglyc = DIA_NK %in% Diabetes_codes) # identify RF for hypoglycaemia by searching for GDM/DM Diagnoses 
# 
# Sample6_rf_hypoglyc_s <- Sample6_rf_hypoglyc %>%
#   group_by(patient_id_child, case_id_child) %>%
#   summarise(RF_Hypoglyc = any(RF_Hypoglyc)) # 6903, per newborn
# 
# Sample6c <- left_join(Sample6, Sample6_rf_hypoglyc_s, 
#                      by = c("patient_id_child", "case_id_child")) %>% # merging both data sets together diagnose and RF hypoglyc
#                         relocate(Country, .after = case_id_child)
# 
# 
# # 3.4.5.6 RF Hyperbilirubinaemia ------------------------------------------
# ## ICD-10
# Hyperbilirubin_rf_codes <- c("P55.0", "P55.1", "D55.0")
# 
# Sample6_rf_hyperbili <- Sample6c %>% 
#   mutate(RF_hyperbili = DIA_NK %in% Hyperbilirubin_rf_codes)
# 
# Sample6_rf_hyperbili_s <- Sample6_rf_hyperbili %>% 
#   group_by(patient_id_child, case_id_child) %>% 
#   summarise(RF_hyperbili = any(RF_hyperbili))  # 6903
# 
# Sample6d <- left_join(Sample6c, Sample6_rf_hyperbili_s, by = c("patient_id_child", "case_id_child"))
# 
# ## GA
# Sample6d <- Sample6d %>% 
#   mutate(RF_Ghyperbili_GA = if_else(Weeks_LPM < 38, "Yes", "No"))


# Maternal Diagnoses ----------------------------------------------
Sample6e <- left_join(Sample6b, Diagnose_red2_corr, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>%
  select(- DIG_DATE_TS, - DIG_RANK, - DIG_TARGET_SITE, - DIA_BK, - ICD_groups) %>% 
  rename(DIA_NK_child = DIA_NK.x, ICD_labels_child = ICD_labels.x, DIA_NK_mat = DIA_NK.y, ICD_labels_mat = ICD_labels.y) %>% 
  relocate(DIA_NK_mat, ICD_labels_mat, .after = admission_neo_n) %>% 
  relocate(Country, .after = case_id_child)

## Hypoglycaemia (maternal diagnoses)
Maternal_RF_hypoglyc_codes <- c("O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")


## Identify risk factors pro explanatory variable (HHH) and diagnoses 
# Sample9 <- Sample6e %>%
#   group_by(patient_id_child, case_id_child) %>% # create per patient id and case id
#   summarise(Hypoglycaemia_present = any(DIA_NK_child %in% Hypoglycaemia_codes), # all three symptoms
#             Hyperbilirubinaemia_present = any(DIA_NK_child %in% Hyperbilirubinaemia_codes),
#             Hypothermia_present = any(DIA_NK_child %in% Hypothermia_codes))
#             
# 
#               high_birthweight = any(Birth_weight_g >= 4500), # general RF
#               maternal_diabetes = any(DIA_NK_mat %in% Maternal_RF_hypoglyc_codes),
#               maternalage = first(RF_Maternal_age), # "first(), takes first number of the grouped patient id/ case id
#               GA = any(Weeks_LPM < 38),
#               origin = first(Country),
#               parity = first(RF_parity),
#                 RF_hypoglyc_codes = any(DIA_NK_child %in% c("P70.0", "P70.1", "P70.8", "Z83.3")), # rsik factors hypoglyc
#                 Maternal_RF_hypoglyc_codes = any(DIA_NK_mat %in% c("O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")),
#                 RF_hyperbili_codes = any(DIA_NK_child %in% c("P55.0", "P55.1", "D55.0"))) %>%
#                   mutate(RF_origin = origin %in% c("Africa", "Asia"), # risk factor specifically
#                          RF_pariti = parity == "Multi") %>%
#   
#                            mutate(Risk_hypoglycaemia = RF_hypoglyc_codes | high_birthweight | maternal_diabetes | Hypothermia_present | RF_pariti, # specific for all HHH
#                                   Risk_hyperbilirubinaemia = RF_hyperbili_codes | GA | (maternal_diabetes & high_birthweight) | RF_origin,
#                                   Risk_hypothermia = Hypoglycaemia_present | RF_pariti | RF_origin,
#                                   HHH_combi = case_when(!Hypoglycaemia_present & !Hyperbilirubinaemia_present & !Hypothermia_present ~ "No_diagnosis",
#                                                         Hypoglycaemia_present & !Hyperbilirubinaemia_present & !Hypothermia_present ~ "Hypoglycaemia_only",
#                                                         !Hypoglycaemia_present & Hyperbilirubinaemia_present & !Hypothermia_present ~ "Hyperbilirubinaemia_only",
#                                                         !Hypoglycaemia_present & !Hyperbilirubinaemia_present & Hypothermia_present ~ "Hypothermia_only",
#                                                         Hypoglycaemia_present & Hyperbilirubinaemia_present & !Hypothermia_present ~ "Hypoglycaemia_Hyperbilirubinaemia",
#                                                         Hypoglycaemia_present & !Hyperbilirubinaemia_present & Hypothermia_present ~ "Hypoglycaemia_Hypothermia",
#                                                         !Hypoglycaemia_present & Hyperbilirubinaemia_present & Hypothermia_present ~ "Hyperbilirubinaemia_Hypothermia",
#                                                         Hypoglycaemia_present & Hyperbilirubinaemia_present & Hypothermia_present ~ "All_HHH",
#                                                         TRUE ~ "Unknown"))



# Code not needed ---------------------------------------------------------
# 
# ### Movement data old part
# summary(as.factor(Move_stat2f$pat_type))
# # newborn  parent 
# # 35049   47705 
# 
# Move_Newborn <- Move_stat2f %>%
#   filter(pat_type %in% "newborn") %>% 
#   filter(MOV_START_DATE_TS > "2019-01-01 00:00:00") %>%
#   filter(MOV_START_DATE_TS < "2023-01-01 00:00:00")     # select only newborns within time frame, 33711 obs
# 
# # Overview CAS_TYPE and Reasons
# summary(as.factor(Move_Newborn$CAS_TYPE))
# # Amb./Tageskli.      Stationär 
# # 1009                32702
# ## 280 via MUKI/SSA
# 
# summary(as.factor(Move_Newborn$MOV_REASON1))
# # Altersh./a.Inst andere/intÜbert  anderes Spital angemeldet,gepl      Geburt Krank/Pfegeheim      n.a.       Notfall Reha.Kli.and.B.       unbekannt  Verlegung <24h      verstorben 
# # 2             456            1512             574                     10769               1       10861         23               2              12              48             114 
# # Zuhause 
# # 9337 
# 
# summary(as.factor(Move_Newborn$MOV_REASON2))
# # amb.Beh.(HA,Pol amb.Pfl.(Spitex and. KH, Spital andere,N.Akut-F         anderes  geheilt/k.Beh.            n.a. Reha. (amb./st) stat.Beh./Pfleg       unbekannt      verstorben         Zuhause 
# #                          9041             231             415           10780              61              51           10861           1921              11             114             115 
# # Zuhause/Spitex 
# # 109
# summary(as.factor(Move_Newborn$ORG))
# # Chirurgie 4.1     FK Geburtsabteilung   FK Gyn. Bettenstation      FK Mutter und Kind FK Schwangerenabteilung          IMC 
# # 2                 12421                     9                       21265                      13                       1 
# 
# # 1. To exclude: cases verstorben (MOV_REASON1), and ambulante Geburt (CAS_TYPE)
# # Move_Newborn2 <- Move_Newborn %>% 
# #   filter(!CAS_TYPE %in% "Amb./Tageskli.") # - 1035 cases (excluded ambulant births)
# Move_Newborn2 <- Move_Newborn %>%
#   filter(!MOV_REASON1 %in% "verstorben") # - 114 cases
# ## With excluded cases: 33597 obs
# 
# # 2. Select newborns that were first transferred from labour ward to Muki
# ## Geburtsabteilung: 00002030
# ## Mutter und Kind Abtl.: 00005010
# 
# # Trans_muki <- Move_Newborn2 %>%
# #   filter(unit_id %in% c("00002030", "00005010")) # select only relevant units, 33573 (-24)
# # summary(as.factor(Trans_muki$ORG))
# 
# ## Aim: to have one row with all relevant information
# 
# # Check for missing data
# summary(Move_Newborn)
# sum(is.na(Move_Newborn$patient_id)) # 0
# sum(is.na(Move_Newborn$case_id)) # 0 
# sum(is.na(Move_Newborn$unit_id)) # 0
# sum(is.na(Move_Newborn$MOV_REASON1)) # 0
# sum(is.na(Move_Newborn$CAS_TYPE)) # 0
# 
# 
# Lookup_move <- Move_Newborn2 %>%
#   filter(unit_id %in% c("00002030", "00005010")) %>%
#   select(patient_id, case_id, ORG, unit_id, MOV_START_DATE_TS, MOV_END_DATE_TS, MOV_KIND, MOV_REASON1) %>%
#   distinct()
# 
# ## Identify transfer labour ward and mother and child unit (first approach)
# 
# # spread(Trans_muki, key = ORG, value = unit_id)
# # -> recommended to switch to pivot_wider(), which is easier to use, more featureful, and still under active development.
# # pivot_wider = widens data, increasing the number of columns and decreasing the number of rows
# 
# # names_from, values_from = A pair of arguments describing which column (or columns) to get the name of the output column (names_from), and which
# # column (or columns) to get the cell values from (values_from).
# 
# Trans_muki <- Lookup_move %>%
#   pivot_wider(
#     names_from = ORG, # FK Geburtsabteilung & FK Mutter und Kind
#     values_from = unit_id) # 00002030 & 00005010
# 
# ## Concl.: this creates columns for each unique value in the ORG column. With multiple entries I think is not suitable. 
# 
# ## Identify transfer labour ward and mother and child unit (second approach)
# 
# Trans_muki2 <- Lookup_move %>% 
#   group_by(patient_id, case_id) %>% 
#   mutate(Gebs = any(unit_id == "00002030"), 
#          Muki = any(unit_id == "00005010")) # -> False/True; to find a way, to identify the direction of transfers Gebs --> Muki
# 
# Trans_muki3 <- Trans_muki2 %>% 
#   group_by(patient_id, case_id) %>% 
#   mutate(Transfer_Gebs_Muki = any(Gebs & lead(Muki))) # "next" (lead()) values in a vector; NAs by Transfer_Gebs_Muki when Gebs True and Muki false
# 
# Trans_muki4 <- Lookup_move %>%
#   group_by(patient_id, case_id) %>%
#   mutate(Gebs = any(unit_id == "00002030"), 
#          Muki = any(unit_id == "00005010"), 
#          Transfer_Gebs_Muki = any(unit_id == "00002030" & 
#                                     lead(unit_id) == "00005010")) # NAs by Transfer_Gebs_Muki when Gebs True and Muki false again. 
# 
# Trans_muki5 <- Lookup_move %>%
#   group_by(patient_id, case_id) %>%
#   mutate(
#     Gebs = any(unit_id == "00002030"), 
#     Muki = any(unit_id == "00005010"), 
#     Unit_next = lead(unit_id), # create a next column with next unit, any and lead in one line was the problem
#     Direct_transfer = (unit_id == "00002030" & Unit_next == "00005010"),  # give the direction
#     Transfer_Gebs_Muki = any(Direct_transfer, na.rm = TRUE))
# ## Here we see if a newborn as transferred from gebs to muki (Transfer_Gebs_Muki True or not); but there are sometimes multiple entries per patient 
# 
# Trans_muki6 <- Lookup_move %>%
#   group_by(patient_id, case_id) %>%
#   mutate(
#     Gebs = any(unit_id == "00002030"), 
#     Muki = any(unit_id == "00005010"), 
#     Unit_next = lead(unit_id),
#     Direct_transfer = (unit_id == "00002030" & Unit_next == "00005010"),  
#     Transfer_Gebs_Muki = any(Direct_transfer, na.rm = TRUE)) %>% 
#   ungroup() %>% # because of group_by!
#   select(patient_id, case_id, Gebs, Muki, Transfer_Gebs_Muki) %>% 
#   distinct() # to get one row of each case
# 
# table(Trans_muki6$Transfer_Gebs_Muki)
# # FALSE  TRUE 
# # 2158   9281
# ## Concl.: I have now 11439 cases, 9281 cases are transfers from Gebs to Muki directly. The other newborns were either not born at USB or these are readmission 
# 

# # Merging Sample1 (cases with consent or NA, >=37 GA)
# Sample3 <- left_join(Sample2, Trans_muki6, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   select(- Gebs, -Muki)
# 
# table(Sample2$Transfer_Gebs_Muki)
# # FALSE  TRUE 
# # 300  8163 
# table(is.na(Sample2$Transfer_Gebs_Muki))
# # FALSE  TRUE 
# # 8463     1 --> patient 83022??!
# ### Movement data old part
# summary(as.factor(Move_stat2f$pat_type))
# # newborn  parent 
# # 35049   47705 
# 
# Move_Newborn <- Move_stat2f %>%
#   filter(pat_type %in% "newborn") %>% 
#   filter(MOV_START_DATE_TS > "2019-01-01 00:00:00") %>%
#   filter(MOV_START_DATE_TS < "2023-01-01 00:00:00")     # select only newborns within time frame, 33711 obs
# 
# # Overview CAS_TYPE and Reasons
# summary(as.factor(Move_Newborn$CAS_TYPE))
# # Amb./Tageskli.      Stationär 
# # 1009                32702
# ## 280 via MUKI/SSA
# 
# summary(as.factor(Move_Newborn$MOV_REASON1))
# # Altersh./a.Inst andere/intÜbert  anderes Spital angemeldet,gepl      Geburt Krank/Pfegeheim      n.a.       Notfall Reha.Kli.and.B.       unbekannt  Verlegung <24h      verstorben 
# # 2             456            1512             574                     10769               1       10861         23               2              12              48             114 
# # Zuhause 
# # 9337 
# 
# summary(as.factor(Move_Newborn$MOV_REASON2))
# # amb.Beh.(HA,Pol amb.Pfl.(Spitex and. KH, Spital andere,N.Akut-F         anderes  geheilt/k.Beh.            n.a. Reha. (amb./st) stat.Beh./Pfleg       unbekannt      verstorben         Zuhause 
# #                          9041             231             415           10780              61              51           10861           1921              11             114             115 
# # Zuhause/Spitex 
# # 109
# summary(as.factor(Move_Newborn$ORG))
# # Chirurgie 4.1     FK Geburtsabteilung   FK Gyn. Bettenstation      FK Mutter und Kind FK Schwangerenabteilung          IMC 
# # 2                 12421                     9                       21265                      13                       1 
# 
# # 1. To exclude: cases verstorben (MOV_REASON1), and ambulante Geburt (CAS_TYPE)
# # Move_Newborn2 <- Move_Newborn %>% 
# #   filter(!CAS_TYPE %in% "Amb./Tageskli.") # - 1035 cases (excluded ambulant births)
# Move_Newborn2 <- Move_Newborn %>%
#   filter(!MOV_REASON1 %in% "verstorben") # - 114 cases
# ## With excluded cases: 33597 obs
# 
# # 2. Select newborns that were first transferred from labour ward to Muki
# ## Geburtsabteilung: 00002030
# ## Mutter und Kind Abtl.: 00005010
# 
# # Trans_muki <- Move_Newborn2 %>%
# #   filter(unit_id %in% c("00002030", "00005010")) # select only relevant units, 33573 (-24)
# # summary(as.factor(Trans_muki$ORG))
# 
# ## Aim: to have one row with all relevant information
# 
# # Check for missing data
# summary(Move_Newborn)
# sum(is.na(Move_Newborn$patient_id)) # 0
# sum(is.na(Move_Newborn$case_id)) # 0 
# sum(is.na(Move_Newborn$unit_id)) # 0
# sum(is.na(Move_Newborn$MOV_REASON1)) # 0
# sum(is.na(Move_Newborn$CAS_TYPE)) # 0


 1. Install packages ------------------------------------------------------
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("feather")
library(feather)

# 2. Import data set --------------------------------------------------------
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2_reduced.RData")
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2_2025-03-06.RData")

# Meona_corr2_reduced: Exclude 1) nicht am USB geboren wurden 2) readmissions waren 3) verstorben sind

## Check id
# patient_id
# sum(is.na(Meona_corr2$patient_id)) # there are no NAs
# # case_id
# sum(is.na(Meona_corr2$case_id))
# # patient_id
# sum(is.na(Meona5$patient_id)) # there are no NAs
# # case_id
# sum(is.na(Meona5$case_id))

# 3. Overview Meona data ---------------------------------
# 3.1 New_meona -----------------------------------------------------------
New_meona <- Meona5 # save data set in new data frame; 309128 obs
summary(New_meona) # overview
ID_new_meona <- New_meona %>% 
  select(patient_id, case_id) %>% 
  distinct() # 9260

# patient_id
sum(is.na(New_meona$patient_id)) # there are no NAs

# case_id
sum(is.na(New_meona$case_id)) 
# there are no NAs

# To exclude the cases without case_id
# New_meona <- New_meona %>% 
#   filter(!is.na(case_id)) # 320'936 obs
# sum(is.na(New_meona$case_id))

## Reviewing of the individual variables 
# pat_geb_dat
sum(is.na(New_meona$pat_geb_dat)) # no NAs
summary(New_meona$pat_geb_dat)
# #                      Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
# "2019-01-01 00:00:00.0000" "2019-12-14 00:00:00.0000" "2020-12-30 00:00:00.0000" "2020-12-26 13:08:17.6889" "2021-12-26 00:00:00.0000" "2022-12-31 00:00:00.0000"

# Creating new data set with only newborns born between 1.1.19-31.12.22
# New_meona2 <- New_meona %>% 
#   filter(pat_geb_dat > "2019-01-01 00:00:00") %>%
#   filter(pat_geb_dat < "2023-01-01 00:00:00")  
# summary(New_meona2$pat_geb_dat)
# excluded cases born before 1.1.19 (324264)

# alter_in_tagen
summary(New_meona$alter_in_tagen)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.000   1.000   1.000   1.677   2.000     53.000 
table(New_meona$alter_in_tagen)
# cases with longer stay often rehosp and as Begleitperson

# order_text
summary(as.factor(New_meona$order_text))
# Bilirubin [mmol/L] Blutzucker [mmol/L]  Gewicht [kg]       Trinkmahlzeit 
# 23635               13112               35052              237329 

# app_comment
summary(as.factor(New_meona$app_comment))
# different documentation in words to specify; NA's: 314710 

# app_date_doku
summary(New_meona$app_date_doku)
# Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
# "2019-01-01 03:35:00.0000" "2019-12-15 14:03:00.0000" "2021-01-01 02:34:00.0000" "2020-12-28 17:09:25.3945" "2021-12-28 08:01:15.0000" "2022-12-31 23:47:00.0000" 

# dokumentation
summary(as.factor(New_meona$dokumentation))
# documentation of weights (numeric) and bili values (numeric) as well as detailed information regarding feeding patterns

# parameter
# form_summary

# breast_feeding_amount
summary(New_meona$breast_feeding_amount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   15.00   25.00   28.73   40.00 2100.00  301500 

# pump_milk
summary(as.factor(New_meona$pump_milk))
# True   NA's 
#  15500 293628 

# pump_milk_amount
summary(New_meona$pump_milk_amount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    5.00   10.00   16.35   22.00  360.00  293022 

# breast_feeding_add_food
summary(as.factor(New_meona$breast_feeding_add_food))
# True   NA's 
# 94463 214665

# breast_feeding_ad_libitum
summary(as.factor(New_meona$breast_feeding_ad_libitum))
# True   NA's 
# 168544 140584

# breast_feeding_massage
# breast_feeding_counter

# additional_feeding_indication
summary(as.factor(New_meona$additional_feeding_indication))
# CHILDREN_BILI   CHILDREN_DISEASE CHILDREN_OPERATION CHILDREN_PREMATURE       CHILDREN_SGA      HYPOGLYCAEMIA     MOTHER_DISEASE  MOTHER_MEDICATION      NO_INDICATION              OTHER     WEIGHT_LOSS_10 
# 290                798                 11               1756               2783               1011               9348                 77              39777              37958                494 
# NA's 
#             214825  

# breast_drinking_habbits
summary(as.factor(New_meona$breast_drinking_habits))
# CLUSTER_FEEDING DENY     INEFFECTIVE     NORMAL    SHORT_STRONG    NA's 
# 314              3668     5852           70891      9932          218471 

# breast_feeding_aids
summary(as.factor(New_meona$breast_feeding_aids))
# B = Becher, F = Flasche, Fi = Finger, L = Löffel, H = Habermann?, S = Stillhüttchen?

# blood_sugar_measurement_type
summary(as.factor(New_meona$blood_sugar_measurement_type))


# bilirubin_type
summary(as.factor(New_meona$bilirubin_type))
# SERUM TRANSCUTAN     NA's 
# 2226      21409     285493

## Conclusion: many variables are no longer useful/important for the further progress, as there are also many NAs what makes it difficult to handle;
## many variables will be excluded

# 3.2 New_meona2 ----------------------------------------------------------
New_meona2 <- New_meona %>%
  filter(!order_text %in% c("Trinkmahlzeit", "Gewicht [kg]")) %>% # exclude Trinkmahlzeit and Gewicht (Gewicht aus birth_corr2)
  mutate(dokumentation = as.numeric(dokumentation)) # dokumentation as numeric; 36747 obs

# Check for duplicates
sum(duplicated(New_meona2)) # 250
Duplicate_rows <- New_meona2 %>%
  filter(duplicated(New_meona2)) # identify which rows are duplicated
New_meona_c <- distinct(New_meona2) 
## Conclusion: Duplicates due to double entries, the reason cannot be determined (e.g., error when entering)-> removed duplicates, 36497 obs

# Check ID
ID_check_new_meona_c <- New_meona_c %>% 
  select(patient_id, case_id) %>% 
  distinct()
# 8609
ID_check_new_meona_c_case <- New_meona_c %>% 
  select(case_id) %>% 
  distinct()
# 8609
ID_check_new_meona_c_patient <- New_meona_c %>% 
  select(patient_id) %>% 
  distinct()
# 8609

rm(New_meona, New_meona2, Duplicate_rows, ID_check_new_meona_c, ID_check_new_meona_c_case, ID_check_new_meona_c_patient)

# 4. Operationalisation ---------------------------------------------------
# 4.1 Blood glucose -----------------------------
summary(as.factor(New_meona_c$order_text))
# Bilirubin [mmol/L] Blutzucker [mmol/L] 
# 23578               12919              

## To identify cases with and without blood sugar measures
# Lookup_bloodmeasure <- New_meona_c %>% 
#   count(patient_id, case_id, order_text, dokumentation, name = "counts") # all values in dokumentation/order_text

Bloodmeasure_y <- New_meona_c %>% 
  filter(order_text == "Blutzucker [mmol/L]") %>%
  distinct(patient_id) %>%
  count() # 3896

Bloodmeasure_n <- New_meona_c %>% 
  filter(!order_text == "Blutzucker [mmol/L]") %>%
  distinct(patient_id) %>%
  count() # 8296

# Identify blood glucose measurements, ID check and Hypoglycaemia No/Yes
Lookup_hypoglyc <- New_meona_c %>% 
  filter(order_text %in% "Blutzucker [mmol/L]") %>% 
  select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation, app_date_doku) 
## 12919; selection variables per patient filtered only by order text Blutzucker  

Lookup_hypoglyc2 <- Lookup_hypoglyc %>% 
  group_by(patient_id, case_id) %>% 
  mutate(Hypoglycaemia = if_else(dokumentation >= 2.6, "0", "1")) %>% 
  mutate(Hypoglycaemia_severe = if_else(dokumentation >= 2.0, "0", "1"))

Lookup_hypoglyc2 <- Lookup_hypoglyc2 %>%
  mutate(Hypoglycaemia_severe = as.numeric(Hypoglycaemia_severe)) %>% 
  mutate(Hypoglycaemia = as.numeric(Hypoglycaemia))
                                 
summary(as.factor(Lookup_hypoglyc2$Hypoglycaemia))
# 0     1 
# 12235 684
summary(as.factor(Lookup_hypoglyc2$Hypoglycaemia_severe))
# 0     1 
# 12763 156 

# CATEGORISATION 1 - Without bilirubin values (fertige Liste mit Birthcorr_2!)
Hypoglyc_cat <- New_meona_c %>%
  select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation) %>% 
  filter(!order_text %in% "Bilirubin [mmol/L]") %>%
  group_by(patient_id, case_id) %>%
  filter(alter_in_tagen <= 3) %>%
  mutate(Hypoglycaemia_category = case_when(
    dokumentation < 2.0 ~ "Severe", 
    dokumentation < 2.6 ~ "Mild",
    dokumentation >= 2.6 ~ "Normoglycaemic")) %>%
  mutate(Summ = if_else(Hypoglycaemia_category %in% "Normoglycaemic", 0,
                       if_else(Hypoglycaemia_category %in% "Mild", 1, 3))) %>% 
  mutate(Sum_Category = sum(Summ)) %>%
  mutate(Hypoglycaemia_cat = if_else(Sum_Category == 0, "Normoglycaemic",
                                     if_else(Sum_Category == 1, "Mild",
                                             if_else(Sum_Category == 2, "Moderate", "Severe"))))
## Hypoglycaemia_cat: this is the final code

# CATEGORISATION 2 - Alternative coding
Hypoglyc_cat2 <- Lookup_hypoglyc2 %>%
  group_by(patient_id, case_id) %>%
  select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation, Hypoglycaemia, Hypoglycaemia_severe) %>%
  filter(alter_in_tagen <= 3) %>%
  group_by(patient_id, case_id) %>%
  summarise(Episode_Hypoglycaemia = sum(Hypoglycaemia),
            Episode_Hypoglycaemia_severe = sum(Hypoglycaemia_severe)) %>%
  mutate(Category_Hypoglycaemia = case_when(
      Episode_Hypoglycaemia_severe >= 1 ~ "Severe",    
      Episode_Hypoglycaemia == 1 ~ "Mild",             
      Episode_Hypoglycaemia == 2 ~ "Moderate",         
      Episode_Hypoglycaemia > 2 ~ "Severe",            
      Episode_Hypoglycaemia == 0 ~ "Normoglyc"))

# Overview
summary(Hypoglyc_cat)
table(Hypoglyc_cat$Hypoglycaemia_cat)
# Mild      Moderate   Normoglycaemic     Severe 
# 1713        417           9614           1149
table(Hypoglyc_cat$alter_in_tagen)
# 0    1    2    3 
# 5927 5350 1447  169

# To have one row per patient with the hypoglycaemia category
Hypoglyc_data_cat <- Hypoglyc_cat %>% 
  group_by(patient_id, case_id) %>% 
  select(patient_id, case_id, pat_geb_dat, alter_in_tagen, Hypoglycaemia_cat) %>% 
  distinct(patient_id, .keep_all = TRUE) # 3890 cases; .keep.all = keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.

# Percentages 
table(as.factor(Hypoglyc_data_cat$Hypoglycaemia_cat))
# Mild       Moderate Normoglycaemic         Severe 
# 275             49           3410            156 

round(prop.table(table(as.factor(Hypoglyc_data_cat$Hypoglycaemia_cat))) * 100, 1)
# Mild       Moderate     Normoglycaemic      Severe 
# 7.1            1.3           87.7            4.0

## For analysis: Hypoglycaemia_cat

## Identify cases WITHOUT any blood sugar measurement
# anti_join: to find newborns that are in Sample2 but not in Hypoglyc_data_cat (which has not had a blood glucose measure?)
No_measure_cases <- anti_join(Sample2, Hypoglyc_data_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) # 4312

# Date format has to be changed (to merge later properly)
No_measure_cases <- No_measure_cases %>%
  mutate(pat_geb_dat = as.Date(CBIS_BIRTH_DATE_TS)) %>%
  select(-CBIS_BIRTH_DATE_TS)

# Add new category "No_measure"
No_measure_cases <- No_measure_cases %>%
  rename(patient_id = patient_id_child, case_id = case_id_child) %>%
  mutate(Hypoglycaemia_cat = "No_measure")

# Combine together Hypoglyc_data_cat and No_measure_cases
Hypoglyc_data_cat2 <- bind_rows(Hypoglyc_data_cat, No_measure_cases) # combines the two data sets vertically; all other variables before NAs..
Hypoglyc_data_cat2 <- Hypoglyc_data_cat2 %>%
  select(patient_id, case_id, pat_geb_dat, alter_in_tagen, Hypoglycaemia_cat) # select needed variables

summary(as.factor(Hypoglyc_data_cat2$Hypoglycaemia_cat))
# Mild       Moderate     No_measure Normoglycaemic         Severe 
# 275             49           5264           3410            156 

round(prop.table(table(as.factor(Hypoglyc_data_cat2$Hypoglycaemia_cat))) * 100, 1)
# Mild       Moderate     No_measure    Normoglycaemic    Severe 
# 3.0            0.5          57.5         37.3            1.7 

## Next step: merging Hypoglyc_data_cat2 with Sample2 (attention: without variable birth date! It doesn't work with these variables, 
## pat_geb_dat is also a little different than CBIS_BIRTH_DATE_TS)
Sample2b <- left_join(Sample2, Hypoglyc_data_cat2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(- pat_geb_dat)

table(Sample2b$Hypoglycaemia_cat)
# Mild       Moderate     No_measure Normoglycaemic         Severe 
# 177             28           4312           2289             97 
round(prop.table(table(as.factor(Sample2b$Hypoglycaemia_cat))) * 100, 1)
# Mild       Moderate     No_measure     Normoglycaemic     Severe 
# 2.6            0.4           62.5           33.2            1.4 

Hypoglyc_neo <- Sample2b %>% 
  filter(admission_neo %in% "Yes", Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe")) # 95 cases, without "No_measure"

table(Hypoglyc_neo$Hypoglycaemia_cat)
# Mild    Moderate Normoglycaemic     Severe 
# 4         2             61            28 

round(prop.table(table(Hypoglyc_neo$Hypoglycaemia_cat)) * 100, 1)
# Mild       Moderate     Normoglycaemic    Severe 
# 4.2            2.1           64.2           29.5

Hypoglyc_neo2 <- Sample2b %>% 
  filter(admission_neo %in% "Yes", Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe", "No_measure")) # 155 cases, with "No_measure"
table(Hypoglyc_neo2$Hypoglycaemia_cat)
# Mild       Moderate     No_measure Normoglycaemic         Severe 
# 4              2            60             61             28 

round(prop.table(table(Hypoglyc_neo2$Hypoglycaemia_cat)) * 100, 1)
# Mild       Moderate     No_measure Normoglycaemic         Severe 
# 2.6            1.3           38.7           39.4            18.1 

  
# 4.2 Bilirubin ----------------------------------------------------
# To identify measurement or no measurement
Bilimeasure_y <- New_meona_c %>% 
  filter(order_text == "Bilirubin [mmol/L]") %>%
  distinct(patient_id) %>%
  count() # 8296

Bilimeasure_n <- New_meona_c %>% 
  filter(!order_text == "Bilirubin [mmol/L]") %>%
  distinct(patient_id) %>%
  count() # 3896

# Identify bilirubin data
Lookup_bili <- New_meona_c %>% 
  filter(order_text %in% "Bilirubin [mmol/L]") %>% 
  select(patient_id, case_id, birth_time, alter_in_tagen, order_text, dokumentation, bilirubin_type, age_hours_measure, measurement_time) 

str(Lookup_bili$age_hours_measure) # in minutes, chr mins included -> change in hours and as numeric for next steps needed
Lookup_bili <- Lookup_bili %>% 
  mutate(Age_in_hours = age_hours_measure / 60) %>% 
  mutate(Age_in_hours = as.numeric((Age_in_hours)))

# Recalculation mg/dl «–» µmol/l: Bilirubin 1mg/dl=17.1µmol/l
Lookup_bili <- Lookup_bili %>% 
  mutate(Value_umol_l = dokumentation * 17.1)
# nicht runden!

# ID check Lookup_bili
ID_check_b <- Lookup_bili %>% 
  select(patient_id, case_id) %>% 
  distinct()
# 8296
ID_check_case_b <- Lookup_bili %>% 
  select(case_id) %>% 
  distinct()
# 8296
ID_check_patient_b <- Lookup_bili %>% 
  select(patient_id) %>% 
  distinct()
# 8296

rm(ID_check_b, ID_check_case_b, ID_check_patient_b)

# CATEGORISATION
## Build categories based on USB bilirubin threshold
Bili_cat_USB <- Lookup_bili %>% 
  mutate(Bili_threshold = case_when(Age_in_hours < 24 ~ 150,
                                    Age_in_hours < 48 ~ 250, 
                                    Age_in_hours < 72 ~ 300, 
                                    Age_in_hours >= 72 ~ 350)) %>% 
  mutate(FT = if_else(Value_umol_l >= Bili_threshold, "Yes", "No"))

# Categorisation according NICE Guideline
## Import NICE threshold table bili
Threshold_bili <- read.csv("//uhg/drives/UserProfile/waldispuehlp/RedirFolders/Documents/Thesis/PDF/Threshold_Bili2.csv", header = TRUE, sep = ",")

# Convert from integer to numeric for next steps
Threshold_bili <- Threshold_bili %>% 
  mutate_at(c('Age_in_hours', 'Bili_start_phototherapy_above', 'Bili_start_exchange_therapy_above'), as.numeric)
# mutate_at = allows to transform several variables simultaneously

Bili_cat_Nice <- Lookup_bili %>%
  rowwise() %>%
  mutate(Threshold_age = max(Threshold_bili$Age_in_hours[Threshold_bili$Age_in_hours <= Age_in_hours], na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(Threshold_bili, by = c("Threshold_age" = "Age_in_hours")) %>%
  mutate(Cat = case_when(
    Value_umol_l < Bili_start_phototherapy_above ~ "Jaundice",
    Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Treatment_req_Photo",
    Value_umol_l >= Bili_start_exchange_therapy_above ~ "Treatment_req_ExT"))
## Conclusion: either a therapy would have been theoretically necessary, but there was no entry,
## or there was a therapy according to the procedure documentation, but theoretically not necessary


## Join with Procedure data to compare (with USB guideline)
Bili_data_comp <- left_join(Bili_cat_USB, Procedure_red2_corr, by = c("patient_id",  "case_id")) %>%
  select(- CHOP_NK, - PRO_START_DATE_TS, - PRO_END_DATE_TS, - item.type, - codable, - CHOP_BK)

Bili_data_comp <- Bili_data_comp %>%
  mutate(Documented_Phototherape = if_else(procedure_label == "Sonstige Phototherapie", "Yes", "No")) %>% 
  filter(procedure_label == "Sonstige Phototherapie")

Matches <- Bili_data_comp %>%
  filter(FT %in% "Yes" & Documented_Phototherape %in% "Yes") # 41

## Join with Procedure data to compare (with USB Nice guideline)
Bili_data_comp2 <- left_join(Bili_cat_Nice, Procedure_red2_corr, by = c("patient_id",  "case_id")) %>%
  select(- CHOP_NK, - PRO_START_DATE_TS, - PRO_END_DATE_TS, - item.type, - codable, - CHOP_BK)

Bili_data_comp2 <- Bili_data_comp2 %>%
  mutate(Documented_Phototherape = if_else(procedure_label == "Sonstige Phototherapie", "Yes", "No")) %>% 
  filter(procedure_label == "Sonstige Phototherapie")

Matches2 <- Bili_data_comp2 %>%
  filter(Cat %in% "Treatment_req_Photo", Documented_Phototherape %in% "Yes") # 92
## in the case of an theoretical exchange transfusion, no entry in the procedure, but with a diagnosis of either ikterus or AB0 imm.


# 2. Import data sets --------------------------------------------------------
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr2.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/gestational_age_2024-10-01.RData")

# 3. Birth_corr2 -------------------------------------------------------------
## 10778 obs

summary(Birth_corr2) # quick overview of the data set

## patient_id_mother
sum(is.na(Birth_corr2$patient_id_mother)) # there are no NAs

## case_id_mother
sum(is.na(Birth_corr2$case_id_mother)) # there are no NAs

## patient_id_child
sum(is.na(Birth_corr2$patient_id_child)) # there are 2 NAs
na_id <- Birth_corr2 %>%
  filter(is.na(patient_id_child)) # 2, these two NAs are stillbirths (patient_id_mother 58056 & 63042)

## case_id_child
sum(is.na(Birth_corr2$case_id_child)) # there are 2 NAs
na_case <- Birth_corr2 %>%
  filter(is.na(case_id_child)) # like before, NAs are stillbirths 
Birth_corr2 <- Birth_corr2 %>%
  filter(!is.na(case_id_child)) # excluded cases without case_id, 10776 (-2)

## CBIS_BIRTH_DATE_TS (Birth date and time)
summary(Birth_corr2$CBIS_BIRTH_DATE_TS)
#Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
#"2019-01-01 01:26:00.0000" "2019-12-21 18:54:45.0000" "2021-01-05 15:17:00.0000" "2020-12-30 15:21:19.3272" "2021-12-24 07:42:30.0000" "2022-12-31 16:17:00.0000" 
# from 2019-01-01 01:26:00. to 2022-12-31 16:17:00
sum(is.na(Birth_corr2$CBIS_BIRTH_DATE_TS)) # no NAs

## CBIS_MULTIPLE_BIRTH_FLAG (multiple birth (several children), 1 if yes, otherwise 0)
summary(as.factor(Birth_corr2$CBIS_MULTIPLE_BIRTH_FLAG))
# 0      1 
# 10143   633 
# there are no NAs

## Stillbirths
table(Birth_corr2$CBIS_STILLBIRTH_FLAG) # 77

## CBIS_CONGENITAL_MALFORMATION (congenital malformation (1 = yes, 0 = no, (-1) = unknown))
table(Birth_corr2$CBIS_CONGENITAL_MALFORMATION)
# -1     0     1 
# 73 10083   620 
# there are no NAs

## CBIS_MODE (codes in (1,7) - exact decryption still unclear))
table(Birth_corr2$CBIS_MODE) 
# 1    2    3    4    6 n.a. 
# 5409  841 1066  103   48 3309 
## not relevant

## CBIS_WEIGHT
summary(Birth_corr2$CBIS_WEIGHT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 135    2930    3300    3219    3650    5900      44 

hist(Birth_corr2$CBIS_WEIGHT, main = "Histogram Weight Newborns (2019-2022)", 
     xlab = "weight (g)", ylab = "Freq")
# all newborns are here included, also the premature
(Summ_weight <- Birth_corr2 %>%
    summarize(mean_weight = mean(CBIS_WEIGHT, na.rm = TRUE),
              min_weight = min(CBIS_WEIGHT, na.rm = TRUE),
              max_weight = max(CBIS_WEIGHT, na.rm = TRUE)))
#         mean_weight min_weight max_weight
#           <dbl>      <dbl>      <dbl>
#  1       3219.        135       5900

## CBIS_WEIGHT_UNIT
summary(as.factor(Birth_corr2$CBIS_WEIGHT_UNIT)) # in g

## CBIS_BODY_SIZE
summary(Birth_corr2$CBIS_BODY_SIZE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 19.00   48.00   50.00   49.64   52.00   63.00      45 

hist(Birth_corr2$CBIS_BODY_SIZE, main = "Histogram Body Size Newborns (2019-2022)", 
     xlab = "size (cm)", ylab = "Freq")
(Summ_size_body <- Birth_corr2 %>%
    summarize(mean_size_body = mean(CBIS_BODY_SIZE, na.rm = TRUE),
              min_size_body = min(CBIS_BODY_SIZE, na.rm = TRUE),
              max_size_body = max(CBIS_BODY_SIZE, na.rm = TRUE)))
#       mean_size min_weight max_weight
#         <dbl>      <dbl>      <dbl>
#  1      49.6         19         63

## CIBS_BODY_SIZE_UNIT
summary(as.factor(Birth_corr2$CBIS_BODY_SIZE_UNIT)) # in cm

## CBIS_HEAD_SIZE
summary(Birth_corr2$CBIS_HEAD_SIZE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 12.00   33.00   34.00   34.02   35.00   53.00      57 

(Summ_size_head <- Birth_corr2 %>%
    summarize(mean_size_head = mean(CBIS_HEAD_SIZE, na.rm = TRUE),
              min_size_head = min(CBIS_HEAD_SIZE, na.rm = TRUE),
              max_size_head = max(CBIS_HEAD_SIZE, na.rm = TRUE)))
#             mean_size_head min_weight_head max_weight_head
#               <dbl>           <dbl>           <dbl>
#  1           34.0              12              53

## CIBS_HEAD_SIZE_UNIT
summary(as.factor(Birth_corr2$CBIS_HEAD_SIZE_UNIT)) # in cm

## CBIS_APGAR_SCORE_0Min
summary(Birth_corr2$CBIS_APGAR_SCORE_0MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   8.000   9.000   8.005   9.000  10.000    2597 

## CBIS_APGAR_SCORE_5Min
summary(Birth_corr2$CBIS_APGAR_SCORE_5MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   9.000   9.000   9.077  10.000  10.000    2605 

## CBIS_APGAR_SCORE_10Min
summary(Birth_corr2$CBIS_APGAR_SCORE_10MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000  10.000  10.000   9.599  10.000  10.000    2605 

## CBIS_COMMENT
summary(as.factor(Birth_corr2$CBIS_COMMENT)) 
# various specified comments, but not always -> variable will be deleted as not necessary and there are also person related information

## CBIS_CREATE_DATE (Date of creation of record) -> not needed, will be deleted
## CBIS_CREATE_DAY_BK (Date of creation of record in format YYYYMMDD) -> not needed, will be deleted
## CBIS_UPDATE_DATE (Date of update of record) -> not needed, will be deleted
## CBIS_UPDATE_DAY_BK (Date of update of record in format YYYYMMDD) -> not needed, will be deleted

# Check for id /cases
Check_id <- Birth_corr2 %>% 
  select(patient_id_child, case_id_child) %>% 
  distinct

Check_pat_id <- Birth_corr2 %>% 
  select(patient_id_child) %>% 
  distinct
## 10776 obs

Check_case_id <- Birth_corr2 %>% 
  select(case_id_child) %>% 
  distinct

# Check for duplicates
sum(duplicated(Birth_corr2)) # no duplicates

# How much NICU admission
Adm_neo <- Birth_corr2 %>% 
  filter(admission_neo %in% "Yes") # 1328 newborns have an admission to the Nicu

Birth_corr2 %>%
  summarise(total = n(), # n() gives the current group size
            admitted = sum(admission_neo == "Yes"),  # The syntax of the sum() function is = sum(x,na.rm=FALSE/TRUE); sum returns the sum of all the values present in its arguments
            percent = (admitted / total) * 100)
# total admitted percent
# <int>    <int>   <dbl>
#   1 10776     1328    12.3


# 4. Extraction Sample >=37 GA --------------------------------------------
## Identify sample GA >= 37 and admission to neo
Birth_corr3 <- left_join(Birth_corr2, gestational_age_final, by = c("patient_id_mother", "case_id_mother", "patient_id_child", "case_id_child"))
summary(Birth_corr3) # 10776

Check_pat_id_3 <- Birth_corr3 %>% 
  select(patient_id_child) %>% 
  distinct
## 10776 obs

Check_case_id_3 <- Birth_corr3 %>% 
  select(case_id_child) %>% 
  distinct
## 10776 obs

# Exclude NAs
# Birth_corr3_ga <- Birth_corr3 %>% 
#   filter(is.na(Birth_corr3$gestational_age_total_days)) # 428

Birth_corr3_ga <- Birth_corr3 %>% 
  filter(!is.na(Birth_corr3$gestational_age_total_days)) # all GAs, 10348 (-428)

Sample_Birth_corr <- Birth_corr3_ga %>% 
  filter(gestational_age_total_days >= "259") # only >=37 GA, 9046

Birth_corr3_ga %>%
  summarise(total = n(),
            GA_week37 = sum(gestational_age_total_days >= 259),
            percent = (GA_week37 / total) * 100)
# total GA_week37 percent
# <int>     <int>   <dbl>
#1 10348    9046    87.4

Adm_neo2 <- Sample_Birth_corr %>% 
  filter(admission_neo %in% "Yes") # 528

Sample_Birth_corr %>%
  summarise(total = n(), 
            admitted = sum(admission_neo %in% "Yes"), 
            percent = (admitted / total) * 100)
# total admitted percent
# <int>    <int>   <dbl>
# 9046      528    5.84

# 2. Import data set Temp data --------------------------------------------------------
Temp_data <- read_feather("I:/Verwaltung/MaNtiS/02_Pseudonymisierte_Daten/Temp_pseud.feather")

# 3. Overview Temp data --------------------------------------------------------
summary(Temp_data) # 211808
sum(is.na(Temp_data$patient_id)) # there are no NAs
sum(is.na(Temp_data$case_id)) # 15040

Temp_data2 <- left_join(Sample2, Temp_data, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) 
## Conclusion: 81809 obs; 
## Sample2: from data merged from Birth_corr2, Consent yes/NA and GA >= 37, Trans_muki6

summary(Temp_data2)

# Patient_id, case_id
sum(is.na(Temp_data2$patient_id_child)) # 0
sum(is.na(Temp_data2$case_id_child)) # 0

# Malformation
# summary(as.factor(Temp_data2$CBIS_CONGENITAL_MALFORMATION))
# # -1     0     1 
# # 214 81821  4603 

# VVMO_NUMERIC_VALUE
summary(Temp_data2$VVMO_NUMERIC_VALUE)
# Min.   1st Qu.  Median  Mean   3rd  Qu. Max.    NA's 
# 27.10   36.90   37.10   37.08   37.30   38.90   234  
## Conclusion: NAs were mainly due to transfers to the neo, readmission

# VVMO_MEASURE_DATE_TS
summary(Temp_data2$VVMO_MEASURE_DATE_TS)
# Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max.                       NA's 
# "2019-01-01 04:45:00.0000" "2019-12-21 01:44:00.0000" "2021-01-12 05:45:00.0000" "2020-12-28 19:57:50.8432" "2021-12-17 00:28:30.0000" "2022-12-31 08:56:00.0000"                      "234"  

## VIP_BK, VVMO_TEXT_VALU - these variables are not needed
# Temp_data2 <- Temp_data2 %>%
#   select(-VIP_BK, -VVMO_TEXT_VALUE)

# To exclude missing data in temperature records
Temp_data2 <- Temp_data2 %>% 
  filter(!is.na(VVMO_NUMERIC_VALUE)) # 81575

summary(Temp_data2)

# Check Id
Check_pat_id_temp <- Temp_data2 %>% 
  select(patient_id_child) %>% 
  distinct
## 8230 

Check_case_id_temp <- Temp_data2 %>% 
  select(case_id_child) %>% 
  distinct
## 8230

# Identify cases with neo admission
Temp_adm_neo <- Temp_data2 %>% 
  filter(admission_neo %in% "Yes") # 1494 newborns were admitted to the neo

Temp_data2 %>%
  summarise(total = n(), # n() gives the current group size
            admitted = sum(admission_neo %in% "Yes"),  # sum returns the sum of all the values present in its arguments
            percent = (admitted / total) * 100)
# total admitted percent
# <int>    <int>   <dbl>
#1 81575     1494    1.83

Temp_low <- Temp_data2 %>%
  filter(VVMO_NUMERIC_VALUE < 36.5) # 2804

Temp_low_adm <- Temp_data2 %>%
  filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes") # 101

Temp_low_adm2 <- Temp_data2 %>%
  filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes") %>% 
  select(patient_id_child) %>%
  distinct() # 60 

# 4. Operationalisation --------------------------------------------------
# CATEGORISATION
Temp_data_cat <- Temp_data2 %>%
  select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, CON_VALUE, admission_neo, admission_neo_n, Weeks_LPM, 
         gestational_age_total_days, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS, Transfer_Gebs_Muki) %>% 
  group_by(patient_id_child, case_id_child) %>%
  mutate(Hypothermia_category = case_when(
    VVMO_NUMERIC_VALUE >= 36.5 ~ "Normotherm",
    VVMO_NUMERIC_VALUE >= 36.0 & VVMO_NUMERIC_VALUE < 36.5 ~ "Mild", 
    VVMO_NUMERIC_VALUE < 36.0 ~ "Moderate_Severe")) %>% 
  mutate(cat = if_else(Hypothermia_category %in% "Normotherm", 0,
                       if_else(Hypothermia_category %in% "Mild", 1, 2))) %>% 
  mutate(Sum_Category = sum(cat)) %>% 
  mutate(Hypothermia_cat = if_else(Sum_Category == 0, "Norm",
                                   if_else(Sum_Category == 1, "Mild", "Moderate_Severe")))


## Explanation: Temperatur-Kategorien in numerische Variable zu überführen, damit man sie später einfacher zusammenfassen kann
# Normotherm → 0
# (Keine Hypothermie, normale Temperatur)
# Milde Hypothermie → 1
# (Leichte Abweichung von der Normaltemperatur)
# Moderate oder schwere Hypothermie → 2
# (Deutlich niedrigere Körpertemperatur) 

# To have one row per patient with the hypothermia cat
Temp_data_cat2 <- Temp_data_cat %>%
  group_by(patient_id_child, case_id_child) %>%
  select(patient_id_child, case_id_child, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS, Hypothermia_cat) %>% 
  distinct(patient_id_child, .keep_all = TRUE) # 8680; .keep.all = keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.

# Percentages and neo admission 
table(as.factor(Temp_data_cat2$Hypothermia_cat))
# Mild        Moderate_Severe     Norm 
# 1209             691            6330 

round(prop.table(table(as.factor(Temp_data_cat2$Hypothermia_cat))) * 100, 1)
# Mild       Moderate_Severe      Norm 
# 14.7             8.4            76.9 

# Temp_data_neo <- Temp_data_cat2 %>%
#   filter(admission_neo %in% "Yes")  # 218

# table(Temp_data_neo$Hypothermia_cat)
# # Mild      Moderate_Severe     Norm 
# # 32              28             158 

# round(prop.table(table(Temp_data_neo$Hypothermia_cat)) * 100, 1)
# # Mild     Moderate_Severe        Norm 
# # 14.7            12.8            72.5 

## For analysis: Hypothermia_cat


# Sample6 -----------------------------------------------------------------
## Join with Sample2
Sample6 <- left_join(Sample2, Temp_data_cat2, by = c("patient_id_child", "case_id_child"))

table(Sample6$Hypothermia_cat)
# Mild      Moderate_Severe     Norm 
# 1209             691          6330 

round(prop.table(table(as.factor(Sample6$Hypothermia_cat))) * 100, 1)
# Mild     Moderate_Severe      Norm 
# 14.7            8.4           76.9  

Hypotherm_neo <- Sample6 %>% 
  filter(admission_neo %in% "Yes", Hypothermia_cat %in% c("Norm", "Mild", "Moderate_Severe")) # 218

table(Hypotherm_neo$Hypothermia_cat)
# Mild        Moderate_Severe   Norm 
# 32              28             158 

round(prop.table(table(Hypotherm_neo$Hypothermia_cat)) * 100, 1)
# Mild        Moderate_Severe     Norm 
# 14.7            12.8            72.5

## To merge with Sample3 (overview with icds)
Sample7 <- left_join(Sample3, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) 

Hypotherm_newborn <- Sample7 %>%
  filter(admission_neo %in% "Yes") %>% 
  filter(Transfer_Gebs_Muki == TRUE) %>% 
  filter(DIA_NK %in% c("P80.9", "P80.8")) %>% 
  distinct() # 22

table(Hypotherm_newborn$Hypothermia_cat)
# Mild      Moderate_Severe     Norm 
# 5              14               3 
round(prop.table(table(Hypotherm_newborn$Hypothermia_cat)) * 100, 1)
# Mild      Moderate_Severe       Norm 
# 22.7            63.6            13.6 



# 2. Data -----------------------------------------------------------------
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr1.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Diagnose_red2_corr.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/DRG_red1.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Move_stat2f.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Pat_info.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Procedure_red2_corr.RData")


# 3. Identify newborn dataset --------------------------------------------
Neo <- Move_stat2f %>%
  filter(pat_type %in% "newborn")
## 35,049 observations


# 3.1 NICU admissions -----------------------------------------------------

## How to identify children that were transferred to NICU?
## It seems they are discharged from the hospital data.
Neo_discharge <- Neo %>%
  filter(MOV_TYPE_ID %in% 2)
## 11,885 observations (dataset Neo)

table(Neo_discharge$MOV_REASON1)
# Altersh./a.Inst andere/intÜbert  anderes Spital Krank/Pfegeheim            n.a. Reha.Kli.and.B.      verstorben         Zuhause 
#               2             463            1573               1               3               2             118            9723 

table(Neo_discharge$MOV_REASON2)
# amb.Beh.(HA,Pol amb.Pfl.(Spitex         anderes  geheilt/k.Beh.            n.a. Reha. (amb./st) stat.Beh./Pfleg       unbekannt      verstorben 
#            9411             244              73              53               3               1            1977               5             118 

### Selection of newborns that were first transferred to postnatal unit and then transferred
Neo_Muki <- Neo %>%
  filter(unit_id %in% "00005010") #%>%
# select(patient_id, case_id) %>%
# distinct()
## 22,117 observations, 10.349 cases

Neo_discharge <- Neo_Muki %>%
  filter(MOV_TYPE_ID %in% 2)
## 10,322 observations (dataset Neo_Muki), some cases lost because not discharged until time frame finished

# Neo_discharge_missing <- setdiff(Neo_Muki$case_id, Neo_discharge$case_id)

table(Neo_discharge$MOV_REASON1)
# Altersh./a.Inst andere/intÜbert  anderes Spital Reha.Kli.and.B.         Zuhause 
#               2             318             371               1            9630

table(Neo_discharge$MOV_REASON2)
# amb.Beh.(HA,Pol amb.Pfl.(Spitex         anderes  geheilt/k.Beh. Reha. (amb./st) stat.Beh./Pfleg       unbekannt 
#            9339             238              52              39               1             652               1 


## Select target time frame
Neo_discharge_corr <- Neo_discharge %>%
  filter(MOV_START_DATE_TS > "2019-01-01 00:01:00") %>%
  filter(MOV_START_DATE_TS < "2023-01-06 00:01:00") #%>%
# select(patient_id, case_id) %>%
# distinct()
## 11,457 cases (neo)
## 9,944 cases (neo_muki)


Neo_discharge_hosp <- Neo_discharge_corr %>%
  filter(!MOV_REASON1_ID %in% "2|01") %>% ## (discharged home)
  filter(!MOV_REASON1_ID %in% "2|00") ##  (died)
## 1981 observations (total)
## Muki 671  observations neo

Neo_discharge_hosp_cases <- Neo_discharge_hosp %>%
  select(patient_id, case_id)
## 1981 observations

Neo_hosp_birth <- left_join(Neo_discharge_hosp_cases, Birth_corr1, by = c("patient_id"="patient_id_child", "case_id"="case_id_child"))
## 22 Neonates without birth data, birth outside hospital


Neo_hosp_info <- left_join(Neo_discharge_hosp_cases, Pat_info, by = c("patient_id", "case_id"))

Neo_hosp_ICD <- left_join(Neo_discharge_hosp_cases, Diagnose_red2_corr, by = c("patient_id", "case_id"))
## 293 cases without ICD diagnoses

Neo_hosp_CHOP <- left_join(Neo_discharge_hosp_cases, Procedure_red2_corr, by = c("patient_id", "case_id"))

Neo_hosp_DRG <- left_join(Neo_discharge_hosp_cases, DRG_red1, by = c("patient_id", "case_id"))



# 3.1.1 NICU admission after transfer to postpartum unit-------------------------------------------------------------------
## Trying to identify "healthy babies"
## Exclusion of all birth with direct transfer to NICU (without movment to postpartum unit)

Neo_hosp_mov <- left_join(Neo_discharge_hosp_cases, Move_stat2f)

Neo_hosp_mov_muki <- Neo_hosp_mov %>%
  filter(unit_id %in% "00005010") %>%
  select(patient_id, case_id) %>%
  distinct()
## 671 observations (2 outside time frame)
Neo_hosp_info2 <- left_join(Neo_hosp_mov_muki, Pat_info, by = c("patient_id", "case_id"))



# 3.1.1.1 Hypoglycaemia ---------------------------------------------------
Neo_hypogly <- Neo_hosp_info2 %>%
  group_by(case_id) %>%
  filter(any(DIA_NK %in% "P70.4")) %>% # Hypoglykämie
  filter(any(DIA_NK %in% "P80.8"))  # Hypothermie











