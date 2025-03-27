## My thesis
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
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2_reduced.RData")

# 2.b Pseudonymised data sets: Consent 
Consent <- read_feather("I:/Verwaltung/MaNtiS/02_Pseudonymisierte_Daten/Consent_pseud.feather")
Temp_data <- read_feather("I:/Verwaltung/MaNtiS/02_Pseudonymisierte_Daten/Temp_pseud.feather")


# 3. Birth information ----------------------------------------------------
# 3.1 Birth_corr2 ------------------------------------
summary(Birth_corr2)

## Time frame check (Inclusion criteria 1)
summary(Birth_corr2$CBIS_BIRTH_DATE_TS)
# Min.                       1st Qu.                    Median                     Mean                       3rd Qu.                    Max. 
# "2019-01-01 01:26:00.0000" "2019-12-22 02:57:00.0000" "2021-01-06 00:12:30.0000" "2020-12-31 05:41:56.7116" "2021-12-25 17:35:00.0000" "2022-12-31 16:17:00.0000" 
# Conclusion: no exclusions

## patient_id_child
# sum(is.na(Birth_corr2$patient_id_child)) # there are 2 NAs
# ## case_id_child
# sum(is.na(Birth_corr2$case_id_child)) # there are 2 NAs
# na_case <- Birth_corr2 %>%
#   filter(is.na(case_id_child)) # stillbirths 

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
  filter(!is.na(gestational_age_total_days)) # 8950
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

rm(Lookup1, Lookup1_case_id, Check_case_id, Check_id, Check_weight, Consent_check, na_case, Consent_check, Consent_check2, Consent_exclude_child, Consent_exclude_mother, Check_stillbirth, Adm_nicu)

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

rm(Check_id_sample2_child, Check_id_sample2_child2, Check_id_sample2_mo, Check_id_sample2_mo2, Sample1, Labour_dis, Labour_dis_nicu, Sample2_isna_transfer)


# 3.4.4 Neonatal diagnoses -------------------------------------------------

## Include codes without unsure diagnoses:
# Include_codes <- c("P55.1", "R63.4", "Z83.4", "Z83.5", "P54.6", "H11.3", "P83.4", "L81.3", "U07.2", "Z83.3", "Z38.0", "P92.0", "P83.1", "L53.0", "R25.3", "P05.2", 
#                    "R14", "P05.1", "P05.0", "K21.9", "P12.1", "P12.9", "P15.3", "P15.4", "Q54.0", "D18.01", "D18.05", "P78.2", "P54.5", "P39.4", "K40.90", "K42.9", 
#                    "R05", "Q54.9", "P80.9", "P12.0", "P39.1", "Z84.3", "P90", "Z84.1", "P83.9", "R22.0", "R22.2", "R22.3", "R59.0", "D22.6", "D22.5", "D22.3", 
#                    "D22.9", "Q70.2", "P08.2", "D48.5", "P58.1", "P58.8", "P59.8", "P59.9", "U99.0", "Q53.1", "Z24.6", "P38", "T14.03", "L05.9", "Q18.1", "Z81", 
#                    "P92.1", "P55.0", "P04.0", "P02.7", "P03.3", "P03.0", "P00.0", "P01.2", "P01.3", "P03.4", "P02.5", "P02.1", "P03.1", "P04.1", "P03.8", "P02.2", 
#                    "P02.6", "P04.2", "P01.1", "P03.2", "P92.5", "P92.8", "Q54.8", "P08.1", "P13.1", "P12.8", "P14.3", "D23.4", "D23.9", "R01.2", "P70.4", "P80.8", 
#                    "R39.1", "P51.8", "R76.8", "H21.8", "H11.8", "H02.8", "H57.8", "R68.8", "P15.8", "Z84.8", "P83.8", "L81.8", "P81.8", "R19.88", "P72.8", "S09.8", 
#                    "P96.8", "K00.8", "P94.8", "R23.8", "K09.8", "N83.2", "R39.8", "R29.8", "X59.9", "Q17.3", "Z11", "U99.0", "P81.9", "K00.6", "P70.1", "P70.0", 
#                    "R00.0", "R00.1", "M43.6", "P70.9", "P92.2", "P08.0", "P12.4", "R60.0", "R69", "W64.9", "Z04.3", "R23.4", "T81.2", "P96.3", "L22", "Z38.3", 
#                    "Z38.5", "Y69", "E16.2", "P59.0", "P59.8.")

## Include codes with unsure diagnoses:
# Include_codes2 <- c("P55.1", "R63.4", "Z83.4", "Z83.5", "P54.6", "H11.3", "P83.4", "L81.3", "U07.2", "Z83.3", "Z38.0", "P92.0", "P83.1", "L53.0", "R25.3", "P05.2", 
#                     "R14", "P05.1", "P05.0", "K21.9", "P12.1", "P12.9", "P15.3", "P15.4", "Q54.0", "D18.01", "D18.05", "P78.2", "P54.5", "P39.4", "K40.90", "K42.9", 
#                     "R05", "Q54.9", "P80.9", "P12.0", "P39.1", "Z84.3", "P90", "Z84.1", "P83.9", "R22.0", "R22.2", "R22.3", "R59.0", "D22.6", "D22.5", "D22.3", 
#                     "D22.9", "Q70.2", "P08.2", "D48.5", "P58.1", "P58.8", "P59.8", "P59.9", "U99.0", "Q53.1", "Z24.6", "P38", "T14.03", "L05.9", "Q18.1", "Z81", 
#                     "P92.1", "P55.0", "P04.0", "P02.7", "P03.3", "P03.0", "P00.0", "P01.2", "P01.3", "P03.4", "P02.5", "P02.1", "P03.1", "P04.1", "P03.8", "P02.2", 
#                     "P02.6", "P04.2", "P01.1", "P03.2", "P92.5", "P92.8", "Q54.8", "P08.1", "P13.1", "P12.8", "P14.3", "D23.4", "D23.9", "R01.2", "P70.4", "P80.8", 
#                     "R39.1", "P51.8", "R76.8", "H21.8", "H11.8", "H02.8", "H57.8", "R68.8", "P15.8", "Z84.8", "P83.8", "L81.8", "P81.8", "R19.88", "P72.8", "S09.8", 
#                     "P96.8", "K00.8", "P94.8", "R23.8", "K09.8", "N83.2", "R39.8", "R29.8", "X59.9", "Q17.3", "Z11", "U99.0", "P81.9", "K00.6", "P70.1", "P70.0", 
#                     "R00.0", "R00.1", "M43.6", "P70.9", "P92.2", "P08.0", "P12.4", "R60.0", "R69", "W64.9", "Z04.3", "R23.4", "T81.2", "P96.3", "L22", "Z38.3", 
#                     "Z38.5", "Y69", "E16.2", "P59.0", "P59.8", "Q83.3", "Q69.2", "P83.5", "Q82.5", "Q38.1", "R34", "P21.9", "P22.9", "Z03.8", "Z03.3", "Z03.5", "Q21.2", 
#                     "P12.2", "P91.1", "R50.80", "R50.9", "P15.5"," P13.8", "R01.1", "P29.1", "G93.0", "P39.9", "Z86.1", "Z83.1", "P39.2", "P20.1", "P20.9", "P05.9", "Z29.0",
#                     "Z20.6", "Z20.8", "Z20.5", "Z86.2", "Z83.2", "Z86.7", "P21.1", "Q61.4", "Q25.0", "P29.3", "Q66.2"," Q66.4", "Q66.0", "P61.1", "Q66.8", "Q67.4", "Q10.3", 
#                     "P22.8", "R76.8", "H21.8", "P54.8", "P61.8", "P39.8", "P61.8", "P39.8", "K06.8", "L98.8", "N28.8", "H61.8", "P78.8", "P28.8", "R06.88", "R50.88", "P28.9", 
#                     "P28.8", "Q70.9", "P22.1", "P61.0", "R25.1", "Q21.0", "Q21.1")

## Diagnoses to exclude due to inclusion criteria:
## H27.9, Q24.9, Q04.0, Q62.0, Q62.2, Z82, U07.1, P76.9, Z38.1, B96.2, P50.1, Q36.9, E84.1, Q02,
## Q60.0, P21.0, P28.4, I78.8, Q89.8, Q82.8, Q15.8, Q64.8, Q17.8, I63.8, P76.8, Q35.3, B95.6, M54.2, P28.2

## Diagnoses included (due to HHH-RF or otherwise not recognised not healthy newborn at birth: 
## P55.1, R63.4, Z83.4, Z83.5, P54.6, H11.3, P83.4, L81.3, U07.2, Z83.3, Z38.0, P92.0, P83.1, L53.0, R25.3, P05.2, R14, P05.1, P05.0,
## K21.9, P12.1, P12.9, P15.3, P15.4, Q54.0, D18.01, D18.05, P78.2, P54.5, P39.4, K40.90, K42.9, R05, Q54.9, P80.9, P12.0, P39.1, Z84.3,
## P90, Z84.1, P83.9, R22.0, R22.2, R22.3, R59.0, D22.6, D22.5, D22.3, D22.9, Q70.2, P08.2, D48.5, P58.1, P58.8, P59.8, P59.9, U99.0, Q53.1,
## Z24.6, P38, T14.03, L05.9, Q18.1, Z81, P92.1, P55.0, P04.0, P02.7, P03.3, P03.0, P00.0, P01.2, P01.3, P03.4, P02.5, P02.1, P03.1, P04.1,
## P03.8, P02.2, P02.6, P04.2, P01.1, P03.2, P92.5, P92.8, Q54.8, P08.1, P13.1, P12.8, P14.3, D23.4, D23.9, R01.2, P70.4, P80.8, R39.1, P51.8,
## R76.8, H21.8, H11.8, H02.8, H57.8, R68.8, P15.8, Z84.8, P83.8, L81.8, P81.8, R19.88, P72.8, S09.8, P96.8, K00.8, P94.8, R23.8, K09.8, N83.2,
## R39.8, R29.8, X59.9, Q17.3, Z11, U99.0, P81.9, K00.6, P70.1, P70.0, R00.0, R00.1, M43.6, P70.9, P92.2, P08.0, P12.4, R60.0, R69, W64.9, Z04.3,
## R23.4, T81.2, P96.3, L22, Z38.3, Z38.5, Y69, E16.2, P58.1, P58.8, P59.0, P59.8., P59.9

## Diagnoses unsure:
## Q83.3, Q69.2, P83.5, Q82.5, Q38.1, R34, P21.9, P22.9, Z03.8, Z03.3, Z03.5, Q21.2, P12.2, P91.1, R50.80, R50.9, R01.0,
## P15.5, P13.8, R01.1, P29.1, G93.0, P39.9, Z86.1, Z83.1, P39.2, P20.1, P20.9, P05.9, Z29.0, Z20.6, Z20.8, Z20.5, Z86.2, Z83.2, Z86.7,
## P21.1, Q61.4, Q25.0, P29.3, Q66.2, Q66.4, Q66.0, P61.1, Q66.8, Q67.4, Q10.3, P22.8, R76.8, H21.8, P54.8, P61.8, P39.8, P61.8, P39.8,
## K06.8, L98.8, N28.8, H61.8, P78.8, P28.8, R06.88, R50.88, P28.9, P28.8, Q70.9, P22.1, P61.0, R25.1, Q21.0, Q21.1, "R93.4", "R93.1", 
## "R93.0", "R93.3", "R94.8", "R94.3", "P39.3", "K13.2", "P29.8"


## To identify newborns without illnesses or congenital malformations; merge Sample2 with Diagnose dataset, with relvant variables selected
Sample2_dia_neonatal <- left_join(Sample2, Diagnose_red2_corr, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, DIA_NK, ICD_labels)

Exclude_codes <- c("R93.4", "R93.1", "R93.0", "R93.3", "R94.8", "R94.3", "H27.9", "Q24.9", "Q04.0", "Q62.0", "P83.5", "Q62.2", "R34", "P21.9", "P22.9",
                   "Z82", "U07.1", "P76.9", "Z38.1", "B96.2", "P50.1", "Q54.9", "P39.9", "Z86.1", "P39.2", "Z86.2", "Z86.7", "P21.1", "Q36.9", "E84.1",
                   "Q02", "Q60.0", "Q61.4", "Q53.1", "P21.0", "P28.4", "I78.8", "Q89.8", "Q82.8", "Q15.8", "Q64.8", "Q17.8", "P39.8", "I63.8", "P76.8",
                   "Q35.3", "B95.6", "M54.2", "P28.2")

# # Identify newborns and how much cases (option 1, with exclusion icd-10-codes)
Neonates_exclude <- Sample2_dia_neonatal %>%
  filter(DIA_NK %in% Exclude_codes) %>%
  distinct(patient_id_child, case_id_child, DIA_NK) # 183 cases

# # Go back to the sample, to exclude the cases with diagnoses to exclude, I prefer to take anti_join
Sample2 <- anti_join(Sample2, Neonates_exclude, by = c("patient_id_child", "case_id_child")) # 6662
summary(Sample2)


## HHH DIAGNOSES
# 1. Hypoglycaemia: P70.4, E16.2
# 2. Hyperbilirubinaemia: P58.1, P58.8, P59.0, P59.8., P59.9
# 3. Hypothermia: P80.8, P80.9, P81.8, P81.9

# ICD-10 codes
## Diagnoses
Hypoglycaemia_codes <- c("P70.4", "E16.2")
Hyperbilirubinaemia_codes <- c("P58.1", "P58.8", "P59.0", "P59.8", "P59.9")
Hypothermia_codes <- c("P80.8", "P80.9", "P81.8", "P81.9")

# ICD codes
## RF
Hypoglycaemia_risk_codes <- c("P70.0", "P70.1", "P70.8", "Z83.3")
Hyperbilirubinaemia_rik_codes <- c("P55.0", "P55.1", "D55.0")



# 3.4.4.1 Hypoglycaemia categories ----------------------------------------
source("~/Thesis/Code/01_03_02_Code_Newborn_Meona.R")

summary(as.factor(Sample2c$Hypoglycaemia_cat))
# Mild       Moderate     No_measure  Normoglycaemic         Severe 
# 163             24           4230           2154             91 

# 3.4.4.2 Hyperbilirubinaemia categories ----------------------------------------

summary(as.factor(Sample2c$Hyperbili_cat))
# Hyperbili         No_measurement     Physiological_jaundice 
# 53                    644                   5965
## Conclusion: random check showed that there are cases with no documentation of bilirubin levels - so it was summarised that these would indicate also no measurement during hospital stay

# 3.4.4.3 Hypothermia categories ----------------------------------------
source("~/Thesis/Code/01_07_Code_Temp data.R")

summary(as.factor(Sample2c$Hypothermia_cat))
# Mild      Moderate_Severe     Norm          NA's 
# 952             520            5179         11  

Missing_temp <- Sample2c %>% 
  filter(is.na(Hypothermia_cat))
## 4 cases yes Nicu admission (2 of them stayed for one hour in the postnatal unit), 6 of them ambulant/tagesklinik, 1 child no data entry

 Sample2c <- Sample2c %>% 
   mutate(Hypothermia_cat = replace_na(Hypothermia_cat, "No_measurement"))

## Identify newborns only einling, geburt im KH and relevant icd codes related to HHH
HHH_codes <- c(Hypoglycaemia_codes, Hyperbilirubinaemia_codes, Hypothermia_codes)
HHH_risk_codes <- c(Hypoglycaemia_risk_codes, Hyperbilirubinaemia_rik_codes)

# To identify newborn only with diagnosis "Einling, Geburt im Krankenhaus"
Newborn_Z38.0 <- Sample2_dia_neonatal %>%
  filter(DIA_NK == "Z38.0") %>% 
  distinct(patient_id_child, case_id_child) # 6674

# To identify newborn only with diagnosis "Einling, Geburt im Krankenhaus" and HHH diagnoses and rf
Newborn_red <- Sample2_dia_neonatal %>%
  filter(DIA_NK == "Z38.0" | DIA_NK %in% HHH_codes | DIA_NK %in% HHH_risk_codes) %>% 
  distinct(patient_id_child, case_id_child) # 6712

Sample_test <- left_join(Sample2c, Newborn_red, by = c("patient_id_child", "case_id_child")) # 6662

Id_check_Sample_test <- Sample_test %>%
  select(patient_id_child, case_id_child) %>%
  distinct() # 6662
  
 
# 3.4.4.4 Overview HHH  ---------------------------------------------------
## To have an overview if a child has none, one, two or all HHH diagnoes
# With Test sample
Sample_test2 <- Sample_test %>%
  rowwise() %>%  # to not sum all values in the whole column
  mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
         hyperbili_y = if_else(is.na(Hyperbili_cat), FALSE, Hyperbili_cat == "Hyperbili"),
         hypotherm_y = if_else(is.na(Hypothermia_cat), FALSE, Hypothermia_cat %in% c("Mild", "Moderate_Severe")),
         symptom_count = sum(c(hypoglyc_y, hyperbili_y, hypotherm_y), na.rm = TRUE),
         HHH_diagnoses = case_when(
           symptom_count == 0 ~ "None",
           symptom_count == 1 & hypoglyc_y ~ "Hypoglyc_only",
           symptom_count == 1 & hyperbili_y ~ "Hyperbili_only",
           symptom_count == 1 & hypotherm_y ~ "Hypotherm_only",
           symptom_count == 2 & hypoglyc_y & hyperbili_y ~ "Hypoglyc_Hyperbili",
           symptom_count == 2 & hypoglyc_y & hypotherm_y ~ "Hypoglyc_Hypothermia",
           symptom_count == 2 & hyperbili_y & hypotherm_y ~ "Hyperbili_Hypothermia",
           symptom_count == 3 ~ "All_diagnoses",
           TRUE ~ "Unknown"))

Sample_test2 <- Sample_test2%>% 
  select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)

# Sample3 <- Sample2c %>%
#   rowwise() %>%  # to not sum all values in the whole column
#   mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
#          hyperbili_y = if_else(is.na(Hyperbili_cat), FALSE, Hyperbili_cat == "Hyperbili"),
#          hypotherm_y = if_else(is.na(Hypothermia_cat), FALSE, Hypothermia_cat %in% c("Mild", "Moderate_Severe")),
#          symptom_count = sum(c(hypoglyc_y, hyperbili_y, hypotherm_y), na.rm = TRUE),
#          HHH_diagnoses = case_when(
#            symptom_count == 0 ~ "None",
#            symptom_count == 1 & hypoglyc_y ~ "Hypoglyc_only",
#            symptom_count == 1 & hyperbili_y ~ "Hyperbili_only",
#            symptom_count == 1 & hypotherm_y ~ "Hypotherm_only",
#            symptom_count == 2 & hypoglyc_y & hyperbili_y ~ "Hypoglyc_Hyperbili",
#            symptom_count == 2 & hypoglyc_y & hypotherm_y ~ "Hypoglyc_Hypothermia",
#            symptom_count == 2 & hyperbili_y & hypotherm_y ~ "Hyperbili_Hypothermia",
#            symptom_count == 3 ~ "All_diagnoses",
#            TRUE ~ "Unknown"))
# 
# Sample3 <- Sample3 %>% 
#   select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)


# 3.4.5 Risk factors HHH ------------------------------------------------------

# 1. Hypoglycaemia: P70.0, P70.1, P70.8, Z83.3, birth weight > 4500g, hypothermia, Parity
# 2. Hyperbilirubinaemia: P55.0, P55.1, D55.0, (race/ethnicity), (maternal age), GA, GDM/DM, macrosomic infant (= birth weight >4500g) of a diabetic mother
# 3. Hypothermia: hypoglycaemia, Race/ethnicity, maternal age, parity

# Sample3 <- Sample3 %>% 
#   mutate(RF_weight = if_else(Birth_weight_g >= 4500, "Yes", "No"))

# 3.4.5.1 Parity ----------------------------------------------------------

Parity <- parity1 %>% 
  mutate(RF_parity = if_else(Anzahl_vorausg_LebGeb == 0, "Primi", "Multi"))

## Overview
# summary(parity1)
# table(parity1$Anzahl_vorausg_SS) # 0-12
# table(parity1$Anzahl_vorausg_LebGeb) # 0-7
# table(parity1$Anzahl_fehl_Geb) # 0-12
# table(parity1$Anzahl_Interruptio) # 0-10

# Merge with Sample
Sample_test3 <- left_join(Sample_test2, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)

# 3.4.5.2 Maternal age ----------------------------------------------------
Mother_data <- left_join(Sample_test, Pat_info, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>% 
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

# Sample_test3 <- left_join(Sample_test, Mother_data, by = c("patient_id_mother", "case_id_mother", "CBIS_BIRTH_DATE_TS")) %>% 
#   select(- Birth_date, - PAT_BIRTH_DATE)
# 
# summary(Sample_test3$RF_Maternal_age)
# # Min.     1st Qu.  Median  Mean   3rd Qu.  Max.      NA's 
# #   14.55   30.50   33.73   33.52   36.87   52.37      54 

Sample_test4 <- left_join(Sample_test2, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)
summary(Sample_test4$RF_Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.49   33.73   33.52   36.87   52.37


rm(Underage, Parity)

# 3.4.5.3 Origin --------------------------------------------------
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

# Combine with Sample
Sample_test5 <- left_join(Sample_test4, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) 

table(Sample_test5$Country)
# Africa     America        Asia      Europe     Oceania Switzerland     Unknown 
# 255         182         484        2872          13        2840          16 
round(prop.table(table(as.factor(Sample_test5$Country))) * 100, 1)
# Africa     America        Asia      Europe     Oceania  Switzerland     Unknown 
# 255         182         484        2872          13        2840          16 


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


# 3.4.6 Maternal Diagnoses ----------------------------------------------
## To exclude maternal risk factors for hypoglycaemia (and hyperbilirubinaemia)
Sample_maternal_rf <- left_join(Sample_test5, Diagnose_red2_corr, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>%
  select(- DIG_DATE_TS, - DIG_RANK, - DIG_TARGET_SITE, - DIA_BK, - ICD_groups) %>% 
  rename(DIA_NK_maternal= DIA_NK, ICD_labels_maternal = ICD_labels) %>%
  relocate(DIA_NK_maternal, ICD_labels_maternal, .after = admission_neo_n) %>%
  relocate(Country, .after = case_id_child) 

## ICD-10
Maternal_RF_hypoglyc_codes <- c("O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")
Maternal_RF_hyperbili_codes <- "D55.0"

Maternal_dia_RF <- Sample_maternal_rf %>%
  group_by(patient_id_mother, case_id_mother) %>%
  filter(DIA_NK_maternal %in% Maternal_RF_hypoglyc_codes | DIA_NK_maternal %in% Maternal_RF_hyperbili_codes) %>%
  select(DIA_NK_maternal, ICD_labels_maternal) # 912

summary(as.factor(Maternal_dia_RF$ICD_labels_maternal))

Sample_test_final <- left_join(Sample_test5, Maternal_dia_RF, by = c("patient_id_mother", "case_id_mother"))
Sample_test_final2 <- Sample_test_final %>% 
  filter(!DIA_NK_maternal %in% Maternal_RF_hypoglyc_codes) %>% 
  filter(!DIA_NK_maternal %in% Maternal_RF_hyperbili_codes) %>% 
  select(- DIA_NK_maternal, - ICD_labels_maternal) # 5789


# 4. Overview sample ------------------------------------------------------
# NICU admission
# Percentages Admission NICU
Sample_test_final2 %>%
  ungroup() %>% 
  summarise(total = n(),
            admitted = sum(admission_neo_n %in% 1),
            percent = (admitted / total) * 100)
# total admitted percent
# <int>    <int>   <dbl>
# 5789       82    1.42

table(Sample_test_final2$HHH_diagnoses)
# Hyperbili_Hypothermia       Hyperbili_only    Hypoglyc_Hyperbili    Hypoglyc_Hypothermia      Hypoglyc_only        Hypotherm_only       None 
# 9                           32                1                     74                        109                  1180                 4384 
table(Sample_test_final2$Hypothermia_cat)
# Mild      Moderate_Severe    No_measurement         Norm 
# 814       449                9                      4517
table(Sample_test_final2$Hypoglycaemia_cat)
# Mild      Moderate     No_measure     Normoglycaemic   Severe 
# 105       15           4217           1388             64 
table(Sample_test_final2$Hyperbili_cat)
# Hyperbili       No_measurement      Physiological_jaundice 
# 42              580                 5167 

Sample_test_final2 %>%
  group_by(Hypothermia_cat, Hypoglycaemia_cat, Hyperbili_cat) %>%
  summarise(n_total = n(),
    n_admitted = sum(admission_neo_n == 1),
    percent = (n_admitted / n_total) * 100)





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
# Adm_nicu2 <- Sample2 %>%
#   select(patient_id_child, case_id_child, admission_neo, admission_neo_n) %>%
#   filter(admission_neo %in% "Yes") %>%
#   distinct() # 155 cases where admitted from postnatal unit to the NICU


# 5. Overview Meona data ---------------------------------
# 5.1 New_meona -----------------------------------------------------------
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

# 5.2 New_meona2 ----------------------------------------------------------
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

# 6. Operationalisation ---------------------------------------------------
# 6.1 Blood glucose -----------------------------
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
  select(- pat_geb_dat, - alter_in_tagen)

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


# 6.2 Bilirubin ----------------------------------------------------
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

# CATEGORISATION (Guideline USB)
## Build categories based on USB bilirubin threshold
Bili_cat_USB <- Lookup_bili %>% 
  mutate(Bili_threshold = case_when(Age_in_hours < 24 ~ 150,
                                    Age_in_hours < 48 ~ 250, 
                                    Age_in_hours < 72 ~ 300, 
                                    Age_in_hours >= 72 ~ 350)) %>% 
  mutate(Treatment_required_Hyperbili = if_else(Value_umol_l >= Bili_threshold, "Yes", "No"))

# Check regarding bilirubin type (TCB, Serum)
Bili_cat_USBb <- Bili_cat_USB %>%
  mutate(Bili_levels = case_when(
    bilirubin_type == "TRANSCUTAN" & Value_umol_l < Bili_threshold ~ "bili_tc_norm",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_threshold ~ "bili_tc_photo",
    bilirubin_type == "SERUM" & Value_umol_l < Bili_threshold ~ "bili_serum_norm",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_threshold ~ "bili_serum_photo"))

table(Bili_cat_USBb$Bili_levels)
# bili_serum_norm   bili_serum_photo     bili_tc_norm    bili_tc_photo 
# 2198              26                   21267           87 

# There are NAs, I want to build a new variable (check the distribution with the actual sample)
Sample2c <- left_join(Sample2b, Bili_cat_USB, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, bilirubin_type, Value_umol_l, Treatment_required_Hyperbili) %>% 
  mutate(bilirubin_type = if_else(is.na(bilirubin_type), "No_measure", bilirubin_type), # NA = no measurement, to remove NA
         Value_umol_l = if_else(is.na(Value_umol_l), 0, Value_umol_l),
         Treatment_required_Hyperbili = if_else(is.na(Treatment_required_Hyperbili), "No_measure", Treatment_required_Hyperbili))
 
## now there are sometimes several bili measurement entries per newborn --> generate one row + build a new variable and summarise it to a category varaible to better merge 
Hyperbilirubinaemia_cat <- Bili_cat_USB %>% 
  group_by(patient_id, case_id) %>% 
  summarise(Hyperbili_cat = if_else(any(Treatment_required_Hyperbili == "Yes"), "Hyperbili", "Physiological_jaundice"))

# CATEGORISATION (NICE Guideline)
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
  mutate(Bili_cat = case_when(
    Value_umol_l < Bili_start_phototherapy_above ~ "Jaundice",
    Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Treatment_req_Photo",
    Value_umol_l >= Bili_start_exchange_therapy_above ~ "Treatment_req_ExT"))

# Check regarding bilirubin type (TCB, Serum)

Bili_cat_Niceb <- Bili_cat_Nice %>% 
mutate(Bili_levels = case_when(
    bilirubin_type == "TRANSCUTAN" & Value_umol_l < Bili_start_phototherapy_above ~ "Bili_tc_norm",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Bili_tc_photo",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_start_exchange_therapy_above ~ "Bili_tc_bloodtrans",
    bilirubin_type == "SERUM" & Value_umol_l < Bili_start_phototherapy_above ~ "Bili_serum_norm",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Bili_serum_photo",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_start_exchange_therapy_above ~ "Bili_serum_bloodtrans"))
table(Bili_cat_Niceb$Bili_levels)
# Bili_serum_bloodtrans       Bili_serum_norm     Bili_serum_photo    Bili_tc_bloodtrans   Bili_tc_norm       Bili_tc_photo 
# 2                           2163                59                  10                   21103              241

# Join with Procedure data to compare (with USB Nice guideline)
Bili_comb_procedure <- left_join(Bili_cat_USBb, Procedure_red2_corr, by = c("patient_id", "case_id"), relationship = "many-to-many") %>%
  select(patient_id, case_id, Treatment_required_Hyperbili, Bili_levels, procedure_label)

# Overview of the combinations
(Photo_USB_comb <- Bili_comb_procedure %>%
  mutate(Phototherapy_ind_usb = case_when(
      Bili_levels %in% c("bili_tc_photo", "bili_serum_photo") ~ TRUE, TRUE ~ FALSE), # if bili_tc_photo OR bili_serum_photo value is TRUE, for all other cases TRUE set the value to FALSE
      Phototherapy_doc = case_when(procedure_label %in% "Sonstige Phototherapie" ~ TRUE, TRUE ~ FALSE)) %>%
      group_by(Phototherapy_ind_usb, Phototherapy_doc) %>%
  summarise(Neonates_count = n_distinct(patient_id))) # counts the unique newborns for each group combinations
# Phototherapy_ind_usb   Phototherapy_doc          Neonates_count
# <lgl>                  <lgl>                     <int>
# 1 FALSE                FALSE                     8214
# 2 FALSE                TRUE                       82
# 3 TRUE                 FALSE                      53
# 4 TRUE                 TRUE                       27

# Identification cases normally out of treatment threshold
Bili_comb_procedure2 <- Bili_comb_procedure %>%
  filter(Bili_levels %in% c("bili_tc_norm", "bili_serum_norm") & procedure_label == "Sonstige Phototherapie") %>%
  select(patient_id, case_id, Bili_levels, procedure_label) %>% 
  distinct(patient_id, case_id, .keep_all = T) # 82 cases

## Conclusion: compared USB and NICE Guideline there are more children who needed therapy (according the guideline) when practiced with the NICE guideline.
## Moreover, either a therapy would have been theoretically necessary, but there was no entry in process data,
## or there was a therapy according to the procedure documentation, but theoretically not necessary



# Codes not needed --------------------------------------------------------

# Lookup_hypoglyc_1 <- Lookup_hypoglyc %>%
#   group_by(patient_id, case_id) %>% 
#   mutate(Bloodsugar_measurement = if_else(order_text == "Blutzucker [mmol/L]", "Yes", "No")) 

# Lookup_hypoglyc2 <- Lookup_hypoglyc %>% 
#   mutate(Hypoglycaemia = if_else(dokumentation >= 2.6, "No", "Yes",
#                                  if_else(dokumentation >= 2.0, "No", "Yes"))) 
# Identify blood glucose measurements frequency (with summarise and .groups = "drop")
# Lookup_hypoglyc3 <- Lookup_hypoglyc2 %>%
#   group_by(patient_id, case_id, pat_geb_dat, alter_in_tagen, Hypoglycaemia) %>% 
#   summarise(count = n(), .groups = "drop") 

# Identify blood glucose measurements frequency, with hypoglycaemia definition
# Lookup_hypoglyc4 <- Lookup_hypoglyc %>%
#   mutate(Hypoglycaemia = if_else(dokumentation >= 2.6, "No", "Yes")) %>% 
#   group_by(patient_id, case_id) %>% 
#   summarise(count_measurement = n(), .groups = "drop") %>% 
#   mutate(Category = case_when(count_measurement == 1 ~ "once",
#                               count_measurement == 2 ~ "twice",
#                               count_measurement > 2 ~ "multiple"))
## does not work

## Next step: further categorisation: mild, moderate, severe, no hypo based Hypoglycaemia
# Lookup_hypoglyc5 <- Lookup_hypoglyc %>%
#   mutate(Hypoglycaemia = if_else(dokumentation >= 2.6, "No", "Yes")) %>% 
#   group_by(patient_id, case_id) %>% 
#   summarise(count_measurement = n(), .groups = "drop") %>% 
#   Measurement_Hypo = sum(Hypoglycaemia) %>% 
#   mutate(Hypo_category = case_when(Measurement_Hypo == 0 ~ "Normoglycaemic",
#                                  Measurement_Hypo == 1 ~ "Mild",
#                                  Measurement_Hypo == 2 ~ "Moderate",
#                                  Measurement_Hypo > 2 ~ "Severe"))


# Categorising blood sugar values
# Hypoglycaemia <- New_meona_c %>%
#   mutate(Blood_sugar_category = case_when(
#     order_text == "Blutzucker [mmol/L]" & dokumentation >= 2.6 ~ "normal",
#     order_text == "Blutzucker [mmol/L]" & dokumentation < 2.6 ~ "hypo"))

# Identify blood glucose measurements frequency (with count())
## number of measurements per day counted together per patient based on order text Blutzucker
# Lookup_hypoglyc3 <- Lookup_hypoglyc2_2 %>%
#   count(patient_id, case_id, pat_geb_dat, alter_in_tagen, dokumentation, app_date_doku, Hypoglycaemia, name = "Count_measurement")
## Conclusion: How much Measurements does not really matter, based hypoglaemic episodes


## With bilirubin (category no measurement)
# Hypoglyc_cat <- New_meona_c %>%
#   select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation) %>% 
#   group_by(patient_id, case_id) %>%
#   filter(alter_in_tagen <= 3) %>%
#   mutate(Hypoglycaemia_category = case_when(
#     !str_detect(order_text, "Blutzucker [mmol/L]") ~ "No_measure",  # str_detect: returns a logical vector with TRUE for each element of string that matches pattern and FALSE otherwise
#     dokumentation < 2.0 ~ "Severe", 
#     dokumentation < 2.6 ~ "Mild",
#     dokumentation >= 2.6 ~ "Normoglycaemic")) %>%
#   mutate(cat = if_else(Hypoglycaemia_category %in% "Normoglycaemic", 0,
#                        if_else(Hypoglycaemia_category %in% "Mild", 1, 2))) %>%
#   mutate(Sum_Category = sum(cat)) %>%
#   mutate(Hypoglycaemia_cat = if_else(Sum_Category == 0, "Normoglycaemic",
#                                      if_else(Sum_Category == 1, "Mild", "Moderate", 
#                                              if_else(Sum_Category == 3, "Severe", "text"))))

# Hypoglyc_cat2 <- New_meona_c %>%
#   select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation) %>% 
#   filter(!order_text %in% "Bilirubin [mmol/L]") %>%
#   group_by(patient_id, case_id) %>%
#   filter(alter_in_tagen <= 3) %>%
#   mutate(Hypoglycaemia_category = case_when(
#     dokumentation < 2.0 ~ "Severe", 
#     dokumentation < 2.6 ~ "Mild",
#     dokumentation >= 2.6 ~ "Normoglycaemic")) %>%
#   mutate(cat = if_else(Hypoglycaemia_category %in% "Normoglycaemic", 0,
#                        if_else(Hypoglycaemia_category %in% "Mild", 1, 2))) %>% 
#   mutate(Sum_Category = sum(cat)) %>%
#   mutate(Hypoglycaemia_cat = if_else(Sum_Category == 0, "Normoglycaemic",
#                                      if_else(Sum_Category == 1, "Mild", "Moderate",
#                                              if_else(Sum_Category >= 3, "Severe", "Text"))))
## Hypoglycaemia_cat nicht korrekt: kein severe

# Hypoglyc_cat4 <- New_meona_c %>%
#   select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation) %>% 
#   filter(!order_text %in% "Bilirubin [mmol/L]") %>%
#   group_by(patient_id, case_id) %>%
#   filter(alter_in_tagen <= 3) %>%
#   mutate(Hypoglycaemia_category = case_when(
#     dokumentation < 2.0 ~ "Severe", 
#     dokumentation < 2.6 ~ "Mild",
#     dokumentation >= 2.6 ~ "Normoglycaemic")) %>%
#   mutate(Summ = if_else(Hypoglycaemia_category %in% "Normoglycaemic", 0,
#                         if_else(Hypoglycaemia_category %in% "Mild", 1, 2))) %>% 
#   mutate(Sum_Category = sum(Summ)) %>%
#   mutate(Hypoglycaemia_cat = if_else(Sum_Category == 0, "Normoglycaemic",
#                                      if_else(Sum_Category == 1, "Mild",
#                                              if_else(Sum_Category == 2, "Moderate", # <- dies ist das Problem
#                                                      if_else(Sum_Category == 3, "Severe", "Severe"))))) 
# In two steps?
# Lookup_hypoglyc7 <- Lookup_hypoglyc2_2 %>% 
#   filter(alter_in_tagen <= 3) %>% 
#   filter(Hypoglycaemia %in% c("No", "Yes")) %>% 
#   filter(Hypoglycaemia_severe %in% c("No", "Yes")) %>% 
#   count(patient_id, case_id, pat_geb_dat, alter_in_tagen, dokumentation, Hypoglycaemia, Hypoglycaemia_severe, name = "Episode_Hypoglycaemia")
# 
# Lookup_hypoglyc8 <- Lookup_hypoglyc7 %>% 
#   count(patient_id, case_id, pat_geb_dat, alter_in_tagen, dokumentation, Hypoglycaemia, Hypoglycaemia_severe, name = "Episode_Hypoglycaemia_severe") %>% 
#   mutate(Category_Hypoglycaemia = case_when(
#     Episode_Hypoglycaemia == 1 ~ "Mild",
#     Episode_Hypoglycaemia == 2 ~ "Moderate",
#     Episode_Hypoglycaemia > 2 ~ "Severe",
#     Episode_Hypoglycaemia_severe >= 1 ~ "Severe",
#     Episode_Hypoglycaemia == 0 ~ "Normoglyc"))

## no, same problem

# Lookup_hypoglyc5 <- Lookup_hypoglyc2_2 %>% 
#   filter(alter_in_tagen <= 3) %>%
#     filter(Hypoglycaemia %in% c("No", "Yes")) %>% 
#       filter(Hypoglycaemia_severe %in% c("No", "Yes")) %>% 
#         count(patient_id, case_id, pat_geb_dat, alter_in_tagen, dokumentation, Hypoglycaemia, Hypoglycaemia_severe, name = "Episode_Hypoglycaemia") # %>% 
# count(patient_id, case_id, pat_geb_dat, alter_in_tagen, dokumentation, Hypoglycaemia, Hypoglycaemia_severe, name = "Episode_Hypoglycaemia_severe")
## how to integrate both episodes?

# Bili_data_cat <- Lookup_bili2 %>%
#   mutate(Threshold_age = max(Threshold_bili$Age_in_hours[Threshold_bili$Age_in_hours <= Age_in_hours])) # filters values from Threshold_bili$Age_in_hours that are less than or equal to Age_in_hours
## not working

# Bili_data_cat3 <- Lookup_bili2 %>%
#   rowwise() %>%  # if you are looking for the maximum for each row use rowwise()
#   mutate(Threshold_age = max(Threshold_bili$Age_in_hours[Threshold_bili$Age_in_hours <= Age_in_hours])) %>% # filters values from Threshold_bili$Age_in_hours that are less than or equal to Age_in_hours
#   ungroup() # to turn off rowwise
# 

# Bili_data_cat4 <- left_join(Bili_data_cat, Threshold_bili, by = c("Threshold_age" = "Age_in_hours")) %>%
#   mutate(Cat = case_when(
#       Dokumentation_umol_l < Bili_start_phototherapy_above ~ "Physiological_Jaundice",
#       Dokumentation_umol_l >= Bili_start_phototherapy_above & Dokumentation_umol_l < Bili_start_exchange_therapy_above ~ "Treatment_Required",
#       Dokumentation_umol_l >= Bili_start_exchange_therapy_above ~ "Exchange_Transfusion"))

# In one code
# Bili_data_cat4 <- Lookup_bili2 %>%
#   rowwise() %>%  
#   mutate(Threshold_age = max(Threshold_bili$Age_in_hours[Threshold_bili$Age_in_hours <= Age_in_hours])) %>%
#   ungroup() %>%
#   left_join(Threshold_bili, by = c("Threshold_age" = "Age_in_hours")) %>%
#   mutate(Cat = case_when(
#       Dokumentation_umol_l < Bili_start_phototherapy_above ~ "Jaundice",
#       Dokumentation_umol_l >= Bili_start_phototherapy_above & Dokumentation_umol_l < Bili_start_exchange_therapy_above ~ "Treatment_req_Photo",
#       Dokumentation_umol_l >= Bili_start_exchange_therapy_above ~ "Treatment_req_ExT"))

## To merge with Sample3 (overview with icds)
# Sample5 <- left_join(Sample3, Hypoglyc_data_cat2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   select(- pat_geb_dat) # 15644
# 
# ## Family history/Risk factor for hypoglycaemia? 
# 
# # Hypoclyc_FA <- Sample5 %>% 
# #   filter(Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe", 
# #                                   ICD_labels %in% "Diabetes mellitus in der Familienanamnese", 
# #                                   ICD_labels %in% "Syndrom des Kindes einer Mutter mit gestationsbedingtem Diabetes mellitus")) # does not work
# 
# Hypoglyc_FA <- Sample5 %>% 
#   filter(Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe") & 
#            (ICD_labels %in% c("Diabetes mellitus in der Familienanamnese","Syndrom des Kindes einer Mutter mit gestationsbedingtem Diabetes mellitus"))) %>% 
#   distinct(patient_id_child, .keep_all = TRUE) # 1001 obs
# 
# Hypoglyc_no_FA <- Sample5 %>% 
#   filter(Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe") & 
#            !ICD_labels %in% c("Diabetes mellitus in der Familienanamnese", 
#                               "Syndrom des Kindes einer Mutter mit gestationsbedingtem Diabetes mellitus")) # 6327 obs
# 
# Hypoglyc_no_FA_neo <- Hypoglyc_no_FA %>% 
#   filter(admission_neo %in% "Yes") # 415 obs

# 7. Overview Temp data --------------------------------------------------------
summary(Temp_data) # 211808
sum(is.na(Temp_data$patient_id)) # there are no NAs
sum(is.na(Temp_data$case_id)) # 15040

Temp_data2 <- left_join(Sample2, Temp_data, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS)
# 67726 obs

summary(Temp_data2)

# Patient_id, case_id
sum(is.na(Temp_data2$patient_id_child)) # 0
sum(is.na(Temp_data2$case_id_child)) # 0

# VVMO_NUMERIC_VALUE
summary(Temp_data2$VVMO_NUMERIC_VALUE)
# Min.   1st Qu.  Median  Mean   3rd Qu.  Max.    NA's 
# 27.10   36.90   37.10   37.08   37.30   38.90   13  
## Conclusion: NAs were mainly due to outpatient births and transfers to the Nicu on the same day of birth -> readmission

# To exclude missing data in temperature records
Temp_data2 <- Temp_data2 %>% 
  filter(!is.na(VVMO_NUMERIC_VALUE)) # 67713

summary(Temp_data2)

# Check Id
Check_pat_id_temp <- Temp_data2 %>% 
  select(patient_id_child) %>% 
  distinct
## 6890 

Check_case_id_temp <- Temp_data2 %>% 
  select(case_id_child) %>% 
  distinct
## 6890

# Identify cases with neo admission -> old
# Temp_adm_neo <- Temp_data2 %>% 
#   filter(admission_neo %in% "Yes") 
# Temp_data2 %>%
#   summarise(total = n(), # n() gives the current group size
#             admitted = sum(admission_neo %in% "Yes"),  # sum returns the sum of all the values present in its arguments
#             percent = (admitted / total) * 100)
# # total admitted percent
# # <int>    <int>   <dbl>
# #1 81575     1494    1.83

Temp_low <- Temp_data2 %>%
  filter(VVMO_NUMERIC_VALUE < 36.5) # 2210

# Temp_low_adm <- Temp_data2 %>%
#   filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes")
# Temp_low_adm2 <- Temp_data2 %>%
#   filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes") %>% 
#   select(patient_id_child) %>%
#   distinct() 

# 7.1 Operationalisation --------------------------------------------------
# CATEGORISATION
Temp_data_cat <- Temp_data2 %>%
  select(patient_id_child, case_id_child, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS) %>% 
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
  distinct(patient_id_child, .keep_all = TRUE) # 6890; .keep.all = keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.

# Percentages and neo admission 
table(as.factor(Temp_data_cat2$Hypothermia_cat))
# Mild      Moderate_Severe      Norm 
# 989             533            5368 

round(prop.table(table(as.factor(Temp_data_cat2$Hypothermia_cat))) * 100, 1)
# Mild       Moderate_Severe      Norm 
# 14.4             7.7            77.9 

## For analysis: Hypothermia_cat


# 8. Merging with sample data-----------------------------------------------------------------
## Join with Sample2b
Sample2c <- left_join(Sample2b, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) %>% 
  select(- VVMO_NUMERIC_VALUE, - VVMO_MEASURE_DATE_TS)

summary(as.factor(Sample2c$Hypothermia_cat))
# Mild    Moderate_Severe       Norm            NA's 
# 989             533            5368            13 
## 6 cases yes Nicu admission, 7 no Nicu admission -> 6 of them ambulant/tagesklinik (one child readmission and as a Begleitperson), 1 child no data entry

table(Sample2c$Hypothermia_cat)
# Mild      Moderate_Severe      Norm 
# 989             533            5368 

round(prop.table(table(as.factor(Sample2c$Hypothermia_cat))) * 100, 1)
# Mild     Moderate_Severe      Norm 
# 14.4           7.7           77.9  

Hypotherm_Nicu <- Sample2c %>% 
  filter(admission_neo %in% "Yes", Hypothermia_cat %in% c("Norm", "Mild", "Moderate_Severe")) # 149

table(Hypotherm_Nicu$Hypothermia_cat)
# Mild        Moderate_Severe   Norm 
# 18              21             110 

round(prop.table(table(Hypotherm_Nicu$Hypothermia_cat)) * 100, 1)
# Mild        Moderate_Severe   Norm 
# 12.1            14.1          73.8



# Codes not needed/working ------------------------------------------------

# Temp_data_cat4 <- Temp_data_cat3 %>%
#   # select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, admission_neo, admission_neo_n, Weeks_LPM, gestational_age_total_days, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS) %>% 
#   group_by(patient_id_child, case_id_child) %>%
#   mutate(Hypothermia_categories = if_else(all(VVMO_NUMERIC_VALUE >= 36.5), "Normotherm", 
#     if_else(any(VVMO_NUMERIC_VALUE < 36.0), "Moderate_Severe", 
#             if_else(all(VVMO_NUMERIC_VALUE >= 36.0) & any(VVMO_NUMERIC_VALUE < 36.5), "Mild", 
#                     if_else(Hypothermia_category %in% "Mild Hypothermia" & Count >= 2, "Moderate_Severe", "test")))))  


# Temp_data_cat <- Temp_data2 %>%
#   select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, admission_neo, admission_neo_n, Weeks_LPM, gestational_age_total_days, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS) %>% 
#   group_by(patient_id_child, case_id_child) %>%
#   mutate(Hypothermia_category = case_when(
#     VVMO_NUMERIC_VALUE >= 36.5 ~ "Normotherm",
#     VVMO_NUMERIC_VALUE >= 36.0 & VVMO_NUMERIC_VALUE < 36.5 ~ "Mild_Hypothermia", # <= 60  (one episode, lasting no more than one hour)
#     # VVMO_NUMERIC_VALUE >= 36.0 & VVMO_NUMERIC_VALUE < 36.5 ~ "Moderate/Severe Hypothermia", # recurrent, <36.5°C, at least two episodes, each separated by at least two/one hours? of normal temperature
#     # VVMO_NUMERIC_VALUE >= 36.0 & VVMO_NUMERIC_VALUE < 36.5 ~ "Moderate/Severe Hypothermia", # persistent, duration of two hours or more
#     VVMO_NUMERIC_VALUE < 36.0 ~ "Moderate_Severe_Hypothermia")) %>% 
#   # group_by(patient_id_child, case_id_child, Hypothermia_category) %>% 
#   # mutate(Count = n()) %>% 
#   # ungroup() %>% 
#   mutate(cat = if_else(Hypothermia_category %in% "Normotherm", 0,
#                        if_else(Hypothermia_category %in% "Mild", 1, 2))) %>% 
#   mutate(Category = sum(cat)) %>% 
#   mutate(Category_new = if_else(Category == 0, "Norm",
#                                 if_else(Category == 1, "Mild", "Moderate_Severe")))

# ## To merge with Sample3 (overview with icds)
# Sample7 <- left_join(Sample3, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) 
# 
# Hypotherm_newborn <- Sample7 %>%
#   filter(admission_neo %in% "Yes") %>% 
#   filter(Transfer_Gebs_Muki == TRUE) %>% 
#   filter(DIA_NK %in% c("P80.9", "P80.8")) %>% 
#   distinct() # 22
# 
# table(Hypotherm_newborn$Hypothermia_cat)
# # Mild      Moderate_Severe     Norm 
# # 5              14               3 
# round(prop.table(table(Hypotherm_newborn$Hypothermia_cat)) * 100, 1)
# # Mild      Moderate_Severe       Norm 
# # 22.7            63.6            13.6 


# 9. Birth_corr2 -------------------------------------------------------------
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


# 10. Extraction Sample >=37 GA --------------------------------------------
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




