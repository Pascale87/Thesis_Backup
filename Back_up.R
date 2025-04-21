
# 2. Import data sets --------------------------------------------------------
# 2.a Data sets
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr2.RData")
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr3_2025-04-18.RData")
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
summary(Birth_corr3)

## Time frame check (Inclusion criteria 1)
summary(Birth_corr3$CBIS_BIRTH_DATE_TS)
# Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
# "2019-01-01 01:26:00.0000" "2019-12-21 18:54:45.0000" "2021-01-05 15:17:00.0000" "2020-12-30 15:21:19.3272" "2021-12-24 07:42:30.0000" "2022-12-31 16:17:00.0000" 
# Conclusion: no exclusions

# patient_id_child
# sum(is.na(Birth_corr3$patient_id_child)) # there are 2 NAs
# ## case_id_child
# sum(is.na(Birth_corr3$case_id_child)) # there are 2 NAs
# na_case <- Birth_corr3 %>%
#   filter(is.na(case_id_child)) # 2 stillbirths

Birth_corr3 <- Birth_corr3 %>%
  filter(!is.na(case_id_child)) # excluded cases without case_id, 10776 (-2)

## CBIS_BIRTH_DATE_TS (Birth date and time)
summary(Birth_corr3$CBIS_BIRTH_DATE_TS)
# Min.                    1st Qu.                     Median                       Mean                    3rd Qu.                       Max. 
# "2019-01-01 01:26:00.0000" "2019-12-21 18:21:30.0000" "2021-01-05 14:18:30.0000" "2020-12-30 12:44:50.6337" "2021-12-24 04:11:45.0000" "2022-12-31 16:17:00.0000" 
sum(is.na(Birth_corr3$CBIS_BIRTH_DATE_TS)) # no NAs

## CBIS_MULTIPLE_BIRTH_FLAG (multiple birth (several children), 1 if yes, otherwise 0)
summary(as.factor(Birth_corr3$CBIS_MULTIPLE_BIRTH_FLAG))
# 0      1 
# 10143   633 
# there are no NAs

## Stillbirths
table(Birth_corr3$CBIS_STILLBIRTH_FLAG) # 77

## CBIS_CONGENITAL_MALFORMATION (congenital malformation (1 = yes, 0 = no, (-1) = unknown))
table(Birth_corr3$CBIS_CONGENITAL_MALFORMATION)
# -1     0     1 
# 73 10083   620 
# there are no NAs

## CBIS_MODE (codes in (1,7) - exact decryption still unclear)) --> new variable in Birth_corr3  
# table(Birth_corr3$CBIS_MODE) 
# # 1    2    3    4    6 n.a. 
# 5409  841 1066  103   48 3309 
## not relevant

## CBIS_WEIGHT
summary(Birth_corr3$CBIS_WEIGHT)
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
summary(Birth_corr3$CBIS_BODY_SIZE)
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
summary(Birth_corr3$CBIS_HEAD_SIZE)
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
summary(Birth_corr3$CBIS_APGAR_SCORE_0MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   8.000   9.000   8.005   9.000  10.000    2597 

## CBIS_APGAR_SCORE_5Min
summary(Birth_corr3$CBIS_APGAR_SCORE_5MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   9.000   9.000   9.077  10.000  10.000    2605 

## CBIS_APGAR_SCORE_10Min
summary(Birth_corr3$CBIS_APGAR_SCORE_10MIN)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000  10.000  10.000   9.599  10.000  10.000    2605 

## CBIS_COMMENT
summary(as.factor(Birth_corr3$CBIS_COMMENT)) 
# various specified comments, but not always -> variable will be deleted as not necessary and there are also person related information

## CBIS_CREATE_DATE (Date of creation of record) -> not needed, will be deleted
## CBIS_CREATE_DAY_BK (Date of creation of record in format YYYYMMDD) -> not needed, will be deleted
## CBIS_UPDATE_DATE (Date of update of record) -> not needed, will be deleted
## CBIS_UPDATE_DAY_BK (Date of update of record in format YYYYMMDD) -> not needed, will be deleted

# Check for id /cases
Check_id <- Birth_corr3 %>% 
  select(patient_id_child, case_id_child) %>% 
  distinct
# 10776 obs

Check_pat_id <- Birth_corr3 %>% 
  select(patient_id_child) %>% 
  distinct

Check_case_id <- Birth_corr3 %>% 
  select(case_id_child) %>% 
  distinct

# Check for duplicates
sum(duplicated(Birth_corr3)) # no duplicates

rm(Check_case_id, Check_id, Check_pat_id)

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
Consent_check <- left_join(Birth_corr3, Consent, by = c("patient_id_mother" = "patient_id"))
Consent_check2 <- left_join(Birth_corr3, Consent, by = c("patient_id_child" = "patient_id"))

Consent_exclude_mother <- Consent_check %>%
  filter(CON_VALUE %in% "Nein") %>% 
  select(patient_id_mother, case_id_mother) # 1381

Consent_exclude_child <- Consent_check2 %>%
  filter(CON_VALUE %in% "Nein") %>% 
  select(patient_id_child, case_id_child) # 82 

Lookup1 <- Birth_corr3 %>%
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
Birth_corr3_m <- left_join(Lookup1, gestational_age_final, by = c("patient_id_mother", "case_id_mother", "patient_id_child", "case_id_child"), relationship = "many-to-many")
# 9342
summary(Birth_corr3_m) # overview: Gestational age there are now 392 NAs -> most of them amb./tagesklinik
Birth_corr3_m <- Birth_corr3_m %>% 
  filter(!is.na(gestational_age_total_days)) # 8950
table(duplicated(Birth_corr3_m))  # check for duplicates, no ones

# 3.4.1 >= 37 GA ---------------------------------------------------------------
# Inclusion criteria 3
# Select only variable of interest
Birth_corr3_m <- Birth_corr3_m %>%
  select(patient_id_mother, case_id_mother, patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, CBIS_WEIGHT, CBIS_WEIGHT_UNIT,
         CBIS_STILLBIRTH_FLAG, CBIS_CONGENITAL_MALFORMATION, Weeks_LPM, Days_LPM, gestational_age_total_days, admission_neo, admission_neo_n, feeding_method_u3, mode_of_birth) #- CON_VALUE

# Lookup2 <- Birth_corr2_m %>%
#   select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, CBIS_BIRTH_DAY_BK, CBIS_BIRTH_TIM_BK, CBIS_WEIGHT, CBIS_WEIGHT_UNIT, CBIS_STILLBIRTH_FLAG, CBIS_CONGENITAL_MALFORMATION,
#          CBIS_BODY_SIZE, CBIS_BODY_SIZE_UNIT, CBIS_HEAD_SIZE, CBIS_HEAD_SIZE_UNIT, admission_neo, admission_neo_n, Weeks_LPM, Days_LPM, gestational_age_total_days, CON_VALUE) 

# Create data sample with newborns >= 37 GA (259 weeks)
GA <- Birth_corr3_m %>% 
  filter(gestational_age_total_days < "259") # 1148 are under 37 GA
Sample1 <- Birth_corr3_m %>% 
  filter(gestational_age_total_days >= "259") # 7802 

# 3.4.1.1 Birth weight ---------------------------------------------------------
## To exclude newborns which have a birth weight < 2500g and birth weight > 4500g
summary(Sample1$CBIS_WEIGHT) # there are cases with less than 2500 g and also cases with more than 4500g
Weight_exclude <- Sample1 %>% 
  summarise(below_2500 = sum(CBIS_WEIGHT < 2500), # 228
            above_4500 = sum(CBIS_WEIGHT > 4500)) # 77

## Sample with children above 4500g, just in case
# Sample1_r <- Sample1 %>% 
#   filter(!CBIS_WEIGHT < 2500) 

Sample1 <- Sample1 %>% 
  filter(CBIS_WEIGHT >= 2500 & CBIS_WEIGHT <= 4500) %>% 
  select(- CBIS_WEIGHT_UNIT) %>% 
  rename(Birth_weight_g = CBIS_WEIGHT) # 7497

# Build categories for the descriptive analysis (500g step)
Sample1 <- Sample1 %>% 
  filter(Birth_weight_g >= 2500 & Birth_weight_g <= 4500) %>% 
  mutate(weight_cat = case_when(Birth_weight_g >= 2500 & Birth_weight_g < 3000 ~ "2500-2999",
                                Birth_weight_g >= 3000 & Birth_weight_g < 3500 ~ "3000-3500",
                                Birth_weight_g >= 3500 & Birth_weight_g < 4000 ~ "3500-4000",
                                Birth_weight_g >= 4000 & Birth_weight_g <= 4500 ~ "4000-4500"))


# 3.4.2 Stillbirth --------------------------------------
table(Sample1$CBIS_STILLBIRTH_FLAG) 
# 0      1 
# 7491   6

# ## CBIS_CONGENITAL_MALFORMATION (congenital malformation (1 = yes, 0 = no, (-1) = unknown)) --> will be excluded later
# table(Sample1$CBIS_CONGENITAL_MALFORMATION)
# # -1    0    1 
# # 18 7080  399

# Check_stillbirth <- Sample1 %>% 
#   filter(CBIS_STILLBIRTH_FLAG == 1, CBIS_CONGENITAL_MALFORMATION == 1) # 0 cases with both conditions

# to exclude cases with stillbirths 
Sample1 <- Sample1 %>% 
  filter(CBIS_STILLBIRTH_FLAG == 0) # 7491, no unknown cases present

# ## To check --> should be performed later too
# # Percentages
# Birth_corr3_m %>%
#   summarise(total = n(),
#             GA_week37 = sum(gestational_age_total_days >= 259),
#             percent = (GA_week37 / total) * 100)
# # total GA_week37 percent
# # <int>     <int>   <dbl>
# #1  8950      7802    87.2

# Admission neo (just a between step for control, inclusion criteria 2 is actually missing) --> to performe later too
# Adm_nicu <- Sample1 %>% 
#   filter(admission_neo %in% "Yes") # 340
# Sample1 %>%
#   summarise(total = n(), 
#             admitted = sum(admission_neo %in% "Yes"), 
#             percent = (admitted / total) * 100)
# # total admitted percent
# # <int>    <int>   <dbl>
# # 1  7802   448    5.74

rm(Lookup1, Weight_exclude, Consent_check, Consent_check2, Consent_exclude_child, Consent_exclude_mother, gestational_age_final, GA)

# 3.4.3 Mode of delivery --------------------------------------------------
table(Sample1$mode_of_birth)
# Instrum_del  Normal_del  Pl_CS      Upl_CS 
# 1077         3753        1455       1127 

table(is.na(Sample1$mode_of_birth)) # 79

# create new variable
Sample1 <- Sample1 %>%
  mutate(Birth_mode = ifelse(is.na(mode_of_birth), "No_data", mode_of_birth))

Sample1 <- Sample1 %>% 
  mutate(Birth_mode_group = case_when(
    Birth_mode %in% c("Pl_CS", "Upl_CS") ~ "Sectio",
    TRUE ~ Birth_mode))

Sample1 <- Sample1 %>%
  mutate(Birth_mode_group = recode(Birth_mode_group, 
                                   "Normal_del" = "Vaginal", 
                                   "Instrum_del" = "Instrumental_vaginal", 
                                   "Sectio" = "C-section",
                                   "No_data" = "No_data"))  

Sample1 <- Sample1 %>% 
  select(-mode_of_birth, -Birth_mode)

Sample1 <- Sample1 %>%
  mutate(Birth_mode_group = factor(Birth_mode_group,
                                   levels = c("Vaginal", "Instrumental_vaginal", "C-section", "No_data")))
# 3.4.4 Nutrition ---------------------------------------------------------
table(Sample1$feeding_method_u3)
# excl_bf   excl_ff   ff_plus partly_bf 
# 1636       100        60      5391

table(is.na(Sample1$feeding_method_u3)) # TRUE = 304

# create new variable and rename the values (with recode)
Sample1 <- Sample1 %>%
  mutate(Feeding_group = if_else(is.na(feeding_method_u3), "No_data", feeding_method_u3))

Sample1 <- Sample1 %>%
  mutate(Feeding_group = recode(Feeding_group, 
                                "excl_bf" = "Fully_breastfed", 
                                "partly_bf" = "Partly_breastfed", 
                                "ff_plus" = "Mixed_feeding_no_breastfed", 
                                "excl_ff" = "Formula_only",
                                "No_data" = "No_data"))

Sample1 <- Sample1 %>% 
  select(- feeding_method_u3)

# in factor for analysis
Sample1 <- Sample1 %>%
  mutate(Feeding_group = factor(Feeding_group,
                                levels = c("Fully_breastfed", "Partly_breastfed", "Mixed_feeding_no_breastfed", "Formula_only", "No_data")))


# 3.4.5 Newborn transfer -----------------------------------------------------
# LOS data (extracted from Movement data, only newborns admitted to the Muki and discharged from Muki)
## Overview
summary(LOS_newborns) # 9268
table(is.na(LOS_newborns$patient_id_child)) # no NAs
table(is.na(LOS_newborns$case_id_child)) # no NAs
table(is.na(LOS_newborns$LOS_neonatal)) # no NAs

# Merging with Sample1, with variables selection and arrangement and renaming variables
Sample2 <- left_join(Sample1, LOS_newborns, by = c("patient_id_child", "case_id_child")) %>% 
  select(- CBIS_STILLBIRTH_FLAG, - CBIS_CONGENITAL_MALFORMATION) %>% 
  relocate(Birth_weight_g, weight_cat, .after = last_col()) %>% # .after = Destination of columns selected by
  relocate(admission_postnatal_neonatal, discharge_postnatal_neonatal, LOS_neonatal, admission_neo, admission_neo_n, .after = last_col()) %>% 
  rename(admission_postnatalunit = admission_postnatal_neonatal, discharge_postnatalunit = discharge_postnatal_neonatal, LOS = LOS_neonatal) 

summary(Sample2)

# Check admission_Muki
table(is.na(Sample2$admission_postnatalunit))
# FALSE  TRUE 
# 7207   284 

# Double check cases not transferred directly from the labour ward to the postnatal unit
Sample2_isna_transfer <- Sample2 %>% 
  filter(is.na(admission_postnatalunit)) # 284

# Identify the cases when not transferred to the postnatal unit
Move_newborn <- left_join(Sample2_isna_transfer, Move_stat2f, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, CBIS_BIRTH_DATE_TS, admission_neo, admission_neo_n, MOV_START_DATE_TS, MOV_END_DATE_TS, CAS_TYPE, MOV_KIND, MOV_TYPE,
         MOV_REASON1, MOV_REASON2, unit_id, ORG) # 580, without distinct

# Directly transferred from the labour ward to NICU
Labour_dis_nicu <- Move_newborn %>% 
  filter(admission_neo %in% "Yes") %>%
  distinct(patient_id_child, case_id_child) # 236

# If not, where was the discharge?
Labour_dis_nicu_no <- Move_newborn %>% 
  filter(admission_neo %in% "No") %>%  
  distinct(patient_id_child, case_id_child) # 48

## Other discharge reasons: Birth centre/other hospital n= 32; Home n= 7; Other unit within hospital n= 6; unsure n= 2, death n= 1

# Dataset cleaned without cases with no transfer from the labour ward to neonatal unit
Sample2 <- Sample2 %>% 
  filter(!is.na(admission_postnatalunit)) # 7207

# attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
# Sample2 <- Sample2 %>% 
#   mutate(LOS = LOS/60)
# attr(Sample2$LOS, "units") <- "h"

# Check ids
## child
Check_id_sample2_child <- Sample2 %>% 
  select(patient_id_child) %>% 
  distinct # 7207
Check_id_sample2_child2 <- Sample2 %>% 
  select(case_id_child) %>% 
  distinct # 7207
## mother
Check_id_sample2_mo <- Sample2 %>% 
  select(patient_id_mother) %>% 
  distinct # 6550 -> mother gave more than one birth 
Check_id_sample2_mo2 <- Sample2 %>% 
  select(case_id_mother) %>% 
  distinct # 7145

rm(Check_id_sample2_child, Check_id_sample2_child2, Check_id_sample2_mo, Check_id_sample2_mo2, Sample1, Labour_dis_nicu, Labour_dis_nicu_no, Sample2_isna_transfer)


# 3.4.6 Neonatal diagnoses -------------------------------------------------
## To exclude cases with diagnoses to be excluded
# Merge Sample with diagnose data
Sample2_dia_neonatal <- left_join(Sample2, Diagnose_red2_corr, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, DIA_NK, ICD_labels)

table(Sample2_dia_neonatal$ICD_labels) # overview diagnoses

## Include diagnoses: 
# Included_icd_codes <- c("P55.1", "R63.4", "Q83.3", "Q69.2", "Z83.4", "Q82.5", "Q38.1", "Z83.5", "R01.0", "Z03.8", "Z03.3", "Z03.5", 
#                         "P54.6", "H11.3", "R00.1", "P83.4", "L81.3", "U07.2", "Z38.0", "P92.0", "L53.0", "P83.1", "R25.3", "D18.08", "D22.7",
#                         "R50.80", "R50.9", "R14", "P05.1", "P05.0", "K21.9", "P12.1", "P15.5", "P12.9", "P15.3", "P15.4", "P13.8", "Q98.0", "Q70.0", "Q69.9", "Q70.3", 
#                         "D18.01", "D18.05", "P78.2", "P54.5", "K40.90", "K42.9", "R01.1", "R05", "P80.9", "Z83.1", "P12.0", "Q63.2", "Q52.3", "Q54.1", "Q26.1",
#                         "Z84.3", "Z20.6", "Z20.8", "Z20.5", "Z84.1", "Z83.2", "P83.9", "K13.2", "R22.0", "R22.2", "L98.8", "Q82.8", "Q53.0", "Q67.0", "Q67.6",
#                         "R22.3", "R59.0", "D22.6", "D22.5", "D22.3", "D22.9", "Q70.2", "P08.2", "D48.5", "P07.12", "P58.1", "P58.8", "D18.18", "Q66.5", "Q67.3", 
#                         "P59.8", "P59.9", "Z24.6", "P38", "T14.03", "Q66.2", "Q66.4", "Q66.0", "L05.9", "Q17.5", "Q18.1", "Q68.0", "Z81", "P92.1", "D55.0",
#                         "P55.0", "P92.5", "R90.8", "Q66.8", "Q10.3", "P92.8", "P08.1", "P07.3", "P13.1", "R25.1", "Q54.0", "K06.8", "K07.1", "Q84.6", 
#                         "P12.8", "P14.3", "D23.4", "D23.9", "R01.2", "P70.4", "P80.8", "P51.8", "R68.8", "P15.8", "Z84.8", "L81.8", "P59.0", "Z04.8",
#                         "P81.8", "R19.88", "P72.8", "S09.8", "P96.8", "K00.8", "P94.8", "R23.8", "N83.2", "R39.1", "R39.8", "R29.8", "K09.8", 
#                         "X59.9", "R50.88", "Z11", "U99.0", "P81.9", "K00.6", "P70.1", "P70.0", "R00.0", "M43.6", "P70.9", "P61.0", "E16.2", 
#                         "P92.2", "P08.0", "P12.4", "R60.0", "R69", "W64.9", "Z04.3", "R23.4", "T81.2", "P96.3", "L22", "Z38.3", "Z38.5", "Y69", "Z83.3")


## Exclude diagnoses: 
Exclude_icd_codes <- c("R93.4", "R93.1", "R93.0", "R93.3", "R94.8", "R94.3", "H27.9", "Q24.9", "Q03.0", "Q04.0", "Q62.0", "P83.5", "Q62.2", "R31", 
                       "R34", "P21.9", "P22.9", "Z82", "I34.0", "U07.1", "P76.9", "Q21.2", "Q50.1", "Z38.1", "P12.2", "B96.2", "P05.2", "P50.1", "P39.3", 
                       "P39.4", "P29.1", "G93.0", "P39.9", "Z86.1", "P39.2", "P20.0", "Q04.3", "P20.1", "P20.9", "P05.9", "Z29.0", "P39.1", "Z86.2", 
                       "Z86.7", "P21.1", "Q33.2", "Q33.8", "Q36.1", "Q36.9", "E84.1", "Q02", "Q04.6", "Q18.8", "Q20.8", "Q22.2", "Q04.8", "Q52.9", 
                       "Q60.0", "Q60.3", "Q61.3", "Q61.4", "Q62.3", "Q62.7", "Q63.1", "Q53.1", "Q54.8", "Q54.9", "Q67.4", "Q23.8", "Q25.0", "Q25.4", 
                       "Q53.2", "Q72.3", "Q74.0", "P29.3", "P61.1", "Q65.8", "P04.0", "P02.7", "P03.3", "P03.0", "P00.0", "P01.2", "P01.3", "P00.2", 
                       "P02.8", "Q52.4", "Q55.2", "Q62.8", "Q74.8", "Q63.8", "Q75.8", "P03.4", "P02.5", "P02.1", "P03.1", "P04.1", "P03.8", "P02.2", 
                       "P02.6", "P04.2", "P01.1", "P03.2", "P21.0", "Q22.8", "R90.8", "P28.4", "P22.8", "P29.8", "I78.8", "R76.8", "H21.8", "H11.8", 
                       "H02.8", "Q37.1", "H57.8", "Q89.8", "Q15.8", "Q64.7", "Q64.8", "Q17.8", "P54.8", "P61.8", "Q79.5", "P39.8", "N28.8", "N28.88", 
                       "H61.8", "P78.8", "P83.8", "Q07.8", "P28.8", "R06.88", "I25.3", "I63.8", "P75", "P76.8", "Q17.3", "Q35.3", "B95.6", "P28.9", 
                       "R06.1", "Q70.9", "Q74.2", "P22.1", "Q21.0", "Q21.1", "Q28.88", "Q30.0", "Q33.0", "Q39.0", "Q90.9", "Z38.5", "M54.2", "P28.2", 
                       "P90", "P91.1", "B95.7", "Q25.6", "Q35.5", "Q37.4", "Q37.5")

# In Diagnose data there are N28.8 and N28.88 (not for excel table because there is no N28.88 in the who icd-codes)

# Check 
length(Exclude_icd_codes) # 162
length(unique(Exclude_icd_codes)) # 162
Exclude_icd_codes[duplicated(Exclude_icd_codes)] # 0

Newborns_excluded_diagnoses <- Sample2_dia_neonatal %>%
  filter(DIA_NK %in% Exclude_icd_codes) %>%  # 1634
  distinct(patient_id_child, case_id_child) # 1315 cases, these are to exclude

Sample3 <- anti_join(Sample2, Newborns_excluded_diagnoses, by = c("patient_id_child", "case_id_child"))  
n_distinct(Sample3$case_id_child) # check 5892 cases -> cleaned data set with only newborns with allowed diagnoses


# 3.4.7 Building Newborn groups -------------------------------------------
## Aim to have two sample groups: one without risk factors and one with risk factors

## HHH RELEAVANT DIAGNOSES
# 1. Hypothermia: P80.0, P80.8, P80.9
# 2. Hypoglycaemia: P70.4, E16.2
# 3. Hyperbilirubinaemia: P58.1, P58.8, P59.0, P59.8., P59.9

# ICD-10 codes
## Diagnoses
Hypothermia_icd <- c("P80.0", "P80.8", "P80.9")
Hypoglycaemia_icd <- c("P70.0", "P70.1", "P70.4", "P70.9", "E16.2")
Hyperbilirubinaemia_icd <- c("P58.1", "P58.8", "P59.0", "P59.8", "P59.9")

## HHH RELEVANT RISK FACTORS (NEONATAL/MATERNAL)
# 1. Hypothermia: P07.3, P70.4, E16.2, P05.0, P05.1, P81.8, P81.9
# 2. Hypoglycaemia: P70.0, P70.1, P70.9, P80.8, P80.9, P58.1, P58.8, P59.0, P59.8, P59.9, P05.0, P05.1, P07.3, P08.0, P08.1, R63.4, P92.5, P92.2, P92.8, Q38.1 
# 3. Hyperbilirubinaemia: P07.3, D55.0, P55.0, P55.1, P12.0, Z83.2, P92.5, P92.2, P92.8, R63.4, Q38.1, P80.8, P80.9, P70.4, E16.2, P05.1 
# 4. Maternal diagnose: D55.0, O24.0, O24.1, O24.4, E10.90, E10.91, E11.20, E11.90, E11.91, E13.90, E13.91,E14.90)

Hypothermia_risk_icd <- c("E16.2", "P05.0", "P05.1", "P07.1", "P07.3", "P70.0", "P70.1", "P70.4", "P70.9", "P81.8", "P81.9")
Hypoglycaemia_risk_icd <- c("P05.0", "P05.1", "P07.1", "P07.3", "P08.0", "P08.1", "P58.1", "P58.8", "P59.0", "P59.8", "P59.9", "P80.0", "P80.8", "P80.9", "P92.0", "P92.2", "P92.5", "P92.8",
                            "P92.9", "Q38.1", "R63.4", "Z83.3")
Hyperbilirubinaemia_risk_icd <- c("D55.0", "E16.2", "P05.1", "P07.3", "P12.0", "P55.0", "P55.1", "P70.0", "P70.1", "P70.4", "P70.9", "P80.0", "P80.8", "P80.9", "P92.0", "P92.2", "P92.5", "P92.8",
                                  "P92.9", "Q38.1", "R63.4"," Z83.2", "Z83.3")
Maternal_risk_icd <- c("D55.0", "O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")

## ICD codes related to HHH summarised
# HHH_diagnoses_icd <- c(Hypothermia_icd, Hypoglycaemia_icd, Hyperbilirubinaemia_icd)
HHH_risk_icd_neonatal <- c(Hypothermia_risk_icd, Hypoglycaemia_risk_icd, Hyperbilirubinaemia_risk_icd)
HHH_risk_icd_maternal <- Maternal_risk_icd

## Building two groups
# Child: identify cases with relevant icd codes
Diag_neonatal <- Diagnose_red2_corr %>%
  rename(patient_id_child = patient_id, case_id_child = case_id) %>%
  filter(DIA_NK %in% HHH_risk_icd_neonatal) %>%
  distinct(patient_id_child, case_id_child) %>%
  mutate(has_neonatal_rf = TRUE)

# Mother: identify cases with relevant icd codes
Diag_maternal <- Diagnose_red2_corr %>%
  rename(patient_id_mother = patient_id, case_id_mother = case_id) %>%
  filter(DIA_NK %in% HHH_risk_icd_maternal) %>%
  distinct(patient_id_mother, case_id_mother) %>%
  mutate(has_maternal_rf = TRUE)

Sample3 <- left_join(Sample3, Diag_neonatal, by = c("patient_id_child", "case_id_child"))
Sample3 <- left_join(Sample3, Diag_maternal, by = c("patient_id_mother", "case_id_mother"))

Sample3 <- Sample3 %>% 
  mutate(has_neonatal_rf = replace_na(has_neonatal_rf, FALSE),
         has_maternal_rf = replace_na(has_maternal_rf, FALSE),
         has_risk_icd = has_neonatal_rf | has_maternal_rf,
         Risk_group = if_else(has_risk_icd, "With_RF", "Without_RF"))

table(Sample3$Risk_group)
# With_RF    Without_RF 
# 1900       3992

# Subset in newborn without and with risk factors
Newborn_group1 <- Sample3 %>% 
  filter(Risk_group %in% "Without_RF") %>% 
  select(- has_neonatal_rf, - has_maternal_rf)
Newborn_group2 <- Sample3 %>% 
  filter(Risk_group %in% "With_RF") %>% 
  select(- has_neonatal_rf, - has_maternal_rf)


# 3.4.7.1 Maternal Diabetes ----------------------------------------------
## Merge with Sample3 
Sample_maternal_rf <- left_join(Sample3, Diagnose_red2_corr, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>%
  select(- DIG_DATE_TS, - DIG_RANK, - DIG_TARGET_SITE, - DIA_BK, - ICD_groups) %>% 
  rename(DIA_NK_maternal= DIA_NK, ICD_labels_maternal = ICD_labels) # 45544 -> no mother has g6pd!

## ICD-10
# Maternal_RF_hypoglyc_icd <- c("O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")
# Maternal_RF_hyperbili_icd <- c("D55.0", "O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")
# Maternal_RF_g6pd <- "D55.0"
Maternal_RF_icd <- c("D55.0", "O24.0", "O24.1", "O24.4", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91","E14.90")
Maternal_GDM_icd <- c("O24.4")
Maternal_DM_icd <- c("O24.0", "O24.1", "E10.90", "E10.91", "E11.20", "E11.90", "E11.91", "E13.90", "E13.91", "E14.90")

# Identify cases with maternal risk factors
Maternal_RF_cases <- Sample_maternal_rf %>%
  filter(DIA_NK_maternal %in% Maternal_RF_icd) %>%
  distinct(patient_id_mother) %>%
  mutate(Has_maternal_rf = TRUE) # 741

# Identify mother with gdm and dm type
Maternal_DM <- Sample_maternal_rf %>%
  mutate(maternal_gdm = DIA_NK_maternal %in% Maternal_GDM_icd, maternal_DM  = DIA_NK_maternal %in% Maternal_DM_icd) %>%
  group_by(patient_id_mother) %>%
  summarise(maternal_gdm = any(DIA_NK_maternal %in% Maternal_GDM_icd), maternal_DM = any(DIA_NK_maternal %in% Maternal_DM_icd)) # 5299, --> logistical value (T/F)

Maternal_DM2 <- Sample_maternal_rf %>% 
  group_by(patient_id_mother) %>% 
  summarise(maternal_gdm = any(DIA_NK_maternal %in% Maternal_GDM_icd), maternal_DM = any(DIA_NK_maternal %in% Maternal_DM_icd)) %>%
  mutate(Maternal_diabetes_type = case_when(maternal_gdm == TRUE & maternal_DM == TRUE ~ "DM_GDM",
                                            maternal_gdm == TRUE ~ "GDM", 
                                            maternal_DM == TRUE ~ "preex_DM", TRUE ~ "No_diabetes")) # 

# Differentiated diabetes type for group 2
Newborn_group2 <- left_join(Newborn_group2, Maternal_DM2, by = "patient_id_mother") %>% 
  mutate(Maternal_diabetes_type = factor(Maternal_diabetes_type, levels = c("No_diabetes", "GDM", "preex_DM", "DM_GDM"))) %>% # as factor for analysis
  select(-maternal_DM, -maternal_gdm)

# Check for group 1 
Newborn_group1_test <- left_join(Newborn_group1, Maternal_DM2, by = "patient_id_mother") %>% 
  mutate(Maternal_diabetes_type = factor(Maternal_diabetes_type, levels = c("No_diabetes", "GDM", "preex_DM", "DM_GDM"))) %>% # as factor for analysis
  select(-maternal_DM, -maternal_gdm) # there are n= 42 cases with GDM! These are to be moved to group 2

GDM_mothers <- Maternal_DM2 %>% 
  filter(Maternal_diabetes_type == "GDM") %>%
  select(patient_id_mother)

Newborn_group1_c <- Newborn_group1 %>%
  anti_join(GDM_mothers, by = "patient_id_mother") # Gibt alle Beobachtungen des Data Frames x zurück, für die es keinen Match im Data Frame y gibt

GDM_cases_to_add <- Newborn_group1 %>%
  inner_join(GDM_mothers, by = "patient_id_mother") # 42

Newborn_group2_c <- bind_rows(Newborn_group2, GDM_cases_to_add)

Newborn_group2_c <- Newborn_group2_c %>%
  mutate(has_risk_icd = TRUE, Risk_group = "With_RF", Maternal_diabetes_type = replace_na(Maternal_diabetes_type, "GDM"))
## Conclusion: Newborn group without rf n= 3950 and Newborn group with rf n= 1942

rm(Diag_neonatal, Diag_maternal, GDM_cases_to_add, GDM_mothers, Maternal_DM, Maternal_DM2, Maternal_RF_cases, Newborn_group1, Newborn_group2, Newborn_group1_test, Sample2)

# 3.4.8 Other Maternal Influencing Factors ------------------------------------------------------

# 1. Hypoglycaemia: Maternal diabetes (O24.0, O24.1, O24.4, E10.90, E10.91, E11.20, E11.90, E11.91, E13.90, E13.91, E14.90), Caesariean secion (mode of delivery), Parity
# 2. Hyperbilirubinaemia: Maternal age, Maternal diabetes O24.0, O24.1, O24.4, E10.90, E10.91, E11.20, E11.90, E11.91, E13.90, E13.91, E14.90), Race/Ethnicity) -> D55.0
# 3. Hypothermia: Race/ethnicity, maternal age, parity, Ceasarean sectio (mode of delivery)

# 3.4.8.1 Parity ----------------------------------------------------------

Parity <- parity1 %>% 
  mutate(RF_parity = if_else(Anzahl_vorausg_LebGeb == 0, "Primi", "Multi"))

# Merge with Samples 
## Whole sample
Sample3 <- left_join(Sample3, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)

## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)

# Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)

# 3.4.8.2 Maternal age ----------------------------------------------------
Mother_data <- left_join(Sample3, Pat_info, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>% 
  select(patient_id_mother, case_id_mother, PAT_BIRTH_DATE, CBIS_BIRTH_DATE_TS, PAT_CITIZENSHIP_COUNTRY) # %>% 
  # distinct(patient_id_mother, .keep_all = TRUE)

Mother_data <- Mother_data %>%
  distinct(patient_id_mother, case_id_mother, .keep_all = TRUE) # remove duplicates, 6849

Mother_data <- Mother_data %>% 
  mutate(Birth_date = as.Date(CBIS_BIRTH_DATE_TS)) %>%
  mutate(RF_Maternal_age = interval(start = PAT_BIRTH_DATE, end = Birth_date) / duration(n = 1, unit = "years"))
summary(Mother_data$RF_Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.49   33.78   33.54   36.91   52.37

Underage <- Mother_data %>% 
  filter(RF_Maternal_age < 18) # 8 cases

# Merge together with samples
## Whole sample
Sample3 <- left_join(Sample3, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)

## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)
summary(Newborn_group1_c$RF_Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.90   30.50   33.67   33.48   36.84   51.03

## Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)
summary(Newborn_group2_c$RF_Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.45   33.96   33.69   37.07   52.37 

rm(Mother_data, Underage, Parity)

# 3.4.8.3 Origin --------------------------------------------------
## From Luisa 
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

# Combine with Samples
## Whole sample
Sample3<- left_join(Sample3, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) 

## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) 
table(Newborn_group1_c$Country)
# Africa     America      Asia      Europe    Oceania  Switzerland  Unknown 
# 131         105         249        1752     8        1695         10 
round(prop.table(table(as.factor(Newborn_group1_c$Country))) * 100, 1)
# Africa     America        Asia      Europe     Oceania Switzerland     Unknown 
# 3.3         2.7         6.3        44.4         0.2        42.9         0.3 

## Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) 
table(Newborn_group2_c$Country)
# Africa     America        Asia      Europe     Oceania Switzerland     Unknown 
# 95          57         196         797           4         790           3 

round(prop.table(table(as.factor(Newborn_group2_c$Country))) * 100, 1)
# Africa     America        Asia      Europe     Oceania Switzerland     Unknown 
# 4.9         2.9        10.1        41.0         0.2        40.7         0.2

rm(Countries_FOS, Countries_FOS1, Countries1, Countries2)


# 3.5 Operationalisation HHH ----------------------------------------------
# 3.5.1 Hypothermia categories ----------------------------------------
source("~/Thesis/Code/01_07_Code_Temp data.R")

## Exclude missing temperature data
# Whole sample
Missing_temp <- Sample3 %>% 
  filter(is.na(Hypothermia_cat))
## n= 1 Nicu admission, n= 6 ambulant/tagesklinik, 1 child no data entry
Sample3 <- Sample3 %>% 
  filter(!is.na(Hypothermia_cat)) # 5884

# Group 1
Missing_temp1 <- Newborn_group1_c %>% 
  filter(is.na(Hypothermia_cat))
## n= 5 ambulant/tagesklinik
Newborn_group1_c <- Newborn_group1_c %>%
  filter(!is.na(Hypothermia_cat)) # 3945

# Group 2
Missing_temp2 <- Newborn_group2_c %>% 
  filter(is.na(Hypothermia_cat))
## n= 1 ambulant/tagesklinik, n= 2 unknown

Newborn_group2_c <- Newborn_group2_c %>%
  filter(!is.na(Hypothermia_cat)) # 1844


# 3.5.2 Hypoglycaemia categories ----------------------------------------
# 3.5.3 Hyperbilirubinaemia categories ----------------------------------------
source("~/Thesis/Code/01_03_02_Code_Newborn_Meona.R")


# 3.5.4 Overview HHH  ---------------------------------------------------
## To have an overview if a child has none, one, two or all HHH diagnoses based on my categories
# Group 1
Newborn_group1_c <- Newborn_group1_c %>%
  rowwise() %>%  # to not sum all values in the whole column
  mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
         hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
           TRUE ~ "Unknown")) %>% 
  select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
table(Newborn_group1_c$HHH_diagnoses)
# Hyperbili_Hypothermia        Hyperbili_only        Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only       None 
# 1                            11                    16                           13                   724                  3180

# Group 2
Newborn_group2_c <- Newborn_group2_c %>%
  rowwise() %>%  # to not sum all values in the whole column
  mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
         hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
           TRUE ~ "Unknown")) %>% 
  select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
table(Newborn_group2_c$HHH_diagnoses)
# All_diagnoses        Hyperbili_Hypothermia        Hyperbili_only      Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only        None 
# 1                    10                           19                  62                           97                    486                  1264


# 4. Overview sample ------------------------------------------------------
## Whole sample
summary(Sample3)
# Whole sample
# To change LOS from mins to h: attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
Sample_final_all <- Sample3 %>%
  mutate(LOS = LOS/60)
attr(Sample_final_all$LOS, "units") <- "h"
 
# NICU admission
## Percentages Admission NICU
Sample_final_nicu <- Sample_final_all %>%
  filter(admission_neo_n == 1) # 46

Sample_final_all %>%
  ungroup() %>%
  summarise(total = n(),
            admitted = sum(admission_neo_n %in% 1),
            percent = (admitted / total) * 100)
#   <int>    <int>   <dbl>
#1  5884       46   0.782

# Group 1
# To change LOS from mins to h: attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
Group1_final <- Newborn_group1_c %>%
  mutate(LOS = LOS/60)
attr(Group1_final$LOS, "units") <- "h"

# NICU admission
## Percentages Admission NICU
Group1_final_nicu <- Group1_final %>%
  filter(admission_neo_n == 1) # 2

Group1_final %>%
  ungroup() %>%
  summarise(total = n(),
            admitted = sum(admission_neo_n %in% 1),
            percent = (admitted / total) * 100)
#  total  admitted percent
#  <int>   <int>   <dbl>
#1  3945      2    0.0507

# Group 2
# To change LOS from mins to h: attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
Group2_final <- Newborn_group2_c %>%
  mutate(LOS = LOS/60)
attr(Group2_final$LOS, "units") <- "h"

# NICU admission
## Percentages Admission NICU
Group2_final_nicu <- Group2_final %>%
  filter(admission_neo_n == 1) # 44

Group2_final %>%
  ungroup() %>%
  summarise(total = n(),
            admitted = sum(admission_neo_n %in% 1),
            percent = (admitted / total) * 100)
#  total admitted percent
#  <int>    <int>   <dbl>
#1  1939      44    2.27


# Overview: Newborns admitted Nicu
## GA
# table(Sample_final_nicu$Weeks_LPM)
# # 37 38 39 40 41 42 
# # 2 13 14 11  6  1 
# round(prop.table(table(Sample_final_nicu$Weeks_LPM)) * 100, 1)
# # 37   38   39   40   41   42 
# # 4.3 27.7 29.8 23.4 12.8  2.1 
# 
# table(Sample_final_nicu$weight_cat)
# # 2500-2999 3000-3500 3500-4000  4000-4500 
# # 10        14        19         4 
# 
# table(Sample_final_nicu$LOS) # unübersichtlich
# Sample_final_nicu <- Sample_final_nicu %>%
#   mutate(LOS_cat = case_when(
#     LOS < 6 ~ "<6h",
#     LOS < 12 ~ "6–12h",
#     LOS < 24 ~ "12–24h",
#     LOS < 48 ~ "24–48h",
#     LOS < 72 ~ "48–72h",
#     TRUE ~ "≥72h"))
# table(Sample_final_nicu$LOS_cat)
# # <6h   ≥72h 12–24h 24–48h 48–72h  6–12h 
# # 8      7     12     10      5      5 
# round(prop.table(table(Sample_final_nicu$LOS_cat)) * 100, 1)
# # <6h   ≥72h   12–24h  24–48h  48–72h   6–12h 
# # 17.0   14.9   25.5    21.3    10.6    10.6 
# 
# table(Sample_final_nicu$RF_parity)
# # Multi Primi 
# # 19    28
# round(prop.table(table(Sample_final_nicu$RF_parity)) * 100, 1)
# # Multi Primi 
# # 40.4  59.6 
# 
# table(Sample_final_nicu$RF_Maternal_age)
# Sample_final_nicu %>%
#   mutate(Maternal_age_group = case_when(
#     RF_Maternal_age < 20 ~ "<20",
#     RF_Maternal_age < 25 ~ "20–24",
#     RF_Maternal_age < 30 ~ "25–29",
#     RF_Maternal_age < 35 ~ "30–34",
#     RF_Maternal_age < 40 ~ "35–39",
#     TRUE ~ "≥40")) %>%
#   count(Maternal_age_group) %>%
#   ungroup() %>% 
#   mutate(percent = round(n/sum(n) * 100, 1)) %>%
#   arrange(Maternal_age_group)
# 
# # Maternal_age_group  n percent
# # <chr>              <int>   <dbl>
# # 1 20–24               5    10.6
# # 2 25–29               7    14.9
# # 3 30–34               16   34  
# # 4 35–39               14   29.8
# # 5 ≥40                 5    10.6
# 
# table(Sample_final_nicu$Country)
# # Africa      Asia      Europe  Switzerland     Unknown 
# # 4           2          21          19           1 
# 
# table(Sample_final_nicu$Maternal_diabetes_type)
# # No_diabetes  GDM    preex_DM    DM_GDM 
# # 38           9      0           0 
# 
# table(Sample_final_nicu$HHH_diagnoses)
# # Hyperbili_only Hypoglyc_Hypothermia        Hypoglyc_only       Hypotherm_only      None 
# # 7                   11                    4                    8                   17 
# 
# table(Sample_final_nicu$Hypothermia_cat)
# # Mild   Moderate_Severe    Norm
# # 4      15                 28
# 
# table(Sample_final_nicu$Hypoglycaemia_cat)
# # Mild       Moderate   No_measurement   Normoglycaemic   Severe 
# # 1             0           14            18              14
# 
# table(Sample_final_nicu$Hyperbilirubinaemia_cat)
# # Hyperbilirubinaemia_serum   Hyperbilirubinaemia_tcb         No_measurement        Physiological 
# # 5                           2                               17                    23 
# 
# ## Newborn Group 1
# summary(Newborn_group1)
# Newborn_group1_final <- Newborn_group1%>%
#   mutate(LOS = LOS/60)
# attr(Newborn_group1_final$LOS, "units") <- "h"
# 
# # NICU admission
# ## Percentages Admission NICU
# Newborn_group1_final_nicu <- Newborn_group1_final %>%
#   filter(admission_neo_n == 1) # 11
# 
# Newborn_group1_final %>%
#   ungroup() %>%
#   summarise(total = n(),
#             admitted = sum(admission_neo_n %in% 1),
#             percent = (admitted / total) * 100)
# # total admitted percent
# # <int>    <int>   <dbl>
# #1 136       11    8.09

# Overview: Newborns admitted Nicu
## GA
# table(Sample_final_nicu$Weeks_LPM)
# # 37 38 39 40 41 42 
# # 2 13 14 11  6  1 
# round(prop.table(table(Sample_final_nicu$Weeks_LPM)) * 100, 1)
# # 37   38   39   40   41   42 
# # 4.3 27.7 29.8 23.4 12.8  2.1 
# 
# table(Sample_final_nicu$weight_cat)
# # 2500-2999 3000-3500 3500-4000  4000-4500 
# # 10        14        19         4 
# 
# table(Sample_final_nicu$LOS) # unübersichtlich
# Sample_final_nicu <- Sample_final_nicu %>%
#   mutate(LOS_cat = case_when(
#     LOS < 6 ~ "<6h",
#     LOS < 12 ~ "6–12h",
#     LOS < 24 ~ "12–24h",
#     LOS < 48 ~ "24–48h",
#     LOS < 72 ~ "48–72h",
#     TRUE ~ "≥72h"))
# table(Sample_final_nicu$LOS_cat)
# # <6h   ≥72h 12–24h 24–48h 48–72h  6–12h 
# # 8      7     12     10      5      5 
# round(prop.table(table(Sample_final_nicu$LOS_cat)) * 100, 1)
# # <6h   ≥72h   12–24h  24–48h  48–72h   6–12h 
# # 17.0   14.9   25.5    21.3    10.6    10.6 
# 
# table(Sample_final_nicu$RF_parity)
# # Multi Primi 
# # 19    28
# round(prop.table(table(Sample_final_nicu$RF_parity)) * 100, 1)
# # Multi Primi 
# # 40.4  59.6 
# 
# table(Sample_final_nicu$RF_Maternal_age)
# Sample_final_nicu %>%
#   mutate(Maternal_age_group = case_when(
#     RF_Maternal_age < 20 ~ "<20",
#     RF_Maternal_age < 25 ~ "20–24",
#     RF_Maternal_age < 30 ~ "25–29",
#     RF_Maternal_age < 35 ~ "30–34",
#     RF_Maternal_age < 40 ~ "35–39",
#     TRUE ~ "≥40")) %>%
#   count(Maternal_age_group) %>%
#   ungroup() %>% 
#   mutate(percent = round(n/sum(n) * 100, 1)) %>%
#   arrange(Maternal_age_group)
# 
# # Maternal_age_group  n percent
# # <chr>              <int>   <dbl>
# # 1 20–24               5    10.6
# # 2 25–29               7    14.9
# # 3 30–34               16   34  
# # 4 35–39               14   29.8
# # 5 ≥40                 5    10.6
# 
# table(Sample_final_nicu$Country)
# # Africa      Asia      Europe  Switzerland     Unknown 
# # 4           2          21          19           1 
# 
# table(Sample_final_nicu$Maternal_diabetes_type)
# # No_diabetes  GDM    preex_DM    DM_GDM 
# # 38           9      0           0 
# 
# table(Sample_final_nicu$HHH_diagnoses)
# # Hyperbili_only Hypoglyc_Hypothermia        Hypoglyc_only       Hypotherm_only      None 
# # 7                   11                    4                    8                   17 
# 
# table(Sample_final_nicu$Hypothermia_cat)
# # Mild   Moderate_Severe    Norm
# # 4      15                 28
# 
# table(Sample_final_nicu$Hypoglycaemia_cat)
# # Mild       Moderate   No_measurement   Normoglycaemic   Severe 
# # 1             0           14            18              14
# 
# table(Sample_final_nicu$Hyperbilirubinaemia_cat)
# # Hyperbilirubinaemia_serum   Hyperbilirubinaemia_tcb         No_measurement        Physiological 
# # 5                           2                               17                    23 




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



# Sample_without_rf <- left_join(Sample2, Newborn_dia_red, by = c("patient_id_child", "case_id_child")) # 6836
# 
# # identify newborns with hypoglycaemia and hyperbili rf
# Neonates_hypogl_risk <- Sample2_dia_neonatal %>% 
#   filter(DIA_NK %in% Hypoglycaemia_risk_codes) %>% 
#   distinct(patient_id_child, case_id_child) %>% 
#   mutate(Hypoglycaemia_risk = T) # 837
# 
# Neonates_hyperbili_risk <- Sample2_dia_neonatal %>% 
#   filter(DIA_NK %in% Hyperbilirubinaemia_rik_codes) %>% 
#   distinct(patient_id_child, case_id_child) %>% 
#   mutate(Hyperbilirubin_risk = T) # 12
# 
# # Join together with Sample
# Sample_test2 <- left_join(Sample_test, Neonates_hypogl_risk, by = c("patient_id_child", "case_id_child")) %>%
#   mutate(Hypoglycaemia_risk = if_else(is.na(Hypoglycaemia_risk), F, Hypoglycaemia_risk)) 
# 
# Sample_test2 <- left_join(Sample_test2 , Neonates_hyperbili_risk, by = c("patient_id_child", "case_id_child")) %>% 
#   mutate(Hyperbilirubin_risk = if_else(is.na(Hyperbilirubin_risk), F, Hyperbilirubin_risk))
# 
# # Check id
# Id_check_Sample_test2 <- Sample_test2 %>%
#   select(patient_id_child, case_id_child) %>%
#   distinct() # 6836
# 
# 
# Sample2_dia_neonatal <- left_join(Sample2, Diagnose_red2_corr, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   select(patient_id_child, case_id_child, DIA_NK, ICD_labels)



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
# 
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

# # Sample_test3 <- left_join(Sample_test, Mother_data, by = c("patient_id_mother", "case_id_mother", "CBIS_BIRTH_DATE_TS")) %>% 
#   select(- Birth_date, - PAT_BIRTH_DATE)
# 
# summary(Sample_test3$RF_Maternal_age)
# # Min.     1st Qu.  Median  Mean   3rd Qu.  Max.      NA's 
# #   14.55   30.50   33.73   33.52   36.87   52.37      54 
# # Identify newborns admitted to NICU based on categories
# Summary_neonatal_Nicu_hhh <- Sample_final_all %>%
#   group_by(Hypothermia_cat, Hypoglycaemia_cat, Hyperbili_cat) %>%
#   summarise(n_total = n(),
#             n_admitted = sum(admission_neo_n == 1),
#             percent = (n_admitted / n_total) * 100)
# 
# print(Summary_neonatal_Nicu_hhh, n = Inf)

# Identify newborns with documented HHH-icd-10 -> no more relevant!
# Newborn_icd_HHH <- Diagnose_red2_corr %>% 
#   mutate(Doc_Hypothermia = DIA_NK %in% Hypothermia_icd,
#          Doc_Hypoglycaemia = DIA_NK %in% Hypoglycaemia_icd,
#          Doc_Hyperbilirubinaemia = DIA_NK %in% Hyperbilirubinaemia_icd) %>% 
#   group_by(patient_id, case_id) %>% 
#   summarise(Doc_Hypothermia = any(Doc_Hypothermia),
#             Doc_Hypoglycaemia = any(Doc_Hypoglycaemia),
#             Doc_Hyperbilirubinaemia = any(Doc_Hyperbilirubinaemia))
# 
# Sample3 <- left_join(Sample3, Newborn_icd_HHH, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# # Grouping of newborns into HHH/risk factor groups
# # Merge Sample 3 (cleaned sample set) with diagnoses data to start
# Sample3_filtered_icd <- left_join(Sample3, Diagnose_red2_corr, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   select(patient_id_child, case_id_child, DIA_NK, ICD_labels, patient_id_mother, case_id_mother, CBIS_BIRTH_DATE_TS, Weeks_LPM, Days_LPM, gestational_age_total_days, 
#          Birth_weight_g, weight_cat, admission_postnatalunit, discharge_postnatalunit, LOS, admission_neo, admission_neo_n)
# 
# # Identify newborns with a H diagnosis 
# Newborn_with_HHH <- Sample3_filtered_icd %>% 
#   group_by(patient_id_child, case_id_child) %>% 
#   filter(any(DIA_NK %in% HHH_diagnoses_icd)) %>% 
#   ungroup() # 775 
# 
# Newborn_with_risk <- Sample3_filtered_icd %>% 
#   group_by(patient_id_child, case_id_child) %>% 
#   filter(any(DIA_NK %in% HHH_risk_icd)) %>% 
#   ungroup() # 4245; but those codes that are exclusively risk factors but are not themselves HHH diagnoses need to be identified, because Hs influence each other
# 
# # Clean up the risk factor list - remove the HHH diagnoses from the neonatal risk factors
# Newborn_with_rf <- setdiff(HHH_risk_icd, HHH_diagnoses_icd) # Calculates the difference between two vectors
# 
# # Build all newborns to go further the next steps
# All_newborns <- Sample3_filtered_icd %>% 
#   group_by(patient_id_child, case_id_child) # %>% # 8899
# # distinct(patient_id_child, case_id_child) # 5726
# 
# # Newborns with HHH diagnoses but without other relevant neonatal risk factors, group 1
# Newborn_with_HHH_no_rf <- All_newborns %>%
#   filter(any(DIA_NK %in% HHH_diagnoses_icd) & !any(DIA_NK %in% Newborn_with_rf)) %>% # at least on H diagnosis, no rf
#   ungroup() # 308
# 
# Newborn_with_HHH_no_rf_Check_id <- Newborn_with_HHH_no_rf %>% 
#   select(patient_id_child, case_id_child) %>% 
#   distinct # 136
# 
# # Newborns with HHH diagnoses and other neonatal rf relevant to HHH, group 2
# Newborn_with_HHH_and_rf <- All_newborns %>%
#   filter(any(DIA_NK %in% HHH_diagnoses_icd) & any(DIA_NK %in% Newborn_with_rf)) %>%
#   ungroup() # 467
# 
# Newborn_with_HHH_and_rf_Check_id <- Newborn_with_HHH_and_rf %>% 
#   select(patient_id_child, case_id_child) %>% 
#   distinct # 137
# 
# # Newborns without a HHH diagnosis but neonatal rf relevant to HHH, group 3
# Newborn_no_HHH_with_rf <- All_newborns %>%
#   filter(!any(DIA_NK %in% HHH_diagnoses_icd) & any(DIA_NK %in% Newborn_with_rf)) %>%
#   ungroup() # 3470
# 
# Newborn_no_HHH_with_rf_Check_id <- Newborn_no_HHH_with_rf %>% 
#   select(patient_id_child, case_id_child) %>% 
#   distinct # 1486
# 
# # Newborns neither a HHH diagnosis nor a neonatal rf relevant for HHH group 4
# Newborn_fully_healthy <- All_newborns %>%
#   filter(!any(DIA_NK %in% HHH_diagnoses_icd) & !any(DIA_NK %in% Newborn_with_rf)) %>%
#   ungroup() # 4654
# 
# Newborn_fully_healthy_Check_id <- Newborn_fully_healthy %>% 
#   select(patient_id_child, case_id_child) %>% 
#   distinct # 3967
# 
# # Build newborn groups
# Newborn_group1 <- Newborn_with_HHH_no_rf %>% distinct(patient_id_child, case_id_child, .keep_all = TRUE) %>% 
#   select(- DIA_NK, - ICD_labels)
# Newborn_group2 <- Newborn_with_HHH_and_rf %>% distinct(patient_id_child, case_id_child, .keep_all = TRUE) %>% 
#   select(- DIA_NK, - ICD_labels)
# Newborn_group3 <- Newborn_no_HHH_with_rf %>% distinct(patient_id_child, case_id_child, .keep_all = TRUE) %>% 
#   select(- DIA_NK, - ICD_labels)
# Newborn_group4 <- Newborn_fully_healthy %>% distinct(patient_id_child, case_id_child, .keep_all = TRUE) %>% 
#   select(- DIA_NK, - ICD_labels)
# 
# # Add variables Newborn_icd_HHH
# Newborn_group1 <- left_join(Newborn_group1, Newborn_icd_HHH, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# Newborn_group2 <- left_join(Newborn_group2, Newborn_icd_HHH, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# Newborn_group3 <- left_join(Newborn_group3, Newborn_icd_HHH, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# Newborn_group4 <- left_join(Newborn_group4, Newborn_icd_HHH, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id"))
# 
# 
# rm(Newborn_with_HHH_no_rf_Check_id, Newborn_with_HHH_and_rf_Check_id, Newborn_no_HHH_with_rf_Check_id, Newborn_fully_healthy_Check_id, Sample3_filtered_icd)

# Simple with yes or no maternal RF
# Sample3_j <- left_join(Sample3, Maternal_RF_cases, by = "patient_id_mother") %>%
#   mutate(Has_maternal_rf = if_else(is.na(Has_maternal_rf), "No", "Yes"))

# Compare categories with if its documented as diagnosis or not 
## Hypothermia
# table(Sample4$Hypothermia_cat, Sample4$Doc_Hypothermia)
# #                 FALSE TRUE
# # Mild              792   24
# # Moderate_Severe   360   86
# # Norm             4451    5
# 
# Table_hypotherm <- table(Sample4$Hypothermia_cat, Sample4$Doc_Hypothermia)
# round(prop.table(Table_hypotherm) * 100, 1) # total percentage distribution in relation to the entire data set
# #                 FALSE TRUE
# # Mild             13.9  0.4 -> 13.9 % of the newborns had mild hypothermia without a diagnosis, 0.4 % with a diagnosis.
# # Moderate_Severe   6.3  1.5 -> 1.5 % of newborns had Moderate/Severe with a diagnosis
# # Norm             77.8  0.1 -> although normotherm there were documented diagnosis
# round(prop.table(Table_hypotherm, margin = 1) * 100, 1) # per row, how much newborns within a category were documented/not documented icd
# #                 FALSE TRUE
# # Mild             97.1  2.9 -> 2.9 % of newborns with mild hypothermia had a documented ICD diagnosis
# # Moderate_Severe  80.7 19.3
# # Norm             99.9  0.1
# 
# # Janitor: make a nice table
# # Sample4 %>%
# #   tabyl(Hypothermia_cat, Doc_Hypothermia) %>%
# #   adorn_percentages("row") %>%
# #   adorn_pct_formatting(digits = 1) %>%
# #   adorn_ns() -> Hypotherm_table
# # 
# # write.csv(Hypotherm_table, "Hypothermia_table1.csv", row.names = FALSE)
# 
# ## Hypoglycaemia
# table(Sample4$Hypoglycaemia_cat, Sample4$Doc_Hypoglycaemia)
# #                 FALSE TRUE
# # Mild              52   69
# # Moderate           6   10
# # No_measurement  3749    2
# # Normoglycaemic  1760   20
# # Severe            10   40
# 
# Table_Hypoglyc <- table(Sample4$Hypoglycaemia_cat, Sample4$Doc_Hypoglycaemia)
# round(prop.table(Table_Hypoglyc) * 100, 1) 
# #                 FALSE TRUE
# # Mild             0.9  1.2 -> 1.2 % of all newborns had mild hypoglycaemia and an ICD diagnosis
# # Moderate         0.1  0.2
# # No_measurement  65.6  0.0
# # Normoglycaemic  30.8  0.3 -> 0.3 % with a diagnosis, although newborns are classified as normoglycaemic
# # Severe           0.2  0.7
# round(prop.table(Table_Hypoglyc, margin = 1) * 100, 1) # per row, how much newborns within a category were documented/not documented icd
# #                 FALSE TRUE
# # Mild            43.0 57.0 -> half was documented
# # Moderate        37.5 62.5 -> much more documented
# # No_measurement  99.9  0.1
# # Normoglycaemic  98.9  1.1
# # Severe          20.0 80.0 -> 80% were documented cases
# 
# ## Hyperbilirubinaemia
# table(Sample4$Hyperbilirubinaemia_cat, Sample4$Doc_Hyperbilirubinaemia)
# #                           FALSE TRUE
# # Hyperbilirubinaemia_serum    24   12
# # Hyperbilirubinaemia_tcb       2    2
# # No_measurement              554    1
# # Physiological              5101   22
# 
# Table_Hyperbil <- table(Sample4$Hyperbilirubinaemia_cat, Sample4$Doc_Hyperbilirubinaemia)
# round(prop.table(Table_Hyperbil) * 100, 1) 
# #                           FALSE TRUE
# # Hyperbilirubinaemia_serum   0.4  0.2 
# # Hyperbilirubinaemia_tcb     0.0  0.0
# # No_measurement              9.7  0.0
# # Physiological              89.2  0.4 -> 0.4% had a documented diagnosis although physiological based on the categorisation
# round(prop.table(Table_Hyperbil, margin = 1) * 100, 1)
# #                            FALSE TRUE
# # Hyperbilirubinaemia_serum  66.7 33.3 -> only 33.3 % of clinically abnormal cases (serum-based) were actually coded as hyperbilirubinaemia
# # Hyperbilirubinaemia_tcb    50.0 50.0
# # No_measurement             99.8  0.2
# # Physiological              99.6  0.4
# 
# table(Sample4$Phototherapy_status, Sample4$Doc_Hyperbilirubinaemia)
# #                               FALSE TRUE
# # Indicated_AND_Documented         4    8
# # Indicated_BUT_Not_Documented    22    6
# # No_measurement                 554    1
# # Not_Indicated_BUT_Documented     1   11
# # Not_Indicated_NOT_Documented  5100   11
# 
# Table_Hyperbili_Photo <- table(Sample4$Phototherapy_status, Sample4$Doc_Hyperbilirubinaemia)
# round(prop.table(Table_Hyperbili_Photo) * 100, 1) 
# #                               FALSE TRUE
# # Indicated_AND_Documented       0.1  0.1
# # Indicated_BUT_Not_Documented   0.4  0.1
# # No_measurement                 9.7  0.0
# # Not_Indicated_BUT_Documented   0.0  0.2
# # Not_Indicated_NOT_Documented  89.2  0.2
# 
# Sample4_check <- Sample4 %>%
#   filter(Phototherapy_status == "No_measurement", Doc_Hyperbilirubinaemia == TRUE)
# Newborn Groups
## Group 1
# Newborn_group1 <- Newborn_group1 %>%
#   rowwise() %>%  # to not sum all values in the whole column
#   mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
#          hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
#            TRUE ~ "Unknown")) %>% 
#   select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
# table(Newborn_group1$HHH_diagnoses)
# # Hyperbili_Hypothermia    Hyperbili_only   Hypoglyc_Hypothermia    Hypoglyc_only       Hypotherm_only        None 
# # 2                        5                19                      20                  64                    26 
# 
# 
# 
# ## Group 2
# Newborn_group2 <- Newborn_group2 %>%
#   rowwise() %>%  # to not sum all values in the whole column
#   mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
#          hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
#            TRUE ~ "Unknown")) %>% 
#   select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
# table(Newborn_group2$HHH_diagnoses)
# # Hyperbili_Hypothermia     Hyperbili_only       Hypoglyc_Hypothermia         Hypoglyc_only       Hypotherm_only        None 
# # 3                         6                    35                           45                  35                    12 
# 
# ## Group 3
# Newborn_group3 <- Newborn_group3 %>%
#   rowwise() %>%  # to not sum all values in the whole column
#   mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
#          hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
#            TRUE ~ "Unknown")) %>% 
#   select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
# table(Newborn_group3$HHH_diagnoses)
# # All_diagnoses         Hyperbili_Hypothermia       Hyperbili_only        Hypoglyc_Hypothermia        Hypoglyc_only        Hypotherm_only       None 
# # 1                     4                           7                     6                           31                   348                  1087 
# 
# ## Group 4
# Newborn_group4 <- Newborn_group4 %>%
#   rowwise() %>%  # to not sum all values in the whole column
#   mutate(hypoglyc_y = Hypoglycaemia_cat %in% c("Mild", "Moderate", "Severe"),
#          hyperbili_y = if_else(is.na(Hyperbilirubinaemia_cat), FALSE, Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb")),
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
#            TRUE ~ "Unknown")) %>% 
#   select(- hypoglyc_y, - hypotherm_y, - hyperbili_y, - symptom_count)
# table(Newborn_group4$HHH_diagnoses)
# # Hyperbili_Hypothermia     Hyperbili_only        Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only       None 
# # 2                         10                    16                           14                   727                  3193

# 1. Install packages ------------------------------------------------------
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("feather")

# 2. Import data sets --------------------------------------------------------
# Temp_data <- read_feather("I:/Verwaltung/MaNtiS/02_Pseudonymisierte_Daten/Temp_pseud.feather")


# 3. Overview Temp data --------------------------------------------------------
summary(Temp_data) # 211808
sum(is.na(Temp_data$patient_id)) # there are no NAs
sum(is.na(Temp_data$case_id)) # 15040

Temp_data2 <- left_join(Sample3, Temp_data, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  select(patient_id_child, case_id_child, VVMO_NUMERIC_VALUE, VVMO_MEASURE_DATE_TS)
# 67099 obs

summary(Temp_data2)

# Patient_id, case_id
sum(is.na(Temp_data2$patient_id_child)) # 0
sum(is.na(Temp_data2$case_id_child)) # 0

# VVMO_NUMERIC_VALUE
summary(Temp_data2$VVMO_NUMERIC_VALUE)
# Min.    1st Qu.   Median      Mean    3rd Qu.      Max.       NA's 
# 27.10   36.90     37.10       37.08   37.30       38.90      12  
## Conclusion: NAs were mainly due to outpatient births and transfers to the Nicu on the same day of birth -> now not present anymore due to sample selection before in Sample2c

# To exclude missing data in temperature records
Temp_data2 <- Temp_data2 %>%
  filter(!is.na(VVMO_NUMERIC_VALUE))

summary(Temp_data2)

# Check Id
Check_pat_id_temp <- Temp_data2 %>% 
  select(patient_id_child) %>% 
  distinct
## 6824

Check_case_id_temp <- Temp_data2 %>% 
  select(case_id_child) %>% 
  distinct
## 6824

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
  filter(VVMO_NUMERIC_VALUE < 36.5) # 2200

# Temp_low_adm <- Temp_data2 %>%
#   filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes")
# Temp_low_adm2 <- Temp_data2 %>%
#   filter(VVMO_NUMERIC_VALUE < 36.5, admission_neo %in% "Yes") %>% 
#   select(patient_id_child) %>%
#   distinct() 

# 4. Operationalisation --------------------------------------------------
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
# Mild      Moderate_Severe     Norm 
# 982             532           5310 

round(prop.table(table(as.factor(Temp_data_cat2$Hypothermia_cat))) * 100, 1)
# Mild       Moderate_Severe      Norm 
# 14.4             7.8            77.8

## For analysis: Hypothermia_cat

rm(Check_case_id_temp, Check_pat_id_temp, Temp_low, Temp_data, Temp_data_cat)


# 5. Merging with sample data-----------------------------------------------------------------
## Whole sample
Sample3 <- left_join(Sample3, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) %>% 
  select(- VVMO_NUMERIC_VALUE, - VVMO_MEASURE_DATE_TS)

## Join with Groups
Newborn_group1_c <- left_join(Newborn_group1_c, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) %>% 
  select(- VVMO_NUMERIC_VALUE, - VVMO_MEASURE_DATE_TS)

Newborn_group2_c <- left_join(Newborn_group2_c, Temp_data_cat2, by = c("patient_id_child", "case_id_child")) %>% 
  select(- VVMO_NUMERIC_VALUE, - VVMO_MEASURE_DATE_TS)

summary(as.factor(Newborn_group1_c$Hypothermia_cat))
# Mild    Moderate_Severe        Norm         NA's 
# 510     211                     3153         5 
## 

round(prop.table(table(as.factor(Newborn_group1_c$Hypothermia_cat))) * 100, 1)
# Mild     Moderate_Severe  Norm 
# 13.2         5.4          81.4

Hypotherm_Nicu1 <- Newborn_group1_c %>%
  filter(admission_neo %in% "Yes", Hypothermia_cat %in% c("Norm", "Mild", "Moderate_Severe")) # 7

summary(as.factor(Newborn_group2_c$Hypothermia_cat))
# Mild    Moderate_Severe       Norm         NA's 
# 306     235                   1303         3 
## 

round(prop.table(table(as.factor(Newborn_group2_c$Hypothermia_cat))) * 100, 1)
# Mild     Moderate_Severe  Norm 
# 16.6         12.7         70.7

Hypotherm_Nicu2 <- Newborn_group2_c %>%
  filter(admission_neo %in% "Yes", Hypothermia_cat %in% c("Norm", "Mild", "Moderate_Severe")) # 40

# 1. Install packages ------------------------------------------------------
# install.packages("tidyverse") 
library(tidyverse)
# install.packages("feather")
library(feather)

# 2. Import data set --------------------------------------------------------
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2.RData")
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2_reduced.RData")
# load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Meona_corr2_2025-03-06.RData")

# Meona_corr2_reduced: Exclude 1) nicht am USB geboren wurden 2) readmissions waren 3) verstorben sind

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


# 4.1.1 Categorisation ----------------------------------------------------
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
# Hypoglyc_cat2 <- Lookup_hypoglyc2 %>%
#   group_by(patient_id, case_id) %>%
#   select(patient_id, case_id, pat_geb_dat, alter_in_tagen, order_text, dokumentation, Hypoglycaemia, Hypoglycaemia_severe) %>%
#   filter(alter_in_tagen <= 3) %>%
#   group_by(patient_id, case_id) %>%
#   summarise(Episode_Hypoglycaemia = sum(Hypoglycaemia),
#             Episode_Hypoglycaemia_severe = sum(Hypoglycaemia_severe)) %>%
#   mutate(Category_Hypoglycaemia = case_when(
#       Episode_Hypoglycaemia_severe >= 1 ~ "Severe",    
#       Episode_Hypoglycaemia == 1 ~ "Mild",             
#       Episode_Hypoglycaemia == 2 ~ "Moderate",         
#       Episode_Hypoglycaemia > 2 ~ "Severe",            
#       Episode_Hypoglycaemia == 0 ~ "Normoglyc"))

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

## Next step: merging Hypoglyc_data_cat with newborn groups (attention: without variable birth date! It doesn't work with these variables, 
## pat_geb_dat is also a little different than CBIS_BIRTH_DATE_TS)
Sample3 <- left_join(Sample3, Hypoglyc_data_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hypoglycaemia_cat = if_else(is.na(Hypoglycaemia_cat), "No_measurement", Hypoglycaemia_cat)) %>% 
  select(- pat_geb_dat, - alter_in_tagen)

Newborn_group1_c <- left_join(Newborn_group1_c, Hypoglyc_data_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hypoglycaemia_cat = if_else(is.na(Hypoglycaemia_cat), "No_measurement", Hypoglycaemia_cat)) %>% 
  select(- pat_geb_dat, - alter_in_tagen)

Newborn_group2_c <- left_join(Newborn_group2_c, Hypoglyc_data_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hypoglycaemia_cat = if_else(is.na(Hypoglycaemia_cat), "No_measurement", Hypoglycaemia_cat)) %>% 
  select(- pat_geb_dat, - alter_in_tagen)

table(Newborn_group1_c$Hypoglycaemia_cat)
# Mild       Moderate   No_measurement    Normoglycaemic      Severe 
# 24              1           3079            766              4 
round(prop.table(table(as.factor(Newborn_group1_c$Hypoglycaemia_cat))) * 100, 1)
# Mild       Moderate No_measurement Normoglycaemic         Severe 
# 0.6            0.0           79.5           19.8            0.1 

Hypoglyc_neo1 <- Newborn_group1_c %>%
  filter(admission_neo %in% "Yes", Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe")) # 3 cases

table(Newborn_group2_c$Hypoglycaemia_cat)
# Mild       Moderate   No_measurement    Normoglycaemic      Severe 
# 97              15           672           1014              46 
# round(prop.table(table(as.factor(Newborn_group2_c$Hypoglycaemia_cat))) * 100, 1)
# Mild       Moderate No_measurement Normoglycaemic         Severe 
# 5.3            0.8           36.4           55.0            2.5 

Hypoglyc_neo2 <- Newborn_group2_c %>%
  filter(admission_neo %in% "Yes", Hypoglycaemia_cat %in% c("Normoglycaemic", "Mild", "Moderate", "Severe")) # 30 cases

rm(Bloodmeasure_n, Bloodmeasure_y)


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


# 4.2.1 USB Categorisation ----------------------------------------------------
# CATEGORISATION (Guideline USB)
## Build categories based on USB bilirubin threshold
Bili_cat_USB <- Lookup_bili %>% 
  mutate(Bili_threshold = case_when(Age_in_hours < 24 ~ 150,
                                    Age_in_hours < 48 ~ 250, 
                                    Age_in_hours < 72 ~ 300, 
                                    Age_in_hours >= 72 ~ 350)) %>% 
  mutate(Treatment_required_Hyperbili = if_else(Value_umol_l >= Bili_threshold, "Yes", "No")) # 235678

table(Bili_cat_USB$Treatment_required_Hyperbili)
# No      Yes 
# 23465   113 

# Check regarding bilirubin type (TCB, Serum) and build categories
Bili_cat_USB_measure <- Bili_cat_USB %>%
  mutate(Bili_levels = case_when(
    bilirubin_type == "TRANSCUTAN" & Value_umol_l < Bili_threshold ~ "bili_tc_norm",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_threshold ~ "bili_tc_photo",
    bilirubin_type == "SERUM" & Value_umol_l < Bili_threshold ~ "bili_serum_norm",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_threshold ~ "bili_serum_photo"))

table(Bili_cat_USB_measure$Bili_levels)
# bili_serum_norm   bili_serum_photo     bili_tc_norm    bili_tc_photo 
# 2198              26                   21267           87 

# Build categories based on measurement (tcb-serum = serum, tcb-no serum = tcb, no tcb-serum = serum)
Bili_cat_USB_measure2 <- Bili_cat_USB_measure %>%
  group_by(patient_id, case_id) %>% 
  mutate(has_tc_photo = any(Bili_levels == "bili_tc_photo"),
         has_serum_photo = any(Bili_levels == "bili_serum_photo"),
         has_serum_norm = any(Bili_levels == "bili_serum_norm"),
         has_any_serum = has_serum_photo | has_serum_norm,
         Hyperbilirubinaemia_cat = case_when(
           has_tc_photo & has_any_serum ~ "Hyperbilirubinaemia_serum",
           has_tc_photo & !has_any_serum ~ "Hyperbilirubinaemia_tcb",
           TRUE ~ "Physiological")) %>%
  ungroup()


# 4.2.2 Phototherapy ------------------------------------------------------
# Identify reported cases for phototherapy started
Bili_procedure <- left_join(Bili_cat_USB_measure2, Procedure_red2_corr, by = c("patient_id", "case_id"), relationship = "many-to-many") %>%
  select(patient_id, case_id, Hyperbilirubinaemia_cat, procedure_label)

Photo_USB <- Bili_procedure %>% 
  group_by(patient_id, case_id, Hyperbilirubinaemia_cat) %>% 
  summarise(has_photo_doc = any(procedure_label == "Sonstige Phototherapie", na.rm = TRUE)) %>% 
  mutate(Phototherapy_ind_usb = case_when(Hyperbilirubinaemia_cat %in% c("Hyperbilirubinaemia_serum", "Hyperbilirubinaemia_tcb") ~ TRUE, TRUE ~ FALSE),
         Phototherapy_doc = has_photo_doc,
         Phototherapy_status = case_when(Phototherapy_ind_usb & Phototherapy_doc ~ "Indicated_AND_Documented",
                                         Phototherapy_ind_usb & !Phototherapy_doc ~ "Indicated_BUT_Not_Documented",
                                         !Phototherapy_ind_usb & Phototherapy_doc ~ "Not_Indicated_BUT_Documented",
                                         TRUE ~ "Not_Indicated_NOT_Documented")) %>% 
  select(- has_photo_doc, - Phototherapy_ind_usb, - Phototherapy_doc)

(Photo_USB_comb <- Photo_USB %>%
  group_by(Phototherapy_status) %>%
  summarise(Neonates_count = n_distinct(patient_id)))
# 1 Indicated_AND_Documented              22
# 2 Indicated_BUT_Not_Documented          45
# 3 Not_Indicated_BUT_Documented          62
# 4 Not_Indicated_NOT_Documented          8167

# Now there are sometimes several bili measurement entries per newborn --> generate one row 
## check if there are only one Hyperbilirubinaemia_cat per patient
Bili_cat_USB_measure2 %>%
  group_by(patient_id, case_id) %>%
  summarise(n_cat = n_distinct(Hyperbilirubinaemia_cat)) %>%
  filter(n_cat > 1)

# Build variable with just one row per patient
Bili_cat_USB_measure3 <- Bili_cat_USB_measure2 %>%
  distinct(patient_id, case_id, .keep_all = TRUE) %>%
  select(patient_id, case_id, Hyperbilirubinaemia_cat) 

summary(as.factor(Bili_cat_USB_measure3$Hyperbilirubinaemia_cat))
# Hyperbilirubinaemia_serum   Hyperbilirubinaemia_tcb    Physiological 
# 59                          8                          8229

# Merge together Bili categories and information doc procedure
Newborn_bili_status <- left_join(Bili_cat_USB_measure3, Photo_USB, by = c("patient_id", "case_id", "Hyperbilirubinaemia_cat"))

# Merge with newborn groups
Sample3 <- left_join(Sample3, Newborn_bili_status, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hyperbilirubinaemia_cat = if_else(is.na(Hyperbilirubinaemia_cat), "No_measurement", Hyperbilirubinaemia_cat),
         Phototherapy_status = if_else(is.na(Phototherapy_status), "No_measurement", Phototherapy_status))

Newborn_group1_c <- left_join(Newborn_group1_c, Newborn_bili_status, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hyperbilirubinaemia_cat = if_else(is.na(Hyperbilirubinaemia_cat), "No_measurement", Hyperbilirubinaemia_cat),
         Phototherapy_status = if_else(is.na(Phototherapy_status), "No_measurement", Phototherapy_status))

Newborn_group2_c <- left_join(Newborn_group2_c, Newborn_bili_status, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
  mutate(Hyperbilirubinaemia_cat = if_else(is.na(Hyperbilirubinaemia_cat), "No_measurement", Hyperbilirubinaemia_cat),
         Phototherapy_status = if_else(is.na(Phototherapy_status), "No_measurement", Phototherapy_status))

summary(as.factor(Newborn_group1_c$Hyperbilirubinaemia_cat))
# Hyperbilirubinaemia_serum   Hyperbilirubinaemia_tcb       No_measurement         Physiological 
# 10                          1                             412                    3451 

summary(as.factor(Newborn_group2_c$Hyperbilirubinaemia_cat))
# Hyperbilirubinaemia_serum   Hyperbilirubinaemia_tcb       No_measurement           Physiological 
# 26                          3                             143                      1672 

# 4.2.3 NICE Categorisation -------------------------------------
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
Bili_cat_Nice <- Bili_cat_Nice %>% 
mutate(Bili_levels = case_when(
    bilirubin_type == "TRANSCUTAN" & Value_umol_l < Bili_start_phototherapy_above ~ "Bili_tc_norm",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Bili_tc_photo",
    bilirubin_type == "TRANSCUTAN" & Value_umol_l >= Bili_start_exchange_therapy_above ~ "Bili_tc_bloodtrans",
    bilirubin_type == "SERUM" & Value_umol_l < Bili_start_phototherapy_above ~ "Bili_serum_norm",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_start_phototherapy_above & Value_umol_l < Bili_start_exchange_therapy_above ~ "Bili_serum_photo",
    bilirubin_type == "SERUM" & Value_umol_l >= Bili_start_exchange_therapy_above ~ "Bili_serum_bloodtrans"))

table(Bili_cat_Nice$Bili_levels)
# Bili_serum_bloodtrans       Bili_serum_norm     Bili_serum_photo    Bili_tc_bloodtrans   Bili_tc_norm       Bili_tc_photo 
# 2                           2163                59                  10                   21103              241

#  4.2.4 Photothrapy ------------------------------------------------------
# Identify reported cases for phototherapy started (like USB)
Bili_procedure2 <- left_join(Bili_cat_Nice, Procedure_red2_corr, by = c("patient_id", "case_id"), relationship = "many-to-many") %>%
  select(patient_id, case_id, Bili_levels, procedure_label)

# Identify reported cases for phototherpy startet (NICE Gudelien, to compare)
Bili_cat_Nice2 <- Bili_procedure2 %>%
  group_by(patient_id, case_id) %>%
  summarise(has_tcb_required_t = any(Bili_levels == "Bili_tc_photo" | Bili_levels == "Bili_tc_bloodtrans"),
    has_serum_required_t = any(Bili_levels == "Bili_serum_photo" | Bili_levels == "Bili_serum_bloodtrans"),
    Bili_group_level = case_when(
      any(Bili_levels %in% c("Bili_serum_bloodtrans", "Bili_tc_bloodtrans")) ~ "Exchange_Transfusion",
      any(Bili_levels %in% c("Bili_serum_photo", "Bili_tc_photo")) ~ "Phototherapy",
      any(Bili_levels %in% c("Bili_serum_norm", "Bili_tc_norm")) ~ "Physiological",
      TRUE ~ "None"),
    Phototherapy_doc = any(procedure_label == "Sonstige Phototherapie", na.rm = TRUE)) %>%
  mutate(Phototherapy_ind = Bili_group_level %in% c("Phototherapy", "Exchange_Transfusion"), # no newborn had a exchange transfusion
    Phototherapy_status_NICE = case_when(
      Phototherapy_ind & Phototherapy_doc ~ "Indicated_AND_Documented",
      Phototherapy_ind & !Phototherapy_doc ~ "Indicated_NOT_Documented",
      !Phototherapy_ind & Phototherapy_doc ~ "Not_Indicated_BUT_Documented",
      TRUE ~ "Not_Indicated_NOT_Documented"))

# Overview 
(Photo_Nice_summary <- Bili_cat_Nice2 %>%
  group_by(Phototherapy_status_NICE) %>%
  summarise(Neonates_count = n_distinct(patient_id)))
# 1 Indicated_AND_Documented                 33
# 2 Indicated_NOT_Documented                121
# 3 Not_Indicated_BUT_Documented             51
# 4 Not_Indicated_NOT_Documented           8091

## check if there are only one Hyperbilirubinaemia_cat per patient
Bili_cat_Nice2 %>%
  group_by(patient_id, case_id) %>%
  summarise(n_cat = n_distinct(Phototherapy_status_NICE)) %>%
  filter(n_cat > 1) # seems that there are just cases with one row

# Merge with newborn groups
Sample3 <- left_join(Sample3, Bili_cat_Nice2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  mutate(Phototherapy_status_NICE = if_else(is.na(Phototherapy_status_NICE), "No_measurement", Phototherapy_status_NICE)) %>% 
  select(- has_tcb_required_t, - has_serum_required_t, - Bili_group_level, - Phototherapy_doc, - Phototherapy_ind)

Newborn_group1_c <- left_join(Newborn_group1_c, Bili_cat_Nice2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  mutate(Phototherapy_status_NICE = if_else(is.na(Phototherapy_status_NICE), "No_measurement", Phototherapy_status_NICE)) %>% 
  select(- has_tcb_required_t, - has_serum_required_t, - Bili_group_level, - Phototherapy_doc, - Phototherapy_ind)

Newborn_group2_c <- left_join(Newborn_group2_c, Bili_cat_Nice2, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
  mutate(Phototherapy_status_NICE = if_else(is.na(Phototherapy_status_NICE), "No_measurement", Phototherapy_status_NICE)) %>% 
  select(- has_tcb_required_t, - has_serum_required_t, - Bili_group_level, - Phototherapy_doc, - Phototherapy_ind)

## Conclusion: compared USB and NICE Guideline there are more children who needed therapy (according the guideline) when practiced with the NICE guideline.
## Moreover, either a therapy would have been theoretically necessary, but there was no entry in process data, or there was a therapy according to the procedure 
## documentation, but theoretically not necessary.


rm(Bili_cat_Nice, Bili_cat_Nice2, Bili_cat_USB, Bili_cat_USB_measure, Bili_cat_USB_measure2, Bili_cat_USB_measure3, Bili_procedure, Bili_procedure2, Bilimeasure_n, Bilimeasure_y,
Lookup_bili, Newborn_bili_status, Photo_Nice_summary, Photo_USB, Photo_USB_comb)






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
# There are NAs, I want to build a new variable (check the distribution with the actual sample)
# Sample2c <- left_join(Sample2b, Bili_cat_USB, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   select(patient_id_child, case_id_child, bilirubin_type, Value_umol_l, Treatment_required_Hyperbili) %>% 
#   mutate(bilirubin_type = if_else(is.na(bilirubin_type), "No_measure", bilirubin_type), # NA = no measurement, to remove NA
#          Value_umol_l = if_else(is.na(Value_umol_l), 0, Value_umol_l),
#          Treatment_required_Hyperbili = if_else(is.na(Treatment_required_Hyperbili), "No_measure", Treatment_required_Hyperbili))

### OLD
## now there are sometimes several bili measurement entries per newborn --> generate one row + build a new variable and summarise it to a category variable to better merge 
# Hyperbilirubinaemia_cat <- Bili_cat_USB %>% 
#   group_by(patient_id, case_id) %>% 
#   summarise(Hyperbili_cat = if_else(any(Treatment_required_Hyperbili == "Yes"), "Hyperbili", "Physiological_jaundice"))
# 
# table(Hyperbilirubinaemia_cat$Hyperbili_cat)
# # Hyperbili           Physiological_jaundice 
# # 76                  8220 

#Join with Procedure data to compare (with USB guideline)
# Bili_comb_procedure <- left_join(Bili_cat_USBb, Procedure_red2_corr, by = c("patient_id", "case_id"), relationship = "many-to-many") %>%
#   select(patient_id, case_id, Treatment_required_Hyperbili, Bili_levels, procedure_label)
# 
# # Overview of the combinations
# (Photo_USB_comb <- Bili_comb_procedure %>%
#   mutate(Phototherapy_ind_usb = case_when(
#       Bili_levels %in% c("bili_tc_photo", "bili_serum_photo") ~ TRUE, TRUE ~ FALSE), # if bili_tc_photo OR bili_serum_photo value is TRUE, for all other cases TRUE set the value to FALSE
#       Phototherapy_doc = case_when(procedure_label %in% "Sonstige Phototherapie" ~ TRUE, TRUE ~ FALSE)) %>%
#       group_by(Phototherapy_ind_usb, Phototherapy_doc) %>%
#   summarise(Neonates_count = n_distinct(patient_id))) # counts the unique newborns for each group combinations
# Phototherapy_ind_usb   Phototherapy_doc          Neonates_count
# <lgl>                  <lgl>                     <int>
# 1 FALSE                FALSE                     8214
# 2 FALSE                TRUE                       82
# 3 TRUE                 FALSE                      53
# 4 TRUE                 TRUE                       27
# 
# # Identification cases normally out of treatment threshold
# Bili_comb_procedure2 <- Bili_comb_procedure %>%
#   filter(Bili_levels %in% c("bili_tc_norm", "bili_serum_norm") & procedure_label == "Sonstige Phototherapie") %>%
#   select(patient_id, case_id, Bili_levels, procedure_label) %>% 
#   distinct(patient_id, case_id, .keep_all = T) # 82 cases

# Left_join back to my sample to add variable (bring together with Sample, Replace NA to "no_measurement")
# Sample_test4 <- left_join(Sample_test3, Hyperbilirubinaemia_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>% 
#   mutate(Hyperbili_cat = replace_na(Hyperbili_cat, "No_measurement"))
# 
# # Add variable with procedure variable phototherapy to combine
# Photo_USB_comb <- Bili_comb_procedure %>%
#   mutate(Phototherapy_ind_usb = case_when(Bili_levels %in% c("bili_tc_photo", "bili_serum_photo") ~ TRUE, TRUE ~ FALSE), # TRUE if phototherapy is indicated, all other cases FALSE
#          Phototherapy_doc = case_when(procedure_label %in% "Sonstige Phototherapie" ~ TRUE, TRUE ~ FALSE)) %>% # TRUE if phototherapy is documented, all other cases FALSE
#   group_by(patient_id, case_id) %>% # per patient
#   summarise(Phototherapy_ind_usb = any(Phototherapy_ind_usb), # check per child and case whether phototherapy was indicated at least once
#             Phototherapy_doc = any(Phototherapy_doc)) # check per child and case whether phototherapy was documented at least once
# 
# Sample_test4 <- left_join(Sample_test4, Photo_USB_comb, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) %>%
#   mutate(Phototherapy_ind_usb = replace_na(Phototherapy_ind_usb, FALSE), # if NA
#          Phototherapy_doc = replace_na(Phototherapy_doc, FALSE)) # in NA

## Identify cases WITHOUT any blood sugar measurement
# anti_join: to find newborns that are in Sample2 but not in Hypoglyc_data_cat (which has not had a blood glucose measure?)
# No_measure_cases <- anti_join(Sample2, Hypoglyc_data_cat, by = c("patient_id_child" = "patient_id", "case_id_child" = "case_id")) # 4311
# 
# # Date format has to be changed (to merge later properly)
# No_measure_cases <- No_measure_cases %>%
#   mutate(pat_geb_dat = as.Date(CBIS_BIRTH_DATE_TS)) %>%
#   select(-CBIS_BIRTH_DATE_TS)
# 
# # Add new category "No_measure"
# No_measure_cases <- No_measure_cases %>%
#   rename(patient_id = patient_id_child, case_id = case_id_child) %>%
#   mutate(Hypoglycaemia_cat = "No_measurement")
# 
# # Combine together Hypoglyc_data_cat and No_measure_cases
# Hypoglyc_data_cat2 <- bind_rows(Hypoglyc_data_cat, No_measure_cases) # combines the two data sets vertically; all other variables before NAs..
# Hypoglyc_data_cat2 <- Hypoglyc_data_cat2 %>%
#   select(patient_id, case_id, pat_geb_dat, alter_in_tagen, Hypoglycaemia_cat) # select needed variables
# 
# summary(as.factor(Hypoglyc_data_cat2$Hypoglycaemia_cat))
# # Mild       Moderate No_measurement Normoglycaemic         Severe 
# # 275             49           4311           3410            156  
# 
# round(prop.table(table(as.factor(Hypoglyc_data_cat2$Hypoglycaemia_cat))) * 100, 1)
# # Mild       Moderate No_measurement Normoglycaemic         Severe 
# # 3.4            0.6           52.6           41.6            1.9 



# Descriptive analysis ----------------------------------------------------

# Sample: whole sample
# Numerical
Summary1 <- Sample_final_nicu %>%
  ungroup() %>% 
  summarise(
    min_ga = min(Weeks_LPM, na.rm = TRUE), # gestational age
    max_ga = max(Weeks_LPM, na.rm = TRUE),
    mean_ga = mean(Weeks_LPM, na.rm = TRUE),
    sd_ga = sd(Weeks_LPM, na.rm = TRUE),
    iqr_ga_q1 = quantile(Weeks_LPM, 0.25, na.rm = TRUE),
    iqr_ga_q3 = quantile(Weeks_LPM, 0.75, na.rm = TRUE),
    min_weight = min(Birth_weight_g, na.rm = TRUE), # birth weight
    max_weight = max(Birth_weight_g, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE),
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    iqr_weight_q1 = quantile(Birth_weight_g, 0.25, na.rm = TRUE),
    iqr_weight_q3 = quantile(Birth_weight_g, 0.75, na.rm = TRUE),
    min_los = min(LOS, na.rm = TRUE), # LOS
    max_los = max(LOS, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE),
    sd_los = sd(LOS, na.rm = TRUE),
    iqr_los_q1 = quantile(LOS, 0.25, na.rm = TRUE),
    iqr_los_q3 = quantile(LOS, 0.75, na.rm = TRUE),
    min_mat_age = min(RF_Maternal_age, na.rm = TRUE), # mother age
    max_mat_age = max(RF_Maternal_age, na.rm = TRUE),
    mean_mat_age = mean(RF_Maternal_age, na.rm = TRUE),
    sd_mat_age = sd(RF_Maternal_age, na.rm = TRUE),
    iqr_mat_age_q1 = quantile(RF_Maternal_age, 0.25, na.rm = TRUE),
    iqr_mat_age_q3 = quantile(RF_Maternal_age, 0.75, na.rm = TRUE))

Summary1_table <- tibble(
  Variable = c("Gestational age (weeks)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Min = round(c(Summary1$min_ga, Summary1$min_weight, Summary1$min_los, Summary1$min_mat_age), 1),
  Max = round(c(Summary1$max_ga, Summary1$max_weight, Summary1$max_los, Summary1$max_mat_age), 1),
  Mean = round(c(Summary1$mean_ga, Summary1$mean_weight, Summary1$mean_los, Summary1$mean_mat_age), 1),
  SD = round(c(Summary1$sd_ga, Summary1$sd_weight, Summary1$sd_los, Summary1$sd_mat_age), 1),
  IQR = c(
    paste0(round(Summary1$iqr_ga_q1, 1), " – ", round(Summary1$iqr_ga_q3, 1)),
    paste0(round(Summary1$iqr_weight_q1, 1), " – ", round(Summary1$iqr_weight_q3, 1)),
    paste0(round(Summary1$iqr_los_q1, 1), " – ", round(Summary1$iqr_los_q3, 1)),
    paste0(round(Summary1$iqr_mat_age_q1, 1), " – ", round(Summary1$iqr_mat_age_q3, 1))))

n_total <- nrow(Sample_final_nicu)
Summary1_table <- Summary1_table %>% 
  mutate(n = n_total)

Summary1_table
ft1 <- flextable(Summary1_table)
ft1 <- autofit(ft1)
ft1

# Categorical (Country, DM, HHH)
Categorical_summary_table2 <- bind_rows(
  Sample_final_nicu %>%
    count(Country, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country),
  
  Sample_final_nicu %>%
    count(Maternal_diabetes_type, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Maternal_diabetes_type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Maternal_diabetes_type),
  
  Sample_final_nicu %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia_cat", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),
  
  Sample_final_nicu %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia_cat", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),
  
  Sample_final_nicu %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia_cat", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary_table2
ft2 <- flextable(Categorical_summary_table2)
ft2 <- autofit(ft3)
ft2

# OLD - Sample size (act)
# n_total <- 5935
# 
# # Numeric variables (birth weight, GA, LOS, admission NICU)
# # Summary
# Summary_numeric <- Sample_test_final %>%
#   ungroup() %>% 
#   summarise(
#     min_weight = min(Birth_weight_g, na.rm = TRUE),
#     max_weight = max(Birth_weight_g, na.rm = TRUE),
#     mean_weight = mean(Birth_weight_g, na.rm = TRUE),
#     sd_weight = sd(Birth_weight_g, na.rm = TRUE),
#     iqr_weight_q1 = quantile(Birth_weight_g, 0.25, na.rm = TRUE),
#     iqr_weight_q3 = quantile(Birth_weight_g, 0.75, na.rm = TRUE),
#     min_ga = min(Weeks_LPM, na.rm = TRUE),
#     max_ga = max(Weeks_LPM, na.rm = TRUE),
#     mean_ga = mean(Weeks_LPM, na.rm = TRUE),
#     sd_ga = sd(Weeks_LPM, na.rm = TRUE),
#     iqr_ga_q1 = quantile(Weeks_LPM, 0.25, na.rm = TRUE),
#     iqr_ga_q3 = quantile(Weeks_LPM, 0.75, na.rm = TRUE),
#     min_los = min(as.numeric(LOS, units = "h"), na.rm = TRUE),
#     max_los = max(as.numeric(LOS, units = "h"), na.rm = TRUE),
#     mean_los = mean(as.numeric(LOS, units = "h"), na.rm = TRUE),
#     sd_los = sd(as.numeric(LOS, units = "h"), na.rm = TRUE),
#     iqr_los_q1 = quantile(LOS, units = "h", 0.25, na.rm = TRUE),
#     iqr_los_q3 = quantile(LOS, units = "h", 0.75, na.rm = TRUE),
#     Nicu_count = sum(admission_neo_n == 1),
#     Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)
# 
# # Table, numerical variables (birth weight, LOS, GA)
# Summary_numeric_table <- tibble(Variable = c("Birth weight (g)", "Gestational age (weeks)", "LOS (hours)"),
#                                 Min = round(c(Summary_numeric$min_weight, Summary_numeric$min_ga, Summary_numeric$min_los), 2),
#                                 Max = round(c(Summary_numeric$max_weight, Summary_numeric$max_ga, Summary_numeric$max_los), 2),
#                                 Mean = round(c(Summary_numeric$mean_weight, Summary_numeric$mean_ga, Summary_numeric$mean_los), 2),
#                                 SD = round(c(Summary_numeric$sd_weight, Summary_numeric$sd_ga, Summary_numeric$sd_los), 2),
#                                 IQR_Range = c(
#                                   paste(round(Summary_numeric$iqr_weight_q1, 2), "–", round(Summary_numeric$iqr_weight_q3, 2)),
#                                   paste(round(Summary_numeric$iqr_ga_q1, 2), "–", round(Summary_numeric$iqr_ga_q3, 2)),
#                                   paste(round(Summary_numeric$iqr_los_q1, 2), "–", round(Summary_numeric$iqr_los_q3, 2))))
# 
# # NICU admission
# Nicu_table <- tibble("NICU admission" = c("Yes", "No"),
#                      n = c(Summary_numeric$Nicu_count, n_total - Summary_numeric$Nicu_count),
#                      percent = c(Summary_numeric$Nicu_percent, 100 - Summary_numeric$Nicu_percent))









