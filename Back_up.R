# 2. Import data sets --------------------------------------------------------
# 2.a Data sets
load(file = "I:/Verwaltung/MaNtiS/03_Prozessierte_Daten/Birth_corr3_2025-04-22.RData")
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

# # patient_id_child
# sum(is.na(Birth_corr3$patient_id_child)) # there are 2 NAs
# # case_id_child
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

# Create data sample with newborns >= 37 GA (259 days)
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
  mutate(Weight_cat = case_when(Birth_weight_g >= 2500 & Birth_weight_g < 3000 ~ "2500-2999",
                                Birth_weight_g >= 3000 & Birth_weight_g < 3500 ~ "3000-3500",
                                Birth_weight_g >= 3500 & Birth_weight_g < 4000 ~ "3500-4000",
                                Birth_weight_g >= 4000 & Birth_weight_g <= 4500 ~ "4000-4500"))
# as factor
Sample1 <- Sample1 %>%
  mutate(Weight_cat = factor(Weight_cat,
                                   levels = c("2500-2999", "3000-3500", "3500-4000", "4000-4500")))


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
# Instrum_del  Normal_del       Pl_CS     Upl_CS 
# 1082        3771              1495      1143 

table(is.na(Sample1$mode_of_birth)) # no NAs

# create new variable
# Sample1 <- Sample1 %>%
#   mutate(Birth_mode = ifelse(is.na(mode_of_birth), "No_data", mode_of_birth))

Sample1 <- Sample1 %>%
  mutate(Birth_mode = recode(mode_of_birth, 
                                   "Normal_del" = "Vaginal", 
                                   "Instrum_del" = "Instrumental_vaginal", 
                                   "Pl_CS" = "C-section_pl",
                                   "Upl_CS" = "C-section_upl")) 
table(Sample1$Birth_mode)
# C-section_pl    C-section_upl        Instrumental_vaginal       Vaginal 
# 1495            1143                 1082                       3771 

Sample1 <- Sample1 %>% 
  select(-mode_of_birth)

Sample1 <- Sample1 %>%
  mutate(Birth_mode = factor(Birth_mode,
                                   levels = c("Vaginal", "Instrumental_vaginal", "C-section_pl", "C-section_upl")))
# 3.4.4 Feeding type ---------------------------------------------------------
table(Sample1$feeding_method_u3)
# excl_bf   excl_ff   ff_plus partly_bf 
# 1636       100        60      5391

table(is.na(Sample1$feeding_method_u3)) # TRUE = 304

# create new variable and rename the values (with recode)
Sample1 <- Sample1 %>%
  mutate(Feeding_group = if_else(is.na(feeding_method_u3), "No_data", feeding_method_u3))
table(is.na(Sample1$Feeding_group))  # 7491 

Sample1 <- Sample1 %>%
  mutate(Feeding_group = recode(Feeding_group, 
                                "excl_bf" = "Fully_breastfed", 
                                "partly_bf" = "Partly_breastfed", 
                                "ff_plus" = "Mixed_feeding_no_breastfed", 
                                "excl_ff" = "Formula_only",
                                "No_data" = "No_data"))
table(Sample1$Feeding_group)

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
  relocate(Birth_weight_g, Weight_cat, .after = last_col()) %>% # .after = Destination of columns selected by
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

rm(Newborns_excluded_diagnoses)

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
                                            maternal_DM == TRUE ~ "preex_DM", TRUE ~ "No_diabetes")) 

# Differentiated diabetes type for group 2
Newborn_group2 <- left_join(Newborn_group2, Maternal_DM2, by = "patient_id_mother") %>% 
  mutate(Maternal_diabetes_type = factor(Maternal_diabetes_type, levels = c("No_diabetes", "GDM", "preex_DM", "DM_GDM"))) %>% # as factor for analysis
  select(-maternal_DM, -maternal_gdm)

# Check for group 1 
Newborn_group1_test <- left_join(Newborn_group1, Maternal_DM2, by = "patient_id_mother") %>% 
  mutate(Maternal_diabetes_type = factor(Maternal_diabetes_type, levels = c("No_diabetes", "GDM", "preex_DM", "DM_GDM"))) %>% # as factor for analysis
  select(-maternal_DM, -maternal_gdm) 
## Conclusion: there are n = 42 cases with GDM! These are to be moved to group 2

# Identify Mothers with GDM
GDM_mothers <- Maternal_DM2 %>% 
  filter(Maternal_diabetes_type == "GDM") %>%
  select(patient_id_mother) # 708

# Remove the cases in question in Group 1
Newborn_group1_c <- Newborn_group1 %>%
  anti_join(GDM_mothers, by = "patient_id_mother") %>% # Gibt alle Beobachtungen des Data Frames x zurück, für die es keinen Match im Data Frame y gibt
  select(- has_risk_icd)

GDM_cases_to_add <- Newborn_group1 %>%
  inner_join(GDM_mothers, by = "patient_id_mother") # 42

# Add the cases in question to Group 2
Newborn_group2_c <- bind_rows(Newborn_group2, GDM_cases_to_add)

Newborn_group2_c <- Newborn_group2_c %>%
  mutate(has_risk_icd = TRUE, Risk_group = "With_RF", Maternal_diabetes_type = replace_na(Maternal_diabetes_type, "GDM"))
## Conclusion: Newborn group without rf n= 3950 and Newborn group with rf n= 1942

# Transforming the 42 cases also in the whole sample for later analysis
gdm_patient_ids <- GDM_cases_to_add$patient_id_mother

length(unique(gdm_patient_ids)) # 41, twins

Sample3 <- Sample3 %>%
  mutate(Risk_group = case_when(patient_id_mother %in% gdm_patient_ids ~ "With_RF",
      TRUE ~ Risk_group)) # IF patient_id_mother is included in the list gdm_patient_ids then set Risk_group to ‘With_RF’

table(Sample3$Risk_group)
# With_RF  Without_RF 
# 1942       3950 

Sample3 <- Sample3 %>% 
  mutate(Risk_group = factor(Risk_group,
                             levels = c("Without_RF", "With_RF")))

# Remove variables not needed
Sample3 <- Sample3 %>% 
  select(- has_neonatal_rf, - has_maternal_rf, - has_risk_icd)

rm(Diag_neonatal, Diag_maternal, GDM_cases_to_add, GDM_mothers, Maternal_DM, Maternal_DM2, Maternal_RF_cases, Newborn_group1, Newborn_group2, Newborn_group1_test, Sample2)

# 3.4.8 Other Maternal Factors ------------------------------------------------------

# 1. Hypoglycaemia: Maternal diabetes (O24.0, O24.1, O24.4, E10.90, E10.91, E11.20, E11.90, E11.91, E13.90, E13.91, E14.90), Caesariean secion (mode of delivery), Parity
# 2. Hyperbilirubinaemia: Maternal age, Maternal diabetes O24.0, O24.1, O24.4, E10.90, E10.91, E11.20, E11.90, E11.91, E13.90, E13.91, E14.90), Race/Ethnicity) -> D55.0
# 3. Hypothermia: Race/ethnicity, maternal age, parity, Ceasarean sectio (mode of delivery)

# 3.4.8.1 Parity ----------------------------------------------------------
table(is.na(parity1)) # 2 NAs by Anzahl_vorausg_LebGeb, seems to be two primipara
Parity <- parity1 %>%
  mutate(Anzahl_vorausg_LebGeb = as.numeric(Anzahl_vorausg_LebGeb), # character in as numeric, to be able to replace NA
         Anzahl_vorausg_LebGeb = replace_na(Anzahl_vorausg_LebGeb, 0))

Parity <- Parity %>%
  mutate(RF_parity = if_else(Anzahl_vorausg_LebGeb == 0, "Primi", "Multi")) %>%
  mutate(RF_parity = factor(RF_parity, levels = c("Primi", "Multi")))

# Merge with Samples 
## Whole sample
Sample3 <- left_join(Sample3, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>% 
  rename(Co_Parity = Anzahl_vorausg_LebGeb) %>% 
  select(-Anzahl_vorausg_SS, -Anzahl_fehl_Geb, -Anzahl_Interruptio)
table(Sample3$Co_Parity)  
#    0    1    2    3    4    5    6    7 
# 2801 2232  664  142   40    7    5    1
table(Sample3$RF_parity)
# Primi Multi 
# 2801  3091

## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>%
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)
Newborn_group1_c <- Newborn_group1_c %>%
  mutate(RF_parity = factor(RF_parity,
                            levels = c("Primi", "Multi")))

# Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Parity, by = c("patient_id_child", "case_id_child" = "case_id", "patient_id_mother", "case_id_mother")) %>%
  select(-Anzahl_vorausg_SS, -Anzahl_vorausg_LebGeb, -Anzahl_fehl_Geb, -Anzahl_Interruptio)
Newborn_group2_c <- Newborn_group2_c %>%
  mutate(RF_parity = factor(RF_parity,
                            levels = c("Primi", "Multi")))

# 3.4.8.2 Maternal age ----------------------------------------------------
Mother_data <- left_join(Sample3, Pat_info, by = c("patient_id_mother" = "patient_id", "case_id_mother" = "case_id"), relationship = "many-to-many") %>% 
  select(patient_id_mother, case_id_mother, PAT_BIRTH_DATE, CBIS_BIRTH_DATE_TS, PAT_CITIZENSHIP_COUNTRY) # %>% 
  # distinct(patient_id_mother, .keep_all = TRUE)

Mother_data <- Mother_data %>%
  distinct(patient_id_mother, case_id_mother, .keep_all = TRUE) # remove duplicates, 6849

Mother_data <- Mother_data %>% 
  mutate(Birth_date = as.Date(CBIS_BIRTH_DATE_TS)) %>%
  mutate(Maternal_age = interval(start = PAT_BIRTH_DATE, end = Birth_date) / duration(n = 1, unit = "years"))
summary(Mother_data$Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.49   33.78   33.54   36.91   52.37

# Merge together with samples
## Whole sample
Sample3 <- left_join(Sample3, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x) 
summary(Sample3$Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.48   33.79   33.55   36.92   52.37 --> to exclude underage women (n=8)

Sample3 <- Sample3 %>%
  filter(Maternal_age > 18) # 5884

## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)
summary(Newborn_group1_c$Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.90   30.50   33.67   33.48   36.84   51.03

Newborn_group1_c <- Newborn_group1_c %>%
  filter(Maternal_age > 18) # - n= 6, 3944

## Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Mother_data, by = c("patient_id_mother", "case_id_mother")) %>% # without date of birth!
  select(- Birth_date, - PAT_BIRTH_DATE, - CBIS_BIRTH_DATE_TS.y) %>% 
  rename(CBIS_BIRTH_DATE_TS = CBIS_BIRTH_DATE_TS.x)
summary(Newborn_group2_c$Maternal_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.55   30.45   33.96   33.69   37.07   52.37 

Newborn_group2_c <- Newborn_group2_c %>%
  filter(Maternal_age > 18) # - n= 2, 1940

rm(Mother_data, Parity)

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
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) %>% 
  mutate(Country = factor(Country, levels = c("Switzerland", "Europe", "Africa", "America", "Asia", "Oceania", "Unknown")))
  
## Group 1
Newborn_group1_c <- left_join(Newborn_group1_c, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) %>% 
  mutate(Country = factor(Country, levels = c("Switzerland", "Europe", "Africa", "America", "Asia", "Oceania", "Unknown")))

table(Newborn_group1_c$Country)
# Africa     America      Asia      Europe    Oceania  Switzerland  Unknown 
# 131         105         249        1752     8        1695         10 
round(prop.table(table(as.factor(Newborn_group1_c$Country))) * 100, 1)
# Africa     America        Asia      Europe     Oceania Switzerland     Unknown 
# 3.3         2.7         6.3        44.4         0.2        42.9         0.3 

## Group 2
Newborn_group2_c <- left_join(Newborn_group2_c, Countries2b, by = "PAT_CITIZENSHIP_COUNTRY") %>% 
  select(- PAT_CITIZENSHIP_COUNTRY, - pat_citizenship_country_new, -landercode_bfs_code_des_pays_ofs_codice_del_paese_ust, 
         -iso2, -iso3, -region_region_regione, - kontinent_continent_continente) %>% 
  mutate(Country = factor(Country, levels = c("Switzerland", "Europe", "Africa", "America", "Asia", "Oceania", "Unknown")))

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
  filter(!is.na(Hypothermia_cat)) # 5876

# Group 1
Missing_temp1 <- Newborn_group1_c %>% 
  filter(is.na(Hypothermia_cat))
## n= 5 ambulant/tagesklinik
Newborn_group1_c <- Newborn_group1_c %>%
  filter(!is.na(Hypothermia_cat)) # 3939

# Group 2
Missing_temp2 <- Newborn_group2_c %>% 
  filter(is.na(Hypothermia_cat))
## n= 1 ambulant/tagesklinik, n= 2 unknown

Newborn_group2_c <- Newborn_group2_c %>%
  filter(!is.na(Hypothermia_cat)) # 1937

rm(Missing_temp, Missing_temp1, Missing_temp2)


# 3.5.2 Hypoglycaemia categories ----------------------------------------
# 3.5.3 Hyperbilirubinaemia categories ----------------------------------------
source("~/Thesis/Code/01_03_02_Code_Newborn_Meona.R")


# 3.5.4 Overview HHH  ---------------------------------------------------
## To have an overview if a child has none, one, two or all HHH diagnoses based on my categories
# Whole sample
Sample3 <- Sample3 %>%
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
table(Sample3$HHH_diagnoses)
# All_diagnoses    Hyperbili_Hypothermia        Hyperbili_only      Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only        None 
# 1                    12                       34                    78                         110                  1206                  4435
round(prop.table(table(as.factor(Sample3$HHH_diagnoses))) * 100, 1)
# All_diagnoses   Hyperbili_Hypothermia      Hyperbili_only       Hypoglyc_Hypothermia         Hypoglyc_only      Hypotherm_only        None 
# 0.0              0.2                        0.6                 1.3                          1.9                 20.5                 75.5 

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
# 1                            12                    16                           13                   722                  3175
round(prop.table(table(as.factor(Newborn_group1_c$HHH_diagnoses))) * 100, 1)
# Hyperbili_Hypothermia        Hyperbili_only        Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only         None 
# 0.0                          0.3                   0.4                           0.3                  18.3                  80.6 

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
# 1                    11                           22                  62                           97                    484                  1260
round(prop.table(table(as.factor(Newborn_group2_c$HHH_diagnoses))) * 100, 1)
# All_diagnoses         Hyperbili_Hypothermia        Hyperbili_only       Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only       None 
# 0.1                   0.6                           1.1                   3.2                         5.0                  25.0                 65.0

# 4. Overview sample ------------------------------------------------------

# 4.1 Whole sample --------------------------------------------------------
# Check
summary(Sample3)

# To change LOS from mins to h: attr: Object Attributes, Description: Get or set specific attributes of an object.: attr(x, which) <- value
Sample_final_all <- Sample3 %>%
  mutate(LOS = LOS/60)
attr(Sample_final_all$LOS, "units") <- "h"
 

# 4.1.1 NICU Admision -----------------------------------------------------
## Percentages Admission NICU
Sample_final_all_nicu <- Sample_final_all %>%
  filter(admission_neo_n == 1) # 46

Sample_final_all %>%
  ungroup() %>%
  summarise(total = n(),
            admitted = sum(admission_neo_n %in% 1),
            percent = (admitted / total) * 100)
#   <int>    <int>   <dbl>
#1  5876       46   0.783

# % for the HHH categories
## Hypothermia
(Hypothermia_nicu <- Sample_final_all_nicu %>%
  ungroup() %>% 
  count(Hypothermia_cat) %>%
  mutate(percentage = round(n/sum(n)*100, 1)))

# Hypoglycaemia
(Hypoglycaemia_nicu <- Sample_final_all_nicu %>%
  ungroup() %>% 
  count(Hypoglycaemia_cat) %>%
  mutate(percentage = round(n/sum(n)*100, 1)))

# Hyperbilirubinaemia
(Hyperbilirubinaemia_nicu <- Sample_final_all_nicu %>%
    ungroup() %>% 
  count(Hyperbilirubinaemia_cat) %>%
  mutate(percentage = round(n/sum(n)*100, 1)))

(HHH_combi_nicu <- Sample_final_all_nicu %>%
  ungroup() %>% 
  count(HHH_diagnoses) %>%
  mutate(percentage = round(n/sum(n)*100, 1)))


# 4.1.2 Preparation Logistic Regression -----------------------------------
# COVARIATES
## Mode of delivery
table(Sample_final_all$Birth_mode) 

# Levels
Sample_final_all_c <- Sample_final_all %>%
  mutate(Birth_mode = factor(Birth_mode,
                             levels = c("Vaginal", "Instrumental_vaginal", "C-section_pl", "C-section_upl")))

## Feeding type
table(Sample_final_all_c$Feeding_group) # there are 6 cases (0.1%) with no data, for regression analysis to remove
Sample_final_all_c <- Sample_final_all_c %>% 
  filter(Feeding_group != "No_data") # 5870

# Levels 
Sample_final_all_c <- Sample_final_all_c %>%
  mutate(Feeding_group = factor(Feeding_group,
                                levels = c("Fully_breastfed", "Partly_breastfed", "Mixed_feeding_no_breastfed", "Formula_only")))

## Country
table(Sample_final_all_c$Country) # there are 13 (0.2%) unknown cases -- drin behalten!
# Sample_final_all_c <- Sample_final_all_c %>% 
#   filter(Country != "Unknown") 

# Levels
Sample_final_all_c <- Sample_final_all_c %>%
  mutate(Country = factor(Country, 
                          levels = c("Switzerland", "Europe", "Africa", "America", "Asia", "Oceania", "Unknown")))

## GA
Sample_final_all_c <- Sample_final_all_c %>%
  rename(Gest_age = gestational_age_total_days) # numeric

## Parity
table(Sample_final_all_c$Co_Parity) # numeric

# EXPLANATORY VARIABLES
# I want to carry out three bivariable regression analyses (dependent variable: admission_neo_n), whereby only the relevant 'No_measurement' cases are to be excluded in each case:
# Model 1: admission_neo_n ~ Hypothermia_cat --> no exclusion of no_measurement cases, due there are only full cases
# Model 2: admission_neo_n ~ Hypoglycaemia_cat --> only exclude cases with Hypoglycaemia_cat = "No_measurement"
# Model 3: admission_neo_n ~ Hyperbilirubinaemia_cat --> exclude only cases with Hyperbilirubinaemia_cat == "No_measurement"
# To analysis bivariable modelling for 1. Hypothermia_bi (as binomial, 0/1) ~ Hypoglycaemia_cat (without "No_measurement" cases)
# To analysis bivariable modelling for 2. Hypoglycaemia_bi (as binomial, 0/1) without "No_measurement" ~ Hypothermia_cat (no exclusion needed)
# To analysis bivariable modelling for 3. Hyperbilirubinaemia (as binomial, 0/1) ~ Hypothermia (no exclusion needed)
# To analysis bivariable modelling for 4. Hypothermia_bi (as binomial, 0/1) ~ Hyperbilirubinaemia_cat (without "No_measurement" cases)
# To analysis bivariable modelling for 5. Hypoglycaemia_bi ~ Hyperbilirubinaemia_bi (in both without binomial 0/1, "No_measurement" cases)
# To analysis bivariable modelling for 6. Hyperbilirubinaemia_bi (as 0/1) ~ Hypoglycaemia_cat (in both without "No_measurement" cases)
# Create a data set without "No_measurement" in both variables Hypoglycaemia_cat, Hyperbilirubinaemia_cat and Hyperbilirubinaemia_bi (binomial as 0,1) for multivariable models (with covariables).


## 1. Create the different data sets for examining the relationships between explanatory with admission to NICU (bivariable analysis)
Sample_original <- Sample_final_all_c

Sample_original <- Sample_original %>%
  mutate(
    Hypothermia_cat = factor(Hypothermia_cat,
                             levels = c("Norm", "Mild", "Moderate_Severe")),
    Hypoglycaemia_cat = factor(Hypoglycaemia_cat, 
                               levels = c("Normoglycaemic", "Mild", "Moderate", "Severe", "No_measurement")),
    Hyperbilirubinaemia_cat = factor(Hyperbilirubinaemia_cat, 
                                     levels = c("Physiological", "Hyperbilirubinaemia_tcb", "Hyperbilirubinaemia_serum", "No_measurement")))

# Hypothermia - NICU --> no exclusions
Sample_Hypotherm <- Sample_original 

# Hypoglycaemia - NICU --> exclusions "No_measurement" in Hypoglycaemia_cat
Sample_Hypoglyc <- Sample_original %>% 
  filter(Hypoglycaemia_cat != "No_measurement") %>% 
  mutate(Hypoglycaemia_cat = factor(Hypoglycaemia_cat,
                                    levels = c("Normoglycaemic", "Mild", "Moderate", "Severe")))

# Hyperbilirubinaemia - NICU --> exclusion "No_measurement" in Hyperbilirubinaemia_cat
Sample_Hyperbili <- Sample_original %>% 
  filter(Hyperbilirubinaemia_cat != "No_measurement") %>% 
  mutate(Hyperbilirubinaemia_cat = factor(Hyperbilirubinaemia_cat,
                                    levels = c("Physiological", "Hyperbilirubinaemia_tcb", "Hyperbilirubinaemia_serum")))

Sample_Hyperbili <- Sample_Hyperbili %>%
  mutate(Hyperbilirubinaemia_bi = ifelse(Hyperbilirubinaemia_cat == "Physiological", 0, 1))

## 2. Bivariable models for associations between the predictors
Sample_original <- Sample_original %>%
  mutate(Hypothermia_bi = ifelse(Hypothermia_cat == "Norm", 0, 1),
    Hypoglycaemia_bi = ifelse(Hypoglycaemia_cat == "Normoglycaemic", 0, 
                              ifelse(Hypoglycaemia_cat == "No_measurement", NA, 1)),
    Hyperbilirubinaemia_bi = ifelse(Hyperbilirubinaemia_cat == "Physiological", 0,
                                    ifelse(Hyperbilirubinaemia_cat == "No_measurement", NA, 1)))


## 3. Create data sets for the different analyses

# Hypothermia_bi ~ Hypoglycaemia_cat
Sample_hypot_hypo <- Sample_original %>%
  filter(!is.na(Hypothermia_bi), Hypoglycaemia_cat != "No_measurement") %>%
  mutate(Hypoglycaemia_cat = factor(Hypoglycaemia_cat,
                                    levels = c("Normoglycaemic", "Mild", "Moderate", "Severe")))

# Hypothermia_bi ~ Hypoglycaemia_bi
Sample_hypot_hypo_bi <- Sample_original %>%
  filter(!is.na(Hypothermia_bi), !is.na(Hypoglycaemia_bi))

# Hypoglycaemia_bi ~ Hypothermia_cat
Sample_hypo_hypot <- Sample_original %>%
  filter(!is.na(Hypoglycaemia_bi)) 

# Hypoglycaemia_bi ~ Hypothermia_bi
Sample_hypo_hypot_bi <- Sample_original %>%
  filter(!is.na(Hypoglycaemia_bi), !is.na(Hypothermia_bi))

# Hyperbilirubinaemia_bi ~ Hypothermia_cat
Sample_bili_hypot <- Sample_original %>%
  filter(!is.na(Hyperbilirubinaemia_bi)) 

# Hyperbilirubinaemia ~ Hypothermia_bi
Sample_bili_hypot_bi<- Sample_original %>%
  filter(!is.na(Hyperbilirubinaemia_bi), !is.na(Hypothermia_bi))

# Hypothermia_bi ~ Hyperbilirubinaemia_cat
Sample_hypot_bili <- Sample_original %>%
  filter(!is.na(Hypothermia_bi), Hyperbilirubinaemia_cat != "No_measurement") %>%
  mutate(Hyperbilirubinaemia_cat = factor(Hyperbilirubinaemia_cat,
                                          levels = c("Physiological", "Hyperbilirubinaemia_tcb", "Hyperbilirubinaemia_serum")))

# Hypothermia_bi ~ Hyperbilirubinaemia_bi
Sample_hypot_bili_bi <- Sample_original %>%
  filter(!is.na(Hypothermia_bi), !is.na(Hyperbilirubinaemia_bi))

# Hypoglycaemia_bi ~ Hyperbilirubinaemia_cat
Sample_hypo_bili_cat <- Sample_original %>%
  filter(!is.na(Hypoglycaemia_bi), Hyperbilirubinaemia_cat != "No_measurement") %>%
  mutate(Hyperbilirubinaemia_cat = factor(Hyperbilirubinaemia_cat,
                                          levels = c("Physiological", "Hyperbilirubinaemia_tcb", "Hyperbilirubinaemia_serum")))

# Hypoglycaemia_bi ~ Hyperbilirubinaemia_bi
Sample_hypo_bili <- Sample_original %>%
  filter(!is.na(Hypoglycaemia_bi), !is.na(Hyperbilirubinaemia_bi))

# Hyperbilirubinaemia_bi ~ Hypoglycaemia_cat
Sample_bili_hypo <- Sample_original %>%
  filter(!is.na(Hyperbilirubinaemia_bi), Hypoglycaemia_cat != "No_measurement") %>%
  mutate(Hypoglycaemia_cat = factor(Hypoglycaemia_cat,
                                    levels = c("Normoglycaemic", "Mild", "Moderate", "Severe")))

# Hyperbilirubinaemia_bi ~ Hypoglycaemia_bi
Sample_bili_hypo_bi <- Sample_original %>%
  filter(!is.na(Hyperbilirubinaemia_bi), !is.na(Hypoglycaemia_bi))

## 4. Create data set for multivariable analysis
Sample_no_missing <- Sample_original %>%
  filter(Hypoglycaemia_cat != "No_measurement",
         Hyperbilirubinaemia_cat != "No_measurement") %>%
  mutate(Hypoglycaemia_cat = factor(Hypoglycaemia_cat,
                               levels = c("Normoglycaemic", "Mild", "Moderate", "Severe")),
         Hyperbilirubinaemia_cat = factor(Hyperbilirubinaemia_cat,
                                     levels = c("Physiological", "Hyperbilirubinaemia_tcb", "Hyperbilirubinaemia_serum")))

# 6. Sample Multivariable -------------------------------------------------
## Sample with no missing
# NICU admission
Sample_no_missing_nicu <- Sample_no_missing %>% 
  filter(admission_neo_n %in% 1) # 19

# % for the HHH categories in NICU admission affected newborns
## Hypothermia
(Hypothermia_nicu_nm <- Sample_no_missing_nicu %>%
    ungroup() %>% 
    count(Hypothermia_cat) %>%
    mutate(percentage = round(n/sum(n)*100, 1)))

# Hypoglycaemia
(Hypoglycaemia_nicu_nm <- Sample_no_missing_nicu %>%
    ungroup() %>% 
    count(Hypoglycaemia_cat) %>%
    mutate(percentage = round(n/sum(n)*100, 1)))

# Hyperbilirubinaemia
(Hyperbilirubinaemia_nicu_nm <- Sample_no_missing_nicu %>%
    ungroup() %>% 
    count(Hyperbilirubinaemia_cat) %>%
    mutate(percentage = round(n/sum(n)*100, 1)))

(HHH_combi_nicu_nm <- Sample_no_missing_nicu %>%
    ungroup() %>% 
    count(HHH_diagnoses) %>%
    mutate(percentage = round(n/sum(n)*100, 1)))

# Overview HHH
Sample_no_missing <- Sample_no_missing %>%
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
table(Sample_no_missing$HHH_diagnoses)
# All_diagnoses Hyperbili_Hypothermia        Hyperbili_only  Hypoglyc_Hypothermia       Hypoglyc_only       Hypotherm_only      None 
# 1                    10                     9                    68                   104                 503                 1160
round(prop.table(table(as.factor(Sample_no_missing$HHH_diagnoses))) * 100, 1)
# All_diagnoses Hyperbili_Hypothermia        Hyperbili_only  Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only       None 
# 0.1                   0.5                   0.5                   3.7                   5.6                  27.1                 62.5 


# 4.2 Group 1 (without RF) ------------------------------------------------
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
#1  3939      2    0.0508

# Group1_final_nicu <- Group1_final_nicu %>%
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
# table(Group1_final_nicu$HHH_diagnoses)
# # Hypotherm_only None 
# # 1              1 


# 4.3 Group 2 (with RF) ---------------------------------------------------
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
#1  1937      44    2.27

summary(Group2_final_nicu)

Group2_final_nicu <- Group2_final_nicu %>%
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
table(Group2_final_nicu$HHH_diagnoses)
# Hyperbili_Hypothermia        Hyperbili_only        Hypoglyc_Hypothermia         Hypoglyc_only       Hypotherm_only      None 
# 1                            10                    12                            3                  7                   11 
round(prop.table(table(as.factor(Group2_final_nicu$HHH_diagnoses))) * 100, 1)
# Hyperbili_Hypothermia       Hyperbili_only       Hypoglyc_Hypothermia         Hypoglyc_only        Hypotherm_only       None 
# 2.3                         22.7                  27.3                        6.8                  15.9                 25.0 

# LOS
LOS_Group2_NICU <- Group2_final_nicu %>%
  filter(LOS < 24) # 26

n_under_24 <- 26
n_total <- 44

percent_under_24 <- round((n_under_24 / n_total) * 100, 1)
percent_under_24 # 59.1

rm(Newborn_group1_c, Newborn_group2_c)



## OLD
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
         Hyperbilirubinaemia_cat = case_when(
           has_serum_photo ~ "Hyperbilirubinaemia_serum",
           has_tc_photo ~ "Hyperbilirubinaemia_tcb",
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
# Sample: Total sample
# Numerical
Summary1 <- Sample_final_all %>%
  ungroup() %>% 
  summarise(
    mean_ga = mean(gestational_age_total_days, na.rm = TRUE), # GA
    sd_ga = sd(gestational_age_total_days, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE), # Birth weight
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE), # LOS
    sd_los = sd(LOS, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE), # maternal age
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    Nicu_count = sum(admission_neo_n == 1), # NICU
    Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)

Summary1_table <- tibble(
  Variable = c("Gestational age (days)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Mean = round(c(Summary1$mean_ga, Summary1$mean_weight, Summary1$mean_los, Summary1$mean_mat_age), 1),
  SD = round(c(Summary1$sd_ga, Summary1$sd_weight, Summary1$sd_los, Summary1$sd_mat_age), 1))

n_total <- nrow(Sample_final_all)
Summary1_table <- Summary1_table %>% 
  mutate(n = n_total)

Summary1_table
ft <- flextable(Summary1_table)
ft <- autofit(ft)
ft

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical_summary_table2 <- bind_rows(
  Sample_final_all %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Sample_final_all %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Sample_final_all %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Sample_final_all %>%
    count(Country_2, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country_2),

  Sample_final_all %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Sample_final_all %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Sample_final_all %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary_table2
ft2 <- flextable(Categorical_summary_table2)
ft2 <- autofit(ft2)
ft2

# NICU admission
Table_nicu_all <- tibble("NICU admission" = c("Yes", "No"),
                     n = c(Summary1$Nicu_count, n_total - Summary1$Nicu_count),
                     percent = round(c(Summary1$Nicu_percent, 100 - Summary1$Nicu_percent), 1))

ft3 <- flextable(Table_nicu_all)
ft3 <- autofit(ft3)
ft3

## Newborn GROUP 1
# Numerical
Summary2 <- Group1_final %>%
  ungroup() %>% 
  summarise(
    mean_ga = mean(gestational_age_total_days, na.rm = TRUE), # GA
    sd_ga = sd(gestational_age_total_days, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE), # Birth weight
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE), # LOS
    sd_los = sd(LOS, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE), # Mother's age
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    Nicu_count = sum(admission_neo_n == 1), # NICU
    Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)

Summary2_table <- tibble(
  Variable = c("Gestational age (days)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Mean = round(c(Summary2$mean_ga, Summary2$mean_weight, Summary2$mean_los, Summary2$mean_mat_age), 1),
  SD = round(c(Summary2$sd_ga, Summary2$sd_weight, Summary2$sd_los, Summary2$sd_mat_age), 1))

n_total <- nrow(Group1_final)
Summary2_table <- Summary2_table %>% 
  mutate(n = n_total)

Summary2_table
ft4 <- flextable(Summary2_table)
ft4 <- autofit(ft4)
ft4

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical_summary2_table2 <- bind_rows(
  Group1_final %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Group1_final %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Group1_final %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Group1_final %>%
    count(Country_2, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country_2),

  Group1_final %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Group1_final %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Group1_final %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary2_table2
ft5 <- flextable(Categorical_summary2_table2)
ft5 <- autofit(ft5)
ft5

# NICU admission
Table_nicu_g1 <- tibble("NICU admission" = c("Yes", "No"),
                     n = c(Summary2$Nicu_count, n_total - Summary2$Nicu_count),
                     percent = round(c(Summary2$Nicu_percent, 100 - Summary2$Nicu_percent), 1))

ft6 <- flextable(Table_nicu_g1)
ft6 <- autofit(ft6)
ft6


## Newborn GROUP 2
# Numerical
Summary3 <- Group2_final %>%
  ungroup() %>% 
  summarise(
    mean_ga = mean(gestational_age_total_days, na.rm = TRUE), # GA
    sd_ga = sd(gestational_age_total_days, na.rm = TRUE), 
    mean_weight = mean(Birth_weight_g, na.rm = TRUE), # Birth weight
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE), # LOS
    sd_los = sd(LOS, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE), # Mother's age
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    Nicu_count = sum(admission_neo_n == 1), # NICU
    Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)

Summary3_table <- tibble(
  Variable = c("Gestational age (days)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Mean = round(c(Summary3$mean_ga, Summary3$mean_weight, Summary3$mean_los, Summary3$mean_mat_age), 1),
  SD = round(c(Summary3$sd_ga, Summary3$sd_weight, Summary3$sd_los, Summary3$sd_mat_age), 1))

n_total <- nrow(Group2_final)
Summary3_table <- Summary3_table %>% 
  mutate(n = n_total)

Summary3_table
ft7 <- flextable(Summary3_table)
ft7 <- autofit(ft7)
ft7

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical_summary3_table2 <- bind_rows(
  Group2_final %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Group2_final %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Group2_final %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Group2_final %>%
    count(Country_2, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country_2),

  Group2_final %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Group2_final %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Group2_final %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary3_table2
ft8 <- flextable(Categorical_summary3_table2)
ft8 <- autofit(ft8)
ft8

# NICU admission
Table_nicu_g2 <- tibble("NICU admission" = c("Yes", "No"),
                     n = c(Summary3$Nicu_count, n_total - Summary3$Nicu_count),
                     percent = round(c(Summary3$Nicu_percent, 100 - Summary3$Nicu_percent), 1))

ft9 <- flextable(Table_nicu_g2)
ft9 <- autofit(ft9)
ft9

# Newborn GROUP 2 - NICU ADMISSION (n = 44)
# Numerical
Summary4 <- Group2_final_nicu %>%
  ungroup() %>% 
  summarise(
    min_ga = min(Weeks_LPM, na.rm = TRUE), # gestational age
    max_ga = max(Weeks_LPM, na.rm = TRUE),
    mean_ga = mean(Weeks_LPM, na.rm = TRUE),
    sd_ga = sd(Weeks_LPM, na.rm = TRUE),
    median_ga = median(Weeks_LPM, na.rm = TRUE),
    iqr_ga = IQR(Weeks_LPM, na.rm = TRUE),
    min_weight = min(Birth_weight_g, na.rm = TRUE), # birth weight
    max_weight = max(Birth_weight_g, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE),
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    median_weight = median(Birth_weight_g, na.rm = TRUE),
    iqr_weight = IQR(Birth_weight_g, na.rm = TRUE),
    min_los = min(LOS, na.rm = TRUE), # LOS
    max_los = max(LOS, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE),
    sd_los = sd(LOS, na.rm = TRUE),
    median_los = median(LOS, na.rm = TRUE),
    iqr_los = IQR(LOS, na.rm = TRUE),
    min_mat_age = min(Maternal_age, na.rm = TRUE), # mother age
    max_mat_age = max(Maternal_age, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE),
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    median_mat_age = median(Maternal_age, na.rm = TRUE),
    iqr_mat_age = IQR(Maternal_age, na.rm = TRUE))

Summary4_table <- tibble(
  Variable = c("Gestational age (weeks)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Min = round(c(Summary4$min_ga, Summary4$min_weight, Summary4$min_los, Summary4$min_mat_age), 1),
  Max = round(c(Summary4$max_ga, Summary4$max_weight, Summary4$max_los, Summary4$max_mat_age), 1),
  Mean = round(c(Summary4$mean_ga, Summary4$mean_weight, Summary4$mean_los, Summary4$mean_mat_age), 1),
  SD = round(c(Summary4$sd_ga, Summary4$sd_weight, Summary4$sd_los, Summary4$sd_mat_age), 1),
  Median = round(c(Summary4$median_ga, Summary4$median_weight, Summary4$median_los, Summary4$median_mat_age), 1),
  IQR = round(c(Summary4$iqr_ga, Summary4$iqr_weight, Summary4$iqr_los, Summary4$iqr_mat_age), 1))

n_total <- nrow(Group2_final_nicu)
Summary4_table <- Summary4_table %>% 
  mutate(n = n_total)

Summary4_table
ft10 <- flextable(Summary4_table)
ft10 <- autofit(ft10)
ft10

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical_summary4_table2 <- bind_rows(
  Group2_final_nicu %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Group2_final_nicu %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Group2_final_nicu %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Group2_final_nicu %>%
    count(Country, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country),

  Group2_final_nicu %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Group2_final_nicu %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Group2_final_nicu %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary4_table2
ft11 <- flextable(Categorical_summary4_table2)
ft11 <- autofit(ft11)
ft11


# Newborn GROUP 1 - NICU ADMISSION (n = 2)
# Numerical
Summary5 <- Group1_final_nicu %>%
  ungroup() %>% 
  summarise(
    min_ga = min(Weeks_LPM, na.rm = TRUE), # gestational age
    max_ga = max(Weeks_LPM, na.rm = TRUE),
    mean_ga = mean(Weeks_LPM, na.rm = TRUE),
    sd_ga = sd(Weeks_LPM, na.rm = TRUE),
    median_ga = median(Weeks_LPM, na.rm = TRUE),
    iqr_ga = IQR(Weeks_LPM, na.rm = TRUE),
    min_weight = min(Birth_weight_g, na.rm = TRUE), # birth weight
    max_weight = max(Birth_weight_g, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE),
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    median_weight = median(Birth_weight_g, na.rm = TRUE),
    iqr_weight = IQR(Birth_weight_g, na.rm = TRUE),
    min_los = min(LOS, na.rm = TRUE), # LOS
    max_los = max(LOS, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE),
    sd_los = sd(LOS, na.rm = TRUE),
    median_los = median(LOS, na.rm = TRUE),
    iqr_los = IQR(LOS, na.rm = TRUE),
    min_mat_age = min(Maternal_age, na.rm = TRUE), # mother age
    max_mat_age = max(Maternal_age, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE),
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    median_mat_age = median(Maternal_age, na.rm = TRUE),
    iqr_mat_age = IQR(Maternal_age, na.rm = TRUE))

Summary5_table <- tibble(
  Variable = c("Gestational age (weeks)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Min = round(c(Summary5$min_ga, Summary5$min_weight, Summary5$min_los, Summary5$min_mat_age), 1),
  Max = round(c(Summary5$max_ga, Summary5$max_weight, Summary5$max_los, Summary5$max_mat_age), 1),
  Mean = round(c(Summary5$mean_ga, Summary5$mean_weight, Summary5$mean_los, Summary5$mean_mat_age), 1),
  SD = round(c(Summary5$sd_ga, Summary5$sd_weight, Summary5$sd_los, Summary5$sd_mat_age), 1),
  Median = round(c(Summary5$median_ga, Summary5$median_weight, Summary5$median_los, Summary5$median_mat_age), 1),
  IQR = round(c(Summary5$iqr_ga, Summary5$iqr_weight, Summary5$iqr_los, Summary5$iqr_mat_age), 1))

n_total <- nrow(Group1_final_nicu)
Summary5_table <- Summary5_table %>% 
  mutate(n = n_total)

Summary5_table
ft12 <- flextable(Summary5_table)
ft12 <- autofit(ft12)
ft12

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical_summary5_table2 <- bind_rows(
  Group1_final_nicu %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Group1_final_nicu %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Group1_final_nicu %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Group1_final_nicu %>%
    count(Country, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country),

  Group1_final_nicu %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Group1_final_nicu %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Group1_final_nicu %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical_summary5_table2
ft13 <- flextable(Categorical_summary5_table2)
ft13 <- autofit(ft13)
ft13

# Newborn Total sample - NICU ADMISSION (n = 46)
# Numerical
Summary6 <- Sample_final_all_nicu %>%
  ungroup() %>% 
  summarise(
    mean_ga = mean(gestational_age_total_days, na.rm = TRUE), # GA
    sd_ga = sd(gestational_age_total_days, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE), # Birth weight
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE), # LOS
    sd_los = sd(LOS, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE), # maternal age
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    Nicu_count = sum(admission_neo_n == 1), # NICU
    Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)

Summary6_table <- tibble(
  Variable = c("Gestational age (days)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Mean = round(c(Summary6$mean_ga, Summary6$mean_weight, Summary6$mean_los, Summary6$mean_mat_age), 1),
  SD = round(c(Summary6$sd_ga, Summary6$sd_weight, Summary6$sd_los, Summary6$sd_mat_age), 1))

n_total <- nrow(Sample_final_all_nicu)
Summary6_table <- Summary6_table %>% 
  mutate(n = n_total)

Summary6_table
ft14 <- flextable(Summary6_table)
ft14 <- autofit(ft14)
ft14

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical6_summary_table2 <- bind_rows(
  Sample_final_all_nicu %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Sample_final_all_nicu %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Sample_final_all_nicu %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Sample_final_all_nicu %>%
    count(Country_2, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country_2),

  Sample_final_all_nicu %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Sample_final_all_nicu %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Sample_final_all_nicu %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical6_summary_table2
ft15 <- flextable(Categorical6_summary_table2)
ft15 <- autofit(ft15)
ft15

# No missing sample (multivariable model)
# Numerical
Summary7 <- Sample_no_missing %>%
  ungroup() %>% 
  summarise(
    mean_ga = mean(Gest_age, na.rm = TRUE), # GA
    sd_ga = sd(Gest_age, na.rm = TRUE),
    mean_weight = mean(Birth_weight_g, na.rm = TRUE), # Birth weight
    sd_weight = sd(Birth_weight_g, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE), # LOS
    sd_los = sd(LOS, na.rm = TRUE),
    mean_mat_age = mean(Maternal_age, na.rm = TRUE), # maternal age
    sd_mat_age = sd(Maternal_age, na.rm = TRUE),
    Nicu_count = sum(admission_neo_n == 1), # NICU
    Nicu_percent = (sum(admission_neo_n == 1) / n()) * 100)

Summary7_table <- tibble(
  Variable = c("Gestational age (days)", "Birth weight (g)", "LOS (hours)", "Maternal age (years)"),
  Mean = round(c(Summary7$mean_ga, Summary7$mean_weight, Summary7$mean_los, Summary7$mean_mat_age), 1),
  SD = round(c(Summary7$sd_ga, Summary7$sd_weight, Summary7$sd_los, Summary7$sd_mat_age), 1))

n_total <- nrow(Sample_no_missing)
Summary7_table <- Summary7_table %>% 
  mutate(n = n_total)

Summary7_table
ft16 <- flextable(Summary7_table)
ft16 <- autofit(ft16)
ft16   

# Categorical (Mode of delivery, parity, Feeding type, Country, HHH)
Categorical7_summary_table2 <- bind_rows(
  Sample_no_missing %>%
    count(Birth_mode, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Birth Mode", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Birth_mode),
  
  Sample_no_missing %>%
    count(RF_parity, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Parity", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = RF_parity),
  
  Sample_no_missing %>%
    count(Feeding_group, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Feeding Type", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Feeding_group),
  
  Sample_no_missing %>%
    count(Country_2, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Country", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Country_2),

  Sample_no_missing %>%
    count(Hypothermia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypothermia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypothermia_cat),

  Sample_no_missing %>%
    count(Hypoglycaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hypoglycaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hypoglycaemia_cat),

  Sample_no_missing %>%
    count(Hyperbilirubinaemia_cat, name = "n") %>%
    ungroup() %>%
    mutate(Variable = "Hyperbilirubinaemia", Percent = round(n / sum(n) * 100, 1)) %>%
    rename(Category = Hyperbilirubinaemia_cat)) %>%
  select(Variable, Category, n, Percent)

Categorical7_summary_table2
ft17 <- flextable(Categorical7_summary_table2)
ft17 <- autofit(ft17)
ft17

# NICU admission
Table_nomissing_nicu_all <- tibble("NICU admission" = c("Yes", "No"),
                     n = c(Summary7$Nicu_count, n_total - Summary7$Nicu_count),
                     percent = round(c(Summary7$Nicu_percent, 100 - Summary7$Nicu_percent), 1))

ft18 <- flextable(Table_nomissing_nicu_all)
ft18 <- autofit(ft18)
ft18

## Inferential Analysis
# 1. Logistic regression --------------------------------------------------
# 1. Logistic regression --------------------------------------------------
# 1.1 Bivariable regression HHH and NICU admission --------------------------------------------

# HYPOTHERMIA
summary(glm(admission_neo_n ~ Hypothermia_cat, data = Sample_Hypotherm, family = binomial))
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -5.2038     0.2006 -25.948  < 2e-16 ***
# Hypothermia_catMild              0.0834     0.4913   0.170    0.865    
# Hypothermia_catModerate_Severe   1.8273     0.3304   5.531 3.19e-08 ***

mod_hypot <- glm(admission_neo_n ~ Hypothermia_cat, data = Sample_Hypotherm, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypot), confint.default(mod_hypot, level = 0.95)))
#                                Odds ratio       2.5 %       97.5 %
# (Intercept)                    0.005495713 0.003709511  0.008142007
# Hypothermia_catMild            1.086977300 0.414945945  2.847406188
# Hypothermia_catModerate_Severe 6.217312073 3.253609241 11.880642864

# Interpretation: Newborns with moderate/severe hypothermia had a significantly increased likelihood of admission to the 
# NICU (OR = 6.22, 95% CI [3.25–11.88], p < 0.001) compared to normothermic newborns. In newborns with mild hypothermia there 
# was no statistically significant difference compared to normothermic newborns (OR = 1.09, 95% CI [0.41–2.85], p = 0.865).

# HYPOGLYCAEMIA
summary(glm(admission_neo_n ~ Hypoglycaemia_cat, data = Sample_Hypoglyc, family = binomial))
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 -4.6657     0.2437 -19.147   <2e-16 ***
# Hypoglycaemia_catMild      -14.9004   969.6567  -0.015    0.988    
# Hypoglycaemia_catModerate  -14.9004  2776.6742  -0.005    0.996    
# Hypoglycaemia_catSevere      3.7212     0.3982   9.344   <2e-16 ***

# table(Sample_Hypoglyc$Hypoglycaemia_cat, Sample_Hypoglyc$admission_neo_n)
#                   0    1
# Normoglycaemic 1806   17
# Mild            123    0
# Moderate         15    0
# Severe           36   14

mod_hypoglyc <- glm(admission_neo_n ~ Hypoglycaemia_cat, data = Sample_Hypoglyc, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypoglyc), confint.default(mod_hypoglyc, level = 0.95)))
#                           Odds ratio        2.5 %      97.5 %
# (Intercept)               9.413068e-03  0.005838686  0.01517565
# Hypoglycaemia_catMild     3.379350e-07  0.000000000         Inf
# Hypoglycaemia_catModerate 3.379350e-07  0.000000000         Inf
# Hypoglycaemia_catSevere   4.131373e+01 18.928846831 90.17051747

# Interpretation: Newborns with severe hypoglycaemia had a significantly increased likelihood of admission to the NICU (OR = 41.31, 95% CI [18.93–90.17], p < 0.001)
# compared to normoglycaemic newborns. No estimates could be determined for mild and moderate hypoglycaemia, as no NICU admissions were observed in these groups.

# HYPERBILIRUBINAEMIA
summary(glm(admission_neo_n ~ Hyperbilirubinaemia_cat, data = Sample_Hyperbili, family = binomial))
#                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       -5.6206     0.2298 -24.456   <2e-16 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb     3.6057     0.5798   6.219    5e-10 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum   5.7747     0.6020   9.593   <2e-16 ***

#table(Sample_Hyperbili$Hyperbilirubinaemia_cat, Sample_Hyperbili$admission_neo_n)
#                              0    1
# Physiological             5245   19
# Hyperbilirubinaemia_tcb     30    4
# Hyperbilirubinaemia_serum    6    7

mod_hyperbili <- glm(admission_neo_n ~ Hyperbilirubinaemia_cat, data = Sample_Hyperbili, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili), confint.default(mod_hyperbili, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      3.622498e-03  0.002308755 5.683794e-03
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.680702e+01 11.814463286 1.146693e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 3.220614e+02 98.982214017 1.047901e+03

# Interpretation: Those newborns with hyperbilirubinaemia detected by TcB had significantly higher odds of NICU admission (OR = 36.81, 95% CI [11.81–114.67], p < 0.001), 
# compared to newborns with physiological bilirubin levels. The association was also significant in newborns with hyperbilirubinaemia diagnosed by serum 
# measurement (OR = 322.06, 95% CI [98.98-1,047.90], p < 0.001).

summary(glm(admission_neo_n ~ Hyperbilirubinaemia_bi, data = Sample_Hyperbili, family = binomial))
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -5.6206     0.2298  -24.46   <2e-16 ***
# Hyperbilirubinaemia_bi   4.4350     0.4141   10.71   <2e-16 ***

mod_hyperbili2 <- glm(admission_neo_n ~ Hyperbilirubinaemia_bi, data = Sample_Hyperbili, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili2), confint.default(mod_hyperbili2, level = 0.95)))
# Odds ratio                2.5 %       97.5 %
# (Intercept)             0.003622498  0.002308755 5.683794e-03
# Hyperbilirubinaemia_bi 84.349415183 37.460157543 1.899304e+02

# Interpretation: Newborns with hyperbilirubinaemia had a significantly higher likelihood of admission to the NICU compared 
# to those with physiological jaundice (OR = 84.35, 95% CI [37.46–189.93], p < 0.001).



# 1.2 Bivariable regression with associations between HHH ------------------
## Hypothermia - Hypoglycaemia_cat
summary(glm(Hypothermia_bi ~ Hypoglycaemia_cat, data = Sample_hypot_hypo, family = binomial))
#                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.83141    0.05095 -16.319  < 2e-16 ***
# Hypoglycaemia_catMild      0.17463    0.19685   0.887    0.375    
# Hypoglycaemia_catModerate  0.13827    0.55009   0.251    0.802    
# Hypoglycaemia_catSevere    1.32096    0.29578   4.466 7.97e-06 ***

mod_hypot_hypogl <- glm(Hypothermia_bi ~ Hypoglycaemia_cat, data = Sample_hypot_hypo, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypot_hypogl), confint.default(mod_hypot_hypogl, level = 0.95)))
#                             Odds ratio     2.5 %   97.5 %
# (Intercept)                0.4354331 0.3940526 0.481159
# Hypoglycaemia_catMild      1.1908111 0.8096209 1.751475
# Hypoglycaemia_catModerate  1.1482821 0.3906733 3.375075
# Hypoglycaemia_catSevere    3.7470258 2.0985482 6.690436

# Interpretation: Compared to normoglycaemic newborns (30.4 % probability of hypothermia), newborns with severe hypoglycaemia 
# have a 3.75-fold increased risk of hypothermia compared to normoglycaemic newborns (OR = 3.75, 95% CI: 2.10-6.69, p < 0.001).

## Hypothermia - Hypoglycaemia_bi
summary(glm(Hypothermia_bi ~ Hypoglycaemia_bi, data = Sample_hypot_hypo_bi, family = binomial))
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -0.83141    0.05095 -16.319  < 2e-16 ***
# Hypoglycaemia_bi  0.48764    0.15655   3.115  0.00184 ** 

mod_hypot_hypogl2 <- glm(Hypothermia_bi ~ Hypoglycaemia_bi, data = Sample_hypot_hypo_bi, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypot_hypogl2), confint.default(mod_hypot_hypogl2, level = 0.95)))
#                   Odds ratio     2.5 %   97.5 %
# (Intercept)       0.4354331 0.3940526 0.481159
# Hypoglycaemia_bi  1.6284728 1.1981936 2.213268


## Hypoglycaemia - Hypothermia_cat
summary(glm(Hypoglycaemia_bi ~ Hypothermia_cat, data = Sample_hypo_hypot, family = binomial))
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    -2.44629    0.09939 -24.613  < 2e-16 ***
# Hypothermia_catMild             0.26507    0.20415   1.298 0.194142    
# Hypothermia_catModerate_Severe  0.71443    0.19300   3.702 0.000214 ***

mod_hypogly_hypot <- glm(Hypoglycaemia_bi ~ Hypothermia_cat, data = Sample_hypo_hypot, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypogly_hypot), confint.default(mod_hypogly_hypot, level = 0.95)))
#                                 Odds ratio      2.5 %    97.5 %
# (Intercept)                    0.08661417 0.07128339 0.1052421
# Hypothermia_catMild            1.30351906 0.87367136 1.9448525
# Hypothermia_catModerate_Severe 2.04302282 1.39955690 2.9823312

# Interpretation: moderate to severe hypothermia was significantly associated with an increased risk of hypoglycaemia (OR = 2.04, 95% CI: 1.40-2.98, p < 0.001).
# For mild hypothermia, the increase in odds was not statistically significant (OR = 1.30, 95% CI 087-1.94, p = 0.194).

## Hypoglycaemia - Hypothermia_bi
summary(glm(Hypoglycaemia_bi ~ Hypothermia_bi, data = Sample_hypo_hypot_bi, family = binomial))
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -2.44629    0.09939 -24.613  < 2e-16 ***
# Hypothermia_bi  0.48764    0.15655   3.115  0.00184 ** 

mod_hypogly_hypot2 <- glm(Hypoglycaemia_bi ~ Hypothermia_bi, data = Sample_hypo_hypot_bi, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypogly_hypot2), confint.default(mod_hypogly_hypot2, level = 0.95)))
#                 Odds ratio      2.5 %    97.5 %
# (Intercept)    0.08661417 0.07128339 0.1052421
# Hypothermia_bi 1.62847279 1.19819359 2.2132681


## Hypothermia - Hyperbilirubinaemia_cat
summary(glm(Hypothermia_bi ~ Hyperbilirubinaemia_cat, data = Sample_hypot_bili, family = binomial))
#                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                      -1.26023    0.03322 -37.934   <2e-16 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    0.52263    0.36809   1.420    0.156    
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum -0.44452    0.76942  -0.578    0.563 

mod_hypot_hyperbi <- glm(Hypothermia_bi ~ Hyperbilirubinaemia_cat, data = Sample_hypot_bili, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypot_hyperbi), confint.default(mod_hypot_hyperbi, level = 0.95)))
#                                                   Odds ratio     2.5 %    97.5 %
# (Intercept)                                       0.2835894 0.2657122 0.3026693
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    1.6864556 0.8197007 3.4697207
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  0.6411319 0.1419099 2.8965563

# Interpretation: The model does not support a relevant association between hyperbilirubinaemia categories and hypothermia.

## Hypothermia - Hyperbilirubinaemia_bi
summary(glm(Hypothermia_bi ~ Hyperbilirubinaemia_bi, data = Sample_hypot_bili_bi, family = binomial))
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -1.26023    0.03322 -37.934   <2e-16 ***
# Hyperbilirubinaemia_bi  0.29882    0.32778   0.912    0.362  

mod_hypot_hyperbi_2 <- glm(Hypothermia_bi ~ Hyperbilirubinaemia_bi, data = Sample_hypot_bili_bi, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypot_hyperbi_2), confint.default(mod_hypot_hyperbi_2, level = 0.95)))
#                         Odds ratio     2.5 %    97.5 %
# (Intercept)             0.2835894 0.2657122 0.3026693
# Hyperbilirubinaemia_bi  1.3482626 0.7092011 2.5631827

# Interpretation: There was a 35% increase in the odds of hypothermia in newborns with hyperbilirubinaemia, but this difference 
# was not statistically significant (OR = 1.35, 95% CI: 0.71-2.56, p = 0.362).

# Hypoglycaemia - Hyperbilirubinaemia_cat
summary(glm(Hypoglycaemia_bi ~ Hyperbilirubinaemia_cat, data = Sample_hypo_bili_cat, family = binomial))
#                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       -2.2689     0.0801 -28.327   <2e-16 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    -0.4392     1.0359  -0.424    0.672    
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum -12.2972   441.3717  -0.028    0.978    

mod_hypogly_hyperbili <- glm(Hypoglycaemia_bi ~ Hyperbilirubinaemia_cat, data = Sample_hypo_bili_cat, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypogly_hyperbili), confint.default(mod_hypogly_hyperbili, level = 0.95)))
#                                                   Odds ratio      2.5 %    97.5 %
# (Intercept)                                      1.034275e-01 0.08840134 0.1210078
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   6.445736e-01 0.08462831 4.9094111
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 4.564581e-06 0.00000000       Inf

table(Sample_hypo_bili_cat$Hyperbilirubinaemia_cat, Sample_hypo_bili_cat$Hypoglycaemia_bi)
#                              0    1
# Physiological             1663  172
# Hyperbilirubinaemia_tcb     15    1
# Hyperbilirubinaemia_serum    4    0

# Hypoglycaemia - Hyperbilirubinaemia_bi
summary(glm(Hypoglycaemia_bi ~ Hyperbilirubinaemia_bi, data = Sample_hypo_bili, family = binomial))
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -2.2689     0.0801 -28.327   <2e-16 ***
# Hyperbilirubinaemia_bi  -0.6755     1.0291  -0.656    0.512     

mod_hypogly_hyperbili2 <- glm(Hypoglycaemia_bi ~ Hyperbilirubinaemia_bi, data = Sample_hypo_bili, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hypogly_hyperbili2), confint.default(mod_hypogly_hyperbili2, level = 0.95)))
#                         Odds ratio      2.5 %    97.5 %
# (Intercept)             0.1034275 0.08840134 0.1210078
# Hyperbilirubinaemia_bi  0.5088739 0.06771479 3.8241673

# Interpretation: Neither the categorical nor the binary modelling showed an association between hyperbilirubinaemia requiring treatment and hypoglycaemia.

# Hyperbilirubinaemia - Hypoglycaemia_cat
summary(glm(Hyperbilirubinaemia_bi ~ Hypoglycaemia_cat, data = Sample_bili_hypo, family = binomial))
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 -4.4719     0.2307 -19.382   <2e-16 ***
# Hypoglycaemia_catMild       -0.3156     1.0303  -0.306    0.759    
# Hypoglycaemia_catModerate  -14.0941  1684.1381  -0.008    0.993    
# Hypoglycaemia_catSevere    -14.0941  1072.3152  -0.013    0.990     

mod_hyperbili_hypogl <- glm(Hyperbilirubinaemia_bi ~ Hypoglycaemia_cat, data = Sample_bili_hypo, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili_hypogl), confint.default(mod_hyperbili_hypogl, level = 0.95)))
#                           Odds ratio       2.5 %     97.5 %
# (Intercept)               1.142514e-02 0.007268921 0.01795778
# Hypoglycaemia_catMild     7.293860e-01 0.096815427 5.49503218
# Hypoglycaemia_catModerate 7.568286e-07 0.000000000        Inf
# Hypoglycaemia_catSevere   7.568286e-07 0.000000000        Inf

table(Sample_bili_hypo$Hypoglycaemia_cat, Sample_bili_hypo$Hyperbilirubinaemia_bi)
#                   0    1
# Normoglycaemic 1663   19
# Mild            120    1
# Moderate         15    0
# Severe           37    0

# Interpretation: no statistically significant association between the presence of hypoglycaemia and hyperbilirubinaemia

# Hyperbilirubinaemia - Hypoglycaemia_bi
summary(glm(Hyperbilirubinaemia_bi ~ Hypoglycaemia_bi, data = Sample_bili_hypo_bi, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -4.4719     0.2307 -19.382   <2e-16 ***
# Hypoglycaemia_bi  -0.6756     1.0288  -0.657    0.511       

mod_hyperbili_hypogl2 <- glm(Hyperbilirubinaemia_bi ~ Hypoglycaemia_bi, data = Sample_bili_hypo_bi, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili_hypogl2), confint.default(mod_hyperbili_hypogl2, level = 0.95)))
#                   Odds ratio      2.5 %     97.5 %
# (Intercept)      0.01142514 0.00726893 0.01795776
# Hypoglycaemia_bi 0.50887406 0.06775407 3.82195206

# Hyperbilirubinaemia - Hypothermia_cat
summary(glm(Hyperbilirubinaemia_bi ~ Hypothermia_cat, data = Sample_bili_hypot, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -4.7926     0.1722 -27.831   <2e-16 ***
# Hypothermia_catMild              0.4685     0.3619   1.295    0.195    
# Hypothermia_catModerate_Severe  -0.1200     0.6045  -0.199    0.843     

mod_hyperbili_hypot <- glm(Hyperbilirubinaemia_bi ~ Hypothermia_cat, data = Sample_bili_hypot, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili_hypot), confint.default(mod_hyperbili_hypot, level = 0.95)))
#                                 Odds ratio       2.5 %     97.5 %
# (Intercept)                    0.008290661 0.005915765 0.01161896
# Hypothermia_catMild            1.597584725 0.785971556 3.24728921
# Hypothermia_catModerate_Severe 0.886894474 0.271236899 2.89998083

# Hyperbilirubinaemia - Hypothermia_bi
summary(glm(Hyperbilirubinaemia_bi ~ Hypothermia_bi, data = Sample_bili_hypot_bi, family = binomial))
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -4.7926     0.1722 -27.831   <2e-16 ***
# Hypothermia_bi   0.2988     0.3278   0.912    0.362    

mod_hyperbili_hypot2 <- glm(Hyperbilirubinaemia_bi ~ Hypothermia_bi, data = Sample_bili_hypot_bi, family = binomial)
exp(cbind("Odds ratio" = coef(mod_hyperbili_hypot2), confint.default(mod_hyperbili_hypot2, level = 0.95)))
#                  Odds ratio       2.5 %     97.5 %
# (Intercept)    0.008290661 0.005915765 0.01161896
# Hypothermia_bi 1.348262603 0.709206590 2.56316294



# 1.3 Multivariable logistic regression  -----------------------------------

## Model NICU ~ HHH
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat, data = Sample_no_missing, family = binomial)) 
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                        -5.1632     0.3648 -14.153  < 2e-16 ***
# Hypothermia_catMild                                -0.5756     0.8215  -0.701    0.484    
# Hypothermia_catModerate_Severe                      0.6934     0.5857   1.184    0.236    
# Hypoglycaemia_catMild                             -14.5726   958.8047  -0.015    0.988    
# Hypoglycaemia_catModerate                         -14.4323  2755.2378  -0.005    0.996    
# Hypoglycaemia_catSevere                             2.8577     0.6445   4.434 9.25e-06 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb      3.3970     0.8549   3.973 7.08e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum    6.1114     1.2087   5.056 4.28e-07 ***
# AIC: 177.34

mod1 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod1), confint.default(mod1, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      5.723526e-03  0.002799924 1.169987e-02
# Hypothermia_catMild                              5.623847e-01  0.112402256 2.813792e+00
# Hypothermia_catModerate_Severe                   2.000520e+00  0.634695753 6.305509e+00
# Hypoglycaemia_catMild                            4.690514e-07  0.000000000          Inf
# Hypoglycaemia_catModerate                        5.396979e-07  0.000000000          Inf
# Hypoglycaemia_catSevere                          1.742199e+01  4.926120456 6.161556e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   2.987534e+01  5.592361779 1.595991e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 4.509485e+02 42.194523587 4.819454e+03


## Model + Gest_age
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age, data = Sample_no_missing, family = binomial)) 
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                         4.66898    8.25631   0.566 0.571730    
# Hypothermia_catMild                                -0.56098    0.82266  -0.682 0.495300    
# Hypothermia_catModerate_Severe                      0.70461    0.58469   1.205 0.228163    
# Hypoglycaemia_catMild                             -14.56659  957.67469  -0.015 0.987864    
# Hypoglycaemia_catModerate                         -14.37422 2730.26472  -0.005 0.995799    
# Hypoglycaemia_catSevere                             2.78763    0.64445   4.326 1.52e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb      3.35501    0.86560   3.876 0.000106 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum    6.32238    1.23907   5.103 3.35e-07 ***
# Gest_age                                           -0.03570    0.03007  -1.187 0.235087 
# AIC: 177.91

mod2 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod2), confint.default(mod2, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      1.065890e+02 9.998447e-06 1.136297e+09
# Hypothermia_catMild                              5.706518e-01 1.137943e-01 2.861685e+00
# Hypothermia_catModerate_Severe                   2.023065e+00 6.431616e-01 6.363552e+00
# Hypoglycaemia_catMild                            4.718568e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        5.719454e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.624246e+01 4.592966e+00 5.743950e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   2.864581e+01 5.251233e+00 1.562647e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 5.568967e+02 4.909953e+01 6.316434e+03
# Gest_age                                         9.649254e-01 9.096987e-01 1.023505e+00  --> only term newborns


## Model + Gest_age + Birth_mode
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode, data = Sample_no_missing, family = binomial))
#                                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                         4.09750    8.77186   0.467 0.640415    
# Hypothermia_catMild                                -0.55890    0.85362  -0.655 0.512637    
# Hypothermia_catModerate_Severe                      0.78321    0.59079   1.326 0.184936    
# Hypoglycaemia_catMild                             -15.50611 1551.71696  -0.010 0.992027    
# Hypoglycaemia_catModerate                         -15.61528 4448.24881  -0.004 0.997199    
# Hypoglycaemia_catSevere                             2.86576    0.65329   4.387 1.15e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb      3.41286    0.91325   3.737 0.000186 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum    5.62458    1.25395   4.485 7.27e-06 ***
# Gest_age                                           -0.03611    0.03166  -1.141 0.254067    
# Birth_modeInstrumental_vaginal                      0.44891    0.92891   0.483 0.628909    
# Birth_modeC-section_pl                              0.71609    0.74120   0.966 0.333980    
# Birth_modeC-section_upl                             1.61108    0.67593   2.383 0.017149 *
# AIC: 178.09

mod3 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod3), confint.default(mod3, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      6.018982e+01 2.055414e-06 1.762572e+09
# Hypothermia_catMild                              5.718391e-01 1.073174e-01 3.047036e+00
# Hypothermia_catModerate_Severe                   2.188489e+00 6.874891e-01 6.966636e+00
# Hypoglycaemia_catMild                            1.844091e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.653372e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.756239e+01 4.880880e+00 6.319299e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.035203e+01 5.067898e+00 1.817806e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 2.771545e+02 2.373324e+01 3.236583e+03
# Gest_age                                         9.645309e-01 9.064919e-01 1.026286e+00
# Birth_modeInstrumental_vaginal                   1.566602e+00 2.536689e-01 9.674986e+00
# Birth_modeC-section_pl                           2.046424e+00 4.787239e-01 8.747948e+00
# Birth_modeC-section_upl                          5.008200e+00 1.331463e+00 1.883798e+01

## Model + Gest_age + Birth_mode + Feeding_group
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group, data = Sample_no_missing, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                         2.23744    9.06876   0.247 0.805126    
# Hypothermia_catMild                                -0.50704    0.84597  -0.599 0.548933    
# Hypothermia_catModerate_Severe                      0.55842    0.63782   0.876 0.381292    
# Hypoglycaemia_catMild                             -15.78664 1504.43276  -0.010 0.991628    
# Hypoglycaemia_catModerate                         -15.64511 4476.10970  -0.003 0.997211    
# Hypoglycaemia_catSevere                             2.61353    0.69732   3.748 0.000178 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb      3.50337    0.90786   3.859 0.000114 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum    5.68995    1.37974   4.124 3.72e-05 ***
#   Gest_age                                           -0.03378    0.03235  -1.044 0.296400    
# Birth_modeInstrumental_vaginal                      0.51451    0.93620   0.550 0.582611    
# Birth_modeC-section_pl                              0.55287    0.78292   0.706 0.480087    
# Birth_modeC-section_upl                             1.69269    0.69078   2.450 0.014270 *  
# Feeding_groupPartly_breastfed                       1.24556    1.51850   0.820 0.412066    
# Feeding_groupMixed_feeding_no_breastfed             3.87566    1.79117   2.164 0.030483 *  
# Feeding_groupFormula_only                           1.98926    2.22322   0.895 0.370912  
# AIC: 178.05 --> better model than without feeding type

mod4 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod4), confint.default(mod4, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      9.369288e+00 1.787994e-07 4.909612e+08
# Hypothermia_catMild                              6.022763e-01 1.147380e-01 3.161437e+00
# Hypothermia_catModerate_Severe                   1.747907e+00 5.007364e-01 6.101371e+00
# Hypoglycaemia_catMild                            1.392998e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.604786e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.364717e+01 3.479248e+00 5.353034e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.322715e+01 5.606899e+00 1.969081e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 2.958776e+02 1.980027e+01 4.421330e+03
# Gest_age                                         9.667822e-01 9.073817e-01 1.030071e+00
# Birth_modeInstrumental_vaginal                   1.672825e+00 2.670253e-01 1.047970e+01
# Birth_modeC-section_pl                           1.738231e+00 3.747020e-01 8.063597e+00
# Birth_modeC-section_upl                          5.434078e+00 1.403231e+00 2.104372e+01
# Feeding_groupPartly_breastfed                    3.474896e+00 1.771726e-01 6.815335e+01
# Feeding_groupMixed_feeding_no_breastfed          4.821475e+01 1.440570e+00 1.613709e+03
# Feeding_groupFormula_only                        7.310140e+00 9.365208e-02 5.706029e+02

## Model + Gest_age + Birth_mode + Feeding_group + Birth_weight
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g, 
            data = Sample_no_missing, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       2.513e+00  9.115e+00   0.276 0.782770    
# Hypothermia_catMild                              -4.547e-01  8.571e-01  -0.531 0.595720    
# Hypothermia_catModerate_Severe                    6.079e-01  6.522e-01   0.932 0.351321    
# Hypoglycaemia_catMild                            -1.581e+01  1.497e+03  -0.011 0.991573    
# Hypoglycaemia_catModerate                        -1.564e+01  4.465e+03  -0.004 0.997204    
# Hypoglycaemia_catSevere                           2.602e+00  6.962e-01   3.738 0.000186 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    3.488e+00  9.060e-01   3.850 0.000118 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  5.731e+00  1.385e+00   4.138  3.5e-05 ***
# Gest_age                                         -3.750e-02  3.406e-02  -1.101 0.270928    
# Birth_modeInstrumental_vaginal                    5.510e-01  9.386e-01   0.587 0.557209    
# Birth_modeC-section_pl                            5.300e-01  7.852e-01   0.675 0.499681    
# Birth_modeC-section_upl                           1.698e+00  6.913e-01   2.456 0.014059 *  
# Feeding_groupPartly_breastfed                     1.238e+00  1.524e+00   0.812 0.416691    
# Feeding_groupMixed_feeding_no_breastfed           3.929e+00  1.797e+00   2.186 0.028802 *  
# Feeding_groupFormula_only                         2.004e+00  2.220e+00   0.903 0.366565    
# Birth_weight_g                                    2.178e-04  6.091e-04   0.358 0.720597  
# AIC: 179.92 --> little higher than mod4

mod5 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g, 
            data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod5), confint.default(mod5, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      1.234262e+01 2.152101e-07 7.078673e+08
# Hypothermia_catMild                              6.346102e-01 1.182898e-01 3.404605e+00
# Hypothermia_catModerate_Severe                   1.836571e+00 5.114767e-01 6.594617e+00
# Hypoglycaemia_catMild                            1.361077e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.605917e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.349144e+01 3.447150e+00 5.280275e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.273318e+01 5.544138e+00 1.932602e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 3.082076e+02 2.042159e+01 4.651543e+03
# Gest_age                                         9.631949e-01 9.009918e-01 1.029692e+00
# Birth_modeInstrumental_vaginal                   1.734909e+00 2.756342e-01 1.091994e+01
# Birth_modeC-section_pl                           1.698975e+00 3.645835e-01 7.917298e+00
# Birth_modeC-section_upl                          5.460597e+00 1.408769e+00 2.116609e+01
# Feeding_groupPartly_breastfed                    3.447782e+00 1.739139e-01 6.835106e+01
# Feeding_groupMixed_feeding_no_breastfed          5.087762e+01 1.501729e+00 1.723701e+03
# Feeding_groupFormula_only                        7.421093e+00 9.571055e-02 5.754081e+02
# Birth_weight_g                                   1.000218e+00 9.990245e-01 1.001413e+00

## Model + Gest_age + Birth_mode + Feeding_group + Birth_weight + Co_parity
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity,
            data = Sample_no_missing, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       2.622e+00  9.047e+00   0.290 0.771992    
# Hypothermia_catMild                              -4.792e-01  8.617e-01  -0.556 0.578157    
# Hypothermia_catModerate_Severe                    6.530e-01  6.509e-01   1.003 0.315807    
# Hypoglycaemia_catMild                            -1.585e+01  1.484e+03  -0.011 0.991477    
# Hypoglycaemia_catModerate                        -1.567e+01  4.460e+03  -0.004 0.997198    
# Hypoglycaemia_catSevere                           2.659e+00  6.969e-01   3.815 0.000136 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    3.483e+00  9.108e-01   3.824 0.000131 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  5.777e+00  1.398e+00   4.133 3.58e-05 ***
# Gest_age                                         -3.686e-02  3.379e-02  -1.091 0.275258    
# Birth_modeInstrumental_vaginal                    3.986e-01  9.607e-01   0.415 0.678186    
# Birth_modeC-section_pl                            5.348e-01  7.827e-01   0.683 0.494481    
# Birth_modeC-section_upl                           1.567e+00  7.075e-01   2.215 0.026753 *  
# Feeding_groupPartly_breastfed                     1.085e+00  1.473e+00   0.736 0.461509    
# Feeding_groupMixed_feeding_no_breastfed           3.926e+00  1.731e+00   2.268 0.023334 *  
# Feeding_groupFormula_only                         1.943e+00  2.210e+00   0.879 0.379276    
# Birth_weight_g                                    2.298e-04  6.104e-04   0.377 0.706529    
# Co_Parity                                        -2.418e-01  3.485e-01  -0.694 0.487805     
# AIC: 181.41

mod6 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity,
            data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod6), confint.default(mod6, level = 0.95)))
# Odds ratio        2.5 %       97.5 %
# (Intercept)                                      1.375853e+01 2.737915e-07 6.913915e+08
# Hypothermia_catMild                              6.192854e-01 1.143895e-01 3.352707e+00
# Hypothermia_catModerate_Severe                   1.921215e+00 5.364150e-01 6.880994e+00
# Hypoglycaemia_catMild                            1.301674e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.572425e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.427796e+01 3.643036e+00 5.595882e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.255556e+01 5.461906e+00 1.940466e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 3.229414e+02 2.085973e+01 4.999640e+03
# Gest_age                                         9.638079e-01 9.020497e-01 1.029794e+00
# Birth_modeInstrumental_vaginal                   1.489765e+00 2.266723e-01 9.791227e+00
# Birth_modeC-section_pl                           1.707047e+00 3.681109e-01 7.916121e+00
# Birth_modeC-section_upl                          4.792812e+00 1.197822e+00 1.917734e+01
# Feeding_groupPartly_breastfed                    2.958009e+00 1.649498e-01 5.304532e+01
# Feeding_groupMixed_feeding_no_breastfed          5.070908e+01 1.704257e+00 1.508816e+03
# Feeding_groupFormula_only                        6.982152e+00 9.174590e-02 5.313638e+02
# Birth_weight_g                                   1.000230e+00 9.990339e-01 1.001427e+00
# Co_Parity                                        7.852077e-01 3.965710e-01 1.554706e+00

## Model + Gest_age + Birth_mode + Feeding_group + Birth_weight + Co_parity + Maternal_age
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity 
            + Maternal_age, data = Sample_no_missing, family = binomial))
#                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                       3.010e-01  9.430e+00   0.032 0.974534    
# Hypothermia_catMild                              -4.495e-01  8.631e-01  -0.521 0.602541    
# Hypothermia_catModerate_Severe                    6.373e-01  6.565e-01   0.971 0.331720    
# Hypoglycaemia_catMild                            -1.596e+01  1.459e+03  -0.011 0.991270    
# Hypoglycaemia_catModerate                        -1.555e+01  4.469e+03  -0.003 0.997224    
# Hypoglycaemia_catSevere                           2.665e+00  7.040e-01   3.785 0.000154 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    3.478e+00  9.255e-01   3.758 0.000171 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  5.755e+00  1.388e+00   4.147 3.36e-05 ***
# Gest_age                                         -3.465e-02  3.382e-02  -1.025 0.305587    
# Birth_modeInstrumental_vaginal                    3.725e-01  9.722e-01   0.383 0.701581    
# Birth_modeC-section_pl                            4.292e-01  7.956e-01   0.539 0.589556    
# Birth_modeC-section_upl                           1.543e+00  7.104e-01   2.172 0.029820 *  
# Feeding_groupPartly_breastfed                     1.157e+00  1.516e+00   0.763 0.445311    
# Feeding_groupMixed_feeding_no_breastfed           4.062e+00  1.783e+00   2.278 0.022749 *  
# Feeding_groupFormula_only                         2.139e+00  2.259e+00   0.947 0.343723    
# Birth_weight_g                                    2.446e-04  6.100e-04   0.401 0.688465    
# Co_Parity                                        -3.315e-01  3.711e-01  -0.893 0.371650    
# Maternal_age                                      4.911e-02  5.629e-02   0.872 0.383002
# AIC: 182.63

mod7 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity 
            + Maternal_age, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod7), confint.default(mod7, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      1.351235e+00 1.271170e-08 1.436342e+08
# Hypothermia_catMild                              6.379785e-01 1.175297e-01 3.463095e+00
# Hypothermia_catModerate_Severe                   1.891286e+00 5.223050e-01 6.848415e+00
# Hypoglycaemia_catMild                            1.166229e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.767107e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          1.436403e+01 3.614434e+00 5.708370e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   3.240548e+01 5.282377e+00 1.987960e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 3.157175e+02 2.080732e+01 4.790504e+03
# Gest_age                                         9.659460e-01 9.039970e-01 1.032140e+00
# Birth_modeInstrumental_vaginal                   1.451426e+00 2.158889e-01 9.757974e+00
# Birth_modeC-section_pl                           1.536028e+00 3.229967e-01 7.304666e+00
# Birth_modeC-section_upl                          4.680580e+00 1.162971e+00 1.883781e+01
# Feeding_groupPartly_breastfed                    3.181775e+00 1.628792e-01 6.215463e+01
# Feeding_groupMixed_feeding_no_breastfed          5.808945e+01 1.762142e+00 1.914933e+03
# Feeding_groupFormula_only                        8.490144e+00 1.014015e-01 7.108629e+02
# Birth_weight_g                                   1.000245e+00 9.990494e-01 1.001441e+00
# Co_Parity                                        7.178398e-01 3.468747e-01 1.485534e+00
# Maternal_age                                     1.050336e+00 9.406117e-01 1.172859e+00

## Model + Gest_age + Birth_mode + Feeding_group + Birth_weight + Co_parity + Maternal_age + Country
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity 
            + Maternal_age + Country, data = Sample_no_missing, family = binomial))
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                      -1.887e+01  2.077e+03  -0.009 0.992752    
# Hypothermia_catMild                              -5.677e-01  9.419e-01  -0.603 0.546651    
# Hypothermia_catModerate_Severe                    6.818e-01  7.056e-01   0.966 0.333918    
# Hypoglycaemia_catMild                            -1.818e+01  3.678e+03  -0.005 0.996055    
# Hypoglycaemia_catModerate                        -1.742e+01  1.159e+04  -0.002 0.998800    
# Hypoglycaemia_catSevere                           3.128e+00  7.812e-01   4.004 6.23e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    3.858e+00  1.050e+00   3.673 0.000239 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  3.714e+01  2.957e+03   0.013 0.989977    
# Gest_age                                         -1.744e-02  3.509e-02  -0.497 0.619185    
# Birth_modeInstrumental_vaginal                   -2.004e-01  1.099e+00  -0.182 0.855299    
# Birth_modeC-section_pl                            5.162e-01  8.635e-01   0.598 0.549957    
# Birth_modeC-section_upl                           1.775e+00  7.615e-01   2.330 0.019782 *  
# Feeding_groupPartly_breastfed                     1.645e+01  2.077e+03   0.008 0.993680    
# Feeding_groupMixed_feeding_no_breastfed           1.991e+01  2.077e+03   0.010 0.992351    
# Feeding_groupFormula_only                         2.081e+00  3.143e+03   0.001 0.999472    
# Birth_weight_g                                    7.862e-05  6.255e-04   0.126 0.899973    
# Co_Parity                                        -6.083e-01  4.336e-01  -1.403 0.160606    
# Maternal_age                                      4.832e-02  6.457e-02   0.748 0.454276    
# CountryEurope                                    -4.952e-01  6.391e-01  -0.775 0.438437    
# CountryAfrica                                     4.837e-01  1.004e+00   0.482 0.629888    
# CountryAmerica                                   -1.797e+01  5.718e+03  -0.003 0.997493    
# CountryAsia                                      -4.986e+01  3.879e+03  -0.013 0.989743    
# CountryOceania                                   -1.536e+01  2.385e+04  -0.001 0.999486    
# CountryUnknown                                    5.409e+00  1.557e+00   3.474 0.000513 ***

mod8 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity + 
              Maternal_age + Country, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod8), confint.default(mod8, level = 0.95)))
#                                                  Odds ratio       2.5 %      97.5 %
# (Intercept)                                      6.373907e-09  0.00000000         Inf
# Hypothermia_catMild                              5.668078e-01  0.08948074    3.590394
# Hypothermia_catModerate_Severe                   1.977351e+00  0.49602263    7.882539
# Hypoglycaemia_catMild                            1.266245e-08  0.00000000         Inf
# Hypoglycaemia_catModerate                        2.718762e-08  0.00000000         Inf
# Hypoglycaemia_catSevere                          2.282791e+01  4.93745402  105.542907
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   4.737368e+01  6.04682097  371.148054
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 1.353935e+16  0.00000000         Inf
# Gest_age                                         9.827111e-01  0.91739641    1.052676
# Birth_modeInstrumental_vaginal                   8.184326e-01  0.09500636    7.050391
# Birth_modeC-section_pl                           1.675664e+00  0.30845492    9.102946
# Birth_modeC-section_upl                          5.898903e+00  1.32598557   26.242409
# Feeding_groupPartly_breastfed                    1.399662e+07  0.00000000         Inf
# Feeding_groupMixed_feeding_no_breastfed          4.456163e+08  0.00000000         Inf
# Feeding_groupFormula_only                        8.009842e+00  0.00000000         Inf --> If Country is controlled, the effect of Feeding_group is no longer applicable
# Birth_weight_g                                   1.000079e+00  0.99885333    1.001305
# Co_Parity                                        5.442705e-01  0.23268221    1.273112
# Maternal_age                                     1.049504e+00  0.92474557    1.191094
# CountryEurope                                    6.094306e-01  0.17413462    2.132865
# CountryAfrica                                    1.622080e+00  0.22680279   11.601018
# CountryAmerica                                   1.570621e-08  0.00000000         Inf
# CountryAsia                                      2.213693e-22  0.00000000         Inf
# CountryOceania                                   2.130658e-07  0.00000000         Inf
# CountryUnknown                                   2.234165e+02 10.56212279 4725.844516

table(Sample_no_missing$Hypothermia_cat, Sample_no_missing$admission_neo_n)
#                    0    1
# Norm            1262   11
# Mild             321    2
# Moderate_Severe  253    6

table(Sample_no_missing$Hypoglycaemia_cat, Sample_no_missing$admission_neo_n)
#                   0    1
# Normoglycaemic 1667   15
# Mild            121    0
# Moderate         15    0
# Severe           33    4

table(Sample_no_missing$Hyperbilirubinaemia_cat, Sample_no_missing$admission_neo_n)
#                              0    1
# Physiological             1821   14
# Hyperbilirubinaemia_tcb     14    2
# Hyperbilirubinaemia_serum    1    3

table(Sample_no_missing$Feeding_group, Sample_no_missing$admission_neo_n)
#                               0    1
# Fully_breastfed             171    1
# Partly_breastfed           1634   15
# Mixed_feeding_no_breastfed   11    2
# Formula_only                 20    1

table(Sample_no_missing$Country, Sample_no_missing$admission_neo_n)
#               0   1
# Switzerland 739  11
# Europe      766   5
# Africa       91   2
# America      55   0
# Asia        179   0
# Oceania       3   0
# Unknown       3   1

table(Sample_no_missing$Feeding_group, Sample_no_missing$Country)
#                               Switzerland Europe Africa America Asia Oceania Unknown
# Fully_breastfed                     80     68      8       6    8       1       1
# Partly_breastfed                   655    690     84      47  168       2       3
# Mixed_feeding_no_breastfed           5      4      1       1    2       0       0
# Formula_only                        10      9      0       1    1       0       0 

# Define adjusted categories
Sample_no_missing <- Sample_no_missing %>%
  mutate(Country_2 = case_when(
    Country == "Switzerland" ~ "Switzerland",
    Country == "Europe" ~ "Europe",
    Country == "Unknown" ~ "Unknown",
    TRUE ~ "Non_Europe")) %>%
  mutate(Country_2 = factor(Country_2, 
                            levels = c("Switzerland", "Europe", "Non_Europe", "Unknown")))

## Model + Gest_age + Birth_mode + Feeding_group + Birth_weight + Co_parity + Maternal_age + Country (new)
summary(glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity 
            + Maternal_age + Country_2, data = Sample_no_missing, family = binomial))
#                                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                      -1.433e+00  9.569e+00  -0.150 0.880951    
# Hypothermia_catMild                              -2.070e-01  8.701e-01  -0.238 0.811924    
# Hypothermia_catModerate_Severe                    8.696e-01  6.801e-01   1.279 0.201053    
# Hypoglycaemia_catMild                            -1.611e+01  1.388e+03  -0.012 0.990742    
# Hypoglycaemia_catModerate                        -1.546e+01  4.294e+03  -0.004 0.997127    
# Hypoglycaemia_catSevere                           3.014e+00  7.735e-01   3.896 9.76e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb    3.941e+00  9.953e-01   3.959 7.51e-05 ***
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum  5.992e+00  1.498e+00   4.000 6.32e-05 ***
# Gest_age                                         -2.912e-02  3.423e-02  -0.851 0.394867    
# Birth_modeInstrumental_vaginal                   -1.307e-01  1.083e+00  -0.121 0.903902    
# Birth_modeC-section_pl                            6.949e-01  8.438e-01   0.823 0.410226    
# Birth_modeC-section_upl                           1.881e+00  7.609e-01   2.472 0.013453 *  
# Feeding_groupPartly_breastfed                     1.572e+00  1.704e+00   0.922 0.356352    
# Feeding_groupMixed_feeding_no_breastfed           4.871e+00  2.033e+00   2.396 0.016573 *  
# Feeding_groupFormula_only                         2.462e+00  2.584e+00   0.953 0.340615    
# Birth_weight_g                                    1.423e-04  6.233e-04   0.228 0.819448    
# Co_Parity                                        -4.452e-01  4.150e-01  -1.073 0.283369    
# Maternal_age                                      5.589e-02  6.206e-02   0.901 0.367824    
# Country_2Europe                                  -5.823e-01  6.293e-01  -0.925 0.354810    
# Country_2Non_Europe                              -1.689e+00  9.587e-01  -1.762 0.078126 .  
# Country_2Unknown                                  5.401e+00  1.502e+00   3.597 0.000322 *** 

mod9 <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity 
            + Maternal_age + Country_2, data = Sample_no_missing, family = binomial)
exp(cbind("Odds ratio" = coef(mod9), confint.default(mod9, level = 0.95)))
#                                                   Odds ratio        2.5 %       97.5 %
# (Intercept)                                      2.385786e-01 1.708677e-09 3.331218e+07
# Hypothermia_catMild                              8.129932e-01 1.477297e-01 4.474104e+00
# Hypothermia_catModerate_Severe                   2.385923e+00 6.291116e-01 9.048676e+00
# Hypoglycaemia_catMild                            1.006600e-07 0.000000e+00          Inf
# Hypoglycaemia_catModerate                        1.929268e-07 0.000000e+00          Inf
# Hypoglycaemia_catSevere                          2.036751e+01 4.472192e+00 9.275885e+01
# Hyperbilirubinaemia_catHyperbilirubinaemia_tcb   5.145819e+01 7.315819e+00 3.619480e+02
# Hyperbilirubinaemia_catHyperbilirubinaemia_serum 4.001377e+02 2.124742e+01 7.535510e+03
# Gest_age                                         9.713003e-01 9.082825e-01 1.038690e+00
# Birth_modeInstrumental_vaginal                   8.774622e-01 1.051026e-01 7.325601e+00
# Birth_modeC-section_pl                           2.003475e+00 3.832856e-01 1.047238e+01
# Birth_modeC-section_upl                          6.557586e+00 1.475907e+00 2.913594e+01
# Feeding_groupPartly_breastfed                    4.816533e+00 1.705802e-01 1.360005e+02
# Feeding_groupMixed_feeding_no_breastfed          1.304480e+02 2.426638e+00 7.012456e+03
# Feeding_groupFormula_only                        1.173329e+01 7.411572e-02 1.857503e+03
# Birth_weight_g                                   1.000142e+00 9.989212e-01 1.001365e+00
# Co_Parity                                        6.406921e-01 2.840529e-01 1.445106e+00
# Maternal_age                                     1.057482e+00 9.363660e-01 1.194263e+00
# Country_2Europe                                  5.586206e-01 1.627277e-01 1.917663e+00
# Country_2Non_Europe                              1.847116e-01 2.821131e-02 1.209386e+00
# Country_2Unknown                                 2.216010e+02 1.168031e+01 4.204255e+03

# Interpretation: In multivariable model with all variables, severe hypoglycaemia and the presence of hyperbilirubinaemia (tcB) were strongly associated with
# an increased likelihood of NICU admission. 

tab_model(mod9)


## BACKWARD SELECTION
mod_full <- glm(admission_neo_n ~ Hypothermia_cat + Hypoglycaemia_cat + Hyperbilirubinaemia_cat + Gest_age + Birth_mode + Feeding_group + Birth_weight_g + Co_Parity + 
                  Maternal_age + Country, data = Sample_no_missing, family = binomial)

backward_mod_full <- step(mod_full, direction = "backward")

# Final model
backward_mod_full_final <- backward_mod_full
summary(backward_mod_full_final)
exp(cbind("Odds ratio" = coef(backward_mod_full_final), confint.default(backward_mod_full_final, level = 0.95)))









