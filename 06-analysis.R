library(tidyverse)
library(gtsummary)
library(labelled)
library(skimr)
library(readr)
library(rio)
library(readr)
library(haven)
library(readxl)
library(janitor)
#read raw data
#Pelvic_clean_8_18_22.sav exported to pelvic.csv in SPSS
pelvic <- read_csv("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2024/Russell/Pelvic pain/Rawdata/pelvic.csv")
#import gender identity data
genderdata <- read_sas("C:/Users/ylyl/Dropbox (University of Michigan)/Data/demo_answers_21jul22.sas7bdat", 
                                 NULL)
pelvic1<- genderdata |>
  select(MRN, Gender_Identity)
#merge
pelvic1 <- left_join(pelvic, pelvic1, by="MRN")
pelvic1<- pelvic1 |> distinct()
#change labels
pelvic1 <-
  pelvic1 |> 
  select(-PATIENT_BIRTH_DATE,-PATIENT_MRN,-PATIENT_GENDER_DESC,-Patient_Race) %>% 
  mutate(PATIENT_MARITAL_STATUS_DESC =
      factor(
        PATIENT_MARITAL_STATUS_DESC,
        levels = 1:8,
        labels = c("Divorced", "Separated", "Married",
                   "Other", "Significant other", "Single", 
                   "Unknown", "Widowed")
      ),
      gender=case_when(Gender_Identity=="Female" ~ "Cis female",
                       Gender_Identity=="Choose not to disclose" ~ NA,
                       Gender_Identity %in% c("Genderqueer", "Other", "Nonbinary",
                                  "Transgender Male / Female-to-Male") ~ "Non-Cis Female",
                       TRUE ~ NA),
      Pain_status = 
      factor(
        Pain_status,
        levels = 1:4,
        labels = c("Getting better", "Getting worse", "I don't know",
                     "No difference")
   )) |>
  labelled::set_variable_labels(
    PATIENT_MARITAL_STATUS_DESC = "Marital Status",
    PATIENT_RACE_DESC = "Race",
    age = "Age",
    PATIENT_ETHNIC_GROUP_DESC = "Ethinicity",
    PATIENT_LANGUAGE_DESC = "Language",
    Hysterectomy = "Prior hysterectomy",
    Oopherectomy ="Prior oopherectomy"
  )
export(pelvic1, "pelvic1.rds")
export(genderdata, "genderdata.csv")
skimr::skim(pelvic1)
# summary
pelvic1 |>
  select(-MRN)|>
  drop_na(gender)|>
  tbl_summary(
    by = gender,
  ) |>
  add_p() |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender Identity**"
  )
#read rds
pelvic1 <- readRDS("pelvic1.rds")
#################################################################################
#07-12-2023 additional data
# MRN 101519080, 025192448 -dysphoria diagnosis
#import dysphoria data
dysph <- read_excel("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2024/Russell/Pelvic pain/Rawdata/Copy of pelvic_pain_MRN_wDysph.xlsx", sheet = "w_Dysph")
dysph1<- dysph |> select(MRN, w_Dysph_dx)
#merge two datasets
pelvic2 <- left_join(pelvic1, dysph1, by="MRN")
pelvic2<- pelvic2 |> distinct()
#label gender identity
pelvic2 <- pelvic2 |>
  mutate(gender1=case_when(w_Dysph_dx==1 & is.na(gender) ~  "Non-Cis Female",
                           TRUE ~ gender))
pelvic2|>
  filter(w_Dysph_dx==1)|>
  select(w_Dysph_dx, gender, gender1, Gender_Identity)|>
  print()
export(pelvic2, "pelvic2.rds")
#summary table
pelvic2 |>
  select(-MRN)|>
  drop_na(gender1)|>
  tbl_summary(
    by = gender1,
  ) |>
  add_p() |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender Identity**")
############################################################################  
#12/11/2023
#old CPP dataset 
#identify duplicates by "MRN"
pelvic2 <- pelvic2 |> distinct(MRN, .keep_all = TRUE)
#append tscore data
tscore <- read_csv("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2024/Russell/Pelvic pain/Rawdata/CPP_T_Score.csv")
View(tscore)
tscore <- tscore |>
  janitor::clean_names()
tscore <- tscore |>
  rename("MRN"="mrn")
pelvic3 <- left_join(pelvic2, tscore, by="MRN")
pelvic3<- pelvic3 |> distinct(MRN, .keep_all = TRUE)
#check gender identity
pelvic3|>
  filter(w_Dysph_dx==1)|>
  select(MRN, w_Dysph_dx, gender, gender1, Gender_Identity)|>
  print()
#summary
pelvic3$gender1 <- forcats::fct_relevel(pelvic3$gender1, "Non-Cis Female", "Cis female")
export(pelvic3, "pelvic3.rds")
pelvic3 |>
  select(-MRN)|>
  drop_na(gender1)|>
  tbl_summary(
    by = gender1,
  ) |>
  add_p() |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender Identity**")
#variables of interest
#race, age, Gender_Identity, Prior hysterectomy, Prior oopherectomy, CurrentNarcoiticUse, EndometriosisYN, 
#FIbromyalgiaDx, ChronicFatigueDx, InterstitialCysitisDx, IBSDx, LowBackPainDx, 
#ChronicHADx, TMJDx, DepressionDx, AnxietyDx, BipolarDx, ChildhoodSexualAbuse, Urination_Pain_Scale,
#Full_bladder_pain, Bowel_pain, Intercourse_pain, BPI_Severity, BPI_interference, pain_days_monthly
#missed_days_90days, days_in_bed_90days, Surgical_history, AgePainStarted, ChildhoodPhysicalAbuse,
#AdultPhysicalAbuse, EmotionalAbuse, w_Dysph_dx, ts_anxious, ts_depression, ts_emotion_s,
#ts_fatigue, ts_meanpurs, ts_physical, ts_satisfaction, ts_sleep
##########################################################################################
#12-12-2023
#updated CPP dataset
pain <- read_csv("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2024/Russell/Pelvic pain/Rawdata/pain.csv")
pain <- pain |> janitor::clean_names()
#create new variable "gender"
pain <-
  pain |> 
  mutate(gender=case_when(gender_identity=="Female" ~ "Cis female",
                          gender_identity=="Choose not to disclose" ~ NA,
                          gender_identity %in% c("Genderqueer", "Other", "Nonbinary",
                                                 "Transgender Male / Female-to-Male") ~ "Non-Cis Female",
                          TRUE ~ NA))
export(pain, "pain.rds") #2195 obs

#identify duplicates
multi_entry_mrn <-pain |>
  select(mrn, dos, gender, ts_anxious, ts_depression, ts_emotion_s, ts_fatigue, ts_meanpurs,
         ts_physical, ts_satisfaction, ts_sleep)|>
  get_dupes(mrn)|>
  count(mrn, name = "visit_count") # Count the number of entries per mrn
  
# Export list of mrn with multiple entries to Excel
library(writexl)
write_xlsx(multi_entry_mrn, "multi_entry_mrn.xlsx")
print(multi_entry_mrn)
#keep mrn single entry with the most recent survey (dos) 
pain1 <- pain |>
  mutate(dos=as.Date(dos))|>
  arrange(mrn, desc(`dos`)) |>
  distinct(mrn, .keep_all = TRUE)  
export(pain1, "pain1.rds") #2123 obs
#check missing 
pain1 %>%
  filter(mrn == "038318524") %>%           
  summarise(across(everything(), ~sum(is.na(.)), .names = 'missing_{.col}'))    

# summary
pain1 |>
  select(gender, age)|>
  drop_na(gender)|>
  tbl_summary(
    by = gender
  ) 

#################################################################################
#01-02-2024 additional data
#import dysphoria data
dysph_1 <- read_csv("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2024/Russell/Pelvic pain/Rawdata/cpp_mrn_wDysph.csv")
dysph_1 <- dysph_1 |> janitor::clean_names()
dysph_1<- dysph_1 |> select(mrn, dysphoria)
#merge two datasets
pain2 <- left_join(pain1, dysph_1, by="mrn")
#label gender identity
pain2 <- pain2 |>
  mutate(gender1=case_when(dysphoria==1 & is.na(gender) ~  "Non-Cis Female",
                           TRUE ~ gender))
pain2|>
  filter(dysphoria==1)|>
  select(mrn, dysphoria, gender, gender1)|>
  print()


pain2$gender1 <- forcats::fct_relevel(pain2$gender1, "Non-Cis Female", "Cis female")
pain2 <- pain2 |>
  mutate(race1=case_when(patient_race_desc=="WHITE OR CAUCASIAN" ~ "White",
                         TRUE ~ "Non-White"),
         um_amb_ob_gyn_endo_cpp_pain_severity_q30_intercourse=as.numeric(um_amb_ob_gyn_endo_cpp_pain_severity_q30_intercourse))
#abuse
pain2 <- pain2 |>
  mutate(child_physicalabuse1=case_when(um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_13_younger=="Never" ~ "No",
                                        um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_13_younger %in%  c("Often", "Seldom", "Sometimes") ~ "Yes",
                                        TRUE ~um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_13_younger),
         adult_physicalabuse1=case_when(um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_14_older=="Never" ~ "No",
                                        um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_14_older %in% c("Often", "Seldom", "Sometimes") ~ "Yes",
                                        TRUE ~ um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_14_older),
         child_sexualabuse1=case_when(um_amb_ob_gyn_endo_cpp_trauma_history_exposed_sex_organs_13_younder=="Y"|um_amb_ob_gyn_endo_cpp_trauma_history_threatened_sex_13_younger=="Y"|um_amb_ob_gyn_endo_cpp_trauma_history_touch_13_younger=="Y"|um_amb_ob_gyn_endo_cpp_trauma_history_forced_sex_13_younger=="Y"|um_amb_ob_gyn_endo_cpp_trauma_history_unwanted_sexual_experience_13_younger=="Y" ~ "Yes",
                                      um_amb_ob_gyn_endo_cpp_trauma_history_exposed_sex_organs_13_younder=="N" &
                                        um_amb_ob_gyn_endo_cpp_trauma_history_threatened_sex_13_younger=="N" & 
                                        um_amb_ob_gyn_endo_cpp_trauma_history_touch_13_younger=="N" &
                                        um_amb_ob_gyn_endo_cpp_trauma_history_forced_sex_13_younger=="N" &
                                        um_amb_ob_gyn_endo_cpp_trauma_history_unwanted_sexual_experience_13_younger=="N" ~ "No",
                                      TRUE ~ NA))
#current narcotic use
pain2 |>
  select(contains("narco"))|>
  names()
[1] "um_amb_ob_gyn_endo_cpp_q17_narcotic"                                       
[2] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication"                     
[3] "um_amb_ob_gyn_endo_cpp_treatments_how_many_narcotic_medications"           
[4] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_name_number_1"       
[5] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_1"
[6] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_helpful_number_1"    
[7] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_name_number_2"       
[8] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_2"
[9] "um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_helpful_number_2"    
[10] "um_amb_ob_gyn_endo_cpp_treatment_to_consider_narcotic_meds"
pain2<- pain2 |>
  mutate(narcotic=case_when(um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_1=="Y" |
         um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_2=="Y" ~ "Yes", 
         um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_1=="N" |
           um_amb_ob_gyn_endo_cpp_treatments_narcotic_medication_still_using_number_2=="N" ~ "No",
         TRUE ~ NA))
pain2<- pain2 |>
  mutate(narcotic1=case_when(um_amb_ob_gyn_endo_cpp_q17_narcotic %in% c("Always", "Sometimes") ~ "Yes",
                             TRUE ~ "No"))
pain2 |>
  select(narcotic1, gender3)|>
  tbl_summary(by=gender3,
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p()
#notes: current narcotic use variable "um_amb_ob_gyn_endo_cpp_q17_narcotic" or "narcotic"? 
export(pain2, "pain2.rds")
pain2 |>
  select(race1, age, um_amb_ob_gyn_endo_cpp_surgical_history_hysterectomy,um_amb_ob_gyn_endo_cpp_surgical_history_removal_of_ovary, um_amb_ob_gyn_endo_cpp_q17_narcotic, um_amb_gyn_endo_gyn_history_endometriosis, um_amb_gyn_endo_gyn_history_endometriosis_diagnosed,
         um_amb_ob_gyn_endo_cpp_conditions_fibromyalgia, um_amb_ob_gyn_endo_cpp_conditions_chronic_fatige_syndrome, um_amb_ob_gyn_endo_cpp_conditions_interstitial_cystitis, um_amb_ob_gyn_endo_cpp_conditions_irritable_bowel_syndrome, um_amb_ob_gyn_endo_cpp_conditions_low_back_pain, 
         um_amb_ob_gyn_endo_cpp_conditions_chronic_headaches, um_amb_ob_gyn_endo_cpp_conditions_tmj, um_amb_ob_gyn_endo_cpp_mental_health_depression, um_amb_ob_gyn_endo_cpp_mental_health_depression,um_amb_ob_gyn_endo_cpp_mental_health_anxiety, um_amb_ob_gyn_endo_cpp_mental_health_bipolar_disorder,  um_amb_ob_gyn_endo_cpp_pain_severity_q27_urination,um_amb_ob_gyn_endo_cpp_pain_severity_q28_full_bladder,
   um_amb_ob_gyn_endo_cpp_pain_severity_q29_bowel_movement, um_amb_ob_gyn_endo_cpp_pain_severity_q30_intercourse, um_amb_ob_gyn_endo_cpp_sq5_brief_pain_inventory_severity, um_amb_ob_gyn_endo_cpp_sq6_brief_pain_inventory_interference, um_amb_ob_gyn_endo_cpp_q10_days_per_month, 
   um_amb_ob_gyn_endo_cpp_q18_missed_days, um_amb_ob_gyn_endo_cpp_q19_days_in_bed, um_amb_ob_gyn_endo_cpp_surgical_history, um_amb_ob_gyn_endo_cpp_pain_as_child_age_started,  um_amb_ob_gyn_endo_cpp_trauma_history_victim_of_emotional_abuse,  ts_anxious, ts_depression, ts_emotion_s,
   ts_fatigue, ts_meanpurs, ts_physical, ts_satisfaction, ts_sleep,child_physicalabuse1,
   child_sexualabuse1, adult_physicalabuse1, narcotic1,gender1)|>
  drop_na(gender1)|>
  tbl_summary(
    by = gender1,
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender Identity**")

#sum
pain2 |>
  select(gender1, um_amb_ob_gyn_endo_cpp_trauma_history_victim_of_emotional_abuse,                  
um_amb_ob_gyn_endo_cpp_trauma_history_exposed_sex_organs_13_younder,              
um_amb_ob_gyn_endo_cpp_trauma_history_exposed_sex_organs_14_older,                
um_amb_ob_gyn_endo_cpp_trauma_history_threatened_sex_13_younger,                  
um_amb_ob_gyn_endo_cpp_trauma_history_threatened_sex_14_older,                    
um_amb_ob_gyn_endo_cpp_trauma_history_touch_13_younger,                           
um_amb_ob_gyn_endo_cpp_trauma_history_touch_14_older,                            
um_amb_ob_gyn_endo_cpp_trauma_history_forced_sex_13_younger,                      
um_amb_ob_gyn_endo_cpp_trauma_history_forced_sex_14_older,                        
um_amb_ob_gyn_endo_cpp_trauma_history_unwanted_sexual_experience_13_younger,      
um_amb_ob_gyn_endo_cpp_trauma_history_unwanted_sexual_experience_14_older,       
um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_13_younger,               
um_amb_ob_gyn_endo_cpp_trauma_history_threaten_your_life_13_younger,              
um_amb_ob_gyn_endo_cpp_trauma_history_hit_kick_or_beat_14_older,                 
um_amb_ob_gyn_endo_cpp_trauma_history_threaten_your_life_14_older, child_physicalabuse1,
child_sexualabuse1, adult_physicalabuse1)|>
  drop_na(gender1)|>
  tbl_summary(
    by = gender1,
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender Identity**")




  
############################################################################  
#sensitivity analysis
pain2 <- pain2 |>
  mutate(gender2=case_when(is.na(gender1) ~ "Not identified",
                           gender1=="Cis female" ~ "Self-identified",
                           TRUE ~ gender1),
         gender3=case_when(is.na(gender1) ~ "Cis female",
                           TRUE ~ gender1)) #impute 1699 missing gender identity "Cis female"
pain2$gender3 <- forcats::fct_relevel(pain2$gender3, "Non-Cis Female", "Cis female")
pain2 |>
#  filter(!gender2=="Non-Cis Female")|>
  select(race1, age, um_amb_ob_gyn_endo_cpp_surgical_history_hysterectomy,um_amb_ob_gyn_endo_cpp_surgical_history_removal_of_ovary, um_amb_ob_gyn_endo_cpp_q17_narcotic, um_amb_gyn_endo_gyn_history_endometriosis, um_amb_gyn_endo_gyn_history_endometriosis_diagnosed,
         um_amb_ob_gyn_endo_cpp_conditions_fibromyalgia, um_amb_ob_gyn_endo_cpp_conditions_chronic_fatige_syndrome, um_amb_ob_gyn_endo_cpp_conditions_interstitial_cystitis, um_amb_ob_gyn_endo_cpp_conditions_irritable_bowel_syndrome, um_amb_ob_gyn_endo_cpp_conditions_low_back_pain, 
         um_amb_ob_gyn_endo_cpp_conditions_chronic_headaches, um_amb_ob_gyn_endo_cpp_conditions_tmj, um_amb_ob_gyn_endo_cpp_mental_health_depression, um_amb_ob_gyn_endo_cpp_mental_health_depression,um_amb_ob_gyn_endo_cpp_mental_health_anxiety, um_amb_ob_gyn_endo_cpp_mental_health_bipolar_disorder,  um_amb_ob_gyn_endo_cpp_pain_severity_q27_urination,um_amb_ob_gyn_endo_cpp_pain_severity_q28_full_bladder,
         um_amb_ob_gyn_endo_cpp_pain_severity_q29_bowel_movement, um_amb_ob_gyn_endo_cpp_pain_severity_q30_intercourse, um_amb_ob_gyn_endo_cpp_sq5_brief_pain_inventory_severity, um_amb_ob_gyn_endo_cpp_sq6_brief_pain_inventory_interference, um_amb_ob_gyn_endo_cpp_q10_days_per_month, 
         um_amb_ob_gyn_endo_cpp_q18_missed_days, um_amb_ob_gyn_endo_cpp_q19_days_in_bed, um_amb_ob_gyn_endo_cpp_surgical_history, um_amb_ob_gyn_endo_cpp_pain_as_child_age_started,  um_amb_ob_gyn_endo_cpp_trauma_history_victim_of_emotional_abuse,  ts_anxious, ts_depression, ts_emotion_s,
         ts_fatigue, ts_meanpurs, ts_physical, ts_satisfaction, ts_sleep,child_physicalabuse1,
         child_sexualabuse1, adult_physicalabuse1, gender3)|>
  tbl_summary(
    by = gender3,
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  bold_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Gender identity**")
  









