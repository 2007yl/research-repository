#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load libraries
library(tidyverse)
library(gtsummary)
library(labelled)
library(haven)
library(lubridate)
library(forcats)
library(rio)
library(skimr)
#Read Data
data<- read.csv("C:/Users/ylyl/OneDrive - Michigan Medicine/ResidentsResearch/Residents/2025/Darington Richardson/Racial disparity/Raw data/t1184_liu_04_28_23.csv")

glimpse(data)       
###Analysis ###
#calculate by year
s_all <-
  data|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  )  
s2015 <-
  data|>
  filter(surgery_year==2015)|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() |>
  mutate(surgeon2015=case_when(frequency<6 ~"low",
                               frequency>=6 ~"high"))  
s2016 <-
  data|>
  filter(surgery_year==2016)|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() |>
  mutate(surgeon2016=case_when(frequency<12 ~"low",
                           frequency>=12 ~"high"))

s2017 <-
  data|>
  filter(surgery_year==2017)|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() |>
  mutate(surgeon2017=case_when(frequency<12 ~"low",
                               frequency>=12 ~"high"))

s2018 <-
  data|>
  filter(surgery_year==2018)|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() |>
  mutate(surgeon2018=case_when(frequency<12 ~"low",
                               frequency>=12 ~"high"))
test0 <-
  s2015 |>
  select(physician_cid, surgeon2015)|>
  full_join(s_all, s2015,by=join_by(physician_cid), relationship="many-to-many")

test <-
  s2016 |>
  select(physician_cid, surgeon2016)|>
  full_join(test0, s2016,by=join_by(physician_cid), relationship="many-to-many")

test1 <-
  s2017 |>
  select(physician_cid, surgeon2017)|>
  full_join(test, s2017,  by=join_by(physician_cid), relationship="many-to-many")

test2 <-
  s2018 |>
  select(physician_cid, surgeon2018)|>
  full_join(test1, s2018,  by=join_by(physician_cid), relationship="many-to-many")

data0 <-
  full_join(data, test2,  by=join_by(physician_cid), relationship="many-to-many")

data0<-
  data0 |>
  mutate(volume=case_when(c(surgery_year==2015 & surgeon2015=="low")~"low",
                          c(surgery_year==2015 & surgeon2015=="high")~"high",
                          c(surgery_year==2016 & surgeon2016=="low")~"low",
                          c(surgery_year==2016 & surgeon2016=="high")~"high",
                          c(surgery_year==2017 & surgeon2017=="low")~"low",
                          c(surgery_year==2017 & surgeon2017=="high")~"high",
                          c(surgery_year==2018 & surgeon2018=="low")~"low",
                          c(surgery_year==2018 & surgeon2018=="high")~"high"))

#Clean up: exclude cancer patients;
#include White and Black patients;
#calculate BMI; surgeon volume; surgery time; time from surgery to discharge
data1 <-
  data0 |>
  filter(gyn_cancer ==0)|>
  mutate(race1=case_when(race %in% c("{0|10}", "{10|70}", " {70|10}") ~ "{10}",
                        race %in% c("{0|20}", "{20|70}", " {70|20}") ~ "{20}",
                        TRUE ~ as.character(race)))|>
  filter(race1 %in% c("{10}","{20}"))|>
  mutate(race1=recode(race1, "{10}" ="White", "{20}" = "Black"))|>
  mutate(height_cm=case_when(height_unit==1 ~ height_no*2.54,
                          height_unit==2 ~ height_no))|>
  mutate(weight_kg=case_when(weight_unit==1 ~ weight_no/2.205,
                          weight_unit==2 ~ weight_no))|>
  mutate(height_m=height_cm/100)|>
  mutate(bmi=weight_kg/height_m ^ 2)|>
  mutate(bmigroup = case_when(bmi < 18.5 ~ "1-Underweight",
                              bmi < 25 ~ "2-Normal weight",
                              bmi < 30 ~ "3-Overweight",
                              bmi >= 30 ~ "4-Obese"))|>
  mutate(asaclass = case_when(asa_class_id %in% c(1,2) ~ "ASA class <3",
                              asa_class_id %in% c(3,4,5) ~ "ASA class >=3")) |>
  mutate(functional = case_when(functional_status %in% c(2,3) ~ "No",
                                       functional_status ==1 ~ "Yes"))|>                          
  mutate(insurance = case_when(e_insurance_type %in% c(1, 2) ~ "Commercial insurance",
                                      e_insurance_type %in% c(3, 999) ~ "Public insurance",
                                      e_insurance_type %in% c(4:5, 998) ~ "Uninsured/other"
                                      ))|>
  mutate(cmplt_image=case_when( flg_w_h_cmplt_ultra == 1 | flg_w_h_cmplt_ct == 1
                                | flg_w_h_cmplt_mri == 1 ~1, 
                                flg_w_h_cmplt_ultra == 0 | flg_w_h_cmplt_ct == 0
                                | flg_w_h_cmplt_mri == 0 ~ 0))|>
  mutate(skin=case_when(skin_antisepsis %in% c(0,90,96) ~ "",
                        skin_antisepsis %in% c(1,4) ~ "Povidone-iodine",
                        skin_antisepsis %in% c(2,3) ~ "Chlorhexidine",
                        skin_antisepsis %in% c(5,6,99) ~ "Other",
                        TRUE ~ as.character(skin_antisepsis)))|>
  mutate(mis_open=case_when(surgical_approach %in% c(28,30,32,34,36,39,46,48,50) ~ 1,
                            TRUE ~ 0))|>
  mutate(approach=case_when(surgical_approach %in% c(25,26,28,30,32,34,36,39,46,
                                                     48,50) ~ "Open",
                            surgical_approach %in% c(27,29,31,37,47) ~ "Laparoscopic",
                            surgical_approach %in% c(33,35,38,49) ~ "Robotic",
                            surgical_approach ==45 ~ "Vaginal")) |>
  mutate(surgery_duration = case_when(
     surgery_duration %in% c("92 days 01:42:00", "92 days 01:32:00",
                             "62 days 02:00:00", "62 days 01:25:00",
                             "61 days 01:50:00", "365 days 02:17:00",
                             "365 days 01:40:00", "365 days 01:38:00",
                             "-13149 days +03:44:15", "31 days 01:28:00",
                             "3 days 01:40:00", "1 days 04:58:12", "1 days 04:05:00",
                             "1 days 01:48:01", "1 days 01:38:00",
                             "1 days 01:31:15", "1 days 01:26:10",
                             "1 days 01:22:06", "1 days 00:49:00") ~ "NA",
                           TRUE ~ as.character(surgery_duration))) |>
  mutate(surgeryt=str_sub(surgery_duration,8,15))|>
  mutate(surgery_duration1 = hms(surgeryt)) |> 
  mutate(surgery_duration2 = surgery_duration1 + days(str_sub(surgery_duration, 1, 1))) |>
  mutate(surgery_time = as.numeric(surgery_duration2, "hours"))|>  
  mutate(surgery_time = case_when(surgery_time < 0.5 ~ "NA",
                                  TRUE ~  as.character(surgery_time))) |>
  mutate(incision_to_discharge = case_when(
    incision_to_discharge %in% c("95 days 03:36:00", "94 days 00:48:00",
                                 "92 days 04:59:00") ~ "NA",TRUE ~ as.character(incision_to_discharge))) |>
  mutate(inci=str_sub(incision_to_discharge,8,15))|>
  mutate(inci1 = hms(inci)) |> 
  mutate(inci2 = inci1 + days(str_sub(incision_to_discharge, 1, 1))) |>
  mutate(incision_time = as.numeric(inci2, "hours")) |>
  mutate(incision_time = case_when(incision_time < 2.0 ~ "NA",
                                   TRUE ~  as.character(incision_time))) |>
  mutate_at(vars("surgery_time", "incision_time"), as.numeric)|>
  mutate(sameday=case_when(incision_time <=24 ~ 1,
                           incision_time >24 ~0,
                           TRUE ~ incision_time))|>
  mutate(sameday=factor(sameday))
#notes:
# surgery_time NA 131(17 cases > 1 day + 114 cases < 30 min)(0.65%); 
# incision_time NA 288 (248cases unknown + 40 cases < 2 hours)(1.4%)

#clean up postop complications and events
data1<-
  data1|> 
  mutate_at(vars("category_5", "category_6", "category_7"), as.numeric)|>
  mutate(cmp_pe=case_when(category_1 == 7 | category_2 == 7 | category_3 == 7
                           |category_4 == 7  |category_5 == 7|category_6 == 7
                           |category_7 == 7~1, TRUE ~ 0))|>
  mutate(cmp_uti=case_when(category_1 == 10 | category_2 == 10 | category_3 == 10
                          |category_4 == 10  |category_5 == 10|category_6 == 10
                          |category_7 == 10~1, TRUE ~ 0))|>
  mutate(cmp_stroke=case_when(category_1 == 11 | category_2 == 11 | category_3 == 11
                           |category_4 == 11  |category_5 == 11|category_6 == 11
                           |category_7 == 11~1, TRUE ~ 0))|>
  mutate(intraop_cmp_cardiac=case_when(category_1 == 12 | category_2 == 12 | category_3 == 12
                              |category_4 == 12  |category_5 == 12|category_6 == 12
                              |category_7 == 12~1, TRUE ~ 0))|>
  mutate(cmp_cardiac=case_when(category_1 == 13 | category_2 == 13 | category_3 == 13
                                       |category_4 == 13  |category_5 == 13|category_6 == 13
                                       |category_7 == 13~1, TRUE ~ 0))|>
  mutate(intraop_cmp_myo=case_when(category_1 == 14 | category_2 == 14 | category_3 == 14
                                       |category_4 == 14  |category_5 == 14|category_6 == 14
                                       |category_7 == 14~1, TRUE ~ 0))|>
  mutate(cmp_myo=case_when(category_1 == 15 | category_2 == 15 | category_3 == 15
                                   |category_4 == 15  |category_5 == 15|category_6 == 15
                                   |category_7 == 15~1, TRUE ~ 0))|>
  mutate(cmp_dysr=case_when(category_1 == 16 | category_2 == 16 | category_3 == 16
                           |category_4 == 16  |category_5 == 16|category_6 == 16
                           |category_7 == 16~1, TRUE ~ 0))|>
  mutate(cmp_trans=case_when(category_1 == 17 | category_2 == 17 | category_3 == 17
                            |category_4 == 17  |category_5 == 17|category_6 == 17
                            |category_7 == 17~1, TRUE ~ 0))|>
  mutate(cmp_dvt=case_when(category_1 == 18 | category_2 == 18 | category_3 == 18
                             |category_4 == 18  |category_5 == 18|category_6 == 18
                             |category_7 == 18~1, TRUE ~ 0))|>
  mutate(cmp_sep=case_when(category_1 == 19 | category_2 == 19 | category_3 == 19
                           |category_4 == 19  |category_5 == 19|category_6 == 19
                           |category_7 == 19~1, TRUE ~ 0))|>
  mutate(cmp_ssep=case_when(category_1 == 20 | category_2 == 20 | category_3 == 20
                           |category_4 == 20  |category_5 == 20|category_6 == 20
                           |category_7 == 20~1, TRUE ~ 0))|>
  mutate(cmp_cdiff=case_when(category_1 == 21 | category_2 == 21 | category_3 == 21
                            |category_4 == 21  |category_5 == 21|category_6 == 21
                            |category_7 == 21~1, TRUE ~ 0))|>
  mutate(cmp_clabsi=case_when(category_1 == 22 | category_2 == 22 | category_3 == 22
                             |category_4 == 22  |category_5 == 22|category_6 == 22
                             |category_7 == 22~1, TRUE ~ 0))|>
  mutate(cmp_bleeding=case_when(category_1 == 23 | category_2 == 23 | category_3 == 23
                              |category_4 == 23  |category_5 == 23|category_6 == 23
                              |category_7 == 23~1, TRUE ~ 0))|>
  mutate(cmp_ileus=case_when(category_1 == 24 | category_2 == 24 | category_3 == 24
                                |category_4 == 24  |category_5 == 24|category_6 == 24
                                |category_7 == 24~1, TRUE ~ 0))|>
  mutate(cmp_bowel=case_when(category_1 == 25 | category_2 == 25 | category_3 == 25
                             |category_4 == 25  |category_5 == 25|category_6 == 25
                             |category_7 == 25~1, TRUE ~ 0))|>
  mutate(cmp_uret=case_when(category_1 == 26 | category_2 == 26 | category_3 == 26
                             |category_4 == 26  |category_5 == 26|category_6 == 26
                             |category_7 == 26~1, TRUE ~ 0))|>
  mutate(cmp_bladder=case_when(category_1 == 27 | category_2 == 27 | category_3 == 27
                            |category_4 == 27  |category_5 == 27|category_6 == 27
                            |category_7 == 27~1, TRUE ~ 0))|>
  mutate(cmp_fistula=case_when(category_1 == 28 | category_2 == 28 | category_3 == 28
                               |category_4 == 28  |category_5 == 28|category_6 == 28
                               |category_7 == 28~1, TRUE ~ 0))|>
  mutate(cmp_vcd=case_when(category_1 == 29 | category_2 == 29 | category_3 == 29
                               |category_4 == 29  |category_5 == 29|category_6 == 29
                               |category_7 == 29~1, TRUE ~ 0))|>
  mutate(cmp_vcc=case_when(category_1 == 30 | category_2 == 30 | category_3 == 30
                               |category_4 == 30  |category_5 == 30|category_6 == 30
                               |category_7 == 30~1, TRUE ~ 0))|>
  mutate(cmp_leak=case_when(category_1 == 31 | category_2 == 31 | category_3 == 31
                           |category_4 == 31  |category_5 == 31|category_6 == 31
                           |category_7 == 31~1, TRUE ~ 0))|>
  mutate(cmp_cuff=case_when(category_1 == 32 | category_2 == 32 | category_3 == 32
                           |category_4 == 32  |category_5 == 32|category_6 == 32
                           |category_7 == 32~1, TRUE ~ 0))|>
  mutate(cmp_abs=case_when(category_1 == 33 | category_2 == 33 | category_3 == 33
                           |category_4 == 33  |category_5 == 33|category_6 == 33
                           |category_7 == 33~1, TRUE ~ 0))|>
  mutate(cmp_gastro=case_when(category_1 == 34 | category_2 == 34 | category_3 == 34
                           |category_4 == 34  |category_5 == 34|category_6 == 34
                           |category_7 == 34~1, TRUE ~ 0))|>
  mutate(cmp_upper=case_when(category_1 == 35 | category_2 == 35 | category_3 == 35
                           |category_4 == 35  |category_5 == 35|category_6 == 35
                           |category_7 == 35~1, TRUE ~ 0))|>
  mutate(cmp_lower=case_when(category_1 == 36 | category_2 == 36 | category_3 == 36
                           |category_4 == 36  |category_5 == 36|category_6 == 36
                           |category_7 == 36~1, TRUE ~ 0))|>
  mutate(cmp_other=case_when(category_1 == 37 | category_2 == 37 | category_3 == 37
                             |category_4 == 37  |category_5 == 37|category_6 == 37
                             |category_7 == 37~1, TRUE ~ 0))|>
  mutate(cmp_ss=case_when(category_1 == 38 | category_2 == 38 | category_3 == 38
                             |category_4 == 38  |category_5 == 38|category_6 == 38
                             |category_7 == 38~1, TRUE ~ 0))|>
  mutate(cmp_ng=case_when(category_1 == 39 | category_2 == 39 | category_3 == 39
                             |category_4 == 39  |category_5 == 39|category_6 == 39
                             |category_7 == 39~1, TRUE ~ 0))|>
  mutate(cmp_pn=case_when(category_1 == 4 | category_2 == 4 | category_3 == 4
                          |category_4 == 4  |category_5 == 4|category_6 == 4
                          |category_7 == 4~1, TRUE ~ 0))|>
  mutate(cmp_ur=case_when(category_1 == 40 | category_2 == 40 | category_3 == 40
                          |category_4 == 40  |category_5 == 40|category_6 == 40
                          |category_7 == 40~1, TRUE ~ 0))|>
  mutate(intraop_cmp_intu=case_when(category_1 == 5 | category_2 == 5 | category_3 == 5
                          |category_4 == 5  |category_5 == 5|category_6 == 5
                          |category_7 == 5~1, TRUE ~ 0))|>
  mutate(cmp_intu=case_when(category_1 == 6 | category_2 == 6 | category_3 == 6
                          |category_4 == 6  |category_5 == 6|category_6 == 6
                          |category_7 == 6~1, TRUE ~ 0))|>
  mutate(cmp_aki=case_when(category_1 == 8 | category_2 == 8 | category_3 == 8
                          |category_4 == 8  |category_5 == 8|category_6 == 8
                          |category_7 == 8~1, TRUE ~ 0))|>
  mutate(cmp_utinc=case_when(category_1 == 9 | category_2 == 9 | category_3 == 9
                           |category_4 == 9  |category_5 == 9|category_6 == 9
                           |category_7 == 9~1, TRUE ~ 0))|>
  mutate(planned_reoperation=case_when(planned_reoperation_1 == 1 | planned_reoperation_1 == 1 
                                       | planned_reoperation_1 == 1 |planned_reoperation_1 == 1 ~1, TRUE ~ 0))|>
  mutate(planned_readmission=case_when(planned_readmission_1 == 1 | planned_readmission_1 == 1 ~1, TRUE ~ 0))|>
  mutate(ed=case_when(e_event_subtype_1 == 1 | e_event_subtype_2 == 1 | e_event_subtype_3 == 1
                             |e_event_subtype_4 == 1  |e_event_subtype_5 == 1|e_event_subtype_6 == 1
                             |e_event_subtype_7 == 1~1, TRUE ~ 0))|>
  mutate(urgentcare=case_when(e_event_subtype_1 == 2 | e_event_subtype_2 == 2 | e_event_subtype_3 == 2
                      |e_event_subtype_4 == 2  |e_event_subtype_5 == 2|e_event_subtype_6 == 2
                      |e_event_subtype_7 == 2~1, TRUE ~ 0))|>
  mutate(event_edorur=case_when(e_eventtype_1 == 4 | e_eventtype_2 == 4 | e_eventtype_3 == 4
                              |e_eventtype_4 == 4  |e_eventtype_5 == 4|e_eventtype_6 == 4
                              |e_eventtype_7 == 4 |e_eventtype_8 == 4 |e_eventtype_9 == 4~1, TRUE ~ 0))|>
  mutate(event_readmit=case_when(e_eventtype_1 == 5 | e_eventtype_2 == 5 | e_eventtype_3 == 5
                                |e_eventtype_4 == 5  |e_eventtype_5 == 5|e_eventtype_6 == 5
                                |e_eventtype_7 == 5 |e_eventtype_8 == 5 |e_eventtype_9 == 5~1, TRUE ~ 0))|>
  mutate(event_return=case_when(e_eventtype_1 == 6 | e_eventtype_2 == 6 | e_eventtype_3 == 6
                                 |e_eventtype_4 == 6  |e_eventtype_5 == 6|e_eventtype_6 == 6
                                 |e_eventtype_7 == 6 |e_eventtype_8 == 6 |e_eventtype_9 == 6~1, TRUE ~ 0))|>
  mutate(cmp_ssi=case_when(c(flg_cmp_deep_ssi==1 | flg_cmp_organ_space_ssi==1) ~1,
                           TRUE ~ 0))|>
  mutate(cmp_cardivas=case_when(c(cmp_stroke==1 | cmp_myo==1 | cmp_cardiac==1) ~1,
                               TRUE ~0)) |>
  mutate(cmp_vte=case_when(c(cmp_dvt==1 | cmp_pe==1) ~1,
                           TRUE ~0)) |>
  mutate(cmp_uti2=case_when(c(cmp_uti==1 | cmp_utinc==1) ~1,
                            TRUE ~0))
  
#labels

data1 <-
  data1 |>
  mutate(ethnicity_hispanic =
           factor(
             ethnicity_hispanic,
             levels = 0:1,
             labels = c("No Hispanic", "Hispanic")
           ),
         hb_mis =
           factor(
             hb_mis,
             levels = 0:1,
             labels = c("No", "Yes")
           ),
  #        asa_class_id=
  #         factor(
  #          asa_class_id,
  #           levels = c(1:5,7),
  #           labels = c("ASA 1", "ASA 2", "ASA 3", "ASA 4", "ASA 5", "None assigned")),
  #       functional_status=
  #         factor(
 #            functional_status,
 #            levels = 0:4,
 #            labels = c("Not Selected", "Independent","Not Independent", 
 #                       "Totally dependent" , "Unknown")
 #          ),
         ultrsnd_uterine_size=
           factor(
             ultrsnd_uterine_size,
             levels = 1:5,
             labels = c( "Size in dimensions", "Size in gestational weeks",
                         "No uterine size reported", "Size in mass", "Other description reported")
          ),
         e_teaching=
           factor(
             e_teaching,
             levels = 1:2,
             labels = c("Teaching Hospital", "Non-teaching Hospital")
          ),
         e_beds=
           factor(
             e_beds,
             levels = 1:3,
             labels = c("Less than 300 beds", "300 - 499 beds", "500 or more beds")),
         
 #        e_insurance_type=
 #          factor(
 #            e_insurance_type,
 #            levels = c(1:5,998:999),
 #            labels = c( "Commerical Insurance (non-HMO)", "Health Maintenance Organization",
 #                        "Government Insurance", "Uninsured/Self Pay without Insurance",
 #                        "Self Pay with Insurance", "Other", "Medicaid Patient")),
         e_w_h=
           factor(
             e_w_h,
             levels = 1:5,
             labels = c("No workup mentioned or documented by surgeon", "Workup not available, completed at outside facility",
                        "Workup not available, no access to surgeon's office notes",
                        "Workup completed", "Workup available")),
 #        skin_antisepsis=
 #          factor(
 #            skin_antisepsis,
 #            levels = c(0:6,90,96,99),
 #            labels = c( "Not Selected", "Povidone-iodine", "Chlorhexidine - alone",
#                         "Chlorhexidine with alcohol", "Iodine with alcohol stick prep",
 #                        "Pcmx", "Isopropyl Alcohol", "Not Applicable", 
 #                        "Info Missing/Not Available", "Other")),
         e_fallopian_removal=
           factor(
             e_fallopian_removal,
             levels = 1:2,
             labels = c("One fallopian removed", "Both removed")),
         e_ovary_removal=
           factor(
             e_ovary_removal,
             levels = 1:2,
             labels = c("One ovary removed", "Both removed")),
  #       sameday=
  #        factor(
  #           sameday,
  #           levels=0:1,
  #           labels = c("No", "Yes")),
         mech_periop_vte_prophylaxis=
           factor(
             mech_periop_vte_prophylaxis,
             levels=0:1,
             labels = c("No", "Yes")),
         pharm_periop_vte_prophylaxis=
          factor(
             pharm_periop_vte_prophylaxis,
             levels=0:1,
             labels = c("No", "Yes"))) |>
#        surgical_approach=
#          factor(
#            surgical_approach,
#            levels=c(25:39, 45:50),
#            labels=c("Open", "Open, lap-assisted","Laparoscopic","Laparoscopic, converted to Open",
#                      "Laparoscopic, hand-assisted (does not include laparoscopic hand-assisted vaginal hysterectomy)",
 #                     "Laparoscopic, hand-assisted, converted to Open","Laparoscopic, Single Port (SIL)",
#                      "Laparoscopic, Single Port (SIL), converted to Open","Robotic Single Port (SIL)",
#                     "Robotic Single Port (SIL), converted to Open","Robotic",
#                        "Robotic, converted to Open", "Robotic, converted to Laparoscopic",        
#                      "Robotic, hand-assisted", "Robotic hand-assisted, converted to Open",
#                      "Vaginal", "Vaginal, converted to Open", "Laparoscopic-assisted Vaginal Hysterectomy (LAVH)",
#                     "Laparoscopic-assisted Vaginal Hysterectomy (LAVH), converted to Open",
#                     "Robotic-assisted Vaginal Hysterectomy (RAVH)",
#                     "Robotic-assisted Vaginal Hysterectomy (RAVH), converted to Open")))|>
      labelled::set_variable_labels(
           ethnicity_hispanic ="Ethnicity",
           race1="Race",
           calculated_age="Age",
           bmi="BMI",
           bmigroup="BMI",
           asa_class_id="ASA class", 
           asaclass="ASA class", 
           flg_cmb_sleep_apnea="Comorbidity-Obstructive sleep apnea",
           flg_cmb_diabetes="Comorbidity-Diabetes",
           flg_cmb_hypertension="Comorbidity-Hypertension",
           flg_cmb_smoker="Comorbidity-Tabacco use",
           flg_cmb_dvt="Comorbidity-History of DVT",
           functional_status="Dependent functional status",
           functional="Independent functional status",
           insurance="Insurance type",
           flg_cmb_coronary_artery="Comorbidity-Coronary artery disease",
           indication_uterine_bleeding="Surgical indication-Abnormal uterine bleeding",
           indication_uterine_fibroids="Surgical indication-Fibroids",
           indication_endometriosis="Surgical indication-Endometriosis",
           indication_chronic_pelvic_pain="Surgical indication-Pelvic pain",
           indication_cervical_dysplasia="Surgical indication-Cervical dysplasia",
           indication_pelvic_organ_prolapse="Surgical indication-Prolapse",
           ultrsnd_uterine_size="Preop uterine size",
           e_teaching="Teaching Hospital",
           e_beds="Hospital size",
           e_insurance_type="Insurance type",
           flg_w_h_cmplt_pap="Pap documented",
           flg_w_h_cmplt_biopsy="Biopsy documented",
           flg_w_h_cmplt_ultra="Ultrasound documented",
           flg_w_h_cmplt_ct="CT documented",
           flg_w_h_cmplt_mri="MRI documented",
           hct_no="Preop hematocrit value",
           e_w_h="Surgical approach conversation",
           flg_hyster_alt_embolization="Alternative Treatment (Uterine artery embolization)",
           flg_hyster_alt_laparoscopy=	"Alternative Treatment (Laparoscopy)",
           flg_hyster_alt_myomectomy=	"Alternative Treatment (Myomectomy)",
           flg_hyster_alt_physical=	"Alternative Treatment (Physical Therapy)",
           flg_hyster_alt_treatment=	"Alternative Treatment (Benign)",
           flg_hyster_alt_hormone=	"Alternative Hormonal",
           flg_hyster_alt_nonhormone=	"Alternative Non-Hormonal",
           flg_hyster_alt_pain=	"Alternative Pain Management",
           flg_hyster_alt_iud=	"Alternative Progesterone IUD",
           flg_hyster_alt_ablation=	"Alternative Endometrial Ablation",
           flg_hyster_alt_scopy=	"Alternative Hysteroscopic/D&C",
           flg_hyster_alt_pessary=	"Alternative Vaginal Pessary",
           skin_antisepsis="Appropriate abdominal prep",
           skin="Appropriate abdominal prep",
           mech_periop_vte_prophylaxis="Perioperative mechanical DVT prophylaxis",
           pharm_periop_vte_prophylaxis="Perioperative pharmacological DVT prophylaxis",
           hb_mis="MIS approach",
           surgery_time="Operative time (hours)",
     #      admit_time="Admission to discharge (hours)",
           incision_time="Operation to dischage (hours)",
           sameday="Same day discharge",
           bleeding_barrier="Bleeding barrier",
           e_fallopian_removal="Bilateral salpingectomy",
           e_ovary_removal= "Surgical menopause",
           fluid_out_ebl_total=	"Estimated Blood Loss (EBL) in ML",
           intraop_complication_bladder_thickness="Intraoperative Complication - Bladder injury - full thickness",
           intraop_complication_bowel_thickness="Intraoperative Complication - Bowel injury - full thickness",
           intraop_warming="Intraop warming",
           flg_intraop_multimodal_pain_med="Intraop multimodal pain management",
           flg_cmp_ssi_any="Infectious complication-any ssi",
           flg_cmp_deep_ssi="Infectious complication-Deep incision ssi",
           flg_cmp_organ_space_ssi="Infectious  complication-Organ/space ssi",
           flg_cmp_superficial_ssi="Infectious complication-superficial ssi",
           cmp_ssi="Infectious complication-Deep incision/Organ ssi",
           event_edorur="Presentation to ED or Urgent Care",
           event_readmit="Readmission",
           event_return="Return to Operating Room",
           ed="ED",
           urgentcare="Urgent care",
           planned_reoperation="Return to Operating Room Planned at Principal Procedure",
           planned_readmission="Readmission Planned at Principal Procedure",
           cmp_uti="Urinary Tract Infection (UTI) - CAUTI",
           cmp_stroke= "Stroke/CVA",
           cmp_cardivas="Major cardivascular complications",
           intraop_cmp_cardiac= "Cardiac Arrest req. CPR(Intraop)",
           cmp_cardiac= "Cardiac Arrest req. CPR",
           intraop_cmp_myo="Myocardial Infarction (Intraop)",
           cmp_myo="Myocardial Infarction",
           cmp_dysr="Cardiac Dysrhythmias",
           cmp_trans="Transfusions w/in first 72 hrs",
           cmp_dvt="Deep Vein Thrombosis req. Therapy",
           cmp_vte="Venous thromboembolism",
           cmp_uti2="Urinary Tract Infection",
           cmp_sep="Sepsis",
           cmp_ssep="Severe Sepsis/Septic Shock",
           cmp_cdiff="C-difficile",
           cmp_clabsi="CLABSI",
           cmp_bleeding="Bleeding Complications",
cmp_ileus=	"Ileus/Small Bowel Obstruction",
cmp_bowel="Bowel Injury",
cmp_uret	="Ureteral Obstruction",
cmp_bladder="Bladder Injury",
cmp_fistula	="Fistula",
cmp_vcd	="Vaginal Cuff Dehiscence",
cmp_vcc	="Vaginal Cuff Cellulitis",
cmp_leak	="Anastomotic Leak",
cmp_cuff="Cuff infection",
cmp_abs	="Pelvic abscess",
cmp_gastro="Gastrointestinal - Anastomotic leak",
cmp_upper	="Nerve Injury - Upper Extremity",
cmp_lower	="Nerve Injury - Lower Extremity",
cmp_other	="Other",
cmp_ss="Septic Shock",
cmp_ng	="Postop Ileus requiring NG tube or NPO",
cmp_pn="Pneumonia",
cmp_ur="Postop Urinary Retention",
intraop_cmp_intu="Unplanned Intubation(Intraop)",
cmp_intu= "Unplanned Intubation",
cmp_pe="Pulmonary Embolism",
cmp_aki="Acute Kidney Injury",
cmp_utinc="Urinary Tract Infection (UTI) - Non-CAUTI",
surgical_approach="Surgical approach",
approach="Surgical approach",
mis_open="MIS converted to open",
volume="Surgeon volume",
cmplt_image="Imaging documented")

# antibiotics
test_a<-
  data1|>
  mutate(id = row_number())|>
  select(id, antibiotic_name_id_1, antibiotic_name_id_2, antibiotic_name_id_3,
         antibiotic_name_id_4, antibiotic_name_id_5)
#test_a[is.na(test_a)] <- ""
# pivot longer to gather _1, _2, _3, _4, _5 into a single column
df_long <- 
  test_a %>%
  pivot_longer(cols = starts_with("antibiotic_name_id_"),
               names_to = "antibiotic_name_id_col", 
               values_to = "antibiotic_name_id_val") 
#select(-c_col) # drop the original column names

# group by id and concatenate values into a space-separated string
df_new <- df_long %>%
  group_by(id) %>%
  summarize(c = paste(antibiotic_name_id_val, collapse = " ")) %>%
  ungroup() # remove the grouping

df_new <-
  df_new |>
  mutate(prefer=case_when(c  %in% c("NA 18 NA NA NA", "14 2 2 NA NA", "14 2 NA NA NA", 
                                    "14 18 NA NA NA","14 18 14 NA NA",  "14 18 18 NA NA",
                                    "14 5 5 NA NA", "14 5 NA NA NA", "18 14 18 NA NA",
                                    "18 14 NA NA NA", "18 18 14 NA NA", "18 NA NA NA NA",
                                    "18 18 18 NA NA",
                                    "18 18 NA NA NA", "2 14 2 NA NA", "2 14 NA NA NA",
                                    "2 2 14 NA NA", "2 2 2 2 NA", "2 2 2 NA NA", "2 2 22 NA NA",
                                    "2 2 NA NA NA", "2 22 2 NA NA", "2 22 NA NA NA",
                                    "2 NA NA NA NA", "21 21 NA NA NA", "22 18 NA NA NA",
                                    "22 2 NA NA NA", "22 5 5 NA NA", "22 5 NA NA NA", 
                                    "5 14 5 NA NA", "5 14 NA NA NA", "5 22 5 NA NA", "5 22 NA NA NA",
                                    "5 5 22 NA NA", "5 5 5 NA NA", "5 5 NA NA NA",
                                    "5 NA NA NA NA", "5 5 14 NA NA", "8 8 8 NA NA",
                                    "8 8 NA NA NA", "8 NA NA NA NA", "21 NA NA NA NA") ~ 1,
                          c =="NA NA NA NA NA" ~ 2,  TRUE ~ 0)) |>
  mutate(prefer= factor(prefer, levels=0:1))
#add new variable preferred antibiotics "prefer" to dataset                               
data1<-
  data1 |>
  mutate(id = row_number())|>
  full_join(df_new, by=join_by(id))
#create variable postop any major complications
data1 <-
  data1 |>
  mutate (cmp_anymajor=case_when(event_readmit==1 |cmp_ssi==1 | cmp_sep==1 |
                                   cmp_ssep==1 |cmp_pn==1 |cmp_cardivas==1|	
                                   cmp_trans==1 |	cmp_vte ==1 ~1, TRUE ~0))

# create categorical variables ebl, hct
data1<-
  data1 |>
  mutate(ebl=case_when(fluid_out_ebl_total>400 ~ ">400 mL",
                       fluid_out_ebl_total <=400 ~ "<=400 mL"))|>
 # mutate_at('asaclass', ~na_if(., ''))|>
  mutate(hct_no=replace(hct_no, hct_no<0, NA ))|>
  mutate(hct=case_when(hct_no <36 ~ "Anemia",
                       hct_no >=36 ~ "Normal"))

#04-28-2023
data1<- data1 |>
  mutate(bmigroup1 = case_when(bmi < 30 ~ "Normal",
                              bmi < 40 ~ "Obese",
                              bmi >= 40 ~ "Morbidly obese"))|>
  mutate(specmn_weight_grams=num(specmn_weight_grams, digits = 0))|>
  mutate(uterine = case_when(specmn_weight_grams < 250 ~ "1:Under 250 g",
                             specmn_weight_grams < 500 ~ "2:250-499 g",
                             specmn_weight_grams < 1000 ~ "3:500-999 g",
                             specmn_weight_grams >=1000 ~ "4:>=1000 g"))|>
  mutate(uterine1 = case_when(uterine=="1:Under 250 g" & volume== "high" ~ "High volume-1-
                              Under 250g",
                              uterine=="2:250-499 g" & volume== "high" ~ "High volume-2-
                              250-499g",
                              uterine=="3:500-999 g" & volume== "high" ~ "High volume-3-
                              500-999g",
                              uterine=="4:>=1000 g" & volume== "high" ~ "High volume-4-
                              >=1000g",
                              uterine=="1:Under 250 g" & volume== "low" ~ "Low volume-1-
                              Under 250g",
                              uterine=="2:250-499 g" & volume== "low" ~ "Low volume-2-
                              250-499g",
                              uterine=="3:500-999 g" & volume== "low" ~ "Low volume-3-
                              500-999g",
                              uterine=="4:>=1000 g" & volume== "low" ~ "Low volume-4-
                              >=1000g"))|>
  mutate(prefer=factor(prefer,
                      levels=c(0:1),
                       labels=c("No", "Yes")))|>
  labelled::set_variable_labels(
    bmigroup1 ="BMI",
    uterine = "Uterine weight",
    uterine1 = "Surgeon volume & Uterine weight",
    prefer = "Preferred antibiotics administered")

data1 <- data1 |>
  mutate(uterine2 = case_when(uterine=="1:Under 250 g" & hb_mis== "Yes" ~ "MIS-1-
                              Under 250g",
                              uterine=="2:250-499 g" & hb_mis== "Yes" ~ "MIS-2-
                              250-499g",
                              uterine=="3:500-999 g" & hb_mis== "Yes" ~ "MIS-3-
                              500-999g",
                              uterine=="4:>=1000 g" & hb_mis== "Yes" ~ "MIS-4-
                              >=1000g",
                              uterine=="1:Under 250 g" & hb_mis== "No" ~ "Open-1-
                              Under 250g",
                              uterine=="2:250-499 g" & hb_mis== "No" ~ "Open-2-
                              250-499g",
                              uterine=="3:500-999 g" & hb_mis== "No" ~ "Open-3-
                              500-999g",
                              uterine=="4:>=1000 g" & hb_mis== "No" ~ "Open-4-
                              >=1000g"))|>
    labelled::set_variable_labels(
    uterine2 = "Surgical approach & Uterine weight")

#save clean dataset 
export(data1, "data1.rds")
export(data1, "data1.csv")  
###done tidy

#Other 
data1|>
  select(race, ultrsnd_uterine_dimensions, ultrsnd_uterine_dimensions2, 
         ultrsnd_uterine_dimensions3, ultrsnd_uterine_gestational_wk, 
         ultrsnd_uterine_mass_gram_no, ultrsnd_uterine_size, cmp_anymajor)|>
  tbl_summary(
    by=race
  )

#create variable ultrsnd_uterine_dimension
test_b<-
  data1|>
  select(id,  ultrsnd_uterine_dimensions, ultrsnd_uterine_dimensions2, 
         ultrsnd_uterine_dimensions3)
#test_a[is.na(test_a)] <- ""
# pivot longer to gather length, width, height into a single column
uterine_long <- 
  test_b %>%
  pivot_longer(cols = starts_with("ultrsnd_uterine_"),
               names_to = "ultrsnd_uterine_col", 
               values_to = "ultrsnd_uterine_val") 
#select(-c_col) # drop the original column names

# group by id and concatenate values into a space-separated string
uterine_new <- uterine_long %>%
  group_by(id) %>%
  summarize(c = paste(ultrsnd_uterine_val, collapse = " ")) %>%
  ungroup() # remove the grouping

uterine_new |>
  tbl_summary()
#any relationship between ultrasound measurement and actual uterine weight?
test01<-
  data1|>
  filter(uterine=="1:Under 250 g")|>
  select(ultrsnd_uterine_dimensions, ultrsnd_uterine_dimensions2, 
         ultrsnd_uterine_dimensions3, specmn_weight_grams, uterine)|>
  mutate(specmn=ultrsnd_uterine_dimensions *ultrsnd_uterine_dimensions2
         *ultrsnd_uterine_dimensions)
flexplot(specmn_weight_grams ~ ultrsnd_uterine_dimensions, data=test01)  

data1|>
  mutate(ultrsnd_uterine_dimensions=replace(ultrsnd_uterine_dimensions,
                                            ultrsnd_uterine_dimensions<=4, NA))|>
  mutate(ultrsnd_uterine_dimensions=replace(ultrsnd_uterine_dimensions,
                                            ultrsnd_uterine_dimensions>=15, NA))|>
  mutate(uterine=case_when(ultrsnd_uterine_dimensions <=10 ~ "Normal",
                           ultrsnd_uterine_dimensions > 10  ~ "Large"))|>
  filter(uterine=="Large")|>
  filter(volume=="high")|>
  filter(hb_mis=="Yes") |>
  select(race,hct, ebl, cmp_anymajor)|>
  tbl_summary(
    by=race
  )|>
  add_p() # data suggested anemia might be a risk factor contributing to Black patients 
#increased odds of major postoperative complications. 
#Normal uterine dimensions https://sso.uptodate.com/contents/image/print?imageKey=OBGYN%2F76299&source=graphics_gallery&topicKey=3253
### other stuff (not important)
#discharge  > 3 days
data1|>
  filter(incision_time>72)|> 
  select(race, flg_cmp_ssi_any, cmp_sep,	cmp_ssep,	cmp_cdiff,
         cmp_uti2,cmp_pn, cmp_cardivas,	cmp_trans,	cmp_vte, cmp_bowel,	cmp_uret,	
         cmp_bladder,	cmp_aki, cmp_vcc,	cmp_ng)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() # no difference

#discharge >1day
data1|>
  filter(incision_time>24)|>
  select(race, flg_cmp_ssi_any, cmp_sep,	cmp_ssep,	cmp_cdiff, cmp_uti2,cmp_pn, 
         cmp_cardivas,	cmp_trans,	cmp_vte, cmp_bowel,	cmp_uret,	
         cmp_bladder,	cmp_aki, cmp_vcd, cmp_vcc,	cmp_ng)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() #different: flg_cmp_ssi_any, cmp_sep, cmp_trans, cmp_uret, cmp_aki, cmp_vcc,	cmp_ng

#discharge 1-3 days
data1|>
  filter(incision_time>24 & incision_time <72)|>
  select(race, flg_cmp_ssi_any, cmp_sep,	cmp_ssep,	cmp_cdiff, cmp_uti2,cmp_pn, 
         cmp_cardivas,	cmp_trans,	cmp_vte, cmp_bowel,	cmp_uret,	
         cmp_bladder,	cmp_aki, cmp_vcd, cmp_vcc,	cmp_ng)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p()#different: flg_cmp_ssi_any cmp_sep cmp_trans cmp_vcc

#discharge >5 days
data1|>
  filter(incision_time>120)|>
  select(race, flg_cmp_ssi_any, cmp_sep,	cmp_ssep,	cmp_cdiff, cmp_uti2,cmp_pn, 
         cmp_cardivas,	cmp_trans,	cmp_vte, cmp_bowel,	cmp_uret,	
         cmp_bladder,	cmp_aki, cmp_vcd, cmp_vcc,	cmp_ng)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() #different: cmp_uret
# non sameday discharge cases
data1|>
  filter(incision_time >=24)|>
  mutate(discharge=case_when(incision_time >=24 &incision_time <72 ~ "1-3 days",
                             incision_time <120 ~ "3-5 days",
                             incision_time <168 ~ "5-7 days",
                             incision_time >=168 ~ "7 or more days"))|>
  select(race, incision_time, discharge)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p()
glimpse(data1)
# Figure: length of hospital stay
data1|>
  mutate(length=case_when(incision_time <24 ~ 1,
                          incision_time <48 ~ 2,
                          incision_time <72 ~ 3,
                          incision_time <96 ~ 4,
                          incision_time <120 ~ 5,
                          incision_time <144 ~ 6,
                          incision_time <168 ~ 7,
                          incision_time >=168 ~ 8))|>
  mutate(length=factor(length))|>
  select(race, length)|>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p()
###Experiment1: calculate surgeon volume: quartile or tertile###
volume <-
  data|>
  select(physician_cid)|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() 
volume1 <-
  volume|>
  summarise(
    median = median(frequency),
    q1 = quantile(frequency, 0.25),
    q2 = quantile(frequency, 0.5),
    q3 = quantile(frequency, 0.75),
    q4 = max(frequency)
  )
volume1
#median   q1   q2    q3    q4
#  12     4    12    33   354
volume2 <-
  volume|>
  summarise(
    q1 = quantile(frequency, 0.33),
    q2 = quantile(frequency, 0.66),
    q3 = max(frequency))
volume2
#5 23 354

volume |>
  mutate(group=case_when(frequency<4 ~1,
                         frequency<=12 ~2,
                         frequency<=33 ~3,
                         frequency>33 ~4))|>
  tbl_summary()
#25%, 26%, 25%, 25% (quartile)
volume |>
  mutate(group1=case_when(frequency<=5 ~1,
                          frequency<=23 ~2,
                          frequency>23 ~3))|>
  tbl_summary()
#34% 33% 34% (tertile)
#calculate surgeon volume by total cases
dat <-
  data|>
  group_by(physician_cid) |>
  summarise(
    frequency = n()
  ) |>
  ungroup() |>
  mutate(surgeon=case_when(frequency<4 ~1,
                           frequency<=12 ~2,
                           frequency<=33 ~3,
                           frequency>33 ~4))
dat <-
  dat |>
  select(physician_cid, surgeon)|>
  left_join(data, dat, by=join_by(physician_cid), relationship="many-to-many")

###Experiment 2: surgery time and time from incision to discharge 
# operation <30min ~ NA
# time from incision to discharge < 2 hours ~ NA
time<-
  data |>
  select(surgery_duration, admit_to_discharge, incision_to_discharge, 
         surgery_time, incision_time)|>
  mutate(surgeryt=str_sub(surgery_duration,8,15))|>
  mutate(surgery_duration1 = hms(surgeryt)) |> 
  mutate(surgery_duration2 = surgery_duration1 + days(str_sub(surgery_duration, 1, 1))) |>
  mutate(surgery_time = as.numeric(surgery_duration2, "hours"))|>  
  mutate(surgery_time = case_when(surgery_time < 0.5 ~ "NA",
                                  TRUE ~  as.character(surgery_time))) |>
  mutate(incision_to_discharge = case_when(
    incision_to_discharge %in% c("95 days 03:36:00", "94 days 00:48:00",
                                 "92 days 04:59:00") ~ "NA",TRUE ~ as.character(incision_to_discharge))) |>
  mutate(inci=str_sub(incision_to_discharge,8,15))|>
  mutate(inci1 = hms(inci)) |> 
  mutate(inci2 = inci1 + days(str_sub(incision_to_discharge, 1, 1))) |>
  mutate(incision_time = as.numeric(inci2, "hours")) |>
  mutate(incision_time = case_when(incision_time < 2.0 ~ "NA",
                                   TRUE ~  as.character(incision_time))) |>
  mutate_at(vars("surgery_time", "incision_time"), as.numeric)|>
  mutate(sameday=case_when(incision_time <=24 ~ 1,
                           incision_time >24 ~0,
                           TRUE ~ incision_time))|>
  mutate(sameday=factor(sameday))


