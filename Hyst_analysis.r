library(tidyverse)
library(gtsummary)
library(forcats)
library(rio)
require(lme4)
require(broom.mixed)
# read rds
data1 <- readRDS("data1.rds")

#Table 1 demo
data1 |>
  mutate(across(where(is.character), ~na_if(., " ")))|>
  mutate_at('asaclass', ~na_if(., ''))|>
  mutate_at('functional', ~na_if(., ''))|>
  select(calculated_age, race1, ethnicity_hispanic,	bmi, bmigroup1, asaclass,	flg_cmb_sleep_apnea,
         flg_cmb_diabetes,	flg_cmb_hypertension,	flg_cmb_smoker,	flg_cmb_dvt,
         functional,	flg_cmb_coronary_artery,indication_uterine_bleeding,
         indication_uterine_fibroids,	indication_endometriosis,
         indication_chronic_pelvic_pain,	indication_cervical_dysplasia,
         indication_pelvic_organ_prolapse,	e_teaching, e_beds, insurance,volume) |>
  tbl_summary(
    by=race,
    #  missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_n()|>
  add_p() |>
  modify_caption("**Table 1. Demographic and Clinical Characteristics**")


#Table2
data1 |>
  mutate(across(where(is.character), ~na_if(., " ")))|>
  #  mutate(across(where(is.numeric()), ~na_if(., " ")))|>
  mutate_at('skin', ~na_if(., ''))|>
  #  mutate_at('pharm_periop_vte_prophylaxis', ~na_if(., ''))|>
  select(race1, flg_hyster_alt_embolization,	flg_hyster_alt_laparoscopy,
         flg_hyster_alt_myomectomy,	flg_hyster_alt_physical,	flg_hyster_alt_hormone,	
         flg_hyster_alt_iud,	flg_hyster_alt_ablation, flg_hyster_alt_scopy,	
         flg_hyster_alt_pessary,	flg_w_h_cmplt_pap, flg_w_h_cmplt_biopsy, 
         cmplt_image, hct_no, hct, skin,mech_periop_vte_prophylaxis,
         pharm_periop_vte_prophylaxis,	hb_mis, e_fallopian_removal,	e_ovary_removal,
         bleeding_barrier,	fluid_out_ebl_total, prefer,
         intraop_warming,	flg_intraop_multimodal_pain_med,approach,
         surgery_time, uterine, uterine2) |>
  tbl_summary(
    by=race1,
  #  missing = "no",
    label = prefer ~ "Preferred antibiotics administered",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_n()|>
  add_p()|>
  modify_caption("**Table 2. Surgical Characteristics**")
#add variables specmn_weight_type, specmn_weight_grams

#Table 3

data1 |>
  select(race1, event_edorur, event_return, sameday, mis_open, 
         intraop_complication_bladder_thickness,	intraop_complication_bowel_thickness,
         cmp_anymajor, cmp_ssi,	cmp_sep,	cmp_ssep, cmp_cardivas, cmp_pn, cmp_vte,
         cmp_trans, event_readmit, flg_cmp_ssi_any,  flg_cmp_superficial_ssi, 	cmp_cdiff,
         cmp_uti2,  cmp_bowel,	cmp_uret,	 cmp_bladder,	cmp_fistula,cmp_aki,	
         cmp_vcd,cmp_vcc,	cmp_ng)|>
  tbl_summary(
    by=race1,
    missing = "no",
    label = list(cmp_vte ~ "Venous thromboembolism",
                 cmp_anymajor ~ "Major Postoperative Complications"),
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Hysterectomy Patients**") |>
  modify_caption("**Table 3. Postop Characteristics**")

#Table4 
# univariate, logistic regression, unadjusted odds ratio
data1$volume <- forcats::fct_relevel(data1$volume, "high")
data1$bmigroup1 <- forcats::fct_relevel(data1$bmigroup1, "Normal")
data1$hb_mis <- forcats::fct_relevel(data1$hb_mis, "Yes")
data1$asaclass <- forcats::fct_relevel(data1$asaclass, "ASA class <3")
data1$e_teaching <- forcats::fct_relevel(data1$e_teaching, "Non-teaching Hospital")
data1$race1 <- forcats::fct_relevel(data1$race1, "White")
data1$hct <- forcats::fct_relevel(data1$hct, "Normal")
data1$prefer <- forcats::fct_relevel(data1$prefer, "Yes")
data1$uterine <- forcats::fct_relevel(data1$uterine, "1:Under 250 g")
data1 |>
  select(bmigroup1, race1, asaclass, e_teaching, insurance,volume, hct, hb_mis, 
         bleeding_barrier, surgery_time, prefer, uterine, cmp_anymajor) |>
  tbl_uvregression(method = glm, 
                   y= cmp_anymajor,
                   method.args = list(family = binomial),
                   # estimate_fun = purrr::partial(style_ratio, digits = 6),
                   #   pvalue_fun = purrr::partial(style_sigfig, digits = 3),
                   exponentiate = TRUE) |>
  add_global_p()|>
  modify_caption("**Table 4. Univariate logistic regression**")
#  bold_p (t=0.10) 

# multivariable logistic regression
glm(cmp_anymajor ~ race + bmigroup1 + asaclass + insurance + e_teaching + 
         surgery_time + bleeding_barrier + prefer + volume + hct + hb_mis+uterine, 
         data=data1, family=binomial)|>
   tbl_regression(exponentiate = TRUE)|>
  modify_caption("**Multivariable logistic regression**") 

 
#mixed model
 glmer(cmp_anymajor ~ race1 + bmigroup1 + asaclass + insurance +
           bleeding_barrier + prefer + surgery_time + hct + hb_mis+uterine +
           (1 | physician_cid), data=data1, family=binomial, control = glmerControl(optimizer = "bobyqa"),
         nAGQ = 10)|>
  tbl_regression(exponentiate = TRUE)|>
  modify_caption("**Mixed model - surgeon (random effect) **")
  
  
#references:
#ebl reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6485959/
#hct_no: ref: https://pubmed.ncbi.nlm.nih.gov/17565082/
#stratified patients into standard categories of anemia (hematocrit <39.0%), 
#normal hematocrit (39.0%-53.9%), and polycythemia (hematocrit > or =54%). 
# https://www.redcrossblood.org/donate-blood/dlp/hematocrit.html normal: 36-48%

data1|>
  select(bmi, bmigroup1, race1, asaclass, e_teaching, insurance,volume, hct, hb_mis, 
         bleeding_barrier, surgery_time, prefer, uterine, ebl, cmp_anymajor) |>
  tbl_summary(
    by=cmp_anymajor) |>
  add_p()
# 05-22-23
# Dr.Mogan notes: remove readmission or transfusion
# old cmp_anymajor group: event_readmit, cmp_ssi, cmp_sep, cmp_ssep, cmp_pn,
#cmp_cardivas,  cmp_trans, cmp_vte;
# new cmp_anymajor group: cmp_ssi, cmp_sep, cmp_ssep, cmp_pn,cmp_cardivas, cmp_vte;


###06-19-2023
#new postop major complications "cmp_anymajor1"
data1 <-
  data1 |>
  mutate (cmp_anymajor1=case_when(cmp_ssi==1 | cmp_sep==1 |
                                   cmp_ssep==1 |cmp_pn==1 |cmp_cardivas==1|	
                                   cmp_vte ==1 ~1, TRUE ~0))|>
  labelled::set_variable_labels(
    cmp_anymajor1 ="Major Postoperative Complications")

#exclude obs with missing values
data2 <- data1 |>
  drop_na(bmigroup1, race1, asaclass, e_teaching, insurance,volume, hct, hb_mis, 
         bleeding_barrier, surgery_time, prefer, uterine, cmp_anymajor1) #18395 obs
data2$bmigroup1<- forcats::fct_relevel(data2$bmigroup1, "Normal", "Obese", "Morbidly obese" )
data2 <- data2|>
  labelled::set_variable_labels(
    bmigroup1 = "BMI (category)",
    asaclass = "Asa",
    volume = "Surgeon volume",
    uterine = "Uterine weight (category)",
    specmn_weight_grams = "Uterine weight (continuous)")

#Table 1 demo
data2 |>
  select(calculated_age, race1, ethnicity_hispanic,	bmi, bmigroup1, asaclass,	flg_cmb_sleep_apnea,
         flg_cmb_diabetes,	flg_cmb_hypertension,	flg_cmb_smoker,	flg_cmb_dvt,
         functional,	flg_cmb_coronary_artery,indication_uterine_bleeding,
         indication_uterine_fibroids,	indication_endometriosis,
         indication_chronic_pelvic_pain,	indication_cervical_dysplasia,
         indication_pelvic_organ_prolapse,	e_teaching, e_beds, insurance,volume) |>
  tbl_summary(
    by=race1,
    #  missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_caption("**Table 1. Demographic and Clinical Characteristics**")
#update 2-28-24
data2 |>
  select(race1,flg_hyster_alt_embolization, flg_hyster_alt_myomectomy, flg_hyster_alt_hormone, flg_hyster_alt_iud, flg_hyster_alt_ablation) |>
  tbl_summary(
    by=race1,
    #  missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() 



#Table2
data2 <- data2 |>
  mutate(mis_uterine=case_when(hb_mis=="Yes" ~ specmn_weight_grams))|>
  mutate(open_uterine=case_when(hb_mis=="No" ~ specmn_weight_grams))|>
  mutate(mis_uterine=as.numeric(mis_uterine))|>
  mutate(open_uterine=as.numeric(open_uterine))|>
  mutate(specmn_weight_grams=as.numeric(specmn_weight_grams))|>
  labelled::set_variable_labels(
    mis_uterine = "Uterine weight (MIS)",
    open_uterine = "Uterine weight (Open)",
    specmn_weight_grams = "Uterine weight (continuous)")
data2 |>
  mutate_at('skin', ~na_if(., ''))|>
  select(race1, flg_w_h_cmplt_pap, flg_w_h_cmplt_biopsy, 
         cmplt_image, hct_no, hct, skin, mech_periop_vte_prophylaxis,
         pharm_periop_vte_prophylaxis,	e_fallopian_removal,	e_ovary_removal,
         bleeding_barrier,	fluid_out_ebl_total, prefer,
         intraop_warming,	flg_intraop_multimodal_pain_med,approach,hb_mis, mis_open, 
         surgery_time, specmn_weight_grams, mis_uterine, open_uterine, uterine, uterine2) |>
  tbl_summary(
    by=race1,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_p()|>
  modify_caption("**Table 2. Surgical Characteristics**")
# table 3
data2 |>
  select(race1, event_edorur, event_return, sameday, 
                  cmp_anymajor1, cmp_trans, event_readmit)|>
  mutate(sameday=factor(
                      sameday,
                      levels=0:1,
                      labels = c("No", "Yes")))|>
  tbl_summary(
    by=race1,
    missing = "no",
    label=sameday ~ "Same day discharge",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Hysterectomy Patients**") |>
  modify_caption("**Table 3. Postop Characteristics**")
 # modify_table_body(~.x %>% dplyr::relocate(stat_2, .after = label))

#Table4 
# univariate, logistic regression, unadjusted odds ratio
data2$volume <- forcats::fct_relevel(data2$volume, "high")
data2$bmigroup1 <- forcats::fct_relevel(data2$bmigroup1, "Normal")
data2$hb_mis <- forcats::fct_relevel(data2$hb_mis, "Yes")
data2$asaclass <- forcats::fct_relevel(data2$asaclass, "ASA class <3")
data2$e_teaching <- forcats::fct_relevel(data2$e_teaching, "Non-teaching Hospital")
data2$race1 <- forcats::fct_relevel(data2$race1, "White")
data2$hct <- forcats::fct_relevel(data2$hct, "Normal")
data2$prefer <- forcats::fct_relevel(data2$prefer, "Yes")
data2$uterine <- forcats::fct_relevel(data2$uterine, "1:Under 250 g")
data2 |>
  select(bmigroup1, race, asaclass, e_teaching, insurance,volume, hct, hb_mis, 
         bleeding_barrier, surgery_time, prefer, uterine, cmp_anymajor1) |>
  tbl_uvregression(method = glm, 
                   y= cmp_anymajor1,
                   method.args = list(family = binomial),
                   exponentiate = TRUE) |>
#  add_global_p()|>
  modify_caption("**Table 4. Univariate logistic regression**")

# multivariable logistic regression
#remove e_teaching (uOR=0.3)
glm(cmp_anymajor1 ~ race + bmigroup1 + asaclass + insurance  + 
      surgery_time + bleeding_barrier + prefer + volume + hct + hb_mis+uterine, 
    data=data2, family=binomial)|>
  tbl_regression(exponentiate = TRUE)|>
  modify_caption("**Multivariable logistic regression**") 

#mixed model
glmer(cmp_anymajor1 ~ race + bmigroup1 + asaclass + insurance +
        bleeding_barrier + prefer + surgery_time + hct + hb_mis+uterine +
        (1 | physician_cid), data=data2, family=binomial, control = glmerControl(optimizer = "bobyqa"),
      nAGQ = 10)|>
  tbl_regression(exponentiate = TRUE)|>
  modify_caption("**Mixed effects logistic regression") #1.32(1.10, 1.58) p=0.002

#save complete dataset 
export(data2, "data2.rds")

data2 |>
  mutate_at('skin', ~na_if(., ''))|>
  select(race, flg_w_h_cmplt_pap, flg_w_h_cmplt_biopsy, 
         cmplt_image, skin, mech_periop_vte_prophylaxis,
         pharm_periop_vte_prophylaxis,	
         bleeding_barrier,	prefer,
         intraop_warming,	flg_intraop_multimodal_pain_med) |>
  tbl_summary(
    by=race,
   # missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_p()|>
  modify_caption("**Intraoperative Characteristics**")
data2 |>
  select(race, approach,hb_mis, mis_open, 
         surgery_time, mis_uterine, open_uterine, volume) |>
  tbl_summary(
    by=race,
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_p()|>
  modify_caption("**Surgical Characteristics**")
#06-22
data2 <- data2|>
  mutate_at('skin', ~na_if(., ''))|>
  mutate(sameday=factor(
    sameday,
    levels=0:1,
    labels = c("No", "Yes")))|>
  labelled::set_variable_labels(
    sameday = "Same day discharge") 
data2 |>  
  select(calculated_age, bmi, bmigroup1, race1, e_teaching, e_beds, insurance,
         flg_cmb_sleep_apnea,
         flg_cmb_diabetes,	flg_cmb_hypertension,	flg_cmb_smoker,	flg_cmb_dvt,
         functional,	flg_cmb_coronary_artery,
         asaclass, hct_no, ebl,  hct, 
         specmn_weight_grams,  uterine, flg_w_h_cmplt_pap, flg_w_h_cmplt_biopsy, 
         cmplt_image, skin, mech_periop_vte_prophylaxis,
         pharm_periop_vte_prophylaxis,	
         bleeding_barrier,	prefer,
         intraop_warming,	flg_intraop_multimodal_pain_med,
         volume, hb_mis, mis_open, mis_uterine, open_uterine,
         bleeding_barrier, surgery_time, sameday, cmp_anymajor1
         ) |>
  tbl_summary(
    by=cmp_anymajor1) |>
  add_p() |>
  modify_header(
    stat_1 ~ "**No**",
    stat_2 ~ "**Yes**"
  ) |> 
  modify_spanning_header(all_stat_cols() ~ "**Major postoperative complications**")
data2 |>
  select(race1, cmplt_image, 	
         bleeding_barrier,	prefer,approach,hb_mis, mis_open, 
         surgery_time, mis_uterine, open_uterine, volume
         ) |>
  tbl_summary(
    by=race,
    # missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_p()
data2 |>
  select(race1, bmi, bmigroup1, insurance,
         flg_cmb_diabetes,	flg_cmb_dvt,
                  asaclass, ebl,  hct, 
         specmn_weight_grams,  uterine
  ) |>
  tbl_summary(
    by=race1,
    # missing = "no",
    digits = list(all_categorical() ~ c(0, 1)))|>
  # add_overall()|>
  add_p()
data2 |>
  select(race1, bmigroup1, insurance, asaclass, flg_cmb_diabetes,  hct, uterine, 
         bleeding_barrier, volume, hb_mis, 
          surgery_time, cmp_anymajor1) |>
  tbl_uvregression(method = glm, 
                   y= cmp_anymajor1,
                   method.args = list(family = binomial),
                   exponentiate = TRUE) |>
  #  add_global_p()|>
  modify_caption("**Univariate logistic regression**")
glmer(cmp_anymajor1 ~ race1 + bmigroup1 +  insurance + asaclass  + flg_cmb_diabetes
       + hct + uterine +  bleeding_barrier + hb_mis + surgery_time + 
        (1 | physician_cid), data=data2, family=binomial, control = glmerControl(optimizer = "bobyqa"),
      nAGQ = 10)|>
  tbl_regression(exponentiate = TRUE)|>
  modify_caption("**Mixed effects logistic regression**") #1.40(1.05, 1.89) p=0.024
glm(cmp_anymajor1 ~ race + bmigroup1 +  insurance + asaclass  + flg_cmb_diabetes
      + hct + uterine + bleeding_barrier + surgery_time, data=data2, family=binomial)|>
  tbl_regression(exponentiate = TRUE)
data2 |>
  select(race, cmp_anymajor1, event_edorur, event_return, sameday, 
         cmp_trans, event_readmit)|>
   tbl_summary(
    by=race,
    missing = "no",
    label=sameday ~ "Same day discharge",
    digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(
    all_stat_cols() ~ "**Hysterectomy Patients**") |>
  modify_caption("**Postop Characteristics**")
#09-15-23
data2 <- readRDS("data2.rds")
data2 |>
  select(volume, hb_mis, 
         uterine, cmp_anymajor1) |>
  tbl_uvregression(method = glm, 
                   y= cmp_anymajor1,
                   method.args = list(family = binomial),
                   exponentiate = TRUE)
glm(cmp_anymajor1 ~ uterine + volume + uterine*volume, data=data2, family=binomial)|>
  tbl_regression(exponentiate = TRUE)
model<- glm(cmp_anymajor1 ~ uterine*volume + surgery_time, data=data2)
#plot<- graph_model(model, y=cmp_anymajor1, x=uterine, lines=volume, draw.legend=T)+
#  theme(legend.position="top") #require package reghelper; outcome couldn't be binary
summary(model)
#look at fitted values (specifically, the predicted probability of observing y==1 in each model.
fitted <- predict(model, type = "response", se.fit = TRUE)
fitted
plot(NA, ylim = c(0, 1), xlab = "uterine weight", ylab = "Predicted Probability of y=1")
points(data2$uterine[data2$volume == "low"], fitted$fit[data2$volume == "low"], col = rgb(1, 0, 0, 0.5))
points(uterine[volume == "high"], fitted$fit[volume == "high"], col = rgb(0, 0, 1, 0.5))

