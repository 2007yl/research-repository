#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load library
library(Hmisc)
library(tidyverse)
library(rio)
library(labelled)
library(janitor)
library(gtsummary)
#Read Data
data=read.csv('rawdata/VirtualVsInPersonChe_DATA_2023-09-01_0906.csv')
#Setting Labels
label(data$record_id)="Record ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$patient_preference_survey_timestamp)="Survey Timestamp"
label(data$demo_commdescrp)=" How would you describe the community you currently live in?"
label(data$demo_traveltime)="How long does it take you to travel to your gynecologic oncologists office? "
label(data$demo_traveldist)="Please approximate the distance from your home to your gynecologic oncologists office. "
label(data$demo_travelmethod___1)="How do you most commonly travel to your appointments?  (choice=Walk or bike)"
label(data$demo_travelmethod___2)="How do you most commonly travel to your appointments?  (choice=Personal car - I drive myself)"
label(data$demo_travelmethod___3)="How do you most commonly travel to your appointments?  (choice=Personal car - someone else drives me)"
label(data$demo_travelmethod___4)="How do you most commonly travel to your appointments?  (choice=Taxi)"
label(data$demo_travelmethod___5)="How do you most commonly travel to your appointments?  (choice=Ride-share service (e.g., Uber, Lyft))"
label(data$demo_travelmethod___6)="How do you most commonly travel to your appointments?  (choice=Public transport (e.g., bus, train))"
label(data$demo_workstatus)="Please select which statement applies to you. "
label(data$demo_income)="What is your estimated annual household income? "
label(data$demo_educ)="What is the highest degree or level of education you have completed? "
label(data$demo_insurance___1)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Medicaid)"
label(data$demo_insurance___2)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Medicare)"
label(data$demo_insurance___3)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Military Insurance)"
label(data$demo_insurance___4)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Private Insurance (PPO, HMO, etc.))"
label(data$demo_insurance___5)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Out-of-pocket / Uninsured)"
label(data$demo_insurance___6)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Unsure)"
label(data$demo_insurance___7)="Please indicate which of the following insurance plans below best describes your current insurance coverage. Select all that apply: (choice=Other)"
label(data$demo_insurance_other)="You have selected other to the previous question about insurance status. Please state your current status:"
label(data$demo_dep)="Do you have any children aged 0 to 17 living at home with you, or who you have regular responsibilities for? "
label(data$tech_firstmeet)="Was your first time meeting your gynecologic oncologist in-person or virtual? "
label(data$tech_device)="What type of device do you most often use to participate in virtual visits with your doctor?"
label(data$tech_device_share)="Please rate your experience getting access to your shared device for your appointment as one of the following:"
label(data$tech_comfortlvl)="How comfortable are you using technology to participate in virtual visits with your doctor?"
label(data$tech_vidqual)="Please rate the video quality during your virtual visit with your gynecologic oncologist:"
label(data$tech_voicequal)="Please rate the voice quality during your virtual visit with your doctor:"
label(data$tech_difficulties___1)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Internet connectivity)"
label(data$tech_difficulties___2)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Logging into Visit)"
label(data$tech_difficulties___3)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Battery/Power Source for Devices)"
label(data$tech_difficulties___4)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Poor Audio Quality)"
label(data$tech_difficulties___5)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Poor Video Quality)"
label(data$tech_difficulties___6)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=Audio or video delays/issues)"
label(data$tech_difficulties___7)="Do any of these factors cause technical difficulties when participating in virtual visits with your doctor? Please select all that apply.  (choice=I have not experienced technical difficulties)"
label(data$tech_preventusage___1)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=Devices ( e.g. mobile phone, computer, tablet) are too expensive)"
label(data$tech_preventusage___2)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=There is no signal or poor signal where I live)"
label(data$tech_preventusage___3)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=Paying for internet/data is too expensive)"
label(data$tech_preventusage___4)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=Its too difficult to use)"
label(data$tech_preventusage___5)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=I am worried about my privacy)"
label(data$tech_preventusage___6)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=Other (please specify):)"
label(data$tech_preventusage___7)="Do any of these factors limit or prevent your use of virtual care with your doctor? Please select all that apply. (choice=None of these)"
label(data$demo_prevent_vv_other)="If you selected other regarding factors limiting or preventing use of virtual care with your doctor, please specify:"
label(data$inpersmatrix_001)="Waiting time prior to seeing your care team"
label(data$inpersmatrix_002)="The courtesy, respect, and sensitivity of the care team"
label(data$inpersmatrix_003)="Explanations the care team gave you about your treatment or condition"
label(data$inpersmatrix_004)="Concern the care team showed for your questions or worries"
label(data$inpersmatrix_005)="Care team efforts to include you in decision about your treatment"
label(data$inpersmatrix_006)="Information the care team gave you about medications (if any)"
label(data$inpersmatrix_007)="Instructions the care team gave you about follow up care (if any)"
label(data$inpersmatrix_008)="Degree to which care team talked with you using words you could understand"
label(data$inpersmatrix_009)="Amount of time care team spent with you"
label(data$inpers_overall)="Overall, how would you rate your experience with IN-PERSON, pre-chemotherapy visits? "
label(data$virtualmatrix_001)="Waiting time prior to seeing your care team"
label(data$virtualmatrix_002)="The courtesy, respect, and sensitivity of the care team"
label(data$virtualmatrix_003)="Explanations the care team gave you about your treatment or condition"
label(data$virtualmatrix_004)="Concern the care team showed for your questions or worries"
label(data$virtualmatrix_005)="Care team efforts to include you in decision about your treatment"
label(data$virtualmatrix_006)="Information the care team gave you about medications (if any)"
label(data$virtualmatrix_007)="Instructions the care team gave you about follow up care (if any)"
label(data$virtualmatrix_008)="Degree to which care team talked with you using words you could understand"
label(data$virtualmatrix_009)="Amount of time care team spent with you"
label(data$virtual_overall)="Overall, how would you rate your experience with VIRTUAL (e.g., Zoom), pre-chemotherapy visits? "
label(data$inpersonlymatrix_001)="Having a physical exam (e.g., listening to heart and lungs) performed during my appointment is important."
label(data$inpersonlymatrix_002)="Having a pelvic exam (e.g., vaginal exam) performed during my appointment is important."
label(data$inpersonlymatrix_003)="Non-verbal communication between me and my doctor is important (i.e. eye contact, shaking hands, body language)."
label(data$inpersonlymatrix_004)="I am concerned about being exposed to an infection and getting sick."
label(data$inpersonlymatrix_005)="The time it takes me to travel to the clinic is a burden to me."
label(data$inpersonlymatrix_006)="Travel expenses (e.g, gas, parking fees) are a burden to me."
label(data$visitpref_recc)="How likely are you to recommend pre-chemotherapy VIRTUAL visits to family or friends with gynecologic cancer?"
label(data$visitpref_choice)="In the future, how likely are you to choose VIRTUAL care for pre-chemotherapy visits? "
label(data$visitpref_visitchoice)="When you have a pre-chemotherapy appointment with your gynecologic oncologist, what type of visit would you prefer? "
label(data$visitpref_freetext)="Considering your preference for the last question, please feel free to share why you chose your preference"
label(data$visitpref_ratio)="What portion of your pre-chemotherapy visits would you like to be in-person versus virtual? "
label(data$slidingscale_pref)="In a chemotherapy schedule, what proportion of your visits would you want to be virtual vs. in-person?"
label(data$schedule_var_choice)="Please select which schedule you would choose:"
label(data$optionsched_pics)="Consider a 6-appointment chemotherapy schedule with 3-6 weeks between appointments. If pre-chemotherapy visits included both in-person visits and virtual visits, which schedule below would you prefer? "
label(data$patient_preference_survey_complete)="Complete?"
label(data$chart_review_for_patient_pref_timestamp)="Survey Timestamp"
label(data$michart_name)="Name:"
label(data$michart_dob)="Age"
label(data$michart_gender)="Gender:"
label(data$michart_gender_other)="If other, please describe:"
label(data$michart_race)="Race"
label(data$michart_race_other)="If other, please describe:"
label(data$michart_eth)="Ethnicity"
label(data$michart_bmi)="BMI"
label(data$michart_ecog)="ECOG Performance Status"
label(data$michart_prim_site)="Primary Site"
label(data$michart_figostage)="Stage"
label(data$michart_histopath)="Histopathologic Type"
label(data$michart_prim_recurr)="Type of disease at the time of review"
label(data$ds)="Disease status at time of review"
label(data$michart_chemosched___1)="Treatment goal of chemotherapy regimen at time of review: (choice=Neoadjuvant)"
label(data$michart_chemosched___2)="Treatment goal of chemotherapy regimen at time of review: (choice=Adjuvant)"
label(data$michart_chemosched___3)="Treatment goal of chemotherapy regimen at time of review: (choice=Maintenance)"
label(data$michart_chemosched___4)="Treatment goal of chemotherapy regimen at time of review: (choice=Control)"
label(data$michart_chemosched___5)="Treatment goal of chemotherapy regimen at time of review: (choice=Curative)"
label(data$michart_chemosched___6)="Treatment goal of chemotherapy regimen at time of review: (choice=Palliative)"
label(data$michart_chemosched___7)="Treatment goal of chemotherapy regimen at time of review: (choice=Not currently undergoing chemotherapy)"
label(data$michart_oralyn)="History of prior chemotherapy regimens:"
label(data$michart_numcycle)="Number of prior cycles of chemotherapy at time of review:"
label(data$michart_reg_virtual)="Chemotherapy reg patient was on at time of virtual visit"
label(data$michart_firstvis)="Was patients first visit before or after start of COVID-19 pandemic (1/31/2020)?  "
label(data$michart_totalnumvirtual)="Total # of Gyn Virtual Visits at time of review:"
label(data$chart_review_for_patient_pref_complete)="Complete?"
#clean var names
data<- data |>
  janitor::clean_names() 

#Setting Factors(will create new variable for factors)
data$demo_commdescrp.factor = factor(data$demo_commdescrp,levels=c("1","2","3","4"))
data$demo_traveltime.factor = factor(data$demo_traveltime,levels=c("1","2","3","4","5"))
data$demo_traveldist.factor = factor(data$demo_traveldist,levels=c("1","2","3","4","5"))
data$demo_travelmethod___1.factor = factor(data$demo_travelmethod___1,levels=c("0","1"))
data$demo_travelmethod___2.factor = factor(data$demo_travelmethod___2,levels=c("0","1"))
data$demo_travelmethod___3.factor = factor(data$demo_travelmethod___3,levels=c("0","1"))
data$demo_travelmethod___4.factor = factor(data$demo_travelmethod___4,levels=c("0","1"))
data$demo_travelmethod___5.factor = factor(data$demo_travelmethod___5,levels=c("0","1"))
data$demo_travelmethod___6.factor = factor(data$demo_travelmethod___6,levels=c("0","1"))
data$demo_workstatus.factor = factor(data$demo_workstatus,levels=c("1","2","3","4","5","6"))
data$demo_income.factor = factor(data$demo_income,levels=c("1","2","3","4","5","6"))
data$demo_educ.factor = factor(data$demo_educ,levels=c("1","2","3","4","5","6"))
data$demo_insurance___1.factor = factor(data$demo_insurance___1,levels=c("0","1"))
data$demo_insurance___2.factor = factor(data$demo_insurance___2,levels=c("0","1"))
data$demo_insurance___3.factor = factor(data$demo_insurance___3,levels=c("0","1"))
data$demo_insurance___4.factor = factor(data$demo_insurance___4,levels=c("0","1"))
data$demo_insurance___5.factor = factor(data$demo_insurance___5,levels=c("0","1"))
data$demo_insurance___6.factor = factor(data$demo_insurance___6,levels=c("0","1"))
data$demo_insurance___7.factor = factor(data$demo_insurance___7,levels=c("0","1"))
data$demo_dep.factor = factor(data$demo_dep,levels=c("1","0"))
data$tech_firstmeet.factor = factor(data$tech_firstmeet,levels=c("1","2","3"))
data$tech_device.factor = factor(data$tech_device,levels=c("1","2","3","4","5","6"))
data$tech_device_share.factor = factor(data$tech_device_share,levels=c("1","2","3","4","5"))
data$tech_comfortlvl.factor = factor(data$tech_comfortlvl,levels=c("1","2","3","4","5"))
data$tech_vidqual.factor = factor(data$tech_vidqual,levels=c("1","2","3","4","5"))
data$tech_voicequal.factor = factor(data$tech_voicequal,levels=c("1","2","3","4","5"))
data$tech_difficulties___1.factor = factor(data$tech_difficulties___1,levels=c("0","1"))
data$tech_difficulties___2.factor = factor(data$tech_difficulties___2,levels=c("0","1"))
data$tech_difficulties___3.factor = factor(data$tech_difficulties___3,levels=c("0","1"))
data$tech_difficulties___4.factor = factor(data$tech_difficulties___4,levels=c("0","1"))
data$tech_difficulties___5.factor = factor(data$tech_difficulties___5,levels=c("0","1"))
data$tech_difficulties___6.factor = factor(data$tech_difficulties___6,levels=c("0","1"))
data$tech_difficulties___7.factor = factor(data$tech_difficulties___7,levels=c("0","1"))
data$tech_preventusage___1.factor = factor(data$tech_preventusage___1,levels=c("0","1"))
data$tech_preventusage___2.factor = factor(data$tech_preventusage___2,levels=c("0","1"))
data$tech_preventusage___3.factor = factor(data$tech_preventusage___3,levels=c("0","1"))
data$tech_preventusage___4.factor = factor(data$tech_preventusage___4,levels=c("0","1"))
data$tech_preventusage___5.factor = factor(data$tech_preventusage___5,levels=c("0","1"))
data$tech_preventusage___6.factor = factor(data$tech_preventusage___6,levels=c("0","1"))
data$tech_preventusage___7.factor = factor(data$tech_preventusage___7,levels=c("0","1"))
data$inpersmatrix_001.factor = factor(data$inpersmatrix_001,levels=c("1","2","3","4","5"))
data$inpersmatrix_002.factor = factor(data$inpersmatrix_002,levels=c("1","2","3","4","5"))
data$inpersmatrix_003.factor = factor(data$inpersmatrix_003,levels=c("1","2","3","4","5"))
data$inpersmatrix_004.factor = factor(data$inpersmatrix_004,levels=c("1","2","3","4","5"))
data$inpersmatrix_005.factor = factor(data$inpersmatrix_005,levels=c("1","2","3","4","5"))
data$inpersmatrix_006.factor = factor(data$inpersmatrix_006,levels=c("1","2","3","4","5"))
data$inpersmatrix_007.factor = factor(data$inpersmatrix_007,levels=c("1","2","3","4","5"))
data$inpersmatrix_008.factor = factor(data$inpersmatrix_008,levels=c("1","2","3","4","5"))
data$inpersmatrix_009.factor = factor(data$inpersmatrix_009,levels=c("1","2","3","4","5"))
data$inpers_overall.factor = factor(data$inpers_overall,levels=c("1","2","3","4","5"))
data$virtualmatrix_001.factor = factor(data$virtualmatrix_001,levels=c("1","2","3","4","5"))
data$virtualmatrix_002.factor = factor(data$virtualmatrix_002,levels=c("1","2","3","4","5"))
data$virtualmatrix_003.factor = factor(data$virtualmatrix_003,levels=c("1","2","3","4","5"))
data$virtualmatrix_004.factor = factor(data$virtualmatrix_004,levels=c("1","2","3","4","5"))
data$virtualmatrix_005.factor = factor(data$virtualmatrix_005,levels=c("1","2","3","4","5"))
data$virtualmatrix_006.factor = factor(data$virtualmatrix_006,levels=c("1","2","3","4","5"))
data$virtualmatrix_007.factor = factor(data$virtualmatrix_007,levels=c("1","2","3","4","5"))
data$virtualmatrix_008.factor = factor(data$virtualmatrix_008,levels=c("1","2","3","4","5"))
data$virtualmatrix_009.factor = factor(data$virtualmatrix_009,levels=c("1","2","3","4","5"))
data$virtual_overall.factor = factor(data$virtual_overall,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_001.factor = factor(data$inpersonlymatrix_001,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_002.factor = factor(data$inpersonlymatrix_002,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_003.factor = factor(data$inpersonlymatrix_003,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_004.factor = factor(data$inpersonlymatrix_004,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_005.factor = factor(data$inpersonlymatrix_005,levels=c("1","2","3","4","5"))
data$inpersonlymatrix_006.factor = factor(data$inpersonlymatrix_006,levels=c("1","2","3","4","5"))
data$visitpref_recc.factor = factor(data$visitpref_recc,levels=c("1","2","3","4","5"))
data$visitpref_choice.factor = factor(data$visitpref_choice,levels=c("1","2","3","4","5"))
data$visitpref_visitchoice.factor = factor(data$visitpref_visitchoice,levels=c("1","2","3"))
data$visitpref_ratio.factor = factor(data$visitpref_ratio,levels=c("1","2","3"))
data$schedule_var_choice.factor = factor(data$schedule_var_choice,levels=c("1","2","3","4","5","6","7","8"))
data$optionsched_pics.factor = factor(data$optionsched_pics,levels=c("1","2","3","4","5","6","7","8"))
data$patient_preference_survey_complete.factor = factor(data$patient_preference_survey_complete,levels=c("0","1","2"))
data$michart_gender.factor = factor(data$michart_gender,levels=c("1","2","3","4"))
data$michart_race.factor = factor(data$michart_race,levels=c("1","2","3","4","5","6"))
data$michart_eth.factor = factor(data$michart_eth,levels=c("1","2"))
data$michart_ecog.factor = factor(data$michart_ecog,levels=c("0","1","2","3","4","5"))
data$michart_prim_site.factor = factor(data$michart_prim_site,levels=c("1","2","3","4","5","6"))
data$michart_figostage.factor = factor(data$michart_figostage,levels=c("1","2","3","4","5"))
data$michart_prim_recurr.factor = factor(data$michart_prim_recurr,levels=c("1","2","3"))
data$ds.factor = factor(data$ds,levels=c("1","2","3","4"))
data$michart_chemosched___1.factor = factor(data$michart_chemosched___1,levels=c("0","1"))
data$michart_chemosched___2.factor = factor(data$michart_chemosched___2,levels=c("0","1"))
data$michart_chemosched___3.factor = factor(data$michart_chemosched___3,levels=c("0","1"))
data$michart_chemosched___4.factor = factor(data$michart_chemosched___4,levels=c("0","1"))
data$michart_chemosched___5.factor = factor(data$michart_chemosched___5,levels=c("0","1"))
data$michart_chemosched___6.factor = factor(data$michart_chemosched___6,levels=c("0","1"))
data$michart_chemosched___7.factor = factor(data$michart_chemosched___7,levels=c("0","1"))
data$michart_oralyn.factor = factor(data$michart_oralyn,levels=c("1","2","3"))
data$michart_firstvis.factor = factor(data$michart_firstvis,levels=c("1","2"))
data$chart_review_for_patient_pref_complete.factor = factor(data$chart_review_for_patient_pref_complete,levels=c("0","1","2"))

levels(data$demo_commdescrp.factor)=c("Urban","Suburban","Rural","Unsure")
levels(data$demo_traveltime.factor)=c("Less than 30 minutes","30 minutes to 1 hour","1 to 2 hours","2 to 4 hours","Greater than 4 hours")
levels(data$demo_traveldist.factor)=c("Less than 20 miles","Between 20 and 50 miles","Between 50 and 100 miles","More than 100 miles","Unsure")
levels(data$demo_travelmethod___1.factor)=c("Unchecked","Checked")
levels(data$demo_travelmethod___2.factor)=c("Unchecked","Checked")
levels(data$demo_travelmethod___3.factor)=c("Unchecked","Checked")
levels(data$demo_travelmethod___4.factor)=c("Unchecked","Checked")
levels(data$demo_travelmethod___5.factor)=c("Unchecked","Checked")
levels(data$demo_travelmethod___6.factor)=c("Unchecked","Checked")
levels(data$demo_workstatus.factor)=c("I have a full-time job","I have a part-time job","I am a student","I am a full-time parent and/or caregiver","I am not employed/not retired","I am retired")
levels(data$demo_income.factor)=c("Less than $25,000","$25,000 - $50,000","$50,000 - $100,000","$100,000 - $200,000","More than $200,000","Prefer not to say")
levels(data$demo_educ.factor)=c("Some High School","High School","Some college","Associates Degree","Bachelors Degree","Masters Degree or higher")
levels(data$demo_insurance___1.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___2.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___3.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___4.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___5.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___6.factor)=c("Unchecked","Checked")
levels(data$demo_insurance___7.factor)=c("Unchecked","Checked")
levels(data$demo_dep.factor)=c("Yes","No")
levels(data$tech_firstmeet.factor)=c("Virtual","In-Person","I cant recall")
levels(data$tech_device.factor)=c("Personal phone","Personal computer","Personal tablet","Shared phone","Shared tablet","Shared computer")
levels(data$tech_device_share.factor)=c("Very Difficult","Difficult","Neutral","Easy","Very Easy")
levels(data$tech_comfortlvl.factor)=c("Very Uncomfortable","Uncomfortable","Neutral","Comfortable","Very Comfortable")
levels(data$tech_vidqual.factor)=c("Poor","Fair","Good","Very good","Excellent")
levels(data$tech_voicequal.factor)=c("Poor","Fair","Good","Very good","Excellent")
levels(data$tech_difficulties___1.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___2.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___3.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___4.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___5.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___6.factor)=c("Unchecked","Checked")
levels(data$tech_difficulties___7.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___1.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___2.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___3.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___4.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___5.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___6.factor)=c("Unchecked","Checked")
levels(data$tech_preventusage___7.factor)=c("Unchecked","Checked")
levels(data$inpersmatrix_001.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_002.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_003.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_004.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_005.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_006.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_007.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_008.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpersmatrix_009.factor)=c("Poor","Fair","Good","Very   Good","Excellent")
levels(data$inpers_overall.factor)=c("Poor","Fair","Good","Very good","Excellent")
levels(data$virtualmatrix_001.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_002.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_003.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_004.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_005.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_006.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_007.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_008.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtualmatrix_009.factor)=c("Poor","Fair","Good","Very  Good","Excellent")
levels(data$virtual_overall.factor)=c("Poor","Fair","Good","Very good","Excellent")
levels(data$inpersonlymatrix_001.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$inpersonlymatrix_002.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$inpersonlymatrix_003.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$inpersonlymatrix_004.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$inpersonlymatrix_005.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$inpersonlymatrix_006.factor)=c("Strongly   Disagree","Disagree","Neither  Agree or  Disagree","Agree","Strongly  Agree")
levels(data$visitpref_recc.factor)=c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely")
levels(data$visitpref_choice.factor)=c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely")
levels(data$visitpref_visitchoice.factor)=c("In-person","Virtual","No preference")
levels(data$visitpref_ratio.factor)=c("All visits are in-person","All visits are virtual","Some visits are in-person and some visits are virtual")
levels(data$schedule_var_choice.factor)=c("Option 1","Option 2","Option 3","Option 4","Option 5","Option 6","No Preference","Prefer Not To Say")
levels(data$optionsched_pics.factor)=c("","","","","","","No Preference","Prefer Not To Answer")
levels(data$patient_preference_survey_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$michart_gender.factor)=c("Female","Transgender","Non-binary/non-conforming","Other")
levels(data$michart_race.factor)=c("American Indian or Alaska Native","Asian","Black or African American","Native Hawaiian or Other Pacific Islander","White","Other")
levels(data$michart_eth.factor)=c("Hispanic","Non-Hispanic")
levels(data$michart_ecog.factor)=c("0","1","2","3","4","5")
levels(data$michart_prim_site.factor)=c("Cervical","Ovarian","Uterine","Vaginal","Vulvar","Gestational trophoblastic disease")
levels(data$michart_figostage.factor)=c("I","II","III","IV","Unstaged")
levels(data$michart_prim_recurr.factor)=c("Primary","Recurrent","Refractory (i.e. platinum resistant)")
levels(data$ds.factor)=c("No evidence of disease","Active disease","Recurrent disease, recently progressed","Recurrent disease, stable")
levels(data$michart_chemosched___1.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___2.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___3.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___4.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___5.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___6.factor)=c("Unchecked","Checked")
levels(data$michart_chemosched___7.factor)=c("Unchecked","Checked")
levels(data$michart_oralyn.factor)=c("History of chemotherapy infusions only","History of oral treatment only","History of both infusion and oral treatment")
levels(data$michart_firstvis.factor)=c("Before COVID-19","After COVID-19")
levels(data$chart_review_for_patient_pref_complete.factor)=c("Incomplete","Unverified","Complete")

#clean up
data<- data |>
  mutate(insurance = case_when(demo_insurance_1==1 ~ "Medicaid",
                               demo_insurance_2==1 ~ "Medicare",
                               demo_insurance_4==1 ~ "Private",
                               demo_insurance_5==1 ~ "Uninsured",
                               demo_insurance_7==1 ~ "Other"),
         demo_commdescrp = factor(demo_commdescrp,
                                  levels=c(1:4),
                                  labels=c("Urban","Suburban","Rural","Unsure")),
         demo_traveltime = factor(demo_traveltime,
                                  levels=c(1:5),
                                  labels=c("Less than 30 minutes","30 minutes to 1 hour","1 to 2 hours","2 to 4 hours","Greater than 4 hours")),
         demo_time1 = case_when(demo_traveltime=="Less than 30 minutes" ~ "< 30 mins",
                                demo_traveltime=="30 minutes to 1 hour" ~ "> 30 mins",
                                demo_traveltime=="1 to 2 hours" ~ "> 30 mins",
                                demo_traveltime=="2 to 4 hours" ~ "> 30 mins",
                                demo_traveltime=="Greater than 4 hours" ~ "> 30 mins"),
         demo_time2 = case_when(demo_traveltime=="Less than 30 minutes" ~ "Less than 30 minutes",
                                demo_traveltime=="30 minutes to 1 hour" ~ "30 minutes to 2 hours",
                                demo_traveltime=="1 to 2 hours" ~ "30 minutes to 2 hours",
                                demo_traveltime=="2 to 4 hours" ~ "Greater than 2 hours",
                                demo_traveltime=="Greater than 4 hours" ~ "Greater than 2 hours"),
         demo_traveldist = factor(demo_traveldist,
                                  levels=c(1:5),
                                  labels=c("Less than 20 miles","Between 20 and 50 miles","Between 50 and 100 miles","More than 100 miles","Unsure")),
         demo_dist1 = case_when(demo_traveldist=="Less than 20 miles" ~ "< 20 miles",
                                demo_traveldist=="Between 20 and 50 miles" ~ "> 20 miles",
                                demo_traveldist=="Between 50 and 100 miles" ~ "> 20 miles",
                                demo_traveldist=="More than 100 miles" ~ "> 20 miles",
                                demo_traveldist=="Unsure" ~ "Unsure"),
         demo_dist2 = case_when(demo_traveldist=="Less than 20 miles" ~ "Less than 20 miles",
                                demo_traveldist=="Between 20 and 50 miles" ~ "Between 20 and 100 miles",
                                demo_traveldist=="Between 50 and 100 miles" ~ "Between 20 and 100 miles",
                                demo_traveldist=="More than 100 miles" ~ "More than 100 miles",
                                demo_traveldist=="Unsure" ~ NA),
         demo_workstatus = factor(demo_workstatus,
                                  levels=c(1:6),
                                  labels=c("Full-time job","Part-time job","Student","Full-time parent and/or caregiver","Not employed","Retired")),
         demo_income = factor(demo_income,
                                  levels=c(1:6),
                                  labels=c("Less than $25,000","$25,000 - $50,000","$50,000 - $100,000","$100,000 - $200,000","More than $200,000","Prefer not to say")),
         demo_educ = factor(demo_educ,
                              levels=c(1:6),
                              labels=c("Some High School","High School","Some college","Associates Degree","Bachelors Degree","Masters Degree or higher")),
         tech_device = factor(tech_device,
                              levels=c(1:6),
                              labels=c("Personal phone","Personal computer","Personal tablet","Shared phone","Shared tablet","Shared computer")),
         tech_device_share = factor(tech_device_share,
                                    levels=c(1:5),
                                    labels=c("Very Difficult","Difficult","Neutral","Easy","Very Easy")),
         
         tech_comfortlvl = factor(tech_comfortlvl,
                                  levels=c(1:5),
                                  labels=c("Very Uncomfortable","Uncomfortable","Neutral","Comfortable","Very Comfortable")),
         tech_vidqual = factor(tech_vidqual,
                               levels=c(1:5),
                               labels=c("Poor","Fair","Good","Very good","Excellent")),
         tech_voicequal = factor(tech_voicequal,
                                levels=c(1:5),
                                labels=c("Poor","Fair","Good","Very good","Excellent")),
         tech_diff = case_when(tech_difficulties_1==1 ~ "Internet connectivity",
                          tech_difficulties_2==1 ~ "Logging into Visit",
                          tech_difficulties_3==1 ~ "Battery/Power Source for Devices",
                          tech_difficulties_4==1 ~ "Poor Audio Quality",
                          tech_difficulties_5==1 ~ "Poor Video Quality",
                          tech_difficulties_6==1 ~ "Audio or video delays/issues",
                          tech_difficulties_7==1 ~ "I have not experienced technical difficulties"),
         tech_use = case_when(tech_preventusage_1==1 ~ "Devices are too expensive",
                              tech_preventusage_2==1 ~ "There is no signal or poor signal where I live",
                              tech_preventusage_3==1 ~ "Paying for internet/data is too expensive",
                              tech_preventusage_4==1 ~ "It's too difficult to use",
                              tech_preventusage_5==1 ~ "I am worried about my privacy",
                              tech_preventusage_6==1 ~ "Other",
                              tech_preventusage_7==1 ~ "None of these"))|>
  mutate(tech_diff1 = case_when(tech_difficulties_1==1 ~ "Technical difficulties",
         tech_difficulties_2==1 ~ "Technical difficulties",
         tech_difficulties_3==1 ~ "Technical difficulties",
         tech_difficulties_4==1 ~ "Technical difficulties",
         tech_difficulties_5==1 ~ "Technical difficulties",
         tech_difficulties_6==1 ~ "Technical difficulties",
         tech_difficulties_7==1 ~ "Not experienced technical difficulties"),
      tech_use1 = case_when(tech_preventusage_1==1 ~ "Difficulties",
                     tech_preventusage_2==1 ~ "Difficulties",
                     tech_preventusage_3==1 ~ "Difficulties",
                     tech_preventusage_4==1 ~ "Difficulties",
                     tech_preventusage_5==1 ~ "Difficulties",
                     tech_preventusage_6==1 ~ "Difficulties",
                     tech_preventusage_7==1 ~ "None of these"))
  
label(data$demo_commdescrp)="Community"
label(data$demo_traveltime)="Travel time from home to gynecologic oncologists office"
label(data$demo_traveldist)="Travel distance from home to gynecologic oncologists office"
label(data$demo_time1)="Travel time from home to gynecologic oncologists office"
label(data$demo_dist1)="Travel distance from home to gynecologic oncologists office"
label(data$demo_time2)="Travel time from home to gynecologic oncologists office"
label(data$demo_dist2)="Travel distance from home to gynecologic oncologists office"
label(data$insurance)="Insurance"         
label(data$demo_workstatus)="Employment"
label(data$demo_income)="Annual household income"
label(data$demo_educ)="Education level"                
label(data$demo_dep)="Have children or dependent at home"
label(data$tech_firstmeet)="Was your first time meeting your gynecologic oncologist in-person or virtual? "
label(data$tech_device)="Type of device in virtual visits"
label(data$tech_device_share)="Experience getting access to your shared device"
label(data$tech_comfortlvl)="Comfortable level using technology to participate in virtual visits"
label(data$tech_vidqual)="Video quality during virtual visit"
label(data$tech_voicequal)="Voice quality during virtual visit"
label(data$tech_diff)="Technology difficulties"  
label(data$tech_use)="Technology factors limit or prevent use of virtual care"
label(data$tech_diff1)="Technology difficulties"  
label(data$tech_use1)="Technology factors limit or prevent use of virtual care"         
         
        

data <- data |>
  mutate(inpersmatrix_001=factor(inpersmatrix_001,
                              levels=c(1:5),
                              labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_002=factor(inpersmatrix_002,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_003=factor(inpersmatrix_003,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_004=factor(inpersmatrix_004,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_005=factor(inpersmatrix_005,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_006=factor(inpersmatrix_006,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_007=factor(inpersmatrix_007,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_008=factor(inpersmatrix_008,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersmatrix_009=factor(inpersmatrix_009,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpers_overall=factor(inpers_overall,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_001=factor(virtualmatrix_001,
                                  levels=c(1:5),
                                  labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_002=factor(virtualmatrix_002,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_003=factor(virtualmatrix_003,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_004=factor(virtualmatrix_004,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_005=factor(virtualmatrix_005,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_006=factor(virtualmatrix_006,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_007=factor(virtualmatrix_007,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_008=factor(virtualmatrix_008,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtualmatrix_009=factor(virtualmatrix_009,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         virtual_overall=factor(virtual_overall,
                                   levels=c(1:5),
                                   labels=c("Poor","Fair","Good","Very Good","Excellent")),
         inpersonlymatrix_001=factor(inpersonlymatrix_001,
                                   levels=c(1:5),
                                   labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         inpersonlymatrix_002=factor(inpersonlymatrix_002,
                                      levels=c(1:5),
                                      labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         inpersonlymatrix_003=factor(inpersonlymatrix_003,
                                      levels=c(1:5),
                                      labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         inpersonlymatrix_004=factor(inpersonlymatrix_004,
                                      levels=c(1:5),
                                      labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         inpersonlymatrix_005=factor(inpersonlymatrix_005,
                                      levels=c(1:5),
                                      labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         inpersonlymatrix_006=factor(inpersonlymatrix_006,
                                      levels=c(1:5),
                                      labels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree")),
         visitpref_recc=factor(visitpref_recc,
                                      levels=c(1:5),
                                      labels=c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely")),
         visitpref_choice=factor(visitpref_choice,
                                levels=c(1:5),
                                labels=c("Very Likely","Likely","Neutral","Unlikely","Very Unlikely")),
         
         visitpref_visitchoice=factor(visitpref_visitchoice,
                                   levels=c(1:3),
                                   labels=c("In-person","Virtual","No preference")),
         visitpref_ratio=factor(visitpref_ratio,
                                       levels=c(1:3),
                                       labels=c("All visits are in-person","All visits are virtual","Some visits are in-person and some visits are virtual")),
         schedule_var_choice=factor(schedule_var_choice,
                                 levels=c(1:8),
                                 labels=c("Option 1","Option 2","Option 3","Option 4","Option 5","Option 6","No Preference","Prefer Not To Say")))
         
data <- data |>
    mutate(group=case_when(slidingscale_pref ==0 ~ "0 cycles",
                           slidingscale_pref <=2 ~ "1-2 cycles",
                           slidingscale_pref <=4 ~ "2-3 cycles",
                           slidingscale_pref <=6 ~ "3-4 cycles",
                           slidingscale_pref <=8 ~ "4-5 cycles",
                           slidingscale_pref <=10 ~ "5-6 cycles"))|>
    mutate(michart_race=factor(michart_race,
                               levels=c(2,3,5,6),
                               labels=c("Asian","Black or African American","White","Other")),
           michart_ecog=factor(michart_ecog,
                               levels=c(0:2),
                               labels=c("0","1","2")),
           michart_prim_site=factor(michart_prim_site,
                               levels=c(1:6),
                               labels=c("Cervical","Ovarian","Uterine","Vaginal","Vulvar","Gestational trophoblastic disease")),
           michart_figostage=factor(michart_figostage,
                                    levels=c(1:5),
                                    labels=c("I","II","III","IV","Unstaged")),
           michart_prim_recurr=factor(michart_prim_recurr,
                                    levels=c(1:3),
                                    labels=c("Primary","Recurrent","Refractory (i.e. platinum resistant)")),
           ds=factor(ds,
                                      levels=c(1:4),
                                      labels=c("No evidence of disease","Active disease","Recurrent disease, recently progressed","Recurrent disease, stable")),
           michart_oralyn=factor(michart_oralyn,
                                      levels=c(1:3),
                                      labels=c("History of chemotherapy infusions only","History of oral treatment only","History of both infusion and oral treatment")),
           michart_firstvis=factor(michart_firstvis,
                                 levels=c(1:2),
                                 labels=c("Before COVID-19","After COVID-19")))
data <- data |>
  mutate(goal=case_when(michart_chemosched_1 == 1 ~ "Neoadjuvant",
                        michart_chemosched_2 == 1 ~ "Adjuvant",
                          michart_chemosched_3 == 1 ~ "Maintenance",
                          michart_chemosched_4 == 1 ~ "Control",
                          michart_chemosched_5 == 1 ~ "Curative",
                          michart_chemosched_6 == 1 ~ "Palliative",
                          michart_chemosched_7 == 1 ~ "Not currently undergoing chemotherapy"))|>
  mutate(goal1=case_when(michart_chemosched_1 == 1 ~ "Not curative",
                        michart_chemosched_2 == 1 ~ "Not curative",
                        michart_chemosched_3 == 1 ~ "Not curative",
                        michart_chemosched_4 == 1 ~ "Not curative",
                        michart_chemosched_5 == 1 ~ "Curative",
                        michart_chemosched_6 == 1 ~ "Not curative",
                        michart_chemosched_7 == 1 ~ "Not currently undergoing chemotherapy"))
  
label(data$goal)="Treatment goal of chemotherapy regimen"
label(data$goal1)="Treatment goal of chemotherapy regimen"
data <- data |>
  mutate(group1=case_when(slidingscale_pref <5 ~ "in-person",
                          slidingscale_pref >=5 ~ "virtual"))
export(data, "data.rds")
data <- data |>
  mutate(agenew=case_when(michart_dob <65 ~ "<65",
                          michart_dob >=65 ~ ">=65"))
#table1 Demo
data |>
  select(group1,michart_dob, michart_race, insurance, demo_commdescrp, demo_workstatus,
         demo_income,demo_educ, demo_dep, demo_dist2, demo_time2)|>
  mutate(michart_dob=as.numeric(michart_dob))|>
  tbl_summary(by=group1,
              label = c(michart_dob ~ "Age",
                        michart_race ~ "Race"),
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table 1. Demographic Characteristics**") 
#11-02-23 age (categorical)
data |>
  select(group1,agenew, michart_race, insurance, demo_commdescrp, demo_workstatus,
         demo_income,demo_educ, demo_dep, demo_dist2, demo_time2)|>
  tbl_summary(by=group1,
              label = c(agenew ~ "Age",
                        michart_race ~ "Race"),
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table 1. Demographic Characteristics**") 
#table2 technology
data |>
  select(group1,tech_device, tech_comfortlvl,tech_diff1, tech_use1)|>
  tbl_summary(by=group1,
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table 2. Technology Characteristics**") 
#table3 clinical
data |>
    select(group1,michart_bmi,
           michart_ecog, michart_figostage, michart_numcycle, michart_firstvis, 
           michart_oralyn, michart_prim_recurr, michart_prim_site, goal1, michart_totalnumvirtual)|>
    mutate(michart_numcycle=as.numeric(michart_numcycle))|>
    mutate(michart_totalnumvirtual=as.numeric(michart_totalnumvirtual))|>
    tbl_summary(by=group1,
                label = c(michart_ecog ~ "ECOG Performance Status",
                          michart_figostage ~ "Stage",
                          michart_firstvis ~ "First visit before or after start of COVID-19 pandemic (1/31/2020)",
                          michart_oralyn ~ "History of prior chemotherapy regimens",
                          michart_prim_recurr ~ "Type of disease at the time of review",
                          michart_prim_site ~ "Primary Site",
                          michart_numcycle ~ "Number of prior cycles of chemotherapy at time of review",
                          michart_totalnumvirtual ~ "Total number of Gyn Virtual Visits at time of review"),
                digits = list(all_categorical() ~ c(0, 1)))|>
    add_p() |>
    modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
    modify_caption("**Table 3. Clinical Characteristics**") 
#11-02-23 barriers
data |>
  select(group1,inpersonlymatrix_001, inpersonlymatrix_002,inpersonlymatrix_003,
         inpersonlymatrix_004,inpersonlymatrix_005,inpersonlymatrix_006)|>
  tbl_summary(by=group1,
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table . Barriers**") 
#01-05-24 barriers
data <- data |>
  mutate(inpersonlymatrix_001 = as.character(inpersonlymatrix_001))|>
  mutate(inpersonlymatrix_002 = as.character(inpersonlymatrix_002))|>
  mutate(inpersonlymatrix_003 = as.character(inpersonlymatrix_003))|>
  mutate(inpersonlymatrix_004 = as.character(inpersonlymatrix_004))|>
  mutate(inpersonlymatrix_005 = as.character(inpersonlymatrix_005))|>
  mutate(inpersonlymatrix_006 = as.character(inpersonlymatrix_006))|>
  mutate(tech_comfortlvl = as.character(tech_comfortlvl))|>
  mutate(inperson1=case_when(inpersonlymatrix_001 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_001 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_001),
         inperson2=case_when(inpersonlymatrix_002 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_002 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_002),
         inperson3=case_when(inpersonlymatrix_003 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_003 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_003),
         inperson4=case_when(inpersonlymatrix_004 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_004 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_004),
         inperson5=case_when(inpersonlymatrix_005 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_005 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_005),
         inperson6=case_when(inpersonlymatrix_006 %in% c("Strongly Disagree", "Disagree") ~ "Disagree",
                             inpersonlymatrix_006 %in% c("Strongly Agree", "Agree") ~ "Agree",
                             TRUE ~ inpersonlymatrix_006),
         tech_comfortlvlnew=case_when(tech_comfortlvl %in% c("Very Uncomfortable", "Uncomfortable") ~ "Uncomfortable",
                                      tech_comfortlvl %in% c("Very Comfortable", "Comfortable") ~ "Comfortable",
                                      TRUE ~ tech_comfortlvl))

data$inperson1 <- forcats::fct_relevel(data$inperson1, "Disagree", "Neither Agree or Disagree", "Agree")
data$inperson2 <- forcats::fct_relevel(data$inperson2, "Disagree", "Neither Agree or Disagree", "Agree")
data$inperson3 <- forcats::fct_relevel(data$inperson3, "Disagree", "Neither Agree or Disagree", "Agree")
data$inperson4 <- forcats::fct_relevel(data$inperson4, "Disagree", "Neither Agree or Disagree", "Agree")
data$inperson5 <- forcats::fct_relevel(data$inperson5, "Disagree", "Neither Agree or Disagree", "Agree")
data$inperson6 <- forcats::fct_relevel(data$inperson6, "Disagree", "Neither Agree or Disagree", "Agree")
data$tech_comfortlvlnew <- forcats::fct_relevel(data$tech_comfortlvlnew, "Uncomfortable", "Neutral", "Comfortable")
label(data$inperson1)="Having a physical exam (e.g., listening to heart and lungs) performed during my appointment is important."
label(data$inperson2)="Having a pelvic exam (e.g., vaginal exam) performed during my appointment is important."
label(data$inperson3)="Non-verbal communication between me and my doctor is important (i.e. eye contact, shaking hands, body language)."
label(data$inperson4)="I am concerned about being exposed to an infection and getting sick."
label(data$inperson5)="The time it takes me to travel to the clinic is a burden to me."
label(data$inperson6)="Travel expenses (e.g, gas, parking fees) are a burden to me."
label(data$tech_comfortlvlnew)="Comfortable level using technology to participate in virtual visits"
data |>
  select(group1,inperson1, inperson2,inperson3,
         inperson4,inperson5,inperson6, tech_comfortlvlnew)|>
  tbl_summary(by=group1,
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table . Barriers**") 
###########################################################################additional 
data |>
  select(group,michart_dob)|>
  mutate(michart_dob=as.numeric(michart_dob))|>
  tbl_summary(              label = michart_dob ~ "Age",
            digits = list(all_categorical() ~ c(0, 1)))|>
  add_p()
#graph
#box plot
data |>
  ggplot(aes(x=group, y=michart_dob))+
  geom_boxplot()+
  labs(x=NULL, y="Age") +
  theme_classic()
#scatter plot
data |>
  ggplot(aes(x=group, y=michart_dob)) +
  geom_point() +
  labs(x=NULL, y="Age") +
  theme_classic()
#04/02/2024 add residential class
ruca <- Response_to_reviewers|>
  janitor::clean_names()

typeof(data$record_id)#data type
data$record_id<- as.numeric(data$record_id)
data<- left_join(data, ruca, by="record_id")
#table1 Demo
data |>
  select(group1,residential_class)|>
  tbl_summary(by=group1,
              digits = list(all_categorical() ~ c(0, 1)))|>
  add_p() |>
  modify_spanning_header(all_stat_cols() ~ "**Patient Preference**") |>
  modify_caption("**Table 1. Demographic Characteristics**") 
