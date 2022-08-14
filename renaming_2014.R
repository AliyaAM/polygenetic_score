
library(dplyr)


#add anxiety to all years 
#add depression new bin to all years (NEW)
#need to add CVD to all years 
#check smoking 
#add PTSD for all years  
#add Alzheimer's for all years   
#add kidney disease for all years    
#limiting longstanding condition (recode to bin)




directory = "/Users/aliya/my_docs"

#"/Users/aliyaamirova/"

DATA_ROOT = "/KCL_postDoc/Data_analysis/"
#SOURCE_ROOT = "/proj/Cumulative_effects_HRS/Version_2_analysis/"

#directory = "/Users/aliyaamirova/Documents/KCL_postDoc/"

#SOURCE_ROOT = "Data_analysis/"
#OUTPUT_ROOT = "Data_analysis/"

HRS_2014_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))

HRS_2014 = data.frame(HRS_2014_data$HHIDPN)

print("add all_HRS_by_years_PGS: arthritis_new,  PTSD, anxiety, alcohol dependence,  smoking cessation, smoking initiation, sig per day, drinks per day" )

HRS_2014$HRS2014_Percvd_cntrl_health = HRS_2014_data$Percvd_cntrl_health 


HRS_2014$HRS2014_self_rated_health = HRS_2014_data$self_rated_health
HRS_2014$HRS2014_life_satisfaction = HRS_2014_data$HRS2014_life_satisfaction 
HRS_2014$HRS2014_alzheimer_bin = HRS_2014_data$HRS2014_alzheimer_bin
HRS_2014$HRS2014_angina_new_bin = HRS_2014_data$HRS2014_angina2yrs_bin

# HRS_2014$HRS2014_arthritis_new = HRS_2014_data$HRS2014_arthritis_new
# unique(HRS_2014$HRS2014_arthritis_new)
# HRS_2014$HRS2014_arthritis_new_bin = case_when(HRS_2014$HRS2014_arthritis_new == 0 ~ 0, 
#                                                HRS_2014$HRS2014_arthritis_new == 1 ~ 1)
# unique(HRS_2014$HRS2014_arthritis_new_bin)


HRS_2014$HRS2014_hypertension_new =  HRS_2014_data$HRS2014_hypertension_new 
unique(HRS_2014$HRS2014_hypertension_new)
HRS_2014$HRS2014_hypertension_new_bin = case_when(HRS_2014$HRS2014_hypertension_new == 0 ~ 0,
                                                  HRS_2014$HRS2014_hypertension_new == 1 ~ 1)

HRS_2014$HRS2014_mi = HRS_2014_data$HRS2014_heartattack2yrs_bin
HRS_2014$HRS2014_depression_bin = HRS_2014_data$HRS2014_depression_bin


HRS_2014$HRS2014_alcohol_days_week = HRS_2014_data$alcohol_days_week
HRS_2014$HRS2014_LGB_2014 = HRS_2014_data$LGB_2014
HRS_2014$HRS2014_marital_status = HRS_2014_data$marital_status2014
HRS_2014$HRS2014_married_bin = HRS_2014_data$married2014_bin
HRS_2014$HRS2014_national_origin_ousideUS = HRS_2014_data$national_origin_ousideUS
HRS_2014$HRS2014_separated_bin = HRS_2014_data$separated2014_bin
HRS_2014$HRS2014_sexual_orientation = HRS_2014_data$sexual_orientation2014
HRS_2014$HRS2014_smokes_ever = HRS_2014_data$smokes_ever
HRS_2014$HRS2014_smokes_ever_bin = HRS_2014_data$smokes_ever_bin
HRS_2014$HRS2014_smokes_now = HRS_2014_data$smokes_now
HRS_2014$HRS2014_smokes_now_bin = HRS_2014_data$mokes_now_bin
HRS_2014$HRS2014_start = HRS_2014_data$start
HRS_2014$HRS2014_stop = HRS_2014_data$stop
HRS_2014$HRS2014_Straight_2014 = HRS_2014_data$Straight_2014
HRS_2014$HRS2014_summary_mean_score_discrim = HRS_2014_data$summary_mean_score_discrim
HRS_2014$HRS2014_summary_mean_score_discrim_bin = HRS_2014_data$summary_mean_score_discrim_bin 
HRS_2014$HRS2014_vigarious_physical_activity = HRS_2014_data$vigarious_physical_activity
HRS_2014$HRS2014_wealth_noIRA = HRS_2014_data$wealth_noIRA
HRS_2014$HRS2014_widowed_bin = HRS_2014_data$widowed2014_bin
HRS_2014$HRS2014_yearsof_education = HRS_2014_data$yearsof_education2014
HRS_2014$HRS2014_diabetes_ever = HRS_2014_data$diabetes_ever
HRS_2014$HRS2014_diabetes_new = HRS_2014_data$diabetes_new
HRS_2014$HRS2014_continious_age = HRS_2014_data$continious_age
HRS_2014$HRS2014_angina_new_bin = HRS_2014_data$angina_new_bin
HRS_2014$HRS2014_number_reasons_discrimination = HRS_2014_data$number_reasons_discrimination
HRS_2014$HRS2014_race_white = HRS_2014_data$race_white
HRS_2014$HRS2014_religion_bin = HRS_2014_data$
  HRS_2014$HRS2014_sex_1_0 = HRS_2014_data$sex_1_0_2014
HRS_2014$HRS2014_sex_1_2 = HRS_2014_data$sex_1_2_2014
HRS_2014$HRS2014_annual_income_self_employment = HRS_2014_data$annual_income_self_employment2014
HRS_2014$HRS2014_never_married_bin = HRS_2014_data$never_married2014_bin
HRS_2014$HRS2014_angina_ever = HRS_2014_data$HRS2014_angina_ever
HRS_2014$HRS2014_angina_new = HRS_2014_data$HRS2014_angina_new
HRS_2014$HRS2014_angina2yrs_bin = HRS_2014_data$HRS2014_angina2yrs_bin
HRS_2014$HRS2014_BMI = HRS_2014_data$HRS2014_BMI
HRS_2014$HRS2014_BMI_cat = HRS_2014_data$HRS2014_BMI_cat
HRS_2014$HRS2014_BMI_category = HRS_2014_data$HRS2014_BMI_category
HRS_2014$HRS2014_checklist_depression_bin = HRS_2014_data$HRS2014_checklist_depression_bin
HRS_2014$HRS2014_discrim_afraidothers = HRS_2014_data$HRS2014_discrim_afraidothers
HRS_2014$HRS2014_discrim_afraidothers_bin = HRS_2014_data$HRS2014_discrim_afraidothers_bin 
HRS_2014$HRS2014_discrim_harassed = HRS_2014_data$HRS2014_discrim_harassed
HRS_2014$HRS2014_discrim_harassed_bin = HRS_2014_data$HRS2014_discrim_harassed_bin
HRS_2014$HRS2014_discrim_lessrespect = HRS_2014_data$HRS2014_discrim_lessrespect
HRS_2014$HRS2014_discrim_lessrespect_bin = HRS_2014_data$HRS2014_discrim_lessrespect_bin
HRS_2014$HRS2014_discrim_medical = HRS_2014_data$HRS2014_discrim_medical
HRS_2014$HRS2014_discrim_medical_bin = HRS_2014_data$HRS2014_discrim_medical_bin
HRS_2014$HRS2014_discrim_notclever = HRS_2014_data$HRS2014_discrim_notclever
HRS_2014$HRS2014_discrim_notclever_bin = HRS_2014_data$HRS2014_discrim_notclever_bin
HRS_2014$HRS2014_discrim_poorerservice = HRS_2014_data$HRS2014_discrim_poorerservice
HRS_2014$HRS2014_discrim_poorerservice_bin = HRS_2014_data$HRS2014_discrim_poorerservice_bin
HRS_2014$HRS2014_height_feet = HRS_2014_data$HRS2014_height_feet
HRS_2014$HRS2014_height_inches = HRS_2014_data$HRS2014_height_inches
HRS_2014$HRS2014_height_meters = HRS_2014_data$HRS2014_height_meters
HRS_2014$HRS2014_limiting_condition_bin = HRS_2014_data$HRS2014_limiting_condition_bin
HRS_2014$HRS2014_normalweight_bin = HRS_2014_data$HRS2014_normalweight_bin
HRS_2014$HRS2014_overweight_bin = HRS_2014_data$HRS2014_overweight_bin
HRS_2014$HRS2014_race = HRS_2014_data$HRS2014_race
HRS_2014$HRS2014_race_black = HRS_2014_data$HRS2014_race_black
HRS_2014$HRS2014_race_hispanic_latino = HRS_2014_data$HRS2014_race_hispanic_latino
HRS_2014$HRS2014_race_nonwhite = HRS_2014_data$HRS2014_race_nonwhite
HRS_2014$HRS2014_race_white = HRS_2014_data$HRS2014_race_white
HRS_2014$HRS2014_reason_discrim1 = HRS_2014_data$HRS2014_reason_discrim1
HRS_2014$HRS2014_reason_discrim1_reason_age = HRS_2014_data$HRS2014_reason_discrim1_reason_age
HRS_2014$HRS2014_reason_discrim1_reason_disability = HRS_2014_data$HRS2014_reason_discrim1_reason_disability
HRS_2014$HRS2014_reason_discrim1_reason_financial = HRS_2014_data$HRS2014_reason_discrim1_reason_financial
HRS_2014$HRS2014_reason_discrim1_reason_gender = HRS_2014_data$HRS2014_reason_discrim1_reason_gender
HRS_2014$HRS2014_reason_discrim1_reason_national = HRS_2014_data$HRS2014_reason_discrim1_reason_national
HRS_2014$HRS2014_reason_discrim1_reason_otherreason = HRS_2014_data$HRS2014_reason_discrim1_reason_otherreason
HRS_2014$HRS2014_reason_discrim1_reason_race = HRS_2014_data$HRS2014_reason_discrim1_reason_race
HRS_2014$HRS2014_reason_discrim1_reason_religion = HRS_2014_data$HRS2014_reason_discrim1_reason_religion
HRS_2014$HRS2014_reason_discrim1_reason_sexuality = HRS_2014_data$HRS2014_reason_discrim1_reason_sexuality
HRS_2014$HRS2014_reason_discrim1_reason_weight = HRS_2014_data$HRS2014_reason_discrim1_reason_weight
HRS_2014$HRS2014_underweight_bin = HRS_2014_data$HRS2014_underweight_bin
HRS_2014$HRS2014_weight_kg = HRS_2014_data$HRS2014_weight_kg
HRS_2014$HRS2014_weight_pounds = HRS_2014_data$HRS2014_weight_pounds
HRS_2014$HRS2014_obese_bin = HRS_2014_data$HRS2014obese_bin




HRS_2014$HRS2014_discrim_harassed_bin = case_when(HRS_2014$HRS2014_discrim_harassed == 1 ~ 1, 
                                                  HRS_2014$HRS2014_discrim_harassed == 2 ~ 1, 
                                                  HRS_2014$HRS2014_discrim_harassed == 3 ~ 1, 
                                                  HRS_2014$HRS2014_discrim_harassed == 4 ~ 1, 
                                                  HRS_2014$HRS2014_discrim_harassed == 5 ~ 0, 
                                                  HRS_2014$HRS2014_discrim_harassed == 6 ~ 0,
                                                  HRS_2014$HRS2014_discrim_harassed == 0 ~ 0) 



HRS_2014$HRS2014_discrim_lessrespect_bin = case_when(HRS_2014$HRS2014_discrim_lessrespect == 1 ~ 1, 
                                                     HRS_2014$HRS2014_discrim_lessrespect == 2 ~ 1, 
                                                     HRS_2014$HRS2014_discrim_lessrespect == 3 ~ 1, 
                                                     HRS_2014$HRS2014_discrim_lessrespect == 4 ~ 1, 
                                                     HRS_2014$HRS2014_discrim_lessrespect == 5 ~ 0, 
                                                     HRS_2014$HRS2014_discrim_lessrespect == 6 ~ 0,
                                                     HRS_2014$HRS2014_discrim_lessrespect == 0 ~ 0) 



HRS_2014$HRS2014_discrim_medical_bin = case_when(HRS_2014$HRS2014_discrim_medical == 1 ~ 1, 
                                                 HRS_2014$HRS2014_discrim_medical == 2 ~ 1, 
                                                 HRS_2014$HRS2014_discrim_medical == 3 ~ 1, 
                                                 HRS_2014$HRS2014_discrim_medical == 4 ~ 1, 
                                                 HRS_2014$HRS2014_discrim_medical == 5 ~ 0, 
                                                 HRS_2014$HRS2014_discrim_medical == 6 ~ 0,
                                                 HRS_2014$HRS2014_discrim_medical == 0 ~ 0) 





HRS_2014$HRS2014_discrim_notclever_bin = case_when(HRS_2014$HRS2014_discrim_notclever == 1 ~ 1, 
                                                   HRS_2014$HRS2014_discrim_notclever == 2 ~ 1, 
                                                   HRS_2014$HRS2014_discrim_notclever == 3 ~ 1, 
                                                   HRS_2014$HRS2014_discrim_notclever == 4 ~ 1, 
                                                   HRS_2014$HRS2014_discrim_notclever == 5 ~ 0, 
                                                   HRS_2014$HRS2014_discrim_notclever == 6 ~ 0,
                                                   HRS_2014$HRS2014_discrim_notclever == 0 ~ 0) 






HRS_2014$HRS2014_discrim_poorerservice_bin = case_when(HRS_2014$HRS2014_discrim_poorerservice == 1 ~ 1, 
                                                       HRS_2014$HRS2014_discrim_poorerservice == 2 ~ 1, 
                                                       HRS_2014$HRS2014_discrim_poorerservice == 3 ~ 1, 
                                                       HRS_2014$HRS2014_discrim_poorerservice == 4 ~ 1, 
                                                       HRS_2014$HRS2014_discrim_poorerservice == 5 ~ 0, 
                                                       HRS_2014$HRS2014_discrim_poorerservice == 6 ~ 0) 




HRS_2014$HRS2014_discrim_afraidothers_bin = case_when(HRS_2014$HRS2014_discrim_afraidothers == 1 ~ 1,
                                                      HRS_2014$HRS2014_discrim_afraidothers == 2 ~ 1, 
                                                      HRS_2014$HRS2014_discrim_afraidothers == 3 ~ 1, 
                                                      HRS_2014$HRS2014_discrim_afraidothers == 4 ~ 1, 
                                                      HRS_2014$HRS2014_discrim_afraidothers == 5 ~ 0, 
                                                      HRS_2014$HRS2014_discrim_afraidothers == 6 ~ 0,
                                                      HRS_2014$HRS2014_discrim_afraidothers == 0 ~ 0) 



HRS_2014$HRS2014_discrim_bin = case_when(HRS_2014$HRS2014_discrim_harassed_bin== 1 | HRS_2014$HRS2014_discrim_lessrespect_bin == 1 | HRS_2014$HRS2014_discrim_medical_bin  == 1 | HRS_2014$HRS2014_discrim_notclever_bin == 1 | HRS_2014$HRS2014_discrim_afraidothers_bin== 1 | HRS_2014$HRS2014_discrim_poorerservice_bin == 1 ~ 1, 
                                         HRS_2014$HRS2014_discrim_harassed_bin== 0 & HRS_2014$HRS2014_discrim_lessrespect_bin == 0 & HRS_2014$HRS2014_discrim_medical_bin  == 0 & HRS_2014$HRS2014_discrim_notclever_bin == 0 & HRS_2014$HRS2014_discrim_afraidothers_bin== 0 & HRS_2014$HRS2014_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2014, file = paste(directory, DATA_ROOT, "HRS_2014_data/HRS2014_dataset_latest_renamed_vars.csv", sep=""))
