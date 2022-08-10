
library(dplyr)





#need to add hypertension_bin to 2018 
#need to add alcohol_days_week to 2008 
#add anxiety to all years 
#add depression new bin to all years (NEW)
#need to add CVD to all years 
#check smoking 
#add PTSD for all years  
#add Alzheimer's for all years   
#add kidney disease for all years    
#limiting longstanding condition (recode to bin)




directory = "/Users/aliyaamirova/Documents/KCL_postDoc/"

SOURCE_ROOT = "Data_analysis/"
OUTPUT_ROOT = "Data_analysis/"


HRS_2018_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))

HRS_2018 = data.frame(HRS_2018_data$HHIDPN)


HRS_2018$HRS2018_alcohol_days_week = HRS_2018_data$alcohol_days_week
HRS_2018$HRS2018_LGB_2016 = HRS_2018_data$LGB_2016
HRS_2018$HRS2018_marital_status = HRS_2018_data$marital_status2018
HRS_2018$HRS2018_married_bin = HRS_2018_data$married2018_bin
HRS_2018$HRS2018_national_origin_ousideUS = HRS_2018_data$national_origin_ousideUS
HRS_2018$HRS2018_separated_bin = HRS_2018_data$separated2018_bin
HRS_2018$HRS2018_sexual_orientation = HRS_2018_data$sexual_orientation2018
HRS_2018$HRS2018_smokes_ever = HRS_2018_data$smokes_ever
HRS_2018$HRS2018_smokes_ever_bin = HRS_2018_data$smokes_ever_bin
HRS_2018$HRS2018_smokes_now = HRS_2018_data$smokes_now
HRS_2018$HRS2018_smokes_now_bin = HRS_2018_data$mokes_now_bin
HRS_2018$HRS2018_start = HRS_2018_data$start
HRS_2018$HRS2018_stop = HRS_2018_data$stop
HRS_2018$HRS2018_Straight_2016 = HRS_2018_data$Straight_2016
HRS_2018$HRS2018_summary_mean_score_discrim = HRS_2018_data$summary_mean_score_discrim
HRS_2018$HRS2018_summary_mean_score_discrim_bin = HRS_2018_data$summary_mean_score_discrim_bin 
HRS_2018$HRS2018_vigarious_physical_activity = HRS_2018_data$vigarious_physical_activity
HRS_2018$HRS2018_wealth_noIRA = HRS_2018_data$wealth_noIRA
HRS_2018$HRS2018_widowed_bin = HRS_2018_data$widowed2018_bin
HRS_2018$HRS2018_yearsof_education = HRS_2018_data$yearsof_education2018
HRS_2018$HRS2018_diabetes_ever = HRS_2018_data$diabetes_ever
HRS_2018$HRS2018_diabetes_new = HRS_2018_data$diabetes_new
HRS_2018$HRS2018_continious_age = HRS_2018_data$continious_age
HRS_2018$HRS2018_angina_new_bin = HRS_2018_data$angina_new_bin
HRS_2018$HRS2018_number_reasons_discrimination = HRS_2018_data$number_reasons_discrimination
HRS_2018$HRS2018_race_white = HRS_2018_data$race_white
HRS_2018$HRS2018_religion_bin = HRS_2018_data$
HRS_2018$HRS2018_sex_1_0 = HRS_2018_data$sex_1_0_2018
HRS_2018$HRS2018_sex_1_2 = HRS_2018_data$sex_1_2_2018
HRS_2018$HRS2018_annual_income_self_employment = HRS_2018_data$annual_income_self_employment2018
HRS_2018$HRS2018_never_married_bin = HRS_2018_data$never_married2018_bin
HRS_2018$HRS2018_angina_ever = HRS_2018_data$HRS2018_angina_ever
HRS_2018$HRS2018_angina_new = HRS_2018_data$HRS2018_angina_new
HRS_2018$HRS2018_angina2yrs_bin = HRS_2018_data$HRS2018_angina2yrs_bin
HRS_2018$HRS2018_BMI = HRS_2018_data$HRS2018_BMI
HRS_2018$HRS2018_BMI_cat = HRS_2018_data$HRS2018_BMI_cat
HRS_2018$HRS2018_BMI_category = HRS_2018_data$HRS2018_BMI_category
HRS_2018$HRS2018_checklist_depression_bin = HRS_2018_data$HRS2018_checklist_depression_bin
HRS_2018$HRS2018_discrim_afraidothers = HRS_2018_data$HRS2018_discrim_afraidothers
HRS_2018$HRS2018_discrim_afraidothers_bin = HRS_2018_data$HRS2018_discrim_afraidothers_bin 
HRS_2018$HRS2018_discrim_harassed = HRS_2018_data$HRS2018_discrim_harassed
HRS_2018$HRS2018_discrim_harassed_bin = HRS_2018_data$HRS2018_discrim_harassed_bin
HRS_2018$HRS2018_discrim_lessrespect = HRS_2018_data$HRS2018_discrim_lessrespect
HRS_2018$HRS2018_discrim_lessrespect_bin = HRS_2018_data$HRS2018_discrim_lessrespect_bin
HRS_2018$HRS2018_discrim_medical = HRS_2018_data$HRS2018_discrim_medical
HRS_2018$HRS2018_discrim_medical_bin = HRS_2018_data$HRS2018_discrim_medical_bin
HRS_2018$HRS2018_discrim_notclever = HRS_2018_data$HRS2018_discrim_notclever
HRS_2018$HRS2018_discrim_notclever_bin = HRS_2018_data$HRS2018_discrim_notclever_bin
HRS_2018$HRS2018_discrim_poorerservice = HRS_2018_data$HRS2018_discrim_poorerservice
HRS_2018$HRS2018_discrim_poorerservice_bin = HRS_2018_data$HRS2018_discrim_poorerservice_bin
HRS_2018$HRS2018_height_feet = HRS_2018_data$HRS2018_height_feet
HRS_2018$HRS2018_height_inches = HRS_2018_data$HRS2018_height_inches
HRS_2018$HRS2018_height_meters = HRS_2018_data$HRS2018_height_meters
HRS_2018$HRS2018_limiting_condition_bin = HRS_2018_data$HRS2018_limiting_condition_bin
HRS_2018$HRS2018_normalweight_bin = HRS_2018_data$HRS2018_normalweight_bin
HRS_2018$HRS2018_overweight_bin = HRS_2018_data$HRS2018_overweight_bin
HRS_2018$HRS2018_race = HRS_2018_data$HRS2018_race
HRS_2018$HRS2018_race_black = HRS_2018_data$HRS2018_race_black
HRS_2018$HRS2018_race_hispanic_latino = HRS_2018_data$HRS2018_race_hispanic_latino
HRS_2018$HRS2018_race_nonwhite = HRS_2018_data$HRS2018_race_nonwhite
HRS_2018$HRS2018_race_white = HRS_2018_data$HRS2018_race_white
HRS_2018$HRS2018_reason_discrim1 = HRS_2018_data$HRS2018_reason_discrim1
HRS_2018$HRS2018_reason_discrim1_reason_age = HRS_2018_data$HRS2018_reason_discrim1_reason_age
HRS_2018$HRS2018_reason_discrim1_reason_disability = HRS_2018_data$HRS2018_reason_discrim1_reason_disability
HRS_2018$HRS2018_reason_discrim1_reason_financial = HRS_2018_data$HRS2018_reason_discrim1_reason_financial
HRS_2018$HRS2018_reason_discrim1_reason_gender = HRS_2018_data$HRS2018_reason_discrim1_reason_gender
HRS_2018$HRS2018_reason_discrim1_reason_national = HRS_2018_data$HRS2018_reason_discrim1_reason_national
HRS_2018$HRS2018_reason_discrim1_reason_otherreason = HRS_2018_data$HRS2018_reason_discrim1_reason_otherreason
HRS_2018$HRS2018_reason_discrim1_reason_race = HRS_2018_data$HRS2018_reason_discrim1_reason_race
HRS_2018$HRS2018_reason_discrim1_reason_religion = HRS_2018_data$HRS2018_reason_discrim1_reason_religion
HRS_2018$HRS2018_reason_discrim1_reason_sexuality = HRS_2018_data$HRS2018_reason_discrim1_reason_sexuality
HRS_2018$HRS2018_reason_discrim1_reason_weight = HRS_2018_data$HRS2018_reason_discrim1_reason_weight
HRS_2018$HRS2018_underweight_bin = HRS_2018_data$HRS2018_underweight_bin
HRS_2018$HRS2018_weight_kg = HRS_2018_data$HRS2018_weight_kg
HRS_2018$HRS2018_weight_pounds = HRS_2018_data$HRS2018_weight_pounds
HRS_2018$HRS2018_obese_bin = HRS_2018_data$HRS2018obese_bin




HRS_2018$HRS2018_discrim_harassed_bin = case_when(HRS_2018$HRS2018_discrim_harassed == 1 ~ 1, 
                                                  HRS_2018$HRS2018_discrim_harassed == 2 ~ 1, 
                                                  HRS_2018$HRS2018_discrim_harassed == 3 ~ 1, 
                                                  HRS_2018$HRS2018_discrim_harassed == 4 ~ 1, 
                                                  HRS_2018$HRS2018_discrim_harassed == 5 ~ 0, 
                                                  HRS_2018$HRS2018_discrim_harassed == 6 ~ 0,
                                                  HRS_2018$HRS2018_discrim_harassed == 0 ~ 0) 



HRS_2018$HRS2018_discrim_lessrespect_bin = case_when(HRS_2018$HRS2018_discrim_lessrespect == 1 ~ 1, 
                                                     HRS_2018$HRS2018_discrim_lessrespect == 2 ~ 1, 
                                                     HRS_2018$HRS2018_discrim_lessrespect == 3 ~ 1, 
                                                     HRS_2018$HRS2018_discrim_lessrespect == 4 ~ 1, 
                                                     HRS_2018$HRS2018_discrim_lessrespect == 5 ~ 0, 
                                                     HRS_2018$HRS2018_discrim_lessrespect == 6 ~ 0,
                                                     HRS_2018$HRS2018_discrim_lessrespect == 0 ~ 0) 



HRS_2018$HRS2018_discrim_medical_bin = case_when(HRS_2018$HRS2018_discrim_medical == 1 ~ 1, 
                                                 HRS_2018$HRS2018_discrim_medical == 2 ~ 1, 
                                                 HRS_2018$HRS2018_discrim_medical == 3 ~ 1, 
                                                 HRS_2018$HRS2018_discrim_medical == 4 ~ 1, 
                                                 HRS_2018$HRS2018_discrim_medical == 5 ~ 0, 
                                                 HRS_2018$HRS2018_discrim_medical == 6 ~ 0,
                                                 HRS_2018$HRS2018_discrim_medical == 0 ~ 0) 





HRS_2018$HRS2018_discrim_notclever_bin = case_when(HRS_2018$HRS2018_discrim_notclever == 1 ~ 1, 
                                                   HRS_2018$HRS2018_discrim_notclever == 2 ~ 1, 
                                                   HRS_2018$HRS2018_discrim_notclever == 3 ~ 1, 
                                                   HRS_2018$HRS2018_discrim_notclever == 4 ~ 1, 
                                                   HRS_2018$HRS2018_discrim_notclever == 5 ~ 0, 
                                                   HRS_2018$HRS2018_discrim_notclever == 6 ~ 0,
                                                   HRS_2018$HRS2018_discrim_notclever == 0 ~ 0) 






HRS_2018$HRS2018_discrim_poorerservice_bin = case_when(HRS_2018$HRS2018_discrim_poorerservice == 1 ~ 1, 
                                                       HRS_2018$HRS2018_discrim_poorerservice == 2 ~ 1, 
                                                       HRS_2018$HRS2018_discrim_poorerservice == 3 ~ 1, 
                                                       HRS_2018$HRS2018_discrim_poorerservice == 4 ~ 1, 
                                                       HRS_2018$HRS2018_discrim_poorerservice == 5 ~ 0, 
                                                       HRS_2018$HRS2018_discrim_poorerservice == 6 ~ 0) 




HRS_2018$HRS2018_discrim_afraidothers_bin = case_when(HRS_2018$HRS2018_discrim_afraidothers == 1 ~ 1,
                                                      HRS_2018$HRS2018_discrim_afraidothers == 2 ~ 1, 
                                                      HRS_2018$HRS2018_discrim_afraidothers == 3 ~ 1, 
                                                      HRS_2018$HRS2018_discrim_afraidothers == 4 ~ 1, 
                                                      HRS_2018$HRS2018_discrim_afraidothers == 5 ~ 0, 
                                                      HRS_2018$HRS2018_discrim_afraidothers == 6 ~ 0,
                                                      HRS_2018$HRS2018_discrim_afraidothers == 0 ~ 0) 



HRS_2018$HRS2018_discrim_bin = case_when(HRS_2018$HRS2018_discrim_harassed_bin== 1 | HRS_2018$HRS2018_discrim_lessrespect_bin == 1 | HRS_2018$HRS2018_discrim_medical_bin  == 1 | HRS_2018$HRS2018_discrim_notclever_bin == 1 | HRS_2018$HRS2018_discrim_afraidothers_bin== 1 | HRS_2018$HRS2018_discrim_poorerservice_bin == 1 ~ 1, 
                                         HRS_2018$HRS2018_discrim_harassed_bin== 0 & HRS_2018$HRS2018_discrim_lessrespect_bin == 0 & HRS_2018$HRS2018_discrim_medical_bin  == 0 & HRS_2018$HRS2018_discrim_notclever_bin == 0 & HRS_2018$HRS2018_discrim_afraidothers_bin== 0 & HRS_2018$HRS2018_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2018, file = paste(directory, SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest_renamed_vars.csv", sep=""))

ls(HRS_2018)