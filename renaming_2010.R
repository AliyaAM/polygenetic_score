
library(dplyr)


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


HRS_2010_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))

HRS_2010 = data.frame(HRS_2010_data$HHIDPN)


HRS_2010$HRS2010_alcohol_days_week = HRS_2010_data$alcohol_days_week
HRS_2010$HRS2010_LGB_2010 = HRS_2010_data$LGB_2010
HRS_2010$HRS2010_marital_status = HRS_2010_data$marital_status2010
HRS_2010$HRS2010_married_bin = HRS_2010_data$married2010_bin
HRS_2010$HRS2010_national_origin_ousideUS = HRS_2010_data$national_origin_ousideUS
HRS_2010$HRS2010_separated_bin = HRS_2010_data$separated2010_bin
HRS_2010$HRS2010_sexual_orientation = HRS_2010_data$sexual_orientation2010
HRS_2010$HRS2010_smokes_ever = HRS_2010_data$smokes_ever
HRS_2010$HRS2010_smokes_ever_bin = HRS_2010_data$smokes_ever_bin
HRS_2010$HRS2010_smokes_now = HRS_2010_data$smokes_now
HRS_2010$HRS2010_smokes_now_bin = HRS_2010_data$mokes_now_bin
HRS_2010$HRS2010_start = HRS_2010_data$start
HRS_2010$HRS2010_stop = HRS_2010_data$stop
HRS_2010$HRS2010_Straight_2010 = HRS_2010_data$Straight_2010
HRS_2010$HRS2010_summary_mean_score_discrim = HRS_2010_data$summary_mean_score_discrim
HRS_2010$HRS2010_summary_mean_score_discrim_bin = HRS_2010_data$summary_mean_score_discrim_bin 
HRS_2010$HRS2010_vigarious_physical_activity = HRS_2010_data$vigarious_physical_activity
HRS_2010$HRS2010_wealth_noIRA = HRS_2010_data$wealth_noIRA
HRS_2010$HRS2010_widowed_bin = HRS_2010_data$widowed2010_bin
HRS_2010$HRS2010_yearsof_education = HRS_2010_data$yearsof_education2010
HRS_2010$HRS2010_diabetes_ever = HRS_2010_data$diabetes_ever
HRS_2010$HRS2010_diabetes_new = HRS_2010_data$diabetes_new
HRS_2010$HRS2010_continious_age = HRS_2010_data$continious_age
HRS_2010$HRS2010_angina_new_bin = HRS_2010_data$angina_new_bin
HRS_2010$HRS2010_number_reasons_discrimination = HRS_2010_data$number_reasons_discrimination
HRS_2010$HRS2010_race_white = HRS_2010_data$race_white
HRS_2010$HRS2010_religion_bin = HRS_2010_data$
  HRS_2010$HRS2010_sex_1_0 = HRS_2010_data$sex_1_0_2010
HRS_2010$HRS2010_sex_1_2 = HRS_2010_data$sex_1_2_2010
HRS_2010$HRS2010_annual_income_self_employment = HRS_2010_data$annual_income_self_employment2010
HRS_2010$HRS2010_never_married_bin = HRS_2010_data$never_married2010_bin
HRS_2010$HRS2010_angina_ever = HRS_2010_data$HRS2010_angina_ever
HRS_2010$HRS2010_angina_new = HRS_2010_data$HRS2010_angina_new
HRS_2010$HRS2010_angina2yrs_bin = HRS_2010_data$HRS2010_angina2yrs_bin
HRS_2010$HRS2010_BMI = HRS_2010_data$HRS2010_BMI
HRS_2010$HRS2010_BMI_cat = HRS_2010_data$HRS2010_BMI_cat
HRS_2010$HRS2010_BMI_category = HRS_2010_data$HRS2010_BMI_category
HRS_2010$HRS2010_checklist_depression_bin = HRS_2010_data$HRS2010_checklist_depression_bin
HRS_2010$HRS2010_discrim_afraidothers = HRS_2010_data$HRS2010_discrim_afraidothers
HRS_2010$HRS2010_discrim_afraidothers_bin = HRS_2010_data$HRS2010_discrim_afraidothers_bin 
HRS_2010$HRS2010_discrim_harassed = HRS_2010_data$HRS2010_discrim_harassed
HRS_2010$HRS2010_discrim_harassed_bin = HRS_2010_data$HRS2010_discrim_harassed_bin
HRS_2010$HRS2010_discrim_lessrespect = HRS_2010_data$HRS2010_discrim_lessrespect
HRS_2010$HRS2010_discrim_lessrespect_bin = HRS_2010_data$HRS2010_discrim_lessrespect_bin
HRS_2010$HRS2010_discrim_medical = HRS_2010_data$HRS2010_discrim_medical
HRS_2010$HRS2010_discrim_medical_bin = HRS_2010_data$HRS2010_discrim_medical_bin
HRS_2010$HRS2010_discrim_notclever = HRS_2010_data$HRS2010_discrim_notclever
HRS_2010$HRS2010_discrim_notclever_bin = HRS_2010_data$HRS2010_discrim_notclever_bin
HRS_2010$HRS2010_discrim_poorerservice = HRS_2010_data$HRS2010_discrim_poorerservice
HRS_2010$HRS2010_discrim_poorerservice_bin = HRS_2010_data$HRS2010_discrim_poorerservice_bin
HRS_2010$HRS2010_height_feet = HRS_2010_data$HRS2010_height_feet
HRS_2010$HRS2010_height_inches = HRS_2010_data$HRS2010_height_inches
HRS_2010$HRS2010_height_meters = HRS_2010_data$HRS2010_height_meters
HRS_2010$HRS2010_limiting_condition_bin = HRS_2010_data$HRS2010_limiting_condition_bin
HRS_2010$HRS2010_normalweight_bin = HRS_2010_data$HRS2010_normalweight_bin
HRS_2010$HRS2010_overweight_bin = HRS_2010_data$HRS2010_overweight_bin
HRS_2010$HRS2010_race = HRS_2010_data$HRS2010_race
HRS_2010$HRS2010_race_black = HRS_2010_data$HRS2010_race_black
HRS_2010$HRS2010_race_hispanic_latino = HRS_2010_data$HRS2010_race_hispanic_latino
HRS_2010$HRS2010_race_nonwhite = HRS_2010_data$HRS2010_race_nonwhite
HRS_2010$HRS2010_race_white = HRS_2010_data$HRS2010_race_white
HRS_2010$HRS2010_reason_discrim1 = HRS_2010_data$HRS2010_reason_discrim1
HRS_2010$HRS2010_reason_discrim1_reason_age = HRS_2010_data$HRS2010_reason_discrim1_reason_age
HRS_2010$HRS2010_reason_discrim1_reason_disability = HRS_2010_data$HRS2010_reason_discrim1_reason_disability
HRS_2010$HRS2010_reason_discrim1_reason_financial = HRS_2010_data$HRS2010_reason_discrim1_reason_financial
HRS_2010$HRS2010_reason_discrim1_reason_gender = HRS_2010_data$HRS2010_reason_discrim1_reason_gender
HRS_2010$HRS2010_reason_discrim1_reason_national = HRS_2010_data$HRS2010_reason_discrim1_reason_national
HRS_2010$HRS2010_reason_discrim1_reason_otherreason = HRS_2010_data$HRS2010_reason_discrim1_reason_otherreason
HRS_2010$HRS2010_reason_discrim1_reason_race = HRS_2010_data$HRS2010_reason_discrim1_reason_race
HRS_2010$HRS2010_reason_discrim1_reason_religion = HRS_2010_data$HRS2010_reason_discrim1_reason_religion
HRS_2010$HRS2010_reason_discrim1_reason_sexuality = HRS_2010_data$HRS2010_reason_discrim1_reason_sexuality
HRS_2010$HRS2010_reason_discrim1_reason_weight = HRS_2010_data$HRS2010_reason_discrim1_reason_weight
HRS_2010$HRS2010_underweight_bin = HRS_2010_data$HRS2010_underweight_bin
HRS_2010$HRS2010_weight_kg = HRS_2010_data$HRS2010_weight_kg
HRS_2010$HRS2010_weight_pounds = HRS_2010_data$HRS2010_weight_pounds
HRS_2010$HRS2010_obese_bin = HRS_2010_data$HRS2010obese_bin




HRS_2010$HRS2010_discrim_harassed_bin = case_when(HRS_2010$HRS2010_discrim_harassed == 1 ~ 1, 
                                                  HRS_2010$HRS2010_discrim_harassed == 2 ~ 1, 
                                                  HRS_2010$HRS2010_discrim_harassed == 3 ~ 1, 
                                                  HRS_2010$HRS2010_discrim_harassed == 4 ~ 1, 
                                                  HRS_2010$HRS2010_discrim_harassed == 5 ~ 0, 
                                                  HRS_2010$HRS2010_discrim_harassed == 6 ~ 0,
                                                  HRS_2010$HRS2010_discrim_harassed == 0 ~ 0) 



HRS_2010$HRS2010_discrim_lessrespect_bin = case_when(HRS_2010$HRS2010_discrim_lessrespect == 1 ~ 1, 
                                                     HRS_2010$HRS2010_discrim_lessrespect == 2 ~ 1, 
                                                     HRS_2010$HRS2010_discrim_lessrespect == 3 ~ 1, 
                                                     HRS_2010$HRS2010_discrim_lessrespect == 4 ~ 1, 
                                                     HRS_2010$HRS2010_discrim_lessrespect == 5 ~ 0, 
                                                     HRS_2010$HRS2010_discrim_lessrespect == 6 ~ 0,
                                                     HRS_2010$HRS2010_discrim_lessrespect == 0 ~ 0) 



HRS_2010$HRS2010_discrim_medical_bin = case_when(HRS_2010$HRS2010_discrim_medical == 1 ~ 1, 
                                                 HRS_2010$HRS2010_discrim_medical == 2 ~ 1, 
                                                 HRS_2010$HRS2010_discrim_medical == 3 ~ 1, 
                                                 HRS_2010$HRS2010_discrim_medical == 4 ~ 1, 
                                                 HRS_2010$HRS2010_discrim_medical == 5 ~ 0, 
                                                 HRS_2010$HRS2010_discrim_medical == 6 ~ 0,
                                                 HRS_2010$HRS2010_discrim_medical == 0 ~ 0) 





HRS_2010$HRS2010_discrim_notclever_bin = case_when(HRS_2010$HRS2010_discrim_notclever == 1 ~ 1, 
                                                   HRS_2010$HRS2010_discrim_notclever == 2 ~ 1, 
                                                   HRS_2010$HRS2010_discrim_notclever == 3 ~ 1, 
                                                   HRS_2010$HRS2010_discrim_notclever == 4 ~ 1, 
                                                   HRS_2010$HRS2010_discrim_notclever == 5 ~ 0, 
                                                   HRS_2010$HRS2010_discrim_notclever == 6 ~ 0,
                                                   HRS_2010$HRS2010_discrim_notclever == 0 ~ 0) 






HRS_2010$HRS2010_discrim_poorerservice_bin = case_when(HRS_2010$HRS2010_discrim_poorerservice == 1 ~ 1, 
                                                       HRS_2010$HRS2010_discrim_poorerservice == 2 ~ 1, 
                                                       HRS_2010$HRS2010_discrim_poorerservice == 3 ~ 1, 
                                                       HRS_2010$HRS2010_discrim_poorerservice == 4 ~ 1, 
                                                       HRS_2010$HRS2010_discrim_poorerservice == 5 ~ 0, 
                                                       HRS_2010$HRS2010_discrim_poorerservice == 6 ~ 0) 




HRS_2010$HRS2010_discrim_afraidothers_bin = case_when(HRS_2010$HRS2010_discrim_afraidothers == 1 ~ 1,
                                                      HRS_2010$HRS2010_discrim_afraidothers == 2 ~ 1, 
                                                      HRS_2010$HRS2010_discrim_afraidothers == 3 ~ 1, 
                                                      HRS_2010$HRS2010_discrim_afraidothers == 4 ~ 1, 
                                                      HRS_2010$HRS2010_discrim_afraidothers == 5 ~ 0, 
                                                      HRS_2010$HRS2010_discrim_afraidothers == 6 ~ 0,
                                                      HRS_2010$HRS2010_discrim_afraidothers == 0 ~ 0) 



HRS_2010$HRS2010_discrim_bin = case_when(HRS_2010$HRS2010_discrim_harassed_bin== 1 | HRS_2010$HRS2010_discrim_lessrespect_bin == 1 | HRS_2010$HRS2010_discrim_medical_bin  == 1 | HRS_2010$HRS2010_discrim_notclever_bin == 1 | HRS_2010$HRS2010_discrim_afraidothers_bin== 1 | HRS_2010$HRS2010_discrim_poorerservice_bin == 1 ~ 1, 
                                         HRS_2010$HRS2010_discrim_harassed_bin== 0 & HRS_2010$HRS2010_discrim_lessrespect_bin == 0 & HRS_2010$HRS2010_discrim_medical_bin  == 0 & HRS_2010$HRS2010_discrim_notclever_bin == 0 & HRS_2010$HRS2010_discrim_afraidothers_bin== 0 & HRS_2010$HRS2010_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2010, file = paste(directory, SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest_renamed_vars.csv", sep=""))
