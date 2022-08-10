
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


HRS2008_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))

HRS_2008 = data.frame(HRS2008_data$HHIDPN)


HRS_2008$HRS2008_alcohol_days_week = HRS2008_data$alcohol_days_week
HRS_2008$HRS2008_LGB_2008 = HRS2008_data$LGB_2008
HRS_2008$HRS2008_marital_status = HRS2008_data$marital_status2008
HRS_2008$HRS2008_married_bin = HRS2008_data$married2008_bin
HRS_2008$HRS2008_national_origin_ousideUS = HRS2008_data$national_origin_ousideUS
HRS_2008$HRS2008_separated_bin = HRS2008_data$separated2008_bin
HRS_2008$HRS2008_sexual_orientation = HRS2008_data$sexual_orientation2008
HRS_2008$HRS2008_smokes_ever = HRS2008_data$smokes_ever
HRS_2008$HRS2008_smokes_ever_bin = HRS2008_data$smokes_ever_bin
HRS_2008$HRS2008_smokes_now = HRS2008_data$smokes_now
HRS_2008$HRS2008_smokes_now_bin = HRS2008_data$mokes_now_bin
HRS_2008$HRS2008_start = HRS2008_data$start
HRS_2008$HRS2008_stop = HRS2008_data$stop
HRS_2008$HRS2008_Straight_2008 = HRS2008_data$Straight_2008
HRS_2008$HRS2008_summary_mean_score_discrim = HRS2008_data$summary_mean_score_discrim
HRS_2008$HRS2008_summary_mean_score_discrim_bin = HRS2008_data$summary_mean_score_discrim_bin 
HRS_2008$HRS2008_vigarious_physical_activity = HRS2008_data$vigarious_physical_activity
HRS_2008$HRS2008_wealth_noIRA = HRS2008_data$wealth_noIRA
HRS_2008$HRS2008_widowed_bin = HRS2008_data$widowed2008_bin
HRS_2008$HRS2008_yearsof_education = HRS2008_data$yearsof_education2008
HRS_2008$HRS2008_diabetes_ever = HRS2008_data$diabetes_ever
HRS_2008$HRS2008_diabetes_new = HRS2008_data$diabetes_new
HRS_2008$HRS2008_continious_age = HRS2008_data$continious_age
HRS_2008$HRS2008_angina_new_bin = HRS2008_data$angina_new_bin
HRS_2008$HRS2008_number_reasons_discrimination = HRS2008_data$number_reasons_discrimination
HRS_2008$HRS2008_race_white = HRS2008_data$race_white
HRS_2008$HRS2008_religion_bin = HRS2008_data$
  HRS_2008$HRS2008_sex_1_0 = HRS2008_data$sex_1_0_2008
HRS_2008$HRS2008_sex_1_2 = HRS2008_data$sex_1_2_2008
HRS_2008$HRS2008_annual_income_self_employment = HRS2008_data$annual_income_self_employment2008
HRS_2008$HRS2008_never_married_bin = HRS2008_data$never_married2008_bin
HRS_2008$HRS2008_angina_ever = HRS2008_data$HRS2008_angina_ever
HRS_2008$HRS2008_angina_new = HRS2008_data$HRS2008_angina_new
HRS_2008$HRS2008_angina2yrs_bin = HRS2008_data$HRS2008_angina2yrs_bin
HRS_2008$HRS2008_BMI = HRS2008_data$HRS2008_BMI
HRS_2008$HRS2008_BMI_cat = HRS2008_data$HRS2008_BMI_cat
HRS_2008$HRS2008_BMI_category = HRS2008_data$HRS2008_BMI_category
HRS_2008$HRS2008_checklist_depression_bin = HRS2008_data$HRS2008_checklist_depression_bin
HRS_2008$HRS2008_discrim_afraidothers = HRS2008_data$HRS2008_discrim_afraidothers
HRS_2008$HRS2008_discrim_afraidothers_bin = HRS2008_data$HRS2008_discrim_afraidothers_bin 
HRS_2008$HRS2008_discrim_harassed = HRS2008_data$HRS2008_discrim_harassed
HRS_2008$HRS2008_discrim_lessrespect = HRS2008_data$HRS2008_discrim_lessrespect
HRS_2008$HRS2008_discrim_lessrespect_bin = HRS2008_data$HRS2008_discrim_lessrespect_bin
HRS_2008$HRS2008_discrim_medical = HRS2008_data$HRS2008_discrim_medical
HRS_2008$HRS2008_discrim_medical_bin = HRS2008_data$HRS2008_discrim_medical_bin
HRS_2008$HRS2008_discrim_notclever = HRS2008_data$HRS2008_discrim_notclever
HRS_2008$HRS2008_discrim_notclever_bin = HRS2008_data$HRS2008_discrim_notclever_bin
HRS_2008$HRS2008_discrim_poorerservice = HRS2008_data$HRS2008_discrim_poorerservice
HRS_2008$HRS2008_discrim_poorerservice_bin = HRS2008_data$HRS2008_discrim_poorerservice_bin
HRS_2008$HRS2008_height_feet = HRS2008_data$HRS2008_height_feet
HRS_2008$HRS2008_height_inches = HRS2008_data$HRS2008_height_inches
HRS_2008$HRS2008_height_meters = HRS2008_data$HRS2008_height_meters
HRS_2008$HRS2008_limiting_condition_bin = HRS2008_data$HRS2008_limiting_condition_bin
HRS_2008$HRS2008_normalweight_bin = HRS2008_data$HRS2008_normalweight_bin
HRS_2008$HRS2008_overweight_bin = HRS2008_data$HRS2008_overweight_bin
HRS_2008$HRS2008_race = HRS2008_data$HRS2008_race
HRS_2008$HRS2008_race_black = HRS2008_data$HRS2008_race_black
HRS_2008$HRS2008_race_hispanic_latino = HRS2008_data$HRS2008_race_hispanic_latino
HRS_2008$HRS2008_race_nonwhite = HRS2008_data$HRS2008_race_nonwhite
HRS_2008$HRS2008_race_white = HRS2008_data$HRS2008_race_white
HRS_2008$HRS2008_reason_discrim1 = HRS2008_data$HRS2008_reason_discrim1
HRS_2008$HRS2008_reason_discrim1_reason_age = HRS2008_data$HRS2008_reason_discrim1_reason_age
HRS_2008$HRS2008_reason_discrim1_reason_disability = HRS2008_data$HRS2008_reason_discrim1_reason_disability
HRS_2008$HRS2008_reason_discrim1_reason_financial = HRS2008_data$HRS2008_reason_discrim1_reason_financial
HRS_2008$HRS2008_reason_discrim1_reason_gender = HRS2008_data$HRS2008_reason_discrim1_reason_gender
HRS_2008$HRS2008_reason_discrim1_reason_national = HRS2008_data$HRS2008_reason_discrim1_reason_national
HRS_2008$HRS2008_reason_discrim1_reason_otherreason = HRS2008_data$HRS2008_reason_discrim1_reason_otherreason
HRS_2008$HRS2008_reason_discrim1_reason_race = HRS2008_data$HRS2008_reason_discrim1_reason_race
HRS_2008$HRS2008_reason_discrim1_reason_religion = HRS2008_data$HRS2008_reason_discrim1_reason_religion
HRS_2008$HRS2008_reason_discrim1_reason_sexuality = HRS2008_data$HRS2008_reason_discrim1_reason_sexuality
HRS_2008$HRS2008_reason_discrim1_reason_weight = HRS2008_data$HRS2008_reason_discrim1_reason_weight
HRS_2008$HRS2008_underweight_bin = HRS2008_data$HRS2008_underweight_bin
HRS_2008$HRS2008_weight_kg = HRS2008_data$HRS2008_weight_kg
HRS_2008$HRS2008_weight_pounds = HRS2008_data$HRS2008_weight_pounds
HRS_2008$HRS2008_obese_bin = HRS2008_data$HRS2008obese_bin





HRS_2008$HRS2008_discrim_harassed_bin = case_when(HRS_2008$HRS2008_discrim_harassed == 1 ~ 1, 
                                      HRS_2008$HRS2008_discrim_harassed == 2 ~ 1, 
                                      HRS_2008$HRS2008_discrim_harassed == 3 ~ 1, 
                                      HRS_2008$HRS2008_discrim_harassed == 4 ~ 1, 
                                      HRS_2008$HRS2008_discrim_harassed == 5 ~ 0, 
                                      HRS_2008$HRS2008_discrim_harassed == 6 ~ 0,
                                      HRS_2008$HRS2008_discrim_harassed == 0 ~ 0) 



HRS_2008$HRS2008_discrim_lessrespect_bin = case_when(HRS_2008$HRS2008_discrim_lessrespect == 1 ~ 1, 
                                         HRS_2008$HRS2008_discrim_lessrespect == 2 ~ 1, 
                                         HRS_2008$HRS2008_discrim_lessrespect == 3 ~ 1, 
                                         HRS_2008$HRS2008_discrim_lessrespect == 4 ~ 1, 
                                         HRS_2008$HRS2008_discrim_lessrespect == 5 ~ 0, 
                                         HRS_2008$HRS2008_discrim_lessrespect == 6 ~ 0,
                                         HRS_2008$HRS2008_discrim_lessrespect == 0 ~ 0) 



HRS_2008$HRS2008_discrim_medical_bin = case_when(HRS_2008$HRS2008_discrim_medical == 1 ~ 1, 
                                     HRS_2008$HRS2008_discrim_medical == 2 ~ 1, 
                                     HRS_2008$HRS2008_discrim_medical == 3 ~ 1, 
                                     HRS_2008$HRS2008_discrim_medical == 4 ~ 1, 
                                     HRS_2008$HRS2008_discrim_medical == 5 ~ 0, 
                                     HRS_2008$HRS2008_discrim_medical == 6 ~ 0,
                                     HRS_2008$HRS2008_discrim_medical == 0 ~ 0) 





HRS_2008$HRS2008_discrim_notclever_bin = case_when(HRS_2008$HRS2008_discrim_notclever == 1 ~ 1, 
                                       HRS_2008$HRS2008_discrim_notclever == 2 ~ 1, 
                                       HRS_2008$HRS2008_discrim_notclever == 3 ~ 1, 
                                       HRS_2008$HRS2008_discrim_notclever == 4 ~ 1, 
                                       HRS_2008$HRS2008_discrim_notclever == 5 ~ 0, 
                                       HRS_2008$HRS2008_discrim_notclever == 6 ~ 0,
                                       HRS_2008$HRS2008_discrim_notclever == 0 ~ 0) 






HRS_2008$HRS2008_discrim_poorerservice_bin = case_when(HRS_2008$HRS2008_discrim_poorerservice == 1 ~ 1, 
                                           HRS_2008$HRS2008_discrim_poorerservice == 2 ~ 1, 
                                           HRS_2008$HRS2008_discrim_poorerservice == 3 ~ 1, 
                                           HRS_2008$HRS2008_discrim_poorerservice == 4 ~ 1, 
                                           HRS_2008$HRS2008_discrim_poorerservice == 5 ~ 0, 
                                           HRS_2008$HRS2008_discrim_poorerservice == 6 ~ 0) 




HRS_2008$HRS2008_discrim_afraidothers_bin = case_when(HRS_2008$HRS2008_discrim_afraidothers == 1 ~ 1,
                                          HRS_2008$HRS2008_discrim_afraidothers == 2 ~ 1, 
                                          HRS_2008$HRS2008_discrim_afraidothers == 3 ~ 1, 
                                          HRS_2008$HRS2008_discrim_afraidothers == 4 ~ 1, 
                                          HRS_2008$HRS2008_discrim_afraidothers == 5 ~ 0, 
                                          HRS_2008$HRS2008_discrim_afraidothers == 6 ~ 0,
                                          HRS_2008$HRS2008_discrim_afraidothers == 0 ~ 0) 



HRS_2008$HRS2008_discrim_bin = case_when(HRS_2008$HRS2008_discrim_harassed_bin== 1 | HRS_2008$HRS2008_discrim_lessrespect_bin == 1 | HRS_2008$HRS2008_discrim_medical_bin  == 1 | HRS_2008$HRS2008_discrim_notclever_bin == 1 | HRS_2008$HRS2008_discrim_afraidothers_bin== 1 | HRS_2008$HRS2008_discrim_poorerservice_bin == 1 ~ 1, 
                             HRS_2008$HRS2008_discrim_harassed_bin== 0 & HRS_2008$HRS2008_discrim_lessrespect_bin == 0 & HRS_2008$HRS2008_discrim_medical_bin  == 0 & HRS_2008$HRS2008_discrim_notclever_bin == 0 & HRS_2008$HRS2008_discrim_afraidothers_bin== 0 & HRS_2008$HRS2008_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2008, file = paste(directory, SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest_renamed_vars.csv", sep=""))
