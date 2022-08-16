
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

print("#add reports 2012 heart attack since last wave from the harmonised study
      
      #https://g2aging.org/?section=concordance-search&sWords=heart+attack&interval=1992%2C2018&page=1&per_page=50&af_src=33") 



HRS_2012_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))

HRS_2012 = data.frame(HRS_2012_data$HHIDPN)

print("add all_HRS_by_years_PGS: arthritis_new,  PTSD, anxiety, alcohol dependence,  smoking cessation, smoking initiation, sig per day, drinks per day" )

HRS_2012$HRS2012_self_rated_health = HRS_2012_data$self_rated_health
HRS_2012$HRS2012_life_satisfaction = HRS_2012_data$HRS2012_life_satisfaction 
HRS_2012$HRS2012_alzheimer_bin = HRS_2012_data$HRS2012_alzheimer_bin
HRS_2012$HRS2012_angina_new_bin = HRS_2012_data$HRS2012_angina2yrs_bin

#HRS_2012$HRS2012_arthritis_new = HRS_2012_data$HRS2012_arthritis_new
#unique(HRS_2012$HRS2012_arthritis_new)
#HRS_2012$HRS2012_arthritis_new_bin = case_when(HRS_2012$HRS2012_arthritis_new == 0 ~ 0, 
#                                               HRS_2012$HRS2012_arthritis_new == 1 ~ 1)
#unique(HRS_2012$HRS2012_arthritis_new_bin)


HRS_2012$HRS2012_hypertension_new =  HRS_2012_data$HRS2012_hypertension_new 
unique(HRS_2012$HRS2012_hypertension_new)
HRS_2012$HRS2012_hypertension_new_bin = case_when(HRS_2012$HRS2012_hypertension_new == 0 ~ 0,
                                                  HRS_2012$HRS2012_hypertension_new == 1 ~ 1)
#HRS_2012_data$hear
#HRS_2012$HRS2012_mi = HRS_2012_data$HRS2012_heartattack2yrs_bin
HRS_2012$HRS2012_depression_bin = HRS_2012_data$HRS2012_depression_bin



HRS_2012$HRS2012_alcohol_days_week = HRS_2012_data$alcohol_days_week
HRS_2012$HRS2012_LGB_2012 = HRS_2012_data$LGB_2012
HRS_2012$HRS2012_marital_status = HRS_2012_data$marital_status2012
HRS_2012$HRS2012_married_bin = HRS_2012_data$married2012_bin
HRS_2012$HRS2012_national_origin_ousideUS = HRS_2012_data$national_origin_ousideUS
HRS_2012$HRS2012_separated_bin = HRS_2012_data$separated2012_bin
HRS_2012$HRS2012_sexual_orientation = HRS_2012_data$sexual_orientation2012
HRS_2012$HRS2012_smokes_ever = HRS_2012_data$smokes_ever
HRS_2012$HRS2012_smokes_ever_bin = HRS_2012_data$smokes_ever_bin
HRS_2012$HRS2012_smokes_now = HRS_2012_data$smokes_now
HRS_2012$HRS2012_smokes_now_bin = HRS_2012_data$mokes_now_bin
HRS_2012$HRS2012_start = HRS_2012_data$start
HRS_2012$HRS2012_stop = HRS_2012_data$stop
HRS_2012$HRS2012_Straight_2012 = HRS_2012_data$Straight_2012
HRS_2012$HRS2012_summary_mean_score_discrim = HRS_2012_data$summary_mean_score_discrim
HRS_2012$HRS2012_summary_mean_score_discrim_bin = HRS_2012_data$summary_mean_score_discrim_bin 
HRS_2012$HRS2012_vigarious_physical_activity = HRS_2012_data$vigarious_physical_activity
HRS_2012$HRS2012_wealth_noIRA = HRS_2012_data$wealth_noIRA
HRS_2012$HRS2012_widowed_bin = HRS_2012_data$widowed2012_bin
HRS_2012$HRS2012_yearsof_education = HRS_2012_data$yearsof_education2012
HRS_2012$HRS2012_diabetes_ever = HRS_2012_data$diabetes_ever
HRS_2012$HRS2012_diabetes_new = HRS_2012_data$diabetes_new
HRS_2012$HRS2012_continious_age = HRS_2012_data$continious_age
HRS_2012$HRS2012_angina_new_bin = HRS_2012_data$angina_new_bin
HRS_2012$HRS2012_number_reasons_discrimination = HRS_2012_data$number_reasons_discrimination
HRS_2012$HRS2012_race_white = HRS_2012_data$race_white

  HRS_2012$HRS2012_sex_1_0 = HRS_2012_data$sex_1_0_2012
HRS_2012$HRS2012_sex_1_2 = HRS_2012_data$sex_1_2_2012
HRS_2012$HRS2012_annual_income_self_employment = HRS_2012_data$annual_income_self_employment2012
HRS_2012$HRS2012_never_married_bin = HRS_2012_data$never_married2012_bin
HRS_2012$HRS2012_angina_ever = HRS_2012_data$HRS2012_angina_ever
HRS_2012$HRS2012_angina_new = HRS_2012_data$HRS2012_angina_new
HRS_2012$HRS2012_angina2yrs_bin = HRS_2012_data$HRS2012_angina2yrs_bin

# HRS_2012$HRS2012_mi_bin = case_when(HRS_2012$HRS2012_mi == 1 ~ 1,
#                                     HRS_2012$HRS2012_mi == 0 ~ 0)


HRS_2012$HRS2012_BMI = HRS_2012_data$HRS2012_BMI
HRS_2012$HRS2012_BMI_cat = HRS_2012_data$HRS2012_BMI_cat
HRS_2012$HRS2012_BMI_category = HRS_2012_data$HRS2012_BMI_category
HRS_2012$HRS2012_checklist_depression_bin = HRS_2012_data$HRS2012_checklist_depression_bin
HRS_2012$HRS2012_discrim_afraidothers = HRS_2012_data$HRS2012_discrim_afraidothers
HRS_2012$HRS2012_discrim_afraidothers_bin = HRS_2012_data$HRS2012_discrim_afraidothers_bin 
HRS_2012$HRS2012_discrim_harassed = HRS_2012_data$HRS2012_discrim_harassed
HRS_2012$HRS2012_discrim_harassed_bin = HRS_2012_data$HRS2012_discrim_harassed_bin
HRS_2012$HRS2012_discrim_lessrespect = HRS_2012_data$HRS2012_discrim_lessrespect
HRS_2012$HRS2012_discrim_lessrespect_bin = HRS_2012_data$HRS2012_discrim_lessrespect_bin
HRS_2012$HRS2012_discrim_medical = HRS_2012_data$HRS2012_discrim_medical
HRS_2012$HRS2012_discrim_medical_bin = HRS_2012_data$HRS2012_discrim_medical_bin
HRS_2012$HRS2012_discrim_notclever = HRS_2012_data$HRS2012_discrim_notclever
HRS_2012$HRS2012_discrim_notclever_bin = HRS_2012_data$HRS2012_discrim_notclever_bin
HRS_2012$HRS2012_discrim_poorerservice = HRS_2012_data$HRS2012_discrim_poorerservice
HRS_2012$HRS2012_discrim_poorerservice_bin = HRS_2012_data$HRS2012_discrim_poorerservice_bin
HRS_2012$HRS2012_height_feet = HRS_2012_data$HRS2012_height_feet
HRS_2012$HRS2012_height_inches = HRS_2012_data$HRS2012_height_inches
HRS_2012$HRS2012_height_meters = HRS_2012_data$HRS2012_height_meters
HRS_2012$HRS2012_limiting_condition_bin = HRS_2012_data$HRS2012_limiting_condition_bin
HRS_2012$HRS2012_normalweight_bin = HRS_2012_data$HRS2012_normalweight_bin
HRS_2012$HRS2012_overweight_bin = HRS_2012_data$HRS2012_overweight_bin
HRS_2012$HRS2012_race = HRS_2012_data$HRS2012_race
HRS_2012$HRS2012_race_black = HRS_2012_data$HRS2012_race_black
HRS_2012$HRS2012_race_hispanic_latino = HRS_2012_data$HRS2012_race_hispanic_latino
HRS_2012$HRS2012_race_nonwhite = HRS_2012_data$HRS2012_race_nonwhite
HRS_2012$HRS2012_race_white = HRS_2012_data$HRS2012_race_white
HRS_2012$HRS2012_reason_discrim1 = HRS_2012_data$HRS2012_reason_discrim1
HRS_2012$HRS2012_reason_discrim1_reason_age = HRS_2012_data$HRS2012_reason_discrim1_reason_age
HRS_2012$HRS2012_reason_discrim1_reason_disability = HRS_2012_data$HRS2012_reason_discrim1_reason_disability
HRS_2012$HRS2012_reason_discrim1_reason_financial = HRS_2012_data$HRS2012_reason_discrim1_reason_financial
HRS_2012$HRS2012_reason_discrim1_reason_gender = HRS_2012_data$HRS2012_reason_discrim1_reason_gender
HRS_2012$HRS2012_reason_discrim1_reason_national = HRS_2012_data$HRS2012_reason_discrim1_reason_national
HRS_2012$HRS2012_reason_discrim1_reason_otherreason = HRS_2012_data$HRS2012_reason_discrim1_reason_otherreason
HRS_2012$HRS2012_reason_discrim1_reason_race = HRS_2012_data$HRS2012_reason_discrim1_reason_race
HRS_2012$HRS2012_reason_discrim1_reason_religion = HRS_2012_data$HRS2012_reason_discrim1_reason_religion
HRS_2012$HRS2012_reason_discrim1_reason_sexuality = HRS_2012_data$HRS2012_reason_discrim1_reason_sexuality
HRS_2012$HRS2012_reason_discrim1_reason_weight = HRS_2012_data$HRS2012_reason_discrim1_reason_weight
HRS_2012$HRS2012_underweight_bin = HRS_2012_data$HRS2012_underweight_bin
HRS_2012$HRS2012_weight_kg = HRS_2012_data$HRS2012_weight_kg
HRS_2012$HRS2012_weight_pounds = HRS_2012_data$HRS2012_weight_pounds
HRS_2012$HRS2012_obese_bin = HRS_2012_data$HRS2012obese_bin




HRS_2012$HRS2012_discrim_harassed_bin = case_when(HRS_2012$HRS2012_discrim_harassed == 1 ~ 1, 
                                                  HRS_2012$HRS2012_discrim_harassed == 2 ~ 1, 
                                                  HRS_2012$HRS2012_discrim_harassed == 3 ~ 1, 
                                                  HRS_2012$HRS2012_discrim_harassed == 4 ~ 1, 
                                                  HRS_2012$HRS2012_discrim_harassed == 5 ~ 0, 
                                                  HRS_2012$HRS2012_discrim_harassed == 6 ~ 0,
                                                  HRS_2012$HRS2012_discrim_harassed == 0 ~ 0) 



HRS_2012$HRS2012_discrim_lessrespect_bin = case_when(HRS_2012$HRS2012_discrim_lessrespect == 1 ~ 1, 
                                                     HRS_2012$HRS2012_discrim_lessrespect == 2 ~ 1, 
                                                     HRS_2012$HRS2012_discrim_lessrespect == 3 ~ 1, 
                                                     HRS_2012$HRS2012_discrim_lessrespect == 4 ~ 1, 
                                                     HRS_2012$HRS2012_discrim_lessrespect == 5 ~ 0, 
                                                     HRS_2012$HRS2012_discrim_lessrespect == 6 ~ 0,
                                                     HRS_2012$HRS2012_discrim_lessrespect == 0 ~ 0) 



HRS_2012$HRS2012_discrim_medical_bin = case_when(HRS_2012$HRS2012_discrim_medical == 1 ~ 1, 
                                                 HRS_2012$HRS2012_discrim_medical == 2 ~ 1, 
                                                 HRS_2012$HRS2012_discrim_medical == 3 ~ 1, 
                                                 HRS_2012$HRS2012_discrim_medical == 4 ~ 1, 
                                                 HRS_2012$HRS2012_discrim_medical == 5 ~ 0, 
                                                 HRS_2012$HRS2012_discrim_medical == 6 ~ 0,
                                                 HRS_2012$HRS2012_discrim_medical == 0 ~ 0) 





HRS_2012$HRS2012_discrim_notclever_bin = case_when(HRS_2012$HRS2012_discrim_notclever == 1 ~ 1, 
                                                   HRS_2012$HRS2012_discrim_notclever == 2 ~ 1, 
                                                   HRS_2012$HRS2012_discrim_notclever == 3 ~ 1, 
                                                   HRS_2012$HRS2012_discrim_notclever == 4 ~ 1, 
                                                   HRS_2012$HRS2012_discrim_notclever == 5 ~ 0, 
                                                   HRS_2012$HRS2012_discrim_notclever == 6 ~ 0,
                                                   HRS_2012$HRS2012_discrim_notclever == 0 ~ 0) 






HRS_2012$HRS2012_discrim_poorerservice_bin = case_when(HRS_2012$HRS2012_discrim_poorerservice == 1 ~ 1, 
                                                       HRS_2012$HRS2012_discrim_poorerservice == 2 ~ 1, 
                                                       HRS_2012$HRS2012_discrim_poorerservice == 3 ~ 1, 
                                                       HRS_2012$HRS2012_discrim_poorerservice == 4 ~ 1, 
                                                       HRS_2012$HRS2012_discrim_poorerservice == 5 ~ 0, 
                                                       HRS_2012$HRS2012_discrim_poorerservice == 6 ~ 0) 




HRS_2012$HRS2012_discrim_afraidothers_bin = case_when(HRS_2012$HRS2012_discrim_afraidothers == 1 ~ 1,
                                                      HRS_2012$HRS2012_discrim_afraidothers == 2 ~ 1, 
                                                      HRS_2012$HRS2012_discrim_afraidothers == 3 ~ 1, 
                                                      HRS_2012$HRS2012_discrim_afraidothers == 4 ~ 1, 
                                                      HRS_2012$HRS2012_discrim_afraidothers == 5 ~ 0, 
                                                      HRS_2012$HRS2012_discrim_afraidothers == 6 ~ 0,
                                                      HRS_2012$HRS2012_discrim_afraidothers == 0 ~ 0) 



HRS_2012$HRS2012_discrim_bin = case_when(HRS_2012$HRS2012_discrim_harassed_bin== 1 | HRS_2012$HRS2012_discrim_lessrespect_bin == 1 | HRS_2012$HRS2012_discrim_medical_bin  == 1 | HRS_2012$HRS2012_discrim_notclever_bin == 1 | HRS_2012$HRS2012_discrim_afraidothers_bin== 1 | HRS_2012$HRS2012_discrim_poorerservice_bin == 1 ~ 1, 
                                         HRS_2012$HRS2012_discrim_harassed_bin== 0 & HRS_2012$HRS2012_discrim_lessrespect_bin == 0 & HRS_2012$HRS2012_discrim_medical_bin  == 0 & HRS_2012$HRS2012_discrim_notclever_bin == 0 & HRS_2012$HRS2012_discrim_afraidothers_bin== 0 & HRS_2012$HRS2012_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2012, file = paste(directory, DATA_ROOT, "HRS_2012_data/HRS2012_dataset_latest_renamed_vars.csv", sep=""))
