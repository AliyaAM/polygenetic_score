
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


HRS_2016_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))

HRS_2016 = data.frame(HRS_2016_data$HHIDPN)


print("add all_HRS_by_years_PGS:  arthritis_new, PTSD, anxiety, alcohol dependence,  smoking cessation, smoking initiation, sig per day, drinks per day" )


HRS_2016$HRS2016_mi = HRS_2016_data$HRS2016_heartattack2yrs_bin

unique(HRS_2016_data$HRS2016_heartattack2yrs_bin)

HRS_2016$HRS2016_mi_bin = case_when(HRS_2016$HRS2016_mi == 1 ~ 1,
                                    HRS_2016$HRS2016_mi == 5 ~ 0)
unique(HRS_2016$HRS2016_mi_bin)



HRS_2016$HRS2016_Percvd_cntrl_health = HRS_2016_data$Percvd_cntrl_health 


HRS_2016$HRS2016_self_rated_health = HRS_2016_data$self_rated_health
HRS_2016$HRS2016_life_satisfaction = HRS_2016_data$HRS2016_life_satisfaction 
HRS_2016$HRS2016_alzheimer_bin = HRS_2016_data$HRS2016_alzheimer_bin
HRS_2016$HRS2016_angina_new_bin = HRS_2016_data$HRS2016_angina2yrs_bin

# HRS_2016$HRS2016_arthritis_new = HRS_2016_data$HRS2016_arthritis_new
# unique(HRS_2016$HRS2016_arthritis_new)
# HRS_2016$HRS2016_arthritis_new_bin = case_when(HRS_2016$HRS2016_arthritis_new == 0 ~ 0, 
#                                                HRS_2016$HRS2016_arthritis_new == 1 ~ 1)
# unique(HRS_2016$HRS2016_arthritis_new_bin)


HRS_2016$HRS2016_hypertension_new =  HRS_2016_data$HRS2016_hypertension_new 
unique(HRS_2016$HRS2016_hypertension_new)
HRS_2016$HRS2016_hypertension_new_bin = case_when(HRS_2016$HRS2016_hypertension_new == 0 ~ 0,
                                                  HRS_2016$HRS2016_hypertension_new == 1 ~ 1)

HRS_2016$HRS2016_mi = HRS_2016_data$HRS2016_heartattack2yrs_bin
HRS_2016$HRS2016_depression_bin = HRS_2016_data$HRS2016_depression_bin

HRS_2016$HRS2016_alcohol_days_week = HRS_2016_data$alcohol_days_week
HRS_2016$HRS2016_LGB_2016 = HRS_2016_data$LGB_2016
HRS_2016$HRS2016_marital_status = HRS_2016_data$marital_status2016
HRS_2016$HRS2016_married_bin = HRS_2016_data$married2016_bin
HRS_2016$HRS2016_national_origin_ousideUS = HRS_2016_data$national_origin_ousideUS
HRS_2016$HRS2016_separated_bin = HRS_2016_data$separated2016_bin
HRS_2016$HRS2016_sexual_orientation = HRS_2016_data$sexual_orientation2016
HRS_2016$HRS2016_smokes_ever = HRS_2016_data$smokes_ever
HRS_2016$HRS2016_smokes_ever_bin = HRS_2016_data$smokes_ever_bin
HRS_2016$HRS2016_smokes_now = HRS_2016_data$smokes_now
HRS_2016$HRS2016_smokes_now_bin = HRS_2016_data$mokes_now_bin
HRS_2016$HRS2016_start = HRS_2016_data$start
HRS_2016$HRS2016_stop = HRS_2016_data$stop
HRS_2016$HRS2016_Straight_2016 = HRS_2016_data$Straight_2016
HRS_2016$HRS2016_summary_mean_score_discrim = HRS_2016_data$summary_mean_score_discrim
HRS_2016$HRS2016_summary_mean_score_discrim_bin = HRS_2016_data$summary_mean_score_discrim_bin 
HRS_2016$HRS2016_vigarious_physical_activity = HRS_2016_data$vigarious_physical_activity
HRS_2016$HRS2016_wealth_noIRA = HRS_2016_data$wealth_noIRA
HRS_2016$HRS2016_widowed_bin = HRS_2016_data$widowed2016_bin
HRS_2016$HRS2016_yearsof_education = HRS_2016_data$yearsof_education2016
HRS_2016$HRS2016_diabetes_ever = HRS_2016_data$diabetes_ever
HRS_2016$HRS2016_diabetes_new = HRS_2016_data$diabetes_new
HRS_2016$HRS2016_continious_age = HRS_2016_data$continious_age
HRS_2016$HRS2016_angina_new_bin = HRS_2016_data$angina_new_bin
HRS_2016$HRS2016_number_reasons_discrimination = HRS_2016_data$number_reasons_discrimination
HRS_2016$HRS2016_race_white = HRS_2016_data$race_white
HRS_2016$HRS2016_religion_bin = HRS_2016_data$
  HRS_2016$HRS2016_sex_1_0 = HRS_2016_data$sex_1_0_2016
HRS_2016$HRS2016_sex_1_2 = HRS_2016_data$sex_1_2_2016
HRS_2016$HRS2016_annual_income_self_employment = HRS_2016_data$annual_income_self_employment2016
HRS_2016$HRS2016_never_married_bin = HRS_2016_data$never_married2016_bin
HRS_2016$HRS2016_angina_ever = HRS_2016_data$HRS2016_angina_ever
HRS_2016$HRS2016_angina_new = HRS_2016_data$HRS2016_angina_new
HRS_2016$HRS2016_angina2yrs_bin = HRS_2016_data$HRS2016_angina2yrs_bin
HRS_2016$HRS2016_BMI = HRS_2016_data$HRS2016_BMI
HRS_2016$HRS2016_BMI_cat = HRS_2016_data$HRS2016_BMI_cat
HRS_2016$HRS2016_BMI_category = HRS_2016_data$HRS2016_BMI_category
HRS_2016$HRS2016_checklist_depression_bin = HRS_2016_data$HRS2016_checklist_depression_bin
HRS_2016$HRS2016_discrim_afraidothers = HRS_2016_data$HRS2016_discrim_afraidothers
HRS_2016$HRS2016_discrim_afraidothers_bin = HRS_2016_data$HRS2016_discrim_afraidothers_bin 
HRS_2016$HRS2016_discrim_harassed = HRS_2016_data$HRS2016_discrim_harassed
HRS_2016$HRS2016_discrim_harassed_bin = HRS_2016_data$HRS2016_discrim_harassed_bin
HRS_2016$HRS2016_discrim_lessrespect = HRS_2016_data$HRS2016_discrim_lessrespect
HRS_2016$HRS2016_discrim_lessrespect_bin = HRS_2016_data$HRS2016_discrim_lessrespect_bin
HRS_2016$HRS2016_discrim_medical = HRS_2016_data$HRS2016_discrim_medical
HRS_2016$HRS2016_discrim_medical_bin = HRS_2016_data$HRS2016_discrim_medical_bin
HRS_2016$HRS2016_discrim_notclever = HRS_2016_data$HRS2016_discrim_notclever
HRS_2016$HRS2016_discrim_notclever_bin = HRS_2016_data$HRS2016_discrim_notclever_bin
HRS_2016$HRS2016_discrim_poorerservice = HRS_2016_data$HRS2016_discrim_poorerservice
HRS_2016$HRS2016_discrim_poorerservice_bin = HRS_2016_data$HRS2016_discrim_poorerservice_bin
HRS_2016$HRS2016_height_feet = HRS_2016_data$HRS2016_height_feet
HRS_2016$HRS2016_height_inches = HRS_2016_data$HRS2016_height_inches
HRS_2016$HRS2016_height_meters = HRS_2016_data$HRS2016_height_meters
HRS_2016$HRS2016_limiting_condition_bin = HRS_2016_data$HRS2016_limiting_condition_bin
HRS_2016$HRS2016_normalweight_bin = HRS_2016_data$HRS2016_normalweight_bin
HRS_2016$HRS2016_overweight_bin = HRS_2016_data$HRS2016_overweight_bin
HRS_2016$HRS2016_race = HRS_2016_data$HRS2016_race
HRS_2016$HRS2016_race_black = HRS_2016_data$HRS2016_race_black
HRS_2016$HRS2016_race_hispanic_latino = HRS_2016_data$HRS2016_race_hispanic_latino
HRS_2016$HRS2016_race_nonwhite = HRS_2016_data$HRS2016_race_nonwhite
HRS_2016$HRS2016_race_white = HRS_2016_data$HRS2016_race_white
HRS_2016$HRS2016_reason_discrim1 = HRS_2016_data$HRS2016_reason_discrim1
HRS_2016$HRS2016_reason_discrim1_reason_age = HRS_2016_data$HRS2016_reason_discrim1_reason_age
HRS_2016$HRS2016_reason_discrim1_reason_disability = HRS_2016_data$HRS2016_reason_discrim1_reason_disability
HRS_2016$HRS2016_reason_discrim1_reason_financial = HRS_2016_data$HRS2016_reason_discrim1_reason_financial
HRS_2016$HRS2016_reason_discrim1_reason_gender = HRS_2016_data$HRS2016_reason_discrim1_reason_gender
HRS_2016$HRS2016_reason_discrim1_reason_national = HRS_2016_data$HRS2016_reason_discrim1_reason_national
HRS_2016$HRS2016_reason_discrim1_reason_otherreason = HRS_2016_data$HRS2016_reason_discrim1_reason_otherreason
HRS_2016$HRS2016_reason_discrim1_reason_race = HRS_2016_data$HRS2016_reason_discrim1_reason_race
HRS_2016$HRS2016_reason_discrim1_reason_religion = HRS_2016_data$HRS2016_reason_discrim1_reason_religion
HRS_2016$HRS2016_reason_discrim1_reason_sexuality = HRS_2016_data$HRS2016_reason_discrim1_reason_sexuality
HRS_2016$HRS2016_reason_discrim1_reason_weight = HRS_2016_data$HRS2016_reason_discrim1_reason_weight
HRS_2016$HRS2016_underweight_bin = HRS_2016_data$HRS2016_underweight_bin
HRS_2016$HRS2016_weight_kg = HRS_2016_data$HRS2016_weight_kg
HRS_2016$HRS2016_weight_pounds = HRS_2016_data$HRS2016_weight_pounds
HRS_2016$HRS2016_obese_bin = HRS_2016_data$HRS2016obese_bin




HRS_2016$HRS2016_discrim_harassed_bin = case_when(HRS_2016$HRS2016_discrim_harassed == 1 ~ 1, 
                                                  HRS_2016$HRS2016_discrim_harassed == 2 ~ 1, 
                                                  HRS_2016$HRS2016_discrim_harassed == 3 ~ 1, 
                                                  HRS_2016$HRS2016_discrim_harassed == 4 ~ 1, 
                                                  HRS_2016$HRS2016_discrim_harassed == 5 ~ 0, 
                                                  HRS_2016$HRS2016_discrim_harassed == 6 ~ 0,
                                                  HRS_2016$HRS2016_discrim_harassed == 0 ~ 0) 



HRS_2016$HRS2016_discrim_lessrespect_bin = case_when(HRS_2016$HRS2016_discrim_lessrespect == 1 ~ 1, 
                                                     HRS_2016$HRS2016_discrim_lessrespect == 2 ~ 1, 
                                                     HRS_2016$HRS2016_discrim_lessrespect == 3 ~ 1, 
                                                     HRS_2016$HRS2016_discrim_lessrespect == 4 ~ 1, 
                                                     HRS_2016$HRS2016_discrim_lessrespect == 5 ~ 0, 
                                                     HRS_2016$HRS2016_discrim_lessrespect == 6 ~ 0,
                                                     HRS_2016$HRS2016_discrim_lessrespect == 0 ~ 0) 



HRS_2016$HRS2016_discrim_medical_bin = case_when(HRS_2016$HRS2016_discrim_medical == 1 ~ 1, 
                                                 HRS_2016$HRS2016_discrim_medical == 2 ~ 1, 
                                                 HRS_2016$HRS2016_discrim_medical == 3 ~ 1, 
                                                 HRS_2016$HRS2016_discrim_medical == 4 ~ 1, 
                                                 HRS_2016$HRS2016_discrim_medical == 5 ~ 0, 
                                                 HRS_2016$HRS2016_discrim_medical == 6 ~ 0,
                                                 HRS_2016$HRS2016_discrim_medical == 0 ~ 0) 





HRS_2016$HRS2016_discrim_notclever_bin = case_when(HRS_2016$HRS2016_discrim_notclever == 1 ~ 1, 
                                                   HRS_2016$HRS2016_discrim_notclever == 2 ~ 1, 
                                                   HRS_2016$HRS2016_discrim_notclever == 3 ~ 1, 
                                                   HRS_2016$HRS2016_discrim_notclever == 4 ~ 1, 
                                                   HRS_2016$HRS2016_discrim_notclever == 5 ~ 0, 
                                                   HRS_2016$HRS2016_discrim_notclever == 6 ~ 0,
                                                   HRS_2016$HRS2016_discrim_notclever == 0 ~ 0) 






HRS_2016$HRS2016_discrim_poorerservice_bin = case_when(HRS_2016$HRS2016_discrim_poorerservice == 1 ~ 1, 
                                                       HRS_2016$HRS2016_discrim_poorerservice == 2 ~ 1, 
                                                       HRS_2016$HRS2016_discrim_poorerservice == 3 ~ 1, 
                                                       HRS_2016$HRS2016_discrim_poorerservice == 4 ~ 1, 
                                                       HRS_2016$HRS2016_discrim_poorerservice == 5 ~ 0, 
                                                       HRS_2016$HRS2016_discrim_poorerservice == 6 ~ 0) 




HRS_2016$HRS2016_discrim_afraidothers_bin = case_when(HRS_2016$HRS2016_discrim_afraidothers == 1 ~ 1,
                                                      HRS_2016$HRS2016_discrim_afraidothers == 2 ~ 1, 
                                                      HRS_2016$HRS2016_discrim_afraidothers == 3 ~ 1, 
                                                      HRS_2016$HRS2016_discrim_afraidothers == 4 ~ 1, 
                                                      HRS_2016$HRS2016_discrim_afraidothers == 5 ~ 0, 
                                                      HRS_2016$HRS2016_discrim_afraidothers == 6 ~ 0,
                                                      HRS_2016$HRS2016_discrim_afraidothers == 0 ~ 0) 



HRS_2016$HRS2016_discrim_bin = case_when(HRS_2016$HRS2016_discrim_harassed_bin== 1 | HRS_2016$HRS2016_discrim_lessrespect_bin == 1 | HRS_2016$HRS2016_discrim_medical_bin  == 1 | HRS_2016$HRS2016_discrim_notclever_bin == 1 | HRS_2016$HRS2016_discrim_afraidothers_bin== 1 | HRS_2016$HRS2016_discrim_poorerservice_bin == 1 ~ 1, 
                                         HRS_2016$HRS2016_discrim_harassed_bin== 0 & HRS_2016$HRS2016_discrim_lessrespect_bin == 0 & HRS_2016$HRS2016_discrim_medical_bin  == 0 & HRS_2016$HRS2016_discrim_notclever_bin == 0 & HRS_2016$HRS2016_discrim_afraidothers_bin== 0 & HRS_2016$HRS2016_discrim_poorerservice_bin == 0 ~ 0) 


write.csv(HRS_2016, file = paste(directory, DATA_ROOT, "HRS_2016_data/HRS2016_dataset_latest_renamed_vars.csv", sep=""))
