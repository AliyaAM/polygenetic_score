





library(dplyr)
library(tidyr)
library(stats)
library(survival)
library(survminer)

#in this analysis cases are included by merging dataframes by year/wave not by personal baseline 
unique(all_HRS_by_years_PGS$HRS2018_race_white)
unique(all_HRS_by_years_PGS$HRS2014_race_white)

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
print("ELSA anxiety: now new var, but wha ttype emotional psychiatric problem do you have")  
print("no _new vars for CAD, ALZ, only: how old were you when you were diagnosed with CAD and ALZ") 


directory = "/Users/aliya/my_docs/"

#"/Users/aliyaamirova/"

DATA_ROOT = "KCL_postDoc/Data_analysis/"

all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 


######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



######### HRS 
#all_HRS_by_years_PGS$HRS2010_discrim_bin

discrimination_var = "HRS2010_discrim_bin" 

print("unique(all_HRS_by_years_PGS$HRS2012_hypertension_new_bin) # all answers are either 1 or N, check") 




######## ELSA: 

#discrimination_var  = "w5agediscrimination2" 
#discrimination_var  = "w5disabilitydiscrimination2" #not enough data when run on the total sample
#discrimination_var  = "w5sexdiscrimination2" nothing is significant when run on the total sample
#discrimination_var = "w5racediscrimination2" #MI is coming out as sig. when run on the total sample
#discrimination_var = "w5discrim_financial2"
#discrimination_var = "w5weightdiscrimination2"


###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



#print("add all_HRS_by_years_PGS:  PTSD, anxiety, alcohol dependence,  smoking cessation, smoking initiation, sig per day, drinks per day" )

############### Add analysis of PGS x discrimination on the following outcomes: 

# all_HRS_by_years_PGS$HRS2010_self_rated_health
# all_HRS_by_years_PGS$HRS2010_life_satisfaction 
# all_HRS_by_years_PGS$HRS2010_alzheimer_bin 

#polygenic scores (already added)
#E4_WELLB_SSGAC16 - subjective wellbeing 
#E4_PTSDEA_PGC18  - PTSD   
#"E4_ANXFS_ANGST16", #anxiety factor 
#E4_ANXCC_ANGST16#anxiety control 
##"E4_ALC_PGC18" # alcohol dependence 
#E4_BMI2_GIANT18 # BMI 

###################

#"E4_EDU2_SSGAC16", 
#"E4_BMI_GIANT15", 
#"E4_SCZ_PGC14", 
#"E4_EVRSMK_TAG10", 
#"E4_AD2_IGAP13", 
#"E4_WC_GIANT15", 
#"E4_NEUROT_SSGAC16", 
#"E4_CD_CARDIOGRAM11", 



#ADD to HRS: 
print('separately add health behaviours: discrimination x PGS analysis ')
#health behaviour: 
#E4_SCP_GSCAN19  smoking cessation 
#E4_SI_GSCAN19 smoking initiation 
#E4_CPD_GSCAN19 sig per day 
#E4_DPW_GSCAN19 drinks per week  


# unique(all_HRS_by_years_PGS$w6diabetes_new)
# all_HRS_by_years_PGS$HRS2008_BMI
# all_HRS_by_years_PGS$HRS2008_smokes_now
# all_HRS_by_years_PGS$HRS2008_diabetes_new
# all_HRS_by_years_PGS$HRS2008_checklist_depression_bin
# all_HRS_by_years_PGS$HRS2010_alcohol_days_week
# all_HRS_by_years_PGS$HRS2008_discrim_bin
# all_HRS_by_years_PGS$HRS2008_reason_discrim1_reason_age
# all_HRS_by_years_PGS$HRS2012_race_white
# all_HRS_by_years_PGS$HRS2012_obese_bin





all_HRS_by_years_PGS = subsetting_function(data_ELSA =all_HRS_by_years_PGS, 
                                         subsetting_VAR1_ELSA = "NA",
                                         subsetting_VAR2_ELSA = "NA",
                                         ELSA_var1_value = "NA",
                                         ELSA_var2_value = "NA")



# 
# arthritis_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                      
#                                      analysis_variable_name = "arthritis_wave6", 
#                                      wave_number = "wave 6",
#                                      outcome_name = "Arthritis", 
#                                      dataset = "ELSA", 
#                                      
#                                      
#                                      
#                                      outcome_ELSA = "w6arthritis_new", 
#                                      
#                                      gene_ELSA = "RA", 
#                                      
#                                      covariate1 = "NA", 
#                                      covariate2 = "NA",
#                                      covariate3 = "NA", 
#                                      covariate4 = "NA", 
#                                      discrimination_VAR_elsa = discrimination_var)
# 
# 
# print("done A6")
# 
# arthritis_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                      
#                                      analysis_variable_name = "arthritis_wave7", 
#                                      wave_number = "wave 7",
#                                      outcome_name = "Arthritis", 
#                                      dataset = "ELSA", 
#                                      
#                                      
#                                      
#                                      outcome_ELSA = "w7arthritis_new", 
#                                      
#                                      gene_ELSA = "RA", 
#                                      
#                                      covariate1 = "NA", 
#                                      covariate2 = "NA",
#                                      covariate3 = "NA", 
#                                      covariate4 = "NA", 
#                                      discrimination_VAR_elsa = discrimination_var)
# 
# print("done A7")
# 
# 
# arthritis_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                      
#                                      analysis_variable_name = "arthritis_wave8", 
#                                      wave_number = "wave 8",
#                                      outcome_name = "Arthritis", 
#                                      dataset = "ELSA", 
#                                      
#                                      
#                                      
#                                      outcome_ELSA = "w8arthritis_new", 
#                                      
#                                      gene_ELSA = "RA", 
#                                      
#                                      covariate1 = "NA", 
#                                      covariate2 = "NA",
#                                      covariate3 = "NA", 
#                                      covariate4 = "NA", 
#                                      discrimination_VAR_elsa = discrimination_var)
# 
# print("done A8")

############

#hypertension_new_bin 
#E4_HTN_COGENT17 #hypertension 

# 
# hypertension_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                 
#                                 analysis_variable_name = "hypertension", 
#                                 wave_number = "wave 6",
#                                 outcome_name = "hypertension", 
#                                 dataset = "HRS", 
#                                 
#                                 
#                                 outcome_ELSA = "HRS2012_hypertension_new_bin", 
#                                 
#                                 gene_ELSA = "E4_HTN_COGENT17", 
#                                 
#                                 covariate1 = "NA", 
#                                 covariate2 = "NA",
#                                 covariate3 = "NA", 
#                                 covariate4 = "NA", 
#                                 discrimination_VAR_elsa = discrimination_var)
# 
# print("done hypertension_w6")
# 
# 
# hypertension_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                         
#                                         analysis_variable_name = "hypertension", 
#                                         wave_number = "wave 7",
#                                         outcome_name = "hypertension", 
#                                         dataset = "HRS", 
#                                         
#                                         
#                                         outcome_ELSA = "HRS2014_hypertension_new_bin", 
#                                         
#                                         gene_ELSA = "E4_HTN_COGENT17", 
#                                         
#                                         covariate1 = "NA", 
#                                         covariate2 = "NA",
#                                         covariate3 = "NA", 
#                                         covariate4 = "NA", 
#                                         discrimination_VAR_elsa = discrimination_var)
# 
# print("done hypertension_w7")
# 
# 
# 
# hypertension_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                         
#                                         analysis_variable_name = "hypertension", 
#                                         wave_number = "wave 8",
#                                         outcome_name = "hypertension", 
#                                         dataset = "HRS", 
#                                         
#                                         
#                                         outcome_ELSA = "HRS2016_hypertension_new_bin", 
#                                         
#                                         gene_ELSA = "E4_HTN_COGENT17", 
#                                         
#                                         covariate1 = "NA", 
#                                         covariate2 = "NA",
#                                         covariate3 = "NA", 
#                                         covariate4 = "NA", 
#                                         discrimination_VAR_elsa = discrimination_var)
# 
# print("done hypertension_w8")

########################


T2DM_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                
                                analysis_variable_name = "diabetes_new", 
                                wave_number = "wave 6",
                                outcome_name = "T2DM", 
                                dataset = "HRS", 
                                
                                
                                outcome_ELSA = "HRS2012_diabetes_new", 
                                
                                gene_ELSA = "E4_T2D_DIAGRAM12", 
                                
                                covariate1 = "NA", 
                                covariate2 = "NA",
                                covariate3 = "NA", 
                                covariate4 = "NA", 
                                discrimination_VAR_elsa = discrimination_var)

print("done T2DM6")


T2DM_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                
                                analysis_variable_name = "diabetes_new", 
                                wave_number = "wave 7",
                                outcome_name = "T2DM", 
                                dataset = "HRS", 
                                
                                
                                outcome_ELSA = "HRS2014_diabetes_new", 
                                
                                gene_ELSA = "E4_T2D_DIAGRAM12", 
                                
                                covariate1 = "NA", 
                                covariate2 = "NA",
                                covariate3 = "NA", 
                                covariate4 = "NA", 
                                discrimination_VAR_elsa = discrimination_var)

print("done T2DM7")


T2DM_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                
                                analysis_variable_name = "diabetes_new", 
                                wave_number = "wave 8",
                                outcome_name = "T2DM", 
                                dataset = "HRS", 
                                
                                
                                
                                outcome_ELSA = "HRS2016_diabetes_new", 
                                
                                gene_ELSA = "E4_T2D_DIAGRAM12", 
                                
                                covariate1 = "NA", 
                                covariate2 = "NA",
                                covariate3 = "NA", 
                                covariate4 = "NA", 
                                discrimination_VAR_elsa = discrimination_var)

print("done T2DM8")


########## Diabetes v2

# 
# T2DM_w6_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                    
#                                    analysis_variable_name = "w6diabetes_new", 
#                                    wave_number = "wave 6",
#                                    outcome_name = "T2DM", 
#                                    dataset = "ELSA", 
#                                    
#                                    
#                                    
#                                    outcome_ELSA = "w6diabetes_new", 
#                                    
#                                    gene_ELSA = "Diabetes", 
#                                    
#                                    covariate1 = "NA", 
#                                    covariate2 = "NA",
#                                    covariate3 = "NA", 
#                                    covariate4 = "NA", 
#                                    discrimination_VAR_elsa = discrimination_var)
# print("done T2DM6 v2")
# 
# 
# 
# T2DM_w7_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                    
#                                    analysis_variable_name = "w7diabetes_new", 
#                                    wave_number = "wave 7",
#                                    outcome_name = "T2DM", 
#                                    dataset = "ELSA", 
#                                    
#                                    
#                                    
#                                    outcome_ELSA = "w6diabetes_new", 
#                                    
#                                    gene_ELSA = "Diabetes", 
#                                    
#                                    covariate1 = "NA", 
#                                    covariate2 = "NA",
#                                    covariate3 = "NA", 
#                                    covariate4 = "NA", 
#                                    discrimination_VAR_elsa = discrimination_var)
# 
# print("done T2DM7 v2")
# 
# 
# T2DM_w8_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                    
#                                    analysis_variable_name = "w8diabetes_new", 
#                                    wave_number = "wave 8",
#                                    outcome_name = "T2DM", 
#                                    dataset = "ELSA", 
#                                    
#                                    
#                                    
#                                    outcome_ELSA = "w8diabetes_new", 
#                                    
#                                    gene_ELSA = "Diabetes", 
#                                    
#                                    covariate1 = "NA", 
#                                    covariate2 = "NA",
#                                    covariate3 = "NA", 
#                                    covariate4 = "NA", 
#                                    discrimination_VAR_elsa = discrimination_var)
# 
# print("done T2DM8 v2")


########################################

######### MI


unique(all_HRS_by_years_PGS$HRS2010_mi_bin)
#unique(all_HRS_by_years_PGS$HRS2012_mi) no data 
unique(all_HRS_by_years_PGS$HRS2014_mi_bin)
unique(all_HRS_by_years_PGS$HRS2016_mi_bin)

#all_HRS_by_years_PGS$HRS2012_mi
# MI_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
# 
#                               analysis_variable_name = "HRS2012_mi",
#                               wave_number = "wave 6",
#                               outcome_name = "MI",
#                               dataset = "HRS",
# 
# 
# 
#                               outcome_ELSA = "HRS2012_mi",
# 
#                               gene_ELSA = "E4_MI_CARDIOGRAM15",
# 
#                               covariate1 = "NA",
#                               covariate2 = "NA",
#                               covariate3 = "NA",
#                               covariate4 = "NA",
#                               discrimination_VAR_elsa = discrimination_var)
# 
# print("done MI6 v2")

MI_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,

                              analysis_variable_name = "HRS2014_mi_bin",
                              wave_number = "wave 7",
                              outcome_name = "MI",
                              dataset = "HRS",



                              outcome_ELSA = "HRS2014_mi_bin",

                              gene_ELSA = "E4_MI_CARDIOGRAM15",

                              covariate1 = "NA",
                              covariate2 = "NA",
                              covariate3 = "NA",
                              covariate4 = "NA",
                              discrimination_VAR_elsa = discrimination_var)

print("done MI7 v2")


MI_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,

                              analysis_variable_name = "HRS2016_mi_bin",
                              wave_number = "wave 8",
                              outcome_name = "MI",
                              dataset = "HRS",



                              outcome_ELSA = "HRS2016_mi_bin",

                              gene_ELSA = "E4_MI_CARDIOGRAM15",

                              covariate1 = "NA",
                              covariate2 = "NA",
                              covariate3 = "NA",
                              covariate4 = "NA",
                              discrimination_VAR_elsa = discrimination_var)

print("done MI8 v2")

########################################

# 
# 
# widespread_pain_bin_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                                
#                                                analysis_variable_name = "widespread_pain_bin_wave6", 
#                                                wave_number = "wave 6",
#                                                outcome_name = "widespread_pain_bin", 
#                                                dataset = "ELSA", 
#                                                
#                                                
#                                                outcome_ELSA = "w6_widespread_pain_bin", 
#                                                
#                                                gene_ELSA = "chronic_pain_2018", 
#                                                
#                                                covariate1 = "NA", 
#                                                covariate2 = "NA",
#                                                covariate3 = "NA", 
#                                                covariate4 = "NA", 
#                                                discrimination_VAR_elsa = discrimination_var)
# 
# print("done widespread_pain_bin6 v2")
# 
# 
# widespread_pain_bin_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                                
#                                                analysis_variable_name = "widespread_pain_bin_wave7", 
#                                                wave_number = "wave 7",
#                                                outcome_name = "widespread_pain_bin", 
#                                                dataset = "ELSA", 
#                                                
#                                                
#                                                
#                                                outcome_ELSA = "w7_widespread_pain_bin", 
#                                                
#                                                gene_ELSA = "chronic_pain_2018", 
#                                                
#                                                covariate1 = "NA", 
#                                                covariate2 = "NA",
#                                                covariate3 = "NA", 
#                                                covariate4 = "NA", 
#                                                discrimination_VAR_elsa = discrimination_var)
# 
# 
# print("done widespread_pain_bin7 v2")
# 
# widespread_pain_bin_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                                
#                                                analysis_variable_name = "widespread_pain_bin_wave8", 
#                                                wave_number = "wave 8",
#                                                outcome_name = "widespread_pain_bin", 
#                                                dataset = "ELSA", 
#                                                
#                                                
#                                                outcome_ELSA = "w8_widespread_pain_bin", 
#                                                
#                                                gene_ELSA = "chronic_pain_2018", 
#                                                
#                                                covariate1 = "NA", 
#                                                covariate2 = "NA",
#                                                covariate3 = "NA", 
#                                                covariate4 = "NA", 
#                                                discrimination_VAR_elsa = discrimination_var)
# print("done widespread_pain_bin8 v2")
# 
# #######################################
# 
# 
# 
# # pain_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
# #                                      
# #                                      analysis_variable_name = "Pain_wave6", 
# #                                      wave_number = "wave 6",
# #                                      outcome_name = "Pain", 
# #                                      dataset = "ELSA", 
# #                                      
# #                             
# #                                      
# #                                      outcome_ELSA = "w6_pain_bin", 
# #                                      
# #                                      gene_ELSA = "chronic_pain_2018", 
# #                                      
# #                                      covariate1 = "NA", 
# #                                      covariate2 = "NA",
# #                                      covariate3 = "NA", 
# #                                      covariate4 = "NA", 
# #                                      discrimination_VAR_elsa = discrimination_var)
# # 
# # print("done P6 v2")
# 
# 
# pain_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                 
#                                 analysis_variable_name = "Pain_wave7", 
#                                 wave_number = "wave 7",
#                                 outcome_name = "Pain", 
#                                 dataset = "ELSA", 
#                                 
#                                 
#                                 
#                                 outcome_ELSA = "w7_pain_bin", 
#                                 
#                                 gene_ELSA = "chronic_pain_2018", 
#                                 
#                                 covariate1 = "NA", 
#                                 covariate2 = "NA",
#                                 covariate3 = "NA", 
#                                 covariate4 = "NA", 
#                                 discrimination_VAR_elsa = discrimination_var)
# 
# 
# print("done P7 v2")
# 
# pain_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                 
#                                 analysis_variable_name = "Pain_wave8", 
#                                 wave_number = "wave 8",
#                                 outcome_name = "Pain", 
#                                 dataset = "ELSA", 
#                                 
#                                 
#                                 
#                                 outcome_ELSA = "w8_pain_bin", 
#                                 
#                                 gene_ELSA = "chronic_pain_2018", 
#                                 
#                                 covariate1 = "NA", 
#                                 covariate2 = "NA",
#                                 covariate3 = "NA", 
#                                 covariate4 = "NA", 
#                                 discrimination_VAR_elsa = discrimination_var)
# print("done P8 v2")
# 
# 
# ########################################
# 
# 
# 
# sleep_disturbance_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                              
#                                              analysis_variable_name = "sleep_disturbance_wave6", 
#                                              wave_number = "wave 6",
#                                              outcome_name = "sleep_disturbance", 
#                                              dataset = "ELSA", 
#                                              
#                                              
#                                              
#                                              outcome_ELSA = "w6_sleep_disturbance_bin", 
#                                              
#                                              gene_ELSA = "INS_COM", 
#                                              
#                                              covariate1 = "NA", 
#                                              covariate2 = "NA",
#                                              covariate3 = "NA", 
#                                              covariate4 = "NA", 
#                                              discrimination_VAR_elsa = discrimination_var)
# 
# 
# print("done SD6")
# 
# # sleep_disturbance_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
# #                                      
# #                                      analysis_variable_name = "sleep_disturbance_wave7", 
# #                                      wave_number = "wave 7",
# #                                      outcome_name = "sleep_disturbance", 
# #                                      dataset = "ELSA", 
# #                                      
# #                                     
# #                                      
# #                                      outcome_ELSA = "w7_sleep_disturbance_bin", 
# #                                      
# #                                      gene_ELSA = "INS_COM", 
# #                                      
# #                                      covariate1 = "NA", 
# #                                      covariate2 = "NA",
# #                                      covariate3 = "NA", 
# #                                      covariate4 = "NA", 
# #                                      discrimination_VAR_elsa = discrimination_var)
# 
# 
# 
# sleep_disturbance_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
#                                              
#                                              analysis_variable_name = "sleep_disturbance_wave8", 
#                                              wave_number = "wave 8",
#                                              outcome_name = "sleep_disturbance", 
#                                              dataset = "ELSA", 
#                                              
#                                              
#                                              
#                                              outcome_ELSA = "w8_sleep_disturbance_bin", 
#                                              
#                                              gene_ELSA = "INS_COM", 
#                                              
#                                              covariate1 = "NA", 
#                                              covariate2 = "NA",
#                                              covariate3 = "NA", 
#                                              covariate4 = "NA", 
#                                              discrimination_VAR_elsa = discrimination_var)
# 
# 
# print("done SD8")
# 
# 
# ########################################
# 
# ######### cesd_bin (DS or MDD19)



Depression_w6 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                      
                                      analysis_variable_name = "checklist_depression_bin", 
                                      wave_number = "wave 6",
                                      outcome_name = "Depression", 
                                      dataset = "HRS", 
                                      
                                      
                                      
                                      #outcome_ELSA = "w6cesd_bin", 
                                      outcome_ELSA = "HRS2012_checklist_depression_bin", 
                                      
                                      gene_ELSA = "E4_MDD_PGC13", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)

print("done D6")


Depression_w7 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                      
                                      analysis_variable_name = "checklist_depression_bin", 
                                      wave_number = "wave 7",
                                      outcome_name = "Depression", 
                                      dataset = "HRS", 
                                      
                                      
                                      #outcome_ELSA = "w7cesd_bin", 
                                      outcome_ELSA = "HRS2014_checklist_depression_bin", 
                                      
                                      gene_ELSA = "E4_MDD_PGC13", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)

print("done D7")


Depression_w8 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                      
                                      analysis_variable_name = "checklist_depression_bin", 
                                      wave_number = "wave 8",
                                      outcome_name = "Depression", 
                                      dataset = "HRS", 
                                      
                                      
                                      
                                      #outcome_ELSA = "w8cesd_bin", 
                                      outcome_ELSA = "HRS2016_checklist_depression_bin",  
                                      
                                      gene_ELSA = "E4_MDD_PGC13", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)
print("done D8")

######### cesd_bin (DS or MDD19)


Depression_w6_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                         
                                         analysis_variable_name = "checklist_depression_bin", 
                                         wave_number = "wave 6",
                                         outcome_name = "Depression", 
                                         dataset = "HRS", 
                                         
                                         
                                         #outcome_ELSA = "w8cesd_bin", 
                                         outcome_ELSA = "HRS2012_checklist_depression_bin",  
                                         
                                         gene_ELSA = "E4_DEPSYMP_SSGAC16", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)

print("done D6 v2")


Depression_w7_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                         
                                         analysis_variable_name = "checklist_depression_bin", 
                                         wave_number = "wave 7",
                                         outcome_name = "Depression", 
                                         dataset = "HRS", 
                                         
                                         
                                         
                                         #outcome_ELSA = "w7cesd_bin", 
                                         outcome_ELSA = "HRS2014_checklist_depression_bin",  
                                         
                                         gene_ELSA = "E4_DEPSYMP_SSGAC16", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)


print("done D7 v2")
unique(all_HRS_by_years_PGS$w8cesd_bin) 

Depression_w8_v2 = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS, 
                                         
                                         analysis_variable_name = "checklist_depression_bin", 
                                         wave_number = "wave 8",
                                         outcome_name = "Depression", 
                                         dataset = "HRS", 
                                         
                                         
                                         
                                         #outcome_ELSA = "w8cesd_bin", 
                                         outcome_ELSA = "HRS2016_checklist_depression_bin",  
                                         
                                         
                                         gene_ELSA = "E4_DEPSYMP_SSGAC16", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)

print("done D8 v2")


unadjusted_results_wave_6 = rbind(#arthritis_w6,
  #hypertension_w6, 
  T2DM_w6,
  #T2DM_w6_v2,
  #MI_w6,
  #pain_w6, p value = 1
  #widespread_pain_bin_w6, 
  #sleep_disturbance_w6,
  Depression_w6,
  Depression_w6_v2) 



unadjusted_results_wave_7 = rbind(#arthritis_w7, 
  #hypertension_w7,
  T2DM_w7,
  #T2DM_w7_v2,
  #MI_w7,
  #pain_w7,
  #widespread_pain_bin_w7, 
  #sleep_disturbance_w7,
  Depression_w7,
  Depression_w7_v2) 



unadjusted_results_wave_8 = rbind(#arthritis_w8, 
  #hypertension_w8,
  T2DM_w8,
  #T2DM_w8_v2,
  #MI_w8,
  #widespread_pain_bin_w8, 
  #pain_w8,
  #sleep_disturbance_w8,
  Depression_w8,
  Depression_w8_v2) 



###########
###########

###########
###########

###########
###########

# hypertension_results = rbind(hypertension_w6,
#                              hypertension_w7,
#                              hypertension_w8)



diabetes_results = rbind(T2DM_w6, 
                         T2DM_w7, 
                         T2DM_w8)


# diabetes_results_v2 = rbind(T2DM_w6_v2, 
#                             T2DM_w7_v2, 
#                             T2DM_w8_v2)
# 
# 
# mi_results = rbind(MI_w6, 
#                    MI_w7, 
#                    MI_w8)
# 
# pain_w6 = c("Pain_w6", "ELSA", "wave 6", "Pain", "NA", "NA", "NA", "NA", "NA", "NA") 
# 
# 
# pain_results = rbind(pain_w6, 
#                      pain_w7,
#                      pain_w8)
# 
# widespread_pain_results = rbind(widespread_pain_bin_w6, 
#                                 widespread_pain_bin_w7, 
#                                 widespread_pain_bin_w8) 
# 
# sleep_disturbance_w7 = c("sleep_disturbance_w7", "ELSA", "wave 7", "sleep_disturbance", "NA", "NA", "NA", "NA", "NA", "NA") 
# 
# sleep_disturbance_results = rbind(sleep_disturbance_w6,
#                                   sleep_disturbance_w7, 
#                                   sleep_disturbance_w8)


depression_results = rbind(Depression_w6, 
                           Depression_w7, 
                           Depression_w8)


depression_results_v2 = rbind(Depression_w6_v2, 
                              Depression_w7_v2, 
                              Depression_w8_v2)

##############################
##############################



HRS_results_all_discrim_bin_unadjusted = rbind(#hypertension_results, 
                                               diabetes_results,  
                                          
                                                #mi_results,  
                                
                                                depression_results, 
                                                depression_results_v2) 

write.csv(HRS_results_all_discrim_bin_unadjusted, file = paste(SOURCE_ROOT, "HRS_results_all_discrim_bin_unadjusted.csv", sep=""))


##############################
##############################
##############################
##############################
##############################
##############################


#   round results 

# arthritis_discrim_w6 %>% round(arthritis_discrim_w6$OR, digits = 2)
# 
# round(arthritis_discrim_w6$CI1, digits = 2)
# 
# round(arthritis_discrim_w6$CI2, digits = 2)
# 
# round(arthritis_discrim_w6$p_value, digits = 4)


##### arthritis from the harmonised file 
print("add arthritis from the harmonised file")
#	Harmonized ELSA	Section B: Health	
#Description:
#  r6arthrs:w6 R had arthritis since last IW
#Response type:
#  Enumerated
#Responses:
#  0.no
#1.yes
#.c:no prev IW
#.r:Refuse
print("add dibetes from sscratch")



# interesting: 

#below is sig: 

# pain_bin_geneMI_interaction_w8  = glm(w8_pain_bin ~ MI * w5discrim_bin2, data = all_HRS_by_years_PGS, family = binomial)
# summary(pain_bin_geneMI_interaction_w8)


