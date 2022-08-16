
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


harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))


all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



######### HRS 
#all_HRS_by_years_PGS$HRS2010_discrim_bin

discrimination_var = "HRS2010_discrim_bin" 

##### 
#####   IMPORTANT 
#####
# add HRS (R10HRTATT, R12HRTATT, R13HRTATT, R14HRTATT) : harmonised_data_all_waves

#R11HRTATT -- heart attack since last wave in the harmonised file (HRS, 11 means HRS 2012) 
#0.no
#1.yes
#.d:DK
#.m:Missing
#.r:Refuse

#harmonised_data_all_waves$R10HRTATT -- HRS2010 
#R12HRTATT -- HRS2012 
#R13HRTATT -- HRS2014 
#R14HRTATT -- HRS2016


print("unique(all_HRS_by_years_PGS$HRS2012_hypertension_new_bin) # all answers are either 1 or N, check") 



########################################

######### MI


MI_w6_ELSA = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                              
                              analysis_variable_name = "MI_wave6", 
                              wave_number = "wave 6",
                              outcome_name = "MI", 
                              dataset = "ELSA", 
                              
                              
                              
                              outcome_ELSA = "w6_MI_new_bin", 
                              
                              gene_ELSA = "MI", 
                              
                              covariate1 = "NA", 
                              covariate2 = "NA",
                              covariate3 = "NA", 
                              covariate4 = "NA", 
                              discrimination_VAR_elsa = discrimination_var)

print("done MI6 v2")


MI_w7_ELSA  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                              
                              analysis_variable_name = "MI_wave7", 
                              wave_number = "wave 7",
                              outcome_name = "MI", 
                              dataset = "ELSA", 
                              
                              
                              
                              outcome_ELSA = "w7_MI_new_bin", 
                              
                              gene_ELSA = "MI", 
                              
                              covariate1 = "NA", 
                              covariate2 = "NA",
                              covariate3 = "NA", 
                              covariate4 = "NA", 
                              discrimination_VAR_elsa = discrimination_var)

print("done MI7 v2")


MI_w8_ELSA  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                              
                              analysis_variable_name = "MI_wave8", 
                              wave_number = "wave 8",
                              outcome_name = "MI", 
                              dataset = "ELSA", 
                              
                              
                              
                              outcome_ELSA = "w8_MI_new_bin", 
                              
                              gene_ELSA = "MI", 
                              
                              covariate1 = "NA", 
                              covariate2 = "NA",
                              covariate3 = "NA", 
                              covariate4 = "NA", 
                              discrimination_VAR_elsa = discrimination_var)

print("done MI8 v2")

########################################

############ HRS 


#all_HRS_by_years_PGS$HRS2012_mi
# MI_w6_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
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

MI_w7_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                              
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


MI_w8_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                              
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

mi_results_ELSA = rbind(MI_w6_ELSA, 
                         MI_w7_ELSA, 
                         MI_w8_ELSA)

mi_results_HRS = rbind(MI_w6_HRS, 
                       MI_w7_HRS, 
                       MI_w8_HRS)

