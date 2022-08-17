
library(dplyr)
library(tidyr)
library(stats)
library(survival)
library(survminer)

#in this analysis cases are included by merging dataframes by year/wave not by personal baseline 

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

###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



DATA_ROOT = "KCL_postDoc/Data_analysis/"


# harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))


all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



######### HRS 
#all_HRS_by_years_PGS$HRS2010_discrim_bin
discrimination_var_ELSA =  "w5discrim_bin2" 

discrimination_var_HRS = "HRS2010_discrim_bin" 

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

#R10HRTATT -- HRS2010 
#R12HRTATT -- HRS2012 
#R13HRTATT -- HRS2014 
#R14HRTATT -- HRS2016
# harmonised_data_all_waves$HHIDPN = harmonised_data_all_waves$hhidpn
# 
# ID_hhidpn = unique(all_HRS_by_years_PGS$HHIDPN)
# 
# harmonised_data_all_waves = subset(harmonised_data_all_waves, harmonised_data_all_waves$HHIDPN %in% c(ID_hhidpn))
# 
# #harmonised_data_all_waves = case_when(harmonised_data_all_waves, harmonised_data_all_waves$hhidpn == )
# 
# all_HRS_by_years_PGS$HRS2010_mi_bin = case_when(harmonised_data_all_waves$r10hrtatt == 0 ~ 0, 
#                                                 harmonised_data_all_waves$r10hrtatt == 1 ~ 1)
# 
# 
# all_HRS_by_years_PGS$HRS2012_mi_bin = case_when(harmonised_data_all_waves$r11hrtatt == 0 ~ 0, 
#                                                 harmonised_data_all_waves$r11hrtatt == 1 ~ 1)
# 
# 
# all_HRS_by_years_PGS$HRS2014_mi_bin = case_when(harmonised_data_all_waves$r12hrtatt == 0 ~ 0, 
#                                                 harmonised_data_all_waves$r12hrtatt == 1 ~ 1)
# 
# 
# all_HRS_by_years_PGS$HRS2016_mi_bin = case_when(harmonised_data_all_waves$r13hrtatt == 0 ~ 0, 
#                                                 harmonised_data_all_waves$r13hrtatt == 1 ~ 1)
# 
# 
# all_HRS_by_years_PGS$HRS2018_mi_bin = case_when(harmonised_data_all_waves$r14hrtatt == 0 ~ 0, 
#                                                 harmonised_data_all_waves$r14hrtatt == 1 ~ 1)

# unique(harmonised_data_all_waves$r10hrtatt)
# unique(harmonised_data_all_waves$r12hrtatt)
# unique(harmonised_data_all_waves$r13hrtatt)
# unique(harmonised_data_all_waves$r14hrtatt)
# 
# unique(harmonised_data_all_waves$r10hrtatt)
# unique(harmonised_data_all_waves$r12hrtatt)
# unique(harmonised_data_all_waves$r13hrtatt)
# unique(harmonised_data_all_waves$r14hrtatt)



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
                              discrimination_VAR_elsa = discrimination_var_ELSA)

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
                              discrimination_VAR_elsa = discrimination_var_ELSA)

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
                              discrimination_VAR_elsa = discrimination_var_ELSA)

print("done MI8 v2")

########################################

############ HRS 


#all_HRS_by_years_PGS$HRS2012_mi
MI_w6_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,

                              analysis_variable_name = "HRS2012_mi_bin",
                              wave_number = "wave 6",
                              outcome_name = "MI",
                              dataset = "HRS",



                              outcome_ELSA = "HRS2012_mi_bin",

                              gene_ELSA = "E4_MI_CARDIOGRAM15",

                              covariate1 = "NA",
                              covariate2 = "NA",
                              covariate3 = "NA",
                              covariate4 = "NA",
                              discrimination_VAR_elsa = discrimination_var_HRS)

print("done MI6 v2")

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
                              discrimination_VAR_elsa = discrimination_var_HRS)

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
                              discrimination_VAR_elsa = discrimination_var_HRS)

print("done MI8 v2")

########################################

mi_results_ELSA = rbind(MI_w6_ELSA, 
                         MI_w7_ELSA, 
                         MI_w8_ELSA)

mi_results_HRS = rbind(MI_w6_HRS, 
                       MI_w7_HRS, 
                       MI_w8_HRS)

