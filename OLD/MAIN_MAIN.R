

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

######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")

DATA_ROOT = "KCL_postDoc/Data_analysis/"


source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))


ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 



source(paste(SOURCE_ROOT, "MAIN.R", sep=""))
source(paste(SOURCE_ROOT, "MAIN_adjusted.R", sep=""))

ELSA_results_all_discrim_bin_unadjusted

ELSA_results_all_discrim_bin_adjusted$OR
ELSA_results_all_discrim_bin_adjusted$CI1
ELSA_results_all_discrim_bin_adjusted$CI2
ELSA_results_all_discrim_bin_adjusted$p_value



ELSA_discrim_bin = cbind(ELSA_results_all_discrim_bin_unadjusted, 
                         ELSA_results_all_discrim_bin_adjusted$OR, 
                         ELSA_results_all_discrim_bin_adjusted$CI1, 
                         ELSA_results_all_discrim_bin_adjusted$CI2, 
                         ELSA_results_all_discrim_bin_adjusted$p_value)


write.csv(ELSA_discrim_bin, file = paste(SOURCE_ROOT, "ELSA_discrim_bin.csv", sep=""))



source(paste(SOURCE_ROOT, "MAIN_ageism.R", sep=""))
source(paste(SOURCE_ROOT, "MAIN_ageism_adjusted.R", sep=""))

ELSA_ageism = cbind(ELSA_results_ageism_unadjusted, 
                    ELSA_ageism_adjusted$OR, 
                    ELSA_ageism_adjusted$CI1, 
                    ELSA_ageism_adjusted$CI2, 
                    ELSA_ageism_adjusted$p_value) 

write.csv(ELSA_ageism, file = paste(SOURCE_ROOT, "ELSA_ageism.csv", sep=""))

################### HRS 

source(paste(SOURCE_ROOT, "MAIN_HRS.R", sep=""))
source(paste(SOURCE_ROOT, "MAIN_HRS_adjusted.R", sep=""))

source(paste(SOURCE_ROOT, "MAIN_HRS_ageism.R", sep=""))
source(paste(SOURCE_ROOT, "MAIN_HRS_ageism_adjusted.R", sep=""))


HRS_results_all_discrim_bin_unadjusted
HRS_results_all_discrim_bin_adjusted


HRS_discrim_bin = cbind(HRS_results_all_discrim_bin_unadjusted, 
                        HRS_results_all_discrim_bin_adjusted$OR, 
                        HRS_results_all_discrim_bin_adjusted$CI1, 
                        HRS_results_all_discrim_bin_adjusted$CI2, 
                        HRS_results_all_discrim_bin_adjusted$p_value)

write.csv(HRS_discrim_bin, file = paste(SOURCE_ROOT, "HRS_discrim_bin.csv", sep=""))
#######

HRS_results_ageism_unadjusted
HRS_results_ageism_adjusted

HRS_ageism = cbind(HRS_results_ageism_unadjusted, 
                    HRS_results_ageism_adjusted$OR, 
                    HRS_results_ageism_adjusted$CI1, 
                    HRS_results_ageism_adjusted$CI2, 
                    HRS_results_ageism_adjusted$p_value) 

write.csv(HRS_ageism, file = paste(SOURCE_ROOT, "HRS_ageism.csv", sep=""))
