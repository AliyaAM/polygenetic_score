





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



directory = "/Users/aliya/my_docs/"

#"/Users/aliyaamirova/"

DATA_ROOT = "KCL_postDoc/Data_analysis/"

ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 



######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))





arthritis_discrim_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                             
                                             analysis_variable_name = "arthritis_wave6", 
                                             wave_number = "wave_6",
                                             outcome_name = "w6arthritis_new", 
                                             
                                             subsetting_VAR1_ELSA = "NA", 
                                             subsetting_VAR2_ELSA = "NA",
                                             
                                             ELSA_var1_value = "NA",
                                             
                                             ELSA_var2_value = "NA",
                                             
                                             outcome_ELSA = "w6arthritis_new", 
                                             
                                             gene_ELSA = "RA", 
                                             
                                             covariate1 = "NA", 
                                             covariate2 = "NA",
                                             covariate3 = "NA", 
                                             covariate4 = "NA", 
                                             discrimination_VAR_elsa = "w5discrim_bin2")
  
  

