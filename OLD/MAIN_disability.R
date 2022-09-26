





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

DATA_ROOT = "KCL_postDoc/Data_analysis/"

ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 



######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")


###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))


#subsetting_VAR1_ELSA =  "w5sex_1_0"
ELSA_var1_value = 1

subsetting_VAR1_ELSA = "w5limill"

#discrimination_var = "w5discrim_bin2" 

#discrimination_var  = "w5agediscrimination2" 

discrimination_var  = "w5disabilitydiscrimination2" 

#discrimination_var  = "w5sexdiscrimination2" 

#discrimination_var = "w5racediscrimination2" #MI is coming out as sig. when run on the total sample
#discrimination_var = "w5discrim_financial2"
#discrimination_var = "w5weightdiscrimination2"



unique(ELSA_data_with_PGS$w6arthritis_new)
# 
# arthritis_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
#                                      
#                                      analysis_variable_name = "arthritis_wave6", 
#                                      wave_number = "wave 6",
#                                      outcome_name = "Arthritis", 
#                                      dataset = "ELSA", 
#                                      
#                                      subsetting_VAR1_ELSA =subsetting_VAR1_ELSA,
#                                      subsetting_VAR2_ELSA = "NA",
#                                      
#                                      ELSA_var1_value = 0,
#                                      
#                                      ELSA_var2_value = "NA",
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
# arthritis_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
#                                      
#                                      analysis_variable_name = "arthritis_wave7", 
#                                      wave_number = "wave 7",
#                                      outcome_name = "Arthritis", 
#                                      dataset = "ELSA", 
#                                      
#                                      subsetting_VAR1_ELSA =subsetting_VAR1_ELSA,
#                                      subsetting_VAR2_ELSA = "NA",
#                                      
#                                      ELSA_var1_value = 0,
#                                      
#                                      ELSA_var2_value = "NA",
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
# arthritis_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS,
# 
#                                      analysis_variable_name = "arthritis_wave8",
#                                      wave_number = "wave 8",
#                                      outcome_name = "Arthritis",
#                                      dataset = "ELSA",
# 
#                                     subsetting_VAR1_ELSA =subsetting_VAR1_ELSA,
#                                      subsetting_VAR2_ELSA = "NA",
# 
#                                      ELSA_var1_value = 0,
# 
#                                      ELSA_var2_value = "NA",
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

ELSA_data_with_PGS = subsetting_function(data_ELSA =ELSA_data_with_PGS, 
                                         subsetting_VAR1_ELSA = subsetting_VAR1_ELSA,
                                         subsetting_VAR2_ELSA = "NA",
                                         ELSA_var1_value = ELSA_var1_value,
                                         ELSA_var2_value = "NA")

unique(ELSA_data_with_PGS$w6diabetes_new)
unique(ELSA_data_with_PGS$T2D_2018)
unique(ELSA_data_with_PGS$w5disabilitydiscrimination2)

# glm_test = glm(w6diabetes_new ~ T2D_2018 * w5sexdiscrimination2, family = binomial,
#     data = ELSA_data_with_PGS)
# 
# summary(glm_test)
# glm_test$coefficients
# 
# glm_test_confint = confint(glm_test, "T2D_2018:w5disabilitydiscrimination2")
# 
# 
# T2DM_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS,
# 
#                                 analysis_variable_name = "w6diabetes_new",
#                                 wave_number = "wave 6",
#                                 outcome_name = "T2DM",
#                                 dataset = "ELSA",
# 
# 
#                                 outcome_ELSA = "w6diabetes_new",
# 
#                                 gene_ELSA = "T2D_2018",
# 
#                                 covariate1 = "NA",
#                                 covariate2 = "NA",
#                                 covariate3 = "NA",
#                                 covariate4 = "NA",
#                                 discrimination_VAR_elsa = discrimination_var)
# 
# confint(T2DM_w6, "gene:discrimination")
# 
# print("done T2DM6")


# T2DM_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS,
# 
#                                 analysis_variable_name = "w7diabetes_new",
#                                 wave_number = "wave 7",
#                                 outcome_name = "T2DM",
#                                 dataset = "ELSA",
# 
# 
# 
#                                 outcome_ELSA = "w6diabetes_new",
# 
#                                 gene_ELSA = "T2D_2018",
# 
#                                 covariate1 = "NA",
#                                 covariate2 = "NA",
#                                 covariate3 = "NA",
#                                 covariate4 = "NA",
#                                 discrimination_VAR_elsa = discrimination_var)
# 
# print("done T2DM7")
#

# T2DM_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS,
# 
#                                 analysis_variable_name = "w8diabetes_new",
#                                 wave_number = "wave 8",
#                                 outcome_name = "T2DM",
#                                 dataset = "ELSA",
# 
# 
# 
#                                 outcome_ELSA = "w8diabetes_new",
# 
#                                 gene_ELSA = "T2D_2018",
# 
#                                 covariate1 = "NA",
#                                 covariate2 = "NA",
#                                 covariate3 = "NA",
#                                 covariate4 = "NA",
#                                 discrimination_VAR_elsa = discrimination_var)
# 
# print("done T2DM8")


########## Diabetes v2


# T2DM_w6_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
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



# T2DM_w7_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
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


# T2DM_w8_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
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


MI_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS,

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

# 
# MI_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
#                               
#                               analysis_variable_name = "MI_wave7", 
#                               wave_number = "wave 7",
#                               outcome_name = "MI", 
#                               dataset = "ELSA", 
#                               
#                               
#                               outcome_ELSA = "w7_MI_new_bin", 
#                               
#                               gene_ELSA = "MI", 
#                               
#                               covariate1 = "NA", 
#                               covariate2 = "NA",
#                               covariate3 = "NA", 
#                               covariate4 = "NA", 
#                               discrimination_VAR_elsa = discrimination_var)
# 
# print("done MI7 v2")


MI_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                              
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



widespread_pain_bin_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                               
                                               analysis_variable_name = "widespread_pain_bin_wave6", 
                                               wave_number = "wave 6",
                                               outcome_name = "widespread_pain_bin", 
                                               dataset = "ELSA", 
                                               
                                               
                                               
                                               outcome_ELSA = "w6_widespread_pain_bin", 
                                               
                                               gene_ELSA = "chronic_pain_2018", 
                                               
                                               covariate1 = "NA", 
                                               covariate2 = "NA",
                                               covariate3 = "NA", 
                                               covariate4 = "NA", 
                                               discrimination_VAR_elsa = discrimination_var)

print("done widespread_pain_bin6 v2")


# widespread_pain_bin_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
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

widespread_pain_bin_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                               
                                               analysis_variable_name = "widespread_pain_bin_wave8", 
                                               wave_number = "wave 8",
                                               outcome_name = "widespread_pain_bin", 
                                               dataset = "ELSA", 
                                               
                                               
                                               
                                               outcome_ELSA = "w8_widespread_pain_bin", 
                                               
                                               gene_ELSA = "chronic_pain_2018", 
                                               
                                               covariate1 = "NA", 
                                               covariate2 = "NA",
                                               covariate3 = "NA", 
                                               covariate4 = "NA", 
                                               discrimination_VAR_elsa = discrimination_var)
print("done widespread_pain_bin8 v2")

#######################################



# pain_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
#                                      
#                                      analysis_variable_name = "Pain_wave6", 
#                                      wave_number = "wave 6",
#                                      outcome_name = "Pain", 
#                                      dataset = "ELSA", 
#                                      

#                                      
#                                      outcome_ELSA = "w6_pain_bin", 
#                                      
#                                      gene_ELSA = "chronic_pain_2018", 
#                                      
#                                      covariate1 = "NA", 
#                                      covariate2 = "NA",
#                                      covariate3 = "NA", 
#                                      covariate4 = "NA", 
#                                      discrimination_VAR_elsa = discrimination_var)
# 
# print("done P6 v2")


pain_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                
                                analysis_variable_name = "Pain_wave7", 
                                wave_number = "wave 7",
                                outcome_name = "Pain", 
                                dataset = "ELSA", 
                                
                                
                                
                                outcome_ELSA = "w7_pain_bin", 
                                
                                gene_ELSA = "chronic_pain_2018", 
                                
                                covariate1 = "NA", 
                                covariate2 = "NA",
                                covariate3 = "NA", 
                                covariate4 = "NA", 
                                discrimination_VAR_elsa = discrimination_var)


print("done P7 v2")

pain_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                
                                analysis_variable_name = "Pain_wave8", 
                                wave_number = "wave 8",
                                outcome_name = "Pain", 
                                dataset = "ELSA", 
                                
                                
                                
                                outcome_ELSA = "w8_pain_bin", 
                                
                                gene_ELSA = "chronic_pain_2018", 
                                
                                covariate1 = "NA", 
                                covariate2 = "NA",
                                covariate3 = "NA", 
                                covariate4 = "NA", 
                                discrimination_VAR_elsa = discrimination_var)
print("done P8 v2")


########################################



sleep_disturbance_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                             
                                             analysis_variable_name = "sleep_disturbance_wave6", 
                                             wave_number = "wave 6",
                                             outcome_name = "sleep_disturbance", 
                                             dataset = "ELSA", 
                                             
                                             
                                             
                                             outcome_ELSA = "w6_sleep_disturbance_bin", 
                                             
                                             gene_ELSA = "INS_COM", 
                                             
                                             covariate1 = "NA", 
                                             covariate2 = "NA",
                                             covariate3 = "NA", 
                                             covariate4 = "NA", 
                                             discrimination_VAR_elsa = discrimination_var)


print("done SD6")

# sleep_disturbance_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
#                                      
#                                      analysis_variable_name = "sleep_disturbance_wave7", 
#                                      wave_number = "wave 7",
#                                      outcome_name = "sleep_disturbance", 
#                                      dataset = "ELSA", 
#                                      

#                                      outcome_ELSA = "w7_sleep_disturbance_bin", 
#                                      
#                                      gene_ELSA = "INS_COM", 
#                                      
#                                      covariate1 = "NA", 
#                                      covariate2 = "NA",
#                                      covariate3 = "NA", 
#                                      covariate4 = "NA", 
#                                      discrimination_VAR_elsa = discrimination_var)



sleep_disturbance_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                             
                                             analysis_variable_name = "sleep_disturbance_wave8", 
                                             wave_number = "wave 8",
                                             outcome_name = "sleep_disturbance", 
                                             dataset = "ELSA", 
                                             
                                             
                                             
                                             outcome_ELSA = "w8_sleep_disturbance_bin", 
                                             
                                             gene_ELSA = "INS_COM", 
                                             
                                             covariate1 = "NA", 
                                             covariate2 = "NA",
                                             covariate3 = "NA", 
                                             covariate4 = "NA", 
                                             discrimination_VAR_elsa = discrimination_var)


print("done SD8")


########################################

######### cesd_bin (DS or MDD19)



Depression_w6 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                      
                                      analysis_variable_name = "w6_depression_bin", 
                                      wave_number = "wave 6",
                                      outcome_name = "Depression", 
                                      dataset = "ELSA", 
                                      
                                      
                                      #outcome_ELSA = "w6cesd_bin", 
                                      outcome_ELSA = "w6_depression_bin", 
                                      
                                      gene_ELSA = "MDD19", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)

print("done D6")


Depression_w7 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                      
                                      analysis_variable_name = "w7_depression_bin", 
                                      wave_number = "wave 7",
                                      outcome_name = "Depression", 
                                      dataset = "ELSA", 
                                      
                                      
                                      
                                      #outcome_ELSA = "w7cesd_bin", 
                                      outcome_ELSA = "w7_depression_bin", 
                                      
                                      gene_ELSA = "MDD19", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)

print("done D7")


Depression_w8 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                      
                                      analysis_variable_name = "w8_depression_bin", 
                                      wave_number = "wave 8",
                                      outcome_name = "Depression", 
                                      dataset = "ELSA", 
                                      
                                      
                                      
                                      #outcome_ELSA = "w8cesd_bin", 
                                      outcome_ELSA = "w8_depression_bin",  
                                      
                                      gene_ELSA = "MDD19", 
                                      
                                      covariate1 = "NA", 
                                      covariate2 = "NA",
                                      covariate3 = "NA", 
                                      covariate4 = "NA", 
                                      discrimination_VAR_elsa = discrimination_var)
print("done D8")

######### cesd_bin (DS or MDD19)


Depression_w6_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                         
                                         analysis_variable_name = "w6_depression_bin", 
                                         wave_number = "wave 6",
                                         outcome_name = "Depression", 
                                         dataset = "ELSA", 
                                         
                                         
                                         
                                         #outcome_ELSA = "w8cesd_bin", 
                                         outcome_ELSA = "w6_depression_bin",  
                                         
                                         gene_ELSA = "DS", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)

print("done D6 v2")


Depression_w7_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                         
                                         analysis_variable_name = "w7_depression_bin", 
                                         wave_number = "wave 7",
                                         outcome_name = "Depression", 
                                         dataset = "ELSA", 
                                         
                                         
                                         
                                         #outcome_ELSA = "w7cesd_bin", 
                                         outcome_ELSA = "w7_depression_bin",  
                                         
                                         gene_ELSA = "DS", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)


print("done D7 v2")
unique(ELSA_data_with_PGS$w8cesd_bin) 

Depression_w8_v2 = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                         
                                         analysis_variable_name = "w8_depression_bin", 
                                         wave_number = "wave 8",
                                         outcome_name = "Depression", 
                                         dataset = "ELSA", 
                                         
                                         
                                         #outcome_ELSA = "w8cesd_bin", 
                                         outcome_ELSA = "w8_depression_bin",  
                                         
                                         
                                         gene_ELSA = "DS", 
                                         
                                         covariate1 = "NA", 
                                         covariate2 = "NA",
                                         covariate3 = "NA", 
                                         covariate4 = "NA", 
                                         discrimination_VAR_elsa = discrimination_var)

print("done D8 v2")


ELSA_unadjusted_results_wave_6 = rbind(#arthritis_w6, 
  #T2DM_w6,
  #T2DM_w6_v2,
  #MI_w6,
  #pain_w6, p value = 1
  widespread_pain_bin_w6, 
  sleep_disturbance_w6,
  Depression_w6,
  Depression_w6_v2) 



ELSA_unadjusted_results_wave_7 = rbind(#arthritis_w7, 
  #T2DM_w7,
  #T2DM_w7_v2,
  #MI_w7,
  pain_w7,
  #widespread_pain_bin_w7, 
  #sleep_disturbance_w7,
  Depression_w7,
  Depression_w7_v2) 



ELSA_unadjusted_results_wave_8 = rbind(#arthritis_w8, 
  #T2DM_w8,
  #T2DM_w8_v2,
  MI_w8,
  widespread_pain_bin_w8, 
  pain_w8,
  sleep_disturbance_w8,
  Depression_w8,
  Depression_w8_v2) 
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


print("interesting that there is an interactive effect between MIgene and discrimination on the onset of  pain, see comment below")

# interesting: 

#below is sig: 

# pain_bin_geneMI_interaction_w8  = glm(w8_pain_bin ~ MI * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
# summary(pain_bin_geneMI_interaction_w8)


