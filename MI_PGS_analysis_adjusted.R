
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

######### to do: 

#Simply control for age, sex and wealth (done) 
#Once you introduce the polygenic score control for principal components 
#Derive a binary variable (new MI over waves 6-8) 

directory = "/Users/aliya/my_docs/"

#"/Users/aliyaamirova/"

###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



###### sourcing code for the adjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))

#source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



DATA_ROOT = "KCL_postDoc/Data_analysis/"


# harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))

all_HRS_by_years_PGS_before_subsetting_to_baseline_free = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
ELSA_data_with_PGS_before_subsetting_to_baseline_free = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
#all_HRS_by_years_PGS_before_subsetting_to_baseline_free$HRS2010_diabetes_ever
#all_HRS_by_years_PGS_before_subsetting_to_baseline_free$HRS2010_diabetes_new

ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

#ELSA_data_with_PGS  = subset(ELSA_data_with_PGS_before_subsetting_to_baseline_free, ELSA_data_with_PGS_before_subsetting_to_baseline_free$ == 0 & ELSA_data_with_PGS_before_subsetting_to_baseline_free$ == 0)
#all_HRS_by_years_PGS  = subset(all_HRS_by_years_PGS_before_subsetting_to_baseline_free, all_HRS_by_years_PGS_before_subsetting_to_baseline_free$ == 0 & all_HRS_by_years_PGS_before_subsetting_to_baseline_free$ == 0)

#unique(all_HRS_by_years_PGS$HRS2012_race_nonwhite)
#all_HRS_by_years_PGS = subset(all_HRS_by_years_PGS, all_HRS_by_years_PGS$HRS2012_race 

######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/MI_adjusted_discrim_bin/", sep = "")
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")



######### HRS 
#all_HRS_by_years_PGS$HRS2010_discrim_bin 

discrimination_var_ELSA =  "w5discrim_bin2" 

discrimination_var_HRS = "HRS2010_discrim_bin" 


covariate1_ELSA = "w5wealth"
covariate2_ELSA = "NA"
covariate3_ELSA = "NA"
covariate4_ELSA = "NA"

covariate1_HRS = "HRS2010_wealth_noIRA"
covariate2_HRS = "NA"
covariate3_HRS = "NA"
covariate4_HRS = "NA"

#covariate4_ELSA = "NA"
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

ELSA_data_with_PGS$age = ELSA_data_with_PGS$w5age
ELSA_data_with_PGS$sex = ELSA_data_with_PGS$w5sex


all_HRS_by_years_PGS$pc1 = all_HRS_by_years_PGS$PC1_5A
all_HRS_by_years_PGS$pc2 = all_HRS_by_years_PGS$PC1_5B
all_HRS_by_years_PGS$pc3 = all_HRS_by_years_PGS$PC1_5C
all_HRS_by_years_PGS$pc4 = all_HRS_by_years_PGS$PC1_5D
all_HRS_by_years_PGS$pc5 = all_HRS_by_years_PGS$PC1_5E 
all_HRS_by_years_PGS$pc6 = all_HRS_by_years_PGS$PC6_10A
all_HRS_by_years_PGS$pc7 = all_HRS_by_years_PGS$PC6_10B
all_HRS_by_years_PGS$pc8 = all_HRS_by_years_PGS$PC6_10C
all_HRS_by_years_PGS$pc9 = all_HRS_by_years_PGS$PC6_10D
all_HRS_by_years_PGS$pc10 = all_HRS_by_years_PGS$PC6_10E

all_HRS_by_years_PGS$age = all_HRS_by_years_PGS$HRS2010_continious_age
all_HRS_by_years_PGS$sex = all_HRS_by_years_PGS$HRS2010_sex_1_0


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
                                   
                                   covariate1 = covariate1_ELSA, 
                                   covariate2 = covariate2_ELSA,
                                   covariate3 = covariate3_ELSA, 
                                   covariate4 = "NA", 
                                   discrimination_VAR_elsa = discrimination_var_ELSA)




MI_w7_ELSA  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                    
                                    analysis_variable_name = "MI_wave7", 
                                    wave_number = "wave 7",
                                    outcome_name = "MI", 
                                    dataset = "ELSA", 
                                    
                                    
                                    
                                    outcome_ELSA = "w7_MI_new_bin", 
                                    
                                    gene_ELSA = "MI", 
                                    
                                    covariate1 = covariate1_ELSA, 
                                    covariate2 = covariate2_ELSA,
                                    covariate3 = covariate3_ELSA, 
                                    covariate4 = "NA",  
                                    discrimination_VAR_elsa = discrimination_var_ELSA)



MI_w8_ELSA  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                    
                                    analysis_variable_name = "MI_wave8", 
                                    wave_number = "wave 8",
                                    outcome_name = "MI", 
                                    dataset = "ELSA", 
                                    
                                    
                                    
                                    outcome_ELSA = "w8_MI_new_bin", 
                                    
                                    gene_ELSA = "MI", 
                                    
                                    covariate1 = covariate1_ELSA, 
                                    covariate2 = covariate2_ELSA,
                                    covariate3 = covariate3_ELSA, 
                                    covariate4 = "NA", 
                                    discrimination_VAR_elsa = discrimination_var_ELSA)





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
                                  
                                  covariate1 = covariate1_HRS,
                                  covariate2 = covariate2_HRS,
                                  covariate3 = covariate3_HRS,
                                  covariate4 = "NA",
                                  
                                  
                                  discrimination_VAR_elsa = discrimination_var_HRS)



MI_w7_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                  
                                  analysis_variable_name = "HRS2014_mi_bin",
                                  wave_number = "wave 7",
                                  outcome_name = "MI",
                                  dataset = "HRS",
                                  
                                  
                                  
                                  outcome_ELSA = "HRS2014_mi_bin",
                                  
                                  gene_ELSA = "E4_MI_CARDIOGRAM15",
                                  
                                  covariate1 = covariate1_HRS,
                                  covariate2 = covariate2_HRS,
                                  covariate3 = covariate3_HRS,
                                  covariate4 = "NA",
                                  discrimination_VAR_elsa = discrimination_var_HRS)




MI_w8_HRS = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                  
                                  analysis_variable_name = "HRS2016_mi_bin",
                                  wave_number = "wave 8",
                                  outcome_name = "MI",
                                  dataset = "HRS",
                                  
                                  
                                  
                                  outcome_ELSA = "HRS2016_mi_bin",
                                  
                                  gene_ELSA = "E4_MI_CARDIOGRAM15",
                                  
                                  covariate1 = covariate1_HRS,
                                  covariate2 = covariate2_HRS,
                                  covariate3 = covariate3_HRS,
                                  covariate4 = "NA",
                                  discrimination_VAR_elsa = discrimination_var_HRS)


########################################



########################################
########################################
######################################## adjusted results 
########################################
########################################

mi_results_ELSA = data.frame(MI_w6_ELSA, 
                             MI_w7_ELSA, 
                             MI_w8_ELSA)

write.csv(mi_results_ELSA, file = paste(OUTPUT_ROOT, "mi_results_ELSA_adjusted.csv", sep = ""))




mi_results_HRS = data.frame(MI_w6_HRS, 
                            MI_w7_HRS, 
                            MI_w8_HRS)

write.csv(mi_results_HRS, file = paste(OUTPUT_ROOT, "mi_results_HRS_adjusted.csv", sep = ""))

########################################

#some literature adjust for top one (ELSA): https://www.cambridge.org/core/journals/psychological-medicine/article/longterm-effects-of-a-polygenetic-predisposition-to-general-cognition-on-healthy-cognitive-ageing-evidence-from-the-english-longitudinal-study-of-ageing/091461C7CA0BA6FC54DAA69A8BBD6CB8
#some adjust for top four (ELSA): https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002827


#adjusting for principal components 
ELSA_data_with_PGS$pc1
ELSA_data_with_PGS$pc2
ELSA_data_with_PGS$pc3
ELSA_data_with_PGS$pc4
ELSA_data_with_PGS$pc5
ELSA_data_with_PGS$pc6
ELSA_data_with_PGS$pc7
ELSA_data_with_PGS$pc8
ELSA_data_with_PGS$pc9
ELSA_data_with_PGS$pc10



MI_w6_ELSA_pca = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                       
                                       analysis_variable_name = "MI_wave6", 
                                       wave_number = "wave 6",
                                       outcome_name = "MI", 
                                       dataset = "ELSA", 
                                       
                                       
                                       
                                       outcome_ELSA = "w6_MI_new_bin", 
                                       
                                       gene_ELSA = "MI", 
                                       
                                       covariate1 = covariate1_ELSA, 
                                       covariate2 = covariate2_ELSA,
                                       covariate3 = covariate3_ELSA, 
                                       covariate4 = covariate4_ELSA, 
                                       discrimination_VAR_elsa = discrimination_var_ELSA)



MI_w7_ELSA_pca  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                        
                                        analysis_variable_name = "MI_wave7", 
                                        wave_number = "wave 7",
                                        outcome_name = "MI", 
                                        dataset = "ELSA", 
                                        
                                        
                                        
                                        outcome_ELSA = "w7_MI_new_bin", 
                                        
                                        gene_ELSA = "MI", 
                                        
                                        covariate1 = covariate1_ELSA, 
                                        covariate2 = covariate2_ELSA,
                                        covariate3 = covariate3_ELSA, 
                                        covariate4 = covariate4_ELSA, 
                                        discrimination_VAR_elsa = discrimination_var_ELSA)




MI_w8_ELSA_pca  = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                        
                                        analysis_variable_name = "MI_wave8", 
                                        wave_number = "wave 8",
                                        outcome_name = "MI", 
                                        dataset = "ELSA", 
                                        
                                        
                                        
                                        outcome_ELSA = "w8_MI_new_bin", 
                                        
                                        gene_ELSA = "MI", 
                                        
                                        covariate1 = covariate1_ELSA, 
                                        covariate2 = covariate2_ELSA,
                                        covariate3 = covariate3_ELSA, 
                                        covariate4 = covariate4_ELSA,  
                                        discrimination_VAR_elsa = discrimination_var_ELSA)




###############  ELSA MI_composite


ELSA_data_with_PGS$MI_composite = case_when(ELSA_data_with_PGS$w6_MI_new_bin == 0 | ELSA_data_with_PGS$w7_MI_new_bin == 0 | ELSA_data_with_PGS$w8_MI_new_bin == 0 ~ 0, 
                                            ELSA_data_with_PGS$w6_MI_new_bin == 1 | ELSA_data_with_PGS$w7_MI_new_bin == 1 | ELSA_data_with_PGS$w8_MI_new_bin == 1 ~ 1) 

unique(ELSA_data_with_PGS$MI_composite)
unique(ELSA_data_with_PGS$w6_MI_new_bin)
unique(ELSA_data_with_PGS$w7_MI_new_bin)
unique(ELSA_data_with_PGS$w8_MI_new_bin)

MI_ELSA_composite = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                          
                                          analysis_variable_name = "MI_composite", 
                                          wave_number = "wave 6",
                                          outcome_name = "MI", 
                                          dataset = "ELSA", 
                                          
                                          
                                          
                                          outcome_ELSA = "MI_composite", 
                                          
                                          gene_ELSA = "MI", 
                                          
                                          covariate1 = covariate1_ELSA, 
                                          covariate2 = covariate2_ELSA,
                                          covariate3 = covariate3_ELSA, 
                                          covariate4 = "NA", 
                                          discrimination_VAR_elsa = discrimination_var_ELSA)

write.csv(MI_ELSA_composite, file = paste(OUTPUT_ROOT, "MI_ELSA_composite_adjusted.csv", sep = ""))

#missing 0 in unique(ELSA_data_with_PGS$MI_composite)

MI_ELSA_composite_pca = PGS_glm_function_ELSA(data_ELSA = ELSA_data_with_PGS, 
                                              
                                              analysis_variable_name = "MI_composite", 
                                              wave_number = "wave 6",
                                              outcome_name = "MI", 
                                              dataset = "ELSA", 
                                              
                                              
                                              
                                              outcome_ELSA = "MI_composite", 
                                              
                                              gene_ELSA = "MI", 
                                              
                                              covariate1 = covariate1_ELSA, 
                                              covariate2 = covariate2_ELSA,
                                              covariate3 = covariate3_ELSA, 
                                              covariate4 = covariate4_ELSA, 
                                              discrimination_VAR_elsa = discrimination_var_ELSA)

write.csv(MI_ELSA_composite_pca, file = paste(OUTPUT_ROOT, "MI_ELSA_composite_pca_adjusted.csv", sep = ""))

#####

########################################

#adjusting for principal components 
all_HRS_by_years_PGS$PC1_5A
all_HRS_by_years_PGS$PC1_5B
all_HRS_by_years_PGS$PC1_5C
all_HRS_by_years_PGS$PC1_5D
all_HRS_by_years_PGS$PC1_5E
all_HRS_by_years_PGS$PC6_10A
all_HRS_by_years_PGS$PC6_10B
all_HRS_by_years_PGS$PC6_10C
all_HRS_by_years_PGS$PC6_10D
all_HRS_by_years_PGS$PC6_10E


#all_HRS_by_years_PGS$HRS2012_mi
MI_w6_HRS_pca = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                      
                                      analysis_variable_name = "HRS2012_mi_bin",
                                      wave_number = "wave 6",
                                      outcome_name = "MI",
                                      dataset = "HRS",
                                      
                                      
                                      
                                      outcome_ELSA = "HRS2012_mi_bin",
                                      
                                      gene_ELSA = "E4_MI_CARDIOGRAM15",
                                      
                                      covariate1 = covariate1_HRS,
                                      covariate2 = covariate2_HRS,
                                      covariate3 = covariate3_HRS,
                                      covariate4 = covariate4_HRS,
                                      discrimination_VAR_elsa = discrimination_var_HRS)



#gene is positively asssociated with MI phenotype (when only PC1_5A is included as a covariate), when al four are included still associated 
MI_w7_HRS_pca = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                      
                                      analysis_variable_name = "HRS2014_mi_bin",
                                      wave_number = "wave 7",
                                      outcome_name = "MI",
                                      dataset = "HRS",
                                      
                                      
                                      
                                      outcome_ELSA = "HRS2014_mi_bin",
                                      
                                      gene_ELSA = "E4_MI_CARDIOGRAM15",
                                      
                                      covariate1 = covariate1_HRS,
                                      covariate2 = covariate2_HRS,
                                      covariate3 = covariate3_HRS,
                                      covariate4 = covariate4_HRS,
                                      discrimination_VAR_elsa = discrimination_var_HRS)




MI_w8_HRS_pca = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                      
                                      analysis_variable_name = "HRS2016_mi_bin",
                                      wave_number = "wave 8",
                                      outcome_name = "MI",
                                      dataset = "HRS",
                                      
                                      
                                      
                                      outcome_ELSA = "HRS2016_mi_bin",
                                      
                                      gene_ELSA = "E4_MI_CARDIOGRAM15",
                                      
                                      covariate1 = covariate1_HRS,
                                      covariate2 = covariate2_HRS,
                                      covariate3 = covariate3_HRS,
                                      covariate4 = covariate4_HRS,
                                      discrimination_VAR_elsa = discrimination_var_HRS)


########################################
########################################
######################################## adjusted results (pca) 
########################################
########################################

mi_results_ELSA_pca = rbind(MI_w6_ELSA_pca, 
                            MI_w7_ELSA_pca, 
                            MI_w8_ELSA_pca)


write.csv(mi_results_ELSA_pca, file = paste(OUTPUT_ROOT, "mi_results_ELSA_pca_adjusted.csv", sep = ""))


mi_results_HRS_pca = rbind(MI_w6_HRS_pca, 
                           MI_w7_HRS_pca, 
                           MI_w8_HRS_pca)

write.csv(mi_results_HRS_pca, file = paste(OUTPUT_ROOT, "mi_results_HRS_pca_adjusted.csv", sep = ""))

########################################
########################################
unique(all_HRS_by_years_PGS$HRS2016_mi_bin)
unique(all_HRS_by_years_PGS$HRS2014_mi_bin)
unique(all_HRS_by_years_PGS$HRS2012_mi_bin)

all_HRS_by_years_PGS$MI_composite = case_when(all_HRS_by_years_PGS$HRS2016_mi_bin == 0 & all_HRS_by_years_PGS$HRS2014_mi_bin == 0 & all_HRS_by_years_PGS$HRS2012_mi_bin == 0 ~ 0, 
                                              all_HRS_by_years_PGS$HRS2016_mi_bin == 1 | all_HRS_by_years_PGS$HRS2014_mi_bin == 1 | all_HRS_by_years_PGS$HRS2012_mi_bin == 1 ~ 1) 

unique(all_HRS_by_years_PGS$MI_composite) 

table(all_HRS_by_years_PGS$MI_composite)
table(all_HRS_by_years_PGS$HRS2016_mi_bin)
table(all_HRS_by_years_PGS$HRS2014_mi_bin)
table(all_HRS_by_years_PGS$HRS2012_mi_bin)



#all_HRS_by_years_PGS$HRS2012_mi
MI_HRS_composite = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                         
                                         analysis_variable_name = "MI_composite",
                                         wave_number = "wave 6",
                                         outcome_name = "MI",
                                         dataset = "HRS",
                                         
                                         
                                         
                                         outcome_ELSA = "MI_composite",
                                         
                                         gene_ELSA = "E4_MI_CARDIOGRAM15",
                                         
                                         covariate1 = covariate1_HRS,
                                         covariate2 = covariate2_HRS,
                                         covariate3 = covariate3_HRS,
                                         covariate4 = "NA",
                                         discrimination_VAR_elsa = discrimination_var_HRS)

write.csv(MI_HRS_composite, file = paste(OUTPUT_ROOT, "MI_HRS_composite_adjusted.csv", sep = ""))


#all_HRS_by_years_PGS$HRS2012_mi
MI_HRS_composite_pca = PGS_glm_function_ELSA(data_ELSA = all_HRS_by_years_PGS,
                                             
                                             analysis_variable_name = "MI_composite",
                                             wave_number = "wave 6",
                                             outcome_name = "MI",
                                             dataset = "HRS",
                                             
                                             
                                             
                                             outcome_ELSA = "MI_composite",
                                             
                                             gene_ELSA = "E4_MI_CARDIOGRAM15",
                                             
                                             covariate1 = covariate1_HRS,
                                             covariate2 = covariate2_HRS,
                                             covariate3 = covariate3_HRS,
                                             covariate4 = covariate4_HRS,
                                             discrimination_VAR_elsa = discrimination_var_HRS)


########################################
########################################
######################################## adjusted results, composite (pca) 
########################################
########################################

write.csv(MI_HRS_composite_pca, file = paste(OUTPUT_ROOT, "MI_HRS_composite_pca_adjusted.csv", sep = ""))

