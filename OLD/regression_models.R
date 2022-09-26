
library(dplyr)
library(tidyr)
library(stats)

#library(survival)
#library(survminer)





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




directory = "/Users/aliyaamirova/"

DATA_ROOT = "Documents/KCL_postDoc/Data_analysis/"
SOURCE_ROOT = "/proj/Cumulative_effects_HRS/Version_2_analysis/"

#check why there are NAs still 

HRS_all_data_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_PGS.csv", sep = ""))
ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = ""))
unique(HRS_all_data_PGS$checklist_depression_bin_t1)

#run chi-sq test for the depression ~ discrim 

depression_t1_gene = glm(checklist_depression_bin_t1 ~ E4_DEPSYMP_SSGAC16, data = HRS_all_data_PGS, family = binomial) 
depression_t1_gene_summary = summary(depression_t1_gene)

#change the below to the coeffcient interaction
HRS_all_data_PGS$interaction_term_discrim_gene_DepSys_t0 = HRS_all_data_PGS$E4_DEPSYMP_SSGAC16 * HRS_all_data_PGS$discrim_bin_t0
unique(HRS_all_data_PGS$interaction_term_discrim_gene_DepSys_t0)

#HRS_all_data_PGS$interaction_term_discrim_gene_DepSys_t0 = as.double(HRS_all_data_PGS$interaction_term_discrim_gene_DepSys_t0)

as.double()
#no data in for the following lm: 

depression_t1_gene_interact = lm(checklist_depression_bin_t1 ~ E4_DEPSYMP_SSGAC16 + discrim_bin_t0 +E4_DEPSYMP_SSGAC16:discrim_bin_t0, data = HRS_all_data_PGS) 
depression_t1_gene_interact = glm(checklist_depression_bin_t1 ~ interaction_term_discrim_gene_DepSys_t0, data = HRS_all_data_PGS) 

vars = c("checklist_depression_bin_t1", "E4_DEPSYMP_SSGAC16", "discrim_bin_t0")
data_depression_t1_gene_interact = drop_na(HRS_all_data_PGS, any_of(vars))

