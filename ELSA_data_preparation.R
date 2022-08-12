library(dplyr)
library(tidyr)
library(stats)

library(survival)
library(survminer)





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




directory = "/Users/aliya/my_docs"

#"/Users/aliyaamirova/"

DATA_ROOT = "/KCL_postDoc/Data_analysis/"
SOURCE_ROOT = "/proj/Cumulative_effects_HRS/Version_2_analysis/"

source((paste(directory, SOURCE_ROOT, "sort_timepoints.R", sep="")))

#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data
#/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Version_2_analysis

###########
###########

#ELSA wave 5: 
ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep = ""))
#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DATA_ELSA


ELSA_data_wave_6 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave_6_elsa_data_v2.csv", sep = ""))
ELSA_data_wave_7 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave_7_elsa_data.csv", sep = ""))
ELSA_data_wave_8 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave8.csv", sep = ""))


#HRS polygenic scores data: 
polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "/HRS_polygenetic_scores_biomarkers/pgenscore4e_r.csv", sep = ""))

#polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/pgenscore4e_r.csv", sep = ""))

#ELSA polygenic scores data: 
polygenic_scores_ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 

#polygenic_scores_ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 

#subset polygenic scores data to those who provided data for the main survey 
ID_ELSA = unique(ELSA_data$idauniq)
ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]

#subset main ELSA data to thos ewho provided PGS data 
ID_ELSA_PGS_unique =unique(polygenic_scores_data$idauniq)
#ELSA_data_PGS_subset = ELSA_data[polygenic_scores_data$idauniq %in% ID_ELSA_PGS_unique, ]

#nrow(ELSA_data_PGS_subset)
#nrow(ELSA_data_polygenic_scores)

#join PGS scores with ELSA main dataset 
ELSA_data_with_PGS = bind_cols(ELSA_data, ELSA_data_polygenic_scores) 




print("ELSA MI is below for wave 5")
#MI in ELSA: 
#wave 5: ELSA_data_with_PGS$henmmi
#add other waves from different files (also called henmmi)
#number of heart attacks in the last two years
#Responses:
#1 None
#1 1
#2 2
#3 3 or more
print("ELSA MI add for wave6, wave7, wave8")

ID_data_with_PGS =unique(ELSA_data_with_PGS$idauniq)


ELSA_data_wave_6 = ELSA_data_wave_6[ELSA_data_with_PGS$idauniq %in% ID_data_with_PGS,]
ELSA_data_wave_7 = ELSA_data_wave_7[ELSA_data_with_PGS$idauniq %in% ID_data_with_PGS,]
ELSA_data_wave_8 = ELSA_data_wave_8[ELSA_data_with_PGS$idauniq %in% ID_data_with_PGS,]

unique(ELSA_data$henmmi)

w5_MI_new = case_when(ELSA_data$henmmi == 0 ~ 0, 
                      ELSA_data$henmmi == 1 ~ 1, 
                      ELSA_data$henmmi == 2 ~ 2, 
                      ELSA_data$henmmi == 3 ~ 3)


w5_MI_new_bin = case_when(w5_MI_new == 0 ~ 0, 
                          w5_MI_new == 1 ~ 1,
                          w5_MI_new == 2 ~ 1,
                          w5_MI_new == 3 ~ 1)
                      
#ELSA_data_wave_6$HeNmMI

w6_MI_new = case_when(ELSA_data_wave_6$HeNmMI == 0 ~ 0, 
                      ELSA_data_wave_6$HeNmMI == 1 ~ 1, 
                      ELSA_data_wave_6$HeNmMI == 2 ~ 2, 
                      ELSA_data_wave_6$HeNmMI == 3 ~ 3)


w6_MI_new_bin = case_when(w6_MI_new == 0 ~ 0, 
                          w6_MI_new == 1 ~ 1,
                          w6_MI_new == 2 ~ 1,
                          w6_MI_new == 3 ~ 1)


#ELSA_data_wave_7$HeNmMI

w7_MI_new = case_when(ELSA_data_wave_7$HeNmMI == 0 ~ 0, 
                      ELSA_data_wave_7$HeNmMI == 1 ~ 1, 
                      ELSA_data_wave_7$HeNmMI == 2 ~ 2, 
                      ELSA_data_wave_7$HeNmMI == 3 ~ 3)


w7_MI_new_bin = case_when(w7_MI_new == 0 ~ 0, 
                          w7_MI_new == 1 ~ 1,
                          w7_MI_new == 2 ~ 1,
                          w7_MI_new == 3 ~ 1)

#ELSA_data_wave_8$henmmi

w8_MI_new = case_when(ELSA_data_wave_8$henmmi == 0 ~ 0, 
                      ELSA_data_wave_8$henmmi == 1 ~ 1, 
                      ELSA_data_wave_8$henmmi == 2 ~ 2, 
                      ELSA_data_wave_8$henmmi == 3 ~ 3)


w8_MI_new_bin = case_when(w8_MI_new == 0 ~ 0, 
                          w8_MI_new == 1 ~ 1,
                          w8_MI_new == 2 ~ 1,
                          w8_MI_new == 3 ~ 1)

MI_data =data.frame(w5_MI_new,
                    w5_MI_new_bin,
                    w6_MI_new,
                    w6_MI_new_bin,
                    w7_MI_new,
                    w7_MI_new_bin,
                    w8_MI_new,
                    w8_MI_new_bin)

colnames(MI_data) = c("w5_MI_new",
                       "w5_MI_new_bin",
                       "w6_MI_new",
                       "w6_MI_new_bin",
                       "w7_MI_new",
                       "w7_MI_new_bin",
                       "w8_MI_new",
                       "w8_MI_new_bin")


ELSA_data_with_PGS = bind_cols(ELSA_data_with_PGS, MI_data) 

unique(ELSA_data_with_PGS$w5_MI_new)

#ELSA_data_with_PGS$age (check min age is above 50 in ELSA)
print(min(ELSA_data_with_PGS$w8age, na.rm = TRUE))

#subset the data to those aged 50 and over: 
ELSA_data_with_PGS = subset(ELSA_data_with_PGS, 
                            ELSA_data_with_PGS$w1age >=50 & 
                              ELSA_data_with_PGS$w2age >=50 & 
                              ELSA_data_with_PGS$w3age >=50 & 
                              ELSA_data_with_PGS$w4age >=50 & 
                              ELSA_data_with_PGS$w5age >=50 & 
                              ELSA_data_with_PGS$w6age >=50 & 
                              ELSA_data_with_PGS$w7age >=50 & 
                              ELSA_data_with_PGS$w8age >=50) 

#write.csv(ELSA_data_with_PGS, file =  paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 