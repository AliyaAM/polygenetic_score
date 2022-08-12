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

ID_data_with_PGS =unique(ELSA_data$idauniq)


ELSA_data_wave_6 = ELSA_data_wave_6[ELSA_data$idauniq %in% ID_ELSA,]
ELSA_data_wave_7 = ELSA_data_wave_7[ELSA_data$idauniq %in% ID_ELSA,]
ELSA_data_wave_8 = ELSA_data_wave_8[ELSA_data$idauniq %in% ID_ELSA,]

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

unique(ELSA_data_with_PGS$w5_MI_new_bin)

#ELSA_data_with_PGS$age (check min age is above 50 in ELSA)
print(min(ELSA_data_with_PGS$w8age, na.rm = TRUE))




######################################
######################################

print("the names for chronic pain and insomnia are below")

# # # # # # NEW_VARIABLES	in ELSA: 

#w8incidentcancer	Incident cancer between w5 and w8	
#w8incidentdiabetes	Numeric	8	2	Incident diabetes between w5 and w8	
#w8incidentarthritis	Numeric	8	2	Incident arthritis between w5 and w8	
#w8incidentlungdis	Numeric	8	2	Incident lung disease between w5 and w8	
#w8incidentstroke	Numeric	8	2	Incident stroke between w5 and w8	


#hepain	Numeric	2	0	Whether often troubled with pain	
#unique(ELSA_data_wave_6$hepain) empty vector 
#hepaa	Numeric	2	0	Severity of pain most of the time	
#unique(ELSA_data_wave_6$hepaa) #empty vector 

#hepawba	Numeric	2	0	Whether feel pain in back	
unique(ELSA_data_wave_6$hepawba) 

w8_back_pain_bin = case_when(ELSA_data_wave_6$hepawba == 0 ~ 0, 
                             ELSA_data_wave_6$hepawba == 1 ~ 1)

w7_back_pain_bin = case_when(ELSA_data_wave_7$hepawba == 0 ~ 0, 
                             ELSA_data_wave_7$hepawba == 1 ~ 1)

w6_back_pain_bin = case_when(ELSA_data_wave_6$hepawba == 0 ~ 0, 
                             ELSA_data_wave_6$hepawba == 1 ~ 1)

w5_back_pain_bin = case_when(ELSA_data$hepawba == 0 ~ 0, 
                             ELSA_data$hepawba == 1 ~ 1)

#hepawhi	Numeric	2	0	Whether feel pain in hips	
unique(ELSA_data_wave_6$hepawhi) 


w8_hip_pain_bin = case_when(ELSA_data_wave_6$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_6$hepawhi == 1 ~ 1)

w7_hip_pain_bin = case_when(ELSA_data_wave_7$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_7$hepawhi == 1 ~ 1)

w6_hip_pain_bin = case_when(ELSA_data_wave_6$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_6$hepawhi == 1 ~ 1)

w5_hip_pain_bin = case_when(ELSA_data$hepawhi == 0 ~ 0, 
                             ELSA_data$hepawhi == 1 ~ 1)

#hepawkn	Numeric	2	0	Whether feel pain in knees	

w8_knees_pain_bin = case_when(ELSA_data_wave_6$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_6$hepawkn == 1 ~ 1)

w7_knees_pain_bin = case_when(ELSA_data_wave_7$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_7$hepawkn == 1 ~ 1)

w6_knees_pain_bin = case_when(ELSA_data_wave_6$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_6$hepawkn == 1 ~ 1)

w5_knees_pain_bin = case_when(ELSA_data$hepawkn == 0 ~ 0, 
                            ELSA_data$hepawkn == 1 ~ 1)

#hepawfe	Numeric	2	0	Whether feel pain in feet	


w8_feet_pain_bin = case_when(ELSA_data_wave_6$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_6$hepawfe == 1 ~ 1)

w7_feet_pain_bin = case_when(ELSA_data_wave_7$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_7$hepawfe == 1 ~ 1)

w6_feet_pain_bin = case_when(ELSA_data_wave_6$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_6$hepawfe == 1 ~ 1)

w5_feet_pain_bin = case_when(ELSA_data$hepawfe == 0 ~ 0, 
                              ELSA_data$hepawfe == 1 ~ 1)

#hepawmo	Numeric	2	0	Whether feel pain in mouth or teeth	
	
w8_mouth_teeth_pain_bin = case_when(ELSA_data_wave_6$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_6$hepawmo == 1 ~ 1)

w7_mouth_teeth_pain_bin = case_when(ELSA_data_wave_7$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_7$hepawmo == 1 ~ 1)

w6_mouth_teeth_pain_bin = case_when(ELSA_data_wave_6$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_6$hepawmo == 1 ~ 1)

w5_mouth_teeth_pain_bin = case_when(ELSA_data$hepawmo == 0 ~ 0, 
                             ELSA_data$hepawmo == 1 ~ 1)

#hepawot	Numeric	2	0	Whether feel pain elsewhere	


w8_elsewhere_pain_bin = case_when(ELSA_data_wave_6$hepawot == 0 ~ 0, 
                             ELSA_data_wave_6$hepawot == 1 ~ 1)

w7_elsewhere_pain_bin = case_when(ELSA_data_wave_7$hepawot == 0 ~ 0, 
                             ELSA_data_wave_7$hepawot == 1 ~ 1)

w6_elsewhere_pain_bin = case_when(ELSA_data_wave_6$hepawot == 0 ~ 0, 
                             ELSA_data_wave_6$hepawot == 1 ~ 1)

w5_elsewhere_pain_bin = case_when(ELSA_data$hepawot == 0 ~ 0, 
                             ELSA_data$hepawot == 1 ~ 1)

#hepawal	Numeric	2	0	Whether feel pain all over	
w8_widespread_pain_bin = case_when(ELSA_data_wave_6$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_6$hepawal == 1 ~ 1)

w7_widespread_pain_bin = case_when(ELSA_data_wave_7$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_7$hepawal == 1 ~ 1)

w6_widespread_pain_bin = case_when(ELSA_data_wave_6$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_6$hepawal == 1 ~ 1)

w5_widespread_pain_bin = case_when(ELSA_data$hepawal == 0 ~ 0, 
                                  ELSA_data$hepawal == 1 ~ 1)


w5_pain_bin = case_when(w5_widespread_pain_bin == 0 & w5_elsewhere_pain_bin == 0 &  w5_mouth_teeth_pain_bin == 0 & w5_feet_pain_bin == 0 & w5_knees_pain_bin == 0 & w5_hip_pain_bin == 0 &  w5_back_pain_bin == 0 ~ 0, 
                        w5_widespread_pain_bin == 1 | w5_elsewhere_pain_bin == 1 | w5_mouth_teeth_pain_bin == 1 | w5_feet_pain_bin == 1 | w5_knees_pain_bin == 1 | w5_hip_pain_bin == 1 |  w5_back_pain_bin == 1 ~ 1)  

w6_pain_bin = case_when(w6_widespread_pain_bin == 0 & w6_elsewhere_pain_bin == 0 & w6_mouth_teeth_pain_bin == 0 & w6_feet_pain_bin == 0 & w6_knees_pain_bin == 0 & w6_hip_pain_bin == 0 &  w6_back_pain_bin == 0 ~ 0, 
                        w6_widespread_pain_bin == 1 | w6_elsewhere_pain_bin == 1 | w6_mouth_teeth_pain_bin == 1 | w6_feet_pain_bin == 1 | w6_knees_pain_bin == 1 | w6_hip_pain_bin == 1 |  w6_back_pain_bin == 1 ~ 1)  

w7_pain_bin = case_when(w7_widespread_pain_bin == 0 & w7_elsewhere_pain_bin == 0 & w7_mouth_teeth_pain_bin == 0 & w7_feet_pain_bin == 0 & w7_knees_pain_bin == 0 & w7_hip_pain_bin == 0 &  w7_back_pain_bin == 0 ~ 0, 
                        w7_widespread_pain_bin == 1 | w7_elsewhere_pain_bin == 1 | w7_mouth_teeth_pain_bin == 1 | w7_feet_pain_bin == 1 | w7_knees_pain_bin == 1 | w7_hip_pain_bin == 1 |  w7_back_pain_bin == 1 ~ 1)  

w8_pain_bin = case_when(w8_widespread_pain_bin == 0 & w8_elsewhere_pain_bin == 0 & w8_mouth_teeth_pain_bin == 0 & w8_feet_pain_bin == 0 & w8_knees_pain_bin == 0 & w8_hip_pain_bin == 0 &  w8_back_pain_bin == 0 ~ 0, 
                        w8_widespread_pain_bin == 1 | w8_elsewhere_pain_bin == 1 | w8_mouth_teeth_pain_bin == 1 | w8_feet_pain_bin == 1 | w8_knees_pain_bin == 1 | w8_hip_pain_bin == 1 |  w8_back_pain_bin == 1 ~ 1)  


pain_all = data.frame(w5_widespread_pain_bin, 
                      w5_elsewhere_pain_bin, 
                      w5_mouth_teeth_pain_bin, 
                      w5_feet_pain_bin, 
                      w5_knees_pain_bin, 
                      w5_hip_pain_bin,
                      w5_back_pain_bin, 
                      w5_pain_bin,
                      
                      w6_widespread_pain_bin, 
                      w6_elsewhere_pain_bin, 
                      w6_mouth_teeth_pain_bin, 
                      w6_feet_pain_bin, 
                      w6_knees_pain_bin, 
                      w6_hip_pain_bin,
                      w6_back_pain_bin, 
                      w6_pain_bin, 
                      
                      w7_widespread_pain_bin, 
                      w7_elsewhere_pain_bin, 
                      w7_mouth_teeth_pain_bin, 
                      w7_feet_pain_bin, 
                      w7_knees_pain_bin, 
                      w7_hip_pain_bin,
                      w7_back_pain_bin, 
                      w7_pain_bin, 
                      
                      w8_widespread_pain_bin, 
                      w8_elsewhere_pain_bin, 
                      w8_mouth_teeth_pain_bin, 
                      w8_feet_pain_bin, 
                      w8_knees_pain_bin, 
                      w8_hip_pain_bin,
                      w8_back_pain_bin, 
                      w8_pain_bin) 


colnames(pain_all) = c("w5_widespread_pain_bin", 
                      "w5_elsewhere_pain_bin", 
                      "w5_mouth_teeth_pain_bin", 
                      "w5_feet_pain_bin", 
                      "w5_knees_pain_bin", 
                      "w5_hip_pain_bin",
                      "w5_back_pain_bin", 
                      "w5_pain_bin",
                      
                      "w6_widespread_pain_bin", 
                      "w6_elsewhere_pain_bin", 
                      "w6_mouth_teeth_pain_bin", 
                      "w6_feet_pain_bin", 
                      "w6_knees_pain_bin", 
                      "w6_hip_pain_bin",
                      "w6_back_pain_bin", 
                      "w6_pain_bin", 
                      
                      "w7_widespread_pain_bin", 
                      "w7_elsewhere_pain_bin", 
                      "w7_mouth_teeth_pain_bin", 
                      "w7_feet_pain_bin", 
                      "w7_knees_pain_bin", 
                      "w7_hip_pain_bin",
                      "w7_back_pain_bin", 
                      "w7_pain_bin", 
                      
                      "w8_widespread_pain_bin", 
                      "w8_elsewhere_pain_bin", 
                      "w8_mouth_teeth_pain_bin", 
                      "w8_feet_pain_bin", 
                      "w8_knees_pain_bin", 
                      "w8_hip_pain_bin",
                      "w8_back_pain_bin", 
                      "w8_pain_bin") 

ELSA_data_with_PGS = bind_cols(ELSA_data_with_PGS, pain_all) 


#SLEEP	Numeric	8	2		
#w6heslpar	Numeric	8	0	Difficulty falling asleep wave 6	
#w6heslpbr	Numeric	8	0	Waking in night wave 6	
#w6heslpdr	Numeric	8	0	Wake up feeling tired wave 6	
#w6sleepm	Numeric	8	0	Sleep problems wave 6	
#w6slqual	Numeric	8	0	Sleep quality rating wave 6	
#w6sldur	Numeric	8	0	Sleep duration category wave 6	

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