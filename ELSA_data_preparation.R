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
ELSA_data_age = subset(ELSA_data, 
                       ELSA_data$w1age >=50 & 
                         ELSA_data$w2age >=50 & 
                         ELSA_data$w3age >=50 & 
                         ELSA_data$w4age >=50 & 
                         ELSA_data$w5age >=50 & 
                         ELSA_data$w6age >=50 & 
                         ELSA_data$w7age >=50 & 
                         ELSA_data$w8age >=50) 

ELSA_data_wave_6 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave_6_elsa_data_v2.csv", sep = ""))
ELSA_data_wave_7 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave_7_elsa_data.csv", sep = ""))
ELSA_data_wave_8 = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_all/wave8.csv", sep = ""))


#HRS polygenic scores data: 
polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "/HRS_polygenetic_scores_biomarkers/pgenscore4e_r.csv", sep = ""))

#polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/pgenscore4e_r.csv", sep = ""))

#ELSA polygenic scores data: 
polygenic_scores_ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 

#polygenic_scores_ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 


principal_component_ELSA = read.csv("/Users/aliya/my_docs/KCL_postDoc/polygenetic_score/ELSA_PGS_data/Principal_Component_ELSA_2019.csv") 
#principal_component_ELSA = principal_component_ELSA[principal_component_ELSA$IQAUNIQ %in% ID_ELSA,]
polygenic_scores_ELSA_data = bind_cols(polygenic_scores_ELSA_data, principal_component_ELSA) 


#subset polygenic scores data to those who provided data for the main survey 
ID_ELSA = unique(ELSA_data$idauniq)
ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]


#subset main ELSA data to thos ewho provided PGS data 
ID_ELSA_PGS_unique =unique(polygenic_scores_data$idauniq)

#nrow(ELSA_data_PGS_subset)
#nrow(ELSA_data_polygenic_scores)

#join PGS scores with ELSA main dataset 
ELSA_data_with_PGS = bind_cols(ELSA_data, ELSA_data_polygenic_scores) 


ELSA_data_with_PGS$pc1
###add principal components 





###################
###################



unique(ELSA_data_with_PGS$w5discrim_bin2)
unique(ELSA_data_with_PGS$w5discrim_bin)

ELSA_data_with_PGS$w5agediscrimination2
ELSA_data_with_PGS$w5disabilitydiscrimination2
ELSA_data_with_PGS$w5sexdiscrimination2
ELSA_data_with_PGS$w5discrim_sexuality2
ELSA_data_with_PGS$w5racediscrimination2
ELSA_data_with_PGS$w5discrim_financial2
ELSA_data_with_PGS$w5weightdiscrimination2


unique(ELSA_data_with_PGS$w5discrim_continuous)



unique(ELSA_data_with_PGS$w5discrim_age) 
ELSA_data_with_PGS$w5discrim_disability
ELSA_data_with_PGS$w5discrim_financial
ELSA_data_with_PGS$w5discrim_gender
ELSA_data_with_PGS$w5discrim_race
ELSA_data_with_PGS$w5discrim_sexuality
ELSA_data_with_PGS$w5discrim_weight



###################
###################

###################
###################


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

w6_back_pain_bin = case_when(ELSA_data_wave_6$hepawba == 0 ~ 0, 
                             ELSA_data_wave_6$hepawba == 1 ~ 1)

w7_back_pain_bin = case_when(ELSA_data_wave_7$hepawba == 0 ~ 0, 
                             ELSA_data_wave_7$hepawba == 1 ~ 1)

w8_back_pain_bin = case_when(ELSA_data_wave_8$hepawba == 0 ~ 0, 
                             ELSA_data_wave_8$hepawba == 1 ~ 1)

ELSA_data$hepawba.x
w5_back_pain_bin = case_when(ELSA_data$hepawba.x == 0 ~ 0, 
                             ELSA_data$hepawba.x == 1 ~ 1)

#hepawhi	Numeric	2	0	Whether feel pain in hips	
unique(ELSA_data_wave_6$hepawhi) 


w6_hip_pain_bin = case_when(ELSA_data_wave_6$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_6$hepawhi == 1 ~ 1)

w7_hip_pain_bin = case_when(ELSA_data_wave_7$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_7$hepawhi == 1 ~ 1)

w8_hip_pain_bin = case_when(ELSA_data_wave_8$hepawhi == 0 ~ 0, 
                             ELSA_data_wave_8$hepawhi == 1 ~ 1)

w5_hip_pain_bin = case_when(ELSA_data$hepawhi.x == 0 ~ 0, 
                             ELSA_data$hepawhi.x == 1 ~ 1)

#hepawkn	Numeric	2	0	Whether feel pain in knees	

w6_knees_pain_bin = case_when(ELSA_data_wave_6$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_6$hepawkn == 1 ~ 1)

w7_knees_pain_bin = case_when(ELSA_data_wave_7$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_7$hepawkn == 1 ~ 1)

w8_knees_pain_bin = case_when(ELSA_data_wave_8$hepawkn == 0 ~ 0, 
                            ELSA_data_wave_8$hepawkn == 1 ~ 1)

w5_knees_pain_bin = case_when(ELSA_data$hepawkn.x == 0 ~ 0, 
                            ELSA_data$hepawkn.x == 1 ~ 1)

#hepawfe	Numeric	2	0	Whether feel pain in feet	


w6_feet_pain_bin = case_when(ELSA_data_wave_6$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_6$hepawfe == 1 ~ 1)

w7_feet_pain_bin = case_when(ELSA_data_wave_7$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_7$hepawfe == 1 ~ 1)

w8_feet_pain_bin = case_when(ELSA_data_wave_8$hepawfe == 0 ~ 0, 
                              ELSA_data_wave_8$hepawfe == 1 ~ 1)

w5_feet_pain_bin = case_when(ELSA_data$hepawfe.x == 0 ~ 0, 
                              ELSA_data$hepawfe.x == 1 ~ 1)

#hepawmo	Numeric	2	0	Whether feel pain in mouth or teeth	
	
w6_mouth_teeth_pain_bin = case_when(ELSA_data_wave_6$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_6$hepawmo == 1 ~ 1)

w7_mouth_teeth_pain_bin = case_when(ELSA_data_wave_7$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_7$hepawmo == 1 ~ 1)

w8_mouth_teeth_pain_bin = case_when(ELSA_data_wave_8$hepawmo == 0 ~ 0, 
                             ELSA_data_wave_8$hepawmo == 1 ~ 1)

w5_mouth_teeth_pain_bin = case_when(ELSA_data$hepawmo.x == 0 ~ 0, 
                             ELSA_data$hepawmo.x == 1 ~ 1)

#hepawot	Numeric	2	0	Whether feel pain elsewhere	


w6_elsewhere_pain_bin = case_when(ELSA_data_wave_6$hepawot == 0 ~ 0, 
                             ELSA_data_wave_6$hepawot == 1 ~ 1)

w7_elsewhere_pain_bin = case_when(ELSA_data_wave_7$hepawot == 0 ~ 0, 
                             ELSA_data_wave_7$hepawot == 1 ~ 1)

w8_elsewhere_pain_bin = case_when(ELSA_data_wave_8$hepawot == 0 ~ 0, 
                             ELSA_data_wave_8$hepawot == 1 ~ 1)

w5_elsewhere_pain_bin = case_when(ELSA_data$hepawot.x == 0 ~ 0, 
                             ELSA_data$hepawot.x == 1 ~ 1)

#hepawal	Numeric	2	0	Whether feel pain all over	
w6_widespread_pain_bin = case_when(ELSA_data_wave_6$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_6$hepawal == 1 ~ 1)

w7_widespread_pain_bin = case_when(ELSA_data_wave_7$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_7$hepawal == 1 ~ 1)

w8_widespread_pain_bin = case_when(ELSA_data_wave_8$hepawal == 0 ~ 0, 
                                  ELSA_data_wave_8$hepawal == 1 ~ 1)

w5_widespread_pain_bin = case_when(ELSA_data$hepawal.x == 0 ~ 0, 
                                  ELSA_data$hepawal.x == 1 ~ 1)


w5_pain_bin = case_when(w5_widespread_pain_bin == 0 & w5_elsewhere_pain_bin == 0 &  w5_feet_pain_bin == 0 & w5_knees_pain_bin == 0 & w5_hip_pain_bin == 0 &  w5_back_pain_bin == 0 ~ 0, 
                        w5_widespread_pain_bin == 1 | w5_elsewhere_pain_bin == 1 | w5_feet_pain_bin == 1 | w5_knees_pain_bin == 1 | w5_hip_pain_bin == 1 |  w5_back_pain_bin == 1 ~ 1)  

unique(w5_pain_bin)

w6_pain_bin = case_when(w6_widespread_pain_bin == 0 & w6_elsewhere_pain_bin == 0  & w6_feet_pain_bin == 0 & w6_knees_pain_bin == 0 & w6_hip_pain_bin == 0 &  w6_back_pain_bin == 0 ~ 0, 
                        w6_widespread_pain_bin == 1 | w6_elsewhere_pain_bin == 1  | w6_feet_pain_bin == 1 | w6_knees_pain_bin == 1 | w6_hip_pain_bin == 1 |  w6_back_pain_bin == 1 ~ 1)  
unique(w6_pain_bin)

w7_pain_bin = case_when(w7_widespread_pain_bin == 0 & w7_elsewhere_pain_bin == 0  & w7_feet_pain_bin == 0 & w7_knees_pain_bin == 0 & w7_hip_pain_bin == 0 &  w7_back_pain_bin == 0 ~ 0, 
                        w7_widespread_pain_bin == 1 | w7_elsewhere_pain_bin == 1  | w7_feet_pain_bin == 1 | w7_knees_pain_bin == 1 | w7_hip_pain_bin == 1 |  w7_back_pain_bin == 1 ~ 1)  

unique(w7_pain_bin)

w8_pain_bin = case_when(w8_widespread_pain_bin == 0 & w8_elsewhere_pain_bin == 0  & w8_feet_pain_bin == 0 & w8_knees_pain_bin == 0 & w8_hip_pain_bin == 0 &  w8_back_pain_bin == 0 ~ 0, 
                        w8_widespread_pain_bin == 1 | w8_elsewhere_pain_bin == 1  | w8_feet_pain_bin == 1 | w8_knees_pain_bin == 1 | w8_hip_pain_bin == 1 |  w8_back_pain_bin == 1 ~ 1)  


unique(w8_pain_bin)

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

unique(ELSA_data_with_PGS$w5_pain_bin)
unique(ELSA_data_with_PGS$w6_pain_bin) # no 0 
unique(ELSA_data_with_PGS$w7_pain_bin)
unique(ELSA_data_with_PGS$w8_pain_bin) 

# widespread pain has 0 and 1s 
unique(ELSA_data_with_PGS$w5_widespread_pain_bin)
unique(ELSA_data_with_PGS$w6_widespread_pain_bin) 
unique(ELSA_data_with_PGS$w7_widespread_pain_bin)
unique(ELSA_data_with_PGS$w8_widespread_pain_bin) 

# widespread pain has 0 and 1s 
unique(ELSA_data_with_PGS$w5_elsewhere_pain_bin)
unique(ELSA_data_with_PGS$w6_elsewhere_pain_bin) 
unique(ELSA_data_with_PGS$w7_elsewhere_pain_bin)
unique(ELSA_data_with_PGS$w8_elsewhere_pain_bin) 


#SLEEP	Numeric	8	2		
#w6heslpar	Numeric	8	0	Difficulty falling asleep wave 6
#1 Not during the last month
#2 Less than once a week
#3 Once or twice a week
#4 Three or more times a week
#ELSA_data_wave_7$heslpar null 
#ELSA_data_wave_8$heslpar
#w6heslpbr	Numeric	8	0	Waking in night wave 6	
#w6heslpdr	Numeric	8	0	Wake up feeling tired wave 6	
#w6sleepm	Numeric	8	0	Sleep problems wave 6	
#w6slqual	Numeric	8	0	Sleep quality rating wave 6	
#w6sldur	Numeric	8	0	Sleep duration category wave 6	

############
############
############

#HESLPF	ELSA 2012	HE. Health	
#Description:
#  During the last month, how would you rate your sleep quality overall. Would you say it was... INTERVIEWER: Read out...
#Text:
#  During the last month, how would you rate your sleep quality overall. Would you say it was... INTERVIEWER: Read out...
#Response type:
#  Enumerated
#Responses:
#  1 ...Very good,
#2 Good,
#3 Fairly bad,
#4 or, Very bad?
  
w6_sleep_qual = case_when(ELSA_data_wave_6$heslpf == 1 ~ 4, 
                                   ELSA_data_wave_6$heslpf == 2 ~ 3, 
                                   ELSA_data_wave_6$heslpf == 3 ~ 2, 
                                   ELSA_data_wave_6$heslpf == 4 ~ 1)

#FROM: Jackowska, Kumari, Steptoe  (2013) Sleep disturbance was assessed with three questions that are the most common symptoms of insomnia.Specifically, participants were requested to indicate whether in the past month
#they had difficulties falling sleep, staying asleep, and
#whether they felt tired upon waking up in the mornin




#HESLPE	ELSA 2012: how many hours of sleep do you have on average 

w6_hrs_sleep = ELSA_data_wave_6$heslpe




#HESLPA	ELSA 2012:  How often do you have difficulty falling asleep? INTERVIEWER: Count as yes if cannot get to sleep for at least 30 minutes.
#1 Not during the last month
#2 Less than once a week
#3 Once or twice a week
#4 Three or more times a week


w6_diff_sleep_onset = case_when(ELSA_data_wave_6$heslpa == 1 ~ 1, 
                          ELSA_data_wave_6$heslpa == 2 ~ 2, 
                          ELSA_data_wave_6$heslpa == 3 ~ 3, 
                          ELSA_data_wave_6$heslpa == 4 ~ 4)




w6_diff_sleep_onset_bin  = case_when(ELSA_data_wave_6$heslpa == 1 ~ 0, 
                              ELSA_data_wave_6$heslpa == 2 ~ 0, 
                              ELSA_data_wave_6$heslpa == 3 ~ 1, 
                              ELSA_data_wave_6$heslpa == 4 ~ 1)



#HESLPC	ELSA 2012: How often in the past month did you have trouble staying asleep (including waking far too early)?
#1 Not at all
#2 1-3 days
#3 4-7 days
#4 8-14 days
#5 15-21 days
#6 22-31 days


w6_asleep_prob = case_when(ELSA_data_wave_6$heslpc == 1 ~ 0, 
                           ELSA_data_wave_6$heslpc == 2 ~ 1, 
                           ELSA_data_wave_6$heslpc == 3 ~ 2, 
                           ELSA_data_wave_6$heslpc == 4 ~ 3, 
                           ELSA_data_wave_6$heslpc == 5 ~ 4, 
                           ELSA_data_wave_6$heslpc == 6 ~ 5)

w6_asleep_prob_bin = case_when(ELSA_data_wave_6$heslpc == 1 ~ 0, 
                               ELSA_data_wave_6$heslpc == 2 ~ 0, 
                               ELSA_data_wave_6$heslpc == 3 ~ 1, 
                               ELSA_data_wave_6$heslpc == 4 ~ 1, 
                               ELSA_data_wave_6$heslpc == 5 ~ 1, 
                               ELSA_data_wave_6$heslpc == 6 ~ 1)

#HESLPD (2012) waking up feeling tired 
#1 Not during the last month
#2 Less than once a week
#3 Once or twice a week
#4 Three or more times a week

w6_wkup_tired = case_when(ELSA_data_wave_6$heslpd == 1 ~ 1, 
                          ELSA_data_wave_6$heslpd == 2 ~ 2, 
                          ELSA_data_wave_6$heslpd == 3 ~ 3, 
                          ELSA_data_wave_6$heslpd == 4 ~ 4)

w6_wkup_tired_bin = case_when(ELSA_data_wave_6$heslpd == 1 ~ 0, 
                              ELSA_data_wave_6$heslpd == 2 ~ 0, 
                              ELSA_data_wave_6$heslpd == 3 ~ 1, 
                              ELSA_data_wave_6$heslpd == 4 ~ 1)




w6_sleep_disturbance = (w6_diff_sleep_onset + w6_asleep_prob + w6_wkup_tired)/3 

#################
print(" sleep disturbance wave 7 was not collected, for sleep onset (heslpa), asleep_prob (heslpc), and wkup_tired (heslpd)")

# 
# w7_diff_sleep_onset = case_when(ELSA_data_wave_7$heslpa == 1 ~ 1, 
#                                 ELSA_data_wave_7$heslpa == 2 ~ 2, 
#                                 ELSA_data_wave_7$heslpa == 3 ~ 3, 
#                                 ELSA_data_wave_7$heslpa == 4 ~ 4)
# 
# 
# 
# 
# w7_asleep_prob = case_when(ELSA_data_wave_7$heslpc == 1 ~ 0, 
#                            ELSA_data_wave_7$heslpc == 2 ~ 1, 
#                            ELSA_data_wave_7$heslpc == 3 ~ 2, 
#                            ELSA_data_wave_7$heslpc == 4 ~ 3, 
#                            ELSA_data_wave_7$heslpc == 5 ~ 4, 
#                            ELSA_data_wave_7$heslpc == 6 ~ 5)
# 
# 
# 
# w7_wkup_tired = case_when(ELSA_data_wave_7$heslpd == 1 ~ 1, 
#                           ELSA_data_wave_7$heslpd == 2 ~ 2, 
#                           ELSA_data_wave_7$heslpd == 3 ~ 3, 
#                           ELSA_data_wave_7$heslpd == 4 ~ 4)
# 
# 
# 
# w7_sleep_disturbance = (w7_diff_sleep_onset + w7_asleep_prob + w7_wkup_tired)/3 


########### 2016: 

#HESLPA	ELSA 2016: difficulty falling asleep 
#1 Not during the last month
#2 Less than once a week
#3 Once or twice a week
#4 Three or more times a week

w8_diff_sleep_onset = case_when(ELSA_data_wave_8$heslpa == 1 ~ 1, 
                                ELSA_data_wave_8$heslpa == 2 ~ 2, 
                                ELSA_data_wave_8$heslpa == 3 ~ 3, 
                                ELSA_data_wave_8$heslpa == 4 ~ 4)

w8_diff_sleep_onset_bin  = case_when(ELSA_data_wave_8$heslpa == 1 ~ 0, 
                                     ELSA_data_wave_8$heslpa == 2 ~ 0, 
                                     ELSA_data_wave_8$heslpa == 3 ~ 1, 
                                     ELSA_data_wave_8$heslpa == 4 ~ 1)


#HESLPC	ELSA 2016: staying asleep 
#1 Not at all
#2 1-3 days
#3 4-7 days
#4 8-14 days
#5 15-21 days
#6 22-31 days


w8_asleep_prob = case_when(ELSA_data_wave_8$heslpc == 1 ~ 0, 
                           ELSA_data_wave_8$heslpc == 2 ~ 1, 
                           ELSA_data_wave_8$heslpc == 3 ~ 2, 
                           ELSA_data_wave_8$heslpc == 4 ~ 3, 
                           ELSA_data_wave_8$heslpc == 5 ~ 4, 
                           ELSA_data_wave_8$heslpc == 6 ~ 5)




w8_asleep_prob_bin = case_when(ELSA_data_wave_8$heslpc == 1 ~ 0, 
                               ELSA_data_wave_8$heslpc == 2 ~ 0, 
                               ELSA_data_wave_8$heslpc == 3 ~ 1, 
                               ELSA_data_wave_8$heslpc == 4 ~ 1, 
                               ELSA_data_wave_8$heslpc == 5 ~ 1, 
                               ELSA_data_wave_8$heslpc == 6 ~ 1)


#HESLPD	ELSA 2016: How often do you wake up after your usual amount of sleep feeling tired and worn out?


w8_wkup_tired = case_when(ELSA_data_wave_8$heslpd == 1 ~ 1, 
                          ELSA_data_wave_8$heslpd == 2 ~ 2, 
                          ELSA_data_wave_8$heslpd == 3 ~ 3, 
                          ELSA_data_wave_8$heslpd == 4 ~ 4)




w8_wkup_tired_bin = case_when(ELSA_data_wave_8$heslpd == 1 ~ 0, 
                              ELSA_data_wave_8$heslpd == 2 ~ 0, 
                              ELSA_data_wave_8$heslpd == 3 ~ 1, 
                              ELSA_data_wave_8$heslpd == 4 ~ 1)


#HESLPE	ELSA 2016 (hours of sleep)




w8_hrs_sleep = ELSA_data_wave_8$heslpe

#HESLPF	ELSA 2016 (sleep quality) 


w8_sleep_qual = case_when(ELSA_data_wave_8$heslpf == 1 ~ 4, 
                          ELSA_data_wave_8$heslpf == 2 ~ 3, 
                          ELSA_data_wave_8$heslpf == 3 ~ 2, 
                          ELSA_data_wave_8$heslpf == 4 ~ 1)


w8_sleep_disturbance = (w8_diff_sleep_onset + w8_asleep_prob + w8_wkup_tired)/3 


sleep_data = data.frame(w6_sleep_qual, 
                        w6_wkup_tired, 
                        w6_wkup_tired_bin, 
                        w6_hrs_sleep, 
                        w6_asleep_prob, 
                        w6_asleep_prob_bin, 
                        w6_diff_sleep_onset, 
                        w6_diff_sleep_onset_bin,  
                        w6_sleep_disturbance,
                        w8_diff_sleep_onset, 
                        w8_diff_sleep_onset_bin, 
                        w8_asleep_prob, 
                        w8_asleep_prob_bin, 
                        w8_wkup_tired, 
                        w8_wkup_tired_bin, 
                        w8_hrs_sleep, 
                        w8_sleep_qual,
                        w8_sleep_disturbance)


colnames(sleep_data) = c("w6_sleep_qual", 
                         "w6_wkup_tired",  
                         "w6_wkup_tired_bin", 
                         "w6_hrs_sleep", 
                         "w6_asleep_prob", 
                         "w6_asleep_prob_bin", 
                         "w6_diff_sleep_onset", 
                         "w6_diff_sleep_onset_bin", 
                         "w6_sleep_disturbance",
                         "w8_diff_sleep_onset", 
                         "w8_diff_sleep_onset_bin", 
                         "w8_asleep_prob", 
                         'w8_asleep_prob_bin', 
                         "w8_wkup_tired", 
                         "w8_wkup_tired_bin", 
                         "w8_hrs_sleep", 
                         "w8_sleep_qual",
                         "w8_sleep_disturbance")


ELSA_data_with_PGS = bind_cols(ELSA_data_with_PGS, sleep_data) 

unique(ELSA_data_with_PGS$w8_sleep_disturbance)
#1 Not during the last month
#2 Less than once a week
#3 Once or twice a week
#4 Three or more times a week

ELSA_data_with_PGS$w8_sleep_disturbance_bin = case_when(ELSA_data_with_PGS$w8_sleep_disturbance == 1 ~ 0, 
                                                        ELSA_data_with_PGS$w8_sleep_disturbance == 2 ~ 0,
                                                        ELSA_data_with_PGS$w8_sleep_disturbance == 3 ~ 1, 
                                                        ELSA_data_with_PGS$w8_sleep_disturbance == 4 ~ 1)




ELSA_data_with_PGS$w6_sleep_disturbance_bin = case_when(ELSA_data_with_PGS$w6_sleep_disturbance == 1 ~ 0, 
                                                        ELSA_data_with_PGS$w6_sleep_disturbance == 2 ~ 0,
                                                        ELSA_data_with_PGS$w6_sleep_disturbance == 3 ~ 1, 
                                                        ELSA_data_with_PGS$w6_sleep_disturbance == 4 ~ 1)


unique(ELSA_data_with_PGS$w6_sleep_disturbance_bin)
unique(ELSA_data_with_PGS$w8_sleep_disturbance_bin)




#PSCEDA whether depressed in the past week. 
#1 Yes
#2 No
unique(ELSA_data_wave_6$PScedA)
unique(ELSA_data_wave_7$PScedA)
unique(ELSA_data_wave_8$psceda)

w6_depression_bin = case_when(ELSA_data_wave_6$PScedA == 1 ~ 1, 
                              ELSA_data_wave_6$PScedA == 2 ~ 0) 

w7_depression_bin = case_when(ELSA_data_wave_7$PScedA == 1 ~ 1, 
                              ELSA_data_wave_7$PScedA == 2 ~ 0)                            

w8_depression_bin = case_when(ELSA_data_wave_8$psceda == 1 ~ 1, 
                              ELSA_data_wave_8$psceda == 2 ~ 0)  


depression_data =data.frame(w6_depression_bin, 
                            w7_depression_bin, 
                            w8_depression_bin)

colnames(depression_data) = c("w6_depression_bin",
                              "w7_depression_bin",
                              "w8_depression_bin")


ELSA_data_with_PGS = bind_cols(ELSA_data_with_PGS, depression_data) 

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

unique(ELSA_data_with_PGS$w6_pain_bin)


write.csv(ELSA_data_with_PGS, file =  paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 