library(dplyr)

directory = "/Users/aliyaamirova/Documents/KCL_postDoc/"

SOURCE_ROOT = "Data_analysis/"
OUTPUT_ROOT = "Data_analysis/"


HRS_2008_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS_2010_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS_2012_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS_2014_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS_2016_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS_2018_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))


ELSA_data = read.csv(paste(directory, SOURCE_ROOT, "DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep = ""))

#HRS polygenic scores data: 
polygenic_scores_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2012_data/pgenscore4e_r.csv", sep = ""))

#ELSA polygenic scores data: 
polygenic_scores_ELSA_data = read.csv(paste(directory, SOURCE_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 


ID_ELSA = unique(polygenic_scores_ELSA_data$idauniq)

#polygenic_scores_data$HHIDPN = subset(polygenic_scores_data, polygenic_scores_data$HHIDPN == c(ID))

#HRS_2008_data_with_PGS = subset(HRS_2008_data, HRS_2008_data$)

ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]
ELSA_data_with_PGS = left_join(ELSA_data, ELSA_data_polygenic_scores) 


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

HRS_2008_data$HHIDPN
HRS_2008_data$HRS2008_emo_psychiat_prob_ever
HRS_2008_data$HRS2008_emo_psychiat_prob_bin

HRS_2008_data$HRS2008_checklist_depression_bin
unique(HRS_2008_data$smokes_now_bin)



polygenic_scores_data$E4_ADHD_PGC17

#check minimum age: 

print(min(HRS_2008_data$continious_age))

print(min(HRS_2010_data$continious_age))

print(min(HRS_2012_data$continious_age))

print(min(HRS_2014_data$continious_age))

print(min(HRS_2016_data$continious_age))

print(min(HRS_2018_data$continious_age))


#HRS_2008_data$HHIDPN example: 16870011
#polygenic_scores_data$HHID example: 16700
#polygenic_scores_data$PN example: 11

#check how HRS HHIDPN were coded 
polygenic_scores_data$HHIDPN = paste(polygenic_scores_data$HHID, 0, polygenic_scores_data$PN, sep = "")
polygenic_scores_data$HHIDPN = as.numeric(polygenic_scores_data$HHIDPN)


all_cases = c(HRS_2008_data$HHIDPN, 
              HRS_2010_data$HHIDPN,
              HRS_2012_data$HHIDPN,
              HRS_2014_data$HHIDPN,
              HRS_2016_data$HHIDPN,
              HRS_2016_data$HHIDPN)

ID = unique(polygenic_scores_data$HHIDPN)

#polygenic_scores_data$HHIDPN = subset(polygenic_scores_data, polygenic_scores_data$HHIDPN == c(ID))

#HRS_2008_data_with_PGS = subset(HRS_2008_data, HRS_2008_data$)

HRS_2008_data_polygenic_scores = polygenic_scores_data[HRS_2008_data$HHIDPN %in% ID,]
HRS_2010_data_polygenic_scores = polygenic_scores_data[HRS_2010_data$HHIDPN %in% ID,]
HRS_2012_data_polygenic_scores = polygenic_scores_data[HRS_2012_data$HHIDPN %in% ID,]
HRS_2014_data_polygenic_scores = polygenic_scores_data[HRS_2014_data$HHIDPN %in% ID,]
HRS_2016_data_polygenic_scores = polygenic_scores_data[HRS_2016_data$HHIDPN %in% ID,]
HRS_2018_data_polygenic_scores = polygenic_scores_data[HRS_2018_data$HHIDPN %in% ID,]


########
all_polygenic_scores = rbind(HRS_2008_data_polygenic_scores, 
                               HRS_2010_data_polygenic_scores, 
                               HRS_2012_data_polygenic_scores, 
                               HRS_2014_data_polygenic_scores, 
                               HRS_2016_data_polygenic_scores, 
                               HRS_2018_data_polygenic_scores) 

#154 people out of those who took part in the HRS study between 2008 and 2018 provided their DNA sample 
people_with_PGS = unique(all_polygenic_scores$HHIDPN)

# add these 154 individual PGS to appropriate dataframes 


HRS_2008_data_with_PGS = left_join(HRS_2008_data, HRS_2008_data_polygenic_scores) 
HRS_2010_data_with_PGS = left_join(HRS_2010_data, HRS_2010_data_polygenic_scores) 
HRS_2012_data_with_PGS = left_join(HRS_2012_data, HRS_2012_data_polygenic_scores) 
HRS_2014_data_with_PGS = left_join(HRS_2014_data, HRS_2014_data_polygenic_scores) 
HRS_2016_data_with_PGS = left_join(HRS_2016_data, HRS_2016_data_polygenic_scores) 
HRS_2018_data_with_PGS = left_join(HRS_2018_data, HRS_2018_data_polygenic_scores) 


nrow(HRS_2008_data_with_PGS)
nrow(HRS_2010_data_with_PGS)
nrow(HRS_2012_data_with_PGS)
nrow(HRS_2014_data_with_PGS)
nrow(HRS_2016_data_with_PGS)
nrow(HRS_2018_data_with_PGS)


nrow(all_polygenic_scores)

unique(all_polygenic_scores$HHIDPN)

#ageism
HRS_2008_data_with_PGS$reason_discrim1_reason_age


#restrict to race 
HRS_2008_data_with_PGS$race_white

#recode across the years so it is called the same (NO PGS) 
HRS_2008_data_with_PGS$HRS2008_cancer_bin



########################### 


#recode so it is bin var for diabetes 
HRS_2008_data_with_PGS$diabetes_new

#E4_T2D_DIAGRAM12 (PGS) 
# European Ancestry: Type 2 diabetes (T2D) Polygenic Score (DIAGRAM 2012). The
# PGSs for Type II Diabetes (T2D) were created using GWAS meta-analysis results
# from a 2012 study conducted by the DIAbetes Genetics Replication and
# Meta-analysis (DIAGRAM) Consortium.
#E4_T2D_DIAGRAM12 (PGS: T2DM) 
HRS_2008_data_with_PGS$E4_T2D_DIAGRAM12

########################### 


#coranary heart disease (no separate question on coronary heart disease)
#Did a doctor ever tell [FIRST NAME] that [he/she] had a heart attack, coronary heart disease, angina, congestive heart failure, or other heart problems?
HRS_2008_data_with_PGS$heartcondition_bin


#add new
HRS_2008_data_with_PGS$heartcondition_new_bin

#E4_CD_CARDIOGRAM11 (PGS: CAD)
HRS_2008_data_with_PGS$E4_CD_CARDIOGRAM11

########################### 

#stroke (NO PGS)
HRS_2008_data_with_PGS$stroke_new_bin


########################### 

#chronic lung disease  (NO PGS)
HRS_2008_data_with_PGS$HRS2008_lungdisease_bin
#add new (recode to bin)
HRS_2008_data_with_PGS$HRS2008_lungdisease_new


########################### 

#arthritis 
#add new arthritis (NO PGS) 
HRS_2008_data_with_PGS$HRS2008_arthritis_bin
#add new   (recode to bin)
HRS_2008_data_with_PGS$HRS2008_arthritis_new

########################### 

#limiting longstanding condition 
HRS_2008_data_with_PGS$limiting_condition_bin
#add new   (recode to bin)

########################### 

#depression 
HRS_2008_data_with_PGS$checklist_depression_bin

#E4_DEPSYMP_SSGAC16 depressive symptoms (PGS) 
HRS_2008_data_with_PGS$E4_DEPSYMP_SSGAC16

#E4_MDD_PGC13 (major depressive disorder  2013 study)
HRS_2008_data_with_PGS$E4_MDD_PGC13

#E4_MDD2_PGC18 (najor depressive disorder 2018 study)
HRS_2008_data_with_PGS$E4_MDD2_PGC18

########################### 

#anxiety (ADD) 

#E4_ANXFS_ANGST16              EA ANXIETY FACTOR SCORE PGS (ANGST 2016)
#E4_ANXCC_ANGST16              EA ANXIETY CASE CONTROL PGS (ANGST 2016)


########################### 

#heart failure 
HRS_2008_data_with_PGS$heartfailure2yrs_bin


########################### 

#heart attack 
HRS_2008_data_with_PGS$HRS2008_heartattack2yrs_bin

# EA Myocardial Infartcion (MI) PGS (CARDIOGRAM 2015)
HRS_2008_data_with_PGS$E4_MI_CARDIOGRAM15

########################### 

# hypertension 
HRS_2008_data_with_PGS$hypertension_new_bin

#E4_HTN_COGENT17
# European Ancestry: Hypertension. PGSs for Hypertension were created using
# results from a 2017 study conducted by the Continental Origins and Genetic
# Epidemiology Network (COGENT) consortium.

HRS_2008_data_with_PGS$E4_HTN_COGENT17

########################### 

#add smoking, alchohol consumption, BMI PGS and weight discrimination 

HRS_2008_data_with_PGS$vigarious_physical_activity

#add smoking, alchohol consumption, BMI PGS
#add smoking, alchohol consumption, BMI PGS 
#add smoking, alchohol consumption, BMI PGS 
#add smoking, alchohol consumption, BMI PGS 
#add smoking, alchohol consumption, BMI PGS 
#add smoking, alchohol consumption, BMI PGS 
#add smoking, alchohol consumption, BMI PGS 

#We also have PTSD PGS 
#We also have Alzheimer's PGS 
#We have kidney disease PGS 

nrow(HRS_2008_data_with_PGS)

summary_stat = summary(HRS_2008_data_with_PGS$wealth_noIRA)
summary_stat[2]
summary_stat[3]
summary_stat[5]

first_quantile_value = summary_stat[6]/4
second_quantile_value = summary_stat[6]/4 + summary_stat[6]/4
third_quantile_value = summary_stat[6]/4 + summary_stat[6]/4 + summary_stat[6]/4

subset_poorest = subset(HRS_2008_data_with_PGS, HRS_2008_data_with_PGS$wealth_noIRA<first_quantile_value)
nrow(subset_poorest)

subset_1 = subset(HRS_2008_data_with_PGS, HRS_2008_data_with_PGS$wealth_noIRA<summary_stat[2])
nrow(subset_1)

subset_2 = subset(HRS_2008_data_with_PGS, HRS_2008_data_with_PGS$wealth_noIRA>summary_stat[2] & HRS_2008_data_with_PGS$wealth_noIRA<summary_stat[3])
nrow(subset_2)

subset_3 = subset(HRS_2008_data_with_PGS, HRS_2008_data_with_PGS$wealth_noIRA>summary_stat[3] & HRS_2008_data_with_PGS$wealth_noIRA<summary_stat[5])
nrow(subset_3)

subset_4 = subset(HRS_2008_data_with_PGS, HRS_2008_data_with_PGS$wealth_noIRA>summary_stat[5])
nrow(subset_4)