library(dplyr)





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




directory = "/Users/aliyaamirova/Documents/KCL_postDoc/"

SOURCE_ROOT = "Data_analysis/"
OUTPUT_ROOT = "Data_analysis/"

HRS_2008_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS_2010_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS_2012_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS_2014_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS_2016_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS_2018_data =  read.csv(paste(directory, SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))

vars_names_2016 =ls(HRS_2016_data)

write.csv(vars_names_2016, file = paste(directory, SOURCE_ROOT, "HRS_2016_data/HRS2016_vars_names.csv", sep=""))

ELSA_data = read.csv(paste(directory, SOURCE_ROOT, "DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep = ""))

#HRS polygenic scores data: 
polygenic_scores_data = read.csv(paste(directory, SOURCE_ROOT, "HRS_2012_data/pgenscore4e_r.csv", sep = ""))

#ELSA polygenic scores data: 
polygenic_scores_ELSA_data = read.csv(paste(directory, SOURCE_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 
ID_ELSA = unique(polygenic_scores_ELSA_data$idauniq)

ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]
ELSA_data_with_PGS = left_join(ELSA_data, ELSA_data_polygenic_scores) 



#check minimum age is above 50: 

print(min(HRS_2008_data$continious_age))
print(min(HRS_2010_data$continious_age))
print(min(HRS_2012_data$continious_age))
print(min(HRS_2014_data$continious_age))
print(min(HRS_2016_data$continious_age))
print(min(HRS_2018_data$continious_age))

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



#check how HRS HHIDPN were coded, rame the IDs so they match the HRS dataset

polygenic_scores_data$HHIDPN = paste(polygenic_scores_data$HHID, 0, polygenic_scores_data$PN, sep = "")
polygenic_scores_data$HHIDPN = as.numeric(polygenic_scores_data$HHIDPN)


all_cases = c(HRS_2008_data$HHIDPN, 
              HRS_2010_data$HHIDPN,
              HRS_2012_data$HHIDPN,
              HRS_2014_data$HHIDPN,
              HRS_2016_data$HHIDPN,
              HRS_2016_data$HHIDPN)

ID = unique(polygenic_scores_data$HHIDPN)



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


people_with_PGS = unique(all_polygenic_scores$HHIDPN)

# add  PGSs to appropriate dataframes 

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

