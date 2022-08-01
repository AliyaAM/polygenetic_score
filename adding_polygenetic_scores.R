library(dplyr)

SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


HRS_2008_data = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS_2010_data = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS_2012_data =  read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS_2014_data =  read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS_2016_data =  read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS_2018_data =  read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))


need to add CVD to all years 
need to add hypertension_bin to 2018 

HRS_2008_data$HHIDPN
HRS_2008_data$HRS2008_emo_psychiat_prob_ever
HRS_2008_data$HRS2008_emo_psychiat_prob_bin

HRS_2008_data$HRS2008_checklist_depression_bin
unique(HRS_2008_data$smokes_now_bin)


polygenetic_scores_data = read.csv(paste(SOURCE_ROOT, "HRS_polygenetic_scores_biomarkers/pgenscore4e_r.csv", sep = ""))


#check minimum age: 

print(min(HRS_2008_data$continious_age))

print(min(HRS_2010_data$continious_age))

print(min(HRS_2012_data$continious_age))

print(min(HRS_2014_data$continious_age))

print(min(HRS_2016_data$continious_age))

print(min(HRS_2018_data$continious_age))


#HRS_2008_data$HHIDPN example: 16870011
#polygenetic_scores_data$HHID example: 16700
#polygenetic_scores_data$PN example: 11


polygenetic_scores_data$HHIDPN = paste(polygenetic_scores_data$HHID, polygenetic_scores_data$PN, sep = "")
polygenetic_scores_data$HHIDPN = as.numeric(polygenetic_scores_data$HHIDPN)

all_cases = c(HRS_2008_data$HHIDPN, 
              HRS_2010_data$HHIDPN,
              HRS_2012_data$HHIDPN,
              HRS_2014_data$HHIDPN,
              HRS_2016_data$HHIDPN,
              HRS_2016_data$HHIDPN)

ID = unique(polygenetic_scores_data$HHIDPN)

#polygenetic_scores_data$HHIDPN = subset(polygenetic_scores_data, polygenetic_scores_data$HHIDPN == c(ID))

#HRS_2008_data_with_PGS = subset(HRS_2008_data, HRS_2008_data$)


HRS_2008_data_polygenetic_scores = polygenetic_scores_data[HRS_2008_data$HHIDPN %in% ID,]
HRS_2010_data_polygenetic_scores = polygenetic_scores_data[HRS_2010_data$HHIDPN %in% ID,]
HRS_2012_data_polygenetic_scores = polygenetic_scores_data[HRS_2012_data$HHIDPN %in% ID,]
HRS_2014_data_polygenetic_scores = polygenetic_scores_data[HRS_2014_data$HHIDPN %in% ID,]
HRS_2016_data_polygenetic_scores = polygenetic_scores_data[HRS_2016_data$HHIDPN %in% ID,]
HRS_2018_data_polygenetic_scores = polygenetic_scores_data[HRS_2018_data$HHIDPN %in% ID,]


########
all_polygenetic_scores = rbind(HRS_2008_data_polygenetic_scores, 
                               HRS_2010_data_polygenetic_scores, 
                               HRS_2012_data_polygenetic_scores, 
                               HRS_2014_data_polygenetic_scores, 
                               HRS_2016_data_polygenetic_scores, 
                               HRS_2018_data_polygenetic_scores) 

#154 people out of those who took part in the HRS study between 2008 and 2018 provided their DNA sample 
people_with_PGS = unique(all_polygenetic_scores$HHIDPN)

# add these 154 individual PGS to appropriate dataframes 


print("remember to add HRS_2008_data_polygenetic_scores to the HRS_2008_data") 

HRS_2010_data_with_PGS = left_join(HRS_2010_data, HRS_2010_data_polygenetic_scores) 

head(HRS_2010_data_with_PGS)

HRS_2012_data_with_PGS = left_join(HRS_2012_data, HRS_2012_data_polygenetic_scores) 

HRS_2014_data_with_PGS = left_join(HRS_2014_data, HRS_2014_data_polygenetic_scores) 

HRS_2016_data_with_PGS = left_join(HRS_2016_data, HRS_2016_data_polygenetic_scores) 

HRS_2018_data_with_PGS = left_join(HRS_2018_data, HRS_2018_data_polygenetic_scores) 





