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




directory = "/Users/aliyaamirova/"

DATA_ROOT = "Documents/KCL_postDoc/Data_analysis/"
SOURCE_ROOT = "/proj/Cumulative_effects_HRS/Version_2_analysis/"

source((paste(directory, SOURCE_ROOT, "sort_timepoints.R", sep="")))


###########
###########

ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep = ""))

#HRS polygenic scores data: 
polygenic_scores_data = read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/pgenscore4e_r.csv", sep = ""))

#ELSA polygenic scores data: 
polygenic_scores_ELSA_data = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/polygenic_scores_ELSA_data_standardized.csv", sep = "")) 
ID_ELSA = unique(polygenic_scores_ELSA_data$idauniq)

ELSA_data_polygenic_scores = polygenic_scores_ELSA_data[ELSA_data$idauniq %in% ID_ELSA,]
ELSA_data_with_PGS = left_join(ELSA_data, ELSA_data_polygenic_scores) 

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

write.csv(ELSA_data_with_PGS, file =  paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

#############

HRS_2008_data = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/HRS2008_dataset_latest_renamed_vars.csv", sep=""))
HRS_2010_data = read.csv(paste(directory, DATA_ROOT, "HRS_2010_data/HRS2010_dataset_latest_renamed_vars.csv", sep=""))
HRS_2012_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2012_data/HRS2012_dataset_latest_renamed_vars.csv", sep=""))
HRS_2014_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2014_data/HRS2014_dataset_latest_renamed_vars.csv", sep=""))
HRS_2016_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2016_data/HRS2016_dataset_latest_renamed_vars.csv", sep=""))
HRS_2018_data =  read.csv(paste(directory, DATA_ROOT, "HRS_2018_data/HRS2018_dataset_latest_renamed_vars.csv", sep=""))

#create HHIDPN so they match the names in the polygenic score 
HRS_2008_data$HHIDPN = HRS_2008_data$HRS2008_data.HHIDPN
HRS_2010_data$HHIDPN = HRS_2010_data$HRS_2010_data.HHIDPN
HRS_2012_data$HHIDPN = HRS_2012_data$HRS_2012_data.HHIDPN
HRS_2014_data$HHIDPN = HRS_2014_data$HRS_2014_data.HHIDPN
HRS_2016_data$HHIDPN = HRS_2016_data$HRS_2016_data.HHIDPN
HRS_2018_data$HHIDPN = HRS_2018_data$HRS_2018_data.HHIDPN


#check minimum age is above 50: 

print(min(HRS_2008_data$HRS2008_continious_age))
print(min(HRS_2010_data$HRS2010_continious_age))
print(min(HRS_2012_data$HRS2012_continious_age))
print(min(HRS_2014_data$HRS2014_continious_age))
print(min(HRS_2016_data$HRS2016_continious_age))
print(min(HRS_2018_data$HRS2018_continious_age))

HRS_2008_data$HRS2008_discrim_harassed

# only include the variables of interest, because later we need ot drop the NAs. 
HRS_2008_data = data.frame(HRS_2008_data$HHIDPN,  
                           HRS_2008_data$HRS2008_checklist_depression_bin, 
                           HRS_2008_data$HRS2008_discrim_bin) 

colnames(HRS_2008_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")

HRS_2010_data = data.frame(HRS_2010_data$HHIDPN,
                           HRS_2010_data$HRS2010_checklist_depression_bin, 
                           HRS_2010_data$HRS2010_discrim_bin) 


colnames(HRS_2010_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")

HRS_2012_data = data.frame(HRS_2012_data$HHIDPN,
                           HRS_2012_data$HRS2012_checklist_depression_bin, 
                           HRS_2012_data$HRS2012_discrim_bin) 



colnames(HRS_2012_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")


HRS_2014_data = data.frame(HRS_2014_data$HHIDPN,
                           HRS_2014_data$HRS2014_checklist_depression_bin, 
                           HRS_2014_data$HRS2014_discrim_bin) 


colnames(HRS_2014_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")


HRS_2016_data = data.frame(HRS_2016_data$HHIDPN,
                           HRS_2016_data$HRS2016_checklist_depression_bin, 
                           HRS_2016_data$HRS2016_discrim_bin) 


colnames(HRS_2016_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")


HRS_2018_data = data.frame(HRS_2018_data$HHIDPN,
                           HRS_2018_data$HRS2018_checklist_depression_bin, 
                           HRS_2018_data$HRS2018_discrim_bin) 
        

colnames(HRS_2018_data) = c("HHIDPN", 
                            "checklist_depression_bin", 
                            "discrim_bin")

HRS_all_data = rbind(HRS_2008_data,
                     HRS_2010_data,
                     HRS_2012_data,
                     HRS_2014_data,
                     HRS_2016_data,
                     HRS_2018_data)

HRS_all_data_SORTED = sort_timepoints(data = HRS_all_data)

#drop NAs
polygenic_scores_data = na.omit(polygenic_scores_data)
nrow(polygenic_scores_data)
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

#HRS_2008_data_with_PGS = left_join(HRS_2008_data, HRS_2008_data_polygenic_scores) 
#HRS_2010_data_with_PGS = left_join(HRS_2010_data, HRS_2010_data_polygenic_scores) 
#HRS_2012_data_with_PGS = left_join(HRS_2012_data, HRS_2012_data_polygenic_scores) 
#HRS_2014_data_with_PGS = left_join(HRS_2014_data, HRS_2014_data_polygenic_scores) 
#HRS_2016_data_with_PGS = left_join(HRS_2016_data, HRS_2016_data_polygenic_scores) 
#HRS_2018_data_with_PGS = left_join(HRS_2018_data, HRS_2018_data_polygenic_scores) 


#only take depression, discrimination_bin, 



one = full_join(HRS_2008_data,
            HRS_2010_data)

two = full_join(HRS_2012_data,
            HRS_2014_data)

three = full_join(HRS_2016_data,
              HRS_2018_data)


one_and_two =  full_join(one,
                     two)

HRS_all_data = full_join(one_and_two, 
                     three)



na.omit(HRS_2008_data) #3201+ (ageism) 6263+ (discrim_bin any)

na.omit(HRS_2010_data) #3748, 7278

na.omit(HRS_2012_data) #3312

na.omit(HRS_2014_data) #3400

na.omit(HRS_2016_data) #3089

na.omit(HRS_2018_data) #2839, 5142

print("not enough cases with complete data across all years")

HRS_all_data_SORTED_noNAs = na.omit(HRS_all_data_SORTED) 
unique(HRS_all_data_SORTED_noNAs$HHIDPN)



na.omit(one) 
na.omit(two) 
na.omit(three) 
na.omit(one_and_two)




one[complete.cases(one), ]
complete.cases(one)


#######
#######
polygenic_scores_data = data.frame(polygenic_scores_data$HHIDPN, 
                                    polygenic_scores_data$E4_EDU2_SSGAC16, 
                                    polygenic_scores_data$E4_BMI_GIANT15, 
                                    polygenic_scores_data$E4_SCZ_PGC14, 
                                    polygenic_scores_data$E4_EVRSMK_TAG10, 
                                    polygenic_scores_data$E4_AD2_IGAP13, 
                                    polygenic_scores_data$E4_WC_GIANT15, 
                                    polygenic_scores_data$E4_NEUROT_SSGAC16, 
                                    polygenic_scores_data$E4_WELLB_SSGAC16, 
                                    polygenic_scores_data$E4_DEPSYMP_SSGAC16, 
                                    polygenic_scores_data$E4_CD_CARDIOGRAM11, 
                                    polygenic_scores_data$E4_MI_CARDIOGRAM15, 
                                    polygenic_scores_data$E4_T2D_DIAGRAM12, 
                                    polygenic_scores_data$E4_ANXFS_ANGST16, 
                                    polygenic_scores_data$E4_ALC_PGC18) 


colnames(polygenic_scores_data) = c("HHIDPN", 
                                     "E4_EDU2_SSGAC16", 
                                     "E4_BMI_GIANT15", 
                                     "E4_SCZ_PGC14", 
                                     "E4_EVRSMK_TAG10", 
                                     "E4_AD2_IGAP13", 
                                     "E4_WC_GIANT15", 
                                     "E4_NEUROT_SSGAC16", 
                                     "E4_WELLB_SSGAC16", 
                                      "E4_DEPSYMP_SSGAC16",
                                      "E4_CD_CARDIOGRAM11", 
                                      "E4_MI_CARDIOGRAM15", 
                                      "E4_T2D_DIAGRAM12", 
                                      "E4_ANXFS_ANGST16", 
                                      "E4_ALC_PGC18") 

nrow(polygenic_scores_data)

na.omit(polygenic_scores_data)

HRS_all_data_PGS = right_join(HRS_all_data_SORTED_noNAs, polygenic_scores_data)


HRS_all_data_PGS$HHIDPN

write.csv(HRS_all_data_PGS, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_PGS.csv", sep = "")) 


HRS_all_data_PGS_drop_na = na.omit(HRS_all_data_PGS)

case_ID = unique(HRS_all_data_PGS_drop_na$HHIDPN) #5923

unique(HRS_all_data_PGS_drop_na$start_new)

timepoint_0 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==0 & HRS_all_data_PGS_drop_na$HHIDPN case_ID
)
nrow(timepoint_0)


checklist_depression_bin_t0 = timepoint_0$checklist_depression_bin
discrim_bin_t0 = timepoint_0$discrim_bin
timepoints_indiv_t0 = timepoint_0$timepoints_indiv


#############

timepoint_1 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==1)
nrow(timepoint_1)


timepoint_2 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==2)
nrow(timepoint_2)

timepoint_3 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==3)
nrow(timepoint_3) 

timepoint_4 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==4)
nrow(timepoint_4)

timepoint_5 = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$start_new==5)
nrow(timepoint_5)


HHIDPN = c(timepoint_0$HHIDPN, 
           timepoint_1$HHIDPN,
           timepoint_2$HHIDPN,
           timepoint_3$HHIDPN,
           timepoint_4$HHIDPN,
           timepoint_5$HHIDPN)

unique(HHIDPN)

E4_EDU2_SSGAC16 = c(timepoint_0$E4_EDU2_SSGAC16, 
                    timepoint_1$E4_EDU2_SSGAC16,
                    timepoint_2$E4_EDU2_SSGAC16,
                    timepoint_3$E4_EDU2_SSGAC16,
                    timepoint_4$E4_EDU2_SSGAC16,
                    timepoint_5$E4_EDU2_SSGAC16)


"E4_EDU2_SSGAC16", 
"E4_BMI_GIANT15", 
"E4_SCZ_PGC14", 
"E4_EVRSMK_TAG10", 
"E4_AD2_IGAP13", 
"E4_WC_GIANT15", 
"E4_NEUROT_SSGAC16", 
"E4_WELLB_SSGAC16", 
"E4_DEPSYMP_SSGAC16",
"E4_CD_CARDIOGRAM11", 
"E4_MI_CARDIOGRAM15", 
"E4_T2D_DIAGRAM12", 
"E4_ANXFS_ANGST16", 
"E4_ALC_PGC18"

# how to add the id for all time points and merge it 

timepoint_0$start_new
timepoint_0$stop_new
timepoint_0$HHIDPN

unique(HRS_all_data_PGS_drop_na$timepoints_indiv)



#################
six_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==6)
nrow(six_timepoints) #7949
unique(six_timepoints$HHIDPN) #2949 cases with six data points 

five_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==5)
nrow(five_timepoints) #
unique(five_timepoints$HHIDPN) # 1251

four_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==4)
nrow(four_timepoints) #
unique(four_timepoints$HHIDPN) # 756

three_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==3)
nrow(three_timepoints) #
unique(three_timepoints$HHIDPN) # 473

two_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==2)
nrow(two_timepoints) #
unique(two_timepoints$HHIDPN) #320

one_timepoints = subset(HRS_all_data_PGS_drop_na, HRS_all_data_PGS_drop_na$timepoints_indiv==1)
nrow(one_timepoints) #
unique(one_timepoints$HHIDPN) #167


##########
##########

write.csv(HRS_all_data_PGS_drop_na, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_PGS_drop_na.csv", sep = "")) 



#write.csv(HRS_2008_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_2008_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2010_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2010_data/HRS_2010_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2012_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2012_data/HRS_2012_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2014_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2014_data/HRS_2014_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2016_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2016_data/HRS_2016_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2018_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2018_data/HRS_2018_data_with_PGS.csv", sep = "")) 



