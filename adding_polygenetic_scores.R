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

write.csv(HRS_all_data_SORTED, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_SORTED.csv", sep = "")) 
HRS_all_data_SORTED_drop_na = na.omit(HRS_all_data_SORTED)
write.csv(HRS_all_data_SORTED_drop_na, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_SORTED_drop_na.csv", sep = "")) 

case_ID = unique(HRS_all_data_SORTED_drop_na$HHIDPN) #5923
unique(HRS_all_data_SORTED_drop_na$start_new)


#### all responses at timepoint 5: 

timepoint_5 = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==5)
nrow(timepoint_5)



checklist_depression_bin_t5 = timepoint_5$checklist_depression_bin
discrim_bin_t5 = timepoint_5$discrim_bin
timepoints_indiv_t5 = timepoint_5$timepoints_indiv



timepoint_4 = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==4)
nrow(timepoint_4)

checklist_depression_bin_t4 = timepoint_4$checklist_depression_bin
discrim_bin_t4 = timepoint_4$discrim_bin
timepoints_indiv_t4 = timepoint_4$timepoints_indiv



timepoint_3 = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==3)
nrow(timepoint_3) 

checklist_depression_bin_t3 = timepoint_3$checklist_depression_bin
discrim_bin_t3 = timepoint_3$discrim_bin
timepoints_indiv_t3 = timepoint_3$timepoints_indiv


#### all responses at timepoint 0 but restricted to those in who took part in timepoint 5
ID_timepoint_3 = unique(timepoint_3$HHIDPN)


timepoint_0_subset = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==0)
nrow(timepoint_0_subset)

timepoint_0 = subset(timepoint_0_subset, timepoint_0_subset$HHIDPN %in% ID_timepoint_3)
nrow(timepoint_0)

checklist_depression_bin_t0 = timepoint_0$checklist_depression_bin
discrim_bin_t0 = timepoint_0$discrim_bin
timepoints_indiv_t0 = timepoint_0$timepoints_indiv

ID_timepoint_0 = unique(timepoint_0$HHIDPN)
#############

timepoint_1_subset = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==1)
nrow(timepoint_1)

timepoint_1 = subset(timepoint_1_subset, timepoint_1_subset$HHIDPN %in% ID_timepoint_3)
nrow(timepoint_1)

checklist_depression_bin_t1 = timepoint_1$checklist_depression_bin
discrim_bin_t1 = timepoint_1$discrim_bin
timepoints_indiv_t1 = timepoint_1$timepoints_indiv


timepoint_2 = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$start_new==2)
nrow(timepoint_2)

checklist_depression_bin_t2 = timepoint_2$checklist_depression_bin
discrim_bin_t2 = timepoint_2$discrim_bin
timepoints_indiv_t2 = timepoint_2$timepoints_indiv



############
# 
# (checklist_depression_bin_t1, 
#   checklist_depression_bin_t2, 
#   checklist_depression_bin_t3, 
#   checklist_depression_bin_t4, 
#   checklist_depression_bin_t5, 
#   discrim_bin_t1, 
#   discrim_bin_t2, 
#   discrim_bin_t3, 
#   discrim_bin_t4, 
#   discrim_bin_t5, 
#   timepoints_indiv_t1, 
#   timepoints_indiv_t2, 
#   timepoints_indiv_t3, 
#   timepoints_indiv_t4, 
#   timepoints_indiv_t5)



#"E4_EDU2_SSGAC16", 
#"E4_BMI_GIANT15", 
#"E4_SCZ_PGC14", 
#"E4_EVRSMK_TAG10", 
#"E4_AD2_IGAP13", 
#"E4_WC_GIANT15", 
#"E4_NEUROT_SSGAC16", 
#"E4_WELLB_SSGAC16", 
#"E4_DEPSYMP_SSGAC16",
#"E4_CD_CARDIOGRAM11", 
#"E4_MI_CARDIOGRAM15", 
#"E4_T2D_DIAGRAM12", 
#"E4_ANXFS_ANGST16", 
#"E4_ALC_PGC18"


#################
six_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==6)
nrow(six_timepoints) #7949
unique(six_timepoints$HHIDPN) #2949 cases with six data points 

five_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==5)
nrow(five_timepoints) #
unique(five_timepoints$HHIDPN) # 1251

four_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==4)
nrow(four_timepoints) #
unique(four_timepoints$HHIDPN) # 756

three_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==3)
nrow(three_timepoints) #
unique(three_timepoints$HHIDPN) # 473

two_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==2)
nrow(two_timepoints) #
unique(two_timepoints$HHIDPN) #320

one_timepoints = subset(HRS_all_data_SORTED_drop_na, HRS_all_data_SORTED_drop_na$timepoints_indiv==1)
nrow(one_timepoints) #
unique(one_timepoints$HHIDPN) #167


#######
####### ADD PGS
#######


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

HRS_all_data_PGS = right_join(HRS_all_data_SORTED_drop_na, polygenic_scores_data)


HRS_all_data_PGS$HHIDPN

HRS_all_data_PGS_drop_na = na.omit(HRS_all_data_PGS)

write.csv(HRS_all_data_PGS, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_PGS.csv", sep = "")) 
write.csv(HRS_all_data_PGS_drop_na, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_all_data_PGS_drop_na.csv", sep = "")) 

##########
##########




#write.csv(HRS_2008_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2008_data/HRS_2008_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2010_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2010_data/HRS_2010_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2012_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2012_data/HRS_2012_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2014_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2014_data/HRS_2014_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2016_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2016_data/HRS_2016_data_with_PGS.csv", sep = "")) 
#write.csv(HRS_2018_data_with_PGS, file =  paste(directory, DATA_ROOT, "HRS_2018_data/HRS_2018_data_with_PGS.csv", sep = "")) 



