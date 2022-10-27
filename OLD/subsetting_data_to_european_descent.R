

directory = "/Users/aliya/my_docs/"
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")
DATA_ROOT = "KCL_postDoc/Data_analysis/"



all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS_before_subsetting_toEUDesc.csv", sep = ""))
ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS_before_subsetting_toEUDesc.csv", sep = ""))


#save these existing files under another name (just in case)
#write.csv(all_HRS_by_years_PGS, paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS_before_subsetting_toEUDesc.csv", sep = ""))
#write.csv(ELSA_data_with_PGS, paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS_before_subsetting_toEUDesc.csv", sep = ""))




# add ethnicity variable from HRS 2010 
all_HRS_by_years_PGS$HRS2012_race_black
unique(all_HRS_by_years_PGS$HRS2012_race_white)
unique(all_HRS_by_years_PGS$HRS2012_race_nonwhite)
HRS2010_race_nonwhite
all_HRS_by_years_PGS$HRS2010_race_nonwhite
all_HRS_by_years_PGS$HRS2010_race_non
# HRS_2010_data = read.csv(paste(directory, DATA_ROOT, "HRS_2010_data/HRS2010_dataset_latest_renamed_vars.csv", sep=""))
# all_HRS_by_years_PGS$HHIDPN
# HRS_2010_data$HHIDPN = HRS_2010_data$HRS_2010_data.HHIDPN 
# 
# all_HRS_by_years_PGS =  inner_join(all_HRS_by_years_PGS, 
#                   HRS_2010_data, 
#                   by = c("HHIDPN")) 





#subset the PGS data to those with european descent because the PGS are for euoropean descent 
unique(ELSA_data_with_PGS$w5ethnicity)
table(ELSA_data_with_PGS$w5ethnicity)

ELSA_data_with_PGS_subset = subset(ELSA_data_with_PGS, ELSA_data_with_PGS$w5ethnicity == 1) 
write.csv(ELSA_data_with_PGS_subset, paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 

#rewrite the files so they are restricted to the EUdescent: 
#paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
#paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 


##########
##########
##########



######  the total score 
#unique(ELSA_data_with_PGS$w6cesd)
#[1]  4  1  8  0  3  2  5  7  6 NA

#check if the univariate regression not sig. 

ELSA_data_with_PGS$w6cesd_bin = case_when(ELSA_data_with_PGS$w6cesd == 0 ~ 0, 
                                          ELSA_data_with_PGS$w6cesd == 1 ~ 0, 
                                          ELSA_data_with_PGS$w6cesd == 2 ~ 0,
                                          ELSA_data_with_PGS$w6cesd == 3 ~ 0,
                                          ELSA_data_with_PGS$w6cesd == 4 ~ 1,
                                          ELSA_data_with_PGS$w6cesd == 5 ~ 1,
                                          ELSA_data_with_PGS$w6cesd == 6 ~ 1,
                                          ELSA_data_with_PGS$w6cesd == 7 ~ 1,
                                          ELSA_data_with_PGS$w6cesd == 8 ~ 1) 


ELSA_data_with_PGS$w7cesd_bin = case_when(ELSA_data_with_PGS$w7cesd == 0 ~ 0, 
                                          ELSA_data_with_PGS$w7cesd == 1 ~ 0, 
                                          ELSA_data_with_PGS$w7cesd == 2 ~ 0,
                                          ELSA_data_with_PGS$w7cesd == 3 ~ 0,
                                          ELSA_data_with_PGS$w7cesd == 4 ~ 1,
                                          ELSA_data_with_PGS$w7cesd == 5 ~ 1,
                                          ELSA_data_with_PGS$w7cesd == 6 ~ 1,
                                          ELSA_data_with_PGS$w7cesd == 7 ~ 1,
                                          ELSA_data_with_PGS$w7cesd == 8 ~ 1)


ELSA_data_with_PGS$w8cesd_bin = case_when(ELSA_data_with_PGS$w8cesd == 0 ~ 0, 
                                          ELSA_data_with_PGS$w8cesd == 1 ~ 0, 
                                          ELSA_data_with_PGS$w8cesd == 2 ~ 0,
                                          ELSA_data_with_PGS$w8cesd == 3 ~ 0,
                                          ELSA_data_with_PGS$w8cesd == 4 ~ 1,
                                          ELSA_data_with_PGS$w8cesd == 5 ~ 1,
                                          ELSA_data_with_PGS$w8cesd == 6 ~ 1,
                                          ELSA_data_with_PGS$w8cesd == 7 ~ 1,
                                          ELSA_data_with_PGS$w8cesd == 8 ~ 1)



ELSA_data_with_PGS$DS
#DS-PGS were significantly associated with both the cognitive-affective 
#(β=0.092, 95% CI: 0.048; 0.135) 
#and somatic (β=0.081, 95% CI: 0.039; 0.123) symptoms 
#after controlling for age, sex and 10 PCs (Table 2)

ELSA_data_with_PGS$w6_continious_age
ELSA_data_with_PGS$w6age
ELSA_data_with_PGS$w6sex

# the continious is significant but the binary is not: 

test = glm(ELSA_data_with_PGS$w6cesd ~ ELSA_data_with_PGS$DS + ELSA_data_with_PGS$pc1 +  ELSA_data_with_PGS$pc2 +  ELSA_data_with_PGS$pc3 +  ELSA_data_with_PGS$pc4 + ELSA_data_with_PGS$pc5 +ELSA_data_with_PGS$pc6 + ELSA_data_with_PGS$pc7 + ELSA_data_with_PGS$pc8 + ELSA_data_with_PGS$pc9 + ELSA_data_with_PGS$pc10 + ELSA_data_with_PGS$w6sex + ELSA_data_with_PGS$w6age) 
summary(test)


test_interaction = glm(ELSA_data_with_PGS$w6cesd ~ ELSA_data_with_PGS$DS * ELSA_data_with_PGS$w5discrim_bin + ELSA_data_with_PGS$pc1 +  ELSA_data_with_PGS$pc2 +  ELSA_data_with_PGS$pc3 +  ELSA_data_with_PGS$pc4 + ELSA_data_with_PGS$pc5 +ELSA_data_with_PGS$pc6 + ELSA_data_with_PGS$pc7 + ELSA_data_with_PGS$pc8 + ELSA_data_with_PGS$pc9 + ELSA_data_with_PGS$pc10 + ELSA_data_with_PGS$w6sex + ELSA_data_with_PGS$w6age) 
summary(test_interaction)


test_noage_no_sex = glm(ELSA_data_with_PGS$w6cesd ~ ELSA_data_with_PGS$DS + ELSA_data_with_PGS$pc1 +  ELSA_data_with_PGS$pc2 +  ELSA_data_with_PGS$pc3 +  ELSA_data_with_PGS$pc4 + ELSA_data_with_PGS$pc5 +ELSA_data_with_PGS$pc6 + ELSA_data_with_PGS$pc7 + ELSA_data_with_PGS$pc8 + ELSA_data_with_PGS$pc9 + ELSA_data_with_PGS$pc10) 
summary(test_noage_no_sex)

