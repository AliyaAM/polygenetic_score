

library(dplyr)
library(tidyr)
library(stats)
library(survival)
library(survminer)

#in this analysis cases are included by merging dataframes by year/wave not by personal baseline 


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

ELSA_data_with_PGS = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
all_HRS_by_years_PGS = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 


#covariates

ELSA_data_with_PGS$w5age
ELSA_data_with_PGS$w5ethnicity
ELSA_data_with_PGS$w5sex
ELSA_data_with_PGS$w5wealth
ELSA_data_with_PGS$w5wealthq
ELSA_data_with_PGS$w5limill
ELSA_data_with_PGS$w4bmi_clean
ELSA_data_with_PGS$w5married
ELSA_data_with_PGS$ELSA_Education

#outcomes w5

ELSA_data_with_PGS$w5arthritis_new
#ELSA_data_with_PGS$w5asthma_new no asthma PGS 
#ELSA_data_with_PGS$w5cancer_new no cancer PGS 
ELSA_data_with_PGS$w5diabetes_new
#ELSA_data_with_PGS$w5lungdis_new no lungdis PGS 
ELSA_data_with_PGS$w5cesd_bin
ELSA_data_with_PGS$w5cesd
#ELSA_data_with_PGS$w5srh no srh PGS 
#ELSA_data_with_PGS$w5stroke_new no stroke PGS 

print("add anxiety phenotype, MI, CAD, chronic pain, ALZ, insomnia")

#outcomes w6
ELSA_data_with_PGS$w6arthritis_new
#ELSA_data_with_PGS$w6asthma_new no asthma PGS 
#ELSA_data_with_PGS$w6cancer_new  no cancer PGS 
ELSA_data_with_PGS$w6diabetes_new
#ELSA_data_with_PGS$w6lungdis_new no lungdis PGS 
ELSA_data_with_PGS$w6cesd_bin
ELSA_data_with_PGS$w6cesd
#ELSA_data_with_PGS$w6srh no srh PGS 
#ELSA_data_with_PGS$w6stroke_new no stroke PGS 

#outcome w7 
ELSA_data_with_PGS$w7arthritis_new
ELSA_data_with_PGS$w7diabetes_new
ELSA_data_with_PGS$w7cesd_bin
ELSA_data_with_PGS$w7cesd

#outcome w8 
ELSA_data_with_PGS$w8arthritis_new
ELSA_data_with_PGS$w8diabetes_new
ELSA_data_with_PGS$w8cesd_bin
ELSA_data_with_PGS$w8cesd




# health behaviours   
ELSA_data_with_PGS$w5smokenum
ELSA_data_with_PGS$w5alcunits
ELSA_data_with_PGS$w6pa5level


#predictor discrimination: 
ELSA_data_with_PGS$w5discrim_bin
ELSA_data_with_PGS$w5discrim_bin2
ELSA_data_with_PGS$w5agediscrimination2
ELSA_data_with_PGS$w5sexdiscrimination2
ELSA_data_with_PGS$w5racediscrimination2
ELSA_data_with_PGS$w5disabilitydiscrimination2
ELSA_data_with_PGS$w5discrim_sexuality2
ELSA_data_with_PGS$w5weightdiscrimination2

# polygenic scores
ELSA_data_with_PGS$BMI

#PGS for diseases 
unique(ELSA_data_with_PGS$RA) #empty vector 
unique(ELSA_data_with_PGS$CAD)
unique(ELSA_data_with_PGS$CAD_2018)
unique(ELSA_data_with_PGS$MI)
unique(ELSA_data_with_PGS$T2D_2018)
unique(ELSA_data_with_PGS$Diabetes)
unique(ELSA_data_with_PGS$MDD19)
unique(ELSA_data_with_PGS$DS)
unique(ELSA_data_with_PGS$ANXIETY_CC)
unique(ELSA_data_with_PGS$ANXIETY_FC)
unique(ELSA_data_with_PGS$chronic_pain_2018)
unique(ELSA_data_with_PGS$ALZ_2013)
unique(ELSA_data_with_PGS$INS_COM)

#PGS for health behaviours
ELSA_data_with_PGS$SMK_NUMBER
ELSA_data_with_PGS$DrinksPerWeek19

  
########################################
######################################## data analysis 

######### arthritis  (w 6)

arthritis_discrim = glm(w6arthritis_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_discrim)

arthritis_gene = glm(w6arthritis_new ~ RA, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene)

arthritis_gene_interaction = glm(w6arthritis_new ~ RA * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene_interaction)

###########

######### arthritis  (w 7)

arthritis_discrim__w7 = glm(w7arthritis_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_discrim__w7)

arthritis_gene_w7 = glm(w7arthritis_new ~ RA, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene_w7)

arthritis_gene_interaction_w7 = glm(w7arthritis_new ~ RA * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene_interaction_w7)


######### arthritis  (w 8)

arthritis_discrim__w8 = glm(w8arthritis_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_discrim__w8)

arthritis_gene_w8 = glm(w8arthritis_new ~ RA, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene_w8)

arthritis_gene_interaction_w8 = glm(w8arthritis_new ~ RA * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(arthritis_gene_interaction_w8)


########################################

######### diabetes  (w 6)

diabetes_discrim = glm(w6diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim)

diabetes_gene = glm(w6adiabetes_new ~ T2D_2018, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene)

diabetes_gene_interaction = glm(w6diabetes_new ~ T2D_2018 * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction)

###########

######### diabetes  (w 7)

diabetes_discrim__w7 = glm(w7diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim__w7)

diabetes_gene_w7 = glm(w7diabetes_new ~ T2D_2018, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_w7)

diabetes_gene_interaction_w7 = glm(w7diabetes_new ~ T2D_2018 * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction_w7)


######### diabetes  (w 8)

diabetes_discrim__w8 = glm(w8diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim__w8)

diabetes_gene_w8 = glm(w8diabetes_new ~ T2D_2018, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_w8)

diabetes_gene_interaction_w8 = glm(w8diabetes_new ~ T2D_2018 * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction_w8)

########################################

######### diabetes (PGS v2)  (w 6)

diabetes_discrim = glm(w6diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim)

diabetes_gene = glm(w6adiabetes_new ~ Diabetes, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene)

diabetes_gene_interaction = glm(w6diabetes_new ~ Diabetes * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction)

###########

######### diabetes  (PGS v2) (w 7)

diabetes_discrim__w7 = glm(w7diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim__w7)

diabetes_gene_w7 = glm(w7diabetes_new ~ Diabetes, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_w7)

diabetes_gene_interaction_w7 = glm(w7diabetes_new ~ Diabetes * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction_w7)


######### diabetes (PGS v2) (w 8)

diabetes_discrim__w8 = glm(w8diabetes_new ~ w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_discrim__w8)

diabetes_gene_w8 = glm(w8diabetes_new ~ Diabetes, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_w8)

diabetes_gene_interaction_w8 = glm(w8diabetes_new ~ Diabetes * w5discrim_bin2, data = ELSA_data_with_PGS, family = binomial)
summary(diabetes_gene_interaction_w8)

########################################
########################################
########################################
########################################
########################################
########################################
########################################


#HRS 
all_HRS_by_years_PGS$HRS2012_checklist_depression_bin

unique(all_HRS_by_years_PGS$HRS2012_checklist_depression_bin)

unique(all_HRS_by_years_PGS$E4_HBA1CAA_MAGIC17)

depression_discrim <- glm(HRS2012_checklist_depression_bin ~ HRS2010_discrim_bin, data = all_HRS_by_years_PGS, family = binomial)
depression_discrim_summary = summary(depression_discrim)

######
var_1 = "E4_DEPSYMP_SSGAC16"
drop_na(all_HRS_by_years_PGS, any_of(var_1))

depression_gene = lm(HRS2012_checklist_depression_bin ~ E4_DEPSYMP_SSGAC16, data = all_HRS_by_years_PGS) 
depression_gene_summary = summary(depression_gene)

#####


depression_gene = glm(HRS2012_checklist_depression_bin ~ E4_DEPSYMP_SSGAC16, data = all_HRS_by_years_PGS, family = poisson) 
depression_gene_summary = summary(depression_gene)



depression_discrimXgene = glm(HRS2012_checklist_depression_bin ~ E4_DEPSYMP_SSGAC16 * HRS2010_discrim_bin, data = all_HRS_by_years_PGS, family = poisson) 
depression_discrimXgene_summary = summary(depression_discrimXgene)


#########
##########

all_HRS_by_years_PGS$HRS2012_diabetes_new


all_HRS_by_years_PGS$HRS2012_BMI
all_HRS_by_years_PGS$HRS2012

##########
print("add to ELSA anxiety phenotype, MI, CAD, chronic pain, ALZ, insomnia")

print("for HRS: add hypertension_bin to 2018, add alcohol_days_week to 2008, anxiety to all years, add depression new bin to all years (NEW), add Alzheimer's for all years, add kidney disease for all years")


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

ls(ELSA_data_with_PGS)