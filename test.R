


#Fit Proportional Hazards Regression Model

#the Dementia paper with PGS (Olesia): replicate for MI and Diabetes: https://agsjournals.onlinelibrary.wiley.com/doi/full/10.1111/jgs.16406
#PGS x SES on time to dementia diagnosis 
#instead of Cox proportional hazard model, they used the AFT survival model (Sasty et al., 1997) 
#We could assess the PGS x discrimination effect on the time to MI/diabetes diagnosis 
#They have also adjusted for the presence of a particular allele known to be associated with dementia, I do not know enough about this to know if we should adjust for any alleles for MI/diabetes analysis 
#Using Bayesian shrinkage to adjust for PCAs more efficiently to avoid sparsity (LASSO-like). 

library("data.table")
library("survival")
library("sjPlot")
library("sjlabelled")




directory = "/Users/aliya/my_docs/"

#"/Users/aliyaamirova/"

###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = paste(directory, "proj/polygenetic_score/", sep = "")

OUTPUT_ROOT = paste(directory, "proj/polygenetic_score/OUTPUT/", sep = "")

folder <- OUTPUT_ROOT

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}

###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))
source(paste(SOURCE_ROOT, "cox_model_PGS.R", sep=""))
source(paste(SOURCE_ROOT, "glm_model_PGS.R", sep=""))
source(paste(SOURCE_ROOT, "glm_gaussian_PGS.R", sep=""))

#source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



DATA_ROOT = "KCL_postDoc/Data_analysis/"


###########################

all_HRS_by_years_PGS_before_subsetting_to_baseline_free = read.csv(paste(directory, DATA_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep = "")) 
ELSA_data_with_PGS_before_subsetting_to_baseline_free = read.csv(paste(directory, DATA_ROOT, "DATA_ELSA/ELSA_data_with_PGS.csv", sep = "")) 
#all_HRS_by_years_PGS_before_subsetting_to_baseline_free$HRS2010_diabetes_ever
#all_HRS_by_years_PGS_before_subsetting_to_baseline_free$HRS2010_diabetes_new

#subset to those who did not have mi at baseline
#ELSA_data_with_PGS  = subset(ELSA_data_with_PGS_before_subsetting_to_baseline_free, ELSA_data_with_PGS_before_subsetting_to_baseline_free$w4_MI_new_bin == 0)
#all_HRS_by_years_PGS  = subset(all_HRS_by_years_PGS_before_subsetting_to_baseline_free, all_HRS_by_years_PGS_before_subsetting_to_baseline_free$ == 0 & all_HRS_by_years_PGS_before_subsetting_to_baseline_free$ == 0)
#
ELSA_data_with_PGS  = ELSA_data_with_PGS_before_subsetting_to_baseline_free
all_HRS_by_years_PGS = all_HRS_by_years_PGS_before_subsetting_to_baseline_free



# reorganise the dataset so there is time variable and other columns include outcomes at all time points 
##### ELSA data preparation 

list(ELSA_data_with_PGS)
#gene_ELSA = "MI"

ID_ELSA = c(ELSA_data_with_PGS$idauniq, 
            ELSA_data_with_PGS$idauniq, 
            ELSA_data_with_PGS$idauniq, 
            ELSA_data_with_PGS$idauniq)

data_cox_ELSA_initial = data.frame(ID_ELSA)

unique(ELSA_data_with_PGS$w5_mi_new_bin_g2)
unique(ELSA_data_with_PGS$w6_mi_new_bin_g2)
unique(ELSA_data_with_PGS$w7_mi_new_bin_g2)
unique(ELSA_data_with_PGS$w8_mi_new_bin_g2)

ELSA_data_with_PGS$w5_mi_new_bin_g2

data_cox_ELSA_initial$MI_outcome = c(ELSA_data_with_PGS$w5_mi_new_bin_g2,
                                     ELSA_data_with_PGS$w6_mi_new_bin_g2,
                                     ELSA_data_with_PGS$w7_mi_new_bin_g2,
                                     ELSA_data_with_PGS$w8_mi_new_bin_g2)


# data_cox_ELSA_initial$MI_outcome = c(ELSA_data_with_PGS$w5_MI_new_bin,
#                                      ELSA_data_with_PGS$w6_MI_new_bin,
#                                      ELSA_data_with_PGS$w7_MI_new_bin,
#                                      ELSA_data_with_PGS$w8_MI_new_bin)

unique(data_cox_ELSA_initial$MI_outcome)
table(data_cox_ELSA_initial$MI_outcome)

time_point_1_ELSA = rep(1, nrow(ELSA_data_with_PGS))
time_point_2_ELSA = rep(2, nrow(ELSA_data_with_PGS))
time_point_3_ELSA = rep(3, nrow(ELSA_data_with_PGS))
time_point_4_ELSA = rep(4, nrow(ELSA_data_with_PGS))

data_cox_ELSA_initial$time_point = c(time_point_1_ELSA, 
                                     time_point_2_ELSA,
                                     time_point_3_ELSA,
                                     time_point_4_ELSA)

data_cox_ELSA_initial$years =  data_cox_ELSA_initial$time_point

data_cox_ELSA_initial$follow_up = data_cox_ELSA_initial$years


data_cox_ELSA_initial$age = c(ELSA_data_with_PGS$w5age,
                              ELSA_data_with_PGS$w6age, 
                              ELSA_data_with_PGS$w7age, 
                              ELSA_data_with_PGS$w8age)


data_cox_ELSA_initial$sex = c(ELSA_data_with_PGS$w5sex,
                              ELSA_data_with_PGS$w6sex, 
                              ELSA_data_with_PGS$w7sex, 
                              ELSA_data_with_PGS$w8sex)

data_cox_ELSA_initial$PGS_mi = c(ELSA_data_with_PGS$MI, 
                                 ELSA_data_with_PGS$MI, 
                                 ELSA_data_with_PGS$MI,
                                 ELSA_data_with_PGS$MI) 



data_cox_ELSA_initial$wealth = c(ELSA_data_with_PGS$w5wealth, 
                                 ELSA_data_with_PGS$w6wealth, 
                                 ELSA_data_with_PGS$w7wealth, 
                                 ELSA_data_with_PGS$w7wealth)

data_cox_ELSA_initial$baseline_discriminaition = c(ELSA_data_with_PGS$w5discrim_bin, 
                                                   ELSA_data_with_PGS$w5discrim_bin, 
                                                   ELSA_data_with_PGS$w5discrim_bin, 
                                                   ELSA_data_with_PGS$w5discrim_bin)


data_cox_ELSA_initial$discriminaition =  c(ELSA_data_with_PGS$w5discrim_bin, 
                                           ELSA_data_with_PGS$w6discrim_bin, 
                                           ELSA_data_with_PGS$w7discrim_bin, 
                                           ELSA_data_with_PGS$w8discrim_bin) 

unique(data_cox_ELSA_initial$discriminaition)

data_cox_ELSA_initial$pc1 = c(ELSA_data_with_PGS$pc1, 
                              ELSA_data_with_PGS$pc1, 
                              ELSA_data_with_PGS$pc1, 
                              ELSA_data_with_PGS$pc1) 


data_cox_ELSA_initial$pc2 = c(ELSA_data_with_PGS$pc2, 
                              ELSA_data_with_PGS$pc2, 
                              ELSA_data_with_PGS$pc2, 
                              ELSA_data_with_PGS$pc2) 


data_cox_ELSA_initial$pc3 = c(ELSA_data_with_PGS$pc3, 
                              ELSA_data_with_PGS$pc3, 
                              ELSA_data_with_PGS$pc3, 
                              ELSA_data_with_PGS$pc3) 


data_cox_ELSA_initial$pc4 = c(ELSA_data_with_PGS$pc4, 
                              ELSA_data_with_PGS$pc4, 
                              ELSA_data_with_PGS$pc4, 
                              ELSA_data_with_PGS$pc4) 



data_cox_ELSA_initial$pc5 = c(ELSA_data_with_PGS$pc5, 
                              ELSA_data_with_PGS$pc5, 
                              ELSA_data_with_PGS$pc5, 
                              ELSA_data_with_PGS$pc5) 

data_cox_ELSA_initial$pc6 = c(ELSA_data_with_PGS$pc6, 
                              ELSA_data_with_PGS$pc6, 
                              ELSA_data_with_PGS$pc6, 
                              ELSA_data_with_PGS$pc6) 

data_cox_ELSA_initial$pc7 = c(ELSA_data_with_PGS$pc7, 
                              ELSA_data_with_PGS$pc7, 
                              ELSA_data_with_PGS$pc7, 
                              ELSA_data_with_PGS$pc7) 


data_cox_ELSA_initial$pc8 = c(ELSA_data_with_PGS$pc8, 
                              ELSA_data_with_PGS$pc8, 
                              ELSA_data_with_PGS$pc8, 
                              ELSA_data_with_PGS$pc8) 


data_cox_ELSA_initial$pc9 = c(ELSA_data_with_PGS$pc9, 
                              ELSA_data_with_PGS$pc9, 
                              ELSA_data_with_PGS$pc9, 
                              ELSA_data_with_PGS$pc9) 


data_cox_ELSA_initial$pc10 = c(ELSA_data_with_PGS$pc10, 
                               ELSA_data_with_PGS$pc10, 
                               ELSA_data_with_PGS$pc10, 
                               ELSA_data_with_PGS$pc10) 

data_cox_ELSA_initial$alcohol = c(ELSA_data_with_PGS$w5alcunits, 
                                  ELSA_data_with_PGS$w6alcunits, 
                                  ELSA_data_with_PGS$w7alcunits,
                                  ELSA_data_with_PGS$w7alcunits) 



unique(data_cox_ELSA_initial$alcohol)

unique(ELSA_data_with_PGS$w5smokec)
unique(ELSA_data_with_PGS$w5smoket)

data_cox_ELSA_initial$smoking = c(ELSA_data_with_PGS$w5smokenum, 
                                  ELSA_data_with_PGS$w6smokenum,
                                  ELSA_data_with_PGS$w7smokenum,
                                  ELSA_data_with_PGS$w7smokenum)


unique(data_cox_ELSA_initial$smoking)


data_cox_ELSA_initial$physical_activity  = c(ELSA_data_with_PGS$w5pacomb1, 
                                             ELSA_data_with_PGS$w6pacomb1,
                                             ELSA_data_with_PGS$w7pacomb1,
                                             ELSA_data_with_PGS$w7pacomb1)

unique(data_cox_ELSA_initial$physical_activity)

data_cox_ELSA_initial$BMI = c(ELSA_data_with_PGS$w6bmi_clean, 
                              ELSA_data_with_PGS$w6bmi_clean, 
                              ELSA_data_with_PGS$w6bmi_clean, 
                              ELSA_data_with_PGS$w6bmi_clean)

data_cox_ELSA_initial$diabetes_history = c(ELSA_data_with_PGS$w5diabetes_ever,
                                           ELSA_data_with_PGS$w6diabetes_ever,
                                           ELSA_data_with_PGS$w7diabetes_ever,
                                           ELSA_data_with_PGS$w8diabetes_ever) 


unique(data_cox_ELSA_initial$diabetes_history)

#ELSA_data_with_PGS$hypertension 


#add: hypertension_history stressful_event 
data_cox_ELSA_initial$depression_original = c(ELSA_data_with_PGS$w5cesd, 
                                              ELSA_data_with_PGS$w6cesd, 
                                              ELSA_data_with_PGS$w7cesd, 
                                              ELSA_data_with_PGS$w8cesd) 

unique(data_cox_ELSA_initial$depression_original)

data_cox_ELSA_initial$depression = 8 - data_cox_ELSA_initial$depression_original

data_cox_ELSA_initial$PGS_diab = c(ELSA_data_with_PGS$T2D_2018, 
                                   ELSA_data_with_PGS$T2D_2018, 
                                   ELSA_data_with_PGS$T2D_2018, 
                                   ELSA_data_with_PGS$T2D_2018)


data_cox_ELSA_initial$diabetes_outcome = c(ELSA_data_with_PGS$w5diabetes_new, 
                                           ELSA_data_with_PGS$w6diabetes_new,
                                           ELSA_data_with_PGS$w7diabetes_new,
                                           ELSA_data_with_PGS$w8diabetes_new)




data_cox_ELSA_initial$PGS_depres_symp = c(ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS)

#gene_ELSA = "MDD19"
#gene_ELSA = "DS"



data_cox_ELSA_initial$PGS_depres_symp = c(ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS, 
                                          ELSA_data_with_PGS$DS)



data_cox_ELSA_initial$depressive_symptoms = c(ELSA_data_with_PGS$w5cesd, 
                                              ELSA_data_with_PGS$w6cesd, 
                                              ELSA_data_with_PGS$w7cesd, 
                                              ELSA_data_with_PGS$w8cesd)


length(unique(data_cox_ELSA_initial$ID))
summary(data_cox_ELSA_initial)


##########################################

##### HRS data preparation 


all_HRS_by_years_PGS$pc1 = all_HRS_by_years_PGS$PC1_5A
all_HRS_by_years_PGS$pc2 = all_HRS_by_years_PGS$PC1_5B
all_HRS_by_years_PGS$pc3 = all_HRS_by_years_PGS$PC1_5C
all_HRS_by_years_PGS$pc4 = all_HRS_by_years_PGS$PC1_5D
all_HRS_by_years_PGS$pc5 = all_HRS_by_years_PGS$PC1_5E 
all_HRS_by_years_PGS$pc6 = all_HRS_by_years_PGS$PC6_10A
all_HRS_by_years_PGS$pc7 = all_HRS_by_years_PGS$PC6_10B
all_HRS_by_years_PGS$pc8 = all_HRS_by_years_PGS$PC6_10C
all_HRS_by_years_PGS$pc9 = all_HRS_by_years_PGS$PC6_10D
all_HRS_by_years_PGS$pc10 = all_HRS_by_years_PGS$PC6_10E


#gene_HRS = "E4_MI_CARDIOGRAM15"


ID_HRS = c(all_HRS_by_years_PGS$HHIDPN, 
           all_HRS_by_years_PGS$HHIDPN, 
           all_HRS_by_years_PGS$HHIDPN, 
           all_HRS_by_years_PGS$HHIDPN)

data_cox_HRS_initial = data.frame(ID_HRS)


data_cox_HRS_initial$MI_outcome = c(all_HRS_by_years_PGS$HRS2010_mi_bin,
                                    all_HRS_by_years_PGS$HRS2012_mi_bin,
                                    all_HRS_by_years_PGS$HRS2014_mi_bin,
                                    all_HRS_by_years_PGS$HRS2016_mi_bin)

unique(data_cox_HRS_initial$MI_outcome)
table(data_cox_HRS_initial$MI_outcome)

time_point_1_HRS = rep(1, nrow(all_HRS_by_years_PGS))
time_point_2_HRS = rep(2, nrow(all_HRS_by_years_PGS))
time_point_3_HRS = rep(3, nrow(all_HRS_by_years_PGS))
time_point_4_HRS = rep(4, nrow(all_HRS_by_years_PGS))

data_cox_HRS_initial$time_point = c(time_point_1_HRS, 
                                    time_point_2_HRS,
                                    time_point_3_HRS,
                                    time_point_4_HRS)

data_cox_HRS_initial$years =  data_cox_HRS_initial$time_point

data_cox_HRS_initial$follow_up = data_cox_HRS_initial$years

data_cox_HRS_initial$age = c(all_HRS_by_years_PGS$HRS2010_continious_age,
                             all_HRS_by_years_PGS$HRS2012_continious_age, 
                             all_HRS_by_years_PGS$HRS2014_continious_age, 
                             all_HRS_by_years_PGS$HRS2016_continious_age)


data_cox_HRS_initial$sex = c(all_HRS_by_years_PGS$HRS2010_sex_1_0,
                             all_HRS_by_years_PGS$HRS2012_sex_1_0, 
                             all_HRS_by_years_PGS$HRS2014_sex_1_0, 
                             all_HRS_by_years_PGS$HRS2014_sex_1_0)


data_cox_HRS_initial$PGS_mi = c(all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15, 
                                all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15, 
                                all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15, 
                                all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15) 


data_cox_HRS_initial$wealth = c(all_HRS_by_years_PGS$HRS2010_wealth_noIRA, 
                                all_HRS_by_years_PGS$HRS2012_wealth_noIRA, 
                                all_HRS_by_years_PGS$HRS2014_wealth_noIRA, 
                                all_HRS_by_years_PGS$HRS2016_wealth_noIRA)


data_cox_HRS_initial$baseline_discriminaition = c(all_HRS_by_years_PGS$HRS2010_discrim_bin, 
                                                  all_HRS_by_years_PGS$HRS2010_discrim_bin, 
                                                  all_HRS_by_years_PGS$HRS2010_discrim_bin, 
                                                  all_HRS_by_years_PGS$HRS2010_discrim_bin)


data_cox_HRS_initial$discriminaition =  c(all_HRS_by_years_PGS$HRS2010_discrim_bin, 
                                          all_HRS_by_years_PGS$HRS2012_discrim_bin, 
                                          all_HRS_by_years_PGS$HRS2014_discrim_bin, 
                                          all_HRS_by_years_PGS$HRS2016_discrim_bin) 

unique(data_cox_HRS_initial$discriminaition)

data_cox_HRS_initial$pc1 = c(all_HRS_by_years_PGS$pc1, 
                             all_HRS_by_years_PGS$pc1, 
                             all_HRS_by_years_PGS$pc1, 
                             all_HRS_by_years_PGS$pc1) 


data_cox_HRS_initial$pc2 = c(all_HRS_by_years_PGS$pc2, 
                             all_HRS_by_years_PGS$pc2, 
                             all_HRS_by_years_PGS$pc2, 
                             all_HRS_by_years_PGS$pc2) 


data_cox_HRS_initial$pc3 = c(all_HRS_by_years_PGS$pc3, 
                             all_HRS_by_years_PGS$pc3, 
                             all_HRS_by_years_PGS$pc3, 
                             all_HRS_by_years_PGS$pc3) 


data_cox_HRS_initial$pc4 = c(all_HRS_by_years_PGS$pc4, 
                             all_HRS_by_years_PGS$pc4, 
                             all_HRS_by_years_PGS$pc4, 
                             all_HRS_by_years_PGS$pc4) 



data_cox_HRS_initial$pc5 = c(all_HRS_by_years_PGS$pc5, 
                             all_HRS_by_years_PGS$pc5, 
                             all_HRS_by_years_PGS$pc5, 
                             all_HRS_by_years_PGS$pc5) 

data_cox_HRS_initial$pc6 = c(all_HRS_by_years_PGS$pc6, 
                             all_HRS_by_years_PGS$pc6, 
                             all_HRS_by_years_PGS$pc6, 
                             all_HRS_by_years_PGS$pc6) 

data_cox_HRS_initial$pc7 = c(all_HRS_by_years_PGS$pc7, 
                             all_HRS_by_years_PGS$pc7, 
                             all_HRS_by_years_PGS$pc7, 
                             all_HRS_by_years_PGS$pc7) 


data_cox_HRS_initial$pc8 = c(all_HRS_by_years_PGS$pc8, 
                             all_HRS_by_years_PGS$pc8, 
                             all_HRS_by_years_PGS$pc8, 
                             all_HRS_by_years_PGS$pc8) 


data_cox_HRS_initial$pc9 = c(all_HRS_by_years_PGS$pc9, 
                             all_HRS_by_years_PGS$pc9, 
                             all_HRS_by_years_PGS$pc9, 
                             all_HRS_by_years_PGS$pc9) 


data_cox_HRS_initial$pc10 = c(all_HRS_by_years_PGS$pc10, 
                              all_HRS_by_years_PGS$pc10, 
                              all_HRS_by_years_PGS$pc10, 
                              all_HRS_by_years_PGS$pc10) 


data_cox_HRS_initial$alcohol = c(all_HRS_by_years_PGS$HRS2010_alcohol_days_week, 
                                 all_HRS_by_years_PGS$HRS2012_alcohol_days_week, 
                                 all_HRS_by_years_PGS$HRS2014_alcohol_days_week,
                                 all_HRS_by_years_PGS$HRS2016_alcohol_days_week) 



unique(data_cox_HRS_initial$alcohol)


data_cox_HRS_initial$smoking = c(all_HRS_by_years_PGS$HRS2010_smokes_ever_bin, 
                                 all_HRS_by_years_PGS$HRS2012_smokes_ever_bin,
                                 all_HRS_by_years_PGS$HRS2014_smokes_ever_bin,
                                 all_HRS_by_years_PGS$HRS2016_smokes_ever_bin)


unique(data_cox_HRS_initial$smoking)

data_cox_HRS_initial$physical_activity  = c(all_HRS_by_years_PGS$HRS2010_vigarious_physical_activity, 
                                            all_HRS_by_years_PGS$HRS2012_vigarious_physical_activity,
                                            all_HRS_by_years_PGS$HRS2014_vigarious_physical_activity,
                                            all_HRS_by_years_PGS$HRS2016_vigarious_physical_activity)

unique(data_cox_HRS_initial$physical_activity)

data_cox_HRS_initial$BMI = c(all_HRS_by_years_PGS$HRS2010_BMI, 
                             all_HRS_by_years_PGS$HRS2012_BMI, 
                             all_HRS_by_years_PGS$HRS2014_BMI, 
                             all_HRS_by_years_PGS$HRS2016_BMI)


data_cox_HRS_initial$diabetes_history = c(all_HRS_by_years_PGS$HRS2010_diabetes_ever,
                                          all_HRS_by_years_PGS$HRS2012_diabetes_ever,
                                          all_HRS_by_years_PGS$HRS2014_diabetes_ever,
                                          all_HRS_by_years_PGS$HRS2016_diabetes_ever) 


unique(data_cox_HRS_initial$diabetes_history)

#all_HRS_by_years_PGS$hypertension 

#add: hypertension_history stressful_event 

data_cox_HRS_initial$depression = c(all_HRS_by_years_PGS$HRS2010_checklist_depression_bin, 
                                    all_HRS_by_years_PGS$HRS2012_checklist_depression_bin, 
                                    all_HRS_by_years_PGS$HRS2014_checklist_depression_bin, 
                                    all_HRS_by_years_PGS$HRS2016_checklist_depression_bin) 




data_cox_HRS_initial$PGS_diab = c(all_HRS_by_years_PGS$E4_T2D_DIAGRAM12, 
                                  all_HRS_by_years_PGS$E4_T2D_DIAGRAM12, 
                                  all_HRS_by_years_PGS$E4_T2D_DIAGRAM12, 
                                  all_HRS_by_years_PGS$E4_T2D_DIAGRAM12)


data_cox_HRS_initial$diabetes_outcome = c(all_HRS_by_years_PGS$HRS2010_diabetes_new, 
                                          all_HRS_by_years_PGS$HRS2012_diabetes_new,
                                          all_HRS_by_years_PGS$HRS2014_diabetes_new,
                                          all_HRS_by_years_PGS$HRS2016_diabetes_new)




data_cox_HRS_initial$PGS_depres_symp = c(all_HRS_by_years_PGS$E4_DEPSYMP_SSGAC16, 
                                         all_HRS_by_years_PGS$E4_DEPSYMP_SSGAC16, 
                                         all_HRS_by_years_PGS$E4_DEPSYMP_SSGAC16, 
                                         all_HRS_by_years_PGS$E4_DEPSYMP_SSGAC16)



# depressive symptoms: r10cesd in RAND file ADD (also run the other analysis per wave and )

length(unique(data_cox_HRS_initial$ID_HRS))
summary(data_cox_HRS_initial)



data_cox_HRS_table = data.table(data_cox_HRS_initial)

data_cox_HRS = na.omit(data_cox_HRS_table, cols = c("baseline_discriminaition",
                                                    "MI_outcome",
                                                    "PGS_mi"))
data_cox_HRS = as.data.frame(data_cox_HRS)
length(unique(data_cox_HRS$ID))
summary(data_cox_HRS)
data_cox_HRS$MI_outcome




data_cox_input = data_cox_HRS
  baseline_discriminaition = "baseline_discriminaition"
  outcome = "diabetes_outcome"
  PGS = "PGS_diab"
  plot_name = "HRS, T2DM, Cox regression: "

  
  data_cox_input$PGS = data_cox_input[ ,   PGS]
  
  data_cox_input$baseline_discriminaition = data_cox_input[ ,   baseline_discriminaition]
  
  Univariate_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition, dist = "Weibull", data = data_cox_input))
  M_uni_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition, dist = "Weibull", data = data_cox_input)
  Univariate_outcome_gene_interaction = summary(M_uni_interaction)
  

  M_uni_interaction$res
  
  #data_cox_input = data.frame(_data_with_PGS$age)
  #from literature: Model 1: Age, race, education, family income, marital status
  flexsurvreg( Surv(follow_up, MI_outcome) ~ PGS_mi + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "gamma", data = data_cox_HRS)
  
  flexsurvreg( Surv(follow_up, MI_outcome) ~ baseline_discriminaition*PGS_mi + scale(age) + sex + scale(wealth) + diabetes_history + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "gamma", data = data_cox_HRS)
  
  Model_1_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_1_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth, dist = "Weibull", data = data_cox_input))
  
  # plot interaction: https://stats.stackexchange.com/questions/464700/representing-interaction-plot-for-flexsurvreg-model-using-plot-model-in-r
 
  print("below: Warning message:
    In flexsurvreg(Surv(follow_up, data_cox_input[, outcome]) ~ PGS *  :
                     Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite.
try anc, read cran about the number of covariates and also anc") 
  
   M_1_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, 
                                  dist = "gamma", data = data_cox_input)
  Model_1_outcome_gene_interaction = summary(M_1_interaction)
  
  #Model 2: 
  #+ alcohol use, smoking status, moderate and vagarious exercise	
  
  Model_2_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_2_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity, dist = "Weibull", data = data_cox_input))
  
  M_2_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_2_outcome_gene_interaction = summary( M_2_interaction)
  
  
  ########## alcohol: 
  Model_2a_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_2a_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol, dist = "Weibull", data = data_cox_input))
  
  M_2a_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_2a_outcome_gene_interaction = summary( M_2a_interaction)

  
  
  #scale(smoking) + scale(physical_activity) +
  Model_2s_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + smoking + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_2s_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + smoking, dist = "Weibull", data = data_cox_input))
  
  M_2s_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + smoking +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_2s_outcome_gene_interaction = summary( M_2s_interaction)
  
  
  ########### 
  
  
  #scale(smoking) + scale(physical_activity) +
  Model_2pa_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_2pa_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + physical_activity, dist = "Weibull", data = data_cox_input))
  
  M_2pa_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_2pa_outcome_gene_interaction = summary( M_2pa_interaction)
  
  #Model 3: 
  #+ BMI, (height separately) systolic blood pressure, antihypertensive medication, Diabetes/fasting blood glucose status, total cholesterol, high-density lipoprotein cholesterol, low-density lipoprotein cholesterol, triglycerides, and use of lipid lowering medication, history of diabetes and hypertension. 	
  Model_3_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth +  diabetes_history + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_3_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + diabetes_history, dist = "Weibull", data = data_cox_input))
  
  M_3_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + diabetes_history + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_3_outcome_gene_interaction = summary( M_3_interaction)
  
  #Model 4: 
  #+  depressive symptoms and chronic stress burden/ stressful events
  
  Model_4_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_4_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth  + depression, dist = "Weibull", data = data_cox_input))
  M_4_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  Model_4_outcome_gene_interaction = summary( M_4_interaction)
  
  
  Model_4_outcome_gene_results = cbind(Model_4_outcome_gene$coefficients, Model_4_outcome_gene$conf.int)
  head(Model_4_outcome_gene_results, 1)
  Model_4_outcome_discrim_results = cbind(Model_4_outcome_discrim$coefficients, Model_4_outcome_discrim$conf.int)
  head(Model_4_outcome_discrim_results, 1)
  Model_4_outcome_gene_interaction_results = cbind(Model_4_outcome_gene_interaction$coefficients, Model_4_outcome_gene_interaction$conf.int)
  tail(Model_4_outcome_gene_interaction_results, 1)
  
  
  #################
  
  #Model 5: all 
  
  Model_5_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input))
  Model_5_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression, dist = "Weibull", data = data_cox_input))
  
  M_5_interaction = flexsurvreg( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = "Weibull", data = data_cox_input)
  
  Model_5_outcome_gene_interaction = summary( M_5_interaction)
  
  Model_5_outcome_gene_results = cbind(Model_5_outcome_gene$coefficients, Model_5_outcome_gene$conf.int)
  head(Model_5_outcome_gene_results, 1)
  Model_5_outcome_discrim_results = cbind(Model_5_outcome_discrim$coefficients, Model_5_outcome_discrim$conf.int)
  head(Model_5_outcome_discrim_results, 1)
  Model_5_outcome_gene_interaction_results = cbind(Model_5_outcome_gene_interaction$coefficients, Model_5_outcome_gene_interaction$conf.int)
  tail(Model_5_outcome_gene_interaction_results, 1)
  
  
  
  #ties = "efron"
  
  ####### plots 
  
  plot_M_uni_int = plot_model(#title(main = NULL), 
                              title = "",
                              axis.title = "Hazard Ratio",
                              show.values = TRUE, 
                              show.p = TRUE, 
                              M_uni_interaction,
                              terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_M_1_int = plot_model(title(main = NULL), 
                            #title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_1_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_M_2_int = plot_model(title(main = NULL), 
                            #title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_2_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  #alcohol
  plot_M_2a_int = plot_model(title(main = NULL), 
                             #title = "",
                             axis.title = "Hazard Ratio",
                             show.values = TRUE, 
                             show.p = TRUE, 
                             M_2a_interaction,
                             terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  #smoking
  plot_M_2s_int = plot_model(title(main = NULL), 
                             #title = "",
                             axis.title = "Hazard Ratio",
                             show.values = TRUE, 
                             show.p = TRUE, 
                             M_2s_interaction, 
                             terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  #physical activity
  plot_M_2pa_int = plot_model(title(main = NULL), 
                              #title = "",
                              axis.title = "Hazard Ratio",
                              show.values = TRUE, 
                              show.p = TRUE, 
                              M_2pa_interaction, 
                              terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_M_3_int = plot_model(title(main = NULL), 
                            #title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_3_interaction, 
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_M_4_int = plot_model(title(main = NULL), 
                            #title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_4_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_M_5_int = plot_model(title(main = NULL), 
                            #title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_5_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  
  plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int, margin = 0.005))
  
  # , tags = c("Univariate",
  #                                                                                                               "Model 1",
  #                                                                                                               "Model 2",
  #                                                                                                               "Model 3", 
  #                                                                                                               "Model 4",
  #                                                                                                               "Model 5")))
  # 
  # merge plots with models heading 
  
  print(plot_M_uni_int)
  # 
  #  jpeg('plot_M_uni_int.jpg')
  #  plot(plot_M_uni_int)
  #  dev.off()
  
  # save_plot(filename =  "plot_M_uni_int.tif",
  #                       fig = last_plot())
  
  
  print(plot_M_1_int)
  # 
  # save_plot(filename =  paste(folder, "plot_M_1_int.tif", sep = ""),
  #           fig = last_plot())
  
  print(plot_M_2_int)
  print(plot_M_2a_int)
  print(plot_M_2s_int)
  print(plot_M_2pa_int)
  print(plot_M_3_int)
  print(plot_M_4_int)
  print(plot_M_5_int)
  
  print(all_models)
  
  
  data_uni = cbind(plot_M_uni_int$data[1:3], plot_M_uni_int$data[5:6], plot_M_uni_int$data[9]) 
  
  ###########################
  
  
  
  data_1 = cbind(plot_M_1_int$data[1:3], plot_M_1_int$data[5:6], plot_M_1_int$data[9]) 
  
  ###########################
  
  
  data_2 = cbind(plot_M_2_int$data[1:3], plot_M_2_int$data[5:6], plot_M_2_int$data[9]) 
  
  ###########################
  
  
  data_2a = cbind(plot_M_2a_int$data[1:3], plot_M_2a_int$data[5:6], plot_M_2a_int$data[9]) 
  
  ###########################
  
  
  data_2s = cbind(plot_M_2s_int$data[1:3], plot_M_2s_int$data[5:6], plot_M_2s_int$data[9]) 
  
  ###########################
  
  
  data_2pa = cbind(plot_M_2pa_int$data[1:3], plot_M_2pa_int$data[5:6], plot_M_2pa_int$data[9]) 
  
  ###########################
  
  
  data_3 = cbind(plot_M_3_int$data[1:3], plot_M_3_int$data[5:6], plot_M_3_int$data[9]) 
  
  
  ###########################
  
  data_4 = cbind(plot_M_4_int$data[1:3], plot_M_4_int$data[5:6], plot_M_4_int$data[9]) 
  
  
  ################
  
  ###########################
  
  data_5 = cbind(plot_M_5_int$data[1:3], plot_M_5_int$data[5:6], plot_M_5_int$data[9]) 
  
  
  Model = c("Univariate",
            "Univariate",
            "Univariate",
            "Model_1",
            "Model_1",
            "Model_1",
            "Model_2",
            "Model_2",
            "Model_2",
            "Model_2a",
            "Model_2a",
            "Model_2a",
            "Model_2s",
            "Model_2s",
            "Model_2s",
            "Model_2pa",
            "Model_2pa",
            "Model_2pa",
            "Model_3",
            "Model_3",
            "Model_3",
            "Model_4",
            "Model_4",
            "Model_4",
            "Model_5",
            "Model_5",
            "Model_5")
  
  
  print(plot_M_uni_int)
  print(plot_M_1_int)
  print(plot_M_2_int)
  print(plot_M_2a_int)
  print(plot_M_2s_int)
  print(plot_M_2pa_int)
  print(plot_M_3_int)
  print(plot_M_4_int)
  print(plot_M_5_int)
  
  
  
  #output_results = table
  
  
  output_results = rbind(data_uni, 
                         data_1,
                         data_2, 
                         data_2a, 
                         data_2s, 
                         data_2pa,
                         data_3, 
                         data_4, 
                         data_5)
  
  
  output_results = data.frame(Model, 
                              output_results)
  
  
  
  
  output_results$Estimate_rounded = round(output_results$estimate, 4)
  output_results$SE_rounded = round(output_results$std.error, 4)
  
  output_results$CI95_edited = paste("[", round(output_results$conf.low, 4), ";", round(output_results$conf.high, 4), "]", sep = "")
  output_results$p_value_rounded = round(output_results$p.value, 4)
  
  output_results_table_edited  = data.frame(output_results$Model, 
                                            output_results$term,
                                            output_results$Estimate_rounded,
                                            output_results$SE_rounded,
                                            output_results$CI95_edited,
                                            output_results$p_value_rounded)
  
  #write.csv(output_results_table_edited, file = paste(folder, analysis_name, ".csv", sep = ""))
  
  
  #return(params = output_results)
  
#}
#plots: https://strengejacke.wordpress.com/2017/10/23/one-function-to-rule-them-all-visualization-of-regression-models-in-rstats-w-sjplot/

