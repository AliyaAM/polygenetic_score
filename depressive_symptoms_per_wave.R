

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

time_point_1_ELSA = rep(0, nrow(ELSA_data_with_PGS))
time_point_2_ELSA = rep(1, nrow(ELSA_data_with_PGS))
time_point_3_ELSA = rep(2, nrow(ELSA_data_with_PGS))
time_point_4_ELSA = rep(3, nrow(ELSA_data_with_PGS))

data_cox_ELSA_initial$time_point = c(time_point_1_ELSA, 
                                     time_point_2_ELSA,
                                     time_point_3_ELSA,
                                     time_point_4_ELSA)

data_cox_ELSA_initial$years = 2 * data_cox_ELSA_initial$time_point

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

print("  bmi was taken in w4, w6, w8, but not the other waves ")
data_cox_ELSA_initial$BMI = c(ELSA_data_with_PGS$w4_bmi, 
                              ELSA_data_with_PGS$w4_bmi, 
                              ELSA_data_with_PGS$w6_bmi, 
                              ELSA_data_with_PGS$w8_bmi)

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

####### stressful event: 
# w4_jobstress
# life_stress_event
# chldhd_stress_event
# w9_loneliness3
# w9_loneliness

#summary score

data_cox_ELSA_initial$loneliness = c(ELSA_data_with_PGS$w5_loneliness, 
                                     ELSA_data_with_PGS$w6_loneliness, 
                                     ELSA_data_with_PGS$w7_loneliness, 
                                     ELSA_data_with_PGS$w8_loneliness)


#summary score (three-item scale, shorter)

data_cox_ELSA_initial$loneliness3 = c(ELSA_data_with_PGS$w5_loneliness3, 
                                      ELSA_data_with_PGS$w6_loneliness3, 
                                      ELSA_data_with_PGS$w7_loneliness3, 
                                      ELSA_data_with_PGS$w8_loneliness3)

#summary score

data_cox_ELSA_initial$life_stress_event = c(ELSA_data_with_PGS$life_stress_event, 
                                            ELSA_data_with_PGS$life_stress_event, 
                                            ELSA_data_with_PGS$life_stress_event, 
                                            ELSA_data_with_PGS$life_stress_event)


unique(data_cox_ELSA_initial$life_stress_event)


#summary score
data_cox_ELSA_initial$chldhd_stress_event = c(ELSA_data_with_PGS$chldhd_stress_event, 
                                              ELSA_data_with_PGS$chldhd_stress_event, 
                                              ELSA_data_with_PGS$chldhd_stress_event, 
                                              ELSA_data_with_PGS$chldhd_stress_event)

unique(data_cox_ELSA_initial$chldhd_stress_event)


#summary score

data_cox_ELSA_initial$jobstress = c(ELSA_data_with_PGS$w5_jobstress, 
                                    ELSA_data_with_PGS$w6_jobstress, 
                                    ELSA_data_with_PGS$w7_jobstress, 
                                    ELSA_data_with_PGS$w8_jobstress)



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

time_point_1_HRS = rep(0, nrow(all_HRS_by_years_PGS))
time_point_2_HRS = rep(1, nrow(all_HRS_by_years_PGS))
time_point_3_HRS = rep(2, nrow(all_HRS_by_years_PGS))
time_point_4_HRS = rep(3, nrow(all_HRS_by_years_PGS))

data_cox_HRS_initial$time_point = c(time_point_1_HRS, 
                                    time_point_2_HRS,
                                    time_point_3_HRS,
                                    time_point_4_HRS)

data_cox_HRS_initial$years = 2 * data_cox_HRS_initial$time_point

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



# depressive symptoms: 

data_cox_HRS_initial$depressive_symptoms = c(all_HRS_by_years_PGS$HRS2010_cesd, 
                                             all_HRS_by_years_PGS$HRS2012_cesd, 
                                             all_HRS_by_years_PGS$HRS2014_cesd,
                                             all_HRS_by_years_PGS$HRS2016_cesd)



data_cox_HRS_initial$loneliness = c(all_HRS_by_years_PGS$HRS2010_loneliness, 
                                    all_HRS_by_years_PGS$HRS2012_loneliness, 
                                    all_HRS_by_years_PGS$HRS2014_loneliness, 
                                    all_HRS_by_years_PGS$HRS2016_loneliness)


#summary score

data_cox_HRS_initial$life_stress_event = c(all_HRS_by_years_PGS$life_stress_event, 
                                           all_HRS_by_years_PGS$life_stress_event, 
                                           all_HRS_by_years_PGS$life_stress_event, 
                                           all_HRS_by_years_PGS$life_stress_event)


unique(data_cox_HRS_initial$life_stress_event)


#summary score
data_cox_HRS_initial$chldhd_stress_event = c(all_HRS_by_years_PGS$chldhd_stress_event, 
                                             all_HRS_by_years_PGS$chldhd_stress_event, 
                                             all_HRS_by_years_PGS$chldhd_stress_event, 
                                             all_HRS_by_years_PGS$chldhd_stress_event)

unique(data_cox_HRS_initial$chldhd_stress_event)


#summary score: no data for 2014, 2016 and 2018

data_cox_HRS_initial$jobstress = c(all_HRS_by_years_PGS$HRS2008_jobstress, 
                                   all_HRS_by_years_PGS$HRS2010_jobstress, 
                                   all_HRS_by_years_PGS$HRS2012_jobstress, 
                                   all_HRS_by_years_PGS$HRS2012_jobstress)



length(unique(data_cox_HRS_initial$ID_HRS))
summary(data_cox_HRS_initial)

########################################## ANALYSIS 
##########################################
########################################## ANALYSIS 
##########################################


#omiting NA in the outcome, discrim, and relevant PGS 

data_cox_ELSA_table = data.table(data_cox_ELSA_initial)
data_cox_ELSA = na.omit(data_cox_ELSA_table, cols = c("baseline_discriminaition",
                                                      "MI_outcome",
                                                      "PGS_mi"))
data_cox_ELSA = as.data.frame(data_cox_ELSA)
length(unique(data_cox_ELSA$ID))
summary(data_cox_ELSA)
data_cox_ELSA$MI_outcome

#### ELSA, MI, discrim_bin Cox model 
ELSA_results_mi = cox_model_PGS(data_cox_input = data_cox_ELSA,
                                baseline_discriminaition = "baseline_discriminaition", 
                                outcome = "MI_outcome",  
                                PGS = "PGS_mi",
                                OUTPUT_ROOT = OUTPUT_ROOT, 
                                alternative_cov = "diabetes_history", 
                                analysis_name = "ELSA_results_mi_cox", 
                                plot_name = "ELSA, MI, Cox regression: ")



ELSA_results_mi$Estimate_rounded = round(ELSA_results_mi$estimate, 2)
ELSA_results_mi$SE_rounded = round(ELSA_results_mi$std.error, 2)

ELSA_results_mi$CI95_edited = paste("[", round(ELSA_results_mi$conf.low), ";", round(ELSA_results_mi$conf.high, 2), "]", sep = "")
ELSA_results_mi$p_value_rounded = round(ELSA_results_mi$p.value, 4)

ELSA_results_mi_table_edited  = data.frame(ELSA_results_mi$Model, 
                                           ELSA_results_mi$term, 
                                           ELSA_results_mi$Estimate_rounded,
                                           ELSA_results_mi$SE_rounded,
                                           ELSA_results_mi$CI95_edited,
                                           ELSA_results_mi$p_value_rounded)


#### ELSA, MI, discrim_bin glm model 

ELSA_results_mi_glm = glm_model_PGS(data_glm_input = data_cox_ELSA, 
                                    baseline_discriminaition = "baseline_discriminaition", 
                                    outcome = "MI_outcome",  
                                    PGS = "PGS_mi",
                                    OUTPUT_ROOT = OUTPUT_ROOT, 
                                    alternative_cov = "diabetes_history", 
                                    analysis_name = "ELSA_results_mi_glm", 
                                    plot_name = "ELSA, MI, GLM: ")


ELSA_results_mi_glm$Estimate_rounded = round(ELSA_results_mi_glm$estimate, 4)
ELSA_results_mi_glm$SE_rounded = round(ELSA_results_mi_glm$std.error, 4)

ELSA_results_mi_glm$CI95_edited = paste("[", round(ELSA_results_mi_glm$conf.low, 4), ";", round(ELSA_results_mi_glm$conf.high, 4), "]", sep = "")
ELSA_results_mi_glm$p_value_rounded = round(ELSA_results_mi_glm$p.value, 4)

ELSA_results_mi_glm_table_edited  = data.frame(ELSA_results_mi_glm$Model, 
                                               ELSA_results_mi_glm$term, 
                                               ELSA_results_mi_glm$Estimate_rounded,
                                               ELSA_results_mi_glm$SE_rounded,
                                               ELSA_results_mi_glm$CI95_edited,
                                               ELSA_results_mi_glm$p_value_rounded)



#### ELSA, MI, discrim_bin glm model no baseline 

data_glm_ELSA_baseline_cases_mi = subset(data_cox_ELSA, data_cox_ELSA$time_point == 0 & data_cox_ELSA$MI_outcome == 1) 
baseline_cases_mi_uniqueIDs = unique(data_glm_ELSA_baseline_cases_mi$ID)

data_glm_ELSA_NObaseline_cases_mi <- data_cox_ELSA[ !(data_cox_ELSA$ID %in% c(baseline_cases_mi_uniqueIDs)), ]


ELSA_results_mi_glm_nobaseline = glm_model_PGS(data_glm_input = data_glm_ELSA_NObaseline_cases_mi, 
                                               baseline_discriminaition = "baseline_discriminaition", 
                                               outcome = "MI_outcome",  
                                               PGS = "PGS_mi",
                                               OUTPUT_ROOT = OUTPUT_ROOT, 
                                               alternative_cov = "diabetes_history", 
                                               analysis_name = "ELSA_results_mi_glm_no_baseline_cases", 
                                               plot_name = "ELSA, MI, GLM (baseline cases excluded): ")


ELSA_results_mi_glm_nobaseline$Estimate_rounded = round(ELSA_results_mi_glm_nobaseline$estimate, 4)
ELSA_results_mi_glm_nobaseline$SE_rounded = round(ELSA_results_mi_glm_nobaseline$std.error, 4)

ELSA_results_mi_glm_nobaseline$CI95_edited = paste("[", round(ELSA_results_mi_glm_nobaseline$conf.low, 4), ";", round(ELSA_results_mi_glm_nobaseline$conf.high, 4), "]", sep = "")
ELSA_results_mi_glm_nobaseline$p_value_rounded = round(ELSA_results_mi_glm_nobaseline$p.value, 4)

ELSA_results_mi_glm_nobaseline_table_edited  = data.frame(ELSA_results_mi_glm_nobaseline$Model, 
                                                          ELSA_results_mi_glm_nobaseline$term, 
                                                          ELSA_results_mi_glm_nobaseline$Estimate_rounded,
                                                          ELSA_results_mi_glm_nobaseline$SE_rounded,
                                                          ELSA_results_mi_glm_nobaseline$CI95_edited,
                                                          ELSA_results_mi_glm_nobaseline$p_value_rounded)




#######################################

#### HRS, MI, discrim_bin
#omiting NA in the outcome, discrim, and relevant PGS 

data_cox_HRS_table = data.table(data_cox_HRS_initial)

data_cox_HRS = na.omit(data_cox_HRS_table, cols = c("baseline_discriminaition",
                                                    "MI_outcome",
                                                    "PGS_mi"))
data_cox_HRS = as.data.frame(data_cox_HRS)
length(unique(data_cox_HRS$ID))
summary(data_cox_HRS)
data_cox_HRS$MI_outcome

#### HRS, MI, discrim_bin, Cox model 
HRS_results_mi = cox_model_PGS(data_cox_input = data_cox_HRS, 
                               baseline_discriminaition = "baseline_discriminaition", 
                               outcome = "MI_outcome",  
                               PGS = "PGS_mi",
                               OUTPUT_ROOT = OUTPUT_ROOT, 
                               alternative_cov = "diabetes_history", 
                               analysis_name = "HRS_results_mi_cox", 
                               plot_name = "HRS, MI, Cox regression: ")


HRS_results_mi$Estimate_rounded = round(HRS_results_mi$estimate, 2)
HRS_results_mi$SE_rounded = round(HRS_results_mi$std.error, 2)

HRS_results_mi$CI95_edited = paste("[", round(HRS_results_mi$conf.low), ";", round(HRS_results_mi$conf.high, 2), "]", sep = "")
HRS_results_mi$p_value_rounded = round(HRS_results_mi$p.value, 4)

HRS_results_mi_table_edited  = data.frame(HRS_results_mi$Model, 
                                          HRS_results_mi$term, 
                                          HRS_results_mi$Estimate_rounded,
                                          HRS_results_mi$SE_rounded,
                                          HRS_results_mi$CI95_edited,
                                          HRS_results_mi$p_value_rounded)


#### HRS, MI, discrim_bin, glm model 

HRS_results_mi_glm = glm_model_PGS(data_glm_input = data_cox_HRS, 
                                   baseline_discriminaition = "baseline_discriminaition", 
                                   outcome = "MI_outcome",  
                                   PGS = "PGS_mi",
                                   OUTPUT_ROOT = OUTPUT_ROOT, 
                                   alternative_cov = "diabetes_history", 
                                   analysis_name = "HRS_results_mi_glm", 
                                   plot_name = "HRS, MI, GLM: ")


HRS_results_mi_glm$Estimate_rounded = round(HRS_results_mi_glm$estimate, 4)
HRS_results_mi_glm$SE_rounded = round(HRS_results_mi_glm$std.error, 4)

HRS_results_mi_glm$CI95_edited = paste("[", round(HRS_results_mi_glm$conf.low, 4), ";", round(HRS_results_mi_glm$conf.high, 4), "]", sep = "")
HRS_results_mi_glm$p_value_rounded = round(HRS_results_mi_glm$p.value, 4)

HRS_results_mi_glm_table_edited  = data.frame(HRS_results_mi_glm$Model, 
                                              HRS_results_mi_glm$term, 
                                              HRS_results_mi_glm$Estimate_rounded,
                                              HRS_results_mi_glm$SE_rounded,
                                              HRS_results_mi_glm$CI95_edited,
                                              HRS_results_mi_glm$p_value_rounded)

#### HRS, MI, discrim_bin, glm model no baseline 

data_glm_HRS_baseline_cases_mi = subset(data_cox_HRS, data_cox_HRS$time_point == 0 & data_cox_HRS$MI_outcome == 1) 
baseline_cases_mi_uniqueIDs = unique(data_glm_HRS_baseline_cases_mi$ID)

data_glm_HRS_NObaseline_cases_mi <- data_cox_HRS[ !(data_cox_HRS$ID %in% c(baseline_cases_mi_uniqueIDs)), ]


HRS_results_mi_glm_nobaseline = glm_model_PGS(data_glm_input = data_glm_HRS_NObaseline_cases_mi, 
                                              baseline_discriminaition = "baseline_discriminaition", 
                                              outcome = "MI_outcome",  
                                              PGS = "PGS_mi",
                                              OUTPUT_ROOT = OUTPUT_ROOT, 
                                              alternative_cov = "diabetes_history", 
                                              analysis_name = "HRS_results_mi_glm_no_baseline", 
                                              plot_name = "HRS, MI, GLM (no baseline): ")


HRS_results_mi_glm_nobaseline$Estimate_rounded = round(HRS_results_mi_glm_nobaseline$estimate, 4)
HRS_results_mi_glm_nobaseline$SE_rounded = round(HRS_results_mi_glm_nobaseline$std.error, 4)

HRS_results_mi_glm_nobaseline$CI95_edited = paste("[", round(HRS_results_mi_glm_nobaseline$conf.low, 4), ";", round(HRS_results_mi_glm_nobaseline$conf.high, 4), "]", sep = "")
HRS_results_mi_glm_nobaseline$p_value_rounded = round(HRS_results_mi_glm_nobaseline$p.value, 4)

HRS_results_mi_glm_nobaseline_table_edited  = data.frame(HRS_results_mi_glm_nobaseline$Model, 
                                                         HRS_results_mi_glm_nobaseline$term, 
                                                         HRS_results_mi_glm_nobaseline$Estimate_rounded,
                                                         HRS_results_mi_glm_nobaseline$SE_rounded,
                                                         HRS_results_mi_glm_nobaseline$CI95_edited,
                                                         HRS_results_mi_glm_nobaseline$p_value_rounded)



#########################################  diabetes 

#omiting NA in the outcome, discrim, and relevant PGS 

data_cox_ELSA_table = data.table(data_cox_ELSA_initial)
data_cox_ELSA = na.omit(data_cox_ELSA_table, cols = c("baseline_discriminaition",
                                                      "diabetes_outcome",
                                                      "PGS_diab"))
data_cox_ELSA = as.data.frame(data_cox_ELSA)
length(unique(data_cox_ELSA$ID))
summary(data_cox_ELSA)
data_cox_ELSA$MI_outcome

#### ELSA, diabetes, discrim_bin, cox model  

ELSA_results_diabetes_cox = cox_model_PGS(data_cox_input = data_cox_ELSA,
                                          baseline_discriminaition = "baseline_discriminaition", 
                                          outcome = "diabetes_outcome",  
                                          PGS = "PGS_diab",
                                          OUTPUT_ROOT = OUTPUT_ROOT, 
                                          alternative_cov = "BMI", 
                                          analysis_name = "ELSA_results_diab_cox", 
                                          plot_name = "ELSA, T2DM, Cox regression: ")


ELSA_results_diabetes_cox$Estimate_rounded = round(ELSA_results_diabetes_cox$estimate, 2)
ELSA_results_diabetes_cox$SE_rounded = round(ELSA_results_diabetes_cox$std.error, 2)

ELSA_results_diabetes_cox$CI95_edited = paste("[", round(ELSA_results_diabetes_cox$conf.low, 2), ";", round(ELSA_results_diabetes_cox$conf.high, 2), "]", sep = "")
ELSA_results_diabetes_cox$p_value_rounded = round(ELSA_results_diabetes_cox$p.value, 2)

ELSA_results_diabetes_cox_table_edited  = data.frame(ELSA_results_diabetes_cox$Model, 
                                                     ELSA_results_diabetes_cox$term, 
                                                     ELSA_results_diabetes_cox$Estimate_rounded,
                                                     ELSA_results_diabetes_cox$SE_rounded,
                                                     ELSA_results_diabetes_cox$CI95_edited,
                                                     ELSA_results_diabetes_cox$p_value_rounded)

#### ELSA, diabetes, discrim_bin, glm model ADD 

ELSA_results_diabetes_glm = glm_model_PGS(data_glm_input = data_cox_ELSA, 
                                          baseline_discriminaition = "baseline_discriminaition", 
                                          outcome = "diabetes_outcome",  
                                          PGS = "PGS_diab",
                                          OUTPUT_ROOT = OUTPUT_ROOT, 
                                          alternative_cov = "BMI", 
                                          
                                          analysis_name = "ELSA_results_diab_glm", 
                                          plot_name = "ELSA, T2DM, GLM: ")



ELSA_results_diabetes_glm$Estimate_rounded = round(ELSA_results_diabetes_glm$estimate, 4)
ELSA_results_diabetes_glm$SE_rounded = round(ELSA_results_diabetes_glm$std.error, 4)

ELSA_results_diabetes_glm$CI95_edited = paste("[", round(ELSA_results_diabetes_glm$conf.low, 4), ";", round(ELSA_results_diabetes_glm$conf.high, 4), "]", sep = "")
ELSA_results_diabetes_glm$p_value_rounded = round(ELSA_results_diabetes_glm$p.value, 4)

ELSA_results_diabetes_glm_table_edited  = data.frame(ELSA_results_diabetes_glm$Model, 
                                                     ELSA_results_diabetes_glm$term, 
                                                     ELSA_results_diabetes_glm$Estimate_rounded,
                                                     ELSA_results_diabetes_glm$SE_rounded,
                                                     ELSA_results_diabetes_glm$CI95_edited,
                                                     ELSA_results_diabetes_glm$p_value_rounded)


#### ELSA, diabetes, discrim_bin, glm model, no baseline 

data_glm_ELSA_baseline_cases = subset(data_cox_ELSA, data_cox_ELSA$time_point == 0 & data_cox_ELSA$diabetes_outcome == 1) 
baseline_cases_uniqueIDs = unique(data_glm_ELSA_baseline_cases$ID)

data_glm_ELSA_NObaseline_cases <- data_cox_ELSA[ !(data_cox_ELSA$ID %in% c(baseline_cases_uniqueIDs)), ]

ELSA_results_diabetes_glm_nobaseline = glm_model_PGS(data_glm_input = data_glm_ELSA_NObaseline_cases, 
                                                     baseline_discriminaition = "baseline_discriminaition", 
                                                     outcome = "diabetes_outcome",  
                                                     PGS = "PGS_diab",
                                                     OUTPUT_ROOT = OUTPUT_ROOT, 
                                                     alternative_cov = "BMI", 
                                                     
                                                     analysis_name = "ELSA_results_diab_glm_no_baseline", 
                                                     plot_name = "ELSA, T2DM, GLM (no baseline): ")


ELSA_results_diabetes_glm_nobaseline$Estimate_rounded = round(ELSA_results_diabetes_glm_nobaseline$estimate, 4)
ELSA_results_diabetes_glm_nobaseline$SE_rounded = round(ELSA_results_diabetes_glm_nobaseline$std.error, 4)

ELSA_results_diabetes_glm_nobaseline$CI95_edited = paste("[", round(ELSA_results_diabetes_glm_nobaseline$conf.low, 4), ";", round(ELSA_results_diabetes_glm_nobaseline$conf.high, 4), "]", sep = "")
ELSA_results_diabetes_glm_nobaseline$p_value_rounded = round(ELSA_results_diabetes_glm_nobaseline$p.value, 4)

ELSA_results_diabetes_glm_nobaseline_table_edited  = data.frame(ELSA_results_diabetes_glm_nobaseline$Model, 
                                                                ELSA_results_diabetes_glm_nobaseline$term, 
                                                                ELSA_results_diabetes_glm_nobaseline$Estimate_rounded,
                                                                ELSA_results_diabetes_glm_nobaseline$SE_rounded,
                                                                ELSA_results_diabetes_glm_nobaseline$CI95_edited,
                                                                ELSA_results_diabetes_glm_nobaseline$p_value_rounded)

#######################################

#omiting NA in the outcome, discrim, and relevant PGS 

#ID_diabebtes_free = 
#HRS2010_diabetes_new
#HRS2010_diabetes_ever


data_cox_HRS_table = data.table(data_cox_HRS_initial)

data_cox_HRS = na.omit(data_cox_HRS_table, cols = c("baseline_discriminaition",
                                                    "diabetes_outcome",
                                                    "PGS_diab"))
data_cox_HRS = as.data.frame(data_cox_HRS)
length(unique(data_cox_HRS$ID))
summary(data_cox_HRS)
data_cox_HRS$MI_outcome

##### HRS, diabetes, discrim_bin Cox model

HRS_results_diabetes = cox_model_PGS(data_cox_input = data_cox_HRS, 
                                     baseline_discriminaition = "baseline_discriminaition", 
                                     outcome = "diabetes_outcome",  
                                     PGS = "PGS_diab",
                                     OUTPUT_ROOT = OUTPUT_ROOT, 
                                     alternative_cov = "BMI", 
                                     
                                     analysis_name = "HRS_results_diab_cox", 
                                     plot_name = "HRS, T2DM, Cox regression: ")



HRS_results_diabetes$Estimate_rounded = round(HRS_results_diabetes$estimate, 2)
HRS_results_diabetes$SE_rounded = round(HRS_results_diabetes$std.error, 2)

HRS_results_diabetes$CI95_edited = paste("[", round(HRS_results_diabetes$conf.low, 2), ";", round(HRS_results_diabetes$conf.high, 2), "]", sep = "")
HRS_results_diabetes$p_value_rounded = round(HRS_results_diabetes$p.value, 4)

HRS_results_diabetes_table_edited  = data.frame(HRS_results_diabetes$Model, 
                                                HRS_results_diabetes$term, 
                                                HRS_results_diabetes$Estimate_rounded,
                                                HRS_results_diabetes$SE_rounded,
                                                HRS_results_diabetes$CI95_edited,
                                                HRS_results_diabetes$p_value_rounded)


##### HRS, diabetes, discrim_bin  glm model 

HRS_results_diabetes_glm = glm_model_PGS(data_glm_input = data_cox_HRS, 
                                         baseline_discriminaition = "baseline_discriminaition", 
                                         outcome = "diabetes_outcome",  
                                         PGS = "PGS_diab",
                                         OUTPUT_ROOT = OUTPUT_ROOT, 
                                         alternative_cov = "BMI", 
                                         
                                         analysis_name = "HRS_results_diab_glm", 
                                         plot_name = "HRS, T2DM, GLM: ")



HRS_results_diabetes_glm$Estimate_rounded = round(HRS_results_diabetes_glm$estimate, 4)
HRS_results_diabetes_glm$SE_rounded = round(HRS_results_diabetes_glm$std.error, 4)

HRS_results_diabetes_glm$CI95_edited = paste("[", round(HRS_results_diabetes_glm$conf.low, 4), ";", round(HRS_results_diabetes_glm$conf.high, 4), "]", sep = "")
HRS_results_diabetes_glm$p_value_rounded = round(HRS_results_diabetes_glm$p.value, 4)

HRS_results_diabetes_glm_table_edited  = data.frame(HRS_results_diabetes_glm$Model, 
                                                    HRS_results_diabetes_glm$term, 
                                                    HRS_results_diabetes_glm$Estimate_rounded,
                                                    HRS_results_diabetes_glm$SE_rounded,
                                                    HRS_results_diabetes_glm$CI95_edited,
                                                    HRS_results_diabetes_glm$p_value_rounded)


##### HRS, diabetes, discrim_bin  glm model no baseline 

data_glm_HRS_baseline_cases = subset(data_cox_HRS, data_cox_HRS$time_point == 0 & data_cox_HRS$diabetes_outcome == 1) 
baseline_cases_uniqueIDs = unique(data_glm_HRS_baseline_cases$ID)

data_glm_HRS_NObaseline_cases <- data_cox_HRS[ !(data_cox_HRS$ID %in% c(baseline_cases_uniqueIDs)), ]

HRS_results_diabetes_glm_nobaseline = glm_model_PGS(data_glm_input = data_glm_HRS_NObaseline_cases, 
                                                    baseline_discriminaition = "baseline_discriminaition", 
                                                    outcome = "diabetes_outcome",  
                                                    PGS = "PGS_diab",
                                                    OUTPUT_ROOT = OUTPUT_ROOT, 
                                                    alternative_cov = "BMI", 
                                                    
                                                    analysis_name = "HRS_results_diab_glm_no_baseline", 
                                                    plot_name = "HRS, T2DM, GLM (no baseline): ")



HRS_results_diabetes_glm_nobaseline$Estimate_rounded = round(HRS_results_diabetes_glm_nobaseline$estimate, 4)
HRS_results_diabetes_glm_nobaseline$SE_rounded = round(HRS_results_diabetes_glm_nobaseline$std.error, 4)

HRS_results_diabetes_glm_nobaseline$CI95_edited = paste("[", round(HRS_results_diabetes_glm_nobaseline$conf.low, 4), ";", round(HRS_results_diabetes_glm_nobaseline$conf.high, 4), "]", sep = "")
HRS_results_diabetes_glm_nobaseline$p_value_rounded = round(HRS_results_diabetes_glm_nobaseline$p.value, 4)

HRS_results_diabetes_glm_nobaseline_table_edited  = data.frame(HRS_results_diabetes_glm_nobaseline$Model, 
                                                               HRS_results_diabetes_glm_nobaseline$term, 
                                                               HRS_results_diabetes_glm_nobaseline$Estimate_rounded,
                                                               HRS_results_diabetes_glm_nobaseline$SE_rounded,
                                                               HRS_results_diabetes_glm_nobaseline$CI95_edited,
                                                               HRS_results_diabetes_glm_nobaseline$p_value_rounded)

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to=OUTPUT_ROOT)

#################################### Depressive symptoms ############################################

#PGS_depres_symp
#depressive_symptoms

data_cox_ELSA_initial = data.table(data_cox_ELSA_initial)

data_glm_gaussian_ELSA = na.omit(data_cox_ELSA_initial, cols = c("baseline_discriminaition",
                                                                 "depressive_symptoms",
                                                                 "PGS_depres_symp"))
data_glm_gaussian_ELSA = as.data.frame(data_glm_gaussian_ELSA)
length(unique(data_glm_gaussian_ELSA$ID))
summary(data_glm_gaussian_ELSA)
data_glm_gaussian_ELSA$MI_outcome


##### ELSA, depressive_symp, discrim_bin  glm model 

ELSA_results_depressive_symp_glm = glm_gaussian_PGS(data_glm_input = data_glm_gaussian_ELSA, 
                                                    baseline_discriminaition = "baseline_discriminaition", 
                                                    outcome = "depressive_symptoms",  
                                                    PGS = "PGS_depres_symp",
                                                    OUTPUT_ROOT = OUTPUT_ROOT, 
                                                    alternative_cov = "life_stress_event", 
                                                    
                                                    analysis_name = "ELSA_results_depressive_glm", 
                                                    plot_name = "ELSA, T2DM, GLM: ")



ELSA_results_depressive_symp_glm$Estimate_rounded = round(ELSA_results_depressive_symp_glm$estimate, 4)
ELSA_results_depressive_symp_glm$SE_rounded = round(ELSA_results_depressive_symp_glm$std.error, 4)

ELSA_results_depressive_symp_glm$CI95_edited = paste("[", round(ELSA_results_depressive_symp_glm$conf.low, 4), ";", round(ELSA_results_depressive_symp_glm$conf.high, 4), "]", sep = "")
ELSA_results_depressive_symp_glm$p_value_rounded = round(ELSA_results_depressive_symp_glm$p.value, 4)

ELSA_results_depressive_symp_glm_table_edited  = data.frame(ELSA_results_depressive_symp_glm$Model, 
                                                            ELSA_results_depressive_symp_glm$term, 
                                                            ELSA_results_depressive_symp_glm$Estimate_rounded,
                                                            ELSA_results_depressive_symp_glm$SE_rounded,
                                                            ELSA_results_depressive_symp_glm$CI95_edited,
                                                            ELSA_results_depressive_symp_glm$p_value_rounded)


##### ELSA, depressive_symp, discrim_bin  glm model no baseline (depressive symptoms are gaussian not binary)
# 
# data_glm_ELSA_baseline_cases = subset(data_glm_gaussian_ELSA, data_glm_gaussian_ELSA$time_point == 0 & data_glm_gaussian_ELSA$depressive_symptoms == 1) 
# baseline_cases_uniqueIDs = unique(data_glm_ELSA_baseline_cases$ID)
# 
# data_glm_ELSA_NObaseline_cases <- data_glm_gaussian_ELSA[ !(data_glm_gaussian_ELSA$ID %in% c(baseline_cases_uniqueIDs)), ]
# 
# ELSA_results_diabetes_glm_nobaseline = glm_gaussian_PGS(data_glm_input = data_glm_ELSA_NObaseline_cases, 
#                                                         baseline_discriminaition = "baseline_discriminaition", 
#                                                         outcome = "depressive_symptoms",  
#                                                         PGS = "PGS_depres_symp",
#                                                         OUTPUT_ROOT = OUTPUT_ROOT, 
#                                                         alternative_cov = "life_stress_event", 
#                                                         
#                                                         analysis_name = "ELSA_results_depressive_glm_no_baseline", 
#                                                         plot_name = "ELSA, T2DM, GLM (no baseline): ")
# 
# 
# 
# ELSA_results_depressive_symp_glm_nobaseline$Estimate_rounded = round(ELSA_results_depressive_symp_glm_nobaseline$estimate, 4)
# ELSA_results_depressive_symp_glm_nobaseline$SE_rounded = round(ELSA_results_depressive_symp_glm_nobaseline$std.error, 4)
# 
# ELSA_results_depressive_symp_glm_nobaseline$CI95_edited = paste("[", round(ELSA_results_depressive_symp_glm_nobaseline$conf.low, 4), ";", round(ELSA_results_depressive_symp_glm_nobaseline$conf.high, 4), "]", sep = "")
# ELSA_results_depressive_symp_glm_nobaseline$p_value_rounded = round(ELSA_results_depressive_symp_glm_nobaseline$p.value, 4)
# 
# ELSA_results_depressive_symp_glm_nobaseline_table_edited  = data.frame(ELSA_results_depressive_symp_glm_nobaseline$Model, 
#                                                                        ELSA_results_depressive_symp_glm_nobaseline$term, 
#                                                                        ELSA_results_depressive_symp_glm_nobaseline$Estimate_rounded,
#                                                                        ELSA_results_depressive_symp_glm_nobaseline$SE_rounded,
#                                                                        ELSA_results_depressive_symp_glm_nobaseline$CI95_edited,
#                                                                        ELSA_results_depressive_symp_glm_nobaseline$p_value_rounded)
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to=OUTPUT_ROOT)


############# ############# #############  
############# ############# ############# #############  HRS 
############# ############# #############

data_cox_HRS_initial = data.table(data_cox_HRS_initial)

data_glm_gaussian_HRS = na.omit(data_cox_HRS_initial, cols = c("baseline_discriminaition",
                                                               "depressive_symptoms",
                                                               "PGS_depres_symp"))
data_glm_gaussian_HRS = as.data.frame(data_glm_gaussian_HRS)
length(unique(data_glm_gaussian_HRS$ID))
summary(data_glm_gaussian_HRS)
data_glm_gaussian_HRS$MI_outcome


##### HRS, depressive_symp, discrim_bin  glm model 

HRS_results_depressive_symp_glm = glm_gaussian_PGS(data_glm_input = data_glm_gaussian_HRS, 
                                                   baseline_discriminaition = "baseline_discriminaition", 
                                                   outcome = "depressive_symptoms",  
                                                   PGS = "PGS_depres_symp",
                                                   OUTPUT_ROOT = OUTPUT_ROOT, 
                                                   alternative_cov = "life_stress_event", 
                                                   
                                                   analysis_name = "HRS_results_depressive_glm", 
                                                   plot_name = "HRS, T2DM, GLM: ")



HRS_results_depressive_symp_glm$Estimate_rounded = round(HRS_results_depressive_symp_glm$estimate, 4)
HRS_results_depressive_symp_glm$SE_rounded = round(HRS_results_depressive_symp_glm$std.error, 4)

HRS_results_depressive_symp_glm$CI95_edited = paste("[", round(HRS_results_depressive_symp_glm$conf.low, 4), ";", round(HRS_results_depressive_symp_glm$conf.high, 4), "]", sep = "")
HRS_results_depressive_symp_glm$p_value_rounded = round(HRS_results_depressive_symp_glm$p.value, 4)

HRS_results_depressive_symp_glm_table_edited  = data.frame(HRS_results_depressive_symp_glm$Model, 
                                                           HRS_results_depressive_symp_glm$term, 
                                                           HRS_results_depressive_symp_glm$Estimate_rounded,
                                                           HRS_results_depressive_symp_glm$SE_rounded,
                                                           HRS_results_depressive_symp_glm$CI95_edited,
                                                           HRS_results_depressive_symp_glm$p_value_rounded)


##### HRS, depressive_symp, discrim_bin  glm model no baseline 
# fix the baseline exclusion below: 
# data_glm_HRS_baseline_cases = subset(data_glm_gaussian_HRS, data_glm_gaussian_HRS$time_point == 0 & data_glm_gaussian_HRS$depressive_symptoms == 1) 
# baseline_cases_uniqueIDs = unique(data_glm_HRS_baseline_cases$ID)
# 
# data_glm_HRS_NObaseline_cases <- data_glm_gaussian_HRS[ !(data_glm_gaussian_HRS$ID %in% c(baseline_cases_uniqueIDs)), ]
# 
# HRS_results_diabetes_glm_nobaseline = glm_gaussian_PGS(data_glm_input = data_glm_HRS_NObaseline_cases, 
#                                                     baseline_discriminaition = "baseline_discriminaition", 
#                                                     outcome = "depressive_symptoms",  
#                                                     PGS = "PGS_depres_symp",
#                                                     OUTPUT_ROOT = OUTPUT_ROOT, 
#                                                     alternative_cov = "life_stress_event", 
#                                                     
#                                                     analysis_name = "HRS_results_depressive_glm_no_baseline", 
#                                                     plot_name = "HRS, T2DM, GLM (no baseline): ")
# 
# 
# 
# HRS_results_depressive_symp_glm_nobaseline$Estimate_rounded = round(HRS_results_depressive_symp_glm_nobaseline$estimate, 4)
# HRS_results_depressive_symp_glm_nobaseline$SE_rounded = round(HRS_results_depressive_symp_glm_nobaseline$std.error, 4)
# 
# HRS_results_depressive_symp_glm_nobaseline$CI95_edited = paste("[", round(HRS_results_depressive_symp_glm_nobaseline$conf.low, 4), ";", round(HRS_results_depressive_symp_glm_nobaseline$conf.high, 4), "]", sep = "")
# HRS_results_depressive_symp_glm_nobaseline$p_value_rounded = round(HRS_results_depressive_symp_glm_nobaseline$p.value, 4)
# 
# HRS_results_depressive_symp_glm_nobaseline_table_edited  = data.frame(HRS_results_depressive_symp_glm_nobaseline$Model, 
#                                                                HRS_results_depressive_symp_glm_nobaseline$term, 
#                                                                HRS_results_depressive_symp_glm_nobaseline$Estimate_rounded,
#                                                                HRS_results_depressive_symp_glm_nobaseline$SE_rounded,
#                                                                HRS_results_depressive_symp_glm_nobaseline$CI95_edited,
#                                                                HRS_results_depressive_symp_glm_nobaseline$p_value_rounded)



