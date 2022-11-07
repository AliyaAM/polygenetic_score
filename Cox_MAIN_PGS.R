

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



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "PGS_glm_function_ELSA.R", sep=""))
source(paste(SOURCE_ROOT, "cox_model_PGS.R", sep=""))

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


######### HRS 
#all_HRS_by_years_PGS$HRS2010_discrim_bin 

discrimination_var_ELSA =  "w5discrim_bin2" 

discrimination_var_HRS = "HRS2010_discrim_bin" 

covariate1_ELSA_pca = "pc1"

covariate1_ELSA = "NA"
covariate2_ELSA = "NA"
covariate3_ELSA = "NA"
covariate4_ELSA = "NA"

covariate1_HRS_pca = "PC1_5A"

covariate1_HRS = "NA"
covariate2_HRS = "NA"
covariate3_HRS = "NA"
covariate4_HRS = "NA"


all_HRS_by_years_PGS$PGS_mi = all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15
ELSA_data_with_PGS$PGS_mi = ELSA_data_with_PGS$MI 

#gene_ELSA = "MI"
#gene_HRS = "E4_MI_CARDIOGRAM15"


ELSA_data_with_PGS$age = ELSA_data_with_PGS$w5age
ELSA_data_with_PGS$sex = ELSA_data_with_PGS$w5sex


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

all_HRS_by_years_PGS$age = all_HRS_by_years_PGS$HRS2010_continious_age
all_HRS_by_years_PGS$sex = all_HRS_by_years_PGS$HRS2010_sex_1_0


ELSA_data_with_PGS$idauniq = ELSA_data_with_PGS$idauniq...4

# reorganise the dataset so there is time variable and other columns include outcomes at all time points 
##### ELSA 
ID_ELSA = c(ELSA_data_with_PGS$idauniq, 
                             ELSA_data_with_PGS$idauniq, 
                             ELSA_data_with_PGS$idauniq, 
                             ELSA_data_with_PGS$idauniq)

data_cox_ELSA_initial = data.frame(ID_ELSA)

data_cox_ELSA_initial$MI_outcome = c(ELSA_data_with_PGS$w5_MI_new_bin,
                                     ELSA_data_with_PGS$w6_MI_new_bin,
                                     ELSA_data_with_PGS$w7_MI_new_bin,
                                     ELSA_data_with_PGS$w8_MI_new_bin)

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

data_cox_ELSA_initial$PGS = c(ELSA_data_with_PGS$MI, 
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


length(unique(data_cox_ELSA_initial$ID))
summary(data_cox_ELSA_initial)

data_cox_ELSA_table = data.table(data_cox_ELSA_initial)

data_cox_ELSA = na.omit(data_cox_ELSA_table, cols = c("MI_outcome", "PGS", "baseline_discriminaition"))
data_cox_ELSA = as.data.frame(data_cox_ELSA)
length(unique(data_cox_ELSA$ID))
summary(data_cox_ELSA)
data_cox_ELSA$MI_outcome

ELSA_results = cox_model_PGS(data_cox_input = data_cox_ELSA, outcome = "MI_outcome")


#######################################
#######################################

##### HRS 

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


data_cox_HRS_initial$PGS = c(all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15, 
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




length(unique(data_cox_HRS_initial$ID))
summary(data_cox_HRS_initial)

data_cox_HRS_table = data.table(data_cox_HRS_initial)

data_cox_HRS = na.omit(data_cox_HRS_table, cols = c("MI_outcome", "PGS", "baseline_discriminaition"))
data_cox_HRS = as.data.frame(data_cox_HRS)
length(unique(data_cox_HRS$ID))
summary(data_cox_HRS)
data_cox_HRS$MI_outcome


HRS_results = cox_model_PGS(data_cox_input = data_cox_HRS, outcome = "MI_outcome")
