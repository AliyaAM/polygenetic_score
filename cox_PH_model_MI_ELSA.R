

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

#source(paste(SOURCE_ROOT, "subsetting_function.R", sep=""))



DATA_ROOT = "KCL_postDoc/Data_analysis/"


# harmonised_data_all_waves = read.csv(paste(directory, DATA_ROOT, "H_HRS_c.csv", sep=""))

#test subsetting to LSA_data_with_PGS$w6ethnicity

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


all_HRS_by_years_PGS$PGS = all_HRS_by_years_PGS$E4_MI_CARDIOGRAM15
ELSA_data_with_PGS$PGS = ELSA_data_with_PGS$MI 

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
#data_cox_ELSA = data.frame(ELSA_data_with_PGS$age)

ID = c(ELSA_data_with_PGS$idauniq, 
       ELSA_data_with_PGS$idauniq, 
       ELSA_data_with_PGS$idauniq, 
       ELSA_data_with_PGS$idauniq)

MI_outcome = c(ELSA_data_with_PGS$w5_MI_new_bin,
               ELSA_data_with_PGS$w6_MI_new_bin,
               ELSA_data_with_PGS$w7_MI_new_bin,
               ELSA_data_with_PGS$w8_MI_new_bin)

unique(MI_outcome)
table(MI_outcome)

time_point_1 = rep(0, nrow(ELSA_data_with_PGS))
time_point_2 = rep(1, nrow(ELSA_data_with_PGS))
time_point_3 = rep(2, nrow(ELSA_data_with_PGS))
time_point_4 = rep(3, nrow(ELSA_data_with_PGS))

time_point = c(time_point_1, 
               time_point_2,
               time_point_3,
               time_point_4)

years = 2 * time_point

follow_up = years


age = c(ELSA_data_with_PGS$w5age,
        ELSA_data_with_PGS$w6age, 
        ELSA_data_with_PGS$w7age, 
        ELSA_data_with_PGS$w8age)


sex = c(ELSA_data_with_PGS$w5sex,
        ELSA_data_with_PGS$w6sex, 
        ELSA_data_with_PGS$w7sex, 
        ELSA_data_with_PGS$w8sex)

PGS = c(ELSA_data_with_PGS$MI, 
            ELSA_data_with_PGS$MI, 
            ELSA_data_with_PGS$MI, 
            ELSA_data_with_PGS$MI) 


wealth = c(ELSA_data_with_PGS$w5wealth, 
           ELSA_data_with_PGS$w6wealth, 
           ELSA_data_with_PGS$w7wealth, 
           ELSA_data_with_PGS$w7wealth)

baseline_discriminaition = c(ELSA_data_with_PGS$w5discrim_bin, 
                             ELSA_data_with_PGS$w5discrim_bin, 
                             ELSA_data_with_PGS$w5discrim_bin, 
                             ELSA_data_with_PGS$w5discrim_bin)


discriminaition =  c(ELSA_data_with_PGS$w5discrim_bin, 
                     ELSA_data_with_PGS$w6discrim_bin, 
                     ELSA_data_with_PGS$w7discrim_bin, 
                     ELSA_data_with_PGS$w8discrim_bin) 

unique(discriminaition)

pc1 = c(ELSA_data_with_PGS$pc1, 
        ELSA_data_with_PGS$pc1, 
        ELSA_data_with_PGS$pc1, 
        ELSA_data_with_PGS$pc1) 


pc2 = c(ELSA_data_with_PGS$pc2, 
        ELSA_data_with_PGS$pc2, 
        ELSA_data_with_PGS$pc2, 
        ELSA_data_with_PGS$pc2) 


pc3 = c(ELSA_data_with_PGS$pc3, 
        ELSA_data_with_PGS$pc3, 
        ELSA_data_with_PGS$pc3, 
        ELSA_data_with_PGS$pc3) 


pc4 = c(ELSA_data_with_PGS$pc4, 
        ELSA_data_with_PGS$pc4, 
        ELSA_data_with_PGS$pc4, 
        ELSA_data_with_PGS$pc4) 



pc5 = c(ELSA_data_with_PGS$pc5, 
        ELSA_data_with_PGS$pc5, 
        ELSA_data_with_PGS$pc5, 
        ELSA_data_with_PGS$pc5) 

pc6 = c(ELSA_data_with_PGS$pc6, 
        ELSA_data_with_PGS$pc6, 
        ELSA_data_with_PGS$pc6, 
        ELSA_data_with_PGS$pc6) 

pc7 = c(ELSA_data_with_PGS$pc7, 
        ELSA_data_with_PGS$pc7, 
        ELSA_data_with_PGS$pc7, 
        ELSA_data_with_PGS$pc7) 


pc8 = c(ELSA_data_with_PGS$pc8, 
        ELSA_data_with_PGS$pc8, 
        ELSA_data_with_PGS$pc8, 
        ELSA_data_with_PGS$pc8) 


pc9 = c(ELSA_data_with_PGS$pc9, 
        ELSA_data_with_PGS$pc9, 
        ELSA_data_with_PGS$pc9, 
        ELSA_data_with_PGS$pc9) 


pc10 = c(ELSA_data_with_PGS$pc10, 
        ELSA_data_with_PGS$pc10, 
        ELSA_data_with_PGS$pc10, 
        ELSA_data_with_PGS$pc10) 

alcohol = c(ELSA_data_with_PGS$w5alcunits, 
            ELSA_data_with_PGS$w6alcunits, 
            ELSA_data_with_PGS$w7alcunits,
            ELSA_data_with_PGS$w7alcunits) 



unique(alcohol)

unique(ELSA_data_with_PGS$w5smokec)
unique(ELSA_data_with_PGS$w5smoket)

smoking = c(ELSA_data_with_PGS$w5smokenum, 
            ELSA_data_with_PGS$w6smokenum,
            ELSA_data_with_PGS$w7smokenum,
            ELSA_data_with_PGS$w7smokenum)


unique(smoking)


physical_activity  = c(ELSA_data_with_PGS$w5pacomb1, 
                       ELSA_data_with_PGS$w6pacomb1,
                       ELSA_data_with_PGS$w7pacomb1,
                       ELSA_data_with_PGS$w7pacomb1)

unique(physical_activity)

BMI = c(ELSA_data_with_PGS$w6bmi_clean, 
        ELSA_data_with_PGS$w6bmi_clean, 
        ELSA_data_with_PGS$w6bmi_clean, 
        ELSA_data_with_PGS$w6bmi_clean)

diabetes_history = c(ELSA_data_with_PGS$w5diabetes_ever,
                     ELSA_data_with_PGS$w6diabetes_ever,
                     ELSA_data_with_PGS$w7diabetes_ever,
                     ELSA_data_with_PGS$w8diabetes_ever) 
                     
                     
unique(diabetes_history)

#ELSA_data_with_PGS$hypertension 


#add: hypertension_history stressful_event 
depression_original = c(ELSA_data_with_PGS$w5cesd, 
                        ELSA_data_with_PGS$w6cesd, 
                        ELSA_data_with_PGS$w7cesd, 
                        ELSA_data_with_PGS$w8cesd) 

unique(depression_original)

depression = 8 - depression_original



data_cox_ELSA_initial = data.frame(ID,
                                   MI_outcome,
                           follow_up, 
                           age,
                           sex, 
                           wealth,
                           alcohol, 
                           smoking,
                           physical_activity,
                           BMI, 
                           diabetes_history, 
                           depression, 
                           PGS, 
                           discriminaition, 
                           baseline_discriminaition, 
                           pc1, 
                           pc2, 
                           pc3, 
                           pc4, 
                           pc5, 
                           pc6, 
                           pc7, 
                           pc8, 
                           pc9, 
                           pc10)

length(unique(data_cox_ELSA_initial$ID))

summary(data_cox_ELSA_initial)

data_cox_ELSA_table = data.table(data_cox_ELSA_initial)
data_cox_ELSA= na.omit(data_cox_ELSA_table, cols = c("MI_outcome", "PGS", "baseline_discriminaition"))
length(unique(data_cox_ELSA$ID))

summary(data_cox_ELSA)

####################### insert function: cox_model_PGS.Rs

Univariate_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition, data_cox_ELSA))
M_uni_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition, data_cox_ELSA)
Univariate_ELSA_MI_outcome_gene_interaction = summary(M_uni_interaction)


Univariate_ELSA_MI_outcome_discrim_results = cbind(Univariate_ELSA_MI_outcome_discrim$coefficients, Univariate_ELSA_MI_outcome_discrim$conf.int)
head(Univariate_ELSA_MI_outcome_discrim_results, 1)
Univariate_ELSA_MI_outcome_gene_interaction_results = cbind(Univariate_ELSA_MI_outcome_gene_interaction$coefficients, Univariate_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Univariate_ELSA_MI_outcome_gene_interaction_results, 1)

#data_cox_ELSA = data.frame(ELSA_data_with_PGS$age)
#from literature: Model 1: Age, race, education, family income, marital status
Model_1_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_1_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth, data_cox_ELSA))

# plot interaction: https://stats.stackexchange.com/questions/464700/representing-interaction-plot-for-coxph-model-using-plot-model-in-r
M_1_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)
Model_1_ELSA_MI_outcome_gene_interaction = summary(M_1_interaction)
  
Model_1_ELSA_MI_outcome_gene_results = cbind(Model_1_ELSA_MI_outcome_gene$coefficients, Model_1_ELSA_MI_outcome_gene$conf.int)
head(Model_1_ELSA_MI_outcome_gene_results, 1)
Model_1_ELSA_MI_outcome_discrim_results = cbind(Model_1_ELSA_MI_outcome_discrim$coefficients, Model_1_ELSA_MI_outcome_discrim$conf.int)
head(Model_1_ELSA_MI_outcome_discrim_results, 1)
Model_1_ELSA_MI_outcome_gene_interaction_results = cbind(Model_1_ELSA_MI_outcome_gene_interaction$coefficients, Model_1_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_1_ELSA_MI_outcome_gene_interaction_results, 1)

#Model 2: 
#+ alcohol use, smoking status, moderate and vagarious exercise	

Model_2_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_2_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity, data_cox_ELSA))

M_2_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_2_ELSA_MI_outcome_gene_interaction = summary( M_2_interaction)

Model_2_ELSA_MI_outcome_gene_results = cbind(Model_2_ELSA_MI_outcome_gene$coefficients, Model_2_ELSA_MI_outcome_gene$conf.int)
head(Model_2_ELSA_MI_outcome_gene_results, 1)
Model_2_ELSA_MI_outcome_discrim_results = cbind(Model_2_ELSA_MI_outcome_discrim$coefficients, Model_2_ELSA_MI_outcome_discrim$conf.int)
head(Model_2_ELSA_MI_outcome_discrim_results, 1)
Model_2_ELSA_MI_outcome_gene_interaction_results = cbind(Model_2_ELSA_MI_outcome_gene_interaction$coefficients, Model_2_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_2_ELSA_MI_outcome_gene_interaction_results, 1)


########## alcohol: 
Model_2a_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + alcohol + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_2a_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + alcohol, data_cox_ELSA))

M_2a_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_2a_ELSA_MI_outcome_gene_interaction = summary( M_2a_interaction)

Model_2a_ELSA_MI_outcome_gene_results = cbind(Model_2a_ELSA_MI_outcome_gene$coefficients, Model_2a_ELSA_MI_outcome_gene$conf.int)
head(Model_2a_ELSA_MI_outcome_gene_results, 1)
Model_2a_ELSA_MI_outcome_discrim_results = cbind(Model_2a_ELSA_MI_outcome_discrim$coefficients, Model_2a_ELSA_MI_outcome_discrim$conf.int)
head(Model_2a_ELSA_MI_outcome_discrim_results, 1)
Model_2a_ELSA_MI_outcome_gene_interaction_results = cbind(Model_2a_ELSA_MI_outcome_gene_interaction$coefficients, Model_2a_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_2a_ELSA_MI_outcome_gene_interaction_results, 1)



#scale(smoking) + scale(physical_activity) +
Model_2as_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + smoking + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_2as_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + smoking, data_cox_ELSA))

M_2s_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + smoking +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_2as_ELSA_MI_outcome_gene_interaction = summary( M_2s_interaction)

Model_2as_ELSA_MI_outcome_gene_results = cbind(Model_2as_ELSA_MI_outcome_gene$coefficients, Model_2as_ELSA_MI_outcome_gene$conf.int)
head(Model_2as_ELSA_MI_outcome_gene_results, 1)
Model_2as_ELSA_MI_outcome_discrim_results = cbind(Model_2as_ELSA_MI_outcome_discrim$coefficients, Model_2as_ELSA_MI_outcome_discrim$conf.int)
head(Model_2as_ELSA_MI_outcome_discrim_results, 1)
Model_2as_ELSA_MI_outcome_gene_interaction_results = cbind(Model_2as_ELSA_MI_outcome_gene_interaction$coefficients, Model_2as_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_2as_ELSA_MI_outcome_gene_interaction_results, 1)


########### 


#scale(smoking) + scale(physical_activity) +
Model_2apa_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_2apa_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + physical_activity, data_cox_ELSA))

M_2pa_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_2apa_ELSA_MI_outcome_gene_interaction = summary( M_2s_interaction)

Model_2apa_ELSA_MI_outcome_gene_results = cbind(Model_2apa_ELSA_MI_outcome_gene$coefficients, Model_2apa_ELSA_MI_outcome_gene$conf.int)
head(Model_2apa_ELSA_MI_outcome_gene_results, 1)
Model_2apa_ELSA_MI_outcome_discrim_results = cbind(Model_2apa_ELSA_MI_outcome_discrim$coefficients, Model_2apa_ELSA_MI_outcome_discrim$conf.int)
head(Model_2apa_ELSA_MI_outcome_discrim_results, 1)
Model_2apa_ELSA_MI_outcome_gene_interaction_results = cbind(Model_2apa_ELSA_MI_outcome_gene_interaction$coefficients, Model_2apa_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_2apa_ELSA_MI_outcome_gene_interaction_results, 1)


#Model 3: 
#+ BMI, (height separately) systolic blood pressure, antihypertensive medication, Diabetes/fasting blood glucose status, total cholesterol, high-density lipoprotein cholesterol, low-density lipoprotein cholesterol, triglycerides, and use of lipid lowering medication, history of diabetes and hypertension. 	
Model_3_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth +  diabetes_history + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_3_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + diabetes_history, data_cox_ELSA))

M_3_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + diabetes_history + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_3_ELSA_MI_outcome_gene_interaction = summary( M_3_interaction)


Model_3_ELSA_MI_outcome_gene_results = cbind(Model_3_ELSA_MI_outcome_gene$coefficients, Model_3_ELSA_MI_outcome_gene$conf.int)
head(Model_3_ELSA_MI_outcome_gene_results, 1)
Model_3_ELSA_MI_outcome_discrim_results = cbind(Model_3_ELSA_MI_outcome_discrim$coefficients, Model_3_ELSA_MI_outcome_discrim$conf.int)
head(Model_3_ELSA_MI_outcome_discrim_results, 1)
Model_3_ELSA_MI_outcome_gene_interaction_results = cbind(Model_3_ELSA_MI_outcome_gene_interaction$coefficients, Model_3_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_3_ELSA_MI_outcome_gene_interaction_results, 1)

#Model 4: 
#+  depressive symptoms and chronic stress burden/ stressful events

Model_4_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_4_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth  + depression, data_cox_ELSA))
M_4_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data = data_cox_ELSA)
Model_4_ELSA_MI_outcome_gene_interaction = summary( M_4_interaction)


Model_4_ELSA_MI_outcome_gene_results = cbind(Model_4_ELSA_MI_outcome_gene$coefficients, Model_4_ELSA_MI_outcome_gene$conf.int)
head(Model_4_ELSA_MI_outcome_gene_results, 1)
Model_4_ELSA_MI_outcome_discrim_results = cbind(Model_4_ELSA_MI_outcome_discrim$coefficients, Model_4_ELSA_MI_outcome_discrim$conf.int)
head(Model_4_ELSA_MI_outcome_discrim_results, 1)
Model_4_ELSA_MI_outcome_gene_interaction_results = cbind(Model_4_ELSA_MI_outcome_gene_interaction$coefficients, Model_4_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_4_ELSA_MI_outcome_gene_interaction_results, 1)


#################

#Model 5: all 

Model_5_ELSA_MI_outcome_gene = summary( coxph( Surv(follow_up, MI_outcome) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA))
Model_5_ELSA_MI_outcome_discrim = summary( coxph( Surv(follow_up, MI_outcome) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression, data_cox_ELSA))

M_5_interaction = coxph( Surv(follow_up, MI_outcome) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + diabetes_history + depression +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_ELSA)

Model_5_ELSA_MI_outcome_gene_interaction = summary( M_5_interaction)

Model_5_ELSA_MI_outcome_gene_results = cbind(Model_5_ELSA_MI_outcome_gene$coefficients, Model_5_ELSA_MI_outcome_gene$conf.int)
head(Model_5_ELSA_MI_outcome_gene_results, 1)
Model_5_ELSA_MI_outcome_discrim_results = cbind(Model_5_ELSA_MI_outcome_discrim$coefficients, Model_5_ELSA_MI_outcome_discrim$conf.int)
head(Model_5_ELSA_MI_outcome_discrim_results, 1)
Model_5_ELSA_MI_outcome_gene_interaction_results = cbind(Model_5_ELSA_MI_outcome_gene_interaction$coefficients, Model_5_ELSA_MI_outcome_gene_interaction$conf.int)
tail(Model_5_ELSA_MI_outcome_gene_interaction_results, 1)

output_1 = head(Univariate_ELSA_MI_outcome_discrim_results, 1)
output_2 = tail(Univariate_ELSA_MI_outcome_gene_interaction_results, 1)

output_3 = head(Model_1_ELSA_MI_outcome_gene_results, 1)
output_4 = head(Model_1_ELSA_MI_outcome_discrim_results, 1)
output_5 = tail(Model_1_ELSA_MI_outcome_gene_interaction_results, 1)

output_6 = head(Model_2a_ELSA_MI_outcome_gene_results, 1)
output_7 = head(Model_2a_ELSA_MI_outcome_discrim_results, 1)
output_8 = tail(Model_2a_ELSA_MI_outcome_gene_interaction_results, 1)

output_9 = head(Model_3_ELSA_MI_outcome_gene_results, 1)
output_10 = head(Model_3_ELSA_MI_outcome_discrim_results, 1)
output_11 = tail(Model_3_ELSA_MI_outcome_gene_interaction_results, 1)

output_12 = head(Model_4_ELSA_MI_outcome_gene_results, 1)
output_13 = head(Model_4_ELSA_MI_outcome_discrim_results, 1)
output_14 = tail(Model_4_ELSA_MI_outcome_gene_interaction_results, 1)

model = c("Univariate", 
          "Univariate",
          
          "Model_1", 
          "Model_1", 
          "Model_1", 
          
          "Model_2a", 
          "Model_2a", 
          "Model_2a", 
          
          "Model_3", 
          "Model_3", 
          "Model_3", 
          
          "Model_4", 
          "Model_4", 
          "Model_4") 


          
results = rbind(output_1, 
                output_2, 
                output_3, 
                output_4, 
                output_5, 
                output_6, 
                output_7, 
                output_8, 
                output_9,
                output_10, 
                output_11, 
                output_12, 
                output_13, 
                output_14) 

table = cbind(model, results)

table = as.data.frame(table)
cbind(table[1:3], table[6], table[9], table[10])
#ties = "efron"
                     
####### plots 

set_label(data_cox_ELSA$MI_outcome) <- "Myocardial Infarction"

plot_M_uni_int = plot_model(title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_uni_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_1_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_1_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_2_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_2_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#alcohol
plot_M_2a_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_2a_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#smoking
plot_M_2s_int = plot_model(title = "",
                           axis.title = "Hazard Ratio",
                           show.values = TRUE, 
                           show.p = TRUE, 
                           M_2s_interaction, 
                           terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#physical activity
plot_M_2pa_int = plot_model(title = "",
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_2pa_interaction, 
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_3_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_3_interaction, 
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_4_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_4_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_5_int = plot_model(title = "",
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_5_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

Model_5 = c("Model_5",
            "Model_5",
            "Model_5")

data_5 = cbind(Model_5, plot_M_5_int$data[1:3], plot_M_5_int$data[5:6], plot_M_5_int$data[9]) 


# merge plots with models heading 

print(plot_M_uni_int)
print(plot_M_1_int)
print(plot_M_2_int)
print(plot_M_2a_int)
print(plot_M_2s_int)
print(plot_M_2pa_int)
print(plot_M_3_int)
print(plot_M_4_int)
print(plot_M_5_int)

#plots: https://strengejacke.wordpress.com/2017/10/23/one-function-to-rule-them-all-visualization-of-regression-models-in-rstats-w-sjplot/
  
