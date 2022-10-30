directory = "/Users/aliya/my_docs/"

OUTPUT = "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/diabetes/"

folder_unadjusted = paste("diabetes_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("diabetes_adjusted_discrim_bin/", sep = "")

names_adjusted_pca = c("gene", 
                       "discrimination", 
                       "gene x discrimination") 


names_adjusted = c("gene", 
                   "discrimination", 
                   "gene x discrimination") 


names_unadjusted_pca = c("gene", 
                         "discrimination", 
                         "gene x discrimination") 


names_unadjusted = c("gene", 
                     "discrimination", 
                     "gene x discrimination") 

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### unadjusted 
############
#adjusted for pca 
ELSA_pca_unadjusted_diabetes = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_results_ELSA_pca_unadjusted.csv", sep = "")))
HRS_pca_unadjusted_diabetes = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_results_HRS_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_unadjusted_diabetes = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_results_ELSA_unadjusted.csv", sep = "")))
#HRS_unadjusted_diabetes = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT,  folder_unadjusted, "diabetes_results_HRS_unadjusted.csv", sep = "")))


############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### adjusted 
############
ELSA_pca_adjusted_diabetes = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_results_ELSA_pca_adjusted.csv", sep = "")))
HRS_pca_adjusted_diabetes = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_results_HRS_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_adjusted_diabetes = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_results_ELSA_adjusted.csv", sep = "")))
#HRS_adjusted_diabetes = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_results_HRS_adjusted.csv", sep = "")))

na_filler = rep("NA", 7)


diabetes_unadjusted = rbind(ELSA_pca_unadjusted_diabetes,
                            HRS_pca_unadjusted_diabetes)

diabetes_adjusted = rbind(ELSA_pca_adjusted_diabetes, 
                          HRS_pca_adjusted_diabetes)

Table_diabetes = cbind(diabetes_unadjusted, diabetes_adjusted)
write.csv(Table_diabetes, file = paste(directory, OUTPUT, "Table_diabetes_per_wave.csv", sep = ""))

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############ ############ ############ ############ ############  ############ ############ ############ ############ ############  

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
############
############ #composite over waves  
############
############ #composite over waves  
############
############ #composite over waves  
############

#composite over waves #### unadjusted 

#composite over waves HRS 
#adjutsted for pca
HRS_composite_pca_unadjusted_diabetes = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_HRS_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_unadjusted_diabetes = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_HRS_composite_unadjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_unadjusted_diabetes = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_ELSA_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_unadjusted_diabetes = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "diabetes_ELSA_composite_unadjusted.csv", sep = "")))



#composite over waves #### adjusted 

#adjutsted for pca HRS
HRS_composite_pca_adjusted_diabetes = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_HRS_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_adjusted_diabetes = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_HRS_composite_adjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_adjusted_diabetes = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_ELSA_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_adjusted_diabetes = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "diabetes_ELSA_composite_adjusted.csv", sep = "")))

######## Table 2 composite & adjusted for PCA 
composite_diabetes_unadjusted = rbind(ELSA_composite_pca_unadjusted_diabetes,
                                HRS_composite_pca_unadjusted_diabetes)

composite_diabetes_adjusted = rbind(ELSA_composite_pca_adjusted_diabetes,
                                    HRS_composite_pca_adjusted_diabetes)

Table_composite_diabetes = cbind(composite_diabetes_unadjusted, composite_diabetes_adjusted)
write.csv(Table_composite_diabetes, file = paste(directory, OUTPUT, "Table_composite_diabetes.csv", sep = ""))






