directory = "/Users/aliya/my_docs/"

OUTPUT = "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/depressive_symptoms/"

folder_unadjusted = paste("depressive_symptoms_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("depressive_symptoms_adjusted_discrim_bin/", sep = "")

names_adjusted_pca = c("gene", 
                       "discrimination", 
                       "gene x discrimination") 



names_unadjusted_pca = c("gene", 
                         "discrimination", 
                         "gene x discrimination") 



############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### unadjusted 
############
#adjusted for pca 
ELSA_pca_unadjusted_depressive_symptoms = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_results_ELSA_pca_unadjusted.csv", sep = "")))
HRS_pca_unadjusted_depressive_symptoms = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_results_HRS_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_unadjusted_depressive_symptoms = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_results_ELSA_unadjusted.csv", sep = "")))
#HRS_unadjusted_depressive_symptoms = cbind(names_unadjusted, read.csv(paste(directory, OUTPUT,  folder_unadjusted, "depressive_symptoms_results_HRS_unadjusted.csv", sep = "")))


############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### adjusted 
############
ELSA_pca_adjusted_depressive_symptoms = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_results_ELSA_pca_adjusted.csv", sep = "")))
HRS_pca_adjusted_depressive_symptoms = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_results_HRS_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_adjusted_depressive_symptoms = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_results_ELSA_adjusted.csv", sep = "")))
#HRS_adjusted_depressive_symptoms = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_results_HRS_adjusted.csv", sep = "")))

na_filler = rep("NA", 7)


depressive_symptoms_unadjusted = rbind(ELSA_pca_unadjusted_depressive_symptoms,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler, 
                              
                              HRS_pca_unadjusted_depressive_symptoms, 
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler,
                              na_filler)

depressive_symptoms_adjusted = rbind(ELSA_pca_adjusted_depressive_symptoms, HRS_pca_adjusted_depressive_symptoms)

Table_depressive_symptoms = cbind(depressive_symptoms_unadjusted, depressive_symptoms_adjusted)
write.csv(Table_depressive_symptoms, file = paste(directory, OUTPUT, "Table_depressive_symptoms_per_wave.csv", sep = ""))

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
HRS_composite_pca_unadjusted_depressive_symptoms = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_HRS_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_unadjusted_depressive_symptoms = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_HRS_composite_unadjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_unadjusted_depressive_symptoms = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_ELSA_composite_pca_unadjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_unadjusted_depressive_symptoms = cbind(names_unadjusted,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "depressive_symptoms_ELSA_composite_unadjusted.csv", sep = "")))



#composite over waves #### adjusted 

#adjutsted for pca HRS
HRS_composite_pca_adjusted_depressive_symptoms = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_HRS_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#HRS_composite_adjusted_depressive_symptoms = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_HRS_composite_adjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_adjusted_depressive_symptoms = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_ELSA_composite_pca_adjusted.csv", sep = "")))
#not adjusted for pca 
#ELSA_composite_adjusted_depressive_symptoms = cbind(names_adjusted, read.csv(paste(directory, OUTPUT, folder_adjusted, "depressive_symptoms_ELSA_composite_adjusted.csv", sep = "")))

######## Table 2 composite & adjusted for PCA 
composite_depressive_symptoms_unadjusted = rbind(ELSA_composite_pca_unadjusted_depressive_symptoms,
                                        na_filler,
                                        na_filler,
                                        na_filler,
                                        
                                        
                                        HRS_composite_pca_unadjusted_depressive_symptoms,
                                        na_filler,
                                        na_filler,
                                        na_filler)

composite_depressive_symptoms_adjusted = rbind(ELSA_composite_pca_adjusted_depressive_symptoms, HRS_composite_pca_adjusted_depressive_symptoms)

Table_composite_depressive_symptoms = cbind(composite_depressive_symptoms_unadjusted, composite_depressive_symptoms_adjusted)
write.csv(Table_composite_depressive_symptoms, file = paste(directory, OUTPUT, "Table_composite_depressive_symptoms.csv", sep = ""))






