directory = "/Users/aliya/my_docs/"

OUTPUT = "KCL_postDoc/Data_analysis/polygenetic_score/RESULTS/MI/"

folder_unadjusted = paste("MI_unadjusted_discrim_bin/", sep = "")
folder_adjusted = paste("MI_adjusted_discrim_bin/", sep = "")

names_adjusted_pca = c("gene", 
                       "discrimination", 
                       "gene x discrimination") 


names_adjusted = c("gene", 
                    "discrimination", 
                    "gene x discrimination") 


names_unadjusted_pca = c("gene", 
                    "discrimination", 
                         "gene x discrimination") 


names_unadjusted = c("Intercept", 
                         "gene", 
                         "discrimination", 
                         "gene x discrimination") 

############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### unadjusted 
############
#adjusted for pca 
ELSA_pca_unadjusted_mi = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "mi_results_ELSA_pca_unadjusted.csv", sep = "")))
HRS_pca_unadjusted_mi = cbind(names_unadjusted_pca, read.csv(paste(directory, OUTPUT, folder_unadjusted, "mi_results_HRS_pca_unadjusted.csv", sep = "")))


############ ############ ############ ############ ############  ############ ############ ############ ############ ############  
####  ####  #### adjusted 
############
ELSA_pca_adjusted_mi = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_ELSA_pca_adjusted.csv", sep = "")))
HRS_pca_adjusted_mi = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "mi_results_HRS_pca_adjusted.csv", sep = "")))



MI_unadjusted = rbind(ELSA_pca_unadjusted_mi,
                      HRS_pca_unadjusted_mi)

MI_adjusted = rbind(ELSA_pca_adjusted_mi, 
                    HRS_pca_adjusted_mi)

Table_MI = cbind(MI_unadjusted, MI_adjusted)
write.csv(Table_MI, file = paste(directory, OUTPUT, "Table_MI_per_wave.csv", sep = ""))

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
HRS_composite_pca_unadjusted_mi = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_HRS_composite_unadjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_unadjusted_mi = cbind(names_unadjusted_pca,  read.csv(paste(directory, OUTPUT, folder_unadjusted, "MI_ELSA_composite_pca_unadjusted.csv", sep = "")))



#composite over waves #### adjusted 
 
#adjutsted for pca HRS
HRS_composite_pca_adjusted_mi = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_HRS_composite_pca_adjusted.csv", sep = "")))


#adjutsted for pca ELSA
ELSA_composite_pca_adjusted_mi = cbind(names_adjusted_pca, read.csv(paste(directory, OUTPUT, folder_adjusted, "MI_ELSA_composite_pca_adjusted.csv", sep = "")))

######## Table 2 composite & adjusted for PCA 
composite_MI_unadjusted = rbind(ELSA_composite_pca_unadjusted_mi,
                                 HRS_composite_pca_unadjusted_mi)

composite_MI_adjusted = rbind(ELSA_composite_pca_adjusted_mi,
                              HRS_composite_pca_adjusted_mi)

Table_composite_MI = cbind(composite_MI_unadjusted, composite_MI_adjusted)
write.csv(Table_composite_MI, file = paste(directory, OUTPUT, "Table_composite_MI.csv", sep = ""))






