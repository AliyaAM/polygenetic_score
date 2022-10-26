

PGS_glm_function_ELSA = function (data_ELSA_subset, 

                             analysis_variable_name, 
                             wave_number,
                             outcome_name, 
                             dataset, 
                             
                      
                             outcome_ELSA, 

                             gene_ELSA, 

                             covariate1, 
                             covariate2,
                             covariate3, 
                             covariate4, 
                             

                             
                             discrimination_VAR_elsa){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  dataset = dataset
  wave_number = wave_number
  outcome_name = outcome_name
  # calculate the number of cases for this subset 
  
  N_ELSA_subset = nrow(data_ELSA_subset)
  print("done 3")
  
  print("total n")
  print(N_ELSA_subset)
  
  #calculate the number of people who perceived this type of discrimination 
  
  ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 

  N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  print("done 4")
  
  print("number discriminated")
  print(N_ELSA_discrimYES)
  #predictor dummy varibale: country (UK vs USA)

  discrimination = c(data_ELSA_subset[ ,  discrimination_VAR_elsa])
  
  print("done 5")
  discrimination
  
  data_both_countries = data.frame(discrimination)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  

  data_both_countries$outcome = c(data_ELSA_subset[ , outcome_ELSA])

  data_both_countries$PC1 = data_ELSA_subset$pc1
  data_both_countries$PC2 = data_ELSA_subset$pc2
  data_both_countries$PC3 = data_ELSA_subset$pc3
  data_both_countries$PC4 = data_ELSA_subset$pc4
  data_both_countries$PC5 = data_ELSA_subset$pc5
  data_both_countries$PC6 = data_ELSA_subset$pc6
  data_both_countries$PC7 = data_ELSA_subset$pc7
  data_both_countries$PC8 = data_ELSA_subset$pc8
  data_both_countries$PC9 = data_ELSA_subset$pc9
  data_both_countries$PC10 = data_ELSA_subset$pc10

  
  data_both_countries$gene = c(data_ELSA_subset[ , gene_ELSA])
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 
  
  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 == "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    # print("we are in test 1")
    # data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
    #                                          data_HRS_subset[ ,   covariate1])
    # 
    # 
    # fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1], 
    #            
    #            data = data_both_countries)
    # 
    # fm2 <- glm(discrimination ~ country_cat 
    #            + data_both_countries[ ,   covariate1] , 
    #            
    #            data = data_both_countries)
    
    
    print("done 6")
    
    glm_outcome_discrim =  glm(outcome ~  discrimination, 
                               data = data_both_countries, 
                               family = binomial)
    
    
    print("done 7")

    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                            data = data_both_countries, 
                            family = binomial)
    
    
    
    
    print("done 8")
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1+ PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                                        data = data_both_countries, 
                                        family = binomial)
    
    
    summary_discrim = summary(glm_outcome_discrim)
    summary_gene = summary(glm_outcome_gene)
    summary_interaction = summary(glm_outcome_gene_interaction)
    
    print("done 9")
    
  }
  
  
  
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1], 
                               
                               data = data_both_countries, 
                               family = binomial)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                            + data_both_countries[ ,   covariate1], 
                            
                            data = data_both_countries, 
                            family = binomial)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                                        + data_both_countries[ ,   covariate1], 
                                        
                                        data = data_both_countries, 
                                        family = binomial)
    
    
    summary_discrim = summary(glm_outcome_discrim)
    summary_gene = summary(glm_outcome_gene)
    summary_interaction = summary(glm_outcome_gene_interaction)
    
    
  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2])
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1]
                               + data_both_countries[ ,   covariate2], 
                               
                               data = data_both_countries, 
                               family = binomial)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2], 
                            
                            data = data_both_countries, 
                            family = binomial)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2], 
                                        
                                        data = data_both_countries, 
                                        family = binomial)
    
    
    summary_discrim = summary(glm_outcome_discrim)
    summary_gene = summary(glm_outcome_gene)
    summary_interaction = summary(glm_outcome_gene_interaction)
    
    
  }
  
  # when  covariate 1 and covariate 2 and covariate 3 (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 == "NA"){
    
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3])
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1]
                               + data_both_countries[ ,   covariate2]
                               + data_both_countries[ ,   covariate3], 
                               
                               data = data_both_countries, 
                               family = binomial)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2]
                            + data_both_countries[ ,   covariate3], 
                            
                            data = data_both_countries, 
                            family = binomial)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2]
                                        + data_both_countries[ ,   covariate3], 
                                        
                                        data = data_both_countries, 
                                        family = binomial)
    
    summary_discrim = summary(glm_outcome_discrim)
    summary_gene = summary(glm_outcome_gene)
    summary_interaction = summary(glm_outcome_gene_interaction)
    
    
  } 
  
  # when  covariate 1 and covariate 2 and covariate 3 and covariate 4 (i.e, not NA, !=NA)  are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 != "NA"){
    
    
    print("we are in, test 4")
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3])
    
    data_both_countries[ ,   covariate4]= c(data_ELSA_subset[ ,   covariate4])
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1]
                               + data_both_countries[ ,   covariate2]
                               + data_both_countries[ ,   covariate3]
                               + data_both_countries[ ,   covariate4], 
                               
                               data = data_both_countries, 
                               family = binomial)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2]
                            + data_both_countries[ ,   covariate3]
                            + data_both_countries[ ,   covariate4],
                            
                            
                            data = data_both_countries, 
                            family = binomial)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2]
                                        + data_both_countries[ ,   covariate3]
                                        + data_both_countries[ ,   covariate4], 
                                        
                                        data = data_both_countries, 
                                        family = binomial)
    
    
    summary_discrim = summary(glm_outcome_discrim)
    summary_gene = summary(glm_outcome_gene)
    summary_interaction = summary(glm_outcome_gene_interaction)
    
    
    
    
  }
  
  print("done 10")
  
  path <- OUTPUT_ROOT
  
  folder = paste(analysis_variable_name, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  
  results_interaction = as.data.frame(summary_interaction$coefficients)
  write.csv(results_interaction, file = paste(OUTPUT_ROOT, folder,  "results_interaction.csv", sep=""))
  
  
  # add other diagnostic statistic 
  
  # results_interaction_other = as.data.frame(summary_interaction$aic, 
  #                                           summary_interaction$deviance,
  #                                           summary_interaction$df.residual,
  #                                           summary_interaction$df.null,
  #                                           summary_interaction$dispersion) 
  # 
  # 
  # colnames(results_interaction_other) = c("aic", 
  #                                         "deviance",
  #                                         "df.residual",
  #                                         "df.null",
  #                                         "dispersion") 
  # 
  # write.csv(results_interaction_other, file = paste(OUTPUT_ROOT, folder,  "results_interaction_other.csv", sep=""))
  print(results_interaction)
  
  print("done 11")
  #print(glm_outcome_gene_interaction)
  #logOR_CI_outcome_gene_interaction = cbind(OR = coef(glm_outcome_gene_interaction), confint(glm_outcome_gene_interaction))
  print("done 11a")
  
  #gene = gene_ELSA
  #discrimination_var = discrimination_VAR_elsa
  #term = paste(gene, ":", discrimination_var, sep = "")
  #p_values = summary_interaction$coefficients[,4]
  print("done 11b")
  
  #print(term)
  #print(glm_outcome_gene_interaction)
  

  #CI_interval_inter = confint(glm_outcome_gene_interaction, "gene:discrimination")

  #print(CI_interval_inter)

  #CI_interval_inter = exp(CI_interval_inter)
  #logOR_results_all = cbind(logOR_CI_outcome_gene_interaction, p_values)
  print("done 11c")
  ##########################
  ##########################

  #coefficient_inter = glm_outcome_gene_interaction$coefficients[length(glm_outcome_gene_interaction$coefficients)]
  #coefficient_inter = exp(coefficient_inter)


  #OR_CI_outcome_gene_interaction = c(coefficient_inter, CI_interval_inter)

  
  OR_CI_outcome_gene_interaction = exp(cbind(OR = coef(glm_outcome_gene_interaction), confint(glm_outcome_gene_interaction)))
  print("done 11d")

  #print(OR_CI_outcome_gene_interaction)
 # p_values = ****  tail ***** (summary_interaction$coefficients[,4] **** , n = 1) ****
  p_values = summary_interaction$coefficients[,4]
  #p_value = tail(p_values, n = 1)
  print("done 11e")

  interaction_OR_CI_pvalue = cbind(OR_CI_outcome_gene_interaction, p_values)


  print("done 12")


  # cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  # cross_country_OR_UK = cross_country_OR[2, 1]
  # CI1_UK = cross_country_OR[2, 2]
  # CI2_UK = cross_country_OR[2, 3]
  #
  #
  # cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  # cross_country_OR_USA = cross_country_OR[1, 1]
  # CI1_USA = cross_country_OR[1, 2]
  # CI2_USA = cross_country_OR[1, 3]
  #
  # ## various equivalent specifications of the LR test
  # cross_national_diff = lrtest(fm1, fm2)
  #
  # chi_value_cross_national = cross_national_diff$stats[1]
  # pvalue_cross_national = cross_national_diff$stats[3]
  #




  #interaction_logOR_CI_pvalue = tail(logOR_results_all, n = 1)

  print("done 13")

  # Interaction_log_OR_findings = data.frame(analysis_variable_name,
  #                                      dataset,
  #                                      wave_number,
  #                                      outcome_name,
  #
  #                                      N_ELSA_subset,
  #
  #                                      N_ELSA_discrimYES,
  #
  #
  #                                   interaction_logOR_CI_pvalue)
  #
  #
  # colnames(Interaction_log_OR_findings) = c("analysis_variable_name",
  #                                    "dataset",
  #
  #                                    "wave_number",
  #                                    "outcome_name",
  #
  #                                    "N_subset",
  #
  #                                    "N_discrimYES",
  #
  #                                    #"interaction_OR_CI_pvalue")
  #                                     "log OR",
  #                                     "CI1",
  #                                     "CI2",
  #                                    "p_value")


  Interaction_findings = data.frame(analysis_variable_name,
                                           dataset,
                                           wave_number,
                                           outcome_name,

                                           N_ELSA_subset,

                                           N_ELSA_discrimYES,


                                    interaction_OR_CI_pvalue)


  colnames(Interaction_findings) = c("analysis_variable_name",
                                            "dataset",

                                            "wave_number",
                                            "outcome_name",

                                            "N_subset",

                                            "N_discrimYES",

                                            #"interaction_OR_CI_pvalue")
                                            "OR",
                                            "CI1",
                                            "CI2",
                                            "p_value")

  write.csv(Interaction_findings, file = paste(OUTPUT_ROOT, folder,  "findings.csv", sep=""))
  

  print("completed")
  
  result_table = 
    
    
    result_table = data.frame(dataset,
                              wave_number,
                              interaction_OR_CI_pvalue)
  
  
  colnames(result_table) = c(  "dataset",
                               "wave_number",
                               "OR",
                               "CI1",
                               "CI2",
                               "p_value")
  
 OR_rounded = round(result_table$OR, 2) 
 
 CI95_edited = paste("[", round(result_table$CI1, 2), ";", round(result_table$CI2, 2), "]")

 p_value_rounded = round(result_table$p_value, 4)
 
 
 result_table_edited = data.frame(result_table$dataset, 
                                  result_table$wave_number, 
                                  OR_rounded, 
                                  CI95_edited, 
                                  p_value_rounded)
 
 
colnames(result_table_edited) = c("dataset", 
                                  "wave", 
                                  "OR", 
                                  "95 % CI", 
                                  "p-value") 

 
 
  #ELSA_OR_value,
  #ELSA_CI1,
  #ELSA_CI2,
  #HRS_OR_value,
  #HRS_CI1,
  #HRS_CI2)


  return(result_table_edited)
}



#https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html



# #glm plotting logit regression
# #http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
