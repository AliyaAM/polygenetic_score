library(dplyr)

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

  data_both_countries$age = data_ELSA_subset$age
  data_both_countries$sex = data_ELSA_subset$sex
  
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
                               family = gaussian)
    
    
    print("done 7")

    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex, 
                            data = data_both_countries, 
                            family = gaussian)
    
    
    
    
    print("done 8")
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1+ PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex, 
                                        data = data_both_countries, 
                                        family = gaussian)
    
    

    summary_discrimination = summary(glm_outcome_discrim)
    summary_discrimination_coefficients = as.data.frame(summary_discrimination$coefficients)
    discrimination_result = slice(summary_discrimination_coefficients, 2)
    
    summary_gene = summary(glm_outcome_gene)
    summary_gene_coefficients = as.data.frame(summary_gene$coefficients)
    gene_result = slice(summary_gene_coefficients, 2)
    
    summary_interaction = summary(glm_outcome_gene_interaction)
    interaction_result = tail(summary_interaction$coefficients, 1)
    
    results = rbind(gene_result, 
                    discrimination_result,
                    interaction_result)
    
    
  }
  
  
  
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1], 
                               
                               data = data_both_countries, 
                               family = gaussian)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                            + data_both_countries[ ,   covariate1], 
                            
                            data = data_both_countries, 
                            family = gaussian)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                                        + data_both_countries[ ,   covariate1], 
                                        
                                        data = data_both_countries, 
                                        family = gaussian)
    
    
    summary_discrimination = summary(glm_outcome_discrim)
    summary_discrimination_coefficients = as.data.frame(summary_discrimination$coefficients)
    discrimination_result = slice(summary_discrimination_coefficients, 2)
    
    summary_gene = summary(glm_outcome_gene)
    summary_gene_coefficients = as.data.frame(summary_gene$coefficients)
    gene_result = slice(summary_gene_coefficients, 2)
    
    summary_interaction = summary(glm_outcome_gene_interaction)
    interaction_result = tail(summary_interaction$coefficients, 1)
    
    results = rbind(gene_result, 
                    discrimination_result,
                    interaction_result)
    
    
  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2])
    
    
    glm_outcome_discrim =  glm(outcome ~  discrimination
                               + data_both_countries[ ,   covariate1]
                               + data_both_countries[ ,   covariate2], 
                               
                               data = data_both_countries, 
                               family = gaussian)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2], 
                            
                            data = data_both_countries, 
                            family = gaussian)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2], 
                                        
                                        data = data_both_countries, 
                                        family = gaussian)
    
    
    summary_discrimination = summary(glm_outcome_discrim)
    summary_discrimination_coefficients = as.data.frame(summary_discrimination$coefficients)
    discrimination_result = slice(summary_discrimination_coefficients, 2)
    
    summary_gene = summary(glm_outcome_gene)
    summary_gene_coefficients = as.data.frame(summary_gene$coefficients)
    gene_result = slice(summary_gene_coefficients, 2)
    
    summary_interaction = summary(glm_outcome_gene_interaction)
    interaction_result = tail(summary_interaction$coefficients, 1)
    
    results = rbind(gene_result, 
                    discrimination_result,
                    interaction_result)
    
    
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
                               family = gaussian)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2]
                            + data_both_countries[ ,   covariate3], 
                            
                            data = data_both_countries, 
                            family = gaussian)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2]
                                        + data_both_countries[ ,   covariate3], 
                                        
                                        data = data_both_countries, 
                                        family = gaussian)
    
    summary_discrimination = summary(glm_outcome_discrim)
    summary_discrimination_coefficients = as.data.frame(summary_discrimination$coefficients)
    discrimination_result = slice(summary_discrimination_coefficients, 2)
    
    summary_gene = summary(glm_outcome_gene)
    summary_gene_coefficients = as.data.frame(summary_gene$coefficients)
    gene_result = slice(summary_gene_coefficients, 2)
    
    summary_interaction = summary(glm_outcome_gene_interaction)
    interaction_result = tail(summary_interaction$coefficients, 1)
    
    results = rbind(gene_result, 
                    discrimination_result,
                    interaction_result)
    
    
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
                               family = gaussian)
    
    
    glm_outcome_gene =  glm(outcome ~  gene + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                            + data_both_countries[ ,   covariate1]
                            + data_both_countries[ ,   covariate2]
                            + data_both_countries[ ,   covariate3]
                            + data_both_countries[ ,   covariate4],
                            
                            
                            data = data_both_countries, 
                            family = gaussian)
    
    
    glm_outcome_gene_interaction =  glm(outcome ~  gene * discrimination + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + age + sex
                                        + data_both_countries[ ,   covariate1]
                                        + data_both_countries[ ,   covariate2]
                                        + data_both_countries[ ,   covariate3]
                                        + data_both_countries[ ,   covariate4], 
                                        
                                        data = data_both_countries, 
                                        family = gaussian)
    
    
    summary_discrimination = summary(glm_outcome_discrim)
    summary_discrimination_coefficients = as.data.frame(summary_discrimination$coefficients)
    discrimination_result = slice(summary_discrimination_coefficients, 2)
    
    summary_gene = summary(glm_outcome_gene)
    summary_gene_coefficients = as.data.frame(summary_gene$coefficients)
    gene_result = slice(summary_gene_coefficients, 2)
    
    summary_interaction = summary(glm_outcome_gene_interaction)
    interaction_result = tail(summary_interaction$coefficients, 1)
    
    results = rbind(gene_result, 
                    discrimination_result,
                    interaction_result)
    
    
    
    
  }
  
  print("done 10")
  
  path <- OUTPUT_ROOT
  
  folder = paste(analysis_variable_name, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  
  results_interaction = as.data.frame(results)
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
  
  
  OR_CI_outcome_gene = exp(cbind(OR = coef(glm_outcome_gene), confint(glm_outcome_gene)))
  OR_CI_outcome_gene = as.data.frame(OR_CI_outcome_gene)
  OR_CI_outcome_gene_result = slice(OR_CI_outcome_gene, 2)
  
  OR_CI_outcome_discrim = exp(cbind(OR = coef(glm_outcome_discrim), confint(glm_outcome_discrim)))
  OR_CI_outcome_discrim = as.data.frame(OR_CI_outcome_discrim)
  OR_CI_outcome_discrim_result = slice(OR_CI_outcome_discrim, 2)
  
  
  OR_CI_outcome_gene_interaction = exp(cbind(OR = coef(glm_outcome_gene_interaction), confint(glm_outcome_gene_interaction)))
  OR_CI_outcome_gene_interaction_result = tail(OR_CI_outcome_gene_interaction, 1)
  
  print("done 11d")
  
  OR_CI = rbind(OR_CI_outcome_gene_result, 
                OR_CI_outcome_discrim_result, 
                OR_CI_outcome_gene_interaction_result)
  
  print(OR_CI)
  
  print("done 11e")
  
  results_all = cbind(results_interaction, OR_CI)
  results_all = as.data.frame(results_all)

  print("done 11f")
  

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
                                    results_all)


  colnames(Interaction_findings) = c("analysis_variable_name",
                                     "dataset",
                                     "wave",
                                     "outcome_name",
                                     "N_subset",
                                     "N_discrimYES",
                                     "Estimate",
                                     "SE",
                                     "t value",
                                     "p_value",
                                     "OR",
                                     "CI1",
                                     "CI2")

  write.csv(Interaction_findings, file = paste(OUTPUT_ROOT, folder,  "findings.csv", sep=""))
  

 Estimate_rounded = round(Interaction_findings$Estimate, 2)
 SE_rounded = round(Interaction_findings$SE, 2)
 OR_rounded = round(Interaction_findings$OR, 2) 
 CI95_edited = paste("[", round(Interaction_findings$CI1, 2), ";", round(Interaction_findings$CI2, 2), "]", sep = "")
 p_value_rounded = round(Interaction_findings$p_value, 4)
 
 result_table_edited = cbind(Interaction_findings, 
                             Estimate_rounded, 
                             SE_rounded, 
                             OR_rounded, 
                             CI95_edited, 
                             p_value_rounded) 
                             
 
 result_table_edited = data.frame(result_table_edited)
 
 
 
 
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
