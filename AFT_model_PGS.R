AFT_model_PGS = function(data_AFT_input,
                         baseline_discriminaition,
                         outcome, 
                         PGS,
                         distribution_AFT, 
                         OUTPUT_ROOT,
                         analysis_name,
                         plot_name, 
                         alternative_cov){
  
  #dist: (ie, exponential, Weibull, lognormal, and gamma), run all and report the one with the best AIC
  folder <- paste(OUTPUT_ROOT, analysis_name, "/", sep = "")
  
  if (file.exists(folder)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(folder)
    
  }
  
  data_AFT_input$PGS = data_AFT_input[ ,   PGS]
  
  print("safer way: Surv(time, outcome) ~ age + discrim, anc = list(shape = ~ sex + discrim) SCALE BMI AND OTHER alternative_covs also delete code for dataframe outputs extract data approp for AFT, check plts")
  # scale(age) and scale(wealth) covariates have to be scaled and it should be gamma dstribution because we are looking at rate  )
  data_AFT_input$baseline_discriminaition = data_AFT_input[ ,   baseline_discriminaition]
  
  #Univariate_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ 1 + baseline_discriminaition,  dist = distribution_AFT, data = data_AFT_input))
  M_uni_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition, dist = distribution_AFT, data = data_AFT_input)
  #Univariate_outcome_gene_interaction = summary(M_uni_interaction)
  print(M_uni_interaction)
  print("M_uni_interaction done")
  
  #data_AFT_input = data.frame(_data_with_PGS$scale(age))
  #from literature: Model 1: scale(age), race, education, family income, marital status
  #Model_1_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  dist = distribution_AFT, data = data_AFT_input))
  #Model_1_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth),  dist = distribution_AFT, data = data_AFT_input))
  
  # plot interaction: https://stats.stackexchange.com/questions/464700/representing-interaction-plot-for-flexsurvreg-model-using-plot-model-in-r
  M_1_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT,  data = data_AFT_input)
  #Model_1_outcome_gene_interaction = summary(M_1_interaction)
  
  print("M_1_interaction done")
  
  
  #Model 2: 
  #+ alcohol use, smoking status, moderate and vagarious exercise	
  
  #Model_2_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
  #Model_2_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity, dist = distribution_AFT, data = data_AFT_input))
  
  M_2_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  dist = distribution_AFT, data = data_AFT_input)
  
  #Model_2_outcome_gene_interaction = summary( M_2_interaction)
  print(M_2_interaction)
  
  
  print("M_2_interaction done")
  

  ########## alcohol: 
  #Model_2a_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + alcohol + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
 # Model_2a_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol, dist = distribution_AFT, data = data_AFT_input))
  
  M_2a_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input)
  
  print(M_2a_interaction)
  
  print("M_2a_interaction done")
  
  #Model_2a_outcome_gene_interaction = summary( M_2a_interaction)
  
  
  
  #scale(smoking) + scale(physical_activity) +
  #Model_2s_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + smoking + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
  #Model_2s_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + smoking, dist = distribution_AFT, data = data_AFT_input))
  
  #M_2s_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + scale(smoking) +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input)
  
  #print(M_2s_interaction)
  
  
  #print("M_2s_interaction done")
  
  #Model_2s_outcome_gene_interaction = summary( M_2s_interaction)
  
  
  ########### 
  
  
  #scale(smoking) + scale(physical_activity) +
  #Model_2pa_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
 # Model_2pa_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + physical_activity, dist = distribution_AFT, data = data_AFT_input))
  
  M_2pa_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  dist = distribution_AFT, data = data_AFT_input)
  
  print(M_2pa_interaction)
  print("M_2pa_interaction done")
  
  #Model_2pa_outcome_gene_interaction = summary( M_2pa_interaction)
  
  
  #Model 3: 
  #+ BMI, (height separately) systolic blood pressure, antihypertensive medication, Diabetes/fasting blood glucose status, total cholesterol, high-density lipoprotein cholesterol, low-density lipoprotein cholesterol, triglycerides, and use of lipid lowering medication, history of diabetes and hypertension. 	
 # Model_3_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) +  data_AFT_input[ ,   alternative_cov] + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
 # Model_3_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + data_AFT_input[ ,   alternative_cov], dist = distribution_AFT, data = data_AFT_input))
  
  M_3_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + data_AFT_input[ ,   alternative_cov] + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10,  dist = distribution_AFT, data = data_AFT_input)
  
  print(M_3_interaction)
  
  print("M_3_interaction done")
  
 # Model_3_outcome_gene_interaction = summary( M_3_interaction)
  

  
  #Model 4: 
  #+  depressive symptoms and chronic stress burden/ stressful events
  
  #Model_4_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
 # Model_4_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth)  + depression, dist = distribution_AFT, data = data_AFT_input))
  M_4_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input)
  #Model_4_outcome_gene_interaction = summary( M_4_interaction)
  
  print(M_4_interaction)
  
  print("M_4_interaction done")
  
  
  
  #################
  
  #Model 5: all 
  
  #Model_5_outcome_gene = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity + data_AFT_input[ ,   alternative_cov] + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input))
 # Model_5_outcome_discrim = summary( flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity + data_AFT_input[ ,   alternative_cov] + depression, dist = distribution_AFT, data = data_AFT_input))
  
  M_5_interaction = flexsurvreg( Surv(follow_up, data_AFT_input[ ,   outcome]) ~ PGS*baseline_discriminaition + scale(age) + sex + scale(wealth) + alcohol + smoking + physical_activity + data_AFT_input[ ,   alternative_cov] + depression +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, dist = distribution_AFT, data = data_AFT_input)
  print(M_5_interaction)
  
  
  print("M_5_interaction done")
  
 # Model_5_outcome_gene_interaction = summary( M_5_interaction)
  
  
  ####### plots 
  plot_M_uni_int =  plot(M_uni_interaction, ylim = c(0.90, 1))
  plot_M_1_int = plot(M_1_interaction, ylim = c(0.90, 1))
  
  plot_M_2_int = plot(M_2_interaction, ylim = c(0.90, 1))
  
  #plot(M_2a_interaction)
  #plot(M_2pa_interaction)
  plot_M_3_int = plot(M_3_interaction, ylim = c(0.90, 1))
  plot_M_4_int = plot(M_4_interaction, ylim = c(0.90, 1))
  plot_M_5_int = plot(M_5_interaction, ylim = c(0.90, 1))
  
  plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int))
  
  # plot_M_uni_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Univariate", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_uni_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # plot_M_1_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Model 1", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_1_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # plot_M_2_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Model 2", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_2_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # #alcohol
  # plot_M_2a_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Model 2 (alcohol)", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_2a_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # #smoking
  # # plot_M_2s_int = plot_model(#title(main = NULL), 
  # #   title = "", #title = paste(plot_name, "Model 2 (smoking)", sep =""),
  # #   axis.title = "Hazard Ratio",
  # #   show.values = TRUE, 
  # #   show.p = TRUE, 
  # #   M_2s_interaction, 
  # #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # #physical activity
  # plot_M_2pa_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Model 2 (physical activity)", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_2pa_interaction, 
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # plot_M_3_int = plot_model(#title(main = NULL), 
  #   title = "", #title = paste(plot_name, "Model 3", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_3_interaction, 
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # plot_M_4_int = plot_model(#title(main = NULL), 
  #   title = "",
  #   #title = paste(plot_name, "Model 4", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_4_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # plot_M_5_int = plot_model(#title(main = NULL), 
  #   title = "",
  #   
  #   #title = paste(plot_name, "Model 5", sep =""),
  #   axis.title = "Hazard Ratio",
  #   show.values = TRUE, 
  #   show.p = TRUE, 
  #   M_5_interaction,
  #   terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))
  # 
  # print("plots are done")
  # 
  # #all_models = plot_models(M_uni_interaction, M_1_interaction, M_2_interaction, M_3_interaction, M_4_interaction, M_5_interaction, grid = TRUE)
  # #plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int, tags = TRUE))
  # plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int), tags = TRUE)
  # 
  # print("plot grid is done")
  
  # merge plots with models heading 
  
  # print(plot_M_uni_int)
  # 
  #  jpeg('plot_M_uni_int.jpg')
  #  plot(plot_M_uni_int)
  #  dev.off()
  
  # save_plot(filename =  "plot_M_uni_int.tif",
  #                       fig = last_plot())
  
  
  # print(plot_M_1_int)
  # # 
  # # save_plot(filename =  paste(folder, "plot_M_1_int.tif", sep = ""),
  # #           fig = last_plot())
  # 
  # print(plot_M_2_int)
  # print(plot_M_2a_int)
  # print(plot_M_2s_int)
  # print(plot_M_2pa_int)
  # print(plot_M_3_int)
  # print(plot_M_4_int)
  # print(plot_M_5_int)
  
  #print(all_models)
  
  
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
  
  
  # print(plot_M_uni_int)
  # print(plot_M_1_int)
  # print(plot_M_2_int)
  # print(plot_M_2a_int)
  # print(plot_M_2s_int)
  # print(plot_M_2pa_int)
  # print(plot_M_3_int)
  # print(plot_M_4_int)
  # print(plot_M_5_int)
  
  
  
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
  
 # write.csv(output_results_table_edited, file = paste(folder, analysis_name, ".csv", sep = ""))
  
  
  return(params = output_results)
  
}
#plots: https://strengejacke.wordpress.com/2017/10/23/one-function-to-rule-them-all-visualization-of-regression-models-in-rstats-w-sjplot/

