cox_model_PGS = function(data_cox_input,  baseline_discriminaition, outcome, PGS, OUTPUT_ROOT, analysis_name, plot_name, alternative_cov){
  
  folder <- paste(OUTPUT_ROOT, analysis_name, "/", sep = "")
  
  if (file.exists(folder)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(folder)
    
  }

  data_cox_input$PGS = data_cox_input[ ,   PGS]
  
  data_cox_input$baseline_discriminaition = data_cox_input[ ,   baseline_discriminaition]

Univariate_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition, data_cox_input))
M_uni_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition, data_cox_input)
Univariate_outcome_gene_interaction = summary(M_uni_interaction)


Univariate_outcome_discrim_results = cbind(Univariate_outcome_discrim$coefficients, Univariate_outcome_discrim$conf.int)
head(Univariate_outcome_discrim_results, 1)
Univariate_outcome_gene_interaction_results = cbind(Univariate_outcome_gene_interaction$coefficients, Univariate_outcome_gene_interaction$conf.int)
tail(Univariate_outcome_gene_interaction_results, 1)

#data_cox_input = data.frame(_data_with_PGS$age)
#from literature: Model 1: Age, race, education, family income, marital status
Model_1_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_1_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth, data_cox_input))

# plot interaction: https://stats.stackexchange.com/questions/464700/representing-interaction-plot-for-coxph-model-using-plot-model-in-r
M_1_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)
Model_1_outcome_gene_interaction = summary(M_1_interaction)

Model_1_outcome_gene_results = cbind(Model_1_outcome_gene$coefficients, Model_1_outcome_gene$conf.int)
head(Model_1_outcome_gene_results, 1)
Model_1_outcome_discrim_results = cbind(Model_1_outcome_discrim$coefficients, Model_1_outcome_discrim$conf.int)
head(Model_1_outcome_discrim_results, 1)
Model_1_outcome_gene_interaction_results = cbind(Model_1_outcome_gene_interaction$coefficients, Model_1_outcome_gene_interaction$conf.int)
tail(Model_1_outcome_gene_interaction_results, 1)

#Model 2: 
#+ alcohol use, smoking status, moderate and vagarious exercise	

Model_2_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_2_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity, data_cox_input))

M_2_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_2_outcome_gene_interaction = summary( M_2_interaction)

Model_2_outcome_gene_results = cbind(Model_2_outcome_gene$coefficients, Model_2_outcome_gene$conf.int)
head(Model_2_outcome_gene_results, 1)
Model_2_outcome_discrim_results = cbind(Model_2_outcome_discrim$coefficients, Model_2_outcome_discrim$conf.int)
head(Model_2_outcome_discrim_results, 1)
Model_2_outcome_gene_interaction_results = cbind(Model_2_outcome_gene_interaction$coefficients, Model_2_outcome_gene_interaction$conf.int)
tail(Model_2_outcome_gene_interaction_results, 1)


########## alcohol: 
Model_2a_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_2a_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol, data_cox_input))

M_2a_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_2a_outcome_gene_interaction = summary( M_2a_interaction)

Model_2a_outcome_gene_results = cbind(Model_2a_outcome_gene$coefficients, Model_2a_outcome_gene$conf.int)
head(Model_2a_outcome_gene_results, 1)
Model_2a_outcome_discrim_results = cbind(Model_2a_outcome_discrim$coefficients, Model_2a_outcome_discrim$conf.int)
head(Model_2a_outcome_discrim_results, 1)
Model_2a_outcome_gene_interaction_results = cbind(Model_2a_outcome_gene_interaction$coefficients, Model_2a_outcome_gene_interaction$conf.int)
tail(Model_2a_outcome_gene_interaction_results, 1)



#scale(smoking) + scale(physical_activity) +
Model_2s_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + smoking + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_2s_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + smoking, data_cox_input))

M_2s_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + smoking +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_2s_outcome_gene_interaction = summary( M_2s_interaction)

Model_2s_outcome_gene_results = cbind(Model_2s_outcome_gene$coefficients, Model_2s_outcome_gene$conf.int)
head(Model_2s_outcome_gene_results, 1)
Model_2s_outcome_discrim_results = cbind(Model_2s_outcome_discrim$coefficients, Model_2s_outcome_discrim$conf.int)
head(Model_2s_outcome_discrim_results, 1)
Model_2s_outcome_gene_interaction_results = cbind(Model_2s_outcome_gene_interaction$coefficients, Model_2s_outcome_gene_interaction$conf.int)
tail(Model_2s_outcome_gene_interaction_results, 1)


########### 


#scale(smoking) + scale(physical_activity) +
Model_2pa_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + physical_activity + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_2pa_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + physical_activity, data_cox_input))

M_2pa_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + physical_activity +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_2pa_outcome_gene_interaction = summary( M_2pa_interaction)

Model_2pa_outcome_gene_results = cbind(Model_2pa_outcome_gene$coefficients, Model_2pa_outcome_gene$conf.int)
head(Model_2pa_outcome_gene_results, 1)
Model_2pa_outcome_discrim_results = cbind(Model_2pa_outcome_discrim$coefficients, Model_2pa_outcome_discrim$conf.int)
head(Model_2pa_outcome_discrim_results, 1)
Model_2pa_outcome_gene_interaction_results = cbind(Model_2pa_outcome_gene_interaction$coefficients, Model_2pa_outcome_gene_interaction$conf.int)
tail(Model_2pa_outcome_gene_interaction_results, 1)


#Model 3: 
#+ BMI, (height separately) systolic blood pressure, antihypertensive medication, Diabetes/fasting blood glucose status, total cholesterol, high-density lipoprotein cholesterol, low-density lipoprotein cholesterol, triglycerides, and use of lipid lowering medication, history of diabetes and hypertension. 	
Model_3_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth +  data_cox_input[ ,   alternative_cov] + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_3_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + data_cox_input[ ,   alternative_cov], data_cox_input))

M_3_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + data_cox_input[ ,   alternative_cov] + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_3_outcome_gene_interaction = summary( M_3_interaction)


Model_3_outcome_gene_results = cbind(Model_3_outcome_gene$coefficients, Model_3_outcome_gene$conf.int)
head(Model_3_outcome_gene_results, 1)
Model_3_outcome_discrim_results = cbind(Model_3_outcome_discrim$coefficients, Model_3_outcome_discrim$conf.int)
head(Model_3_outcome_discrim_results, 1)
Model_3_outcome_gene_interaction_results = cbind(Model_3_outcome_gene_interaction$coefficients, Model_3_outcome_gene_interaction$conf.int)
tail(Model_3_outcome_gene_interaction_results, 1)

#Model 4: 
#+  depressive symptoms and chronic stress burden/ stressful events

Model_4_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_4_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth  + depression, data_cox_input))
M_4_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data = data_cox_input)
Model_4_outcome_gene_interaction = summary( M_4_interaction)


Model_4_outcome_gene_results = cbind(Model_4_outcome_gene$coefficients, Model_4_outcome_gene$conf.int)
head(Model_4_outcome_gene_results, 1)
Model_4_outcome_discrim_results = cbind(Model_4_outcome_discrim$coefficients, Model_4_outcome_discrim$conf.int)
head(Model_4_outcome_discrim_results, 1)
Model_4_outcome_gene_interaction_results = cbind(Model_4_outcome_gene_interaction$coefficients, Model_4_outcome_gene_interaction$conf.int)
tail(Model_4_outcome_gene_interaction_results, 1)


#################

#Model 5: all 

Model_5_outcome_gene = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS + age + sex + wealth + alcohol + smoking + physical_activity + data_cox_input[ ,   alternative_cov] + depression + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input))
Model_5_outcome_discrim = summary( coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + data_cox_input[ ,   alternative_cov] + depression, data_cox_input))

M_5_interaction = coxph( Surv(follow_up, data_cox_input[ ,   outcome]) ~ PGS*baseline_discriminaition + age + sex + wealth + alcohol + smoking + physical_activity + data_cox_input[ ,   alternative_cov] + depression +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, data_cox_input)

Model_5_outcome_gene_interaction = summary( M_5_interaction)

Model_5_outcome_gene_results = cbind(Model_5_outcome_gene$coefficients, Model_5_outcome_gene$conf.int)
head(Model_5_outcome_gene_results, 1)
Model_5_outcome_discrim_results = cbind(Model_5_outcome_discrim$coefficients, Model_5_outcome_discrim$conf.int)
head(Model_5_outcome_discrim_results, 1)
Model_5_outcome_gene_interaction_results = cbind(Model_5_outcome_gene_interaction$coefficients, Model_5_outcome_gene_interaction$conf.int)
tail(Model_5_outcome_gene_interaction_results, 1)

output_1 = head(Univariate_outcome_discrim_results, 1)
output_2 = tail(Univariate_outcome_gene_interaction_results, 1)

output_3 = head(Model_1_outcome_gene_results, 1)
output_4 = head(Model_1_outcome_discrim_results, 1)
output_5 = tail(Model_1_outcome_gene_interaction_results, 1)

output_6 = head(Model_2_outcome_gene_results, 1)
output_7 = head(Model_2_outcome_discrim_results, 1)
output_8 = tail(Model_2_outcome_gene_interaction_results, 1)

output_9 = head(Model_2a_outcome_gene_results, 1)
output_10 = head(Model_2a_outcome_discrim_results, 1)
output_11 = tail(Model_2a_outcome_gene_interaction_results, 1)


output_12 = head(Model_2s_outcome_gene_results, 1)
output_13 = head(Model_2s_outcome_discrim_results, 1)
output_14 = tail(Model_2s_outcome_gene_interaction_results, 1)


output_15 = head(Model_2pa_outcome_gene_results, 1)
output_16 = head(Model_2pa_outcome_discrim_results, 1)
output_17 = tail(Model_2pa_outcome_gene_interaction_results, 1)

output_18 = head(Model_3_outcome_gene_results, 1)
output_19 = head(Model_3_outcome_discrim_results, 1)
output_20 = tail(Model_3_outcome_gene_interaction_results, 1)

output_21 = head(Model_4_outcome_gene_results, 1)
output_22 = head(Model_4_outcome_discrim_results, 1)
output_23 = tail(Model_4_outcome_gene_interaction_results, 1)

output_24 = head(Model_4_outcome_gene_results, 1)
output_25 = head(Model_4_outcome_discrim_results, 1)
output_26 = tail(Model_4_outcome_gene_interaction_results, 1)

model = c("Univariate", 
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
                output_14,
                
                output_15,
                output_16,
                output_17,
                
                output_18,
                output_19,
                output_20,
                
                output_21,
                output_22,
                output_23,
                
                output_24,
                output_25,
                output_26) 

table = cbind(model, results)

table = as.data.frame(table)

output_result = cbind(table[1:3], table[6], table[9], table[10])
#ties = "efron"

####### plots 

plot_M_uni_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Univariate", sep =""),
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_uni_interaction,
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_1_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 1", sep =""),
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_1_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_2_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 2", sep =""),
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_2_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#alcohol
plot_M_2a_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 2 (alcohol)", sep =""),
                           axis.title = "Hazard Ratio",
                           show.values = TRUE, 
                           show.p = TRUE, 
                           M_2a_interaction,
                           terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#smoking
plot_M_2s_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 2 (smoking)", sep =""),
                           axis.title = "Hazard Ratio",
                           show.values = TRUE, 
                           show.p = TRUE, 
                           M_2s_interaction, 
                           terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#physical activity
plot_M_2pa_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 2 (physical activity)", sep =""),
                            axis.title = "Hazard Ratio",
                            show.values = TRUE, 
                            show.p = TRUE, 
                            M_2pa_interaction, 
                            terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_3_int = plot_model(#title(main = NULL), 
  title = "", #title = paste(plot_name, "Model 3", sep =""),
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_3_interaction, 
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_4_int = plot_model(#title(main = NULL), 
  title = "",
  #title = paste(plot_name, "Model 4", sep =""),
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_4_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

plot_M_5_int = plot_model(#title(main = NULL), 
  title = "",
                        
  #title = paste(plot_name, "Model 5", sep =""),
                          axis.title = "Hazard Ratio",
                          show.values = TRUE, 
                          show.p = TRUE, 
                          M_5_interaction,
                          terms = c("PGS", "baseline_discriminaition", "PGS:baseline_discriminaition"))

#all_models = plot_models(M_uni_interaction, M_1_interaction, M_2_interaction, M_3_interaction, M_4_interaction, M_5_interaction, grid = TRUE)
#plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int, tags = TRUE))
plot_grid(list(plot_M_uni_int, plot_M_1_int, plot_M_2_int, plot_M_3_int, plot_M_4_int, plot_M_5_int), tags = TRUE)


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

write.csv(output_results_table_edited, file = paste(folder, analysis_name, ".csv", sep = ""))


return(params = output_results)

}
#plots: https://strengejacke.wordpress.com/2017/10/23/one-function-to-rule-them-all-visualization-of-regression-models-in-rstats-w-sjplot/

