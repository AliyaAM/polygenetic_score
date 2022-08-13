data_both_countries = na.omit(data_both_countries)


# plotting discrimination against wealth and age 

plot_wealth = ggplot(data_both_countries, aes(x = wealth, y = discrimination)) +
  #geom_point(alpha = 0.2) +
  geom_smooth(aes(colour = country_cat), method = "glm", method.args = list(family = "binomial"), fullrange = TRUE) +
  scale_colour_discrete(name="country",
                        breaks = c(0, 1), 
                        labels=c("United States", "England")) + 
  labs(
    title = analysis_variable_name, 
    x = "wealth excluding pension, USD",
    y = "Probability of perceived discrimination"
  )+
  scale_x_continuous(labels = comma, limits = c(-500000, 500000))+ 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits = c(0, 1)) + 
  
  theme(text = element_text(size = 20), legend.justification=c(1,1), legend.position=c(1,1))

plot_age = ggplot(data_both_countries, aes(age, discrimination)) +
  #geom_point(alpha = 0.2) +
  geom_smooth(aes(colour = country_cat), method = "glm", method.args = list(family = "binomial"), fullrange = TRUE) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits = c(0, 1)) + 
  
  scale_colour_discrete(name="country",
                        breaks = c(0, 1), 
                        labels=c("United States", "England")) + 
  labs(
    title = analysis_variable_name, 
    x = "age, years",
    y = "Probability of perceived discrimination"
  )+
  theme(text = element_text(size = 20), legend.justification=c(1,1), legend.position=c(1,1))

