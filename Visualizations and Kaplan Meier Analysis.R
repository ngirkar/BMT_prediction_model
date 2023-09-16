####plot for distribution of the deceased in the training dataset######
# Calculate total counts for each group
total_counts <- imputed_data_complete %>%
  group_by(Deceased) %>%
  summarize(total_count = n())

# Create a bar chart with total count labels directly over each bar
bar_chart <- ggplot(imputed_data_complete, aes(x = factor(Deceased))) +
  geom_bar(fill = "lightblue") +
  geom_text(data = total_counts, aes(label = total_count,y = total_count), vjust = -0.5, size = 3) +  # Add total count labels over the bars
  labs(
    x = "Deceased",
    y = "Count") +
  theme_minimal()+
  theme(
    axis.line = element_line(color = "black")
  )


# Print the bar chart
print(bar_chart)


####plot for distribution of the deceased in the validation dataset######
# Calculate total counts for each group
total_counts1 <- imputed_vdata_complete %>%
  group_by(Deceased) %>%
  summarize(total_count1 = n())

# Create a bar chart with total count labels directly over each bar
bar_chart <- ggplot(imputed_vdata_complete, aes(x = factor(Deceased))) +
  geom_bar(fill = "lightblue") +
  geom_text(data = total_counts1, aes(label = total_count1,y = total_count1), vjust = -0.5, size = 3) +  # Add total count labels over the bars
  labs(
    x = "Deceased",
    y = "Count") +
  theme_minimal()+
  theme(
    axis.line = element_line(color = "black")
  )


# Print the bar chart
print(bar_chart)


##Kaplan-Meier survival analysis 

# Create a Kaplan-Meier survival object
s_object <- Surv(imputed_data_complete$Overall.Survival, imputed_data_complete$Deceased)

# Create Kaplan-Meier plot
km_plot <- survfit(s_object ~ 1, data = imputed_data_complete)
km_plot

plot1 <- ggsurvplot(km_plot,
                    data = imputed_data_complete,
                    conf.int = TRUE,
                    risk.table = ,
                    xlab = "Time (in Days)")
print(plot1)

##Validationdata
s_object2 <- Surv(imputed_vdata_complete$Overall.Survival, imputed_vdata_complete$Deceased)

# Create Kaplan-Meier plot
km_plot2 <- survfit(s_object2 ~ 1, data = imputed_vdata_complete)
km_plot2
plot2 <- ggsurvplot(km_plot2,
                    data = imputed_vdata_complete,
                    conf.int = TRUE,
                    risk.table = FALSE,
                    xlab = "Time (in Days)")
plot2


####plot describing the baseline hazard with different number of knots 

# Create a data frame for prediction
plot_data <- data.frame(Overall.Survival = seq(0, max(imputed_data_complete$Overall.Survival), length.out = 100))

# Create a function to generate hazard curves for a specific degree of freedom
generate_hazard_curve <- function(df) {
  fpmh <- stpm2(Surv(Overall.Survival, Deceased) ~ 1, data = imputed_data_complete, df = df)
  
  # Predict hazard values using the fitted model
  plot_data$hazard <- predict(fpmh, newdata = plot_data, type = "haz")
  
  # Return the plot data with df information
  plot_data$df <- as.factor(rep(df, nrow(plot_data)))
  plot_data
}

# Generate and combine hazard curves for degrees of freedom from 2 to 4
hazard_curves <- lapply(2:5, generate_hazard_curve)
combined_data <- do.call(rbind, hazard_curves)

# Create the plot using ggplot2
ggplot(combined_data, aes(x = Overall.Survival, y = hazard, color = df)) +
  geom_line(size = 1) +
  xlab("Time in (days)") +
  ylab("Hazard") +
  ggtitle("Number of knots for the Baseline Hazard") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        legend.position = c(0.95, 0.8),  # Adjust legend position (inside the box)
        legend.margin = margin(15, 20, 20, 20), 
        legend.key = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) # Add a black border around the plot  # Add a black border around the plot
#theme(panel.grid = element_blank(), legend.position = "right", legend.margin = margin(15, 20, 20, 20), legend.key = element_rect(fill = "white", color = "black"))
