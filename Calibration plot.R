#######CALIBRATION PLOT######

time_horizon = 500
pred5 <- 1-predict(
  fpmf3, 
  newdata = survival_at_500, 
  type = "surv", 
  times = time_horizon
)

pred5

# Create bins for predicted probabilities in the validation data
survival_at_500$predicted_bin <- cut(pred5, quantile(pred5, probs = seq(0.2, 0.8, by = 0.1)))


mean_predicted_prob_summary <- aggregate(pred5 ~ predicted_bin,
                                         data = survival_at_500,
                                         FUN = function(x) mean(x))
mean_observed_risk_summary <- aggregate(Deceased ~ predicted_bin,
                                        data = survival_at_500,
                                        FUN = function(x) mean(x))

# Combine the two summaries
calibration_summary <- merge(mean_predicted_prob_summary, mean_observed_risk_summary, by = "predicted_bin")

# Rename columns for clarity
colnames(calibration_summary) <- c("predicted_bin", "mean_predicted_prob", "mean_observed_risk")

# Print or further analyze the calibration_summary DataFrame
print(calibration_summary)
# Order the bins by the mean predicted probability within each bin
calibration_summary <- calibration_summary %>%
  arrange(mean_predicted_prob)
print(calibration_summary)


# Plot calibration curve with ideal line starting from (0,0)
calibration_plot <- ggplot(calibration_summary, aes(x = mean_predicted_prob, y = mean_observed_risk)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Mean Predicted Probability", y = "Observed Event Rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0, 1) +  # Set x-axis limits
  ylim(0, 1) +
  coord_cartesian(xlim = c(0, 0.8), ylim = c(0, 0.8))# Set y-axis limits

# Set the title using ggtitle and center it
calibration_plot <- calibration_plot + ggtitle("Calibration Curve")

# Print the plot
print(calibration_plot)

###BASELINE HAZARD TABLE AND PLOT########

# Create a copy of the original dataset
zero_covariates <- imputed_data_complete

# Set the values of all covariates to zero for the hypothetical scenario
zero_covariates$Age <- 0
zero_covariates$HCT.CI.Score <- 0
zero_covariates$V.VCO2 <- 0

# Extract the survival times for all observations
survival_times <- zero_covariates$Overall.Survival

# Predict the cumulative hazard at the survival times for all observations
predicted_risk <- predict(fpmf3, newdata = zero_covariates, times = survival_times, type = "cumhaz")

# Create a data frame to store the results
result_table <- data.frame(time = survival_times, hazard = predicted_risk)

# Print the result table
print(result_table)


# Identify the indices of the first occurrence of each unique time
first_occurrence_indices <- !duplicated(result_table$time)

# Filter the result_table to include only the first occurrence of each time
filtered_result_table <- result_table[first_occurrence_indices, ]

# Print the filtered result table
print(filtered_result_table)




###create visualization of the baseline hazard using zero covariate 
# Extract the survival times from the events_data dataset
survival_times_events <- zero_covariates$Overall.Survival

# Predict the cumulative hazard at the extracted survival times using the survival model
predicted_risk_events <- predict(fpmf3, newdata = zero_covariates, times = survival_times_events, type = "haz")

# Create a data frame to store the results
result_table_events <- data.frame(time = survival_times_events, hazard = predicted_risk_events)

# Print the result table for the events_data
print(result_table_events)

# Create a ggplot object for the predicted cumulative hazard
ggplot(result_table_events, aes(x = time, y = hazard)) +
  geom_line(color = "blue", size = 0.7) +
  labs(x = "Survival Time (Days)", y = "Baseline Hazard", 
       title = "Baseline Hazard Plot") +
  scale_x_continuous(breaks = seq(0, max(result_table_events$time), by = 100)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")



###using PredRupdate create calibration plot for model 3 

library(predRupdate)

# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Age" = 0.039864,
                          "HCT.CI.Score" = 0.286153,
                          "V.VCO2" = 0.022697)



#pass this into pred_input_info()
Existing_TTE_Model <- pred_input_info(model_type = "survival",
                                      model_info = coefs_table,
                                      cum_hazard = filtered_result_table)
summary(Existing_TTE_Model)

pred_predict(Existing_TTE_Model, 
             new_data = imputed_vdata_complete,
             survival_time = "Overall.Survival",
             event_indicator = "Deceased",
             time_horizon = 499)

#where BH_table is the baseline hazard above

#now validate against the time-to-event outcomes in the new dataset:
validation_results <- pred_validate(x = Existing_TTE_Model,
                                    new_data = imputed_vdata_complete,
                                    survival_time = "Overall.Survival",
                                    event_indicator = "Deceased",
                                    time_horizon = 499)

summary(validation_results)

ivalidation_results <- pred_validate(x = Existing_TTE_Model,
                                     new_data = imputed_data_complete,
                                     survival_time = "Overall.Survival",
                                     event_indicator = "Deceased",
                                     time_horizon = 499)
