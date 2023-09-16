##LOADING THE REQUIRED LIBRARIES###
library(Hmisc)
library(survival)


#####INTERNAL VALIDATION#########

#data sets that are censored at 5
temp <- survSplit(Surv(Overall.Survival,Deceased) ~ ., data = imputed_vdata_complete, cut = 500,
                  episode ="epoch")
temp
survival_at_500 <- subset(temp, epoch == 1)
survival_at_500

#######OE RATIO ########
#model3
##  Observed / Expected ratio at time t ------------
# Observed: 1-Kaplan Meier at time (t)
# Calculate observed event proportion at the specified time point


horizon <- 500
obj <- survfit(Surv(Overall.Survival,Deceased) ~ 1, 
               data = survival_at_500)
sobj <- summary(obj, times = horizon, extend=TRUE)

pred <- predict(fpmf3,newdata = survival_at_500,type ="surv", 
                times = horizon)
obj
pred
mean(pred)
p= 1-pred

OE <- (1-sobj$surv)/ mean(p)
OE

# Perform bootstrapping to estimate confidence interval
set.seed(123)  # Set a random seed for reproducibility
n_bootstraps <- 1000  # Number of bootstrap samples

bootstrap_OE <- numeric(n_bootstraps)

for (i in 1:n_bootstraps) {
  # Sample with replacement from the predicted survival probabilities
  bootstrap_sample <- sample(p, replace = TRUE)
  
  # Calculate O/E ratio for the bootstrap sample
  bootstrap_OE[i] <- (1 - sobj$surv) / mean(bootstrap_sample)
}

# Calculate the confidence interval
conf_interval <- quantile(bootstrap_OE, c(0.025, 0.975))

# Print the O/E ratio and confidence interval
cat("Observed-to-Expected (O/E) Ratio:", OE, "\n")
cat("95% Confidence Interval:", conf_interval[1], "-", conf_interval[2], "\n")

#####model 2 ####
#model2
horizon <- 500
obj1 <- summary(survfit(Surv(Overall.Survival,Deceased) ~ 1, 
                        data = survival_at_500), 
                times = horizon)

obj1
pred1 <- predict(fpmf2,newdata = survival_at_500,type ="surv", 
                 times = horizon)
pred1
p1 <- 1- pred1
p1
OE1 <- (1-obj1$surv) / mean(p1)
OE1
# Perform bootstrapping to estimate confidence interval
set.seed(123)  # Set a random seed for reproducibility
n1_bootstraps <- 1000  # Number of bootstrap samples

bootstrap_OE1 <- numeric(n1_bootstraps)

for (i in 1:n1_bootstraps) {
  # Sample with replacement from the predicted survival probabilities
  bootstrap_sample1 <- sample(p1, replace = TRUE)
  
  # Calculate O/E ratio for the bootstrap sample
  bootstrap_OE1[i] <- (1 - obj1$surv) / mean(bootstrap_sample1)
}

# Calculate the confidence interval
conf_interval1 <- quantile(bootstrap_OE1, c(0.025, 0.975))

# Print the O/E ratio and confidence interval
cat("Observed-to-Expected (O/E) Ratio:", OE1, "\n")
cat("95% Confidence Interval:", conf_interval1[1], "-", conf_interval1[2], "\n")

####model 
horizon <-500
obj2 <- summary(survfit(Surv(Overall.Survival,Deceased) ~ 1, 
                        data = survival_at_500), 
                times = horizon)

pred2 <- predict(fpmf1,newdata = survival_at_500,type ="surv", 
                 times = horizon)
p2 <- 1-pred2
OE2 <- (1-obj2$surv) / mean(p2)
OE2
# Perform bootstrapping to estimate confidence interval
set.seed(123)  # Set a random seed for reproducibility
n2_bootstraps <- 1000  # Number of bootstrap samples

bootstrap_OE2 <- numeric(n2_bootstraps)

for (i in 1:n2_bootstraps) {
  # Sample with replacement from the predicted survival probabilities
  bootstrap_sample2 <- sample(p2, replace = TRUE)
  
  # Calculate O/E ratio for the bootstrap sample
  bootstrap_OE2[i] <- (1 - obj2$surv) / mean(bootstrap_sample2)
}

# Calculate the confidence interval
conf_interval2 <- quantile(bootstrap_OE2, c(0.025, 0.975))

# Print the O/E ratio and confidence interval
cat("Observed-to-Expected (O/E) Ratio:", OE2, "\n")
cat("95% Confidence Interval:", conf_interval2[1], "-", conf_interval2[2], "\n")


########CALIBRATION SLOPE

####Model 1######
prediction_timepoint <- 500

survival_at_500$spred1 <- 1-predict(
  fpmf1, 
  newdata = survival_at_500, 
  type = "surv", 
  times = prediction_timepoint
)
survival_at_500$lp1 <- log(-log(1 - survival_at_500$spred1))

gval1 <- coxph(Surv(Overall.Survival, Deceased) ~ lp1, data = survival_at_500)
gval1
# Calculate the confidence interval for the coefficient of gval1
conf_interval <- confint(gval1)

# Print the confidence interval
print(conf_interval)

###model 2 ####

prediction_timepoint <- 500

survival_at_500$spred2 <-1-predict(
  fpmf2, 
  newdata = survival_at_500, 
  type = "surv", 
  times = prediction_timepoint
)
survival_at_500$lp2 <- log(-log(1 - survival_at_500$spred2))

gval3 <- coxph(Surv(Overall.Survival, Deceased) ~ lp2, data = survival_at_500)
gval3

# Calculate the confidence interval for the coefficient of gval1
conf_interval2 <- confint(gval3)

# Print the confidence interval
print(conf_interval2)


##model 3###
survival_at_500$spred <- 1-predict(
  fpmf3, 
  newdata = survival_at_500, 
  type = "surv",
)

#survival_probabilities
survival_at_500$lp <- log(-log(1 - survival_at_500$spred))

gval <- coxph(Surv(Overall.Survival, Deceased) ~ lp, data = survival_at_500)
gval

# Calculate the confidence interval for the coefficient of gval1
conf_interval3 <- confint(gval)

# Print the confidence interval
print(conf_interval3)


####DISCRIMINATION#####

predm1 <- predict(fpmf1, newdata = survival_at_500)### if not working add type ="surv"
Harrells_c_statistic  <- concordance(Surv(Overall.Survival, Deceased) ~ predm1, survival_at_500, reverse = TRUE)
Harrells_c_statistic


predm2 <- predict(fpmf2, newdata = survival_at_500)### if not working add type ="surv"
Harrells_c_statistic  <- concordance(Surv(Overall.Survival, Deceased) ~ predm2, survival_at_500, reverse = TRUE)
Harrells_c_statistic

predm3 <- predict(fpmf3, newdata = survival_at_500)### if not working add type ="surv"
Harrells_c_statistic  <- concordance(Surv(Overall.Survival, Deceased) ~ predm3, survival_at_500, reverse = TRUE)
Harrells_c_statistic

