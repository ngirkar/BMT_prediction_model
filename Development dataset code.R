#####DEVELOPMENT DATASET MODEL FITTING ####

library(rstpm2)

###no.of knots for the baseline hazard####
## utility function to row bind from a list
Rbind <- function(object) do.call(rbind,object)
out <- lapply(2:6, function(i) {
  fitaic <- stpm2(Surv(Overall.Survival, Deceased) ~ 1, data=imputed_data_complete, df=i)
  data.frame(
    i,
    AIC=AIC(fitaic),
    BIC=BIC(fitaic),
    beta=as.numeric(coef(fitaic)[2]),
    se=coef(summary(fitaic))[2,2])
})
out %>% Rbind


#####fitting models on the development dataset######
####1.Model 1 ###
fpmf1<- stpm2(Surv(Overall.Survival, Deceased) ~ HCT.CI.Score,
              data=imputed_data_complete, df=2)
summary(fpmf1)
eform(fpmf1)[2,]

###2.Model 2 #####

fpmf2<- stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score,
              data=imputed_data_complete, df=2)
summary(fpmf2)
eform(fpmf2)[2:3,]


####3. Model 3######
fpmf3 <-stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + V.VCO2,
              data=imputed_data_complete, df=2)
summary(fpmf3)
eform(fpmf3)[2:4,]
### Model 4####
fpmf4 <-stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + V.VO2,
              data=imputed_data_complete, df=2)
summary(fpmf4)
eform(fpmf4)[2:4,]

#### Model 5#####
fpmf5<-stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + VO2.Peak,
             data=imputed_data_complete, df=2)
summary(fpmf5)
eform(fpmf5)[2:4,]

#####Model 6#####
fpmf6 <-stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + AT,
              data=imputed_data_complete, df=2)
summary(fpmf6)
eform(fpmf6)[2:4,]


####Likelihood ratio test comparng model 2 and all the other models
anova(fpmf2, fpmf3)
anova(fpmf2, fpmf4)
anova(fpmf2, fpmf5)
anova(fpmf2, fpmf6)


######The finalised model 3 is ######
fpmf3 <-stpm2(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + V.VCO2,
              data=imputed_data_complete, df=2)
summary(fpmf3)
eform(fpmf3)[2:4,]

####checking for proportional hazards and non- linearity#####
fit1 <- coxph(Surv(Overall.Survival, Deceased) ~ HCT.CI.Score, data = imputed_data_complete)
t <-cox.zph(fit1)
t
plot(t)
abline(h = 0, lty = 2, col = "red") 

fit2 <- coxph(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score, data = imputed_data_complete)
t2<-cox.zph(fit2)
t2
# Set up the plotting layout
oldpar <- par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
for (i in 1:2) {
  plot(t2[i], resid = T)
  abline(0, 0, lty = 3, col ="red")
}
par(oldpar)

fit3 <- coxph(Surv(Overall.Survival, Deceased) ~ Age + HCT.CI.Score + V.VCO2, data = imputed_data_complete)
t3<-cox.zph(fit3)
t3
# Set up the plotting layout
oldpar <- par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
for (i in 1:3) {
  plot(t3[i], resid = T)
  abline(0, 0, lty = 3, col ="red")
}
par(oldpar)

###Checking non-linearity for continuous predictors in the model ###
# Residuals vs. continuous predictor
X <- imputed_data_complete$Age
Y <- resid(fit3, type = "martingale")
plot(X, Y, pch = 20, col = "darkgray",
     xlab = "Age", ylab = "Martingale Residual",
     main = "Residuals vs. Predictor")
abline(h = 0)
lines(smooth.spline(X, Y, df = 2), lty = 2, lwd = 2)


X <- imputed_data_complete$HCT.CI.Score
Y <- resid(fit3, type = "martingale")
plot(X, Y, pch = 20, col = "darkgray",
     xlab = "HCT.CI.Score", ylab = "Martingale Residual",
     main = "Residuals vs. Predictor")
abline(h = 0)
lines(smooth.spline(X, Y, df = 2), lty = 2, lwd = 2)

X <- imputed_data_complete$V.VCO2
Y <- resid(fit3, type = "martingale")
plot(X, Y, pch = 20, col = "darkgray",
     xlab = "V.VCO2", ylab = "Martingale Residual",
     main = "Residuals vs. Predictor")
abline(h = 0)
lines(smooth.spline(X, Y, df = 2), lty = 2, lwd = 2)

# Fit model with quadratic
fit2 <- coxph(Surv(Overall.Survival, Deceased) ~
                Age + HCT.CI.Score + I(HCT.CI.Score^2) + V.VCO2,
              data = imputed_data_complete)

# Re-check linearity assumption
X  <- imputed_data_complete$HCT.CI.Score
X2 <- imputed_data_complete$HCT.CI.Score^2
Y <- resid(fit2, type = "martingale")

par(mfrow=c(1,2))
plot(X, Y, pch = 20, col = "darkgray",
     xlab = "HCT.CI.Score", ylab = "Martingale Residual",
     main = "Residuals vs. Linear Term\n(smoother df = 2)",
     cex.main = 0.90)
abline(h = 0)
lines(smooth.spline(X, Y, df = 2), lty = 2, lwd = 2)

plot(X2, Y, pch = 20, col = "darkgray",
     xlab = "HCT.CI.Score^2", ylab = "Martingale Residual",
     main = "Residuals vs. Quadratic Term\n(smoother df = 3)",
     cex.main = 0.90)
abline(h = 0)
lines(smooth.spline(X2, Y, df = 2), lty = 2, lwd = 2)


# Fit model with quadratic
fit3 <- coxph(Surv(Overall.Survival, Deceased) ~
                log(Age) + HCT.CI.Score + V.VCO2,
              data = imputed_data_complete)

# Re-check linearity assumption
X  <- imputed_data_complete$Age
X2 <- log(imputed_data_complete$Age)
Y <- resid(fit3, type = "martingale")

par(mfrow=c(1,2))
plot(X, Y, pch = 20, col = "darkgray",
     xlab = "Age", ylab = "Martingale Residual",
     main = "Residuals vs. Linear Term\n(smoother df = 3)",
     cex.main = 0.90)
abline(h = 0)
lines(smooth.spline(X, Y, df = 2), lty = 2, lwd = 2)

plot(X2, Y, pch = 20, col = "darkgray",
     xlab = "log(Age)", ylab = "Martingale Residual",
     main = "Residuals vs. Quadratic Term\n(smoother df = 3)",
     cex.main = 0.90)
abline(h = 0)
lines(smooth.spline(X2, Y, df = 2), lty = 2, lwd = 2)


