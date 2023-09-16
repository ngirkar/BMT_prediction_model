
###LOADING THE REQUIRED LIBRARIES######
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)
library(pmsampsize)



####READING THE DATA#######
data <- read.csv("BMT data anonymised w OS 210723.csv")



######CREATING A NEW SUBSET OF DATA BY REMOVING COLUMNS NOT REQUIRED ##############
columns_to_remove <- c("ID", "Medication","Medication.2","Medication.3","Medication.4","Medication.5","Medication.6","Medication.7","Medication.8","Co.morbidity.1","Co.morbidity.2","Co.morbidity.3","Cause.of.Death","CCU..transplant.","CCU..post.transplant.","aGvHD","cGvHD","LoS","Risk.Non.relapse.mortality","X2.yr.overall.survival....","X2.yr.Non.relapse.Mortality....","Risk.Non.relapse.mortality","Pre.transplant.Chemotherapy","Abnormal.Graph.4")

# Create new dataset with remaining columns using dplyr
new_data1 <- data %>%
  select(- one_of(columns_to_remove))



####TRANSFORMING VARIABLES######

str(new_data1)# getting the internal structure of the dataset

#converting variables to numeric 
new_data1 <- new_data1 %>%
  mutate(Ramp..W.= as.numeric(Ramp..W.),
         VO2.Peak = as.numeric(VO2.Peak),
         AT = as.numeric(AT),
         V.VO2 = as.numeric(V.VO2),
         V.VCO2 = as.numeric(V.VCO2),
         Hb..g.l. = as.numeric(Hb..g.l.),
         AST = as.numeric(AST),
         LDH = as.numeric(LDH),
         Deceased = ifelse(Deceased == "Yes", 1, 0),
         Overall.Survival = as.numeric(Overall.Survival),
         Age = as.numeric(Age),
         #HCT.CI.Score = factor(HCT.CI.Score, levels = 0:5),
         HCT.CI.Score = as.numeric(HCT.CI.Score),
         DLCO = as.numeric(DLCO)
         
  )


# Print the resulting dataset
str(new_data1)
summary(new_data1)



###SAMPLE SIZE CALCULATION######


#Calculate baseline event rate (events per person-time)
event_rate <- sum(new_data1$Deceased) / sum(new_data1$Overall.Survival)

# Print the baseline event rate
print(event_rate)

#meanfup calculation 
mean(new_data1$Overall.Survival)


#R2 calculation
time_person <- 349.4*131
expectedevents <- event_rate*time_person
lnLnull <- (expectedevents*(log(expectedevents/131)))-expectedevents
max_r2a <- (1- exp((2*lnLnull)/131))
max_r2a
R2 <-max_r2a*0.15
R2

##substituting the values to get the minimum sample size###
pmsampsize(type ="s", rsquared = R2, parameters = 18, rate = event_rate, timepoint = 365.24, meanfup = 349.4)

###varying the predictor parameters to reach a suitable sample size####
pmsampsize(type ="s", rsquared = R2, parameters = 10, rate = event_rate, timepoint = 365.24, meanfup = 349.4)

pmsampsize(type ="s", rsquared = R2, parameters = 5, rate = event_rate, timepoint = 365.24, meanfup = 349.4)

pmsampsize(type ="s", rsquared = R2, parameters = 3, rate = event_rate, timepoint = 365.24, meanfup = 349.4)

pmsampsize(type ="s", rsquared = R2, parameters = 2, rate = event_rate, timepoint = 365.24, meanfup = 349.4)
