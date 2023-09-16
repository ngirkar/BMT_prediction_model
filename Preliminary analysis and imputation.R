
###LOADING THE REQUIRED LIBRARIES######
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)
library(caTools)
library(tableone)
library(VIM)
library(tidyverse)
library(missForest)

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


#######SPLITTING THE DATA INTO TRAINING AND VALIDATION DATASET#######


set.seed(123)

# Create a split for the training set
#stratify by event status(here-Deceased) and split ratio be 80-20
sample <- sample.split(new_data1$Deceased, SplitRatio = 0.8)
training_data <- subset(new_data1, sample == TRUE)
validation_data <- subset(new_data1, sample == FALSE)

dim(training_data)

nrow(training_data)
nrow(validation_data) 


###PRELIMINARY ANALYSIS OF THE TRAINING DATA AND VALIDATION DATA ########

######1.Training data ################
#total missing in the training dataset  
total_missing <- sum(is.na(training_data))
total_missing

# calculating percentage of missing values
percentage = mean(is.na(training_data)) * 100
percentage

#percentage of missing values in each column 
(colMeans(is.na(training_data)))*100

# visual representation of missing data
plot1 = aggr(training_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(training_data), cex.axis=.7, gap=2, ylab=c("Proportion of missingness","Missingness Pattern"))

##looking at the mean and median values of variables - Age, Ramp, AST, LDH, HCT.CI.Score, and Kco and histogram of these variables, we find them to have a non-normal distribution and represent them with median values#####
summary_stats <-summary(training_data)
hist(training_data$Age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "black", border = "black")
hist(training_data$Ramp..W., main = "Histogram of Ramp", xlab ="Ramp", ylab ="Frequency", col ="black", border ="black")
hist(training_data$AST, main = "Histogram of AST", xlab ="AST", ylab ="Frequency", col ="black", border ="black")
hist(training_data$LDH, main = "Histogram of LDH", xlab ="LDH", ylab ="Frequency", col ="black", border ="black")
hist(training_data$HCT.CI.Score, main = "Histogram of HCT.CI.Score", xlab ="HCT.CI.Score", ylab ="Frequency", col ="black", border ="black")
hist(training_data$HCT.CI.Score, main = "Histogram of Kco", xlab ="Kco", ylab ="Frequency", col ="black", border ="black")

# Create a Table 1  for training dataset 
tab1 <- CreateTableOne(vars = c("Age", "Ramp..W.", "VO2.Peak", "AT","V.VO2","V.VCO2","Hb..g.l.","Ur..mmol.l.","Cr..µmol.l.","Bilirubin","AST","LDH","FEV1..l.","FVC..l.","FEV1.FVC....","DLCO","Kco"),
                       data = training_data)

# Print the Table 1 with median values for specific variables
print(tab1, nonnormal = c("Age", "Ramp..W.", "AST", "LDH","HCT.CI.Score","Kco"), addOverall = "median", showAllLevels = TRUE)

##Extracting median values of HCT.CI.Score separately 
summary_stats1 <-summary(training_data$HCT.CI.Score)
summary_stats1

# distribution and proportion for deceased variable 
table(training_data$Deceased)
round(prop.table(table(training_data$Deceased))*100)


##########2.Validation dataset###############

total_missing1 <- sum(is.na(validation_data))
total_missing1

# calculating percentage of missing values
percentage1 = mean(is.na(validation_data)) * 100
percentage1

##percentage of missing values in each column 
(colMeans(is.na(validation_data)))*100

##visual representation of missing values###
plot1 = aggr(validation_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(validation_data), cex.axis=.7, gap=2, ylab=c("Proportion of missingness","Missingness Pattern"))


##looking at the mean and median values of variables - Age, Ramp, AST, LDH, HCT.CI.Score, and Kco and histogram of these variables, we find them to have a non-normal distribution and represent them with median values#####
summary_stats <-summary(validation_data)
summary_stats
hist(validation_data$Age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "black", border = "black")
hist(validation_data$Ramp..W., main = "Histogram of Ramp", xlab ="Ramp", ylab ="Frequency", col ="black", border ="black")
hist(validation_data$AST, main = "Histogram of AST", xlab ="AST", ylab ="Frequency", col ="black", border ="black")
hist(validation_data$LDH, main = "Histogram of LDH", xlab ="LDH", ylab ="Frequency", col ="black", border ="black")
hist(validation_data$HCT.CI.Score, main = "Histogram of HCT.CI.Score", xlab ="HCT.CI.Score", ylab ="Frequency", col ="black", border ="black")
hist(validation_data$HCT.CI.Score, main = "Histogram of Kco", xlab ="Kco", ylab ="Frequency", col ="black", border ="black")


# Create a Table 1  for validation dataset 
tab2 <- CreateTableOne(vars = c("Age", "Ramp..W.", "VO2.Peak", "AT","V.VO2","V.VCO2","Hb..g.l.","Ur..mmol.l.","Cr..µmol.l.","Bilirubin","AST","LDH","FEV1..l.","FVC..l.","FEV1.FVC....","DLCO"),
                       data = validation_data)


# Print the Table 1 with median values for specific variables
print(tab2, nonnormal = c("Age", "Ramp..W.", "AST", "LDH", "HCT.CI.Score", "Kco"), addOverall = "median", showAllLevels = TRUE)

##median and IQR for HCT.CI.Score and Kco
summary(validation_data$HCT.CI.Score)
summary(validation_data$Kco)


# distribution and proportion for deceased variable 
table(validation_data$Deceased)
round(prop.table(table(validation_data$Deceased))*100)



##### SINGLE IMPUTATION FOR THE TRAINING DATASET ##############



# Perform imputation using missForest
imputed_data <- missForest(training_data)
# Extract the imputed dataset-training 
imputed_data_complete <- imputed_data$ximp
summary(imputed_data_complete)



#####SINGLE IMPUTATION ON VALIDATION DATASET####

# Perform imputation using missForest
imputed_vdata <- missForest(validation_data)
# Extract the imputed dataset
imputed_vdata_complete <- imputed_vdata$ximp
summary(imputed_vdata_complete)

