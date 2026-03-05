######
#This program looks at the correlation of the covariates with age
######


#clean/clear workspace
rm(list = ls())          #clear workspace variables
gc()                     #garbage collection
cat("\014")              #clear console

#install required (or those I'm to lazy to not incude)libraries
library(dplyr)
library(agricolae)
library(survey)

svy_means <- function(VARIATE, RESPONSE, data, category){
  print(category)
  data$RESPONSE <- RESPONSE
  data$VARIATE <- VARIATE
  svy_design <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, weights=~my_wts, nest=TRUE)
  
  sub1 <- subset(svy_design, Age_cat == "60-69")
  sub2 <- subset(svy_design, Age_cat == "70-79")
  sub3 <- subset(svy_design, Age_cat == "80 & up")
  
  m1 <- svyby(~RESPONSE, ~VARIATE, sub1, svymean, na.rm=TRUE)
  m2 <- svyby(~RESPONSE, ~VARIATE, sub2, svymean, na.rm=TRUE)
  m3 <- svyby(~RESPONSE, ~VARIATE, sub3, svymean, na.rm=TRUE)
  
  CI1 <- confint(m1, df=degf(svy_design))
  CI2 <- confint(m2, df=degf(svy_design))
  CI3 <- confint(m3, df=degf(svy_design))
  
  mci1 <- cbind(m1[,2],CI1[,1:2])
  mci2 <- cbind(m2[,2],CI2[,1:2])
  mci3 <- cbind(m3[,2],CI3[,1:2])
  
  m1m <- as.matrix(mci1)
  m2m <- as.matrix(mci2)
  m3m <- as.matrix(mci3)
  
  m123 <- matrix(NA, nrow=max(nrow(m1), nrow(m2),nrow(m3)), ncol=9)
  m123[1:nrow(m1m),1:3] <- m1m[1:nrow(m1m), 1:3]
  m123[1:nrow(m2m),4:6] <- m2m[1:nrow(m2m), 1:3]
  m123[1:nrow(m3m),7:9] <- m3m[1:nrow(m3m), 1:3]
  
  fmat <- "%20s   %8.3f   %8.3f   %8.3f   %8.3f   %8.3f   %8.3f   %8.3f   %8.3f   %8.3f"
  for (i in 1:nrow(m123)){
    s1 <- sprintf(fmat," ",
                  m123[i,1],m123[i,2],m123[i,3],
                  m123[i,4],m123[i,5],m123[i,6],
                  m123[i,7],m123[i,8],m123[i,9])
    print(s1)
  }
}

######
# Copy screen output into Excel for formatting etc.
######

#Paste into other programs then change as needed
path1 <- "~/Desktop/Cognitive Function v PFAS"
path2 <- paste0("/02 Cleaned Data/")
dir_in <- paste0(path1, path2)

covariates <- read.csv(file=paste0(dir_in,"Covariates_Cleaned.csv"), header=TRUE)
print(dim(covariates))

#read response data
CFQ <- read.csv(file=paste0(dir_in,"CFQ_Cleaned.csv"), header=TRUE)

covariates_names <- rbind(c("SEQN", "Respondent sequence number"),
                          c("SDDSRVYR", "Data release cycle"),
                          c("RIDSTATR", "Interview/Examination status"),
                          c("WTINT2YR", "Full sample 2 year interview weight"),
                          c("WTMEC2YR", "Full sample 2 year MEC exam weight"),
                          c("SDMVPSU", "Masked variance pseudo-PSU"),
                          c("SDMVSTRA", "Masked variance pseudo-stratum"),
                          c("RIAGENDR", "Gender"),
                          c("RIDAGEYR", "Age in years at screening "),
                          c("RIDRETH1", "Race/Hispanic origin"),
                          c("RIDRETH3", "Race/Hispanic origin w/ NH Asian"),
                          c("DMDBORN4", "Country of birth"),
                          c("DMDEDUC2", "Education level - Adults 20+"),
                          c("DMDMARTL", "Marital status"),
                          c("Age_cat", "Age category"),
                          c("BMI_cat", "BMI category"),
                          c("SMQ_cat", "Smoking category"),
                          c("ALQ_cat", "Drink category"),
                          c("DIQ_cat", "Diabetes category"),
                          c("FIPR_cat","FIPR category"))

#define categorical variables
covariates$RIAGENDR <- as.factor(covariates$RIAGENDR)
covariates$RIDAGEYR <- as.factor(covariates$RIDAGEYR)
covariates$RIDRETH1 <- as.factor(covariates$RIDRETH1)
covariates$RIDRETH3 <- as.factor(covariates$RIDRETH3)
covariates$DMDBORN4 <- as.factor(covariates$DMDBORN4)
covariates$DMDEDUC2 <- as.factor(covariates$DMDEDUC2)
covariates$DMDMARTL <- as.factor(covariates$DMDMARTL)
covariates$Age_cat <- as.factor(covariates$Age_cat)
covariates$BMI_cat <- as.factor(covariates$BMI_cat)
covariates$SMQ_cat <- as.factor(covariates$SMQ_cat)
covariates$ALQ_cat <- as.factor(covariates$ALQ_cat)
covariates$DIQ_cat <- as.factor(covariates$DIQ_cat)
covariates$FIPR_cat <- as.factor(covariates$FIPR_cat)

data <- merge(covariates, CFQ)
keep <- !is.na(data$WTMEC2YR)
data <- data[keep,]
print(paste("Missing CFQ data", nrow(covariates)-nrow(data)))
print(paste("Final Participant Count",nrow(data)))

my_wts <- data$WTINT2YR/2      #change the divsor if different number of cycles
data$my_wts <- my_wts

#print header for table
print("POPULATION ESTIMATES")
print("Means and CI by Category Level")

#Response: CFDCSR
print("Response:  CFDCSR")
svy_means(VARIATE=data$RIAGENDR, RESPONSE=data$CFDCSR, data, "Gender")
svy_means(VARIATE=data$RIDRETH1, RESPONSE=data$CFDCSR, data, "Race/Ethnicity")
svy_means(VARIATE=data$DMDBORN4, RESPONSE=data$CFDCSR, data, "Country born in")
svy_means(VARIATE=data$DMDEDUC2, RESPONSE=data$CFDCSR, data, "Education")
svy_means(VARIATE=data$DMDMARTL, RESPONSE=data$CFDCSR, data, "Marital status")
svy_means(VARIATE=data$BMI_cat, RESPONSE=data$CFDCSR, data, "BMI category")
svy_means(VARIATE=data$SMQ_cat, RESPONSE=data$CFDCSR, data, "Smoking category")
svy_means(VARIATE=data$ALQ_cat, RESPONSE=data$CFDCSR, data, "Drink category")
svy_means(VARIATE=data$DIQ_cat, RESPONSE=data$CFDCSR, data, "Diabetes category")
svy_means(VARIATE=data$FIPR_cat, RESPONSE=data$CFDCSR, data, "FIPR category")


#Response:  CFDCIR
print("Response:  CFDCIR")
svy_means(VARIATE=data$RIAGENDR, RESPONSE=data$CFDCIR, data, "Gender")
svy_means(VARIATE=data$RIDRETH1, RESPONSE=data$CFDCIR, data, "Race/Ethnicity")
svy_means(VARIATE=data$DMDBORN4, RESPONSE=data$CFDCIR, data, "Country born in")
svy_means(VARIATE=data$DMDEDUC2, RESPONSE=data$CFDCIR, data, "Education")
svy_means(VARIATE=data$DMDMARTL, RESPONSE=data$CFDCIR, data, "Marital status")
svy_means(VARIATE=data$BMI_cat, RESPONSE=data$CFDCIR, data, "BMI category")
svy_means(VARIATE=data$SMQ_cat, RESPONSE=data$CFDCIR, data, "Smoking category")
svy_means(VARIATE=data$ALQ_cat, RESPONSE=data$CFDCIR, data, "Drink category")
svy_means(VARIATE=data$DIQ_cat, RESPONSE=data$CFDCIR, data, "Diabetes category")
svy_means(VARIATE=data$FIPR_cat, RESPONSE=data$CFDCIR, data, "FIPR category")


#Response:  CFDAST
print("Response:  CFDAST")
svy_means(VARIATE=data$RIAGENDR, RESPONSE=data$CFDAST, data, "Gender")
svy_means(VARIATE=data$RIDRETH1, RESPONSE=data$CFDAST, data, "Race/Ethnicity")
svy_means(VARIATE=data$DMDBORN4, RESPONSE=data$CFDAST, data, "Country born in")
svy_means(VARIATE=data$DMDEDUC2, RESPONSE=data$CFDAST, data, "Education")
svy_means(VARIATE=data$DMDMARTL, RESPONSE=data$CFDAST, data, "Marital status")
svy_means(VARIATE=data$BMI_cat, RESPONSE=data$CFDAST, data, "BMI category")
svy_means(VARIATE=data$SMQ_cat, RESPONSE=data$CFDAST, data, "Smoking category")
svy_means(VARIATE=data$ALQ_cat, RESPONSE=data$CFDAST, data, "Drink category")
svy_means(VARIATE=data$DIQ_cat, RESPONSE=data$CFDAST, data, "Diabetes category")
svy_means(VARIATE=data$FIPR_cat, RESPONSE=data$CFDAST, data, "FIPR category")


#Response:  CFDDS
print("Response:  CFDDS")
svy_means(VARIATE=data$RIAGENDR, RESPONSE=data$CFDDS, data, "Gender")
svy_means(VARIATE=data$RIDRETH1, RESPONSE=data$CFDDS, data, "Race/Ethnicity")
svy_means(VARIATE=data$DMDBORN4, RESPONSE=data$CFDDS, data, "Country born in")
svy_means(VARIATE=data$DMDEDUC2, RESPONSE=data$CFDDS, data, "Education")
svy_means(VARIATE=data$DMDMARTL, RESPONSE=data$CFDDS, data, "Marital status")
svy_means(VARIATE=data$BMI_cat, RESPONSE=data$CFDDS, data, "BMI category")
svy_means(VARIATE=data$SMQ_cat, RESPONSE=data$CFDDS, data, "Smoking category")
svy_means(VARIATE=data$ALQ_cat, RESPONSE=data$CFDDS, data, "Drink category")
svy_means(VARIATE=data$DIQ_cat, RESPONSE=data$CFDDS, data, "Diabetes category")
svy_means(VARIATE=data$FIPR_cat, RESPONSE=data$CFDDS, data, "FIPR category")
