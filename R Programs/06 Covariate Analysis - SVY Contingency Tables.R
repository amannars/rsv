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

######
# Copy screen output into Excel for formatting etc.
######

#function to print the contingency table in a pretty form
p_con_table <- function(abc, p, label){
  print(label)
  #convert table values to percentages
  rsum <- rowSums(abc)
  for (j in 1:nrow(abc)){
    abc[j,] <- 100*abc[j,]/rsum[j]
  }
  
  s1 <- sprintf("%60s   %8.0f  %8.0f  %8.0f  %10.8f",rownames(abc)[1],abc[1,1],abc[1,2],abc[1,3],p)
  print(s1)
  for (i in 2:nrow(abc)){
    s2 <- sprintf("%60s   %8.0f  %8.0f  %8.0f",rownames(abc)[i],abc[i,1],abc[i,2],abc[i,3])
    print(s2)
  }
}

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

CFQ_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("CFDCSR", "CERAD: Score Delayed Recall"),
                   c("CFDCIR", "CERAD: Intrusion word count Recall"),
                   c("CFDAST", "Animal Fluency: Score Total"),
                   c("CFDDS", "Digit Symbol: Score"))

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

data <- covariates   #merge(CFQ,covariates)
keep <- !is.na(data$WTMEC2YR)
data <- data[keep,]
print(paste("Missing CFQ data", nrow(covariates)-nrow(data)))
print(paste("Final Participant Count",nrow(data)))

my_wts <- data$WTINT2YR/2      #change the divsor if different number of cycles
data$my_wts <- my_wts
svy_design <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, weights=~my_wts, nest=TRUE)

#print header for table
print("POPULATION ESTIMATES")
print("Percentages by Category Level")
s1 <- sprintf("%60s   %8s  %8s  %8s  %10s","Category                                          Level","60-69","70-79","80-Older","p value")
print(s1)

abc <- svytable(~RIAGENDR+Age_cat, svy_design)
p <- svychisq(~RIAGENDR+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Gender")

abc <- svytable(~RIDRETH1+Age_cat, svy_design)
p <- svychisq(~RIDRETH1+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Race/Hispanic origin")

abc <- svytable(~DMDBORN4+Age_cat, svy_design)
p <- svychisq(~DMDBORN4+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Country of birth")

abc <- svytable(~DMDEDUC2+Age_cat, svy_design)
p <- svychisq(~DMDEDUC2+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Education level - Adults 60+")

abc <- svytable(~DMDMARTL+Age_cat, svy_design)
p <- svychisq(~DMDMARTL+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Marital status")

abc <- svytable(~BMI_cat+Age_cat, svy_design)
p <- svychisq(~BMI_cat+Age_cat, svy_design)$p.value
p_con_table(abc, p, "BMI category")

abc <- svytable(~SMQ_cat+Age_cat, svy_design)
p <- svychisq(~SMQ_cat+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Smoking category")

abc <- svytable(~ALQ_cat+Age_cat, svy_design)
p <- svychisq(~ALQ_cat+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Drink category")

abc <- svytable(~DIQ_cat+Age_cat, svy_design)
p <- svychisq(~DIQ_cat+Age_cat, svy_design)$p.value
p_con_table(abc, p, "Diabetes category")

abc <- svytable(~FIPR_cat+Age_cat, svy_design)
p <- svychisq(~FIPR_cat+Age_cat, svy_design)$p.value
p_con_table(abc, p, "FIPR category")
