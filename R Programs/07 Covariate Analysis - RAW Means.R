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

raw_means <- function(this_var, response, data, category){
  print(category)
  var_levels <- unique(this_var)
  var_levels <- sort(var_levels)
  ages <- sort(unique(data$Age_cat))
  N_min <- length(var_levels)
  for (i_var in var_levels){
    keep <- this_var == i_var & (data$Age_cat == ages[1])
    if(sum(keep, na.rm=TRUE) > N_min){
      def <- data[keep,]
      r <- response[keep]
      t1 <- t.test(r)
      u1 <- c(round(t1$estimate,2), round(t1$conf.int[1],2), round(t1$conf.int[2],2))
    } else {
      u1 <- c(mean(r),rep(NA, 2))
    }
    
    keep <- (this_var == i_var) & (data$Age_cat == ages[2])
    if(sum(keep, na.rm=TRUE) > N_min){
      def <- data[keep,]
      r <- response[keep]
      t1 <- t.test(r)
      u2 <- c(round(t1$estimate,2), round(t1$conf.int[1],2), round(t1$conf.int[2],2))
    } else {
      u2 <- c(mean(r),rep(NA, 2))
    }
    
    keep <- (this_var == i_var) & (data$Age_cat == ages[3])
    if(sum(keep, na.rm=TRUE) > N_min){
      def <- data[keep,]
      r <- response[keep]
      t1 <- t.test(r)
      u3 <- c(round(t1$estimate,2), round(t1$conf.int[1],2), round(t1$conf.int[2],2))
    } else {
      u3 <- c(mean(r),rep(NA, 2))
    }
    
    aov_model <- aov(response~this_var+data$Age_cat)
    aov_summary <- summary(aov_model)
    p_value <- aov_summary[[1]][1,5]
    
    if(i_var == var_levels[1]){
      fmat <- c("%60s   %6.2f %6.2f %6.2f   %6.2f %6.2f %6.2f   %6.2f %6.2f %6.2f  %10.3f")      #   %6.2f   %6.2f   %6.2f")
      s1 <- sprintf(fmat, i_var,
                    u1[1],u1[2],u1[3],
                    u2[1],u2[2],u2[3],
                    u3[1],u3[2],u3[3], p_value)
    } else {
      fmat <- c("%60s   %6.2f %6.2f %6.2f   %6.2f %6.2f %6.2f   %6.2f %6.2f %6.2f")      #   %6.2f   %6.2f   %6.2f")
      s1 <- sprintf(fmat, i_var,
                    u1[1],u1[2],u1[3],
                    u2[1],u2[2],u2[3],
                    u3[1],u3[2],u3[3])
    }
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
print(paste("Original Covariate count", nrow(covariates)))
print(paste("Missing CFQ data", nrow(covariates)-nrow(data)))
keep <- !is.na(data$WTMEC2YR)
print(paste("Missing Stat Wts",sum(!keep)))
data <- data[keep,]
print(paste("Missing CFQ AND wt data", nrow(covariates)-nrow(data)))

print(paste("Final Participant Count",nrow(data)))

#print header for table
print("SAMPLE ESTIMATES")
print("Means and CI by Category Level")

response <- data$CFDCSR
cat("\n\nResponse:  CFDCSR\n")
s0 <- sprintf("%70s 60-69 %16s 70-79 %12s 80 and Older"," "," "," ")
s1 <- sprintf("%60s     Mean   LCL   UCL       Mean   LCL   UCL       Mean   LCL   UCL"," ")
print(s0)
print(s1)
raw_means(this_var=data$RIAGENDR, response, data, "Gender")
raw_means(this_var=data$RIDRETH1, response, data, "Race/Ethnicity")
raw_means(this_var=data$DMDBORN4, response, data, "Country born in")
raw_means(this_var=data$DMDEDUC2, response, data, "Education")
raw_means(this_var=data$DMDMARTL, response, data, "Marital status")
raw_means(this_var=data$BMI_cat, response, data, "BMI category")
raw_means(this_var=data$SMQ_cat, response, data, "Smoking category")
raw_means(this_var=data$ALQ_cat, response, data, "Drink category")
raw_means(this_var=data$DIQ_cat, response, data, "Diabetes category")
raw_means(this_var=data$FIPR_cat, response, data, "FIPR category")

response <- data$CFDCIR
cat("\n\nResponse:  CFDCIR\n")
print(s0)
print(s1)
raw_means(this_var=data$RIAGENDR, response, data, "Gender")
raw_means(this_var=data$RIDRETH1, response, data, "Race/Ethnicity")
raw_means(this_var=data$DMDBORN4, response, data, "Country born in")
raw_means(this_var=data$DMDEDUC2, response, data, "Education")
raw_means(this_var=data$DMDMARTL, response, data, "Marital status")
raw_means(this_var=data$BMI_cat, response, data, "BMI category")
raw_means(this_var=data$SMQ_cat, response, data, "Smoking category")
raw_means(this_var=data$ALQ_cat, response, data, "Drink category")
raw_means(this_var=data$DIQ_cat, response, data, "Diabetes category")
raw_means(this_var=data$FIPR_cat, response, data, "FIPR category")

response <- data$CFDAST
cat("\n\nResponse:  CFDAST\n")
print(s0)
print(s1)
raw_means(this_var=data$RIAGENDR, response, data, "Gender")
raw_means(this_var=data$RIDRETH1, response, data, "Race/Ethnicity")
raw_means(this_var=data$DMDBORN4, response, data, "Country born in")
raw_means(this_var=data$DMDEDUC2, response, data, "Education")
raw_means(this_var=data$DMDMARTL, response, data, "Marital status")
raw_means(this_var=data$BMI_cat, response, data, "BMI category")
raw_means(this_var=data$SMQ_cat, response, data, "Smoking category")
raw_means(this_var=data$ALQ_cat, response, data, "Drink category")
raw_means(this_var=data$DIQ_cat, response, data, "Diabetes category")
raw_means(this_var=data$FIPR_cat, response, data, "FIPR category")

response <- data$CFDDS
cat("\n\nResponse:  CFDDS\n")
print(s0)
print(s1)
raw_means(this_var=data$RIAGENDR, response, data, "Gender")
raw_means(this_var=data$RIDRETH1, response, data, "Race/Ethnicity")
raw_means(this_var=data$DMDBORN4, response, data, "Country born in")
raw_means(this_var=data$DMDEDUC2, response, data, "Education")
raw_means(this_var=data$DMDMARTL, response, data, "Marital status")
raw_means(this_var=data$BMI_cat, response, data, "BMI category")
raw_means(this_var=data$SMQ_cat, response, data, "Smoking category")
raw_means(this_var=data$ALQ_cat, response, data, "Drink category")
raw_means(this_var=data$DIQ_cat, response, data, "Diabetes category")
raw_means(this_var=data$FIPR_cat, response, data, "FIPR category")
