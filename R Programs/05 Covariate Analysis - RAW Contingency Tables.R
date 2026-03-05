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
  def <- abc
  for (j in 1:nrow(def)){
    def[j,] <- 100*abc[j,]/rsum[j]
  }
  def <- round(def,1)
  Ntotal <- sum(rsum)
  Prow <- round(100*rsum/Ntotal,1)
  ghi <- cbind(rsum,Prow)
  t <- cbind(abc[,1],def[,1],abc[,2],def[,2],abc[,3],def[,3],ghi[,1],ghi[,2])
  csum <- colSums(abc)
  Pcol <- round(100*csum/Ntotal,1)
  last_row <- c(csum[1],Pcol[1],csum[2],Pcol[2],csum[3],Pcol[3],Ntotal,100)
  t <- rbind(t,last_row)
  rownames(t) <- c(rownames(abc),"Total")
  colnames(t)[7:8] <- c("Total","Total")
  
  fmat1 <- "%60s   %5.0f %5.1f%%   %5.0f %5.1f%%   %5.0f %5.1f%%  %5.0f %5.1f%%    %10.8f"
  fmat2 <- "%60s   %5.0f %5.1f%%   %5.0f %5.1f%%   %5.0f %5.1f%%  %5.0f %5.1f%%"
  s1 <- sprintf(fmat1,rownames(t)[1],t[1,1],t[1,2],t[1,3],t[1,4],t[1,5],t[1,6],t[1,7],t[1,8],p)
  print(s1)
  for (i in 2:nrow(t)){
    s2 <- sprintf(fmat2,rownames(t)[i],t[i,1],t[i,2],t[i,3],t[i,4],t[i,5],t[i,6],t[i,7],t[i,8])
    print(s2)
  }
}

#Paste into other programs then change as needed
path1 <- "~/Desktop/Cognitive Function v PFAS"
path2 <- paste0("/02 Cleaned Data/")
dir_in <- paste0(path1, path2)

covariates <- read.csv(file=paste0(dir_in,"Covariates_Cleaned.csv"), header=TRUE)
print(dim(covariates))

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

data <- covariates
keep <- !is.na(data$WTMEC2YR)
data <- data[keep,]
print(paste("Respondents", nrow(covariates)))
print(paste("Missing Stat Wts data", nrow(covariates)-nrow(data)))
print(paste("Final Participant Count",nrow(data)))

#print header for table
print("SAMPLE ESTIMATES")
print("Percentages by Category Level")
s1 <- sprintf("%60s     %8s       %8s         %8s   %8s    %10s","Category                                          Level","60-69","70-79","80-Older","Total","p value")
print(s1)

abc <- table(data$RIAGENDR, data$Age_cat)
p <- chisq.test(data$RIAGENDR, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Gender")

abc <- table(data$RIDRETH1, data$Age_cat)
p <- chisq.test(data$RIDRETH1, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Race/Hispanic origin")

abc <- table(data$DMDBORN4, data$Age_cat)
p <- chisq.test(data$DMDBORN4, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Country of birth")

abc <- table(data$DMDEDUC2, data$Age_cat)
p <- chisq.test(data$DMDEDUC2, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Education level")

abc <- table(data$DMDMARTL, data$Age_cat)
p <- chisq.test(data$DMDMARTL, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Marital status")

abc <- table(data$BMI_cat, data$Age_cat)
p <- chisq.test(data$BMI_cat, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "BMI category")

abc <- table(data$SMQ_cat, data$Age_cat)
p <- chisq.test(data$SMQ_cat, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Smoking category")

abc <- table(data$ALQ_cat, data$Age_cat)
p <- chisq.test(data$ALQ_cat, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Drink category")

abc <- table(data$DIQ_cat, data$Age_cat)
p <- chisq.test(data$DIQ_cat, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "Diabetes category")

abc <- table(data$FIPR_cat, data$Age_cat)
p <- chisq.test(data$FIPR_cat, data$Age_cat, simulate.p.value = TRUE)$p.value
p_con_table(abc, p, "FIPR category")
