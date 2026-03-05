#clean/clear workspace
rm(list = ls())          #clear workspace variables
gc()                     #garbage collection
cat("\014")              #clear console

#install required libraries
library(dplyr)

#Paste into other programs then change as needed
path1 <- "~/Desktop/Cognitive Function v PFAS"
path2 <- paste0("/01 Download Data/")
dir_in <- paste0(path1, path2)

#Paste into other programs then change as needed
path3 <- paste0(path1,"/02 Cleaned Data")
if(!dir.exists(path3)){
  dir.create(path3)
  print(paste("Directory Created:", path3))
}
dir_out <- paste0(path3,"/")

#Read demographics and other covariates
DEMO <- read.csv(file=paste0(dir_in,"DEMO.csv"), header=TRUE)
BMI <- read.csv(file=paste0(dir_in,"BMI.csv"), header=TRUE)
SMQ <- read.csv(file=paste0(dir_in,"SMQ.csv"), header=TRUE)
ALQ <- read.csv(file=paste0(dir_in,"ALQ.csv"), header=TRUE)
DIQ <- read.csv(file=paste0(dir_in,"DIQ.csv"), header=TRUE)

#merge data, but keep it all for now
print(nrow(DEMO))
co_var1 <- merge(DEMO, BMI, by="SEQN", all=TRUE)
co_var2 <- merge(co_var1, SMQ, by="SEQN", all=TRUE)
co_var3 <- merge(co_var2, ALQ, by="SEQN", all=TRUE)
co_var4 <- merge(co_var3, DIQ, by="SEQN", all=TRUE)
DEMO <- co_var4

#clean data - Gender, Age, and Race have no non-responsive answers
#DMDMARTL
change <- DEMO$DMDMARTL == "Refused" | DEMO$DMDMARTL == "Don't Know"
DEMO$DMDMARTL[change] <- "8 Non-Responsive"
DEMO$DMDMARTL[is.na(DEMO$DMDMARTL)] <- "9 Missing"

#RIDRETH1
change <- DEMO$RIDRETH1 == "Refused" | DEMO$RIDRETH1 == "Don't Know"
DEMO$RIDRETH1[change] <- "8 Non-Responsive"
DEMO$RIDRETH1[is.na(DEMO$RIDRETH1)] <- "9 Missing"

#RIDRETH3
change <- DEMO$RIDRETH3 == "Refused" | DEMO$RIDRETH3 == "Don't Know"
DEMO$RIDRETH3[change] <- "8 Non-Responsive"
DEMO$RIDRETH3[is.na(DEMO$RIDRETH3)] <- "9 Missing"

#DMDEDUC2
change <- DEMO$DMDEDUC2 == "Refused" | DEMO$DMDEDUC2  == "Don't Know"
DEMO$DMDEDUC2[change] <- "8 Non-Responsive"
DEMO$DMDEDUC2[is.na(DEMO$DMDEDUC2)] <- "9 Missing"

#RIDAGEYR
#change <- DEMO$RIDAGEYR == "Refused" | DEMO$RIDAGEYR  == "Don't Know"
#DEMO$RIDAGEYR[change] <- "8 Non-Responsive"
#DEMO$RIDAGEYR[is.na(DEMO$RIDAGEYR)] <- "9 Missing"

#DMDBORN4
change <- DEMO$DMDBORN4 == "Refused" | DEMO$DMDBORN4  == "Don't Know"
DEMO$DMDBORN4[change] <- "8 Non-Responsive"
DEMO$DMDBORN4[is.na(DEMO$DMDBORN4)] <- "9 Missing"

#INDFMPIR
#change <- DEMO$INDFMPIR == "Refused" | DEMO$INDFMPIR  == "Don't Know"
#DEMO$INDFMPIR[change] <- "8 Non-Responsive"
#DEMO$INDFMPIR[is.na(DEMO$INDFMPIR)] <- "9 Missing"

#BMXBMI
#change <- DEMO$BMXBMI == "Refused" | DEMO$BMXBMI  == "Don't Know"
#DEMO$BMXBMI[change] <- "8 Non-Responsive"
#DEMO$BMXBMI[is.na(DEMO$BMXBMI)] <- "9 Missing"

#SMQ020
#change <- DEMO$SMQ020 == "Refused" | DEMO$SMQ020 == "Don't know"
#DEMO$SMQ020[change] <- "8 Non-Responsive"
#DEMO$SMQ020[is.na(DEMO$SMQ020)] <- "9 Missing"

#DIQ010
#change <- DEMO$DIQ010 == "Refused" | DEMO$DIQ010 == "Don't know"
#DEMO$DIQ010[change] <- "8 Non-Responsive"
#DEMO$DIQ010[is.na(DEMO$DIQ010)] <- "9 Missing"

#ALQ110
#change <- DEMO$ALQ110 == "Refused" | DEMO$ALQ110 == "Don't know"
#DEMO$ALQ110[change] <- "8 Non-Responsive"
#DEMO$ALQ110[is.na(DEMO$ALQ110)] <- "9 Missing"

print(paste("TOTAL DEMOGRAPHICS", nrow(DEMO)))
keep <- DEMO$RIDAGEYR >= 60
covariates <- DEMO[keep,]
print(paste("Lost due to age", nrow(DEMO)-nrow(covariates)))
print(paste("Final Covaraites", nrow(covariates)))

######
# Create Categorical Variables and/or change category names
###### 
covariates$DMDMARTL[covariates$DMDMARTL == "Married"] <- "1 Married/Living with partner"
covariates$DMDMARTL[covariates$DMDMARTL == "Living with partner"] <- "1 Married/Living with partner"
covariates$DMDMARTL[covariates$DMDMARTL == "Never married"] <- "2 Never married"
covariates$DMDMARTL[covariates$DMDMARTL == "Widowed"] <- "3 Widowed"
covariates$DMDMARTL[covariates$DMDMARTL == "Divorced"] <- "4 Divorced/Separated"
covariates$DMDMARTL[covariates$DMDMARTL == "Separated"] <- "4 Divorced/Separated"

covariates$RIDRETH1[covariates$RIDRETH1 == "Non-Hispanic White"] <- "1 Non-Hispanic White"
covariates$RIDRETH1[covariates$RIDRETH1 == "Non-Hispanic Black"] <- "2 Non-Hispanic Black"
covariates$RIDRETH1[covariates$RIDRETH1 == "Mexican American"] <- "3 Mexican American"
covariates$RIDRETH1[covariates$RIDRETH1 == "Other Hispanic"] <- "4 Other Hispanic"
covariates$RIDRETH1[covariates$RIDRETH1 == "Other Race - Including Multi-Racial"] <- "5 Other"

#substr(my_string, 1, 1)
covariates$DMDBORN4[substr(covariates$DMDBORN4,1,1) == "B"] <- "1 USA"
covariates$DMDBORN4[substr(covariates$DMDBORN4,1,1) == "O"] <- "2 Others"

FIPR_cat <- rep(NA, nrow(covariates))
FIPR_cat[covariates$INDFMPIR <= 1.5] <- "1 Less than 1.5"
FIPR_cat[covariates$INDFMPIR >= 1.5] <- "2 Between 1.5 and 2.9"
FIPR_cat[covariates$INDFMPIR > 3.0] <- "3 Greater than 3.0"

#AGE_cat <- cut(covariates$RIDAGEYR, breaks=c(-Inf, 69.9, 79.9, 89.9, Inf), labels=FALSE)
AGE_cat <- rep(NA, nrow(covariates))
AGE_cat[covariates$RIDAGEYR >= 60] <- "60-69"
AGE_cat[covariates$RIDAGEYR >= 70] <- "70-79"
AGE_cat[covariates$RIDAGEYR >= 80] <- "80 & up"

#BMI Categories: (from https://www.nhlbi.nih.gov/health/educational/lose_wt/BMI/bmicalc.htm)
#Underweight = <18.5
#Normal weight = 18.5–24.9
#Overweight = 25–29.9
#Obesity = BMI of 30 or greater
BMI_cat <- rep(NA, nrow(covariates))
BMI_cat[covariates$BMXBMI <= 18.5] <- "1 Underwieght"
BMI_cat[covariates$BMXBMI >= 18.5] <- "0 Normal"
BMI_cat[covariates$BMXBMI >= 25.0] <- "2 Overweight"
BMI_cat[covariates$BMXBMI >= 30.0] <- "3 Obese"
change <- covariates$BMXBMI == "Refused" | covariates$BMXBMI == "Don't know"
BMI_cat[change] <- "8 Non-Responsive"
BMI_cat[is.na(covariates$BMXBMI)] <- "9 Missing"

#SMQ020 - Smoked at least 100 cigarettes in life
#SMQ040 - Do you now smoke cigarettes
#SMD650 - Avg # cigarettes/day during past 30 days
SMQ_cat <- rep(NA, nrow(covariates))
SMQ_cat[covariates$SMQ020 == "No"] <- "0 Never smoked"
SMQ_cat[covariates$SMQ040 == "Every day"] <- "2 Current smoker"
SMQ_cat[covariates$SMQ040 == "Some days"] <- "2 Current smoker"
SMQ_cat[covariates$SMQ040 == "Not at all" & covariates$SMQ020 == "Yes" ] <- "1 Former smoker"
change <- covariates$SMQ040 == "Refused" | covariates$SMQ040 == "Don't know"
SMQ_cat[change] <- "8 Non-Responsive"
SMQ_cat[is.na(SMQ_cat)] <- "9 Missing"

ALQ_cat <- rep(NA, nrow(covariates))
ALQ_cat[covariates$ALQ110 == "No"]<- "0 Less than 12"
ALQ_cat[covariates$ALQ110 == "Yes"] <- "1 More than 12"
change <- covariates$ALQ110 == "Refused" | covariates$ALQ110 == "Don't know"
ALQ_cat[change] <- "8 Non-Responsive"
ALQ_cat[is.na(ALQ_cat)] <- "9 Missing"

DIQ_cat <- rep(NA, nrow(covariates))
DIQ_cat[covariates$DIQ010 == "No"] <- "0 No"
DIQ_cat[covariates$DIQ010 == "Borderline"] <- "1 Borderline"
DIQ_cat[covariates$DIQ010 == "Yes"] <- "2 Yes"
change <- covariates$DIQ010 == "Refused" | covariates$DIQ010 == "Don't know"
DIQ_cat[change] <- "8 Non-Responsive"
DIQ_cat[is.na(DIQ_cat)] <- "9 Missing"

covariates$Age_cat <- AGE_cat
covariates$BMI_cat <- BMI_cat
covariates$SMQ_cat <- SMQ_cat
covariates$ALQ_cat <- ALQ_cat
covariates$DIQ_cat <- DIQ_cat
covariates$FIPR_cat <- FIPR_cat

covariates$DMDEDUC2[startsWith(covariates$DMDEDUC2,"Le")] <- "01 Less than 9th grade"
covariates$DMDEDUC2[startsWith(covariates$DMDEDUC2,"9-")] <- "02 9-11th grade (Includes 12th grade with no diploma)"
covariates$DMDEDUC2[startsWith(covariates$DMDEDUC2,"Hi")] <- "03 High school graduate/GED or equivalent"
covariates$DMDEDUC2[startsWith(covariates$DMDEDUC2,"So")] <- "04 Some college or AA degree"
covariates$DMDEDUC2[startsWith(covariates$DMDEDUC2,"Co")] <- "05 College graduate or above"
change <- covariates$DMDEDUC2 == "Refused" | covariates$DMDEDUC2 == "Don't know"
covariates$DMDEDUC2[change] <- "8 Non-Responsive"
covariates$DMDEDUC2[is.na(covariates$DMDEDUC2)] <- "9 Missing"

keep <- c("SEQN", "SDDSRVYR", "RIDSTATR", "WTINT2YR", "WTMEC2YR", "SDMVPSU", 
          "SDMVSTRA", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDRETH3", 
          "DMDBORN4", "DMDEDUC2", "DMDMARTL", 
          "Age_cat", "BMI_cat", "SMQ_cat", "ALQ_cat", "DIQ_cat", "FIPR_cat")
covariates <- select(covariates, all_of(keep))
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

#write cleaned data to file
write.csv(file=paste0(dir_out,"Covariates_cleaned.csv"), x=covariates, row.names = FALSE)
