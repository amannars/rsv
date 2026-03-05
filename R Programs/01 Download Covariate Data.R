#clean/clear workspace
rm(list = ls())          #clear workspace variables
gc()                     #garbage collection
cat("\014")              #clear console

#install required libraries
library(nhanesA)
library(dplyr)

#This function is used to read the NHANES data
#I change it as needed when additional/different cycles are needed
read_NHANES <- function(file1, file2, keepers="ALL"){
  data1 <- nhanes(file1, translated = TRUE, cleanse_numeric = TRUE)
  data2 <- nhanes(file2, translated = TRUE, cleanse_numeric = TRUE)
  if (keepers[1] == "ALL") {                          #keep all the data
    all_data <- rbind(data1, data2)
  }
  else if(keepers[1] == "Common"){                 #set keepers=="Common"
    common <- intersect(colnames(data1), colnames(data2))
    data1 <- select(data1, all_of(common))
    data2 <- select(data2, all_of(common))
    all_data <- rbind(data1, data2)
  } else {                                       #keepers specified
    data1 <- select(data1, all_of(keepers))
    data2 <- select(data2, all_of(keepers))
    all_data <- rbind(data1, data2)
  }
  return(all_data)
}

#Paste into other programs then change as needed
path1 <- "~/Desktop/Cognitive Function v PFAS"
if(!dir.exists(path1)){
  dir.create(path1)
  print(paste("Directory Created:", path1))
}
path1 <- paste0(path1,"/")

path2 <- paste0(path1,"/01 Download Data")
if(!dir.exists(path2)){
  dir.create(path2)
  print(paste("Directory Created:", path2))
}
dir_out <- paste0(path2,"/")
  

######
# Demographics
######
#SEQN - Respondent sequence number
#SDMVPSU - Masked variance pseudo-PSU
#SDMVSTRA - Masked variance pseudo-stratum
#SDDSRVYR - Data release cycle
#RIAGENDR - Gender
#RIDAGEYR - Age in years at screening  
#RIDRETH1 - Race/Hispanic origin
#RIDRETH3 - Race/Hispanic origin w/ NH Asian
#DMDMARTL - Marital status
#DMDEDUC2 - Education level - Adults 20+
#RIDEXPRG - Pregnancy status at exam
#DMDBORN4 - Country of birth
#DMDYRSUS - Length of time in US
#INDFMPIR - Ratio of family income to poverty
keepers <- c("SEQN", "SDDSRVYR", "RIDSTATR", "WTINT2YR", "WTMEC2YR", 
             "SDMVPSU", "SDMVSTRA", "RIAGENDR", "RIDAGEYR", "RIDRETH1", 
             "RIDRETH3", "DMDMARTL", "DMDEDUC2", "RIDEXPRG", "DMDBORN4", 
             "DMDYRSUS", "INDFMPIR")
DEMO <- read_NHANES("DEMO_G", "DEMO_H", keepers)
write.csv(x=DEMO, file=paste0(dir_out,"DEMO.csv"), row.names=FALSE)
print("DEMO ok")

######
# BMI
######
# BMX_I
# SEQN - Respondent sequence number
# BMXBMI - Body Mass Index (kg/m**2)
keepers <- c("SEQN","BMXBMI")
BMI <- read_NHANES("BMX_G", "BMX_H", keepers)
write.csv(x=BMI, file=paste0(dir_out,"BMI.csv"), row.names=FALSE)
print("BMI ok")

#######
# Cigarette use
#######
#SEQN - Respondent sequence number
#SMQ020 - Smoked at least 100 cigarettes in life
#SMQ040 - Do you now smoke cigarettes?
#SMD650 - Avg # cigarettes/day during past 30 days

keepers <- c("SEQN", "SMQ020", "SMQ040", "SMD650")
SMQ <- read_NHANES("SMQ_G", "SMQ_H", keepers)
write.csv(x=SMQ, file=paste0(dir_out,"SMQ.csv"), row.names=FALSE)
print("SMQ ok")

#######
# Alcohol use
#######
#SEQN - Respondent sequence number
#ALQ101 - Had at least 12 alcohol drinks/1 yr?
#ALQ110 - Had at least 12 alcohol drinks/lifetime?
#ALQ120Q - How often drink alcohol over past 12 mos
#ALQ120U - # days drink alcohol per wk, mo, yr
#ALQ130 - Avg # alcoholic drinks/day - past 12 mos
#ALQ141Q - # days have 4/5 drinks - past 12 mos
#ALQ141U - # days per week, month, year?
#ALQ151 - Ever have 4/5 or more drinks every day?
ALQ <- read_NHANES("ALQ_G", "ALQ_H", keepers="Common")
write.csv(x=ALQ, file=paste0(dir_out,"ALQ.csv"), row.names=FALSE)
print("ALQ ok")

######
# Diabetes
######
#SEQN - Respondent sequence number
#DIQ010 - Doctor told you have diabetes
#DID040 - Age when first told you had diabetes
#DIQ219 - CHECK ITEM
#DIQ220 - When was your diabetes diagnosed
#DIQ159 - CHECK ITEM
#DIQ160 - Ever told you have prediabetes
#DIQ170 - Ever told have health risk for diabetes
#DIQ180 - Had blood tested past three years
#DIQ190A - Past yr told control weight
#DIQ190B - Past yr told increase physical activity
#DIQ190C - Past yr told reduce fat/calories in diet
#DIQ200A - Are you controlling weight
#DIQ200B - Are you increasing physical activity
#DIQ200C - Are you reducing fat/calories in diet
#DIQ050 - Taking insulin now
keepers <- c("SEQN","DIQ010")
DIQ <- read_NHANES("DIQ_G", "DIQ_H", keepers)
write.csv(x=DIQ, file=paste0(dir_out,"DIQ.csv"), row.names=FALSE)
print("DIQ ok")