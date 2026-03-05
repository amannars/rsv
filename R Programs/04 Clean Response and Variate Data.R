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

#response - cognitive function
CFQ <- read.csv(file=paste0(dir_in,"CFQ.csv"), header=TRUE)
CFQ_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("CFASTAT", "Cognitive functioning status"),
                   c("CFALANG", "Language - Cognitive Functioning"),
                   c("CFDCCS", "CERAD: Completion Status"),
                   c("CFDCRNC", "CERAD: Reason Not Complete"),
                   c("CFDCST1", "CERAD: Score Trial 1 Recall"),
                   c("CFDCST2", "CERAD: Score Trial 2 Recall"),
                   c("CFDCST3", "CERAD: Score Trial 3 Recall"),
                   c("CFDCSR", "CERAD: Score Delayed Recall"),
                   c("CFDCIT1", "CERAD: Intrusion word count Trial 1"),
                   c("CFDCIT2", "CERAD: Intrusion word count Trial 2"),
                   c("CFDCIT3", "CERAD: Intrusion word count Trial 3"),
                   c("CFDCIR", "CERAD: Intrusion word count Recall"),
                   c("CFDAPP", "Animal Fluency: Sample Practice Pretest"),
                   c("CFDARNC", "Animal Fluency: Reason Not Done"),
                   c("CFDAST", "Animal Fluency: Score Total"),
                   c("CFDDPP", "Digit Symbol: Sample Practice Pretest"),
                   c("CFDDRNC", "Digit Symbol: Reason Not Done"),
                   c("CFDDS", "Digit Symbol: Score"))
#responses are (and there are no non-responsive values):
# "CFDCSR", "CERAD: Score Delayed Recall"
# "CFDCIR", "CERAD: Intrusion word count Recall"
# "CFDAST", "Animal Fluency: Score Total"
# "CFDDS", "Digit Symbol: Score"
keepers <- c("SEQN", "CFDCSR", "CFDCIR", "CFDAST", "CFDDS")
CFQ <- select(CFQ, all_of(keepers))
write.csv(file=paste0(dir_out,"CFQ_cleaned.csv"), x=CFQ, row.names = FALSE)

#Environmental chemicals - PFAS
#since this is serum concentration the data does NOT need to be creatinine normalized
PFAS <- read.csv(file=paste0(dir_in,"PFAS.csv"), header=TRUE)          #serum concentrations
write.csv(file=paste0(dir_out,"PFAS_cleaned.csv"), x=PFAS, row.names = FALSE)