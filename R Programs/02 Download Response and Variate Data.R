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
# Cognitive Function
######
#SEQN - Respondent sequence number
#CFASTAT - Cognitive functioning status
#CFALANG - Language - Cognitive Functioning
#CFDCCS - CERAD: Completion Status
#CFDCRNC - CERAD: Reason Not Complete
#CFDCST1 - CERAD: Score Trial 1 Recall
#CFDCST2 - CERAD: Score Trial 2 Recall
#CFDCST3 - CERAD: Score Trial 3 Recall
#CFDCSR - CERAD: Score Delayed Recall
#CFDCIT1 - CERAD: Intrusion word count Trial 1
#CFDCIT2 - CERAD: Intrusion word count Trial 2
#CFDCIT3 - CERAD: Intrusion word count Trial 3
#CFDCIR - CERAD: Intrusion word count Recall
#CFDAPP - Animal Fluency: Sample Practice Pretest
#CFDARNC - Animal Fluency: Reason Not Done
#CFDAST - Animal Fluency: Score Total
#CFDDPP - Digit Symbol: Sample Practice Pretest
#CFDDRNC - Digit Symbol: Reason Not Done
#CFDDS - Digit Symbol: Score

#for responses in analysis use:
#CFDCSR - CERAD: Score Delayed Recall (higher is higher CF)
#CFDCIR - CERAD: Intrusion word count Recall (lower if higher CF)
#CFDAST - Animal Fluency: Score Total (higher is higher CF)
#CFDDS - Digit Symbol: Score (higher is higher CF)

CFQ <- read_NHANES("CFQ_G","CFQ_H", "Common")
write.csv(x=CFQ, file=paste0(dir_out,"CFQ.csv"), row.names=FALSE)
print("CFQ ok")

######
# PFAS (serum)
######
#SEQN - Respondent sequence number
#WTSB2YR - Subsample B weights
#LBXPFDE - Perfluorodecanoic acid (ng/mL)
#LBDPFDEL - Perfluorodecanoic acid Comment Code
#LBXPFHS - Perfluorohexane sulfonic acid (ng/mL)
#LBDPFHSL - Perfluorohexane sulfonic acid Comt Code
#LBXMPAH - 2-(N-methyl-PFOSA)acetic acid (ng/mL)
#LBDMPAHL - 2-(N-methyl-PFOSA) acetic acid Comt Code
#LBXPFNA - Perfluorononanoic acid (ng/mL)
#LBDPFNAL - Perfluorononanoic acid Comment Code
#LBXPFUA - Perfluoroundecanoic acid (ng/mL)
#LBDPFUAL - Perfluoroundecanoic acid Comment Code
#LBXPFDO - Perfluorododecanoic acid (ng/mL)
#LBDPFDOL - Perfluorododecanoic acid comment
data1 <- nhanes("PFC_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("PFAS_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
PFAS <- rbind(data1, data2)
write.csv(x=PFAS, file=paste0(dir_out,"PFAS.csv"), row.names=FALSE)
print("PFAS ok")

######
# Plasticers (urine)
######
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights (is WTSB2YR in PHTHTE_H)
#URXCNP - Mono(carboxynonyl) Phthalate (ng/mL)
#URDCNPLC - Mono(carboxynonyl) Phthalate comment
#URXCOP - Mono(carboxyoctyl) Phthalate (ng/mL)
#URDCOPLC - Mono(carboxyoctyl) Phthalate comment
#URXECP - Mono-2-ethyl-5-carboxypentyl phthalate
#URDECPLC - Mono-2ethyl5carboxypentyl phthalate cmt
#URXMBP - Mono-n-butyl phthalate (ng/mL)
#URDMBPLC - Mono-n-butyl phthalate comment code
#URXMC1 - Mono-(3-carboxypropyl) phthalate (ng/mL)
#URDMC1LC - Mono-(3-carboxypropyl) phthalate cmt
#URXMEP - Mono-ethyl phthalate (ng/mL)
#URDMEPLC - Mono-ethyl phthalate comment code
#URXMHH - Mono-(2-ethl-5-hydrxhxyl) phthte (ng/mL)
#URDMHHLC - Mono(2ethyl5hydroxyhexyl) phthalate cmt
#URXMHP - Mono-(2-ethyl)-hexyl phthalate (ng/mL)
#URDMHPLC - Mono-(2-ethyl)-hexyl phthalate comment
#URXMNP - Mono-isononyl phthalate (ng/mL)
#URDMNPLC - Mono-isononyl phthalate comment code
#URXMOH - Mono-(2-ethyl-5-oxohexyl) phthalate
#URDMOHLC - Mono-(2-ethyl-5-oxohexyl) phthalate cmt
#URXMZP - Mono-benzyl phthalate (ng/mL)
#URDMZPLC - Mono-benzyl phthalate comment code
#URXMIB - Mono-isobutyl phthalate
#URDMIBLC - Mono-isobutyl phthalate comment code
#URXMHNC - MHNC (ng/mL)
#URDMCHLC - MHNC comment code
#URXUCR - Creatinine, urine (mg/dL)
data1 <- nhanes("PHTHTE_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("PHTHTE_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
PHTHTE <- rbind(data1, data2)
write.csv(x=PHTHTE, file=paste0(dir_out,"PHTHTE.csv"), row.names=FALSE)
print("PHTHTE ok")

######
# PAH (urine)
######
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXP01 - 1-hydroxynaphthalene (ng/L)
#URDP01LC - 1-hydroxynaphthalene comment code
#URXP02 - 2-hydroxynaphthalene (ng/L)
#URDP02LC - 2-hydroxynaphthalene comment code
#URXP03 - 3-hydroxyfluorene (ng/L)
#URDP03LC - 3-hydroxyfluorene comment code
#URXP04 - 2-hydroxyfluorene (ng/L)
#URDP04LC - 2-hydroxyfluorene comment code
#URXP06 - 1-hydroxyphenanthrene (ng/L)
#URDP06LC - 1-hydroxyphenanthrene comment code
#URXP10 - 1-hydroxypyrene (ng/L)
#URDP10LC - 1-hydroxypyrene comment code
#URXUCR - Urinary creatinine
data1 <- nhanes("PAH_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("PAH_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
PAH <- rbind(data1,data2)
write.csv(x=PAH, file=paste0(dir_out,"PAH.csv"), row.names=FALSE)
print("PAH ok")

######
# Urinary metals
######
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXUBA - Barium, urine (ug/L)
#URDUBALC - Urinary Barium Comment Code
#URXUCD - Cadmium, urine (ug/L)
#URDUCDLC - Urinary Cadmium Comment Code
#URXUCO - Cobalt, urine (ug/L)
#URDUCOLC - Urinary Cobalt Comment Code
#URXUCS - Cesium, urine (ug/L)
#URDUCSLC - Urinary Cesium Comment Code
#URXUMO - Molybdenum, urine (ug/L)
#URDUMOLC - Urinary Molybdenum Comment Code
#URXUMN - Manganese, urine (ug/L)
#URDUMNLC - Urinary Mn Comment Code
#URXUPB - Lead, urine (ug/L)
#URDUPBLC - Urinary Lead Comment Code
#URXUSB - Antimony, urine (ug/L)
#URDUSBLC - Urinary Antimony Comment Code
#URXUSN - Tin, urine (ug/L)
#URDUSNLC - USN Comment Code
#URXUSR - Strontium, urine (ug/L)
#URDUSRLC - USR Comment Code
#URXUTL - Thallium, urine (ug/L)
#URDUTLLC - Urinary Thallium Comment Code
#URXUTU - Tungsten, urine (ug/L)
#URDUTULC - Urinary Tungsten Comment Code
#URXUUR - Uranium, urine (ug/L)
#URDUURLC - Urinary Uranium Comment Code
data1 <- nhanes("UHM_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("UM_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
UM <- rbind(data1,data2)
write.csv(x=UM, file=paste0(dir_out,"UM.csv"), row.names=FALSE)
print("UM ok")

######
# Total Arsenic (urine)
######
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXUAS - Urinary arsenic, total (ug/L)
#URDUASLC - Urinary arsenic, total Comment Code
data1 <- nhanes("UAS_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("UTAS_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
UAT <- rbind(data1,data2)
write.csv(x=UAT, file=paste0(dir_out,"UAT.csv"), row.names=FALSE)
print("UAT ok")

######
# Speciated Arsenic
######
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXUCR - Creatinine, urine (mg/dL)
#URXUAS3 - Urinary Arsenous acid (ug/L)
#URDUA3LC - Urinary Arsenous acid comment code
#URXUAS5 - Urinary Arsenic acid (ug/L)
#URDUA5LC - Urinary Arsenic acid comment code
#URXUAB - Urinary Arsenobetaine (ug/L)
#URDUABLC - Urinary Arsenobetaine comment code
#URXUAC - Urinary Arsenocholine (ug/L)
#URDUACLC - Urinary Arsenocholine comment code
#URXUDMA - Urinary Dimethylarsinic acid (ug/L)
#URDUDALC - Urinary Dimethylarsinic acid comment cod
#URXUMMA - Urinary Monomethylarsonic acid (ug/L)
#URDUMMAL - Urinary MMA acid comment code
data1 <- nhanes("UAS_G", translated = TRUE, cleanse_numeric = TRUE)
data2 <- nhanes("UAS_H", translated = TRUE, cleanse_numeric = TRUE)
colnames(data2)[2] <- colnames(data1)[2]               #change colnames to match for weights
common <- intersect(colnames(data1), colnames(data2))
data1 <- select(data1, all_of(common))
data2 <- select(data2, all_of(common))
UAS <- rbind(data1,data2)
write.csv(x=UAS, file=paste0(dir_out,"UAS.csv"), row.names=FALSE)
print("UAS ok")

######
# Creatinine
######
#SEQN - Respondent sequence number
#URXUMA - Albumin, urine (ug/mL)
#URXUMS - Albumin, urine (mg/L)
#URDUMALC - Albumin, urine comment code
#URXUCR - Creatinine, urine (mg/dL)
#URXCRS - Creatinine, urine (umol/L)
#URDUCRLC - Creatinine, urine comment code
#URDACT - Albumin creatinine ratio (mg/g)

keepers <- c("SEQN","URXUCR")
ALB_CR <- read_NHANES("ALB_CR_G","ALB_CR_H", keepers)
write.csv(x=ALB_CR, file=paste0(dir_out,"ALB_CR.csv"), row.names=FALSE)
print("ALB_CR ok")