######
#This program does low/high 2-level variate regression analysis
######

#clean/clear workspace
rm(list = ls())          #clear workspace variables
gc()                     #garbage collection
cat("\014")              #clear console

#install required (or those I'm to lazy to not include) libraries
library(dplyr)
library(survey)

######
# Copy screen output into Excel for formatting etc.
######

#function to print the regression coefficients in a pretty form
#print only the results for the variate
reg_print <- function(response, variate, glm){
  glm_summary <- summary(glm)
  coeff <- glm_summary$coefficients[,1]
  CI <- confint(glm)
  p_value <- glm_summary$coefficients[,4]
  table_out <- cbind(coeff, CI,p_value)
  stars <- rep(" ", nrow(table_out))
  stars[p_value <= 0.10] <- "*"
  stars[p_value <= 0.05] <- "**"
  table_out <- cbind(variate, response, coeff, CI[,1], CI[,2], p_value, stars)
  return(table_out)
}

#Paste into other programs then change as needed
path1 <- "~/Desktop/Cognitive Function v PFAS"
path2 <- paste0("/02 Cleaned Data/")
dir_in <- paste0(path1, path2)

#read input data
covariates <- read.csv(file=paste0(dir_in,"Covariates_Cleaned.csv"), header=TRUE)
CFQ <- read.csv(file=paste0(dir_in,"CFQ_Cleaned.csv"), header=TRUE)
PFAS <- read.csv(file=paste0(dir_in,"PFAS_Cleaned.csv"), header=TRUE)

#merge into single array
data0 <- merge(CFQ,covariates)
data <- merge(data0, PFAS)
print(paste("Missing CFQ data", nrow(covariates)-nrow(data0)))
print(paste("Almost Final Participant Count",nrow(data0)))
print(paste("Missing PFAS data", nrow(data0)-nrow(data)))
print(paste("Final Participant Count",nrow(data)))

#names for reference and labeling output
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

PFAS_names <- rbind(c("SEQN", "Respondent sequence number"),
                    c("WTSB2YR", "Subsample B weights"),
                    c("LBXPFDE", "Perfluorodecanoic acid (ug/L)"),
                    c("LBDPFDEL", "Perfluorodecanoic acid comment"),
                    c("LBXPFHS", "Perfluorohexane sulfonic acid (ug/L)"),
                    c("LBDPFHSL", "Perfluorohexane sulfonic acid comment"),
                    c("LBXMPAH", "2-(N-methyl-PFOSA) acetate (ug/L)"),
                    c("LBDMPAHL", "2-(N-methyl-PFOSA) acetate comment"),
                    c("LBXPFBS", "Perfluorobutane sulfonic acid (ug/L)"),
                    c("LBDPFBSL", "Perfluorobutane sulfonic acid comment"),
                    c("LBXPFHP", "Perfluoroheptanoic acid (ug/L)"),
                    c("LBDPFHPL", "Perfluoroheptanoic acid comment"),
                    c("LBXPFNA", "Perfluorononanoic acid (ug/L)"),
                    c("LBDPFNAL", "Perfluorononanoic acid comment"),
                    c("LBXPFUA", "Perfluoroundecanoic acid (ug/L)"),
                    c("LBDPFUAL", "Perfluoroundecanoic acid comment"),
                    c("LBXPFDO", "Perfluorododecanoic acid (ug/L)"),
                    c("LBDPFDOL", "Perfluorododecanoic acid comment"))

VAR_names <- PFAS_names[seq(from=3, to=nrow(PFAS_names), by=2),]

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

#correct weights for multiple cycles
my_wts <- data$WTMEC2YR/2      #change the divsor if different number of cycles
data$my_wts <- my_wts

#loop through the cognitive function responses
for (i_resp in 2:nrow(CFQ_names)){  #2:nrow(CFQ_names)
  #put response data into desired column
  col_match <- colnames(data) == CFQ_names[i_resp,1]
  this_response <- data[,col_match]
  data$RESPONSE <- this_response
  
  #loop through variates (PFAS species)
  table_out <- c()
  for (i in 1:length(VAR_names[,1])){   #1:length(VAR_names[,1])
    #get data for this variate into desired column
    col_match <- colnames(data) == VAR_names[i]
    this_variate <- data[,col_match]
    data$VARIATE <- this_variate
    
    #loop through quantiles
    for (i_quant in seq(from=0.05, to=0.95, by=0.05)){   #seq(from=0.05, to=0.95, by=0.05)
      break_pt <- quantile(x=data$VARIATE, probs=i_quant, na.rm=TRUE, names=FALSE)
      
      #set new levels of variate
      VAR_quant <- rep(NA, nrow(data))
      VAR_quant[data$VARIATE <= break_pt] <- 0
      VAR_quant[data$VARIATE > break_pt] <- 1
      data$VAR_quant <- as.factor(VAR_quant)
      
      #get new survey design and do regression analysis
      svy_design <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, 
                              weights=~my_wts, nest=TRUE)
      svy_glm <- svyglm(RESPONSE~VAR_quant + 
                          RIAGENDR+DMDMARTL+RIDRETH1+DMDEDUC2+Age_cat+BMI_cat+
                          FIPR_cat+SMQ_cat+ALQ_cat+DMDBORN4,
                        design=svy_design)
      
      #reformat results in OK manner to print
      this_line <- reg_print(response=CFQ_names[i_resp,2], 
                             variate=VAR_names[i,2], 
                             glm=svy_glm)
      
      #save results if p value < 0.10
      if(this_line[2,6] <= 0.10){
        add_line <- c(this_line[2,1:2], i_quant, break_pt, this_line[2,3:7])
        table_out <- rbind(table_out, add_line)
      }
    }
  }
  
  #print results
  if(length(table_out != 0)){
    fmat <- "%40s   %40s    %20s    %20s    %20s    %20s    %20s    %20s     %4s"
    s1 <- sprintf(fmat,
                  "PFAS_Species","CF_Response","Quantile","Concentration","Value","LCL","UCL","p-value"," ")
    print(s1)
    for (j in 1:nrow(table_out)){
      s1 <- sprintf(fmat,
                    table_out[j,1],table_out[j,2],table_out[j,3],table_out[j,4],table_out[j,5],table_out[j,6],table_out[j,7],table_out[j,8],table_out[j,9])
      print(s1)
    }
  } else {
    print(paste("For",CFQ_names[i_resp,1],"no significant correlations found"))
  }
}