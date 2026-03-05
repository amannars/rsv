


PAH <- read.csv(file=paste0(dir_raw,"PAH.csv"), header=TRUE)            #urine concentrations
PAH_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("WTSA2YR", "Subsample A Weights"),
                   c("URXP01", "1-Hydroxynaphthalene"),
                   c("URXP02", "2-Hydroxynaphthalene"),
                   c("URXP03", "3-Hydroxyfluorene"),
                   c("URXP04", "2-Hydroxyfluorene"),
                   c("URXP06", "1-Hydroxyphenanthrene"),
                   c("URXP10", "1-Hydroxypyrene"),
                   c("URXUCR", "Creatinine, urine"))
keep <<- PAH_names[,1]
PAH <- select(PAH, all_of(keep))
change <- startsWith(colnames(PAH),"URXP")
PAH_N <- PAH
PAH_N[,3:8] <- PAH[,3:8]/PAH$URXUCR

PHTHTE <- read.csv(file=paste0(dir_raw,"PHTHTE.csv"), header=TRUE)      #urine concentrations
PHTHTE_names <- rbind(c("SEQN", "Respondent sequence number"),
                      c("URXCNP", "Mono(carboxynonyl) Phthalate"),
                      c("URXCOP", "Mono(carboxyoctyl) Phthalate"),
                      c("URXECP", "Mono-2-ethyl-5-carboxypentyl phthalate"),
                      c("URXMBP", "Mono-n-butyl phthalate"),
                      c("URXMC1", "Mono-(3-carboxypropyl) phthalate"),
                      c("URXMEP", "Mono-ethyl phthalate"),
                      c("URXMHH", "Mono-(2-ethl-5-hydrxhxyl) phthte"),
                      c("URXMHP", "Mono-(2-ethyl)-hexyl phthalate"),
                      #c("URXMNM", "Mono-n-methyl phthalate"),
                      c("URXMNP", "Mono-isononyl phthalate"),
                      c("URXMOH", "Mono-(2-ethyl-5-oxohexyl) phthalate"),
                      c("URXMZP", "Mono-benzyl phthalate"),
                      c("URXMIB", "Mono-isobutyl phthalate"),
                      c("URXMHNC", "MHNC"),
                      c("URXUCR", "Creatinine, urine"))
keep <<- PHTHTE_names[,1]
PHTHTE <- select(PHTHTE, all_of(keep))
change <- startsWith(colnames(PAH),"URXP")
PHTHTE_N <- PHTHTE
PHTHTE_N[,2:14] <- PHTHTE[,2:14]/PHTHTE_N$URXUCR

UM <- read.csv(file=paste0(dir_raw,"UM.csv"), header=TRUE)              #urine concentrations
UM_names <- rbind(c("SEQN", "Respondent sequence number"),
                  c("WTSA2YR", "Subsample A weights"),
                  c("URXUCR", "Creatinine, urine"),
                  c("URXUBA", "Barium"),
                  c("URXUCD", "Cadmium"),
                  c("URXUCO", "Cobalt"),
                  c("URXUCS", "Cesium"),
                  c("URXUMO", "Molybdenum"),
                  c("URXUMN", "Manganese"),
                  c("URXUPB", "Lead"),
                  c("URXUSB", "Antimony"),
                  c("URXUSN", "Tin"),
                  c("URXUSR", "Strontium"),
                  c("URXUTL", "Thallium"),
                  c("URXUTU", "Tungsten"),
                  c("URXUUR", "Uranium"))
keep <<- UM_names[,1]
UM <- select(UM, all_of(keep))
UM_N <- UM
UM_N[,4:16] <- UM_N[,4:16]/UM$URXUCR

UAT <- read.csv(file=paste0(dir_raw,"UAT.csv"), header=TRUE)            #urine concentrations
UAT_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("URXUAS", "Arsenic, total"))
keep <<- UAT_names[,1]
UAT <- select(UAT, all_of(keep))

UAS <- read.csv(file=paste0(dir_raw,"UAS.csv"), header=TRUE)            #urine concentrations
UAS_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("WTSA2YR", "Subsample A Weights"),
                   c("URXUCR", "Creatinine, urine"),
                   c("URXUAS3", "Arsenous acid"),
                   c("URXUAS5", "Arsenic acid"),
                   c("URXUAB", "Arsenobetaine"),
                   c("URXUAC", "Arsenocholine"),
                   c("URXUDMA", "Dimethylarsinic acid"),
                   c("URXUMMA", "Monomethylarsonic acid"))
#c("URXUTM", "Trimethylarsine Oxide"))
keep <<- UAS_names[,1]
UAS <- select(UAS, all_of(keep))

UAs <- merge(UAS, UAT)
UAs_names <- rbind(c("SEQN", "Respondent sequence number"),
                   c("WTSA2YR", "Subsample A Weights"),
                   c("URXUCR", "Creatinine, urine"),
                   c("URXUAS3", "Arsenous acid"),
                   c("URXUAS5", "Arsenic acid"),
                   c("URXUAB", "Arsenobetaine"),
                   c("URXUAC", "Arsenocholine"),
                   c("URXUDMA", "Dimethylarsinic acid"),
                   c("URXUMMA", "Monomethylarsonic acid"),
                   c("URXUAS", "Arsenic, total"))
UAs_N <- UAs
UAs_N[,4:10] <- UAs_N[,4:10]/UAS$URXUCR

ALB_CR <- read.csv(file=paste0(dir_raw,"ALB_CR.csv"), header=TRUE)      #urine concentrations
AC_names <- rbind(c("SEQN", "Respondent sequence number"),
                  c("URXUMA", "Albumin, urine (ug/mL)"),
                  c("URXUMS", "Albumin, urine (mg/L)"),
                  c("URXUCR", "Creatinine, urine (mg/dL)"),
                  c("URXCRS", "Creatinine, urine (umol/L)"),
                  c("URDACT", "Albumin creatinine ratio (mg/g)"))
keep <<- AC_names[,1]
ALB_CR <- select(ALB_CR, all_of(keep))


write.csv(file=paste0(dir_out,"PHTHTE_N.csv"), x=PHTHTE_N, row.names = FALSE)
write.csv(file=paste0(dir_out,"PAH_N.csv"), x=PAH_N, row.names = FALSE)
write.csv(file=paste0(dir_out,"UM_N.csv"), x=UM_N, row.names = FALSE)
write.csv(file=paste0(dir_out,"UAs_N.csv"), x=UAs_N, row.names = FALSE)