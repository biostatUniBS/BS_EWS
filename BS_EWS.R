rm(list=ls());
library(readxl)

library(MASS)
library(Hmisc)
library(ppcor)
library(corrplot)

library(cluster)
library(randomForest)

library(tidyverse)
library(arsenal)


if(Sys.getenv("RSTUDIO") == "1")
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load file ####
maroldi_score<-read_excel("ricoveri_outcome_score_tamponi.xlsx",sheet="Controllo")
str(maroldi_score)
dim(maroldi_score)

maroldi_score_all<-read_excel("ricoveri_outcome_score_tamponi.xlsx",sheet="Sheet1")
str(maroldi_score_all)
dim(maroldi_score_all)
months_march_april<-c("Marzo","Aprile")
data_maroldi_M_A_1_score<-filter(maroldi_score_all, Mese %in% months_march_april, Numero_esame==1)
dim(data_maroldi_M_A_1_score)

# save(data_maroldi_M_A_1_score, file="data_maroldi_M_A_1_score.Rda")


ferritina<-read_excel("Ferritina.xlsx",sheet="Sheet1")
str(ferritina)
names(ferritina)
names(ferritina)[2]<-"Ferritin"

d_dimero<-read_excel("ddimero.xlsx",sheet="Sheet1")
str(d_dimero)
names(d_dimero)
names(d_dimero)[2]<-"D_dimer"

fibrinogeno<-read_excel("Fibrinogeno.xlsx",sheet="Sheet1")
str(fibrinogeno)
names(fibrinogeno)
names(fibrinogeno)[2]<-"Fibrinogen"
summary(fibrinogeno)

LDH<-read_excel("LDH.xlsx",sheet="Sheet1")
str(LDH)

neutrofili_linfociti<-read_excel("Neutrofili_linfociti e neutrofili su linfociti.xlsx",sheet="Sheet1")
str(neutrofili_linfociti)
names(neutrofili_linfociti)
names(neutrofili_linfociti)[2]<-"Neutrophils"
names(neutrofili_linfociti)[3]<-"Lymphocytes"
names(neutrofili_linfociti)[4]<-"Neutrophils_Lymphocytes"

linfociti_perc<-read_excel("Linfociti_perc.xlsx",sheet="Sheet1")
str(linfociti_perc)
names(linfociti_perc)[2]<-"Lymphocytes_perc"

pcr<-read_excel("PCR.xlsx",sheet="Sheet1")
str(pcr)

wbc<-read_excel("WBC.xlsx",sheet="Sheet1")
str(wbc)

#tampone<-read_excel("16 nov tampone.xlsx",sheet="Sheet1")
#str(tampone)
#names(tampone)[2]<-"Swab"

basofili<-read_excel("basofili.xlsx",sheet="Sheet1")
str(basofili)
names(basofili)[2]<-"Basophils"

basofili_perc<-read_excel("Basofili_perc.xlsx",sheet="Sheet1")
str(basofili_perc)
names(basofili_perc)[2]<-"Basophils_perc"

eosinofili<-read_excel("Eosinofili.xlsx",sheet="Sheet1")
str(eosinofili)
names(eosinofili)[2]<-"Eosinophils"

eosinofili_perc<-read_excel("Eosinofili_perc.xlsx",sheet="Sheet1")
str(eosinofili_perc)
names(eosinofili_perc)[2]<-"Eosinophils_perc"

monociti<-read_excel("Monociti.xlsx",sheet="Sheet1")
str(monociti)
names(monociti)[2]<-"Monocytes"

monociti_perc<-read_excel("Monociti_perc.xlsx",sheet="Sheet1")
str(monociti_perc)
names(monociti_perc)[2]<-"Monocytes_perc"

neutrofili_perc<-read_excel("Neutrofili_perc.xlsx",sheet="Sheet1")
str(neutrofili_perc)
names(neutrofili_perc)[2]<-"Neutrophils_perc"

troponina<-read_excel("Troponina.xlsx",sheet="Sheet1")
str(troponina)
names(troponina)[2]<-"Troponin"


# left join - da maggio a Dicembre####
data_1<-left_join(maroldi_score, ferritina, by = "Code")
str(data_1)
dim(data_1)
names(data_1)


data_1<-left_join(data_1, d_dimero, by = "Code")
str(data_1)
dim(data_1)
names(data_1)

data_1<-left_join(data_1, fibrinogeno, by = "Code")
str(data_1)
dim(data_1)
names(data_1)

data_1<-left_join(data_1, LDH, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, neutrofili_linfociti, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, linfociti_perc, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, pcr, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, wbc, by = "Code")
str(data_1)
dim(data_1)

#data_1<-left_join(data_1, tampone, by = "Code")
#str(data_1)
#dim(data_1)

data_1<-left_join(data_1, basofili, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, basofili_perc, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, eosinofili, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, eosinofili_perc, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, monociti, by = "Code")
str(data_1)
dim(data_1)

data_1<-left_join(data_1, monociti_perc, by = "Code")
str(data_1)
dim(data_1)

data_maroldi<-left_join(data_1, neutrofili_perc, by = "Code")
str(data_1)
dim(data_1)

# data_maroldi<-left_join(data_1, troponina, by = "Code")
# str(data_maroldi)
# dim(data_maroldi)


## Intervalli di riferimento Ferritina ####
## - M: 30 - 400
## - F: 13 - 150

#### STD ferritina

lowerF<-13
UpperF<-150
lowerM<-30
UpperM<-400
muF<-mean(c(lowerF,UpperF))
muM<-mean(c(lowerM,UpperM))
sdF<-(UpperF-muF)/3
sdM<-(UpperM-muM)/3
data_maroldi$Ferritin_std<-ifelse(data_maroldi$Sesso=="F", (data_maroldi$Ferritin-muF)/sdF,(data_maroldi$Ferritin-muM)/sdM)

str(data_maroldi)
dim(data_maroldi)

# left join - all####
data_1_all<-left_join(maroldi_score_all, ferritina, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, d_dimero, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, fibrinogeno, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, LDH, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, neutrofili_linfociti, by = "Code")
str(data_1_all)
dim(data_1_all)
names(data_1)

data_1_all<-left_join(data_1_all, linfociti_perc, by = "Code")
str(data_1_all)
dim(data_1_all)
names(data_1_all)

data_1_all<-left_join(data_1_all, pcr, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, wbc, by = "Code")
str(data_1_all)
dim(data_1_all)

# data_1_all<-left_join(data_1_all, tampone, by = "Code")
# str(data_1_all)
# dim(data_1_all)

data_1_all<-left_join(data_1_all, basofili, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, basofili_perc, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, eosinofili, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, eosinofili_perc, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, monociti, by = "Code")
str(data_1_all)
dim(data_1_all)

data_1_all<-left_join(data_1_all, monociti_perc, by = "Code")
str(data_1_all)
dim(data_1_all)

data_maroldi_all<-left_join(data_1_all, neutrofili_perc, by = "Code")
str(data_maroldi_all)
dim(data_maroldi_all)

# data_maroldi_all<-left_join(data_1_all, troponina, by = "Code")
# str(data_maroldi_all)
# dim(data_maroldi_all)

## Intervalli di riferimento Ferritina in all ####
## - M: 30 - 400
## - F: 13 - 150

#### STD ferritina

lowerF<-13
UpperF<-150
lowerM<-30
UpperM<-400
muF<-mean(c(lowerF,UpperF))
muM<-mean(c(lowerM,UpperM))
sdF<-(UpperF-muF)/3
sdM<-(UpperM-muM)/3
data_maroldi_all$Ferritin_std<-ifelse(data_maroldi_all$Sesso=="F", (data_maroldi_all$Ferritin-muF)/sdF,(data_maroldi_all$Ferritin-muM)/sdM)

str(data_maroldi_all)
dim(data_maroldi_all)
# write.table(data_maroldi_all, file="data_maroldi_all.txt", sep=";")

# Filter on first analysis ####
data_maroldi_all_1<-filter(data_maroldi_all, Numero_esame==1)
dim(data_maroldi_all_1)

#filter on first analysis and alive
data_maroldi_all_1_alive<-filter(data_maroldi_all_1, outcome=="alive")
dim(data_maroldi_all_1_alive)
#filter on first analysis and dead
data_maroldi_all_1_dead<-filter(data_maroldi_all_1, outcome=="dead")
dim(data_maroldi_all_1_dead)

# Filter on first Italian lock-down ####
months_march_april<-c("Marzo","Aprile")
data_maroldi_M_A_1<-filter(data_maroldi_all, Mese %in% months_march_april, Numero_esame==1)
dim(data_maroldi_M_A_1)
# save(data_maroldi_M_A_1, file="data_maroldi1_MA.Rda")
sum(is.na(data_maroldi_M_A_1$Ferritin))

data_maroldi_M_1<-filter(data_maroldi_all, Mese %in% "Marzo", Numero_esame==1)
dim(data_maroldi_M_1)
# save(data_maroldi_M_1, file="data_maroldi1_Marzo.Rda")

data_maroldi_A_1<-filter(data_maroldi_all, Mese %in% "Aprile", Numero_esame==1)
dim(data_maroldi_A_1)
# save(data_maroldi_A_1, file="data_maroldi1_Aprile.Rda")

months_remaining_1<-c("Maggio","Giugno", "Luglio", "Agosto","Settembre","Ottobre","Novembre", "Dicembre")
data_maroldi_remaining_1<-filter(data_maroldi_all, Mese %in% months_remaining_1, Numero_esame==1)
dim(data_maroldi_remaining_1)
names(data_maroldi_remaining_1)
# save(data_maroldi_remaining_1, file="data_maroldi1_MGLASON.Rdata")

months_second_wave<-c("Ottobre","Novembre", "Dicembre")
data_maroldi_second_wave<-filter(data_maroldi_all, Mese %in% months_second_wave, Numero_esame==1)
dim(data_maroldi_second_wave)
names(data_maroldi_second_wave)
# save(data_maroldi_second_wave, file="data_maroldi_second_wave.Rdata")

months_summer<-c("Maggio","Giugno","Luglio", "Agosto", "Settembre")
data_maroldi_summer<-filter(data_maroldi_all, Mese %in% months_summer, Numero_esame==1)
dim(data_maroldi_summer)
names(data_maroldi_summer)
# save(data_maroldi_summer, file="data_maroldi_summer.Rdata")

# Descriptive Statistics on first exam####
#dead-alive
table_1exam<- tableby(~Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils_Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Basophils_perc+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_all_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"), digits=2, digits.count=0)
summary(table_1exam)
write2word(table_1exam,"table_1exam.doc")

table_dead_alive_all_1<- tableby(outcome ~Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils_Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_all_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt")
summary(table_dead_alive_all_1)
write2word(table_dead_alive_all_1,"table_dead_alive_all_1.doc")

table_Sex_all_1<- tableby(Sesso~outcome+Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils/Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_all_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_Sex_all_1)
write2word(table_Sex_all_1,"table_Sex_all_1.doc")


# Descriptive Statistics on March and April - first exam - first wave####
#entire sample
#all
table_dead_alive_M_A_1<- tableby(~Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils_Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_M_A_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),digits=2, digits.count=0)
summary(table_dead_alive_M_A_1)
write2word(table_dead_alive_M_A_1,"table_dead_alive_M_A_1.doc")
#dead-alive
table_dead_alive<- tableby(outcome ~Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils_Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Basophils+Basophils_perc+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_M_A_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt")
summary(table_dead_alive)
write2word(table_dead_alive,"table_dead_alive.doc")

# ferritina è l'unica variabile che dobbiamo stratificare per M e F
table_dead_alive_FERRITINA_F<- tableby(data_maroldi_M_A_1$outcome[data_maroldi_M_A_1$Sesso=="F"] ~data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$Sesso=="F"], numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_dead_alive_FERRITINA_F)
write2word(table_dead_alive_FERRITINA_F,"table_dead_alive_FERRITINA_F.doc")

table_dead_alive_FERRITINA_M<- tableby(data_maroldi_M_A_1$outcome[data_maroldi_M_A_1$Sesso=="M"] ~data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$Sesso=="M"], numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_dead_alive_FERRITINA_M)
write2word(table_dead_alive_FERRITINA_M,"table_dead_alive_FERRITINA_M.doc")

#dead-alive MAY_DECEMBER
table_dead_alive_MD<- tableby(outcome ~Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils_Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Basophils+Basophils_perc+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_remaining_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt")
summary(table_dead_alive_MD)
write2word(table_dead_alive_MD,"table_dead_alive_MD.doc")

# ferritina è l'unica variabile che dobbiamo stratificare per M e F
table_dead_alive_FERRITINA_F_MAY_DEC<- tableby(data_maroldi_remaining_1$outcome[data_maroldi_remaining_1$Sesso=="F"] ~data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$Sesso=="F"], numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_dead_alive_FERRITINA_F_MAY_DEC)
write2word(table_dead_alive_FERRITINA_F_MAY_DEC,"table_dead_alive_FERRITINA_F_MAY_DEC.doc")

table_dead_alive_FERRITINA_M_MAY_DEC<- tableby(data_maroldi_remaining_1$outcome[data_maroldi_remaining_1$Sesso=="M"] ~data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$Sesso=="M"], numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_dead_alive_FERRITINA_M_MAY_DEC)
write2word(table_dead_alive_FERRITINA_M_MAY_DEC,"table_dead_alive_FERRITINA_M_MAY_DEC.doc")
# Sex
table_Sex<- tableby(Sesso~outcome+Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils/Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Basophils+Basophils_perc+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_M_A_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_Sex)
write2word(table_Sex,"table_Sex.doc")

# Sex MAY_DECEMBER
table_Sex_MD<- tableby(Sesso~outcome+Eta_paziente+Sesso+giorni_ricovero+Score+D_dimer+Fibrinogen+LDH+Neutrophils+Lymphocytes+Neutrophils/Lymphocytes+Neutrophils_perc+Lymphocytes_perc+PCR+WBC+Basophils+Basophils_perc+Eosinophils+Eosinophils_perc+Monocytes+Monocytes_perc+Neutrophils_perc, data = data_maroldi_remaining_1, numeric.stats = c("Nmiss","meansd", "medianq1q3", "range"),total=TRUE, digits=2, digits.p=3, digits.count=0, test = TRUE,numeric.test = "kwt",cat.test="fe")
summary(table_Sex_MD)

# some additional information on sample

round(tapply(data_maroldi_M_A_1$Eta_paziente,data_maroldi_M_A_1$Sesso,mean),2)
round(tapply(data_maroldi_M_A_1$Eta_paziente,data_maroldi_M_A_1$Sesso,sd),2)
t.test(data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$outcome=="dead"])
t.test(data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$Sesso=="M"],data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$Sesso=="F"])

# test for table 2

data_maroldi_all_1_alive$Mese_dummy<-ifelse(data_maroldi_all_1_alive$Mese=="Marzo"|data_maroldi_all_1_alive$Mese=="Aprile",0,1)
data_maroldi_all_1_dead$Mese_dummy<-ifelse(data_maroldi_all_1_dead$Mese=="Marzo"|data_maroldi_all_1_dead$Mese=="Aprile",0,1)
fisher.test(table(data_maroldi_all_1_alive$Sesso,data_maroldi_all_1_alive$Mese_dummy))
fisher.test(table(data_maroldi_all_1_dead$Sesso,data_maroldi_all_1_dead$Mese_dummy))

wilcox.test(data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Eta_paziente[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Eta_paziente[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Eta_paziente[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$giorni_ricovero[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$giorni_ricovero[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$giorni_ricovero[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$giorni_ricovero[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Score[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Score[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Score[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Score[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$D_dimer[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$D_dimer[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$D_dimer[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$D_dimer[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Fibrinogen[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Fibrinogen[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Fibrinogen[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Fibrinogen[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$LDH[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$LDH[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$LDH[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$LDH[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Neutrophils[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Neutrophils[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Neutrophils[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Neutrophils[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Lymphocytes[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Lymphocytes[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Lymphocytes[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Lymphocytes[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Neutrophils_Lymphocytes[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Neutrophils_Lymphocytes[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Neutrophils_Lymphocytes[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Neutrophils_Lymphocytes[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Neutrophils_perc[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Neutrophils_perc[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Neutrophils_perc[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Neutrophils_perc[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Lymphocytes_perc[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Lymphocytes_perc[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Lymphocytes_perc[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Lymphocytes_perc[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$PCR[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$PCR[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$PCR[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$PCR[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$WBC[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$WBC[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$WBC[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$WBC[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Basophils[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Basophils[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Basophils[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Basophils[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Basophils_perc[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Basophils_perc[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Basophils_perc[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Basophils_perc[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Eosinophils[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Eosinophils[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Eosinophils[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Eosinophils[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Eosinophils_perc[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Eosinophils_perc[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Eosinophils_perc[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Eosinophils_perc[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Monocytes[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Monocytes[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Monocytes[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Monocytes[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Monocytes_perc[data_maroldi_M_A_1$outcome=="alive"],data_maroldi_remaining_1$Monocytes_perc[data_maroldi_remaining_1$outcome=="alive"])
wilcox.test(data_maroldi_M_A_1$Monocytes_perc[data_maroldi_M_A_1$outcome=="dead"],data_maroldi_remaining_1$Monocytes_perc[data_maroldi_remaining_1$outcome=="dead"])

wilcox.test(data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$outcome=="alive" & data_maroldi_M_A_1$Sesso=="F"],data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$outcome=="alive"&data_maroldi_remaining_1$Sesso=="F"])
wilcox.test(data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$outcome=="dead" & data_maroldi_M_A_1$Sesso=="F"],data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$outcome=="dead"&data_maroldi_remaining_1$Sesso=="F"])

wilcox.test(data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$outcome=="alive" & data_maroldi_M_A_1$Sesso=="M"],data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$outcome=="alive"&data_maroldi_remaining_1$Sesso=="M"])
wilcox.test(data_maroldi_M_A_1$Ferritin[data_maroldi_M_A_1$outcome=="dead" & data_maroldi_M_A_1$Sesso=="M"],data_maroldi_remaining_1$Ferritin[data_maroldi_remaining_1$outcome=="dead"&data_maroldi_remaining_1$Sesso=="M"])



#Correlation between quantitative variables ####
names(data_maroldi_M_A_1)
quantitative_variable<-data_maroldi_M_A_1[,c(10,16,18,20,22,23,24,26,28,30,32,34,36,38,40,42,44,46)]
str(quantitative_variable)



corr_quant<-rcorr(as.matrix(quantitative_variable), type="spearman")
rho<-corr_quant$r
p_value_cor<-corr_quant$P
par(las=1, mar=c(0.5,0.5,0.5,0.5))
corrplot(rho, p.mat=p_value_cor, type="upper", tl.col = "black", order = "hclust", tl.cex = 1, cl.cex = 1)

### define number of cluster respect the matrix with quantitative variable

# sil_quant<-matrix(0,30,1)
# for(k in 2:30){
#   c_quant<-pam(quantitative_variable, k, diss = F, metric = "euclidean", stand = TRUE)
#   sil_quant[k] <-c_quant$silinfo$avg.width
# }
# x_quant<-2:30
# par(las=1, cex.axis=0.8, pty = "m", mar=c(3,3,1,3), family="Times")
# plot(x_quant,sil_quant[2:30], type="l", xlab="Number of clusters", ylab="s(i)", main="Optimal number of clusters", )
# k1<-which(sil_quant==max(sil_quant)) #in questo caso risulta 2 ma è meglio 3 (al di poco sotto)
# 
#RF for the first model (all data) ####

table(data_maroldi_M_A_1$Sesso)
data_maroldi_M_A_1$Sesso_dummy<-ifelse(data_maroldi_M_A_1$Sesso=="F",0,1)
data_maroldi_M_A_1_no_miss<-na.roughfix(data_maroldi_M_A_1[,c(4,7,10,16,18,20,22,23,24,26,28,30,32,34,36,38,40,42,44,46,47)])
# Stratification per Outcome ####
percdata<-0.70
# Alive
alive<-which(data_maroldi_M_A_1_no_miss$outcome_dummy==0)
n_alive<-round(dim(as.matrix(alive))[1]*percdata)

# Dead
dead<-which(data_maroldi_M_A_1_no_miss$outcome_dummy==1)
n_dead<-round(dim(as.matrix(dead))[1]*percdata)

set.seed(989987)
sample_alive<-sample(alive,n_alive, replace=FALSE)
sub_alive<-data_maroldi_M_A_1_no_miss[sample_alive,]

sample_dead<-sample(dead,n_dead, replace=FALSE)
sub_dead<-data_maroldi_M_A_1_no_miss[sample_dead,]
data_maroldi_M_A_1_no_miss_sub<-rbind(sub_alive,sub_dead)
#Mese_sub<-data_maroldi$Mese[c(sample_alive,sample_dead)]
#OOS
data_maroldi_M_A_1_no_miss_fresh_data<-data_maroldi_M_A_1_no_miss[-c(sample_alive,sample_dead),]
dim(data_maroldi_M_A_1_no_miss_fresh_data)
# Random Forest ####

prev_death<-table(data_maroldi_M_A_1_no_miss_sub$outcome_dummy)[2]/nrow(data_maroldi_M_A_1_no_miss_sub)
#library(DMwR)
data_maroldi_M_A_1_no_miss_sub$outcome_dummy <- as.factor(data_maroldi_M_A_1_no_miss_sub$outcome_dummy)
data_maroldi_M_A_1_no_miss_sub$Sesso_dummy<-as.factor(data_maroldi_M_A_1_no_miss_sub$Sesso_dummy)
data_maroldi_M_A_1_no_miss_sub<-data_maroldi_M_A_1_no_miss_sub[,c(2,1,3:(ncol(data_maroldi_M_A_1_no_miss_sub)-1))]

data_maroldi_M_A_1_no_miss_sub = as.data.frame(data_maroldi_M_A_1_no_miss_sub)

# original over=200, under=150
data_maroldi_M_A_1_no_miss_sub1<-DMwR::SMOTE(outcome_dummy~., data_maroldi_M_A_1_no_miss_sub, perc.over = 200, k = 2, perc.under = 150)


table(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy)

set.seed(989987)
data_maroldi_M_A_1_no_miss_sub1$outcome_dummy<-as.numeric(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy)
data_maroldi_M_A_1_no_miss_sub1$outcome_dummy<-ifelse(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy==1,0,1)
table(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy)
rf_model1<-randomForest(outcome_dummy~.,data=data_maroldi_M_A_1_no_miss_sub1, ntree=10000, mtry=sqrt(19), replace=TRUE, importante=TRUE, proximity = TRUE, type="regression")
#model 1bis: no age (regression)
rf_model1_bis<-randomForest(outcome_dummy~.,data=data_maroldi_M_A_1_no_miss_sub1[,2], ntree=10000, mtry=sqrt(19), replace=TRUE, importante=TRUE, proximity = TRUE, type="regression")

#model1 (regression)
save(rf_model1, file="rf_model1_reg.Rdata")

#we extract VIM in two cases: 1) with age (rf_model1) 2) without age (rf_model1_bis)
#rf_model1<-rf_model1_bis
imp1<-importance(rf_model1_bis)
imp_rel<-imp1/max(imp1)*100
#imp for graphic
imp <- importance(rf_model1)
idx1 <- order(-imp)
imp1.ord <- imp[idx1,]
# 
imp1rel<-(imp1.ord/max(imp1.ord))*100
# 
write.table(imp1rel, file="imp1rel.txt")
par(mar=c(3,12,2,2), cex=0.5)
# barplot(imp1rel, horiz=TRUE)

# PDP of Random Forests ####
library(pdp)
p1_LDH <- rf_model1 %>%  # the %>% operator is read as "and then"
partial(pred.var = "LDH") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="LDH",
               main = "", xlim=c(0,2000), ylim=c(0.3,0.6))
p1_LDH
# #
p2_D_dimer <- rf_model1 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "D_dimer") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="D-dimer",
              main = "", xlim=c(180,7000), ylim=c(0.3,0.6))
 p2_D_dimer
# #
 p3_Neutrophils_Lymphocytes <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Neutrophils_Lymphocytes") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Neutr/Lymph",
               main = "", xlim=c(0,50),ylim=c(0.3,0.6))
 p3_Neutrophils_Lymphocytes
# #
# #
 p4_Neutrophils_perc <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Neutrophils_perc") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Neutrophils %",
               main = "", xlim=c(0,1),ylim=c(0.3,0.6))
 p4_Neutrophils_perc
# #
 p5_Fibrinogen_perc <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Fibrinogen") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Fibrinogen",
               main = "", xlim=c(50,1000))
 p5_Fibrinogen_perc
# #
# #
 p6_PCR <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "PCR") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="PCR",
               main = "", xlim=c(0,300),ylim=c(0.3,0.6))
 p6_PCR
# #
# #
 p7_Score <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Score") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Score Brescia chest-xray",
               main = "", xlim=c(0,18),ylim=c(0.3,0.6))
 p7_Score
# #
 p8_Lymphocytes_perc <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Lymphocytes_perc") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Lymphocytes %",
               main = "", xlim=c(0,1),ylim=c(0.3,0.6))
 p8_Lymphocytes_perc
# #
# #
 p9_Ferritin_std <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Ferritin_std") %>%
   plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Ferritin std",
               main = "", xlim=c(-4,200),ylim=c(0.3,0.6))
 p9_Ferritin_std
# #
# #
 p10_Monocytes_perc <- rf_model1 %>%  # the %>% operator is read as "and then"
   partial(pred.var = "Monocytes_perc") %>%
plotPartial(smooth = F, lwd = 2, ylab = "BS-EWM", xlab="Monocytes %",
              main = "", xlim=c(0,0.5), ylim=c(0.3,0.6))
 p10_Monocytes_perc
 abline(h=0.5, lty=2, col="red")

#predictions 
pred_rf1 <-predict(rf_model1, predict.all=TRUE)
pred_rf1 <- predict(rf_model1, data_maroldi_M_A_1_no_miss_sub1)

pred_rf1_OOS <-predict(rf_model1,data_maroldi_M_A_1_no_miss_fresh_data)
pred_rf1_OOS_MAY_DECEMBER<-predict(rf_model1,data_maroldi_remaining_1)
# prediction in sample model1 (RF) ####
library(pROC)

roc_modelrf1 <- roc(as.numeric(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy),pred_rf1,percent=F, smooth=F, plot=TRUE, na.rm=T, col="blue",  lty=1, title="Random Forest", cex.axis=1, cex.xlab=1, cex.ylab=1)
legend("right",
       legend=c("Training", "Validating", "Testing"),
       col=c("blue", "red", "green4"),
       lty=c(1,2,3), cex =1, box.lty=0, horiz = FALSE)
AUC_roc_modelrf1<-auc(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy,pred_rf1)

ci(roc_modelrf1)
ci.coords(roc_modelrf1, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)

#GBM in sample ####
library(gbm)
gbm.fit <- gbm(outcome_dummy~.,data=data_maroldi_M_A_1_no_miss_sub1, distribution = "bernoulli",n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 10)  
save(gbm.fit, file="gbm.fit.Rdata")
gbm.perf(gbm.fit, method = "cv")
pred_gbm_train<-predict(gbm.fit)
pred_gbm_OOS<-predict(gbm.fit, data_maroldi_M_A_1_no_miss_fresh_data)
pred_gbm_OOS_MAY_DECEMBER<-predict(gbm.fit,data_maroldi_remaining_1)

roc_gbm_train <- roc(as.numeric(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy),pred_gbm_train,percent=F, smooth=F, plot=TRUE, na.rm=T, col="blue",  lty=1, cex.axis=1, cex.xlab=1, cex.ylab=1)
legend("right",
       legend=c("Training", "Validating", "Testing"),
       col=c("blue", "red", "green4"),
       lty=c(1,2,3), cex =1, box.lty=0, horiz = FALSE)

AUC_roc_gbm_train<-auc(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy,pred_gbm_train)

ci(roc_gbm_train)
ci.coords(roc_gbm_train, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)


#Logit on training sample####
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
set.seed(89898)
logit1 <- train(outcome_dummy ~ .,  data=data_maroldi_M_A_1_no_miss_sub1, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

# logit1 <- glm(outcome_dummy ~ ., data = data_maroldi_M_A_1_no_miss_sub1, family = binomial(link = "logit"))
save(logit1, file="logit1.Rdata")
prob<-predict(logit1)
data_maroldi_M_A_1_no_miss_fresh_data$Sesso_dummy<-as.factor(data_maroldi_M_A_1_no_miss_fresh_data$Sesso_dummy)
prob_OOS<-logit1 %>% predict(data_maroldi_M_A_1_no_miss_fresh_data)
prob_OOS_MD<-logit1 %>% predict(data_maroldi_remaining_1)

roc_logit_model1<- roc(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy,prob,percent=F, smooth=F, plot=TRUE, na.rm=T, col="blue",  lty=1, cex.axis=1, cex.xlab=1, cex.ylab=1)
legend("right",
       legend=c("Training", "Validating", "Testing"),
       col=c("blue", "red", "green4"),
       lty=c(1,2,3), cex =1, box.lty=0, horiz = FALSE)

AUC_roc_logit_model1<-auc(data_maroldi_M_A_1_no_miss_sub1$outcome_dummy,prob)
ci(roc_logit_model1)
ci.coords(roc_logit_model1, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)


# RF prediction OOS model1 (RF) ####
roc_modelrf1_OOS <- roc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,pred_rf1_OOS,percent=F, smooth=F, plot=TRUE, na.rm=T, col="red",  lty=2, add=T)
AUC_roc_modelrf1_OOS<-auc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,pred_rf1_OOS)

ci(roc_modelrf1_OOS)
ci.coords(roc_modelrf1_OOS, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)

# prediction OOS on GBM ####
roc_model_gbm_OOS <- roc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,pred_gbm_OOS,percent=F, smooth=F, plot=TRUE, na.rm=T, col="red",  lty=2, add=T)
AUC_roc_model_gbm_OOS<-auc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,pred_gbm_OOS)

ci(roc_model_gbm_OOS)
ci.coords(roc_model_gbm_OOS, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)

  #prediction OOS logit1####
roc_model_logit1_OOS <- roc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,prob_OOS,percent=F, smooth=F, plot=TRUE, na.rm=T, col="red",  lty=2, add=T)
AUC_roc_logit1_OOS<-auc(data_maroldi_M_A_1_no_miss_fresh_data$outcome_dummy,prob_OOS)

ci(roc_model_logit1_OOS)
ci.coords(roc_model_logit1_OOS, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)


# prediction OOS model1 (RF) MAY_DECEMBER ####
roc_modelrf1_OOS_MAY_DECEMBER <- roc(data_maroldi_remaining_1$outcome_dummy,pred_rf1_OOS_MAY_DECEMBER,percent=F, smooth=F, plot=TRUE, na.rm=T, col="green4",  lty=3, add=T)
AUC_roc_modelrf1_OOS_MAY_DECEMBER<-auc(data_maroldi_remaining_1$outcome_dummy,pred_rf1_OOS_MAY_DECEMBER)

ci(roc_modelrf1_OOS_MAY_DECEMBER)
ci.coords(roc_modelrf1_OOS_MAY_DECEMBER, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)
#predict second way with RF1 ####
roc_modelrf1_OOS_OTT_DEC <- roc(data_maroldi_second_wave$outcome_dummy,pred_rf1_OOS_sec_wave,percent=F, smooth=F, plot=TRUE, na.rm=T, col="green4",  lty=2, add=T)
AUC_roc_modelrf1_OOS_OTT_DEC<-auc(data_maroldi_second_wave$outcome_dummy,pred_rf1_OOS_sec_wave)
ci(roc_modelrf1_OOS_OTT_DEC)
ci.coords(roc_modelrf1_OOS_OTT_DEC, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)


# prediction OOS gbm MAY_DECEMBER ####
roc_gbm_OOS_MAY_DECEMBER <- roc(data_maroldi_remaining_1$outcome_dummy,pred_gbm_OOS_MAY_DECEMBER,percent=F, smooth=F, plot=TRUE, na.rm=T, col="green4",  lty=2, add=T)
AUC_roc_gbm_OOS_MAY_DECEMBER<-auc(data_maroldi_remaining_1$outcome_dummy,pred_gbm_OOS_MAY_DECEMBER)

ci(roc_gbm_OOS_MAY_DECEMBER)
ci.coords(roc_gbm_OOS_MAY_DECEMBER, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)


#prediction OOS logit1 MAY_DECEMBER####
roc_model_logit1_OOS_MAY_DECEMBER <- roc(data_maroldi_remaining_1$outcome_dummy[which(prob_OOS_MD!=0)],prob_OOS_MD,percent=F, smooth=F, plot=TRUE, na.rm=T, col="green4",  lty=2, add=T)
AUC_roc_logit1_OOS<-auc(data_maroldi_remaining_1$outcome_dummy[which(prob_OOS_MD!=0)],prob_OOS_MD)
ci(roc_model_logit1_OOS_MAY_DECEMBER)
ci.coords(roc_model_logit1_OOS_MAY_DECEMBER, "best",
          input=c("threshold", "specificity", "sensitivity"),
          best.method=c("youden"),
          boot.n=10000,
          best.policy="random",
          boot.stratified=T,
          progress=getOption("pROCProgress")$name)

#imp for graphic
imp <- importance(rf_model1)
idx1 <- order(-imp)
imp1.ord <- imp[idx1,]

imp1rel<-(imp1.ord/max(imp1.ord))*100

write.table(imp1rel, file="imp1rel.txt")
par(mar=c(3,8,2,2), cex=0.5)
barplot(imp1rel, horiz=TRUE)




