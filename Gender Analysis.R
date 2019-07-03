getwd()
setwd("C:/Users/bdas2/Box Sync/Gender Data")
##Install packages
require(ggplot2)
library(readstata13)
require(dplyr)

##Reading the FAS dataset
FASDataset<-read.dta13("R:/DOC/FI/FAS/FAST/Data/Clean/Latest Complete File for Analysis/CompleteLatestFAS_012419.dta")
colnames(FASDataset)
View(FASDataset)

##Subsetting gender data
Genderdataset<-FASDataset[,grepl("economy|year|group|hhs_M|hhs_F|denom_adultpop|denom_landarea",names(FASDataset))]
colnames(Genderdataset)
View(Genderdataset)

Femaleborrowerscommercial<-Genderdataset %>%
  filter(!is.na(s_borrowers_A1_hhs_M))%>%
  group_by(economy)%>%
summarise(numberofyearsborrower_CB=length(economy))
View(Femaleborrowerscommercial)

Femaledepositorscommercial<-Genderdataset %>%
  filter(!is.na(s_depositors_A1_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsdepositors_CB=length(economy))
View(Femaledepositorscommercial)

FemaledepositaccCB<-Genderdataset %>%
  filter(!is.na(s_deposit_acc_A1_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsdepositacc_CB=length(economy))
View(FemaledepositaccCB)

FemaleloanaccCB<-Genderdataset %>%
  filter(!is.na(s_loan_acc_A1_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsloanacc_CB=length(economy))
View(FemaleloanaccCB)

FemaledepositsccCB<-Genderdataset %>%
  filter(!is.na(s_deposits_A1_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsdep_CB=length(economy))
View(FemaledepositsccCB)

FemaleloansCB<-Genderdataset %>%
  filter(!is.na(s_loans_A1_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsloans_CB=length(economy))
View(FemaleloansCB)

Combined1<-full_join(Femaleborrowerscommercial,FemaledepositaccCB, by = "economy")
Combined2<-full_join(FemaleloansCB,FemaleloanaccCB, by = "economy")
Combined3<-full_join(FemaledepositsccCB,Femaledepositorscommercial, by = "economy")
Combined4<-full_join(Combined1,Combined2, by = "economy")
Finalcombined<-full_join(Combined3,Combined4, by = "economy")
View(Finalcombined)


Femaleborrowersmicrocredit<-Genderdataset %>%
  filter(!is.na(s_borrowers_B1a_hhs_M))%>%
  group_by(economy)%>%
  summarise(numberofyearsborrower_mc=length(economy))
View(Femaleborrowersmicrocredit)

Femaleloanaccmicrocredit<-Genderdataset %>%
  filter(!is.na(s_loan_acc_B1a_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsloanacc_mc=length(economy))
View(Femaleloanaccmicrocredit)

Femaleloansmicrocredit<-Genderdataset %>%
  filter(!is.na(s_loans_B1a_hhs_F))%>%
  group_by(economy)%>%
  summarise(numberofyearsloans_mc=length(economy))
View(Femaleloansmicrocredit)

Combined11<-full_join(Femaleborrowersmicrocredit,Femaleloanaccmicrocredit, by = "economy")
Combined21<-full_join(Combined11,Femaleloansmicrocredit, by = "economy")
GenderFull<-full_join(Finalcombined,Combined21, by = "economy")



write.csv(GenderFull,"GenderFull.csv")