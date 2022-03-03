#########################################
## First-year project: Simulation
## Linh Nguyen
## Acknowledgment: Brent Donnellan 
## Created: 20-Apr-2020
## Last edited: 18-August-2020
## Edit-Folding-Collapse All => navigate
#########################################

# META ==================================
library(tidyverse)
library(haven)
library(MASS)
library(nlme)
library(simsem)
library(reshape2)
library(psych)
library(simstudy)
library(Matrix)
options(scipen=999)
set.seed(20200618)

# SELF - IMPORTING REAL DATA ===================

# > read in real data----
selfw1 <- read_spss("selfw1.sav")
selfw2 <- read_spss("selfw2.sav")
selfw3 <- read_spss("selfw3.sav")
selfw4 <- read_spss("selfw4.sav")

# BFAS and EPSI datasets
selfw1 <- selfw1 %>%
  dplyr::select(ID, bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence)
selfw2 <- selfw2 %>% 
  dplyr::select(ID, bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence)
selfw3 <- selfw3 %>% 
  dplyr::select(ID, bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence)
selfw4 <- selfw4 %>% 
  dplyr::select(ID, bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence)

#sorting by ID 
selfw1 <- selfw1[order(selfw1$ID),]
selfw2 <- selfw2[order(selfw2$ID),]
selfw3 <- selfw3[order(selfw3$ID),]
selfw4 <- selfw4[order(selfw4$ID),]

#add time variable wave 1-4
selfw1$time <- 1
selfw2$time <- 2
selfw3$time <- 3
selfw4$time <- 4

#combine all wave datasets
self <- rbind(selfw1,selfw2)
self <- rbind(self,selfw3)
self <- rbind(self,selfw4)

#wide dataset
selfw <- melt(self, id = c("ID","time")) 
selfw <- dcast(selfw, ID ~ time + variable,
               value.var = "value")
names <- colnames(selfw1)[2:130]
names(selfw)[2:517] <- paste0(names, "_w", c(rep(1:4, each = 129)))
rm("names", "self")

domain <- selfw %>% 
  dplyr::select(bfas_agreeableness_w1:bfas_opennessdomain_w1,
                bfas_agreeableness_w2:bfas_opennessdomain_w2,
                bfas_agreeableness_w3:bfas_opennessdomain_w3,
                bfas_agreeableness_w4:bfas_opennessdomain_w4)
aspect <- selfw %>% 
  dplyr::select(bfas_assertiveness_w1:bfas_withdrawal_w1,
                bfas_assertiveness_w2:bfas_withdrawal_w2,
                bfas_assertiveness_w3:bfas_withdrawal_w3,
                bfas_assertiveness_w4:bfas_withdrawal_w4)
identity <- selfw %>% 
  dplyr::select(epsi_confusion_w1:epsi_coherence_w1,
                epsi_confusion_w2:epsi_coherence_w2,
                epsi_confusion_w3:epsi_coherence_w3,
                epsi_confusion_w4:epsi_coherence_w4)

#real correlation matrix 
Rreal.domain <- cor(domain, use = "pairwise.complete.obs")
Rreal.aspect <- cor(aspect, use = "pairwise.complete.obs")
Rreal.identity <- cor(identity, use = "pairwise.complete.obs")

# > compute real alpha----
alpha.agree.w1 <- selfw1 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w1 <- (alpha.agree.w1$total)$raw_alpha

alpha.agree.w2 <- selfw2 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w2 <- (alpha.agree.w2$total)$raw_alpha

alpha.agree.w3 <- selfw3 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w3 <- (alpha.agree.w3$total)$raw_alpha

alpha.agree.w4 <- selfw4 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w4 <- (alpha.agree.w4$total)$raw_alpha

alpha.compa.w1 <- selfw1 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w1 <- (alpha.compa.w1$total)$raw_alpha

alpha.compa.w2 <- selfw2 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w2 <- (alpha.compa.w2$total)$raw_alpha

alpha.compa.w3 <- selfw3 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w3 <- (alpha.compa.w3$total)$raw_alpha

alpha.compa.w4 <- selfw4 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha() 
alpha.compa.w4 <- (alpha.compa.w4$total)$raw_alpha

alpha.polit.w1 <- selfw1 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w1 <- (alpha.polit.w1$total)$raw_alpha

alpha.polit.w2 <- selfw2 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w2 <- (alpha.polit.w2$total)$raw_alpha

alpha.polit.w3 <- selfw3 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w3 <- (alpha.polit.w3$total)$raw_alpha

alpha.polit.w4 <- selfw4 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w4 <- (alpha.polit.w4$total)$raw_alpha
  
alpha.consci.w1 <- selfw1 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w1 <- (alpha.consci.w1$total)$raw_alpha  

alpha.consci.w2 <- selfw2 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w2 <- (alpha.consci.w2$total)$raw_alpha  

alpha.consci.w3 <- selfw3 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w3 <- (alpha.consci.w3$total)$raw_alpha  

alpha.consci.w4 <- selfw4 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w4 <- (alpha.consci.w4$total)$raw_alpha  

alpha.indus.w1 <- selfw1 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w1 <- (alpha.indus.w1$total)$raw_alpha

alpha.indus.w2 <- selfw2 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w2 <- (alpha.indus.w2$total)$raw_alpha

alpha.indus.w3 <- selfw3 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w3 <- (alpha.indus.w3$total)$raw_alpha

alpha.indus.w4 <- selfw4 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w4 <- (alpha.indus.w4$total)$raw_alpha

alpha.order.w1 <- selfw1 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w1 <- (alpha.order.w1$total)$raw_alpha

alpha.order.w2 <- selfw2 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w2 <- (alpha.order.w2$total)$raw_alpha

alpha.order.w3 <- selfw3 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w3 <- (alpha.order.w3$total)$raw_alpha

alpha.order.w4 <- selfw4 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w4 <- (alpha.order.w4$total)$raw_alpha

alpha.extra.w1 <- selfw1 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w1 <- (alpha.extra.w1$total)$raw_alpha

alpha.extra.w2 <- selfw2 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w2 <- (alpha.extra.w2$total)$raw_alpha

alpha.extra.w3 <- selfw3 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w3 <- (alpha.extra.w3$total)$raw_alpha

alpha.extra.w4 <- selfw4 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w4 <- (alpha.extra.w4$total)$raw_alpha

alpha.assert.w1 <- selfw1 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w1 <- (alpha.assert.w1$total)$raw_alpha

alpha.assert.w2 <- selfw2 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w2 <- (alpha.assert.w2$total)$raw_alpha

alpha.assert.w3 <- selfw3 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w3 <- (alpha.assert.w3$total)$raw_alpha

alpha.assert.w4 <- selfw4 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w4 <- (alpha.assert.w4$total)$raw_alpha

alpha.enthu.w1 <- selfw1 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w1 <- (alpha.enthu.w1$total)$raw_alpha

alpha.enthu.w2 <- selfw2 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w2 <- (alpha.enthu.w2$total)$raw_alpha

alpha.enthu.w3 <- selfw3 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w3 <- (alpha.enthu.w3$total)$raw_alpha

alpha.enthu.w4 <- selfw4 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w4 <- (alpha.enthu.w4$total)$raw_alpha

alpha.neuro.w1 <- selfw1 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041R, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w1 <- (alpha.neuro.w1$total)$raw_alpha

alpha.neuro.w2 <- selfw2 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041R, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w2 <- (alpha.neuro.w2$total)$raw_alpha

alpha.neuro.w3 <- selfw3 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041R, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w3 <- (alpha.neuro.w3$total)$raw_alpha

alpha.neuro.w4 <- selfw4 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041R, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w4 <- (alpha.neuro.w4$total)$raw_alpha

alpha.withd.w1 <- selfw1 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041R, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w1 <- (alpha.withd.w1$total)$raw_alpha

alpha.withd.w2 <- selfw2 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041R, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w2 <- (alpha.withd.w2$total)$raw_alpha

alpha.withd.w3 <- selfw3 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041R, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w3 <- (alpha.withd.w3$total)$raw_alpha

alpha.withd.w4 <- selfw4 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041R, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w4 <- (alpha.withd.w4$total)$raw_alpha

alpha.volat.w1 <- selfw1 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w1 <- (alpha.volat.w1$total)$raw_alpha

alpha.volat.w2 <- selfw2 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w2 <- (alpha.volat.w2$total)$raw_alpha

alpha.volat.w3 <- selfw3 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w3 <- (alpha.volat.w3$total)$raw_alpha

alpha.volat.w4 <- selfw4 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w4 <- (alpha.volat.w4$total)$raw_alpha

alpha.opend.w1 <- selfw1 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w1 <- (alpha.opend.w1$total)$raw_alpha

alpha.opend.w2 <- selfw2 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w2 <- (alpha.opend.w2$total)$raw_alpha

alpha.opend.w3 <- selfw3 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w3 <- (alpha.opend.w3$total)$raw_alpha

alpha.opend.w4 <- selfw4 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w4 <- (alpha.opend.w4$total)$raw_alpha

alpha.intel.w1 <- selfw1 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w1 <- (alpha.intel.w1$total)$raw_alpha

alpha.intel.w2 <- selfw2 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w2 <- (alpha.intel.w2$total)$raw_alpha

alpha.intel.w3 <- selfw3 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w3 <- (alpha.intel.w3$total)$raw_alpha

alpha.intel.w4 <- selfw4 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w4 <- (alpha.intel.w4$total)$raw_alpha

alpha.opena.w1 <- selfw1 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w1 <- (alpha.opena.w1$total)$raw_alpha

alpha.opena.w2 <- selfw2 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w2 <- (alpha.opena.w2$total)$raw_alpha

alpha.opena.w3 <- selfw3 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w3 <- (alpha.opena.w3$total)$raw_alpha

alpha.opena.w4 <- selfw4 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w4 <- (alpha.opena.w4$total)$raw_alpha

alpha.coher.w1 <- selfw1 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w1 <- (alpha.coher.w1$total)$raw_alpha

alpha.coher.w2 <- selfw2 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w2 <- (alpha.coher.w2$total)$raw_alpha

alpha.coher.w3 <- selfw3 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w3 <- (alpha.coher.w3$total)$raw_alpha

alpha.coher.w4 <- selfw4 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w4 <- (alpha.coher.w4$total)$raw_alpha

alpha.confu.w1 <- selfw1 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w1 <- (alpha.confu.w1$total)$raw_alpha

alpha.confu.w2 <- selfw2 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w2 <- (alpha.confu.w2$total)$raw_alpha

alpha.confu.w3 <- selfw3 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w3 <- (alpha.confu.w3$total)$raw_alpha

alpha.confu.w4 <- selfw4 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w4 <- (alpha.confu.w4$total)$raw_alpha

# SELF - SIMULATION ===========================
# > Domains----

#specify means
means.domain <- c(mean(selfw1$bfas_agreeableness, na.rm = TRUE),
                  mean(selfw1$bfas_conscientiousness, na.rm = TRUE),
                  mean(selfw1$bfas_extraversion, na.rm = TRUE),
                  mean(selfw1$bfas_neuroticism, na.rm = TRUE),
                  mean(selfw1$bfas_opennessdomain, na.rm = TRUE),
                  mean(selfw2$bfas_agreeableness, na.rm = TRUE),
                  mean(selfw2$bfas_conscientiousness, na.rm = TRUE),
                  mean(selfw2$bfas_extraversion, na.rm = TRUE),
                  mean(selfw2$bfas_neuroticism, na.rm = TRUE),
                  mean(selfw2$bfas_opennessdomain, na.rm = TRUE),
                  mean(selfw3$bfas_agreeableness, na.rm = TRUE),
                  mean(selfw3$bfas_conscientiousness, na.rm = TRUE),
                  mean(selfw3$bfas_extraversion, na.rm = TRUE),
                  mean(selfw3$bfas_neuroticism, na.rm = TRUE),
                  mean(selfw3$bfas_opennessdomain, na.rm = TRUE),
                  mean(selfw4$bfas_agreeableness, na.rm = TRUE),
                  mean(selfw4$bfas_conscientiousness, na.rm = TRUE),
                  mean(selfw4$bfas_extraversion, na.rm = TRUE),
                  mean(selfw4$bfas_neuroticism, na.rm = TRUE),
                  mean(selfw4$bfas_opennessdomain, na.rm = TRUE))

#specify sd
sd.domain <- c(sd(selfw1$bfas_agreeableness, na.rm = TRUE),
                  sd(selfw1$bfas_conscientiousness, na.rm = TRUE),
                  sd(selfw1$bfas_extraversion, na.rm = TRUE),
                  sd(selfw1$bfas_neuroticism, na.rm = TRUE),
                  sd(selfw1$bfas_opennessdomain, na.rm = TRUE),
                  sd(selfw2$bfas_agreeableness, na.rm = TRUE),
                  sd(selfw2$bfas_conscientiousness, na.rm = TRUE),
                  sd(selfw2$bfas_extraversion, na.rm = TRUE),
                  sd(selfw2$bfas_neuroticism, na.rm = TRUE),
                  sd(selfw2$bfas_opennessdomain, na.rm = TRUE),
                  sd(selfw3$bfas_agreeableness, na.rm = TRUE),
                  sd(selfw3$bfas_conscientiousness, na.rm = TRUE),
                  sd(selfw3$bfas_extraversion, na.rm = TRUE),
                  sd(selfw3$bfas_neuroticism, na.rm = TRUE),
                  sd(selfw3$bfas_opennessdomain, na.rm = TRUE),
                  sd(selfw4$bfas_agreeableness, na.rm = TRUE),
                  sd(selfw4$bfas_conscientiousness, na.rm = TRUE),
                  sd(selfw4$bfas_extraversion, na.rm = TRUE),
                  sd(selfw4$bfas_neuroticism, na.rm = TRUE),
                  sd(selfw4$bfas_opennessdomain, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between-trait correlations
# And perfect between wave correlations to show no change
R.domain <- Rreal.domain
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w3"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w3"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w3"] <- 1

R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w3"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w3"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w3"] <- 1

R.domain["bfas_extraversion_w1","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w1","bfas_extraversion_w3"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w1","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w3"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w3"] <- 1

R.domain["bfas_neuroticism_w1","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w1","bfas_neuroticism_w3"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w1","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w3"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w3"] <- 1

R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w3"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w3"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w3"] <- 1

#specify alpha
alpha.domain <- c(alpha.agree.w1, alpha.consci.w1, alpha.extra.w1, alpha.neuro.w1, alpha.opend.w1,
                  alpha.agree.w2, alpha.consci.w2, alpha.extra.w2, alpha.neuro.w2, alpha.opend.w2,
                  alpha.agree.w3, alpha.consci.w3, alpha.extra.w3, alpha.neuro.w3, alpha.opend.w3,
                  alpha.agree.w4, alpha.consci.w4, alpha.extra.w4, alpha.neuro.w4, alpha.opend.w4)

#take the square root of alphas to create the attenuation factor
alpha.domain.sq <- diag(sqrt(alpha.domain))

#apply the attenuation factor to correlation matrix
R.domain.a <- alpha.domain.sq %*% R.domain %*% alpha.domain.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.domain.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.domain.a <- sd.domain/sqrt(alpha.domain)

#unfortunately, correlation matrix is not positive definite
#some eigenvalues are negative
eigen(R.domain.a)$values
cholStatus <- try(u <- chol(R.domain.a), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

#change to positive definite 
#https://www.r-bloggers.com/fixing-non-positive-definite-correlation-matrices-using-r-2/
R.domain.new <- R.domain.a

iter <- 0
while (cholError) {
    iter <- iter + 1
    cat("iteration ", iter, "\n")

    # replace -ve eigen values with small +ve number
    newEig <- eigen(R.domain.new)
    newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)

    # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
    # eig vectors
    R.domain.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)

    # normalize modified matrix eqn 6 from Brissette et al 2007
    R.domain.new <- R.domain.new/sqrt(diag(R.domain.new) %*% t(diag(R.domain.new)))

    # try chol again
    cholStatus <- try(u <- chol(R.domain.new), silent = TRUE)
    cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

cholError <- ifelse(eigen(R.domain.new)$values[20] < 0 , TRUE, FALSE)

iter <- 0
while (cholError) {
    iter <- iter + 1
    cat("iteration ", iter, "\n")

    # replace -ve eigen values with small +ve number
    newEig <- eigen(R.domain.new)
    newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)

    # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
    # eig vectors
    R.domain.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)

    # normalize modified matrix eqn 6 from Brissette et al 2007
    R.domain.new <- R.domain.new/sqrt(diag(R.domain.new) %*% t(diag(R.domain.new)))

    # try chol again
    cholStatus <- try(u <- chol(R.domain.new), silent = TRUE)
    cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}


#check new matrix - now all eigen values are positive
eigen(R.domain.new)$values

#simulate n = 50000 dataset
n <- 50000

domain.sim <- genCorData(n, mu = means.domain, sigma = sd.domain.a,
                         corMatrix = R.domain.new)

#rename simulated dataset
names(domain.sim)[-1] <- names(domain)


# > Aspects----

#specify means
means.aspect <- c(mean(selfw1$bfas_assertiveness, na.rm = TRUE),
                  mean(selfw1$bfas_compassion, na.rm = TRUE),
                  mean(selfw1$bfas_enthusiasm, na.rm = TRUE),
                  mean(selfw1$bfas_industriousness, na.rm = TRUE),
                  mean(selfw1$bfas_intellect, na.rm = TRUE),
                  mean(selfw1$bfas_opennessaspect, na.rm = TRUE),
                  mean(selfw1$bfas_orderliness, na.rm = TRUE),
                  mean(selfw1$bfas_politeness, na.rm = TRUE),
                  mean(selfw1$bfas_volatility, na.rm = TRUE),
                  mean(selfw1$bfas_withdrawal, na.rm = TRUE),
                  mean(selfw2$bfas_assertiveness, na.rm = TRUE),
                  mean(selfw2$bfas_compassion, na.rm = TRUE),
                  mean(selfw2$bfas_enthusiasm, na.rm = TRUE),
                  mean(selfw2$bfas_industriousness, na.rm = TRUE),
                  mean(selfw2$bfas_intellect, na.rm = TRUE),
                  mean(selfw2$bfas_opennessaspect, na.rm = TRUE),
                  mean(selfw2$bfas_orderliness, na.rm = TRUE),
                  mean(selfw2$bfas_politeness, na.rm = TRUE),
                  mean(selfw2$bfas_volatility, na.rm = TRUE),
                  mean(selfw2$bfas_withdrawal, na.rm = TRUE),
                  mean(selfw3$bfas_assertiveness, na.rm = TRUE),
                  mean(selfw3$bfas_compassion, na.rm = TRUE),
                  mean(selfw3$bfas_enthusiasm, na.rm = TRUE),
                  mean(selfw3$bfas_industriousness, na.rm = TRUE),
                  mean(selfw3$bfas_intellect, na.rm = TRUE),
                  mean(selfw3$bfas_opennessaspect, na.rm = TRUE),
                  mean(selfw3$bfas_orderliness, na.rm = TRUE),
                  mean(selfw3$bfas_politeness, na.rm = TRUE),
                  mean(selfw3$bfas_volatility, na.rm = TRUE),
                  mean(selfw3$bfas_withdrawal, na.rm = TRUE),
                  mean(selfw4$bfas_assertiveness, na.rm = TRUE),
                  mean(selfw4$bfas_compassion, na.rm = TRUE),
                  mean(selfw4$bfas_enthusiasm, na.rm = TRUE),
                  mean(selfw4$bfas_industriousness, na.rm = TRUE),
                  mean(selfw4$bfas_intellect, na.rm = TRUE),
                  mean(selfw4$bfas_opennessaspect, na.rm = TRUE),
                  mean(selfw4$bfas_orderliness, na.rm = TRUE),
                  mean(selfw4$bfas_politeness, na.rm = TRUE),
                  mean(selfw4$bfas_volatility, na.rm = TRUE),
                  mean(selfw4$bfas_withdrawal, na.rm = TRUE))

#specify sd
sd.aspect <- c(sd(selfw1$bfas_assertiveness, na.rm = TRUE),
                  sd(selfw1$bfas_compassion, na.rm = TRUE),
                  sd(selfw1$bfas_enthusiasm, na.rm = TRUE),
                  sd(selfw1$bfas_industriousness, na.rm = TRUE),
                  sd(selfw1$bfas_intellect, na.rm = TRUE),
                  sd(selfw1$bfas_opennessaspect, na.rm = TRUE),
                  sd(selfw1$bfas_orderliness, na.rm = TRUE),
                  sd(selfw1$bfas_politeness, na.rm = TRUE),
                  sd(selfw1$bfas_volatility, na.rm = TRUE),
                  sd(selfw1$bfas_withdrawal, na.rm = TRUE),
                  sd(selfw2$bfas_assertiveness, na.rm = TRUE),
                  sd(selfw2$bfas_compassion, na.rm = TRUE),
                  sd(selfw2$bfas_enthusiasm, na.rm = TRUE),
                  sd(selfw2$bfas_industriousness, na.rm = TRUE),
                  sd(selfw2$bfas_intellect, na.rm = TRUE),
                  sd(selfw2$bfas_opennessaspect, na.rm = TRUE),
                  sd(selfw2$bfas_orderliness, na.rm = TRUE),
                  sd(selfw2$bfas_politeness, na.rm = TRUE),
                  sd(selfw2$bfas_volatility, na.rm = TRUE),
                  sd(selfw2$bfas_withdrawal, na.rm = TRUE),
                  sd(selfw3$bfas_assertiveness, na.rm = TRUE),
                  sd(selfw3$bfas_compassion, na.rm = TRUE),
                  sd(selfw3$bfas_enthusiasm, na.rm = TRUE),
                  sd(selfw3$bfas_industriousness, na.rm = TRUE),
                  sd(selfw3$bfas_intellect, na.rm = TRUE),
                  sd(selfw3$bfas_opennessaspect, na.rm = TRUE),
                  sd(selfw3$bfas_orderliness, na.rm = TRUE),
                  sd(selfw3$bfas_politeness, na.rm = TRUE),
                  sd(selfw3$bfas_volatility, na.rm = TRUE),
                  sd(selfw3$bfas_withdrawal, na.rm = TRUE),
                  sd(selfw4$bfas_assertiveness, na.rm = TRUE),
                  sd(selfw4$bfas_compassion, na.rm = TRUE),
                  sd(selfw4$bfas_enthusiasm, na.rm = TRUE),
                  sd(selfw4$bfas_industriousness, na.rm = TRUE),
                  sd(selfw4$bfas_intellect, na.rm = TRUE),
                  sd(selfw4$bfas_opennessaspect, na.rm = TRUE),
                  sd(selfw4$bfas_orderliness, na.rm = TRUE),
                  sd(selfw4$bfas_politeness, na.rm = TRUE),
                  sd(selfw4$bfas_volatility, na.rm = TRUE),
                  sd(selfw4$bfas_withdrawal, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between trait correlations
# And perfect between wave correlations to show no change
R.aspect <- Rreal.aspect
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w3"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w3"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w3"] <- 1

R.aspect["bfas_compassion_w1","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w1","bfas_compassion_w3"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w1","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w3"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w3"] <- 1

R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w3"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w3"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w3"] <- 1

R.aspect["bfas_industriousness_w1","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w1","bfas_industriousness_w3"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w1","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w3"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w3"] <- 1

R.aspect["bfas_intellect_w1","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w1","bfas_intellect_w3"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w1","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w3"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w3"] <- 1

R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w3"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w3"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w3"] <- 1

R.aspect["bfas_orderliness_w1","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w1","bfas_orderliness_w3"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w1","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w3"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w3"] <- 1

R.aspect["bfas_politeness_w1","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w1","bfas_politeness_w3"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w1","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w3"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w3"] <- 1

R.aspect["bfas_volatility_w1","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w1","bfas_volatility_w3"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w1","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w3"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w3"] <- 1

R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w3"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w3"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w3"] <- 1

#specify alpha
alpha.aspect <- c(alpha.assert.w1, alpha.compa.w1, alpha.enthu.w1, alpha.indus.w1, alpha.intel.w1,
                  alpha.opena.w1, alpha.order.w1, alpha.polit.w1, alpha.volat.w1, alpha.withd.w1,
                  alpha.assert.w2, alpha.compa.w2, alpha.enthu.w2, alpha.indus.w2, alpha.intel.w2,
                  alpha.opena.w2, alpha.order.w2, alpha.polit.w2, alpha.volat.w2, alpha.withd.w2,
                  alpha.assert.w3, alpha.compa.w3, alpha.enthu.w3, alpha.indus.w3, alpha.intel.w3,
                  alpha.opena.w3, alpha.order.w3, alpha.polit.w3, alpha.volat.w3, alpha.withd.w3,
                  alpha.assert.w4, alpha.compa.w4, alpha.enthu.w4, alpha.indus.w4, alpha.intel.w4,
                  alpha.opena.w4, alpha.order.w4, alpha.polit.w4, alpha.volat.w4, alpha.withd.w4)

#take the square root of alphas to create the attenuation factor
alpha.aspect.sq <- diag(sqrt(alpha.aspect))

#apply the attenuation factor to correlation matrix
R.aspect.a <- alpha.aspect.sq %*% R.aspect %*% alpha.aspect.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.aspect.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.aspect.a <- sd.aspect/sqrt(alpha.aspect)

#unfortunately, correlation matrix is not positive definite
#some eigenvalues are negative
eigen(R.aspect.a)$values
cholStatus <- try(u <- chol(R.aspect.a), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

#change to positive definite 
#https://www.r-bloggers.com/fixing-non-positive-definite-correlation-matrices-using-r-2/
R.aspect.new <- R.aspect.a

iter <- 0
while (cholError) {
    iter <- iter + 1
    cat("iteration ", iter, "\n")

    # replace -ve eigen values with small +ve number
    newEig <- eigen(R.aspect.new)
    newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)

    # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
    # eig vectors
    R.aspect.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)

    # normalize modified matrix eqn 6 from Brissette et al 2007
    R.aspect.new <- R.aspect.new/sqrt(diag(R.aspect.new) %*% t(diag(R.aspect.new)))

    # try chol again
    cholStatus <- try(u <- chol(R.aspect.new), silent = TRUE)
    cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

#check new matrix - now all eigen values are positive
eigen(R.aspect.new)$values

#simulate n = 50000 dataset
n <- 50000
aspect.sim <- genCorData(n, mu = means.aspect, sigma = sd.aspect.a,
                         corMatrix = R.aspect.new)

#rename simulated dataset
names(aspect.sim)[-1] <- colnames(R.aspect)

# > Identity----

#specify means
means.identity <- c(mean(selfw1$epsi_confusion, na.rm = TRUE),
                    mean(selfw1$epsi_coherence, na.rm = TRUE),
                    mean(selfw2$epsi_confusion, na.rm = TRUE),
                    mean(selfw2$epsi_coherence, na.rm = TRUE),
                    mean(selfw3$epsi_confusion, na.rm = TRUE),
                    mean(selfw3$epsi_coherence, na.rm = TRUE),
                    mean(selfw4$epsi_confusion, na.rm = TRUE),
                    mean(selfw4$epsi_coherence, na.rm = TRUE))

#specify sd
sd.identity <- c(sd(selfw1$epsi_confusion, na.rm = TRUE),
                 sd(selfw1$epsi_coherence, na.rm = TRUE),
                 sd(selfw2$epsi_confusion, na.rm = TRUE),
                 sd(selfw2$epsi_coherence, na.rm = TRUE),
                 sd(selfw3$epsi_confusion, na.rm = TRUE),
                 sd(selfw3$epsi_coherence, na.rm = TRUE),
                 sd(selfw4$epsi_confusion, na.rm = TRUE),
                 sd(selfw4$epsi_coherence, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between trait correlations
# And perfect between wave correlations to show no change
R.identity <- Rreal.identity
R.identity["epsi_confusion_w1","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w1","epsi_confusion_w3"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w1","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w3"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w3"] <- 1

R.identity["epsi_coherence_w1","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w1","epsi_coherence_w3"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w1","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w3"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w3"] <- 1

#specify alpha
alpha.identity <- c(alpha.confu.w1, alpha.coher.w1,
                  alpha.confu.w2, alpha.coher.w2,
                  alpha.confu.w3, alpha.coher.w3,
                  alpha.confu.w4, alpha.coher.w4)

#take the square root of alphas to create the attenuation factor
alpha.identity.sq <- diag(sqrt(alpha.identity))

#apply the attenuation factor to correlation matrix
R.identity.a <- alpha.identity.sq %*% R.identity %*% alpha.identity.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.identity.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.identity.a <- sd.identity/sqrt(alpha.identity)

#simulate n = 50000 dataset
n <- 50000

identity.sim <- genCorData(n, mu = means.identity, sigma = sd.identity.a,
                         corMatrix = R.identity.a)

#rename
names(identity.sim)[-1] <- colnames(R.identity)

# SELF - EXPORT ================================

#save file
write.csv(domain.sim,"domainsim.csv", row.names = FALSE)
write.csv(aspect.sim,"aspectsim.csv", row.names = FALSE)
write.csv(identity.sim,"identitysim.csv", row.names = FALSE)

# PEER - IMPORTING REAL DATA ===================
# > read in real data ----
peerw1 <- read_spss("peerw1.sav")
peerw2 <- read_spss("peerw2.sav")
peerw3 <- read_spss("peerw3.sav")
peerw4 <- read_spss("peerw4.sav")
randomID <- readRDS("randomID.R")

# BFAS and EPSI datasets
peerw1 <- peerw1 %>%
  dplyr::select(ID, PeerID,
                bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence) %>% 
  filter(PeerID %in% randomID)
peerw2 <- peerw2 %>% 
  dplyr::select(ID, PeerID,
                bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence) %>% 
  filter(PeerID %in% randomID)
peerw3 <- peerw3 %>% 
  dplyr::select(ID, PeerID,
                bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence) %>% 
  filter(PeerID %in% randomID)
peerw4 <- peerw4 %>%
  dplyr::select(ID, PeerID,
                bfas001R:bfas100, epsi01:epsi12,
                bfas_agreeableness:bfas_withdrawal, 
                epsi_confusion, epsi_coherence) %>% 
  filter(PeerID %in% randomID)


#sorting by ID 
peerw1 <- peerw1[order(peerw1$ID),]
peerw2 <- peerw2[order(peerw2$ID),]
peerw3 <- peerw3[order(peerw3$ID),]
peerw4 <- peerw3[order(peerw4$ID),]


#add time variable wave 1-3
peerw1$time <- 1
peerw2$time <- 2
peerw3$time <- 3
peerw4$time <- 4

#combine all wave datasets
peer <- rbind(peerw1,peerw2)
peer <- rbind(peer,peerw3)
peer <- rbind(peer,peerw4)

#wide dataset
peerw <- melt(peer, id = c("ID","PeerID", "time")) 
peerw <- dcast(peerw, ID + PeerID ~ time + variable,
               value.var = "value")
names <- colnames(peerw1)[3:131]
names(peerw)[3:518] <- paste0(names, "_w", c(rep(1:4, each = 129)))
rm("names", "peer")

domain <- peerw %>% 
  dplyr::select(bfas_agreeableness_w1:bfas_opennessdomain_w1,
                bfas_agreeableness_w2:bfas_opennessdomain_w2,
                bfas_agreeableness_w3:bfas_opennessdomain_w3,
                bfas_agreeableness_w4:bfas_opennessdomain_w4)
aspect <- peerw %>% 
  dplyr::select(bfas_assertiveness_w1:bfas_withdrawal_w1,
                bfas_assertiveness_w2:bfas_withdrawal_w2,
                bfas_assertiveness_w3:bfas_withdrawal_w3,
                bfas_assertiveness_w4:bfas_withdrawal_w4)
identity <- peerw %>% 
  dplyr::select(epsi_confusion_w1:epsi_coherence_w1,
                epsi_confusion_w2:epsi_coherence_w2,
                epsi_confusion_w3:epsi_coherence_w3,
                epsi_confusion_w4:epsi_coherence_w4)

#real correlation matrix 
Rreal.domain <- cor(domain, use = "pairwise.complete.obs")
Rreal.aspect <- cor(aspect, use = "pairwise.complete.obs")
Rreal.identity <- cor(identity, use = "pairwise.complete.obs")

# > compute real alpha----
alpha.agree.w1 <- peerw1 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w1 <- (alpha.agree.w1$total)$raw_alpha

alpha.agree.w2 <- peerw2 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w2 <- (alpha.agree.w2$total)$raw_alpha

alpha.agree.w3 <- peerw3 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w3 <- (alpha.agree.w3$total)$raw_alpha

alpha.agree.w4 <- peerw4 %>% 
  dplyr::select(bfas002R, bfas007, bfas012, bfas017R, bfas022,
                bfas027, bfas032R, bfas037R, bfas042, bfas047,
                bfas052R, bfas057, bfas062R, bfas067R, bfas072, 
                bfas077R, bfas082R, bfas087R, bfas092, bfas097R) %>% 
  psych::alpha()
alpha.agree.w4 <- (alpha.agree.w4$total)$raw_alpha

alpha.compa.w1 <- peerw1 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w1 <- (alpha.compa.w1$total)$raw_alpha

alpha.compa.w2 <- peerw2 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w2 <- (alpha.compa.w2$total)$raw_alpha

alpha.compa.w3 <- peerw3 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w3 <- (alpha.compa.w3$total)$raw_alpha

alpha.compa.w4 <- peerw4 %>% 
  dplyr::select(bfas002R, bfas012, bfas022, bfas032R, bfas042,
                bfas052R, bfas062R, bfas072, bfas082R, bfas092) %>% 
  psych::alpha()
alpha.compa.w4 <- (alpha.compa.w4$total)$raw_alpha

alpha.polit.w1 <- peerw1 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w1 <- (alpha.polit.w1$total)$raw_alpha

alpha.polit.w2 <- peerw2 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w2 <- (alpha.polit.w2$total)$raw_alpha

alpha.polit.w3 <- peerw3 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w3 <- (alpha.polit.w3$total)$raw_alpha

alpha.polit.w4 <- peerw4 %>% 
  dplyr::select(bfas007, bfas017R, bfas027, bfas037R, bfas047,
                bfas057, bfas067R, bfas077R, bfas087R, bfas097R) %>% 
  psych::alpha()
alpha.polit.w4 <- (alpha.polit.w4$total)$raw_alpha

alpha.consci.w1 <- peerw1 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w1 <- (alpha.consci.w1$total)$raw_alpha  

alpha.consci.w2 <- peerw2 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w2 <- (alpha.consci.w2$total)$raw_alpha  

alpha.consci.w3 <- peerw3 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w3 <- (alpha.consci.w3$total)$raw_alpha  

alpha.consci.w4 <- peerw4 %>% 
  dplyr::select(bfas003, bfas008R, bfas013R, bfas018, bfas023R,
                bfas028, bfas033R, bfas038, bfas043, bfas048R, 
                bfas053R, bfas058, bfas063, bfas068R, bfas073,
                bfas078R, bfas083R, bfas088, bfas093R, bfas098) %>% 
  psych::alpha()
alpha.consci.w4 <- (alpha.consci.w4$total)$raw_alpha  

alpha.indus.w1 <- peerw1 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w1 <- (alpha.indus.w1$total)$raw_alpha

alpha.indus.w2 <- peerw2 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w2 <- (alpha.indus.w2$total)$raw_alpha

alpha.indus.w3 <- peerw3 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w3 <- (alpha.indus.w3$total)$raw_alpha

alpha.indus.w4 <- peerw3 %>% 
  dplyr::select(bfas003, bfas013R, bfas023R, bfas033R, bfas043, 
                bfas053R, bfas063, bfas073, bfas083R, bfas093R) %>% 
  psych::alpha()
alpha.indus.w4 <- (alpha.indus.w4$total)$raw_alpha

alpha.order.w1 <- peerw1 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w1 <- (alpha.order.w1$total)$raw_alpha

alpha.order.w2 <- peerw2 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w2 <- (alpha.order.w2$total)$raw_alpha

alpha.order.w3 <- peerw3 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w3 <- (alpha.order.w3$total)$raw_alpha

alpha.order.w4 <- peerw4 %>% 
  dplyr::select(bfas008R, bfas018, bfas028, bfas038, bfas048R, 
                bfas058, bfas068R, bfas078R, bfas088, bfas098) %>% 
  psych::alpha()
alpha.order.w4 <- (alpha.order.w4$total)$raw_alpha

alpha.extra.w1 <- peerw1 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w1 <- (alpha.extra.w1$total)$raw_alpha

alpha.extra.w2 <- peerw2 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w2 <- (alpha.extra.w2$total)$raw_alpha

alpha.extra.w3 <- peerw3 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w3 <- (alpha.extra.w3$total)$raw_alpha

alpha.extra.w4 <- peerw4 %>% 
  dplyr::select(bfas004, bfas009, bfas014R, bfas019, bfas024R, 
                bfas029R, bfas034R, bfas039, bfas044, bfas049R, 
                bfas054R, bfas059, bfas064R, bfas069, bfas074, 
                bfas079R, bfas084, bfas089, bfas094, bfas099R) %>% 
  psych::alpha()
alpha.extra.w4 <- (alpha.extra.w4$total)$raw_alpha

alpha.assert.w1 <- peerw1 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w1 <- (alpha.assert.w1$total)$raw_alpha

alpha.assert.w2 <- peerw2 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w2 <- (alpha.assert.w2$total)$raw_alpha

alpha.assert.w3 <- peerw3 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w3 <- (alpha.assert.w3$total)$raw_alpha

alpha.assert.w4 <- peerw4 %>% 
  dplyr::select(bfas009, bfas019, bfas029R, bfas039, bfas049R, 
                bfas059, bfas069, bfas079R, bfas089, bfas099R) %>% 
  psych::alpha()
alpha.assert.w4 <- (alpha.assert.w4$total)$raw_alpha

alpha.enthu.w1 <- peerw1 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w1 <- (alpha.enthu.w1$total)$raw_alpha

alpha.enthu.w2 <- peerw2 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w2 <- (alpha.enthu.w2$total)$raw_alpha

alpha.enthu.w3 <- peerw3 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w3 <- (alpha.enthu.w3$total)$raw_alpha

alpha.enthu.w4 <- peerw4 %>% 
  dplyr::select(bfas004, bfas014R, bfas024R, bfas034R, bfas044,
                bfas054R, bfas064R, bfas074, bfas084, bfas094) %>% 
  psych::alpha()
alpha.enthu.w4 <- (alpha.enthu.w4$total)$raw_alpha

alpha.neuro.w1 <- peerw1 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w1 <- (alpha.neuro.w1$total)$raw_alpha

alpha.neuro.w2 <- peerw2 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w2 <- (alpha.neuro.w2$total)$raw_alpha

alpha.neuro.w3 <- peerw3 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w3 <- (alpha.neuro.w3$total)$raw_alpha

alpha.neuro.w4 <- peerw4 %>% 
  dplyr::select(bfas001R, bfas006, bfas011, bfas016R, bfas021R,
                bfas026, bfas031, bfas036R, bfas041, bfas046, 
                bfas051, bfas056R, bfas061, bfas066, bfas071R, 
                bfas076R, bfas081, bfas086, bfas091, bfas096) %>% 
  psych::alpha()
alpha.neuro.w4 <- (alpha.neuro.w4$total)$raw_alpha

alpha.withd.w1 <- peerw1 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w1 <- (alpha.withd.w1$total)$raw_alpha

alpha.withd.w2 <- peerw2 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w2 <- (alpha.withd.w2$total)$raw_alpha

alpha.withd.w3 <- peerw3 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w3 <- (alpha.withd.w3$total)$raw_alpha

alpha.withd.w4 <- peerw4 %>% 
  dplyr::select(bfas001R, bfas011, bfas021R, bfas031, bfas041, 
                bfas051, bfas061, bfas071R, bfas081, bfas091) %>% 
  psych::alpha()
alpha.withd.w4 <- (alpha.withd.w4$total)$raw_alpha

alpha.volat.w1 <- peerw1 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w1 <- (alpha.volat.w1$total)$raw_alpha

alpha.volat.w2 <- peerw2 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w2 <- (alpha.volat.w2$total)$raw_alpha

alpha.volat.w3 <- peerw3 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w3 <- (alpha.volat.w3$total)$raw_alpha

alpha.volat.w4 <- peerw4 %>% 
  dplyr::select(bfas006, bfas016R, bfas026, bfas036R, bfas046, 
                bfas056R, bfas066, bfas076R, bfas086, bfas096) %>% 
  psych::alpha()
alpha.volat.w4 <- (alpha.volat.w4$total)$raw_alpha

alpha.opend.w1 <- peerw1 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w1 <- (alpha.opend.w1$total)$raw_alpha

alpha.opend.w2 <- peerw2 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w2 <- (alpha.opend.w2$total)$raw_alpha

alpha.opend.w3 <- peerw3 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w3 <- (alpha.opend.w3$total)$raw_alpha

alpha.opend.w4 <- peerw4 %>% 
  dplyr::select(bfas005, bfas010, bfas015R, bfas020, bfas025,
                bfas030, bfas035, bfas040, bfas045R, bfas050R, 
                bfas055R, bfas060R, bfas065, bfas070, bfas075, 
                bfas080R, bfas085R, bfas090R, bfas095, bfas100) %>% 
  psych::alpha()
alpha.opend.w4 <- (alpha.opend.w4$total)$raw_alpha

alpha.intel.w1 <- peerw1 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w1 <- (alpha.intel.w1$total)$raw_alpha

alpha.intel.w2 <- peerw2 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w2 <- (alpha.intel.w2$total)$raw_alpha

alpha.intel.w3 <- peerw3 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w3 <- (alpha.intel.w3$total)$raw_alpha

alpha.intel.w4 <- peerw4 %>% 
  dplyr::select(bfas005, bfas015R, bfas025, bfas035, bfas045R,
                bfas055R, bfas065, bfas075, bfas085R, bfas095) %>% 
  psych::alpha()
alpha.intel.w4 <- (alpha.intel.w4$total)$raw_alpha

alpha.opena.w1 <- peerw1 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w1 <- (alpha.opena.w1$total)$raw_alpha

alpha.opena.w2 <- peerw2 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w2 <- (alpha.opena.w2$total)$raw_alpha

alpha.opena.w3 <- peerw3 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w3 <- (alpha.opena.w3$total)$raw_alpha

alpha.opena.w4 <- peerw4 %>% 
  dplyr::select(bfas010, bfas020, bfas030, bfas040, bfas050R, 
                bfas060R, bfas070, bfas080R, bfas090R, bfas100) %>% 
  psych::alpha()
alpha.opena.w4 <- (alpha.opena.w4$total)$raw_alpha

alpha.coher.w1 <- peerw1 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w1 <- (alpha.coher.w1$total)$raw_alpha

alpha.coher.w2 <- peerw2 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w2 <- (alpha.coher.w2$total)$raw_alpha

alpha.coher.w3 <- peerw3 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w3 <- (alpha.coher.w3$total)$raw_alpha

alpha.coher.w4 <- peerw4 %>% 
  dplyr::select(epsi02, epsi04, epsi05, epsi06, epsi08, epsi09) %>% 
  psych::alpha()
alpha.coher.w4 <- (alpha.coher.w4$total)$raw_alpha

alpha.confu.w1 <- peerw1 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w1 <- (alpha.confu.w1$total)$raw_alpha

alpha.confu.w2 <- peerw2 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w2 <- (alpha.confu.w2$total)$raw_alpha

alpha.confu.w3 <- peerw3 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w3 <- (alpha.confu.w3$total)$raw_alpha

alpha.confu.w4 <- peerw4 %>% 
  dplyr::select(epsi01, epsi03, epsi07, epsi10, epsi11, epsi12) %>% 
  psych::alpha()
alpha.confu.w4 <- (alpha.confu.w4$total)$raw_alpha

# PEER - SIMULATION ============================
# > Domains----
#specify means
means.domain <- c(mean(peerw1$bfas_agreeableness, na.rm = TRUE),
                  mean(peerw1$bfas_conscientiousness, na.rm = TRUE),
                  mean(peerw1$bfas_extraversion, na.rm = TRUE),
                  mean(peerw1$bfas_neuroticism, na.rm = TRUE),
                  mean(peerw1$bfas_opennessdomain, na.rm = TRUE),
                  mean(peerw2$bfas_agreeableness, na.rm = TRUE),
                  mean(peerw2$bfas_conscientiousness, na.rm = TRUE),
                  mean(peerw2$bfas_extraversion, na.rm = TRUE),
                  mean(peerw2$bfas_neuroticism, na.rm = TRUE),
                  mean(peerw2$bfas_opennessdomain, na.rm = TRUE),
                  mean(peerw3$bfas_agreeableness, na.rm = TRUE),
                  mean(peerw3$bfas_conscientiousness, na.rm = TRUE),
                  mean(peerw3$bfas_extraversion, na.rm = TRUE),
                  mean(peerw3$bfas_neuroticism, na.rm = TRUE),
                  mean(peerw3$bfas_opennessdomain, na.rm = TRUE),
                  mean(peerw4$bfas_agreeableness, na.rm = TRUE),
                  mean(peerw4$bfas_conscientiousness, na.rm = TRUE),
                  mean(peerw4$bfas_extraversion, na.rm = TRUE),
                  mean(peerw4$bfas_neuroticism, na.rm = TRUE),
                  mean(peerw4$bfas_opennessdomain, na.rm = TRUE))

#specify sd
sd.domain <- c(sd(peerw1$bfas_agreeableness, na.rm = TRUE),
               sd(peerw1$bfas_conscientiousness, na.rm = TRUE),
               sd(peerw1$bfas_extraversion, na.rm = TRUE),
               sd(peerw1$bfas_neuroticism, na.rm = TRUE),
               sd(peerw1$bfas_opennessdomain, na.rm = TRUE),
               sd(peerw2$bfas_agreeableness, na.rm = TRUE),
               sd(peerw2$bfas_conscientiousness, na.rm = TRUE),
               sd(peerw2$bfas_extraversion, na.rm = TRUE),
               sd(peerw2$bfas_neuroticism, na.rm = TRUE),
               sd(peerw2$bfas_opennessdomain, na.rm = TRUE),
               sd(peerw3$bfas_agreeableness, na.rm = TRUE),
               sd(peerw3$bfas_conscientiousness, na.rm = TRUE),
               sd(peerw3$bfas_extraversion, na.rm = TRUE),
               sd(peerw3$bfas_neuroticism, na.rm = TRUE),
               sd(peerw3$bfas_opennessdomain, na.rm = TRUE),
               sd(peerw4$bfas_agreeableness, na.rm = TRUE),
               sd(peerw4$bfas_conscientiousness, na.rm = TRUE),
               sd(peerw4$bfas_extraversion, na.rm = TRUE),
               sd(peerw4$bfas_neuroticism, na.rm = TRUE),
               sd(peerw4$bfas_opennessdomain, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between-trait correlations
# And perfect between wave correlations to show no change
R.domain <- Rreal.domain
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w3"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w1","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w1"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w3"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w2","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w2"] <- 1
R.domain["bfas_agreeableness_w3","bfas_agreeableness_w4"] <- 1
R.domain["bfas_agreeableness_w4","bfas_agreeableness_w3"] <- 1

R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w3"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w1","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w1"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w3"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w2","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w2"] <- 1
R.domain["bfas_conscientiousness_w3","bfas_conscientiousness_w4"] <- 1
R.domain["bfas_conscientiousness_w4","bfas_conscientiousness_w3"] <- 1

R.domain["bfas_extraversion_w1","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w1","bfas_extraversion_w3"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w1","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w1"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w3"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w2","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w2"] <- 1
R.domain["bfas_extraversion_w3","bfas_extraversion_w4"] <- 1
R.domain["bfas_extraversion_w4","bfas_extraversion_w3"] <- 1

R.domain["bfas_neuroticism_w1","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w1","bfas_neuroticism_w3"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w1","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w1"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w3"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w2","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w2"] <- 1
R.domain["bfas_neuroticism_w3","bfas_neuroticism_w4"] <- 1
R.domain["bfas_neuroticism_w4","bfas_neuroticism_w3"] <- 1

R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w3"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w1","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w1"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w3"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w2","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w2"] <- 1
R.domain["bfas_opennessdomain_w3","bfas_opennessdomain_w4"] <- 1
R.domain["bfas_opennessdomain_w4","bfas_opennessdomain_w3"] <- 1

#specify alpha
alpha.domain <- c(alpha.agree.w1, alpha.consci.w1, alpha.extra.w1, alpha.neuro.w1, alpha.opend.w1,
                  alpha.agree.w2, alpha.consci.w2, alpha.extra.w2, alpha.neuro.w2, alpha.opend.w2,
                  alpha.agree.w3, alpha.consci.w3, alpha.extra.w3, alpha.neuro.w3, alpha.opend.w3,
                  alpha.agree.w4, alpha.consci.w4, alpha.extra.w4, alpha.neuro.w4, alpha.opend.w4)

#take the square root of alphas to create the attenuation factor
alpha.domain.sq <- diag(sqrt(alpha.domain))

#apply the attenuation factor to correlation matrix
R.domain.a <- alpha.domain.sq %*% R.domain %*% alpha.domain.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.domain.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.domain.a <- sd.domain/sqrt(alpha.domain)

#unfortunately, correlation matrix is not positive definite
#some eigenvalues are negative
eigen(R.domain.a)$values
cholStatus <- try(u <- chol(R.domain.a), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

#change to positive definite 
#https://www.r-bloggers.com/fixing-non-positive-definite-correlation-matrices-using-r-2/
R.domain.new <- R.domain.a

iter <- 0
while (cholError) {
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  
  # replace -ve eigen values with small +ve number
  newEig <- eigen(R.domain.new)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  R.domain.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  
  # normalize modified matrix eqn 6 from Brissette et al 2007
  R.domain.new <- R.domain.new/sqrt(diag(R.domain.new) %*% t(diag(R.domain.new)))
  
  # try chol again
  cholStatus <- try(u <- chol(R.domain.new), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

cholError <- ifelse(eigen(R.domain.new)$values[20] < 0 , TRUE, FALSE)

iter <- 0
while (cholError) {
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  
  # replace -ve eigen values with small +ve number
  newEig <- eigen(R.domain.new)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  R.domain.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  
  # normalize modified matrix eqn 6 from Brissette et al 2007
  R.domain.new <- R.domain.new/sqrt(diag(R.domain.new) %*% t(diag(R.domain.new)))
  
  # try chol again
  cholStatus <- try(u <- chol(R.domain.new), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}


#check new matrix - now all eigen values are positive
eigen(R.domain.new)$values

#simulate n = 50000 dataset
n <- 50000

domain.sim <- genCorData(n, mu = means.domain, sigma = sd.domain.a,
                         corMatrix = R.domain.new)  

#rename simulated dataset
names(domain.sim)[-1] <- names(domain)


# > Aspects----

#specify means
means.aspect <- c(mean(peerw1$bfas_assertiveness, na.rm = TRUE),
                  mean(peerw1$bfas_compassion, na.rm = TRUE),
                  mean(peerw1$bfas_enthusiasm, na.rm = TRUE),
                  mean(peerw1$bfas_industriousness, na.rm = TRUE),
                  mean(peerw1$bfas_intellect, na.rm = TRUE),
                  mean(peerw1$bfas_opennessaspect, na.rm = TRUE),
                  mean(peerw1$bfas_orderliness, na.rm = TRUE),
                  mean(peerw1$bfas_politeness, na.rm = TRUE),
                  mean(peerw1$bfas_volatility, na.rm = TRUE),
                  mean(peerw1$bfas_withdrawal, na.rm = TRUE),
                  mean(peerw2$bfas_assertiveness, na.rm = TRUE),
                  mean(peerw2$bfas_compassion, na.rm = TRUE),
                  mean(peerw2$bfas_enthusiasm, na.rm = TRUE),
                  mean(peerw2$bfas_industriousness, na.rm = TRUE),
                  mean(peerw2$bfas_intellect, na.rm = TRUE),
                  mean(peerw2$bfas_opennessaspect, na.rm = TRUE),
                  mean(peerw2$bfas_orderliness, na.rm = TRUE),
                  mean(peerw2$bfas_politeness, na.rm = TRUE),
                  mean(peerw2$bfas_volatility, na.rm = TRUE),
                  mean(peerw2$bfas_withdrawal, na.rm = TRUE),
                  mean(peerw3$bfas_assertiveness, na.rm = TRUE),
                  mean(peerw3$bfas_compassion, na.rm = TRUE),
                  mean(peerw3$bfas_enthusiasm, na.rm = TRUE),
                  mean(peerw3$bfas_industriousness, na.rm = TRUE),
                  mean(peerw3$bfas_intellect, na.rm = TRUE),
                  mean(peerw3$bfas_opennessaspect, na.rm = TRUE),
                  mean(peerw3$bfas_orderliness, na.rm = TRUE),
                  mean(peerw3$bfas_politeness, na.rm = TRUE),
                  mean(peerw3$bfas_volatility, na.rm = TRUE),
                  mean(peerw3$bfas_withdrawal, na.rm = TRUE),
                  mean(peerw4$bfas_assertiveness, na.rm = TRUE),
                  mean(peerw4$bfas_compassion, na.rm = TRUE),
                  mean(peerw4$bfas_enthusiasm, na.rm = TRUE),
                  mean(peerw4$bfas_industriousness, na.rm = TRUE),
                  mean(peerw4$bfas_intellect, na.rm = TRUE),
                  mean(peerw4$bfas_opennessaspect, na.rm = TRUE),
                  mean(peerw4$bfas_orderliness, na.rm = TRUE),
                  mean(peerw4$bfas_politeness, na.rm = TRUE),
                  mean(peerw4$bfas_volatility, na.rm = TRUE),
                  mean(peerw4$bfas_withdrawal, na.rm = TRUE))

#specify sd
sd.aspect <- c(sd(peerw1$bfas_assertiveness, na.rm = TRUE),
                  sd(peerw1$bfas_compassion, na.rm = TRUE),
                  sd(peerw1$bfas_enthusiasm, na.rm = TRUE),
                  sd(peerw1$bfas_industriousness, na.rm = TRUE),
                  sd(peerw1$bfas_intellect, na.rm = TRUE),
                  sd(peerw1$bfas_opennessaspect, na.rm = TRUE),
                  sd(peerw1$bfas_orderliness, na.rm = TRUE),
                  sd(peerw1$bfas_politeness, na.rm = TRUE),
                  sd(peerw1$bfas_volatility, na.rm = TRUE),
                  sd(peerw1$bfas_withdrawal, na.rm = TRUE),
                  sd(peerw2$bfas_assertiveness, na.rm = TRUE),
                  sd(peerw2$bfas_compassion, na.rm = TRUE),
                  sd(peerw2$bfas_enthusiasm, na.rm = TRUE),
                  sd(peerw2$bfas_industriousness, na.rm = TRUE),
                  sd(peerw2$bfas_intellect, na.rm = TRUE),
                  sd(peerw2$bfas_opennessaspect, na.rm = TRUE),
                  sd(peerw2$bfas_orderliness, na.rm = TRUE),
                  sd(peerw2$bfas_politeness, na.rm = TRUE),
                  sd(peerw2$bfas_volatility, na.rm = TRUE),
                  sd(peerw2$bfas_withdrawal, na.rm = TRUE),
                  sd(peerw3$bfas_assertiveness, na.rm = TRUE),
                  sd(peerw3$bfas_compassion, na.rm = TRUE),
                  sd(peerw3$bfas_enthusiasm, na.rm = TRUE),
                  sd(peerw3$bfas_industriousness, na.rm = TRUE),
                  sd(peerw3$bfas_intellect, na.rm = TRUE),
                  sd(peerw3$bfas_opennessaspect, na.rm = TRUE),
                  sd(peerw3$bfas_orderliness, na.rm = TRUE),
                  sd(peerw3$bfas_politeness, na.rm = TRUE),
                  sd(peerw3$bfas_volatility, na.rm = TRUE),
                  sd(peerw3$bfas_withdrawal, na.rm = TRUE),
                  sd(peerw4$bfas_assertiveness, na.rm = TRUE),
                  sd(peerw4$bfas_compassion, na.rm = TRUE),
                  sd(peerw4$bfas_enthusiasm, na.rm = TRUE),
                  sd(peerw4$bfas_industriousness, na.rm = TRUE),
                  sd(peerw4$bfas_intellect, na.rm = TRUE),
                  sd(peerw4$bfas_opennessaspect, na.rm = TRUE),
                  sd(peerw4$bfas_orderliness, na.rm = TRUE),
                  sd(peerw4$bfas_politeness, na.rm = TRUE),
                  sd(peerw4$bfas_volatility, na.rm = TRUE),
                  sd(peerw4$bfas_withdrawal, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between trait correlations
# And perfect between wave correlations to show no change
R.aspect <- Rreal.aspect
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w3"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w1","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w1"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w3"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w2","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w2"] <- 1
R.aspect["bfas_assertiveness_w3","bfas_assertiveness_w4"] <- 1
R.aspect["bfas_assertiveness_w4","bfas_assertiveness_w3"] <- 1

R.aspect["bfas_compassion_w1","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w1","bfas_compassion_w3"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w1","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w1"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w3"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w2","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w2"] <- 1
R.aspect["bfas_compassion_w3","bfas_compassion_w4"] <- 1
R.aspect["bfas_compassion_w4","bfas_compassion_w3"] <- 1

R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w3"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w1","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w1"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w3"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w2","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w2"] <- 1
R.aspect["bfas_enthusiasm_w3","bfas_enthusiasm_w4"] <- 1
R.aspect["bfas_enthusiasm_w4","bfas_enthusiasm_w3"] <- 1

R.aspect["bfas_industriousness_w1","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w1","bfas_industriousness_w3"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w1","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w1"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w3"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w2","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w2"] <- 1
R.aspect["bfas_industriousness_w3","bfas_industriousness_w4"] <- 1
R.aspect["bfas_industriousness_w4","bfas_industriousness_w3"] <- 1

R.aspect["bfas_intellect_w1","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w1","bfas_intellect_w3"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w1","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w1"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w3"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w2","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w2"] <- 1
R.aspect["bfas_intellect_w3","bfas_intellect_w4"] <- 1
R.aspect["bfas_intellect_w4","bfas_intellect_w3"] <- 1

R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w3"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w1","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w1"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w3"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w2","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w2"] <- 1
R.aspect["bfas_opennessaspect_w3","bfas_opennessaspect_w4"] <- 1
R.aspect["bfas_opennessaspect_w4","bfas_opennessaspect_w3"] <- 1

R.aspect["bfas_orderliness_w1","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w1","bfas_orderliness_w3"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w1","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w1"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w3"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w2","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w2"] <- 1
R.aspect["bfas_orderliness_w3","bfas_orderliness_w4"] <- 1
R.aspect["bfas_orderliness_w4","bfas_orderliness_w3"] <- 1

R.aspect["bfas_politeness_w1","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w1","bfas_politeness_w3"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w1","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w1"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w3"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w2","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w2"] <- 1
R.aspect["bfas_politeness_w3","bfas_politeness_w4"] <- 1
R.aspect["bfas_politeness_w4","bfas_politeness_w3"] <- 1

R.aspect["bfas_volatility_w1","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w1","bfas_volatility_w3"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w1","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w1"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w3"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w2","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w2"] <- 1
R.aspect["bfas_volatility_w3","bfas_volatility_w4"] <- 1
R.aspect["bfas_volatility_w4","bfas_volatility_w3"] <- 1

R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w3"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w1","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w1"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w3"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w2","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w2"] <- 1
R.aspect["bfas_withdrawal_w3","bfas_withdrawal_w4"] <- 1
R.aspect["bfas_withdrawal_w4","bfas_withdrawal_w3"] <- 1

#specify alpha
alpha.aspect <- c(alpha.assert.w1, alpha.compa.w1, alpha.enthu.w1, alpha.indus.w1, alpha.intel.w1,
                  alpha.opena.w1, alpha.order.w1, alpha.polit.w1, alpha.volat.w1, alpha.withd.w1,
                  alpha.assert.w2, alpha.compa.w2, alpha.enthu.w2, alpha.indus.w2, alpha.intel.w2,
                  alpha.opena.w2, alpha.order.w2, alpha.polit.w2, alpha.volat.w2, alpha.withd.w2,
                  alpha.assert.w3, alpha.compa.w3, alpha.enthu.w3, alpha.indus.w3, alpha.intel.w3,
                  alpha.opena.w3, alpha.order.w3, alpha.polit.w3, alpha.volat.w3, alpha.withd.w3,
                  alpha.assert.w4, alpha.compa.w4, alpha.enthu.w4, alpha.indus.w4, alpha.intel.w4,
                  alpha.opena.w4, alpha.order.w4, alpha.polit.w4, alpha.volat.w4, alpha.withd.w4)

#take the square root of alphas to create the attenuation factor
alpha.aspect.sq <- diag(sqrt(alpha.aspect))

#apply the attenuation factor to correlation matrix
R.aspect.a <- alpha.aspect.sq %*% R.aspect %*% alpha.aspect.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.aspect.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.aspect.a <- sd.aspect/sqrt(alpha.aspect)

#unfortunately, correlation matrix is not positive definite
#some eigenvalues are negative
eigen(R.aspect.a)$values
cholStatus <- try(u <- chol(R.aspect.a), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

#change to positive definite 
#https://www.r-bloggers.com/fixing-non-positive-definite-correlation-matrices-using-r-2/
R.aspect.new <- R.aspect.a

iter <- 0
while (cholError) {
    iter <- iter + 1
    cat("iteration ", iter, "\n")

    # replace -ve eigen values with small +ve number
    newEig <- eigen(R.aspect.new)
    newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)

    # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
    # eig vectors
    R.aspect.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)

    # normalize modified matrix eqn 6 from Brissette et al 2007
    R.aspect.new <- R.aspect.new/sqrt(diag(R.aspect.new) %*% t(diag(R.aspect.new)))

    # try chol again
    cholStatus <- try(u <- chol(R.aspect.new), silent = TRUE)
    cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

#check new matrix - now all eigen values are positive
eigen(R.aspect.new)$values

#force matrix to be symmetrical (very very slightly off..)
R.aspect.new <- as.matrix(forceSymmetric(R.aspect.new))

#force to be positive definite AGAIN
cholError <- ifelse(eigen(R.aspect.new)$values[40] < 0 , TRUE, FALSE)

iter <- 0
while (cholError) {
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  
  # replace -ve eigen values with small +ve number
  newEig <- eigen(R.aspect.new)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  R.aspect.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  
  # normalize modified matrix eqn 6 from Brissette et al 2007
  R.aspect.new <- R.aspect.new/sqrt(diag(R.aspect.new) %*% t(diag(R.aspect.new)))
  
  # try chol again
  cholStatus <- try(u <- chol(R.aspect.new), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}


cholError <- ifelse(eigen(R.aspect.new)$values[40] < 0 , TRUE, FALSE)

iter <- 0
while (cholError) {
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  
  # replace -ve eigen values with small +ve number
  newEig <- eigen(R.aspect.new)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  R.aspect.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  
  # normalize modified matrix eqn 6 from Brissette et al 2007
  R.aspect.new <- R.aspect.new/sqrt(diag(R.aspect.new) %*% t(diag(R.aspect.new)))
  
  # try chol again
  cholStatus <- try(u <- chol(R.aspect.new), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

#simulate n = 50000 dataset
n <- 50000
aspect.sim <- genCorData(n, mu = means.aspect, sigma = sd.aspect.a,
                         corMatrix = R.aspect.new)

#rename simulated dataset
names(aspect.sim)[-1] <- colnames(R.aspect)

# > Identity----

#specify means
means.identity <- c(mean(peerw1$epsi_confusion, na.rm = TRUE),
                    mean(peerw1$epsi_coherence, na.rm = TRUE),
                    mean(peerw2$epsi_confusion, na.rm = TRUE),
                    mean(peerw2$epsi_coherence, na.rm = TRUE),
                    mean(peerw3$epsi_confusion, na.rm = TRUE),
                    mean(peerw3$epsi_coherence, na.rm = TRUE),
                    mean(peerw4$epsi_confusion, na.rm = TRUE),
                    mean(peerw4$epsi_coherence, na.rm = TRUE))

#specify sd
sd.identity <- c(sd(peerw1$epsi_confusion, na.rm = TRUE),
                 sd(peerw1$epsi_coherence, na.rm = TRUE),
                 sd(peerw2$epsi_confusion, na.rm = TRUE),
                 sd(peerw2$epsi_coherence, na.rm = TRUE),
                 sd(peerw3$epsi_confusion, na.rm = TRUE),
                 sd(peerw3$epsi_coherence, na.rm = TRUE),
                 sd(peerw4$epsi_confusion, na.rm = TRUE),
                 sd(peerw4$epsi_coherence, na.rm = TRUE))

# Specify correlation matrix for fake data
# With real between trait correlations
# And perfect between wave correlations to show no change
R.identity <- Rreal.identity
R.identity["epsi_confusion_w1","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w1","epsi_confusion_w3"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w1","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w1"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w3"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w2","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w2"] <- 1
R.identity["epsi_confusion_w3","epsi_confusion_w4"] <- 1
R.identity["epsi_confusion_w4","epsi_confusion_w3"] <- 1

R.identity["epsi_coherence_w1","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w1","epsi_coherence_w3"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w1","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w1"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w3"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w2","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w2"] <- 1
R.identity["epsi_coherence_w3","epsi_coherence_w4"] <- 1
R.identity["epsi_coherence_w4","epsi_coherence_w3"] <- 1

#specify alpha
alpha.identity <- c(alpha.confu.w1, alpha.coher.w1,
                  alpha.confu.w2, alpha.coher.w2,
                  alpha.confu.w3, alpha.coher.w3,
                  alpha.confu.w4, alpha.coher.w4)

#take the square root of alphas to create the attenuation factor
alpha.identity.sq <- diag(sqrt(alpha.identity))

#apply the attenuation factor to correlation matrix
R.identity.a <- alpha.identity.sq %*% R.identity %*% alpha.identity.sq

#replace diagonal values of correlation matrix with 1.0
diag(R.identity.a) <- 1.0

#adjust SDs for unreliability using stuff in chapter 6 from Schmidt & Hunter
sd.identity.a <- sd.identity/sqrt(alpha.identity)

#unfortunately, correlation matrix is not positive definite
#some eigenvalues are negative
eigen(R.identity.a)$values
cholStatus <- try(u <- chol(R.identity.a), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

#change to positive definite 
#https://www.r-bloggers.com/fixing-non-positive-definite-correlation-matrices-using-r-2/
R.identity.new <- R.identity.a

iter <- 0
while (cholError) {
    iter <- iter + 1
    cat("iteration ", iter, "\n")

    # replace -ve eigen values with small +ve number
    newEig <- eigen(R.identity.new)
    newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)

    # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
    # eig vectors
    R.identity.new <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)

    # normalize modified matrix eqn 6 from Brissette et al 2007
    R.identity.new <- R.identity.new/sqrt(diag(R.identity.new) %*% t(diag(R.identity.new)))

    # try chol again
    cholStatus <- try(u <- chol(R.identity.new), silent = TRUE)
    cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

#check new matrix - now all eigen values are positive
eigen(R.identity.new)$values


#simulate n = 50000 dataset
n <- 50000

identity.sim <- genCorData(n, mu = means.identity, sigma = sd.identity.a,
                         corMatrix = R.identity.new)

#rename
names(identity.sim)[-1] <- colnames(R.identity)


# PEER - EXPORT ================================

#save file
write.csv(domain.sim,"domainsim_peer.csv", row.names = FALSE)
write.csv(aspect.sim,"aspectsim_peer.csv", row.names = FALSE)
write.csv(identity.sim,"identitysim_peer.csv", row.names = FALSE)


