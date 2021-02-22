##############################################
##  First Year Project: Personality Change  ##
##  Latent models: self + peer              ##
##                                          ##
##  Linh Nguyen                             ##
##  Created: Feb-15-2021                    ##
##  Updated: Feb-22-2021                    ##
##############################################


# > Preprocessing ====
# >> Data ----
require(lavaan)
require(tidyverse)
require(semPlot)
set.seed(202102)

data <- read.csv(file = "lavaanData.csv", na.strings = "-999", header = F)
colnames(data) <- c(#self reports:
                    paste0("w1bf_", 1:100),
                    paste0("w1epsi_", 1:12), 
                    paste0("w2bf_", 1:100),
                    paste0("w2epsi_", 1:12),
                    paste0("w3bf_", 1:100),
                    paste0("w3epsi_", 1:12),
                    paste0("w4bf_", 1:100),
                    paste0("w4epsi_", 1:12),
                    #peer reports:
                    paste0("pw1bf_", 1:100),
                    paste0("pw1epsi_", 1:12),
                    paste0("pw2bf_", 1:100),
                    paste0("pw2epsi_", 1:12),
                    paste0("pw3bf_", 1:100),
                    paste0("pw3epsi_", 1:12),
                    paste0("pw4bf_", 1:100),
                    paste0("pw4epsi_", 1:12))

# >> Parcels ----

# self aspect randomization - 0, 4, 6, 7, 8 vs. 1, 2, 3, 5, 9
sample(c(0,1,2,3,4,5,6,7,8,9), 5)
# peer aspect randomization - 1, 2, 4, 5, 6 vs. 0, 3, 7, 8, 9
sample(c(0,1,2,3,4,5,6,7,8,9), 5)
# self domain randomization - 4,5,9,10,20,30,50,60,80,90 vs. 0,1,2,3,6,7,8,00,40,70 
sample(c(0,1,2,3,4,5,6,7,8,9,
         00,10,20,30,40,50,60,70,80,90), 10)
# peer domain randomization - 1,2,3,6,7,9,10,30,50,70 vs. 0,4,5,8,00,20,40,60,80,90
sample(c(0,1,2,3,4,5,6,7,8,9,
         00,10,20,30,40,50,60,70,80,90), 10)

# >>> Aspects ----

# assertiveness 
data <- data %>% 
  mutate(# self
         assertW1S = rowMeans(select(data, w1bf_9, w1bf_19, w1bf_29, w1bf_39, w1bf_49, w1bf_59,
                                     w1bf_69, w1bf_79, w1bf_89, w1bf_99), na.rm = T),
         assertW2S = rowMeans(select(data, w2bf_9, w2bf_19, w2bf_29, w2bf_39, w2bf_49, w2bf_59,
                                     w2bf_69, w2bf_79, w2bf_89, w2bf_99), na.rm = T),
         assertW3S = rowMeans(select(data, w3bf_9, w3bf_19, w3bf_29, w3bf_39, w3bf_49, w3bf_59,
                                     w3bf_69, w3bf_79, w3bf_89, w3bf_99), na.rm = T),
         assertW4S = rowMeans(select(data, w4bf_9, w4bf_19, w4bf_29, w4bf_39, w4bf_49, w4bf_59,
                                     w4bf_69, w4bf_79, w4bf_89, w4bf_99), na.rm = T),
         
         # peer
         assertW1P = rowMeans(select(data, pw1bf_9, pw1bf_19, pw1bf_29, pw1bf_39, pw1bf_49, pw1bf_59,
                                     pw1bf_69, pw1bf_79, pw1bf_89, pw1bf_99), na.rm = T),
         assertW2P = rowMeans(select(data, pw2bf_9, pw2bf_19, pw2bf_29, pw2bf_39, pw2bf_49, pw2bf_59,
                                     pw2bf_69, pw2bf_79, pw2bf_89, pw2bf_99), na.rm = T),
         assertW3P = rowMeans(select(data, pw3bf_9, pw3bf_19, pw3bf_29, pw3bf_39, pw3bf_49, pw3bf_59,
                                     pw3bf_69, pw3bf_79, pw3bf_89, pw3bf_99), na.rm = T),
         assertW4P = rowMeans(select(data, pw4bf_9, pw4bf_19, pw4bf_29, pw4bf_39, pw4bf_49, pw4bf_59,
                                     pw4bf_69, pw4bf_79, pw4bf_89, pw4bf_99), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         assertW1S1 = rowMeans(select(data, w1bf_9, w1bf_49, w1bf_69, w1bf_79, w1bf_89),na.rm = T),
         assertW2S1 = rowMeans(select(data, w2bf_9, w2bf_49, w2bf_69, w2bf_79, w2bf_89),na.rm = T),
         assertW3S1 = rowMeans(select(data, w3bf_9, w3bf_49, w3bf_69, w3bf_79, w3bf_89),na.rm = T),
         assertW4S1 = rowMeans(select(data, w4bf_9, w4bf_49, w4bf_69, w4bf_79, w4bf_89),na.rm = T),
         
         # second self parcel
         assertW1S2 = rowMeans(select(data, w1bf_19, w1bf_29, w1bf_39, w1bf_59, w1bf_99),na.rm = T),
         assertW2S2 = rowMeans(select(data, w2bf_19, w2bf_29, w2bf_39, w2bf_59, w2bf_99),na.rm = T),
         assertW3S2 = rowMeans(select(data, w3bf_19, w3bf_29, w3bf_39, w3bf_59, w3bf_99),na.rm = T),
         assertW4S2 = rowMeans(select(data, w4bf_19, w4bf_29, w4bf_39, w4bf_59, w4bf_99),na.rm = T),
         
         # first peer parcel
         assertW1P1 = rowMeans(select(data, pw1bf_19, pw1bf_29, pw1bf_49, pw1bf_59, pw1bf_69), na.rm = T),
         assertW2P1 = rowMeans(select(data, pw2bf_19, pw2bf_29, pw2bf_49, pw2bf_59, pw2bf_69), na.rm = T),
         assertW3P1 = rowMeans(select(data, pw3bf_19, pw3bf_29, pw3bf_49, pw3bf_59, pw3bf_69), na.rm = T),
         assertW4P1 = rowMeans(select(data, pw4bf_19, pw4bf_29, pw4bf_49, pw4bf_59, pw4bf_69), na.rm = T),
         
         # second peer parcel
         assertW1P2 = rowMeans(select(data, pw1bf_9, pw1bf_39, pw1bf_79, pw1bf_89, pw1bf_99), na.rm = T),
         assertW2P2 = rowMeans(select(data, pw2bf_9, pw2bf_39, pw2bf_79, pw2bf_89, pw2bf_99), na.rm = T),
         assertW3P2 = rowMeans(select(data, pw3bf_9, pw3bf_39, pw3bf_79, pw3bf_89, pw3bf_99), na.rm = T),
         assertW4P2 = rowMeans(select(data, pw4bf_9, pw4bf_39, pw4bf_79, pw4bf_89, pw4bf_99), na.rm = T))

# compassion
data <- data %>% 
  mutate(# self
         compaW1S = rowMeans(select(data, w1bf_2, w1bf_12, w1bf_22, w1bf_32, w1bf_42, w1bf_52,
                                     w1bf_62, w1bf_72, w1bf_82, w1bf_92), na.rm = T),
         compaW2S = rowMeans(select(data, w2bf_2, w2bf_12, w2bf_22, w2bf_32, w2bf_42, w2bf_52,
                                     w2bf_62, w2bf_72, w2bf_82, w2bf_92), na.rm = T),
         compaW3S = rowMeans(select(data, w3bf_2, w3bf_12, w3bf_22, w3bf_32, w3bf_42, w3bf_52,
                                     w3bf_62, w3bf_72, w3bf_82, w3bf_92), na.rm = T),
         compaW4S = rowMeans(select(data, w4bf_2, w4bf_12, w4bf_22, w4bf_32, w4bf_42, w4bf_52,
                                     w4bf_62, w4bf_72, w4bf_82, w4bf_92), na.rm = T),
         
         # peer
         compaW1P = rowMeans(select(data, pw1bf_2, pw1bf_12, pw1bf_22, pw1bf_32, pw1bf_42, pw1bf_52,
                                     pw1bf_62, pw1bf_72, pw1bf_82, pw1bf_92), na.rm = T),
         compaW2P = rowMeans(select(data, pw2bf_2, pw2bf_12, pw2bf_22, pw2bf_32, pw2bf_42, pw2bf_52,
                                     pw2bf_62, pw2bf_72, pw2bf_82, pw2bf_92), na.rm = T),
         compaW3P = rowMeans(select(data, pw3bf_2, pw3bf_12, pw3bf_22, pw3bf_32, pw3bf_42, pw3bf_52,
                                     pw3bf_62, pw3bf_72, pw3bf_82, pw3bf_92), na.rm = T),
         compaW4P = rowMeans(select(data, pw4bf_2, pw4bf_12, pw4bf_22, pw4bf_32, pw4bf_42, pw4bf_52,
                                     pw4bf_62, pw4bf_72, pw4bf_82, pw4bf_92), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         compaW1S1 = rowMeans(select(data, w1bf_2, w1bf_42, w1bf_62, w1bf_72, w1bf_82),na.rm = T),
         compaW2S1 = rowMeans(select(data, w2bf_2, w2bf_42, w2bf_62, w2bf_72, w2bf_82),na.rm = T),
         compaW3S1 = rowMeans(select(data, w3bf_2, w3bf_42, w3bf_62, w3bf_72, w3bf_82),na.rm = T),
         compaW4S1 = rowMeans(select(data, w4bf_2, w4bf_42, w4bf_62, w4bf_72, w4bf_82),na.rm = T),
         
         # second self parcel
         compaW1S2 = rowMeans(select(data, w1bf_12, w1bf_22, w1bf_32, w1bf_52, w1bf_92),na.rm = T),
         compaW2S2 = rowMeans(select(data, w2bf_12, w2bf_22, w2bf_32, w2bf_52, w2bf_92),na.rm = T),
         compaW3S2 = rowMeans(select(data, w3bf_12, w3bf_22, w3bf_32, w3bf_52, w3bf_92),na.rm = T),
         compaW4S2 = rowMeans(select(data, w4bf_12, w4bf_22, w4bf_32, w4bf_52, w4bf_92),na.rm = T),
         
         # first peer parcel
         compaW1P1 = rowMeans(select(data, pw1bf_12, pw1bf_22, pw1bf_42, pw1bf_52, pw1bf_62), na.rm = T),
         compaW2P1 = rowMeans(select(data, pw2bf_12, pw2bf_22, pw2bf_42, pw2bf_52, pw2bf_62), na.rm = T),
         compaW3P1 = rowMeans(select(data, pw3bf_12, pw3bf_22, pw3bf_42, pw3bf_52, pw3bf_62), na.rm = T),
         compaW4P1 = rowMeans(select(data, pw4bf_12, pw4bf_22, pw4bf_42, pw4bf_52, pw4bf_62), na.rm = T),
         
         # second peer parcel
         compaW1P2 = rowMeans(select(data, pw1bf_2, pw1bf_32, pw1bf_72, pw1bf_82, pw1bf_92), na.rm = T),
         compaW2P2 = rowMeans(select(data, pw2bf_2, pw2bf_32, pw2bf_72, pw2bf_82, pw2bf_92), na.rm = T),
         compaW3P2 = rowMeans(select(data, pw3bf_2, pw3bf_32, pw3bf_72, pw3bf_82, pw3bf_92), na.rm = T),
         compaW4P2 = rowMeans(select(data, pw4bf_2, pw4bf_32, pw4bf_72, pw4bf_82, pw4bf_92), na.rm = T))

# enthusiasm
data <- data %>% 
  mutate(# self
         enthuW1S = rowMeans(select(data, w1bf_4, w1bf_14, w1bf_24, w1bf_34, w1bf_44, w1bf_54,
                                     w1bf_64, w1bf_74, w1bf_84, w1bf_94), na.rm = T),
         enthuW2S = rowMeans(select(data, w2bf_4, w2bf_14, w2bf_24, w2bf_34, w2bf_44, w2bf_54,
                                     w2bf_64, w2bf_74, w2bf_84, w2bf_94), na.rm = T),
         enthuW3S = rowMeans(select(data, w3bf_4, w3bf_14, w3bf_24, w3bf_34, w3bf_44, w3bf_54,
                                     w3bf_64, w3bf_74, w3bf_84, w3bf_94), na.rm = T),
         enthuW4S = rowMeans(select(data, w4bf_4, w4bf_14, w4bf_24, w4bf_34, w4bf_44, w4bf_54,
                                     w4bf_64, w4bf_74, w4bf_84, w4bf_94), na.rm = T),
         
         # peer
         enthuW1P = rowMeans(select(data, pw1bf_4, pw1bf_14, pw1bf_24, pw1bf_34, pw1bf_44, pw1bf_54,
                                     pw1bf_64, pw1bf_74, pw1bf_84, pw1bf_94), na.rm = T),
         enthuW2P = rowMeans(select(data, pw2bf_4, pw2bf_14, pw2bf_24, pw2bf_34, pw2bf_44, pw2bf_54,
                                     pw2bf_64, pw2bf_74, pw2bf_84, pw2bf_94), na.rm = T),
         enthuW3P = rowMeans(select(data, pw3bf_4, pw3bf_14, pw3bf_24, pw3bf_34, pw3bf_44, pw3bf_54,
                                     pw3bf_64, pw3bf_74, pw3bf_84, pw3bf_94), na.rm = T),
         enthuW4P = rowMeans(select(data, pw4bf_4, pw4bf_14, pw4bf_24, pw4bf_34, pw4bf_44, pw4bf_54,
                                     pw4bf_64, pw4bf_74, pw4bf_84, pw4bf_94), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         enthuW1S1 = rowMeans(select(data, w1bf_4, w1bf_44, w1bf_64, w1bf_74, w1bf_84),na.rm = T),
         enthuW2S1 = rowMeans(select(data, w2bf_4, w2bf_44, w2bf_64, w2bf_74, w2bf_84),na.rm = T),
         enthuW3S1 = rowMeans(select(data, w3bf_4, w3bf_44, w3bf_64, w3bf_74, w3bf_84),na.rm = T),
         enthuW4S1 = rowMeans(select(data, w4bf_4, w4bf_44, w4bf_64, w4bf_74, w4bf_84),na.rm = T),
         
         # second self parcel
         enthuW1S2 = rowMeans(select(data, w1bf_14, w1bf_24, w1bf_34, w1bf_54, w1bf_94),na.rm = T),
         enthuW2S2 = rowMeans(select(data, w2bf_14, w2bf_24, w2bf_34, w2bf_54, w2bf_94),na.rm = T),
         enthuW3S2 = rowMeans(select(data, w3bf_14, w3bf_24, w3bf_34, w3bf_54, w3bf_94),na.rm = T),
         enthuW4S2 = rowMeans(select(data, w4bf_14, w4bf_24, w4bf_34, w4bf_54, w4bf_94),na.rm = T),
         
         # first peer parcel
         enthuW1P1 = rowMeans(select(data, pw1bf_14, pw1bf_24, pw1bf_44, pw1bf_54, pw1bf_64), na.rm = T),
         enthuW2P1 = rowMeans(select(data, pw2bf_14, pw2bf_24, pw2bf_44, pw2bf_54, pw2bf_64), na.rm = T),
         enthuW3P1 = rowMeans(select(data, pw3bf_14, pw3bf_24, pw3bf_44, pw3bf_54, pw3bf_64), na.rm = T),
         enthuW4P1 = rowMeans(select(data, pw4bf_14, pw4bf_24, pw4bf_44, pw4bf_54, pw4bf_64), na.rm = T),
         
         # second peer parcel
         enthuW1P2 = rowMeans(select(data, pw1bf_4, pw1bf_34, pw1bf_74, pw1bf_84, pw1bf_94), na.rm = T),
         enthuW2P2 = rowMeans(select(data, pw2bf_4, pw2bf_34, pw2bf_74, pw2bf_84, pw2bf_94), na.rm = T),
         enthuW3P2 = rowMeans(select(data, pw3bf_4, pw3bf_34, pw3bf_74, pw3bf_84, pw3bf_94), na.rm = T),
         enthuW4P2 = rowMeans(select(data, pw4bf_4, pw4bf_34, pw4bf_74, pw4bf_84, pw4bf_94), na.rm = T))

# industriousness
data <- data %>% 
  mutate(# self
         indusW1S = rowMeans(select(data, w1bf_3, w1bf_13, w1bf_23, w1bf_33, w1bf_43, w1bf_53,
                                     w1bf_63, w1bf_73, w1bf_83, w1bf_93), na.rm = T),
         indusW2S = rowMeans(select(data, w2bf_3, w2bf_13, w2bf_23, w2bf_33, w2bf_43, w2bf_53,
                                     w2bf_63, w2bf_73, w2bf_83, w2bf_93), na.rm = T),
         indusW3S = rowMeans(select(data, w3bf_3, w3bf_13, w3bf_23, w3bf_33, w3bf_43, w3bf_53,
                                     w3bf_63, w3bf_73, w3bf_83, w3bf_93), na.rm = T),
         indusW4S = rowMeans(select(data, w4bf_3, w4bf_13, w4bf_23, w4bf_33, w4bf_43, w4bf_53,
                                     w4bf_63, w4bf_73, w4bf_83, w4bf_93), na.rm = T),
         
         # peer
         indusW1P = rowMeans(select(data, pw1bf_3, pw1bf_13, pw1bf_23, pw1bf_33, pw1bf_43, pw1bf_53,
                                     pw1bf_63, pw1bf_73, pw1bf_83, pw1bf_93), na.rm = T),
         indusW2P = rowMeans(select(data, pw2bf_3, pw2bf_13, pw2bf_23, pw2bf_33, pw2bf_43, pw2bf_53,
                                     pw2bf_63, pw2bf_73, pw2bf_83, pw2bf_93), na.rm = T),
         indusW3P = rowMeans(select(data, pw3bf_3, pw3bf_13, pw3bf_23, pw3bf_33, pw3bf_43, pw3bf_53,
                                     pw3bf_63, pw3bf_73, pw3bf_83, pw3bf_93), na.rm = T),
         indusW4P = rowMeans(select(data, pw4bf_3, pw4bf_13, pw4bf_23, pw4bf_33, pw4bf_43, pw4bf_53,
                                     pw4bf_63, pw4bf_73, pw4bf_83, pw4bf_93), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         indusW1S1 = rowMeans(select(data, w1bf_3, w1bf_43, w1bf_63, w1bf_73, w1bf_83),na.rm = T),
         indusW2S1 = rowMeans(select(data, w2bf_3, w2bf_43, w2bf_63, w2bf_73, w2bf_83),na.rm = T),
         indusW3S1 = rowMeans(select(data, w3bf_3, w3bf_43, w3bf_63, w3bf_73, w3bf_83),na.rm = T),
         indusW4S1 = rowMeans(select(data, w4bf_3, w4bf_43, w4bf_63, w4bf_73, w4bf_83),na.rm = T),
         
         # second self parcel
         indusW1S2 = rowMeans(select(data, w1bf_13, w1bf_23, w1bf_33, w1bf_53, w1bf_93),na.rm = T),
         indusW2S2 = rowMeans(select(data, w2bf_13, w2bf_23, w2bf_33, w2bf_53, w2bf_93),na.rm = T),
         indusW3S2 = rowMeans(select(data, w3bf_13, w3bf_23, w3bf_33, w3bf_53, w3bf_93),na.rm = T),
         indusW4S2 = rowMeans(select(data, w4bf_13, w4bf_23, w4bf_33, w4bf_53, w4bf_93),na.rm = T),
         
         # first peer parcel
         indusW1P1 = rowMeans(select(data, pw1bf_13, pw1bf_23, pw1bf_43, pw1bf_53, pw1bf_63), na.rm = T),
         indusW2P1 = rowMeans(select(data, pw2bf_13, pw2bf_23, pw2bf_43, pw2bf_53, pw2bf_63), na.rm = T),
         indusW3P1 = rowMeans(select(data, pw3bf_13, pw3bf_23, pw3bf_43, pw3bf_53, pw3bf_63), na.rm = T),
         indusW4P1 = rowMeans(select(data, pw4bf_13, pw4bf_23, pw4bf_43, pw4bf_53, pw4bf_63), na.rm = T),
         
         # second peer parcel
         indusW1P2 = rowMeans(select(data, pw1bf_3, pw1bf_33, pw1bf_73, pw1bf_83, pw1bf_93), na.rm = T),
         indusW2P2 = rowMeans(select(data, pw2bf_3, pw2bf_33, pw2bf_73, pw2bf_83, pw2bf_93), na.rm = T),
         indusW3P2 = rowMeans(select(data, pw3bf_3, pw3bf_33, pw3bf_73, pw3bf_83, pw3bf_93), na.rm = T),
         indusW4P2 = rowMeans(select(data, pw4bf_3, pw4bf_33, pw4bf_73, pw4bf_83, pw4bf_93), na.rm = T))

# intellect
data <- data %>% 
  mutate(# self
         intelW1S = rowMeans(select(data, w1bf_5, w1bf_15, w1bf_25, w1bf_35, w1bf_45, w1bf_55,
                                     w1bf_65, w1bf_75, w1bf_85, w1bf_95), na.rm = T),
         intelW2S = rowMeans(select(data, w2bf_5, w2bf_15, w2bf_25, w2bf_35, w2bf_45, w2bf_55,
                                     w2bf_65, w2bf_75, w2bf_85, w2bf_95), na.rm = T),
         intelW3S = rowMeans(select(data, w3bf_5, w3bf_15, w3bf_25, w3bf_35, w3bf_45, w3bf_55,
                                     w3bf_65, w3bf_75, w3bf_85, w3bf_95), na.rm = T),
         intelW4S = rowMeans(select(data, w4bf_5, w4bf_15, w4bf_25, w4bf_35, w4bf_45, w4bf_55,
                                     w4bf_65, w4bf_75, w4bf_85, w4bf_95), na.rm = T),
         
         # peer
         intelW1P = rowMeans(select(data, pw1bf_5, pw1bf_15, pw1bf_25, pw1bf_35, pw1bf_45, pw1bf_55,
                                     pw1bf_65, pw1bf_75, pw1bf_85, pw1bf_95), na.rm = T),
         intelW2P = rowMeans(select(data, pw2bf_5, pw2bf_15, pw2bf_25, pw2bf_35, pw2bf_45, pw2bf_55,
                                     pw2bf_65, pw2bf_75, pw2bf_85, pw2bf_95), na.rm = T),
         intelW3P = rowMeans(select(data, pw3bf_5, pw3bf_15, pw3bf_25, pw3bf_35, pw3bf_45, pw3bf_55,
                                     pw3bf_65, pw3bf_75, pw3bf_85, pw3bf_95), na.rm = T),
         intelW4P = rowMeans(select(data, pw4bf_5, pw4bf_15, pw4bf_25, pw4bf_35, pw4bf_45, pw4bf_55,
                                     pw4bf_65, pw4bf_75, pw4bf_85, pw4bf_95), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         intelW1S1 = rowMeans(select(data, w1bf_5, w1bf_45, w1bf_65, w1bf_75, w1bf_85),na.rm = T),
         intelW2S1 = rowMeans(select(data, w2bf_5, w2bf_45, w2bf_65, w2bf_75, w2bf_85),na.rm = T),
         intelW3S1 = rowMeans(select(data, w3bf_5, w3bf_45, w3bf_65, w3bf_75, w3bf_85),na.rm = T),
         intelW4S1 = rowMeans(select(data, w4bf_5, w4bf_45, w4bf_65, w4bf_75, w4bf_85),na.rm = T),
         
         # second self parcel
         intelW1S2 = rowMeans(select(data, w1bf_15, w1bf_25, w1bf_35, w1bf_55, w1bf_95),na.rm = T),
         intelW2S2 = rowMeans(select(data, w2bf_15, w2bf_25, w2bf_35, w2bf_55, w2bf_95),na.rm = T),
         intelW3S2 = rowMeans(select(data, w3bf_15, w3bf_25, w3bf_35, w3bf_55, w3bf_95),na.rm = T),
         intelW4S2 = rowMeans(select(data, w4bf_15, w4bf_25, w4bf_35, w4bf_55, w4bf_95),na.rm = T),
         
         # first peer parcel
         intelW1P1 = rowMeans(select(data, pw1bf_15, pw1bf_25, pw1bf_45, pw1bf_55, pw1bf_65), na.rm = T),
         intelW2P1 = rowMeans(select(data, pw2bf_15, pw2bf_25, pw2bf_45, pw2bf_55, pw2bf_65), na.rm = T),
         intelW3P1 = rowMeans(select(data, pw3bf_15, pw3bf_25, pw3bf_45, pw3bf_55, pw3bf_65), na.rm = T),
         intelW4P1 = rowMeans(select(data, pw4bf_15, pw4bf_25, pw4bf_45, pw4bf_55, pw4bf_65), na.rm = T),
         
         # second peer parcel
         intelW1P2 = rowMeans(select(data, pw1bf_5, pw1bf_35, pw1bf_75, pw1bf_85, pw1bf_95), na.rm = T),
         intelW2P2 = rowMeans(select(data, pw2bf_5, pw2bf_35, pw2bf_75, pw2bf_85, pw2bf_95), na.rm = T),
         intelW3P2 = rowMeans(select(data, pw3bf_5, pw3bf_35, pw3bf_75, pw3bf_85, pw3bf_95), na.rm = T),
         intelW4P2 = rowMeans(select(data, pw4bf_5, pw4bf_35, pw4bf_75, pw4bf_85, pw4bf_95), na.rm = T))

# openness aspect
data <- data %>% 
  mutate(# self
         openaW1S = rowMeans(select(data, w1bf_100, w1bf_10, w1bf_20, w1bf_30, w1bf_40, w1bf_50,
                                     w1bf_60, w1bf_70, w1bf_80, w1bf_90), na.rm = T),
         openaW2S = rowMeans(select(data, w2bf_100, w2bf_10, w2bf_20, w2bf_30, w2bf_40, w2bf_50,
                                     w2bf_60, w2bf_70, w2bf_80, w2bf_90), na.rm = T),
         openaW3S = rowMeans(select(data, w3bf_100, w3bf_10, w3bf_20, w3bf_30, w3bf_40, w3bf_50,
                                     w3bf_60, w3bf_70, w3bf_80, w3bf_90), na.rm = T),
         openaW4S = rowMeans(select(data, w4bf_100, w4bf_10, w4bf_20, w4bf_30, w4bf_40, w4bf_50,
                                     w4bf_60, w4bf_70, w4bf_80, w4bf_90), na.rm = T),
         
         # peer
         openaW1P = rowMeans(select(data, pw1bf_100, pw1bf_10, pw1bf_20, pw1bf_30, pw1bf_40, pw1bf_50,
                                     pw1bf_60, pw1bf_70, pw1bf_80, pw1bf_90), na.rm = T),
         openaW2P = rowMeans(select(data, pw2bf_100, pw2bf_10, pw2bf_20, pw2bf_30, pw2bf_40, pw2bf_50,
                                     pw2bf_60, pw2bf_70, pw2bf_80, pw2bf_90), na.rm = T),
         openaW3P = rowMeans(select(data, pw3bf_100, pw3bf_10, pw3bf_20, pw3bf_30, pw3bf_40, pw3bf_50,
                                     pw3bf_60, pw3bf_70, pw3bf_80, pw3bf_90), na.rm = T),
         openaW4P = rowMeans(select(data, pw4bf_100, pw4bf_10, pw4bf_20, pw4bf_30, pw4bf_40, pw4bf_50,
                                     pw4bf_60, pw4bf_70, pw4bf_80, pw4bf_90), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         openaW1S1 = rowMeans(select(data, w1bf_100, w1bf_40, w1bf_60, w1bf_70, w1bf_84),na.rm = T),
         openaW2S1 = rowMeans(select(data, w2bf_100, w2bf_40, w2bf_60, w2bf_70, w2bf_84),na.rm = T),
         openaW3S1 = rowMeans(select(data, w3bf_100, w3bf_40, w3bf_60, w3bf_70, w3bf_84),na.rm = T),
         openaW4S1 = rowMeans(select(data, w4bf_100, w4bf_40, w4bf_60, w4bf_70, w4bf_84),na.rm = T),
         
         # second self parcel
         openaW1S2 = rowMeans(select(data, w1bf_10, w1bf_20, w1bf_30, w1bf_50, w1bf_94),na.rm = T),
         openaW2S2 = rowMeans(select(data, w2bf_10, w2bf_20, w2bf_30, w2bf_50, w2bf_94),na.rm = T),
         openaW3S2 = rowMeans(select(data, w3bf_10, w3bf_20, w3bf_30, w3bf_50, w3bf_94),na.rm = T),
         openaW4S2 = rowMeans(select(data, w4bf_10, w4bf_20, w4bf_30, w4bf_50, w4bf_94),na.rm = T),
         
         # first peer parcel
         openaW1P1 = rowMeans(select(data, pw1bf_10, pw1bf_20, pw1bf_40, pw1bf_50, pw1bf_64), na.rm = T),
         openaW2P1 = rowMeans(select(data, pw2bf_10, pw2bf_20, pw2bf_40, pw2bf_50, pw2bf_64), na.rm = T),
         openaW3P1 = rowMeans(select(data, pw3bf_10, pw3bf_20, pw3bf_40, pw3bf_50, pw3bf_64), na.rm = T),
         openaW4P1 = rowMeans(select(data, pw4bf_10, pw4bf_20, pw4bf_40, pw4bf_50, pw4bf_64), na.rm = T),
         
         # second peer parcel
         openaW1P2 = rowMeans(select(data, pw1bf_100, pw1bf_30, pw1bf_70, pw1bf_80, pw1bf_94), na.rm = T),
         openaW2P2 = rowMeans(select(data, pw2bf_100, pw2bf_30, pw2bf_70, pw2bf_80, pw2bf_94), na.rm = T),
         openaW3P2 = rowMeans(select(data, pw3bf_100, pw3bf_30, pw3bf_70, pw3bf_80, pw3bf_94), na.rm = T),
         openaW4P2 = rowMeans(select(data, pw4bf_100, pw4bf_30, pw4bf_70, pw4bf_80, pw4bf_94), na.rm = T))

# orderliness
data <- data %>% 
  mutate(# self
         orderW1S = rowMeans(select(data, w1bf_8, w1bf_18, w1bf_28, w1bf_38, w1bf_48, w1bf_58,
                                     w1bf_68, w1bf_78, w1bf_88, w1bf_98), na.rm = T),
         orderW2S = rowMeans(select(data, w2bf_8, w2bf_18, w2bf_28, w2bf_38, w2bf_48, w2bf_58,
                                     w2bf_68, w2bf_78, w2bf_88, w2bf_98), na.rm = T),
         orderW3S = rowMeans(select(data, w3bf_8, w3bf_18, w3bf_28, w3bf_38, w3bf_48, w3bf_58,
                                     w3bf_68, w3bf_78, w3bf_88, w3bf_98), na.rm = T),
         orderW4S = rowMeans(select(data, w4bf_8, w4bf_18, w4bf_28, w4bf_38, w4bf_48, w4bf_58,
                                     w4bf_68, w4bf_78, w4bf_88, w4bf_98), na.rm = T),
         
         # peer
         orderW1P = rowMeans(select(data, pw1bf_8, pw1bf_18, pw1bf_28, pw1bf_38, pw1bf_48, pw1bf_58,
                                     pw1bf_68, pw1bf_78, pw1bf_88, pw1bf_98), na.rm = T),
         orderW2P = rowMeans(select(data, pw2bf_8, pw2bf_18, pw2bf_28, pw2bf_38, pw2bf_48, pw2bf_58,
                                     pw2bf_68, pw2bf_78, pw2bf_88, pw2bf_98), na.rm = T),
         orderW3P = rowMeans(select(data, pw3bf_8, pw3bf_18, pw3bf_28, pw3bf_38, pw3bf_48, pw3bf_58,
                                     pw3bf_68, pw3bf_78, pw3bf_88, pw3bf_98), na.rm = T),
         orderW4P = rowMeans(select(data, pw4bf_8, pw4bf_18, pw4bf_28, pw4bf_38, pw4bf_48, pw4bf_58,
                                     pw4bf_68, pw4bf_78, pw4bf_88, pw4bf_98), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         orderW1S1 = rowMeans(select(data, w1bf_8, w1bf_48, w1bf_68, w1bf_78, w1bf_88),na.rm = T),
         orderW2S1 = rowMeans(select(data, w2bf_8, w2bf_48, w2bf_68, w2bf_78, w2bf_88),na.rm = T),
         orderW3S1 = rowMeans(select(data, w3bf_8, w3bf_48, w3bf_68, w3bf_78, w3bf_88),na.rm = T),
         orderW4S1 = rowMeans(select(data, w4bf_8, w4bf_48, w4bf_68, w4bf_78, w4bf_88),na.rm = T),
         
         # second self parcel
         orderW1S2 = rowMeans(select(data, w1bf_18, w1bf_28, w1bf_38, w1bf_58, w1bf_98),na.rm = T),
         orderW2S2 = rowMeans(select(data, w2bf_18, w2bf_28, w2bf_38, w2bf_58, w2bf_98),na.rm = T),
         orderW3S2 = rowMeans(select(data, w3bf_18, w3bf_28, w3bf_38, w3bf_58, w3bf_98),na.rm = T),
         orderW4S2 = rowMeans(select(data, w4bf_18, w4bf_28, w4bf_38, w4bf_58, w4bf_98),na.rm = T),
         
         # first peer parcel
         orderW1P1 = rowMeans(select(data, pw1bf_18, pw1bf_28, pw1bf_48, pw1bf_58, pw1bf_68), na.rm = T),
         orderW2P1 = rowMeans(select(data, pw2bf_18, pw2bf_28, pw2bf_48, pw2bf_58, pw2bf_68), na.rm = T),
         orderW3P1 = rowMeans(select(data, pw3bf_18, pw3bf_28, pw3bf_48, pw3bf_58, pw3bf_68), na.rm = T),
         orderW4P1 = rowMeans(select(data, pw4bf_18, pw4bf_28, pw4bf_48, pw4bf_58, pw4bf_68), na.rm = T),
         
         # second peer parcel
         orderW1P2 = rowMeans(select(data, pw1bf_8, pw1bf_38, pw1bf_78, pw1bf_88, pw1bf_98), na.rm = T),
         orderW2P2 = rowMeans(select(data, pw2bf_8, pw2bf_38, pw2bf_78, pw2bf_88, pw2bf_98), na.rm = T),
         orderW3P2 = rowMeans(select(data, pw3bf_8, pw3bf_38, pw3bf_78, pw3bf_88, pw3bf_98), na.rm = T),
         orderW4P2 = rowMeans(select(data, pw4bf_8, pw4bf_38, pw4bf_78, pw4bf_88, pw4bf_98), na.rm = T))

# politeness
data <- data %>% 
  mutate(# self
         politW1S = rowMeans(select(data, w1bf_7, w1bf_17, w1bf_27, w1bf_37, w1bf_47, w1bf_57,
                                     w1bf_67, w1bf_77, w1bf_87, w1bf_97), na.rm = T),
         politW2S = rowMeans(select(data, w2bf_7, w2bf_17, w2bf_27, w2bf_37, w2bf_47, w2bf_57,
                                     w2bf_67, w2bf_77, w2bf_87, w2bf_97), na.rm = T),
         politW3S = rowMeans(select(data, w3bf_7, w3bf_17, w3bf_27, w3bf_37, w3bf_47, w3bf_57,
                                     w3bf_67, w3bf_77, w3bf_87, w3bf_97), na.rm = T),
         politW4S = rowMeans(select(data, w4bf_7, w4bf_17, w4bf_27, w4bf_37, w4bf_47, w4bf_57,
                                     w4bf_67, w4bf_77, w4bf_87, w4bf_97), na.rm = T),
         
         # peer
         politW1P = rowMeans(select(data, pw1bf_7, pw1bf_17, pw1bf_27, pw1bf_37, pw1bf_47, pw1bf_57,
                                     pw1bf_67, pw1bf_77, pw1bf_87, pw1bf_97), na.rm = T),
         politW2P = rowMeans(select(data, pw2bf_7, pw2bf_17, pw2bf_27, pw2bf_37, pw2bf_47, pw2bf_57,
                                     pw2bf_67, pw2bf_77, pw2bf_87, pw2bf_97), na.rm = T),
         politW3P = rowMeans(select(data, pw3bf_7, pw3bf_17, pw3bf_27, pw3bf_37, pw3bf_47, pw3bf_57,
                                     pw3bf_67, pw3bf_77, pw3bf_87, pw3bf_97), na.rm = T),
         politW4P = rowMeans(select(data, pw4bf_7, pw4bf_17, pw4bf_27, pw4bf_37, pw4bf_47, pw4bf_57,
                                     pw4bf_67, pw4bf_77, pw4bf_87, pw4bf_97), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         politW1S1 = rowMeans(select(data, w1bf_7, w1bf_47, w1bf_67, w1bf_77, w1bf_87),na.rm = T),
         politW2S1 = rowMeans(select(data, w2bf_7, w2bf_47, w2bf_67, w2bf_77, w2bf_87),na.rm = T),
         politW3S1 = rowMeans(select(data, w3bf_7, w3bf_47, w3bf_67, w3bf_77, w3bf_87),na.rm = T),
         politW4S1 = rowMeans(select(data, w4bf_7, w4bf_47, w4bf_67, w4bf_77, w4bf_87),na.rm = T),
         
         # second self parcel
         politW1S2 = rowMeans(select(data, w1bf_17, w1bf_27, w1bf_37, w1bf_57, w1bf_97),na.rm = T),
         politW2S2 = rowMeans(select(data, w2bf_17, w2bf_27, w2bf_37, w2bf_57, w2bf_97),na.rm = T),
         politW3S2 = rowMeans(select(data, w3bf_17, w3bf_27, w3bf_37, w3bf_57, w3bf_97),na.rm = T),
         politW4S2 = rowMeans(select(data, w4bf_17, w4bf_27, w4bf_37, w4bf_57, w4bf_97),na.rm = T),
         
         # first peer parcel
         politW1P1 = rowMeans(select(data, pw1bf_17, pw1bf_27, pw1bf_47, pw1bf_57, pw1bf_67), na.rm = T),
         politW2P1 = rowMeans(select(data, pw2bf_17, pw2bf_27, pw2bf_47, pw2bf_57, pw2bf_67), na.rm = T),
         politW3P1 = rowMeans(select(data, pw3bf_17, pw3bf_27, pw3bf_47, pw3bf_57, pw3bf_67), na.rm = T),
         politW4P1 = rowMeans(select(data, pw4bf_17, pw4bf_27, pw4bf_47, pw4bf_57, pw4bf_67), na.rm = T),
         
         # second peer parcel
         politW1P2 = rowMeans(select(data, pw1bf_7, pw1bf_37, pw1bf_77, pw1bf_87, pw1bf_97), na.rm = T),
         politW2P2 = rowMeans(select(data, pw2bf_7, pw2bf_37, pw2bf_77, pw2bf_87, pw2bf_97), na.rm = T),
         politW3P2 = rowMeans(select(data, pw3bf_7, pw3bf_37, pw3bf_77, pw3bf_87, pw3bf_97), na.rm = T),
         politW4P2 = rowMeans(select(data, pw4bf_7, pw4bf_37, pw4bf_77, pw4bf_87, pw4bf_97), na.rm = T))

# volatility
data <- data %>% 
  mutate(# self
         volatW1S = rowMeans(select(data, w1bf_6, w1bf_16, w1bf_26, w1bf_36, w1bf_46, w1bf_56,
                                     w1bf_66, w1bf_76, w1bf_86, w1bf_96), na.rm = T),
         volatW2S = rowMeans(select(data, w2bf_6, w2bf_16, w2bf_26, w2bf_36, w2bf_46, w2bf_56,
                                     w2bf_66, w2bf_76, w2bf_86, w2bf_96), na.rm = T),
         volatW3S = rowMeans(select(data, w3bf_6, w3bf_16, w3bf_26, w3bf_36, w3bf_46, w3bf_56,
                                     w3bf_66, w3bf_76, w3bf_86, w3bf_96), na.rm = T),
         volatW4S = rowMeans(select(data, w4bf_6, w4bf_16, w4bf_26, w4bf_36, w4bf_46, w4bf_56,
                                     w4bf_66, w4bf_76, w4bf_86, w4bf_96), na.rm = T),
         
         # peer
         volatW1P = rowMeans(select(data, pw1bf_6, pw1bf_16, pw1bf_26, pw1bf_36, pw1bf_46, pw1bf_56,
                                     pw1bf_66, pw1bf_76, pw1bf_86, pw1bf_96), na.rm = T),
         volatW2P = rowMeans(select(data, pw2bf_6, pw2bf_16, pw2bf_26, pw2bf_36, pw2bf_46, pw2bf_56,
                                     pw2bf_66, pw2bf_76, pw2bf_86, pw2bf_96), na.rm = T),
         volatW3P = rowMeans(select(data, pw3bf_6, pw3bf_16, pw3bf_26, pw3bf_36, pw3bf_46, pw3bf_56,
                                     pw3bf_66, pw3bf_76, pw3bf_86, pw3bf_96), na.rm = T),
         volatW4P = rowMeans(select(data, pw4bf_6, pw4bf_16, pw4bf_26, pw4bf_36, pw4bf_46, pw4bf_56,
                                     pw4bf_66, pw4bf_76, pw4bf_86, pw4bf_96), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         volatW1S1 = rowMeans(select(data, w1bf_6, w1bf_46, w1bf_66, w1bf_76, w1bf_86),na.rm = T),
         volatW2S1 = rowMeans(select(data, w2bf_6, w2bf_46, w2bf_66, w2bf_76, w2bf_86),na.rm = T),
         volatW3S1 = rowMeans(select(data, w3bf_6, w3bf_46, w3bf_66, w3bf_76, w3bf_86),na.rm = T),
         volatW4S1 = rowMeans(select(data, w4bf_6, w4bf_46, w4bf_66, w4bf_76, w4bf_86),na.rm = T),
         
         # second self parcel
         volatW1S2 = rowMeans(select(data, w1bf_16, w1bf_26, w1bf_36, w1bf_56, w1bf_96),na.rm = T),
         volatW2S2 = rowMeans(select(data, w2bf_16, w2bf_26, w2bf_36, w2bf_56, w2bf_96),na.rm = T),
         volatW3S2 = rowMeans(select(data, w3bf_16, w3bf_26, w3bf_36, w3bf_56, w3bf_96),na.rm = T),
         volatW4S2 = rowMeans(select(data, w4bf_16, w4bf_26, w4bf_36, w4bf_56, w4bf_96),na.rm = T),
         
         # first peer parcel
         volatW1P1 = rowMeans(select(data, pw1bf_16, pw1bf_26, pw1bf_46, pw1bf_56, pw1bf_66), na.rm = T),
         volatW2P1 = rowMeans(select(data, pw2bf_16, pw2bf_26, pw2bf_46, pw2bf_56, pw2bf_66), na.rm = T),
         volatW3P1 = rowMeans(select(data, pw3bf_16, pw3bf_26, pw3bf_46, pw3bf_56, pw3bf_66), na.rm = T),
         volatW4P1 = rowMeans(select(data, pw4bf_16, pw4bf_26, pw4bf_46, pw4bf_56, pw4bf_66), na.rm = T),
         
         # second peer parcel
         volatW1P2 = rowMeans(select(data, pw1bf_6, pw1bf_36, pw1bf_76, pw1bf_86, pw1bf_96), na.rm = T),
         volatW2P2 = rowMeans(select(data, pw2bf_6, pw2bf_36, pw2bf_76, pw2bf_86, pw2bf_96), na.rm = T),
         volatW3P2 = rowMeans(select(data, pw3bf_6, pw3bf_36, pw3bf_76, pw3bf_86, pw3bf_96), na.rm = T),
         volatW4P2 = rowMeans(select(data, pw4bf_6, pw4bf_36, pw4bf_76, pw4bf_86, pw4bf_96), na.rm = T))

# withdrawal
data <- data %>% 
  mutate(# self
         withdW1S = rowMeans(select(data, w1bf_1, w1bf_11, w1bf_21, w1bf_31, w1bf_41, w1bf_51,
                                     w1bf_61, w1bf_71, w1bf_81, w1bf_91), na.rm = T),
         withdW2S = rowMeans(select(data, w2bf_1, w2bf_11, w2bf_21, w2bf_31, w2bf_41, w2bf_51,
                                     w2bf_61, w2bf_71, w2bf_81, w2bf_91), na.rm = T),
         withdW3S = rowMeans(select(data, w3bf_1, w3bf_11, w3bf_21, w3bf_31, w3bf_41, w3bf_51,
                                     w3bf_61, w3bf_71, w3bf_81, w3bf_91), na.rm = T),
         withdW4S = rowMeans(select(data, w4bf_1, w4bf_11, w4bf_21, w4bf_31, w4bf_41, w4bf_51,
                                     w4bf_61, w4bf_71, w4bf_81, w4bf_91), na.rm = T),
         
         # peer
         withdW1P = rowMeans(select(data, pw1bf_1, pw1bf_11, pw1bf_21, pw1bf_31, pw1bf_41, pw1bf_51,
                                     pw1bf_61, pw1bf_71, pw1bf_81, pw1bf_91), na.rm = T),
         withdW2P = rowMeans(select(data, pw2bf_1, pw2bf_11, pw2bf_21, pw2bf_31, pw2bf_41, pw2bf_51,
                                     pw2bf_61, pw2bf_71, pw2bf_81, pw2bf_91), na.rm = T),
         withdW3P = rowMeans(select(data, pw3bf_1, pw3bf_11, pw3bf_21, pw3bf_31, pw3bf_41, pw3bf_51,
                                     pw3bf_61, pw3bf_71, pw3bf_81, pw3bf_91), na.rm = T),
         withdW4P = rowMeans(select(data, pw4bf_1, pw4bf_11, pw4bf_21, pw4bf_31, pw4bf_41, pw4bf_51,
                                     pw4bf_61, pw4bf_71, pw4bf_81, pw4bf_91), na.rm = T))

data <- data %>% 
  mutate(# first self parcel
         withdW1S1 = rowMeans(select(data, w1bf_1, w1bf_41, w1bf_61, w1bf_71, w1bf_81),na.rm = T),
         withdW2S1 = rowMeans(select(data, w2bf_1, w2bf_41, w2bf_61, w2bf_71, w2bf_81),na.rm = T),
         withdW3S1 = rowMeans(select(data, w3bf_1, w3bf_41, w3bf_61, w3bf_71, w3bf_81),na.rm = T),
         withdW4S1 = rowMeans(select(data, w4bf_1, w4bf_41, w4bf_61, w4bf_71, w4bf_81),na.rm = T),
         
         # second self parcel
         withdW1S2 = rowMeans(select(data, w1bf_11, w1bf_21, w1bf_31, w1bf_51, w1bf_91),na.rm = T),
         withdW2S2 = rowMeans(select(data, w2bf_11, w2bf_21, w2bf_31, w2bf_51, w2bf_91),na.rm = T),
         withdW3S2 = rowMeans(select(data, w3bf_11, w3bf_21, w3bf_31, w3bf_51, w3bf_91),na.rm = T),
         withdW4S2 = rowMeans(select(data, w4bf_11, w4bf_21, w4bf_31, w4bf_51, w4bf_91),na.rm = T),
         
         # first peer parcel
         withdW1P1 = rowMeans(select(data, pw1bf_11, pw1bf_21, pw1bf_41, pw1bf_51, pw1bf_61), na.rm = T),
         withdW2P1 = rowMeans(select(data, pw2bf_11, pw2bf_21, pw2bf_41, pw2bf_51, pw2bf_61), na.rm = T),
         withdW3P1 = rowMeans(select(data, pw3bf_11, pw3bf_21, pw3bf_41, pw3bf_51, pw3bf_61), na.rm = T),
         withdW4P1 = rowMeans(select(data, pw4bf_11, pw4bf_21, pw4bf_41, pw4bf_51, pw4bf_61), na.rm = T),
         
         # second peer parcel
         withdW1P2 = rowMeans(select(data, pw1bf_1, pw1bf_31, pw1bf_71, pw1bf_81, pw1bf_91), na.rm = T),
         withdW2P2 = rowMeans(select(data, pw2bf_1, pw2bf_31, pw2bf_71, pw2bf_81, pw2bf_91), na.rm = T),
         withdW3P2 = rowMeans(select(data, pw3bf_1, pw3bf_31, pw3bf_71, pw3bf_81, pw3bf_91), na.rm = T),
         withdW4P2 = rowMeans(select(data, pw4bf_1, pw4bf_31, pw4bf_71, pw4bf_81, pw4bf_91), na.rm = T))

# >>> Domains ----

### agreeableness
data <- data %>% 
  mutate(# first self parcel
         agreeW1S1 = rowMeans(select(data, w1bf_42, w1bf_52, w1bf_92, w1bf_17, w1bf_27, 
                                     w1bf_37, w1bf_57, w1bf_67, w1bf_87, w1bf_97),na.rm = T),
         agreeW2S1 = rowMeans(select(data, w2bf_42, w2bf_52, w2bf_92, w2bf_17, w2bf_27, 
                                     w2bf_37, w2bf_57, w2bf_67, w2bf_87, w2bf_97),na.rm = T),
         agreeW3S1 = rowMeans(select(data, w3bf_42, w3bf_52, w3bf_92, w3bf_17, w3bf_27, 
                                     w3bf_37, w3bf_57, w3bf_67, w3bf_87, w3bf_97),na.rm = T),
         agreeW4S1 = rowMeans(select(data, w4bf_42, w4bf_52, w4bf_92, w4bf_17, w4bf_27, 
                                     w4bf_37, w4bf_57, w4bf_67, w4bf_87, w4bf_97),na.rm = T),
         
         # second self parcel
         agreeW1S2 = rowMeans(select(data, w1bf_2, w1bf_12, w1bf_22, w1bf_32, w1bf_62, 
                                     w1bf_72, w1bf_82, w1bf_7, w1bf_47, w1bf_77),na.rm = T),
         agreeW2S2 = rowMeans(select(data, w2bf_2, w2bf_12, w2bf_22, w2bf_32, w2bf_62, 
                                     w2bf_72, w2bf_82, w2bf_7, w2bf_47, w2bf_77),na.rm = T),
         agreeW3S2 = rowMeans(select(data, w3bf_2, w3bf_12, w3bf_22, w3bf_32, w3bf_62, 
                                     w3bf_72, w3bf_82, w3bf_7, w3bf_47, w3bf_77),na.rm = T),
         agreeW4S2 = rowMeans(select(data, w4bf_2, w4bf_12, w4bf_22, w4bf_32, w4bf_62, 
                                     w4bf_72, w4bf_82, w4bf_7, w4bf_47, w4bf_77),na.rm = T),
         
         # first peer parcel
         agreeW1P1 = rowMeans(select(data, pw1bf_12, pw1bf_22, pw1bf_32, pw1bf_62, pw1bf_72, 
                                     pw1bf_92, pw1bf_17, pw1bf_37, pw1bf_57, pw1bf_77), na.rm = T),
         agreeW2P1 = rowMeans(select(data, pw2bf_12, pw2bf_22, pw2bf_32, pw2bf_62, pw2bf_72, 
                                     pw2bf_92, pw2bf_17, pw2bf_37, pw2bf_57, pw2bf_77), na.rm = T),
         agreeW3P1 = rowMeans(select(data, pw3bf_12, pw3bf_22, pw3bf_32, pw3bf_62, pw3bf_72, 
                                     pw3bf_92, pw3bf_17, pw3bf_37, pw3bf_57, pw3bf_77), na.rm = T),
         agreeW4P1 = rowMeans(select(data, pw4bf_12, pw4bf_22, pw4bf_32, pw4bf_62, pw4bf_72, 
                                     pw4bf_92, pw4bf_17, pw4bf_37, pw4bf_57, pw4bf_77), na.rm = T),
         
         # second peer parcel
         agreeW1P2 = rowMeans(select(data, pw1bf_2, pw1bf_42, pw1bf_52, pw1bf_82, pw1bf_7, 
                                     pw1bf_27, pw1bf_47, pw1bf_67, pw1bf_87, pw1bf_97), na.rm = T),
         agreeW2P2 = rowMeans(select(data, pw1bf_2, pw1bf_42, pw1bf_52, pw1bf_82, pw1bf_7, 
                                     pw1bf_27, pw1bf_47, pw1bf_67, pw1bf_87, pw1bf_97), na.rm = T),
         agreeW3P2 = rowMeans(select(data, pw1bf_2, pw1bf_42, pw1bf_52, pw1bf_82, pw1bf_7, 
                                     pw1bf_27, pw1bf_47, pw1bf_67, pw1bf_87, pw1bf_97), na.rm = T),
         agreeW4P2 = rowMeans(select(data, pw1bf_2, pw1bf_42, pw1bf_52, pw1bf_82, pw1bf_7, 
                                     pw1bf_27, pw1bf_47, pw1bf_67, pw1bf_87, pw1bf_97), na.rm = T))

### conscientiousness
data <- data %>% 
  mutate(# first self parcel
         consciW1S1 = rowMeans(select(data, w1bf_43, w1bf_53, w1bf_93, w1bf_18, w1bf_28, 
                                      w1bf_38, w1bf_58, w1bf_68, w1bf_88, w1bf_98),na.rm = T),
         consciW2S1 = rowMeans(select(data, w2bf_43, w2bf_53, w2bf_93, w2bf_18, w2bf_28, 
                                      w2bf_38, w2bf_58, w2bf_68, w2bf_88, w2bf_98),na.rm = T),
         consciW3S1 = rowMeans(select(data, w3bf_43, w3bf_53, w3bf_93, w3bf_18, w3bf_28, 
                                      w3bf_38, w3bf_58, w3bf_68, w3bf_88, w3bf_98),na.rm = T),
         consciW4S1 = rowMeans(select(data, w4bf_43, w4bf_53, w4bf_93, w4bf_18, w4bf_28, 
                                      w4bf_38, w4bf_58, w4bf_68, w4bf_88, w4bf_98),na.rm = T),
         
         # second self parcel
         consciW1S2 = rowMeans(select(data, w1bf_3, w1bf_13, w1bf_23, w1bf_33, w1bf_63, 
                                      w1bf_73, w1bf_83, w1bf_8, w1bf_48, w1bf_78),na.rm = T),
         consciW2S2 = rowMeans(select(data, w2bf_3, w2bf_13, w2bf_23, w2bf_33, w2bf_63, 
                                      w2bf_73, w2bf_83, w2bf_8, w2bf_48, w2bf_78),na.rm = T),
         consciW3S2 = rowMeans(select(data, w3bf_3, w3bf_13, w3bf_23, w3bf_33, w3bf_63, 
                                      w3bf_73, w3bf_83, w3bf_8, w3bf_48, w3bf_78),na.rm = T),
         consciW4S2 = rowMeans(select(data, w4bf_3, w4bf_13, w4bf_23, w4bf_33, w4bf_63, 
                                      w4bf_73, w4bf_83, w4bf_8, w4bf_48, w4bf_78),na.rm = T),
         
         # first peer parcel
         consciW1P1 = rowMeans(select(data, pw1bf_13, pw1bf_23, pw1bf_33, pw1bf_63, pw1bf_73, 
                                      pw1bf_93, pw1bf_18, pw1bf_38, pw1bf_58, pw1bf_78), na.rm = T),
         consciW2P1 = rowMeans(select(data, pw2bf_13, pw2bf_23, pw2bf_33, pw2bf_63, pw2bf_73, 
                                      pw2bf_93, pw2bf_18, pw2bf_38, pw2bf_58, pw2bf_78), na.rm = T),
         consciW3P1 = rowMeans(select(data, pw3bf_13, pw3bf_23, pw3bf_33, pw3bf_63, pw3bf_73, 
                                      pw3bf_93, pw3bf_18, pw3bf_38, pw3bf_58, pw3bf_78), na.rm = T),
         consciW4P1 = rowMeans(select(data, pw4bf_13, pw4bf_23, pw4bf_33, pw4bf_63, pw4bf_73, 
                                      pw4bf_93, pw4bf_18, pw4bf_38, pw4bf_58, pw4bf_78), na.rm = T),
         
         # second peer parcel
         consciW1P2 = rowMeans(select(data, pw1bf_3, pw1bf_43, pw1bf_53, pw1bf_83, pw1bf_8, 
                                      pw1bf_28, pw1bf_48, pw1bf_68, pw1bf_88, pw1bf_98), na.rm = T),
         consciW2P2 = rowMeans(select(data, pw2bf_3, pw2bf_43, pw2bf_53, pw2bf_83, pw2bf_8, 
                                      pw2bf_28, pw2bf_48, pw2bf_68, pw2bf_88, pw2bf_98), na.rm = T),
         consciW3P2 = rowMeans(select(data, pw3bf_3, pw3bf_43, pw3bf_53, pw3bf_83, pw3bf_8, 
                                      pw3bf_28, pw3bf_48, pw3bf_68, pw3bf_88, pw3bf_98), na.rm = T),
         consciW4P2 = rowMeans(select(data, pw4bf_3, pw4bf_43, pw4bf_53, pw4bf_83, pw4bf_8, 
                                      pw4bf_28, pw4bf_48, pw4bf_68, pw4bf_88, pw4bf_98), na.rm = T))

### extraversion
data <- data %>% 
  mutate(# first self parcel
         extraW1S1 = rowMeans(select(data, w1bf_44, w1bf_54, w1bf_94, w1bf_19, w1bf_29, 
                                     w1bf_39, w1bf_59, w1bf_69, w1bf_89, w1bf_99),na.rm = T),
         extraW2S1 = rowMeans(select(data, w2bf_44, w2bf_54, w2bf_94, w2bf_19, w2bf_29, 
                                     w2bf_39, w2bf_59, w2bf_69, w2bf_89, w2bf_99),na.rm = T),
         extraW3S1 = rowMeans(select(data, w3bf_44, w3bf_54, w3bf_94, w3bf_19, w3bf_29, 
                                     w3bf_39, w3bf_59, w3bf_69, w3bf_89, w3bf_99),na.rm = T),
         extraW4S1 = rowMeans(select(data, w4bf_44, w4bf_54, w4bf_94, w4bf_19, w4bf_29, 
                                     w4bf_39, w4bf_59, w4bf_69, w4bf_89, w4bf_99),na.rm = T),
         
         # second self parcel
         extraW1S2 = rowMeans(select(data, w1bf_4, w1bf_14, w1bf_24, w1bf_34, w1bf_64, 
                                     w1bf_74, w1bf_84, w1bf_9, w1bf_49, w1bf_79),na.rm = T),
         extraW2S2 = rowMeans(select(data, w2bf_4, w2bf_14, w2bf_24, w2bf_34, w2bf_64, 
                                     w2bf_74, w2bf_84, w2bf_9, w2bf_49, w2bf_79),na.rm = T),
         extraW3S2 = rowMeans(select(data, w3bf_4, w3bf_14, w3bf_24, w3bf_34, w3bf_64, 
                                     w3bf_74, w3bf_84, w3bf_9, w3bf_49, w3bf_79),na.rm = T),
         extraW4S2 = rowMeans(select(data, w4bf_4, w4bf_14, w4bf_24, w4bf_34, w4bf_64, 
                                     w4bf_74, w4bf_84, w4bf_9, w4bf_49, w4bf_79),na.rm = T),
         
         # first peer parcel
         extraW1P1 = rowMeans(select(data, pw1bf_14, pw1bf_24, pw1bf_34, pw1bf_64, pw1bf_74, 
                                     pw1bf_94, pw1bf_19, pw1bf_39, pw1bf_59, pw1bf_79), na.rm = T),
         extraW2P1 = rowMeans(select(data, pw2bf_14, pw2bf_24, pw2bf_34, pw2bf_64, pw2bf_74, 
                                     pw2bf_94, pw2bf_19, pw2bf_39, pw2bf_59, pw2bf_79), na.rm = T),
         extraW3P1 = rowMeans(select(data, pw3bf_14, pw3bf_24, pw3bf_34, pw3bf_64, pw3bf_74, 
                                     pw3bf_94, pw3bf_19, pw3bf_39, pw3bf_59, pw3bf_79), na.rm = T),
         extraW4P1 = rowMeans(select(data, pw4bf_14, pw4bf_24, pw4bf_34, pw4bf_64, pw4bf_74, 
                                     pw4bf_94, pw4bf_19, pw4bf_39, pw4bf_59, pw4bf_79), na.rm = T),
         
         # second peer parcel
         extraW1P2 = rowMeans(select(data, pw1bf_4, pw1bf_44, pw1bf_54, pw1bf_84, pw1bf_9, 
                                     pw1bf_29, pw1bf_49, pw1bf_69, pw1bf_89, pw1bf_99), na.rm = T),
         extraW2P2 = rowMeans(select(data, pw2bf_4, pw2bf_44, pw2bf_54, pw2bf_84, pw2bf_9, 
                                     pw2bf_29, pw2bf_49, pw2bf_69, pw2bf_89, pw2bf_99), na.rm = T),
         extraW3P2 = rowMeans(select(data, pw3bf_4, pw3bf_44, pw3bf_54, pw3bf_84, pw3bf_9, 
                                     pw3bf_29, pw3bf_49, pw3bf_69, pw3bf_89, pw3bf_99), na.rm = T),
         extraW4P2 = rowMeans(select(data, pw4bf_4, pw4bf_44, pw4bf_54, pw4bf_84, pw4bf_9, 
                                     pw4bf_29, pw4bf_49, pw4bf_69, pw4bf_89, pw4bf_99), na.rm = T))

### neuroticism
data <- data %>% 
  mutate(# first self parcel
         neuroW1S1 = rowMeans(select(data, w1bf_41, w1bf_51, w1bf_91, w1bf_16, w1bf_26, 
                                     w1bf_36, w1bf_56, w1bf_66, w1bf_86, w1bf_96),na.rm = T),
         neuroW2S1 = rowMeans(select(data, w2bf_41, w2bf_51, w2bf_91, w2bf_16, w2bf_26, 
                                     w2bf_36, w2bf_56, w2bf_66, w2bf_86, w2bf_96),na.rm = T),
         neuroW3S1 = rowMeans(select(data, w3bf_41, w3bf_51, w3bf_91, w3bf_16, w3bf_26, 
                                     w3bf_36, w3bf_56, w3bf_66, w3bf_86, w3bf_96),na.rm = T),
         neuroW4S1 = rowMeans(select(data, w4bf_41, w4bf_51, w4bf_91, w4bf_16, w4bf_26, 
                                     w4bf_36, w4bf_56, w4bf_66, w4bf_86, w4bf_96),na.rm = T),
         
         # second self parcel
         neuroW1S2 = rowMeans(select(data, w1bf_1, w1bf_11, w1bf_21, w1bf_31, w1bf_61, 
                                     w1bf_71, w1bf_81, w1bf_16, w1bf_46, w1bf_76),na.rm = T),
         neuroW2S2 = rowMeans(select(data, w2bf_1, w2bf_11, w2bf_21, w2bf_31, w2bf_61, 
                                     w2bf_71, w2bf_81, w2bf_16, w2bf_46, w2bf_76),na.rm = T),
         neuroW3S2 = rowMeans(select(data, w3bf_1, w3bf_11, w3bf_21, w3bf_31, w3bf_61, 
                                     w3bf_71, w3bf_81, w3bf_16, w3bf_46, w3bf_76),na.rm = T),
         neuroW4S2 = rowMeans(select(data, w4bf_1, w4bf_11, w4bf_21, w4bf_31, w4bf_61, 
                                     w4bf_71, w4bf_81, w4bf_16, w4bf_46, w4bf_76),na.rm = T),
         
         # first peer parcel
         neuroW1P1 = rowMeans(select(data, pw1bf_11, pw1bf_21, pw1bf_31, pw1bf_61, pw1bf_71, 
                                     pw1bf_91, pw1bf_16, pw1bf_36, pw1bf_56, pw1bf_76), na.rm = T),
         neuroW2P1 = rowMeans(select(data, pw2bf_11, pw2bf_21, pw2bf_31, pw2bf_61, pw2bf_71, 
                                     pw2bf_91, pw2bf_16, pw2bf_36, pw2bf_56, pw2bf_76), na.rm = T),
         neuroW3P1 = rowMeans(select(data, pw3bf_11, pw3bf_21, pw3bf_31, pw3bf_61, pw3bf_71, 
                                     pw3bf_91, pw3bf_16, pw3bf_36, pw3bf_56, pw3bf_76), na.rm = T),
         neuroW4P1 = rowMeans(select(data, pw4bf_11, pw4bf_21, pw4bf_31, pw4bf_61, pw4bf_71, 
                                     pw4bf_91, pw4bf_16, pw4bf_36, pw4bf_56, pw4bf_76), na.rm = T),
         
         # second peer parcel
         neuroW1P2 = rowMeans(select(data, pw1bf_1, pw1bf_41, pw1bf_51, pw1bf_81, pw1bf_6, 
                                     pw1bf_26, pw1bf_46, pw1bf_66, pw1bf_86, pw1bf_96), na.rm = T),
         neuroW2P2 = rowMeans(select(data, pw2bf_1, pw2bf_41, pw2bf_51, pw2bf_81, pw2bf_6, 
                                     pw2bf_26, pw2bf_46, pw2bf_66, pw2bf_86, pw2bf_96), na.rm = T),
         neuroW3P2 = rowMeans(select(data, pw3bf_1, pw3bf_41, pw3bf_51, pw3bf_81, pw3bf_6, 
                                     pw3bf_26, pw3bf_46, pw3bf_66, pw3bf_86, pw3bf_96), na.rm = T),
         neuroW4P2 = rowMeans(select(data, pw4bf_1, pw4bf_41, pw4bf_51, pw4bf_81, pw4bf_6, 
                                     pw4bf_26, pw4bf_46, pw4bf_66, pw4bf_86, pw4bf_96), na.rm = T))

### openness domain
data <- data %>% 
  mutate(# first self parcel
         opendW1S1 = rowMeans(select(data, w1bf_40, w1bf_50, w1bf_90, w1bf_15, w1bf_25, 
                                     w1bf_35, w1bf_55, w1bf_65, w1bf_85, w1bf_95),na.rm = T),
         opendW2S1 = rowMeans(select(data, w2bf_40, w2bf_50, w2bf_90, w2bf_15, w2bf_25, 
                                     w2bf_35, w2bf_55, w2bf_65, w2bf_85, w2bf_95),na.rm = T),
         opendW3S1 = rowMeans(select(data, w3bf_40, w3bf_50, w3bf_90, w3bf_15, w3bf_25, 
                                     w3bf_35, w3bf_55, w3bf_65, w3bf_85, w3bf_95),na.rm = T),
         opendW4S1 = rowMeans(select(data, w4bf_40, w4bf_50, w4bf_90, w4bf_15, w4bf_25, 
                                     w4bf_35, w4bf_55, w4bf_65, w4bf_85, w4bf_95),na.rm = T),
         
         # second self parcel
         opendW1S2 = rowMeans(select(data, w1bf_100, w1bf_10, w1bf_20, w1bf_30, w1bf_60, 
                                     w1bf_70, w1bf_80, w1bf_5, w1bf_45, w1bf_75),na.rm = T),
         opendW2S2 = rowMeans(select(data, w2bf_100, w2bf_10, w2bf_20, w2bf_30, w2bf_60, 
                                     w2bf_70, w2bf_80, w2bf_5, w2bf_45, w2bf_75),na.rm = T),
         opendW3S2 = rowMeans(select(data, w3bf_100, w3bf_10, w3bf_20, w3bf_30, w3bf_60, 
                                     w3bf_70, w3bf_80, w3bf_5, w3bf_45, w3bf_75),na.rm = T),
         opendW4S2 = rowMeans(select(data, w4bf_100, w4bf_10, w4bf_20, w4bf_30, w4bf_60, 
                                     w4bf_70, w4bf_80, w4bf_5, w4bf_45, w4bf_75),na.rm = T),
         
         # first peer parcel
         opendW1P1 = rowMeans(select(data, pw1bf_10, pw1bf_20, pw1bf_30, pw1bf_60, pw1bf_70, 
                                     pw1bf_90, pw1bf_15, pw1bf_35, pw1bf_55, pw1bf_75), na.rm = T),
         opendW2P1 = rowMeans(select(data, pw2bf_10, pw2bf_20, pw2bf_30, pw2bf_60, pw2bf_70, 
                                     pw2bf_90, pw2bf_15, pw2bf_35, pw2bf_55, pw2bf_75), na.rm = T),
         opendW3P1 = rowMeans(select(data, pw3bf_10, pw3bf_20, pw3bf_30, pw3bf_60, pw3bf_70, 
                                     pw3bf_90, pw3bf_15, pw3bf_35, pw3bf_55, pw3bf_75), na.rm = T),
         opendW4P1 = rowMeans(select(data, pw4bf_10, pw4bf_20, pw4bf_30, pw4bf_60, pw4bf_70, 
                                     pw4bf_90, pw4bf_15, pw4bf_35, pw4bf_55, pw4bf_75), na.rm = T),
         
         # second peer parcel
         opendW1P2 = rowMeans(select(data, pw1bf_100, pw1bf_40, pw1bf_50, pw1bf_80, pw1bf_5, 
                                     pw1bf_25, pw1bf_45, pw1bf_65, pw1bf_85, pw1bf_95), na.rm = T),
         opendW2P2 = rowMeans(select(data, pw2bf_100, pw2bf_40, pw2bf_50, pw2bf_80, pw2bf_5, 
                                     pw2bf_25, pw2bf_45, pw2bf_65, pw2bf_85, pw2bf_95), na.rm = T),
         opendW3P2 = rowMeans(select(data, pw3bf_100, pw3bf_40, pw3bf_50, pw3bf_80, pw3bf_5, 
                                     pw3bf_25, pw3bf_45, pw3bf_65, pw3bf_85, pw3bf_95), na.rm = T),
         opendW4P2 = rowMeans(select(data, pw4bf_100, pw4bf_40, pw4bf_50, pw4bf_80, pw4bf_5, 
                                     pw4bf_25, pw4bf_45, pw4bf_65, pw4bf_85, pw4bf_95), na.rm = T))

data[data == "NaN"] <- NA

# > Latent growth model ====

# >> LGM Agreeableness ----

### with aspects as parcels
lgmAgree <- '

# factor at each time point with same loading
agree1 =~ compaW1S        + a * politW1S + 
          peer * compaW1P + aa * politW1P

agree2 =~ compaW2S        + a * politW2S + 
          peer * compaW2P + aa * politW2P

agree3 =~ compaW3S        + a * politW3S + 
          peer * compaW3P + aa * politW3P
  
agree4 =~ compaW4S        + a * politW4S + 
          peer * compaW4P + aa * politW4P

# second order factor for intercept and slope
interc =~ 1*agree1 + 1*agree2 + 1*agree3 + 1*agree4
slope =~ 0*agree1 + 6*agree2 + 13*agree3 + 19*agree4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
compaW1S ~ 0*1
compaW2S ~ 0*1
compaW3S ~ 0*1
compaW4S ~ 0*1

# fix equal intercepts
politW1S ~ b*1
politW2S ~ b*1
politW3S ~ b*1
politW4S ~ b*1

compaW1P ~ c*1
compaW2P ~ c*1
compaW3P ~ c*1
compaW4P ~ c*1

politW1P ~ d*1
politW2P ~ d*1
politW3P ~ d*1
politW4P ~ d*1

# error covariance - similar aspects across waves and informants
compaW1S ~~ compaW2S + compaW3S + compaW4S +
            compaW1P + compaW2P + compaW3P + compaW4P
compaW2S ~~ compaW3S + compaW4S +
            compaW1P + compaW2P + compaW3P + compaW4P
compaW3S ~~ compaW4S +
            compaW1P + compaW2P + compaW3P + compaW4P
compaW4S ~~ compaW1P + compaW2P + compaW3P + compaW4P

politW1S ~~ politW2S + politW3S + politW4S +
            politW1P + politW2P + politW3P + politW4P
politW2S ~~ politW3S + politW4S +
            politW1P + politW2P + politW3P + politW4P
politW3S ~~ politW4S +
            politW1P + politW2P + politW3P + politW4P
politW4S ~~ politW1P + politW2P + politW3P + politW4P

compaW1P ~~ compaW2P + compaW3P + compaW4P
compaW2P ~~ compaW3P + compaW4P
compaW3P ~~ compaW4P

politW1P ~~ politW2P + politW3P + politW4P
politW2P ~~ politW3P + politW4P
politW3P ~~ politW4P
'
lgmAgree <- sem(lgmAgree, data = data, missing = "ML")
summary(lgmAgree, fit.measures = T, standardized = T)

semPaths(lgmAgree, what = "col", whatLabels = "est", intercepts = T)

### with random parcels
lgmAgree <- '

# factor at each time point with same loading
agree1 =~ agreeW1S1        + a * agreeW1S2 + 
           peer * agreeW1P1 + aa * agreeW1P2

agree2 =~ agreeW2S1        + a * agreeW2S2 + 
           peer * agreeW2P1 + aa * agreeW2P2

agree3 =~ agreeW3S1        + a * agreeW3S2 + 
           peer * agreeW3P1 + aa * agreeW3P2
  
agree4 =~ agreeW4S1        + a * agreeW4S2 + 
           peer * agreeW4P1 + aa * agreeW4P2

# second order factor for intercept and slope
interc =~ 1*agree1 + 1*agree2 + 1*agree3 + 1*agree4
slope =~ 0*agree1 + 6*agree2 + 13*agree3 + 19*agree4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
agreeW1S1 ~ 0*1
agreeW2S1 ~ 0*1
agreeW3S1 ~ 0*1
agreeW4S1 ~ 0*1

# fix equal intercepts
agreeW1S2 ~ b*1
agreeW2S2 ~ b*1
agreeW3S2 ~ b*1
agreeW4S2 ~ b*1

agreeW1P1 ~ c*1
agreeW2P1 ~ c*1
agreeW3P1 ~ c*1
agreeW4P1 ~ c*1

agreeW1P2 ~ d*1
agreeW2P2 ~ d*1
agreeW3P2 ~ d*1
agreeW4P2 ~ d*1

# error covariance - similar parcels across waves
agreeW1S1 ~~ agreeW2S1 + agreeW3S1 + agreeW4S1
agreeW2S1 ~~ agreeW3S1 + agreeW4S1
agreeW3S1 ~~ agreeW4S1

agreeW1S2 ~~ agreeW2S2 + agreeW3S2 + agreeW4S2
agreeW2S2 ~~ agreeW3S2 + agreeW4S2
agreeW3S2 ~~ agreeW4S2

agreeW1P1 ~~ agreeW2P1 + agreeW3P1 + agreeW4P1
agreeW2P1 ~~ agreeW3P1 + agreeW4P1
agreeW3P1 ~~ agreeW4P1

agreeW1P2 ~~ agreeW2P2 + agreeW3P2 + agreeW4P2
agreeW2P2 ~~ agreeW3P2 + agreeW4P2
agreeW3P2 ~~ agreeW4P2
'
lgmAgree <- sem(lgmAgree, data = data, missing = "ML")
summary(lgmAgree, fit.measures = T, standardized = T)

semPaths(lgmAgree, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Conscientiousness ----

### with aspects as parcels
lgmConsci <- '

# factor at each time point with same loading
consci1 =~ indusW1S        + a * orderW1S + 
          peer * indusW1P + aa * orderW1P

consci2 =~ indusW2S        + a * orderW2S + 
          peer * indusW2P + aa * orderW2P

consci3 =~ indusW3S        + a * orderW3S + 
          peer * indusW3P + aa * orderW3P
  
consci4 =~ indusW4S        + a * orderW4S + 
          peer * indusW4P + aa * orderW4P

# second order factor for intercept and slope
interc =~ 1*consci1 + 1*consci2 + 1*consci3 + 1*consci4
slope =~ 0*consci1 + 6*consci2 + 13*consci3 + 19*consci4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
indusW1S ~ 0*1
indusW2S ~ 0*1
indusW3S ~ 0*1
indusW4S ~ 0*1

# fix equal intercepts
orderW1S ~ b*1
orderW2S ~ b*1
orderW3S ~ b*1
orderW4S ~ b*1

indusW1P ~ c*1
indusW2P ~ c*1
indusW3P ~ c*1
indusW4P ~ c*1

orderW1P ~ d*1
orderW2P ~ d*1
orderW3P ~ d*1
orderW4P ~ d*1

# error covariance - similar aspects across waves and informants
indusW1S ~~ indusW2S + indusW3S + indusW4S +
            indusW1P + indusW2P + indusW3P + indusW4P
indusW2S ~~ indusW3S + indusW4S +
            indusW1P + indusW2P + indusW3P + indusW4P
indusW3S ~~ indusW4S +
            indusW1P + indusW2P + indusW3P + indusW4P
indusW4S ~~ indusW1P + indusW2P + indusW3P + indusW4P

orderW1S ~~ orderW2S + orderW3S + orderW4S +
            orderW1P + orderW2P + orderW3P + orderW4P
orderW2S ~~ orderW3S + orderW4S +
            orderW1P + orderW2P + orderW3P + orderW4P
orderW3S ~~ orderW4S +
            orderW1P + orderW2P + orderW3P + orderW4P
orderW4S ~~ orderW1P + orderW2P + orderW3P + orderW4P

indusW1P ~~ indusW2P + indusW3P + indusW4P
indusW2P ~~ indusW3P + indusW4P
indusW3P ~~ indusW4P

orderW1P ~~ orderW2P + orderW3P + orderW4P
orderW2P ~~ orderW3P + orderW4P
orderW3P ~~ orderW4P
'
lgmConsci <- sem(lgmConsci, data = data, missing = "ML")
summary(lgmConsci, fit.measures = T, standardized = T)

semPaths(lgmConsci, what = "col", whatLabels = "est", intercepts = T)

### with random parcels
lgmConsci <- '

# factor at each time point with same loading
consci1 =~ consciW1S1        + a * consciW1S2 + 
           peer * consciW1P1 + aa * consciW1P2

consci2 =~ consciW2S1        + a * consciW2S2 + 
           peer * consciW2P1 + aa * consciW2P2

consci3 =~ consciW3S1        + a * consciW3S2 + 
           peer * consciW3P1 + aa * consciW3P2
  
consci4 =~ consciW4S1        + a * consciW4S2 + 
           peer * consciW4P1 + aa * consciW4P2

# second order factor for intercept and slope
interc =~ 1*consci1 + 1*consci2 + 1*consci3 + 1*consci4
slope =~ 0*consci1 + 6*consci2 + 13*consci3 + 19*consci4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
consciW1S1 ~ 0*1
consciW2S1 ~ 0*1
consciW3S1 ~ 0*1
consciW4S1 ~ 0*1

# fix equal intercepts
consciW1S2 ~ b*1
consciW2S2 ~ b*1
consciW3S2 ~ b*1
consciW4S2 ~ b*1

consciW1P1 ~ c*1
consciW2P1 ~ c*1
consciW3P1 ~ c*1
consciW4P1 ~ c*1

consciW1P2 ~ d*1
consciW2P2 ~ d*1
consciW3P2 ~ d*1
consciW4P2 ~ d*1

# error covariance - similar parcels across waves
consciW1S1 ~~ consciW2S1 + consciW3S1 + consciW4S1
consciW2S1 ~~ consciW3S1 + consciW4S1
consciW3S1 ~~ consciW4S1

consciW1S2 ~~ consciW2S2 + consciW3S2 + consciW4S2
consciW2S2 ~~ consciW3S2 + consciW4S2
consciW3S2 ~~ consciW4S2

consciW1P1 ~~ consciW2P1 + consciW3P1 + consciW4P1
consciW2P1 ~~ consciW3P1 + consciW4P1
consciW3P1 ~~ consciW4P1

consciW1P2 ~~ consciW2P2 + consciW3P2 + consciW4P2
consciW2P2 ~~ consciW3P2 + consciW4P2
consciW3P2 ~~ consciW4P2
'
lgmConsci <- sem(lgmConsci, data = data, missing = "ML")
summary(lgmConsci, fit.measures = T, standardized = T)

semPaths(lgmConsci, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Extraversion ----

### with aspects as parcels
lgmExtra <- '

# factor at each time point with same loading
extra1 =~ assertW1S        + a * enthuW1S + 
          peer * assertW1P + aa * enthuW1P

extra2 =~ assertW2S        + a * enthuW2S + 
          peer * assertW2P + aa * enthuW2P

extra3 =~ assertW3S        + a * enthuW3S + 
          peer * assertW3P + aa * enthuW3P
  
extra4 =~ assertW4S        + a * enthuW4S + 
          peer * assertW4P + aa * enthuW4P

# second order factor for intercept and slope
interc =~ 1*extra1 + 1*extra2 + 1*extra3 + 1*extra4
slope =~ 0*extra1 + 6*extra2 + 13*extra3 + 19*extra4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
assertW1S ~ 0*1
assertW2S ~ 0*1
assertW3S ~ 0*1
assertW4S ~ 0*1

# fix equal intercepts
enthuW1S ~ b*1
enthuW2S ~ b*1
enthuW3S ~ b*1
enthuW4S ~ b*1

assertW1P ~ c*1
assertW2P ~ c*1
assertW3P ~ c*1
assertW4P ~ c*1

enthuW1P ~ d*1
enthuW2P ~ d*1
enthuW3P ~ d*1
enthuW4P ~ d*1

# error covariance - similar aspects across waves and informants
assertW1S ~~ assertW2S + assertW3S + assertW4S +
            assertW1P + assertW2P + assertW3P + assertW4P
assertW2S ~~ assertW3S + assertW4S +
            assertW1P + assertW2P + assertW3P + assertW4P
assertW3S ~~ assertW4S +
            assertW1P + assertW2P + assertW3P + assertW4P
assertW4S ~~ assertW1P + assertW2P + assertW3P + assertW4P

enthuW1S ~~ enthuW2S + enthuW3S + enthuW4S +
            enthuW1P + enthuW2P + enthuW3P + enthuW4P
enthuW2S ~~ enthuW3S + enthuW4S +
            enthuW1P + enthuW2P + enthuW3P + enthuW4P
enthuW3S ~~ enthuW4S +
            enthuW1P + enthuW2P + enthuW3P + enthuW4P
enthuW4S ~~ enthuW1P + enthuW2P + enthuW3P + enthuW4P

assertW1P ~~ assertW2P + assertW3P + assertW4P
assertW2P ~~ assertW3P + assertW4P
assertW3P ~~ assertW4P

enthuW1P ~~ enthuW2P + enthuW3P + enthuW4P
enthuW2P ~~ enthuW3P + enthuW4P
enthuW3P ~~ enthuW4P
'
lgmExtra <- sem(lgmExtra, data = data, missing = "ML")
summary(lgmExtra, fit.measures = T, standardized = T)

semPaths(lgmExtra, what = "col", whatLabels = "est", intercepts = T)

### with random parcels
lgmExtra <- '

# factor at each time point with same loading
extra1 =~ extraW1S1        + a * extraW1S2 + 
           peer * extraW1P1 + aa * extraW1P2

extra2 =~ extraW2S1        + a * extraW2S2 + 
           peer * extraW2P1 + aa * extraW2P2

extra3 =~ extraW3S1        + a * extraW3S2 + 
           peer * extraW3P1 + aa * extraW3P2
  
extra4 =~ extraW4S1        + a * extraW4S2 + 
           peer * extraW4P1 + aa * extraW4P2

# second order factor for intercept and slope
interc =~ 1*extra1 + 1*extra2 + 1*extra3 + 1*extra4
slope =~ 0*extra1 + 6*extra2 + 13*extra3 + 19*extra4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
extraW1S1 ~ 0*1
extraW2S1 ~ 0*1
extraW3S1 ~ 0*1
extraW4S1 ~ 0*1

# fix equal intercepts
extraW1S2 ~ b*1
extraW2S2 ~ b*1
extraW3S2 ~ b*1
extraW4S2 ~ b*1

extraW1P1 ~ c*1
extraW2P1 ~ c*1
extraW3P1 ~ c*1
extraW4P1 ~ c*1

extraW1P2 ~ d*1
extraW2P2 ~ d*1
extraW3P2 ~ d*1
extraW4P2 ~ d*1

# error covariance - similar parcels across waves
extraW1S1 ~~ extraW2S1 + extraW3S1 + extraW4S1
extraW2S1 ~~ extraW3S1 + extraW4S1
extraW3S1 ~~ extraW4S1

extraW1S2 ~~ extraW2S2 + extraW3S2 + extraW4S2
extraW2S2 ~~ extraW3S2 + extraW4S2
extraW3S2 ~~ extraW4S2

extraW1P1 ~~ extraW2P1 + extraW3P1 + extraW4P1
extraW2P1 ~~ extraW3P1 + extraW4P1
extraW3P1 ~~ extraW4P1

extraW1P2 ~~ extraW2P2 + extraW3P2 + extraW4P2
extraW2P2 ~~ extraW3P2 + extraW4P2
extraW3P2 ~~ extraW4P2
'
lgmExtra <- sem(lgmExtra, data = data, missing = "ML")
summary(lgmExtra, fit.measures = T, standardized = T)

semPaths(lgmExtra, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Neuroticism ----

### with aspects as parcels
lgmNeuro <- '

# factor at each time point with same loading
neuro1 =~ volatW1S        + a * withdW1S + 
          peer * volatW1P + aa * withdW1P

neuro2 =~ volatW2S        + a * withdW2S + 
          peer * volatW2P + aa * withdW2P

neuro3 =~ volatW3S        + a * withdW3S + 
          peer * volatW3P + aa * withdW3P
  
neuro4 =~ volatW4S        + a * withdW4S + 
          peer * volatW4P + aa * withdW4P

# second order factor for intercept and slope
interc =~ 1*neuro1 + 1*neuro2 + 1*neuro3 + 1*neuro4
slope =~ 0*neuro1 + 6*neuro2 + 13*neuro3 + 19*neuro4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
volatW1S ~ 0*1
volatW2S ~ 0*1
volatW3S ~ 0*1
volatW4S ~ 0*1

# fix equal intercepts
withdW1S ~ b*1
withdW2S ~ b*1
withdW3S ~ b*1
withdW4S ~ b*1

volatW1P ~ c*1
volatW2P ~ c*1
volatW3P ~ c*1
volatW4P ~ c*1

withdW1P ~ d*1
withdW2P ~ d*1
withdW3P ~ d*1
withdW4P ~ d*1

# error covariance - similar aspects across waves and informants
volatW1S ~~ volatW2S + volatW3S + volatW4S +
            volatW1P + volatW2P + volatW3P + volatW4P
volatW2S ~~ volatW3S + volatW4S +
            volatW1P + volatW2P + volatW3P + volatW4P
volatW3S ~~ volatW4S +
            volatW1P + volatW2P + volatW3P + volatW4P
volatW4S ~~ volatW1P + volatW2P + volatW3P + volatW4P

withdW1S ~~ withdW2S + withdW3S + withdW4S +
            withdW1P + withdW2P + withdW3P + withdW4P
withdW2S ~~ withdW3S + withdW4S +
            withdW1P + withdW2P + withdW3P + withdW4P
withdW3S ~~ withdW4S +
            withdW1P + withdW2P + withdW3P + withdW4P
withdW4S ~~ withdW1P + withdW2P + withdW3P + withdW4P

volatW1P ~~ volatW2P + volatW3P + volatW4P
volatW2P ~~ volatW3P + volatW4P
volatW3P ~~ volatW4P

withdW1P ~~ withdW2P + withdW3P + withdW4P
withdW2P ~~ withdW3P + withdW4P
withdW3P ~~ withdW4P
'
lgmNeuro <- sem(lgmNeuro, data = data, missing = "ML")
summary(lgmNeuro, fit.measures = T, standardized = T)

semPaths(lgmNeuro, what = "col", whatLabels = "est", intercepts = T)

### with random parcels
lgmNeuro <- '

# factor at each time point with same loading
neuro1 =~ neuroW1S1        + a * neuroW1S2 + 
           peer * neuroW1P1 + aa * neuroW1P2

neuro2 =~ neuroW2S1        + a * neuroW2S2 + 
           peer * neuroW2P1 + aa * neuroW2P2

neuro3 =~ neuroW3S1        + a * neuroW3S2 + 
           peer * neuroW3P1 + aa * neuroW3P2
  
neuro4 =~ neuroW4S1        + a * neuroW4S2 + 
           peer * neuroW4P1 + aa * neuroW4P2

# second order factor for intercept and slope
interc =~ 1*neuro1 + 1*neuro2 + 1*neuro3 + 1*neuro4
slope =~ 0*neuro1 + 6*neuro2 + 13*neuro3 + 19*neuro4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
neuroW1S1 ~ 0*1
neuroW2S1 ~ 0*1
neuroW3S1 ~ 0*1
neuroW4S1 ~ 0*1

# fix equal intercepts
neuroW1S2 ~ b*1
neuroW2S2 ~ b*1
neuroW3S2 ~ b*1
neuroW4S2 ~ b*1

neuroW1P1 ~ c*1
neuroW2P1 ~ c*1
neuroW3P1 ~ c*1
neuroW4P1 ~ c*1

neuroW1P2 ~ d*1
neuroW2P2 ~ d*1
neuroW3P2 ~ d*1
neuroW4P2 ~ d*1

# error covariance - similar parcels across waves
neuroW1S1 ~~ neuroW2S1 + neuroW3S1 + neuroW4S1
neuroW2S1 ~~ neuroW3S1 + neuroW4S1
neuroW3S1 ~~ neuroW4S1

neuroW1S2 ~~ neuroW2S2 + neuroW3S2 + neuroW4S2
neuroW2S2 ~~ neuroW3S2 + neuroW4S2
neuroW3S2 ~~ neuroW4S2

neuroW1P1 ~~ neuroW2P1 + neuroW3P1 + neuroW4P1
neuroW2P1 ~~ neuroW3P1 + neuroW4P1
neuroW3P1 ~~ neuroW4P1

neuroW1P2 ~~ neuroW2P2 + neuroW3P2 + neuroW4P2
neuroW2P2 ~~ neuroW3P2 + neuroW4P2
neuroW3P2 ~~ neuroW4P2
'
lgmNeuro <- sem(lgmNeuro, data = data, missing = "ML")
summary(lgmNeuro, fit.measures = T, standardized = T)

semPaths(lgmNeuro, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Openness domain ----

### with aspects as parcels
lgmOpend <- '

# factor at each time point with same loading
opend1 =~ intelW1S        + a * openaW1S + 
          peer * intelW1P + aa * openaW1P

opend2 =~ intelW2S        + a * openaW2S + 
          peer * intelW2P + aa * openaW2P

opend3 =~ intelW3S        + a * openaW3S + 
          peer * intelW3P + aa * openaW3P
  
opend4 =~ intelW4S        + a * openaW4S + 
          peer * intelW4P + aa * openaW4P

# second order factor for intercept and slope
interc =~ 1*opend1 + 1*opend2 + 1*opend3 + 1*opend4
slope =~ 0*opend1 + 6*opend2 + 13*opend3 + 19*opend4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
intelW1S ~ 0*1
intelW2S ~ 0*1
intelW3S ~ 0*1
intelW4S ~ 0*1

# fix equal intercepts
openaW1S ~ b*1
openaW2S ~ b*1
openaW3S ~ b*1
openaW4S ~ b*1

intelW1P ~ c*1
intelW2P ~ c*1
intelW3P ~ c*1
intelW4P ~ c*1

openaW1P ~ d*1
openaW2P ~ d*1
openaW3P ~ d*1
openaW4P ~ d*1

# error covariance - similar aspects across waves and informants
intelW1S ~~ intelW2S + intelW3S + intelW4S +
            intelW1P + intelW2P + intelW3P + intelW4P
intelW2S ~~ intelW3S + intelW4S +
            intelW1P + intelW2P + intelW3P + intelW4P
intelW3S ~~ intelW4S +
            intelW1P + intelW2P + intelW3P + intelW4P
intelW4S ~~ intelW1P + intelW2P + intelW3P + intelW4P

openaW1S ~~ openaW2S + openaW3S + openaW4S +
            openaW1P + openaW2P + openaW3P + openaW4P
openaW2S ~~ openaW3S + openaW4S +
            openaW1P + openaW2P + openaW3P + openaW4P
openaW3S ~~ openaW4S +
            openaW1P + openaW2P + openaW3P + openaW4P
openaW4S ~~ openaW1P + openaW2P + openaW3P + openaW4P

intelW1P ~~ intelW2P + intelW3P + intelW4P
intelW2P ~~ intelW3P + intelW4P
intelW3P ~~ intelW4P

openaW1P ~~ openaW2P + openaW3P + openaW4P
openaW2P ~~ openaW3P + openaW4P
openaW3P ~~ openaW4P
'
lgmOpend <- sem(lgmOpend, data = data, missing = "ML")
summary(lgmOpend, fit.measures = T, standardized = T)

semPaths(lgmOpend, what = "col", whatLabels = "est", intercepts = T)

### with random parcels
lgmOpend <- '

# factor at each time point with same loading
opend1 =~ opendW1S1        + a * opendW1S2 + 
           peer * opendW1P1 + aa * opendW1P2

opend2 =~ opendW2S1        + a * opendW2S2 + 
           peer * opendW2P1 + aa * opendW2P2

opend3 =~ opendW3S1        + a * opendW3S2 + 
           peer * opendW3P1 + aa * opendW3P2
  
opend4 =~ opendW4S1        + a * opendW4S2 + 
           peer * opendW4P1 + aa * opendW4P2

# second order factor for intercept and slope
interc =~ 1*opend1 + 1*opend2 + 1*opend3 + 1*opend4
slope =~ 0*opend1 + 6*opend2 + 13*opend3 + 19*opend4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
opendW1S1 ~ 0*1
opendW2S1 ~ 0*1
opendW3S1 ~ 0*1
opendW4S1 ~ 0*1

# fix equal intercepts
opendW1S2 ~ b*1
opendW2S2 ~ b*1
opendW3S2 ~ b*1
opendW4S2 ~ b*1

opendW1P1 ~ c*1
opendW2P1 ~ c*1
opendW3P1 ~ c*1
opendW4P1 ~ c*1

opendW1P2 ~ d*1
opendW2P2 ~ d*1
opendW3P2 ~ d*1
opendW4P2 ~ d*1

# error covariance - similar parcels across waves
opendW1S1 ~~ opendW2S1 + opendW3S1 + opendW4S1
opendW2S1 ~~ opendW3S1 + opendW4S1
opendW3S1 ~~ opendW4S1

opendW1S2 ~~ opendW2S2 + opendW3S2 + opendW4S2
opendW2S2 ~~ opendW3S2 + opendW4S2
opendW3S2 ~~ opendW4S2

opendW1P1 ~~ opendW2P1 + opendW3P1 + opendW4P1
opendW2P1 ~~ opendW3P1 + opendW4P1
opendW3P1 ~~ opendW4P1

opendW1P2 ~~ opendW2P2 + opendW3P2 + opendW4P2
opendW2P2 ~~ opendW3P2 + opendW4P2
opendW3P2 ~~ opendW4P2
'
lgmOpend <- sem(lgmOpend, data = data, missing = "ML")
summary(lgmOpend, fit.measures = T, standardized = T)

semPaths(lgmOpend, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Assertiveness ----

lgmAssert <- '

# factor at each time point with same loading
assert1 =~ assertW1S1        + a * assertW1S2 + 
           peer * assertW1P1 + aa * assertW1P2

assert2 =~ assertW2S1        + a * assertW2S2 + 
           peer * assertW2P1 + aa * assertW2P2

assert3 =~ assertW3S1        + a * assertW3S2 + 
           peer * assertW3P1 + aa * assertW3P2
  
assert4 =~ assertW4S1        + a * assertW4S2 + 
           peer * assertW4P1 + aa * assertW4P2

# second order factor for intercept and slope
interc =~ 1*assert1 + 1*assert2 + 1*assert3 + 1*assert4
slope =~ 0*assert1 + 6*assert2 + 13*assert3 + 19*assert4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
assertW1S1 ~ 0*1
assertW2S1 ~ 0*1
assertW3S1 ~ 0*1
assertW4S1 ~ 0*1

# fix equal intercepts
assertW1S2 ~ b*1
assertW2S2 ~ b*1
assertW3S2 ~ b*1
assertW4S2 ~ b*1

assertW1P1 ~ c*1
assertW2P1 ~ c*1
assertW3P1 ~ c*1
assertW4P1 ~ c*1

assertW1P2 ~ d*1
assertW2P2 ~ d*1
assertW3P2 ~ d*1
assertW4P2 ~ d*1

# error covariance - similar parcels across waves
assertW1S1 ~~ assertW2S1 + assertW3S1 + assertW4S1
assertW2S1 ~~ assertW3S1 + assertW4S1
assertW3S1 ~~ assertW4S1

assertW1S2 ~~ assertW2S2 + assertW3S2 + assertW4S2
assertW2S2 ~~ assertW3S2 + assertW4S2
assertW3S2 ~~ assertW4S2

assertW1P1 ~~ assertW2P1 + assertW3P1 + assertW4P1
assertW2P1 ~~ assertW3P1 + assertW4P1
assertW3P1 ~~ assertW4P1

assertW1P2 ~~ assertW2P2 + assertW3P2 + assertW4P2
assertW2P2 ~~ assertW3P2 + assertW4P2
assertW3P2 ~~ assertW4P2
'
lgmAssert <- sem(lgmAssert, data = data, missing = "ML")
summary(lgmAssert, fit.measures = T, standardized = T)

semPaths(lgmAssert, what = "col", whatLabels = "est", intercepts = T)

# >> LGM Compassion ----

lgmCompa <- '

# factor at each time point with same loading
compa1 =~ compaW1S1        + a * compaW1S2 + 
           peer * compaW1P1 + aa * compaW1P2

compa2 =~ compaW2S1        + a * compaW2S2 + 
           peer * compaW2P1 + aa * compaW2P2

compa3 =~ compaW3S1        + a * compaW3S2 + 
           peer * compaW3P1 + aa * compaW3P2
  
compa4 =~ compaW4S1        + a * compaW4S2 + 
           peer * compaW4P1 + aa * compaW4P2

# second order factor for intercept and slope
interc =~ 1*compa1 + 1*compa2 + 1*compa3 + 1*compa4
slope =~ 0*compa1 + 6*compa2 + 13*compa3 + 19*compa4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
compaW1S1 ~ 0*1
compaW2S1 ~ 0*1
compaW3S1 ~ 0*1
compaW4S1 ~ 0*1

# fix equal intercepts
compaW1S2 ~ b*1
compaW2S2 ~ b*1
compaW3S2 ~ b*1
compaW4S2 ~ b*1

compaW1P1 ~ c*1
compaW2P1 ~ c*1
compaW3P1 ~ c*1
compaW4P1 ~ c*1

compaW1P2 ~ d*1
compaW2P2 ~ d*1
compaW3P2 ~ d*1
compaW4P2 ~ d*1

# error covariance - similar parcels across waves
compaW1S1 ~~ compaW2S1 + compaW3S1 + compaW4S1
compaW2S1 ~~ compaW3S1 + compaW4S1
compaW3S1 ~~ compaW4S1

compaW1S2 ~~ compaW2S2 + compaW3S2 + compaW4S2
compaW2S2 ~~ compaW3S2 + compaW4S2
compaW3S2 ~~ compaW4S2

compaW1P1 ~~ compaW2P1 + compaW3P1 + compaW4P1
compaW2P1 ~~ compaW3P1 + compaW4P1
compaW3P1 ~~ compaW4P1

compaW1P2 ~~ compaW2P2 + compaW3P2 + compaW4P2
compaW2P2 ~~ compaW3P2 + compaW4P2
compaW3P2 ~~ compaW4P2
'
lgmCompa <- sem(lgmCompa, data = data, missing = "ML")
summary(lgmCompa, fit.measures = T, standardized = T)

semPaths(lgmCompa, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Enthusiasm ----

lgmEnthu <- '

# factor at each time point with same loading
enthu1 =~ enthuW1S1        + a * enthuW1S2 + 
           peer * enthuW1P1 + aa * enthuW1P2

enthu2 =~ enthuW2S1        + a * enthuW2S2 + 
           peer * enthuW2P1 + aa * enthuW2P2

enthu3 =~ enthuW3S1        + a * enthuW3S2 + 
           peer * enthuW3P1 + aa * enthuW3P2
  
enthu4 =~ enthuW4S1        + a * enthuW4S2 + 
           peer * enthuW4P1 + aa * enthuW4P2

# second order factor for intercept and slope
interc =~ 1*enthu1 + 1*enthu2 + 1*enthu3 + 1*enthu4
slope =~ 0*enthu1 + 6*enthu2 + 13*enthu3 + 19*enthu4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
enthuW1S1 ~ 0*1
enthuW2S1 ~ 0*1
enthuW3S1 ~ 0*1
enthuW4S1 ~ 0*1

# fix equal intercepts
enthuW1S2 ~ b*1
enthuW2S2 ~ b*1
enthuW3S2 ~ b*1
enthuW4S2 ~ b*1

enthuW1P1 ~ c*1
enthuW2P1 ~ c*1
enthuW3P1 ~ c*1
enthuW4P1 ~ c*1

enthuW1P2 ~ d*1
enthuW2P2 ~ d*1
enthuW3P2 ~ d*1
enthuW4P2 ~ d*1

# error covariance - similar parcels across waves
enthuW1S1 ~~ enthuW2S1 + enthuW3S1 + enthuW4S1
enthuW2S1 ~~ enthuW3S1 + enthuW4S1
enthuW3S1 ~~ enthuW4S1

enthuW1S2 ~~ enthuW2S2 + enthuW3S2 + enthuW4S2
enthuW2S2 ~~ enthuW3S2 + enthuW4S2
enthuW3S2 ~~ enthuW4S2

enthuW1P1 ~~ enthuW2P1 + enthuW3P1 + enthuW4P1
enthuW2P1 ~~ enthuW3P1 + enthuW4P1
enthuW3P1 ~~ enthuW4P1

enthuW1P2 ~~ enthuW2P2 + enthuW3P2 + enthuW4P2
enthuW2P2 ~~ enthuW3P2 + enthuW4P2
enthuW3P2 ~~ enthuW4P2
'
lgmEnthu <- sem(lgmEnthu, data = data, missing = "ML")
summary(lgmEnthu, fit.measures = T, standardized = T)

semPaths(lgmEnthu, what = "col", whatLabels = "est", intercepts = T)

# >> LGM Industriousness ----

lgmIndus <- '

# factor at each time point with same loading
indus1 =~ indusW1S1        + a * indusW1S2 + 
           peer * indusW1P1 + aa * indusW1P2

indus2 =~ indusW2S1        + a * indusW2S2 + 
           peer * indusW2P1 + aa * indusW2P2

indus3 =~ indusW3S1        + a * indusW3S2 + 
           peer * indusW3P1 + aa * indusW3P2
  
indus4 =~ indusW4S1        + a * indusW4S2 + 
           peer * indusW4P1 + aa * indusW4P2

# second order factor for intercept and slope
interc =~ 1*indus1 + 1*indus2 + 1*indus3 + 1*indus4
slope =~ 0*indus1 + 6*indus2 + 13*indus3 + 19*indus4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
indusW1S1 ~ 0*1
indusW2S1 ~ 0*1
indusW3S1 ~ 0*1
indusW4S1 ~ 0*1

# fix equal intercepts
indusW1S2 ~ b*1
indusW2S2 ~ b*1
indusW3S2 ~ b*1
indusW4S2 ~ b*1

indusW1P1 ~ c*1
indusW2P1 ~ c*1
indusW3P1 ~ c*1
indusW4P1 ~ c*1

indusW1P2 ~ d*1
indusW2P2 ~ d*1
indusW3P2 ~ d*1
indusW4P2 ~ d*1

# error covariance - similar parcels across waves
indusW1S1 ~~ indusW2S1 + indusW3S1 + indusW4S1
indusW2S1 ~~ indusW3S1 + indusW4S1
indusW3S1 ~~ indusW4S1

indusW1S2 ~~ indusW2S2 + indusW3S2 + indusW4S2
indusW2S2 ~~ indusW3S2 + indusW4S2
indusW3S2 ~~ indusW4S2

indusW1P1 ~~ indusW2P1 + indusW3P1 + indusW4P1
indusW2P1 ~~ indusW3P1 + indusW4P1
indusW3P1 ~~ indusW4P1

indusW1P2 ~~ indusW2P2 + indusW3P2 + indusW4P2
indusW2P2 ~~ indusW3P2 + indusW4P2
indusW3P2 ~~ indusW4P2
'
lgmIndus <- sem(lgmIndus, data = data, missing = "ML")
summary(lgmIndus, fit.measures = T, standardized = T)

semPaths(lgmIndus, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Intellect ----

lgmIntel <- '

# factor at each time point with same loading
intel1 =~ intelW1S1        + a * intelW1S2 + 
           peer * intelW1P1 + aa * intelW1P2

intel2 =~ intelW2S1        + a * intelW2S2 + 
           peer * intelW2P1 + aa * intelW2P2

intel3 =~ intelW3S1        + a * intelW3S2 + 
           peer * intelW3P1 + aa * intelW3P2
  
intel4 =~ intelW4S1        + a * intelW4S2 + 
           peer * intelW4P1 + aa * intelW4P2

# second order factor for intercept and slope
interc =~ 1*intel1 + 1*intel2 + 1*intel3 + 1*intel4
slope =~ 0*intel1 + 6*intel2 + 13*intel3 + 19*intel4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
intelW1S1 ~ 0*1
intelW2S1 ~ 0*1
intelW3S1 ~ 0*1
intelW4S1 ~ 0*1

# fix equal intercepts
intelW1S2 ~ b*1
intelW2S2 ~ b*1
intelW3S2 ~ b*1
intelW4S2 ~ b*1

intelW1P1 ~ c*1
intelW2P1 ~ c*1
intelW3P1 ~ c*1
intelW4P1 ~ c*1

intelW1P2 ~ d*1
intelW2P2 ~ d*1
intelW3P2 ~ d*1
intelW4P2 ~ d*1

# error covariance - similar parcels across waves
intelW1S1 ~~ intelW2S1 + intelW3S1 + intelW4S1
intelW2S1 ~~ intelW3S1 + intelW4S1
intelW3S1 ~~ intelW4S1

intelW1S2 ~~ intelW2S2 + intelW3S2 + intelW4S2
intelW2S2 ~~ intelW3S2 + intelW4S2
intelW3S2 ~~ intelW4S2

intelW1P1 ~~ intelW2P1 + intelW3P1 + intelW4P1
intelW2P1 ~~ intelW3P1 + intelW4P1
intelW3P1 ~~ intelW4P1

intelW1P2 ~~ intelW2P2 + intelW3P2 + intelW4P2
intelW2P2 ~~ intelW3P2 + intelW4P2
intelW3P2 ~~ intelW4P2
'
lgmIntel <- sem(lgmIntel, data = data, missing = "ML")
summary(lgmIntel, fit.measures = T, standardized = T)

semPaths(lgmIntel, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Openness aspect ----

lgmOpena <- '

# factor at each time point with same loading
opena1 =~ openaW1S1        + a * openaW1S2 + 
           peer * openaW1P1 + aa * openaW1P2

opena2 =~ openaW2S1        + a * openaW2S2 + 
           peer * openaW2P1 + aa * openaW2P2

opena3 =~ openaW3S1        + a * openaW3S2 + 
           peer * openaW3P1 + aa * openaW3P2
  
opena4 =~ openaW4S1        + a * openaW4S2 + 
           peer * openaW4P1 + aa * openaW4P2

# second order factor for intercept and slope
interc =~ 1*opena1 + 1*opena2 + 1*opena3 + 1*opena4
slope =~ 0*opena1 + 6*opena2 + 13*opena3 + 19*opena4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
openaW1S1 ~ 0*1
openaW2S1 ~ 0*1
openaW3S1 ~ 0*1
openaW4S1 ~ 0*1

# fix equal intercepts
openaW1S2 ~ b*1
openaW2S2 ~ b*1
openaW3S2 ~ b*1
openaW4S2 ~ b*1

openaW1P1 ~ c*1
openaW2P1 ~ c*1
openaW3P1 ~ c*1
openaW4P1 ~ c*1

openaW1P2 ~ d*1
openaW2P2 ~ d*1
openaW3P2 ~ d*1
openaW4P2 ~ d*1

# error covariance - similar parcels across waves
openaW1S1 ~~ openaW2S1 + openaW3S1 + openaW4S1
openaW2S1 ~~ openaW3S1 + openaW4S1
openaW3S1 ~~ openaW4S1

openaW1S2 ~~ openaW2S2 + openaW3S2 + openaW4S2
openaW2S2 ~~ openaW3S2 + openaW4S2
openaW3S2 ~~ openaW4S2

openaW1P1 ~~ openaW2P1 + openaW3P1 + openaW4P1
openaW2P1 ~~ openaW3P1 + openaW4P1
openaW3P1 ~~ openaW4P1

openaW1P2 ~~ openaW2P2 + openaW3P2 + openaW4P2
openaW2P2 ~~ openaW3P2 + openaW4P2
openaW3P2 ~~ openaW4P2
'
lgmOpena <- sem(lgmOpena, data = data, missing = "ML")
summary(lgmOpena, fit.measures = T, standardized = T)

semPaths(lgmOpena, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Orderliness ----

lgmOrder <- '

# factor at each time point with same loading
order1 =~ orderW1S1        + a * orderW1S2 + 
           peer * orderW1P1 + aa * orderW1P2

order2 =~ orderW2S1        + a * orderW2S2 + 
           peer * orderW2P1 + aa * orderW2P2

order3 =~ orderW3S1        + a * orderW3S2 + 
           peer * orderW3P1 + aa * orderW3P2
  
order4 =~ orderW4S1        + a * orderW4S2 + 
           peer * orderW4P1 + aa * orderW4P2

# second order factor for intercept and slope
interc =~ 1*order1 + 1*order2 + 1*order3 + 1*order4
slope =~ 0*order1 + 6*order2 + 13*order3 + 19*order4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
orderW1S1 ~ 0*1
orderW2S1 ~ 0*1
orderW3S1 ~ 0*1
orderW4S1 ~ 0*1

# fix equal intercepts
orderW1S2 ~ b*1
orderW2S2 ~ b*1
orderW3S2 ~ b*1
orderW4S2 ~ b*1

orderW1P1 ~ c*1
orderW2P1 ~ c*1
orderW3P1 ~ c*1
orderW4P1 ~ c*1

orderW1P2 ~ d*1
orderW2P2 ~ d*1
orderW3P2 ~ d*1
orderW4P2 ~ d*1

# error covariance - similar parcels across waves
orderW1S1 ~~ orderW2S1 + orderW3S1 + orderW4S1
orderW2S1 ~~ orderW3S1 + orderW4S1
orderW3S1 ~~ orderW4S1

orderW1S2 ~~ orderW2S2 + orderW3S2 + orderW4S2
orderW2S2 ~~ orderW3S2 + orderW4S2
orderW3S2 ~~ orderW4S2

orderW1P1 ~~ orderW2P1 + orderW3P1 + orderW4P1
orderW2P1 ~~ orderW3P1 + orderW4P1
orderW3P1 ~~ orderW4P1

orderW1P2 ~~ orderW2P2 + orderW3P2 + orderW4P2
orderW2P2 ~~ orderW3P2 + orderW4P2
orderW3P2 ~~ orderW4P2
'
lgmOrder <- sem(lgmOrder, data = data, missing = "ML")
summary(lgmOrder, fit.measures = T, standardized = T)

semPaths(lgmOrder, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Politeness ----

lgmPolit <- '

# factor at each time point with same loading
polit1 =~ politW1S1        + a * politW1S2 + 
           peer * politW1P1 + aa * politW1P2

polit2 =~ politW2S1        + a * politW2S2 + 
           peer * politW2P1 + aa * politW2P2

polit3 =~ politW3S1        + a * politW3S2 + 
           peer * politW3P1 + aa * politW3P2
  
polit4 =~ politW4S1        + a * politW4S2 + 
           peer * politW4P1 + aa * politW4P2

# second polit factor for intercept and slope
interc =~ 1*polit1 + 1*polit2 + 1*polit3 + 1*polit4
slope =~ 0*polit1 + 6*polit2 + 13*polit3 + 19*polit4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
politW1S1 ~ 0*1
politW2S1 ~ 0*1
politW3S1 ~ 0*1
politW4S1 ~ 0*1

# fix equal intercepts
politW1S2 ~ b*1
politW2S2 ~ b*1
politW3S2 ~ b*1
politW4S2 ~ b*1

politW1P1 ~ c*1
politW2P1 ~ c*1
politW3P1 ~ c*1
politW4P1 ~ c*1

politW1P2 ~ d*1
politW2P2 ~ d*1
politW3P2 ~ d*1
politW4P2 ~ d*1

# error covariance - similar parcels across waves
politW1S1 ~~ politW2S1 + politW3S1 + politW4S1
politW2S1 ~~ politW3S1 + politW4S1
politW3S1 ~~ politW4S1

politW1S2 ~~ politW2S2 + politW3S2 + politW4S2
politW2S2 ~~ politW3S2 + politW4S2
politW3S2 ~~ politW4S2

politW1P1 ~~ politW2P1 + politW3P1 + politW4P1
politW2P1 ~~ politW3P1 + politW4P1
politW3P1 ~~ politW4P1

politW1P2 ~~ politW2P2 + politW3P2 + politW4P2
politW2P2 ~~ politW3P2 + politW4P2
politW3P2 ~~ politW4P2
'
lgmPolit <- sem(lgmPolit, data = data, missing = "ML")
summary(lgmPolit, fit.measures = T, standardized = T)

semPaths(lgmPolit, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Volatility ----

lgmVolat <- '

# factor at each time point with same loading
volat1 =~ volatW1S1        + a * volatW1S2 + 
           peer * volatW1P1 + aa * volatW1P2

volat2 =~ volatW2S1        + a * volatW2S2 + 
           peer * volatW2P1 + aa * volatW2P2

volat3 =~ volatW3S1        + a * volatW3S2 + 
           peer * volatW3P1 + aa * volatW3P2
  
volat4 =~ volatW4S1        + a * volatW4S2 + 
           peer * volatW4P1 + aa * volatW4P2

# second volat factor for intercept and slope
interc =~ 1*volat1 + 1*volat2 + 1*volat3 + 1*volat4
slope =~ 0*volat1 + 6*volat2 + 13*volat3 + 19*volat4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
volatW1S1 ~ 0*1
volatW2S1 ~ 0*1
volatW3S1 ~ 0*1
volatW4S1 ~ 0*1

# fix equal intercepts
volatW1S2 ~ b*1
volatW2S2 ~ b*1
volatW3S2 ~ b*1
volatW4S2 ~ b*1

volatW1P1 ~ c*1
volatW2P1 ~ c*1
volatW3P1 ~ c*1
volatW4P1 ~ c*1

volatW1P2 ~ d*1
volatW2P2 ~ d*1
volatW3P2 ~ d*1
volatW4P2 ~ d*1

# error covariance - similar parcels across waves
volatW1S1 ~~ volatW2S1 + volatW3S1 + volatW4S1
volatW2S1 ~~ volatW3S1 + volatW4S1
volatW3S1 ~~ volatW4S1

volatW1S2 ~~ volatW2S2 + volatW3S2 + volatW4S2
volatW2S2 ~~ volatW3S2 + volatW4S2
volatW3S2 ~~ volatW4S2

volatW1P1 ~~ volatW2P1 + volatW3P1 + volatW4P1
volatW2P1 ~~ volatW3P1 + volatW4P1
volatW3P1 ~~ volatW4P1

volatW1P2 ~~ volatW2P2 + volatW3P2 + volatW4P2
volatW2P2 ~~ volatW3P2 + volatW4P2
volatW3P2 ~~ volatW4P2
'
lgmVolat <- sem(lgmVolat, data = data, missing = "ML")
summary(lgmVolat, fit.measures = T, standardized = T)

semPaths(lgmVolat, what = "col", whatLabels = "est", intercepts = T)


# >> LGM Withdrawal ----

lgmWithd <- '

# factor at each time point with same loading
withd1 =~ withdW1S1        + a * withdW1S2 + 
           peer * withdW1P1 + aa * withdW1P2

withd2 =~ withdW2S1        + a * withdW2S2 + 
           peer * withdW2P1 + aa * withdW2P2

withd3 =~ withdW3S1        + a * withdW3S2 + 
           peer * withdW3P1 + aa * withdW3P2
  
withd4 =~ withdW4S1        + a * withdW4S2 + 
           peer * withdW4P1 + aa * withdW4P2

# second withd factor for intercept and slope
interc =~ 1*withd1 + 1*withd2 + 1*withd3 + 1*withd4
slope =~ 0*withd1 + 6*withd2 + 13*withd3 + 19*withd4
interc ~~ slope
interc ~ 1
slope ~ 1

# fix zero intercepts
withdW1S1 ~ 0*1
withdW2S1 ~ 0*1
withdW3S1 ~ 0*1
withdW4S1 ~ 0*1

# fix equal intercepts
withdW1S2 ~ b*1
withdW2S2 ~ b*1
withdW3S2 ~ b*1
withdW4S2 ~ b*1

withdW1P1 ~ c*1
withdW2P1 ~ c*1
withdW3P1 ~ c*1
withdW4P1 ~ c*1

withdW1P2 ~ d*1
withdW2P2 ~ d*1
withdW3P2 ~ d*1
withdW4P2 ~ d*1

# error covariance - similar parcels across waves
withdW1S1 ~~ withdW2S1 + withdW3S1 + withdW4S1
withdW2S1 ~~ withdW3S1 + withdW4S1
withdW3S1 ~~ withdW4S1

withdW1S2 ~~ withdW2S2 + withdW3S2 + withdW4S2
withdW2S2 ~~ withdW3S2 + withdW4S2
withdW3S2 ~~ withdW4S2

withdW1P1 ~~ withdW2P1 + withdW3P1 + withdW4P1
withdW2P1 ~~ withdW3P1 + withdW4P1
withdW3P1 ~~ withdW4P1

withdW1P2 ~~ withdW2P2 + withdW3P2 + withdW4P2
withdW2P2 ~~ withdW3P2 + withdW4P2
withdW3P2 ~~ withdW4P2
'
lgmWithd <- sem(lgmWithd, data = data, missing = "ML")
summary(lgmWithd, fit.measures = T, standardized = T)

semPaths(lgmWithd, what = "col", whatLabels = "est", intercepts = T)


# > Latent stability model ====

# >> LSM Assertiveness ----
lsmAssert <- '

# factor at each time point with same loading
assert1 =~ assertW1S1        + a * assertW1S2 + 
           peer * assertW1P1 + aa * assertW1P2

assert2 =~ assertW2S1        + a * assertW2S2 + 
           peer * assertW2P1 + aa * assertW2P2

assert3 =~ assertW3S1        + a * assertW3S2 + 
           peer * assertW3P1 + aa * assertW3P2
  
assert4 =~ assertW4S1        + a * assertW4S2 + 
           peer * assertW4P1 + aa * assertW4P2

# structural paths between time points 
assert4 ~ assert3
assert3 ~ assert2
assert2 ~ assert1

# error covariance - similar parcels across waves
assertW1S1 ~~ assertW2S1 + assertW3S1 + assertW4S1
assertW2S1 ~~ assertW3S1 + assertW4S1
assertW3S1 ~~ assertW4S1

assertW1S2 ~~ assertW2S2 + assertW3S2 + assertW4S2
assertW2S2 ~~ assertW3S2 + assertW4S2
assertW3S2 ~~ assertW4S2

assertW1P1 ~~ assertW2P1 + assertW3P1 + assertW4P1
assertW2P1 ~~ assertW3P1 + assertW4P1
assertW3P1 ~~ assertW4P1

assertW1P2 ~~ assertW2P2 + assertW3P2 + assertW4P2
assertW2P2 ~~ assertW3P2 + assertW4P2
assertW3P2 ~~ assertW4P2
'
lsmAssert <- sem(lsmAssert, data = data, missing = "ML")
summary(lsmAssert)

semPaths(lsmAssert, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Compassion ----

lsmCompa <- '

# factor at each time point with same loading
compa1 =~ compaW1S1        + a * compaW1S2 + 
           peer * compaW1P1 + aa * compaW1P2

compa2 =~ compaW2S1        + a * compaW2S2 + 
           peer * compaW2P1 + aa * compaW2P2

compa3 =~ compaW3S1        + a * compaW3S2 + 
           peer * compaW3P1 + aa * compaW3P2
  
compa4 =~ compaW4S1        + a * compaW4S2 + 
           peer * compaW4P1 + aa * compaW4P2

# structural paths between time points 
compa4 ~ compa3
compa3 ~ compa2
compa2 ~ compa1

# error covariance - similar parcels across waves
compaW1S1 ~~ compaW2S1 + compaW3S1 + compaW4S1
compaW2S1 ~~ compaW3S1 + compaW4S1
compaW3S1 ~~ compaW4S1

compaW1S2 ~~ compaW2S2 + compaW3S2 + compaW4S2
compaW2S2 ~~ compaW3S2 + compaW4S2
compaW3S2 ~~ compaW4S2

compaW1P1 ~~ compaW2P1 + compaW3P1 + compaW4P1
compaW2P1 ~~ compaW3P1 + compaW4P1
compaW3P1 ~~ compaW4P1

compaW1P2 ~~ compaW2P2 + compaW3P2 + compaW4P2
compaW2P2 ~~ compaW3P2 + compaW4P2
compaW3P2 ~~ compaW4P2
'
lsmCompa <- sem(lsmCompa, data = data, missing = "ML")
summary(lsmCompa)

semPaths(lsmCompa, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Enthusiasm ----

lsmEnthu <- '

# factor at each time point with same loading
enthu1 =~ enthuW1S1        + a * enthuW1S2 + 
           peer * enthuW1P1 + aa * enthuW1P2

enthu2 =~ enthuW2S1        + a * enthuW2S2 + 
           peer * enthuW2P1 + aa * enthuW2P2

enthu3 =~ enthuW3S1        + a * enthuW3S2 + 
           peer * enthuW3P1 + aa * enthuW3P2
  
enthu4 =~ enthuW4S1        + a * enthuW4S2 + 
           peer * enthuW4P1 + aa * enthuW4P2

# structural paths between time points 
enthu4 ~ enthu3
enthu3 ~ enthu2
enthu2 ~ enthu1

# error covariance - similar parcels across waves
enthuW1S1 ~~ enthuW2S1 + enthuW3S1 + enthuW4S1
enthuW2S1 ~~ enthuW3S1 + enthuW4S1
enthuW3S1 ~~ enthuW4S1

enthuW1S2 ~~ enthuW2S2 + enthuW3S2 + enthuW4S2
enthuW2S2 ~~ enthuW3S2 + enthuW4S2
enthuW3S2 ~~ enthuW4S2

enthuW1P1 ~~ enthuW2P1 + enthuW3P1 + enthuW4P1
enthuW2P1 ~~ enthuW3P1 + enthuW4P1
enthuW3P1 ~~ enthuW4P1

enthuW1P2 ~~ enthuW2P2 + enthuW3P2 + enthuW4P2
enthuW2P2 ~~ enthuW3P2 + enthuW4P2
enthuW3P2 ~~ enthuW4P2
'
lsmEnthu <- sem(lsmEnthu, data = data, missing = "ML")
summary(lsmEnthu)

semPaths(lsmEnthu, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Industriousness ----

lsmIndus <- '

# factor at each time point with same loading
indus1 =~ indusW1S1        + a * indusW1S2 + 
           peer * indusW1P1 + aa * indusW1P2

indus2 =~ indusW2S1        + a * indusW2S2 + 
           peer * indusW2P1 + aa * indusW2P2

indus3 =~ indusW3S1        + a * indusW3S2 + 
           peer * indusW3P1 + aa * indusW3P2
  
indus4 =~ indusW4S1        + a * indusW4S2 + 
           peer * indusW4P1 + aa * indusW4P2

# structural paths between time points 
indus4 ~ indus3
indus3 ~ indus2
indus2 ~ indus1

# error covariance - similar parcels across waves
indusW1S1 ~~ indusW2S1 + indusW3S1 + indusW4S1
indusW2S1 ~~ indusW3S1 + indusW4S1
indusW3S1 ~~ indusW4S1

indusW1S2 ~~ indusW2S2 + indusW3S2 + indusW4S2
indusW2S2 ~~ indusW3S2 + indusW4S2
indusW3S2 ~~ indusW4S2

indusW1P1 ~~ indusW2P1 + indusW3P1 + indusW4P1
indusW2P1 ~~ indusW3P1 + indusW4P1
indusW3P1 ~~ indusW4P1

indusW1P2 ~~ indusW2P2 + indusW3P2 + indusW4P2
indusW2P2 ~~ indusW3P2 + indusW4P2
indusW3P2 ~~ indusW4P2
'
lsmIndus <- sem(lsmIndus, data = data, missing = "ML")
summary(lsmIndus)

semPaths(lsmIndus, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Intellect ----

lsmIntel <- '

# factor at each time point with same loading
intel1 =~ intelW1S1        + a * intelW1S2 + 
           peer * intelW1P1 + aa * intelW1P2

intel2 =~ intelW2S1        + a * intelW2S2 + 
           peer * intelW2P1 + aa * intelW2P2

intel3 =~ intelW3S1        + a * intelW3S2 + 
           peer * intelW3P1 + aa * intelW3P2
  
intel4 =~ intelW4S1        + a * intelW4S2 + 
           peer * intelW4P1 + aa * intelW4P2

# structural paths between time points 
intel4 ~ intel3
intel3 ~ intel2
intel2 ~ intel1

# error covariance - similar parcels across waves
intelW1S1 ~~ intelW2S1 + intelW3S1 + intelW4S1
intelW2S1 ~~ intelW3S1 + intelW4S1
intelW3S1 ~~ intelW4S1

intelW1S2 ~~ intelW2S2 + intelW3S2 + intelW4S2
intelW2S2 ~~ intelW3S2 + intelW4S2
intelW3S2 ~~ intelW4S2

intelW1P1 ~~ intelW2P1 + intelW3P1 + intelW4P1
intelW2P1 ~~ intelW3P1 + intelW4P1
intelW3P1 ~~ intelW4P1

intelW1P2 ~~ intelW2P2 + intelW3P2 + intelW4P2
intelW2P2 ~~ intelW3P2 + intelW4P2
intelW3P2 ~~ intelW4P2
'
lsmIntel <- sem(lsmIntel, data = data, missing = "ML")
summary(lsmIntel)

semPaths(lsmIntel, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Openness aspect ----

lsmOpena <- '

# factor at each time point with same loading
opena1 =~ openaW1S1        + a * openaW1S2 + 
           peer * openaW1P1 + aa * openaW1P2

opena2 =~ openaW2S1        + a * openaW2S2 + 
           peer * openaW2P1 + aa * openaW2P2

opena3 =~ openaW3S1        + a * openaW3S2 + 
           peer * openaW3P1 + aa * openaW3P2
  
opena4 =~ openaW4S1        + a * openaW4S2 + 
           peer * openaW4P1 + aa * openaW4P2

# structural paths between time points 
opena4 ~ opena3
opena3 ~ opena2
opena2 ~ opena1

# error covariance - similar parcels across waves
openaW1S1 ~~ openaW2S1 + openaW3S1 + openaW4S1
openaW2S1 ~~ openaW3S1 + openaW4S1
openaW3S1 ~~ openaW4S1

openaW1S2 ~~ openaW2S2 + openaW3S2 + openaW4S2
openaW2S2 ~~ openaW3S2 + openaW4S2
openaW3S2 ~~ openaW4S2

openaW1P1 ~~ openaW2P1 + openaW3P1 + openaW4P1
openaW2P1 ~~ openaW3P1 + openaW4P1
openaW3P1 ~~ openaW4P1

openaW1P2 ~~ openaW2P2 + openaW3P2 + openaW4P2
openaW2P2 ~~ openaW3P2 + openaW4P2
openaW3P2 ~~ openaW4P2
'
lsmOpena <- sem(lsmOpena, data = data, missing = "ML")
summary(lsmOpena)

semPaths(lsmOpena, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Orderliness ----

lsmOrder <- '

# factor at each time point with same loading
order1 =~ orderW1S1        + a * orderW1S2 + 
           peer * orderW1P1 + aa * orderW1P2

order2 =~ orderW2S1        + a * orderW2S2 + 
           peer * orderW2P1 + aa * orderW2P2

order3 =~ orderW3S1        + a * orderW3S2 + 
           peer * orderW3P1 + aa * orderW3P2
  
order4 =~ orderW4S1        + a * orderW4S2 + 
           peer * orderW4P1 + aa * orderW4P2

# structural paths between time points 
order4 ~ order3
order3 ~ order2
order2 ~ order1

# error covariance - similar parcels across waves
orderW1S1 ~~ orderW2S1 + orderW3S1 + orderW4S1
orderW2S1 ~~ orderW3S1 + orderW4S1
orderW3S1 ~~ orderW4S1

orderW1S2 ~~ orderW2S2 + orderW3S2 + orderW4S2
orderW2S2 ~~ orderW3S2 + orderW4S2
orderW3S2 ~~ orderW4S2

orderW1P1 ~~ orderW2P1 + orderW3P1 + orderW4P1
orderW2P1 ~~ orderW3P1 + orderW4P1
orderW3P1 ~~ orderW4P1

orderW1P2 ~~ orderW2P2 + orderW3P2 + orderW4P2
orderW2P2 ~~ orderW3P2 + orderW4P2
orderW3P2 ~~ orderW4P2
'
lsmOrder <- sem(lsmOrder, data = data, missing = "ML")
summary(lsmOrder)

semPaths(lsmOrder, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Politeness ----

lsmPolit <- '

# factor at each time point with same loading
polit1 =~ politW1S1        + a * politW1S2 + 
           peer * politW1P1 + aa * politW1P2

polit2 =~ politW2S1        + a * politW2S2 + 
           peer * politW2P1 + aa * politW2P2

polit3 =~ politW3S1        + a * politW3S2 + 
           peer * politW3P1 + aa * politW3P2
  
polit4 =~ politW4S1        + a * politW4S2 + 
           peer * politW4P1 + aa * politW4P2

# structural paths between time points 
polit4 ~ polit3
polit3 ~ polit2
polit2 ~ polit1

# error covariance - similar parcels across waves
politW1S1 ~~ politW2S1 + politW3S1 + politW4S1
politW2S1 ~~ politW3S1 + politW4S1
politW3S1 ~~ politW4S1

politW1S2 ~~ politW2S2 + politW3S2 + politW4S2
politW2S2 ~~ politW3S2 + politW4S2
politW3S2 ~~ politW4S2

politW1P1 ~~ politW2P1 + politW3P1 + politW4P1
politW2P1 ~~ politW3P1 + politW4P1
politW3P1 ~~ politW4P1

politW1P2 ~~ politW2P2 + politW3P2 + politW4P2
politW2P2 ~~ politW3P2 + politW4P2
politW3P2 ~~ politW4P2
'
lsmPolit <- sem(lsmPolit, data = data, missing = "ML")
summary(lsmPolit)

semPaths(lsmPolit, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Volatility ----

lsmVolat <- '

# factor at each time point with same loading
volat1 =~ volatW1S1        + a * volatW1S2 + 
           peer * volatW1P1 + aa * volatW1P2

volat2 =~ volatW2S1        + a * volatW2S2 + 
           peer * volatW2P1 + aa * volatW2P2

volat3 =~ volatW3S1        + a * volatW3S2 + 
           peer * volatW3P1 + aa * volatW3P2
  
volat4 =~ volatW4S1        + a * volatW4S2 + 
           peer * volatW4P1 + aa * volatW4P2

# structural paths between time points 
volat4 ~ volat3
volat3 ~ volat2
volat2 ~ volat1

# error covariance - similar parcels across waves
volatW1S1 ~~ volatW2S1 + volatW3S1 + volatW4S1
volatW2S1 ~~ volatW3S1 + volatW4S1
volatW3S1 ~~ volatW4S1

volatW1S2 ~~ volatW2S2 + volatW3S2 + volatW4S2
volatW2S2 ~~ volatW3S2 + volatW4S2
volatW3S2 ~~ volatW4S2

volatW1P1 ~~ volatW2P1 + volatW3P1 + volatW4P1
volatW2P1 ~~ volatW3P1 + volatW4P1
volatW3P1 ~~ volatW4P1

volatW1P2 ~~ volatW2P2 + volatW3P2 + volatW4P2
volatW2P2 ~~ volatW3P2 + volatW4P2
volatW3P2 ~~ volatW4P2
'
lsmVolat <- sem(lsmVolat, data = data, missing = "ML")
summary(lsmVolat)

semPaths(lsmVolat, what = "col", whatLabels = "est", structural = T, layout = "spring")


# >> LSM Withdrawal ----

lsmWithd <- '

# factor at each time point with same loading
withd1 =~ withdW1S1        + a * withdW1S2 + 
           peer * withdW1P1 + aa * withdW1P2

withd2 =~ withdW2S1        + a * withdW2S2 + 
           peer * withdW2P1 + aa * withdW2P2

withd3 =~ withdW3S1        + a * withdW3S2 + 
           peer * withdW3P1 + aa * withdW3P2
  
withd4 =~ withdW4S1        + a * withdW4S2 + 
           peer * withdW4P1 + aa * withdW4P2

# structural paths between time points 
withd4 ~ withd3
withd3 ~ withd2
withd2 ~ withd1

# error covariance - similar parcels across waves
withdW1S1 ~~ withdW2S1 + withdW3S1 + withdW4S1
withdW2S1 ~~ withdW3S1 + withdW4S1
withdW3S1 ~~ withdW4S1

withdW1S2 ~~ withdW2S2 + withdW3S2 + withdW4S2
withdW2S2 ~~ withdW3S2 + withdW4S2
withdW3S2 ~~ withdW4S2

withdW1P1 ~~ withdW2P1 + withdW3P1 + withdW4P1
withdW2P1 ~~ withdW3P1 + withdW4P1
withdW3P1 ~~ withdW4P1

withdW1P2 ~~ withdW2P2 + withdW3P2 + withdW4P2
withdW2P2 ~~ withdW3P2 + withdW4P2
withdW3P2 ~~ withdW4P2
'
lsmWithd <- sem(lsmWithd, data = data, missing = "ML")
summary(lsmWithd)

semPaths(lsmWithd, what = "col", whatLabels = "est", structural = T, layout = "spring")