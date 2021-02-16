##############################################
##  First Year Project: Personality Change  ##
##  Latent models: self + peer              ##
##                                          ##
##  Linh Nguyen                             ##
##  Created: Feb-15-2021                    ##
##  Updated: Feb-16-2021                    ##
##############################################


# > Preprocessing ====
# >> Data ----
require(lavaan)
require(tidyverse)
require(semPlot)

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
set.seed(202102)

# self randomization - 0, 4, 6, 7, 8 vs. 1, 2, 3, 5, 9
sample(c(0,1,2,3,4,5,6,7,8,9), 5)
# peer randomization - 1, 2, 4, 5, 6 vs. 0, 3, 7, 8, 9
sample(c(0,1,2,3,4,5,6,7,8,9), 5)

# assertiveness 
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

data[data == "NaN"] <- NA

# > Latent growth model ====

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

semPaths(lgmAssert, what = "col", whatLabels = "est",intercepts = T)

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
