#####################################################################
## First year R script                                             ##
## Linh Nguyen                                                     ##
## Created: 02/03/2020                                             ##
## Last updated: 11/11/2020   #correlated change btw self-peer     ##
## NEXT: MCAR test                                                 ##
## NEXT: SAS for banded main diagonal:                             ##
## https://support.sas.com/resources/papers/proceedings/proceedings/sugi30/198-30.pdf
## To navigate: Edit - Folding - Collapse All                      ##
#####################################################################

# METADATA  ==========================================
renv::restore() 
library(lme4)
library(lmerTest)
library(reshape2)
library(plyr)
library(dplyr)
library(sjPlot)
library(psych)
library(tidyr)
library(mitml)
library(MASS)
library(sjmisc)
library(Hmisc)
library(haven)
library(apaTables)
library(stringr)
library(cowplot)
library(forestplot)
library(renv)
options(scipen = 999)
set.seed(184)

#themes for sjPlot
set_theme(
  base = theme_bw(),
  axis.title.size = 1.8,
  axis.textsize = 1,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3)

# CLEANING ====
# > Data files----
peerw1 <- read_spss("peerw1.sav") %>% 
  dplyr::select(ID, PeerID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
peerw2 <- read_spss("peerw2.sav") %>% 
  dplyr::select(ID, PeerID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
peerw3 <- read_spss("peerw3.sav") %>% 
  dplyr::select(ID, PeerID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
peerw4 <- read_spss("peerw4.sav") %>% 
  dplyr::select(ID, PeerID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
selfw1 <- read_spss("selfw1.sav") %>% 
  dplyr::select(ID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
selfw2 <- read_spss("selfw2.sav") %>% 
  dplyr::select(ID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
selfw3 <- read_spss("selfw3.sav") %>% 
  dplyr::select(ID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)
selfw4 <- read_spss("selfw4.sav") %>% 
  dplyr::select(ID, bfas_agreeableness:bfas_withdrawal, epsi_confusion, epsi_coherence)

demo <- read_spss("selfw1.sav") %>% 
  dplyr::select(ID:race, race_f)


# > Merge different waves into 1 file----
# sorting by ID
selfw1 <- selfw1[order(selfw1$ID),]
selfw2 <- selfw2[order(selfw2$ID),]
selfw3 <- selfw3[order(selfw3$ID),]
selfw4 <- selfw4[order(selfw4$ID),]
peerw1 <- peerw1[order(peerw1$ID),]
peerw2 <- peerw2[order(peerw2$ID),]
peerw3 <- peerw3[order(peerw3$ID),]
peerw4 <- peerw4[order(peerw4$ID),]

#add time variable wave 1-4
selfw1$time <- 1
selfw2$time <- 2
selfw3$time <- 3
selfw4$time <- 4
peerw1$time <- 1
peerw2$time <- 2
peerw3$time <- 3
peerw4$time <- 4

#combine all wave datasets
self <- rbind(selfw1,selfw2)
self <- rbind(self,selfw3)
self <- rbind(self,selfw4)
self <- self[,c(1,19,2:18)]

peer <- rbind(peerw1,peerw2)
peer <- rbind(peer,peerw3)
peer <- rbind(peer,peerw4)
peer <- peer[,c(1,2,20,3:19)]

#change time variable to months
self$time <- as.numeric(self$time)
self$time[self$time == 1] <- 0
self$time[self$time == 2] <- 6 
self$time[self$time == 3] <- 13
self$time[self$time == 4] <- 19

peer$time <- as.numeric(peer$time)
peer$time[peer$time == 1] <- 0
peer$time[peer$time == 2] <- 6 
peer$time[peer$time == 3] <- 13
peer$time[peer$time == 4] <- 19

# > Wide datasets----
selfw <- melt(self, id = c("ID","time")) 
selfw <- dcast(selfw, ID ~ time + variable,
               value.var = "value")
names <- colnames(selfw1)[2:18]
names(selfw)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))
rm("names")

peerw <- melt(peer, id = c("ID","PeerID","time")) 
peerw <- dcast(peerw, ID + PeerID ~ time + variable,
               value.var = "value")
names <- colnames(peerw1)[3:19]
names(peerw)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))
rm("names")

# > Filter out peers with at least 2 waves----
## list of IDs in "Peer Log.xlsx"
fullpeer <- list(2001,2002,2004,2005,2006,2009,2012,2013,2014,2015,2016,2017,2018,2019,2020,2024,2026,2027,2032,2033,2034,2037,2042,2048,2049,2051,2052,2053,2054,2056,2063,2065,2066,2068,2069,2070,2074,2075,2078,2084,2086,2087,2096,2098,2102,2103,2106,2107,2110,2112,2113,2114,2116,2117,2118,2119,2120,2121,2122,2123,2124,2125,2126,2129,2130,2135,2138,2139,2142,2143,2144,2145,2146,2147,2150,2152,2154,2156,2157,2159,2162,2163,2164,2165,2167,2173,2174,2175,2177,2180,2181,2182,2185,2186,2187,2188,2189,2190,2191,2192,2193,2194,2197,2198,2199,2202,2205,2206,2207,2209,2210,2212,2213,2214,2217,2218,2220,2221,2223,2225,2227,2228,2229,2230,2233,2235,2237,2239,2240,2241,2242,2243,2244,2245,2246,2247,2251,2254,2255,2256,2260,2261,2262,2263,2264,2265,2266,2268,2269,2271,2274,2276,2279,2286,2287,2288,2289,2290,2291,2292,2293,2294,2296,2297,2298,2300,2308,2309,2315,2319,2321,2323,2325,2326,2330,2331,2335,2336,2337,2339,2340,2342,2343,2344,2346,2347,2348,2349,2351,2352,2353,2357,2359,2362,2364,2367,2370,2371,2372,2375,2378,2381,2382,2383,2386,2387,2388,2397,2399,2400,2402,2403,2404,2408,2412,2413,2414,2415,2418,2420,2421,2423,2424,2425,2427,2428,2430,2432,2433,2434,2437,2438,2439,2440,2441,2442,2443,2445,2447,2448,2449,2451,2452,2453,2454,2456,2457,2458,2459,2461,2464,2465,2469,2470,2471,2472,2473,2474,2476,2477,2479,2482,2483,2484,2485,2487,2488,2489,2490,2493,2498,2499,2502,2503,2504,2509,2510,2511,2512,2513,2514,2518,2519)

peer <- peer %>%
  filter(PeerID %in% fullpeer)
peerw <- peerw %>% 
  filter(PeerID %in% fullpeer)
rm("fullpeer")

# > Randomly select 1 peer if multiple----
##save list of random ID for future referencce
##split_peerw <- split(peerw, peerw$ID)
##group_sizes <- vapply(split_peerw, nrow, integer(1))
##sampled_obs <- mapply(sample, group_sizes, 1)
##get_rows <- function(df, rows) df[rows, , drop = FALSE]
##peerws <-                        
##  mapply(get_rows, split_peerw, sampled_obs, SIMPLIFY = FALSE)
##peerws <- do.call(rbind, peerws) 
##randomID <- peerws$PeerID
##saveRDS(randomID, "randomID.R")
##rm(split_peerw, peerws, group_sizes, randomID, sampled_obs, get_rows)


randomID <- readRDS("randomID.R")
peer <- peer %>% 
  filter(PeerID %in% randomID)
peerw <- peerw %>% 
  filter(PeerID %in% randomID)


# > Change wide back to long, so that missing values still have a row in long dataset----
# >> self dataset----
selfl <- gather(data = selfw, key = "trait", value = "score",
                -ID)

selfl_sep <- selfl %>% 
  filter (trait != "bfas_withdrawal_w1" &
            trait != "bfas_withdrawal_w2" &
            trait != "bfas_withdrawal_w3" &
            trait != "bfas_withdrawal_w4") %>% 
  separate(col = trait,
           into = c("domain","time"),
           sep = "(\\[|_w|])")

selfl_sep2 <- selfl %>% 
  filter (trait == "bfas_withdrawal_w1" |
            trait == "bfas_withdrawal_w2" |
            trait == "bfas_withdrawal_w3" |
            trait == "bfas_withdrawal_w4") %>%
  separate(col = trait,
           into = c("domain", "time"),
           sep = "(\\[|l_w|])")

selfl_sep2$domain <- "bfas_withdrawal"
selfl <- rbind(selfl_sep, selfl_sep2)
selfl <- spread(data = selfl, key = domain, value = score)
selfl$time <- as.numeric(selfl$time)
selfl$time[selfl$time == 1] <- 0
selfl$time[selfl$time == 2] <- 6 
selfl$time[selfl$time == 3] <- 13
selfl$time[selfl$time == 4] <- 19

# >> peer dataset----
peerl <- gather(data = peerw, key = "trait", value = "score",
                -ID, - PeerID)

peerl_sep <- peerl %>% 
  filter (trait != "bfas_withdrawal_w1" &
            trait != "bfas_withdrawal_w2" &
            trait != "bfas_withdrawal_w3" &
            trait != "bfas_withdrawal_w4") %>% 
  separate(col = trait,
           into = c("domain","time"),
           sep = "(\\[|_w|])")

peerl_sep2 <- peerl %>% 
  filter (trait == "bfas_withdrawal_w1" |
            trait == "bfas_withdrawal_w2" |
            trait == "bfas_withdrawal_w3" |
            trait == "bfas_withdrawal_w4") %>%
  separate(col = trait,
           into = c("domain", "time"),
           sep = "(\\[|l_w|])")

peerl_sep2$domain <- "bfas_withdrawal"
peerl <- rbind(peerl_sep, peerl_sep2)
peerl <- spread(data = peerl, key = domain, value = score)
peerl$time <- as.numeric(peerl$time)
peerl$time[peerl$time == 1] <- 0
peerl$time[peerl$time == 2] <- 6 
peerl$time[peerl$time == 3] <- 13
peerl$time[peerl$time == 4] <- 19



# > Remove misc objects----
rm(list = c("self", "selfl_sep","selfl_sep2", "selfw1","selfw2","selfw3","selfw4",
            "peer", "peerl_sep","peerl_sep2", "peerw1","peerw2","peerw3","peerw4", "randomID"))

# DEMOGRAPHIC INFO ========
demo$race_f <- factor(demo$race_f, 
                      labels = c("white","black","asian","multiple","white non-europe","latino/a","other"))

summary(demo)

ggplot(data = demo,
       mapping = aes(age)) +
  geom_histogram(binwidth = 1) +
  theme_classic()
ggplot(data = demo,
       mapping = aes(gender)) +
  geom_histogram(binwidth = 1) +
  theme_classic()

# IMPUTATION ==========================================

# > Self imputation----
# >> Impute long dataset ----

#old code with mice
{
###create a dataset with all 20 imputed datasets
##long_imp <- complete(data = imp,
##                      action = "long",
##                      include = FALSE)
##
###delete .id column - unnecessary
##long_imp$.id <- NULL
##
###create a long dataset with all 20 imputed dataset
##{
##long_imp_bfasagree <- melt(long_imp,
##                            measure.vars = c("bfas_agreeableness_w1", "bfas_agreeableness_w2", "bfas_agreeableness_w3", ##"bfas_agreeableness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_agreeableness") %>% 
##  dplyr::select(.imp, ID, time, bfas_agreeableness)
##
##long_imp_bfasconsci <- melt(long_imp,
##                            measure.vars = c("bfas_conscientiousness_w1", "bfas_conscientiousness_w2", "bfas_conscientiousness_w3", ##"bfas_conscientiousness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_conscientiousness") %>% 
##  dplyr::select(.imp, ID, time, bfas_conscientiousness)
##
##long_imp_bfasextra <- melt(long_imp,
##                            measure.vars = c("bfas_extraversion_w1", "bfas_extraversion_w2", "bfas_extraversion_w3", ##"bfas_extraversion_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_extraversion") %>% 
##  dplyr::select(.imp, ID, time, bfas_extraversion)
##
##long_imp_bfasneuro <- melt(long_imp,
##                            measure.vars = c("bfas_neuroticism_w1", "bfas_neuroticism_w2", "bfas_neuroticism_w3", ##"bfas_neuroticism_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_neuroticism") %>% 
##  dplyr::select(.imp, ID, time, bfas_neuroticism)
##
##long_imp_bfasopendo <- melt(long_imp,
##                            measure.vars = c("bfas_opennessdomain_w1", "bfas_opennessdomain_w2", "bfas_opennessdomain_w3", ##"bfas_opennessdomain_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_opennessdomain") %>% 
##  dplyr::select(.imp, ID, time, bfas_opennessdomain)
##
##long_imp_bfasassert <- melt(long_imp,
##                            measure.vars = c("bfas_assertiveness_w1", "bfas_assertiveness_w2", "bfas_assertiveness_w3", ##"bfas_assertiveness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_assertiveness") %>% 
##  dplyr::select(.imp, ID, time, bfas_assertiveness)
##
##long_imp_bfascompa <- melt(long_imp,
##                            measure.vars = c("bfas_compassion_w1", "bfas_compassion_w2", "bfas_compassion_w3", "bfas_compassion_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_compassion") %>% 
##  dplyr::select(.imp, ID, time, bfas_compassion)
##
##long_imp_bfasenthu <- melt(long_imp,
##                            measure.vars = c("bfas_enthusiasm_w1", "bfas_enthusiasm_w2", "bfas_enthusiasm_w3", "bfas_enthusiasm_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_enthusiasm") %>% 
##  dplyr::select(.imp, ID, time, bfas_enthusiasm)
##
##long_imp_bfasindu <- melt(long_imp,
##                            measure.vars = c("bfas_industriousness_w1", "bfas_industriousness_w2", "bfas_industriousness_w3", ##"bfas_industriousness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_industriousness") %>% 
##  dplyr::select(.imp, ID, time, bfas_industriousness)
##
##long_imp_bfasinte <- melt(long_imp,
##                            measure.vars = c("bfas_intellect_w1", "bfas_intellect_w2", "bfas_intellect_w3", "bfas_intellect_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_intellect") %>% 
##  dplyr::select(.imp, ID, time, bfas_intellect)
##
##long_imp_bfasopenas <- melt(long_imp,
##                            measure.vars = c("bfas_opennessaspect_w1", "bfas_opennessaspect_w2", "bfas_opennessaspect_w3", ##"bfas_opennessaspect_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_opennessaspect") %>% 
##  dplyr::select(.imp, ID, time, bfas_opennessaspect)
##
##long_imp_bfasorder <- melt(long_imp,
##                            measure.vars = c("bfas_orderliness_w1", "bfas_orderliness_w2", "bfas_orderliness_w3", ##"bfas_orderliness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_orderliness") %>% 
##  dplyr::select(.imp, ID, time, bfas_orderliness)
##
##long_imp_bfaspoli <- melt(long_imp,
##                            measure.vars = c("bfas_politeness_w1", "bfas_politeness_w2", "bfas_politeness_w3", "bfas_politeness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_politeness") %>% 
##  dplyr::select(.imp, ID, time, bfas_politeness)
##
##long_imp_bfasvola <- melt(long_imp,
##                            measure.vars = c("bfas_volatility_w1", "bfas_volatility_w2", "bfas_volatility_w3", "bfas_volatility_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_volatility") %>% 
##  dplyr::select(.imp, ID, time, bfas_volatility)
##
##long_imp_bfaswith <- melt(long_imp,
##                            measure.vars = c("bfas_withdrawal_w1", "bfas_withdrawal_w2", "bfas_withdrawal_w3", "bfas_withdrawal_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_withdrawal") %>% 
##  dplyr::select(.imp, ID, time, bfas_withdrawal)
##
##long_imp_epsiconf <- melt(long_imp,
##                            measure.vars = c("epsi_confusion_w1", "epsi_confusion_w2", "epsi_confusion_w3", "epsi_confusion_w4"),
##                            variable.name = "time",
##                            value.name = "epsi_confusion") %>% 
##  dplyr::select(.imp, ID, time, epsi_confusion)
##
##long_imp_epsicohe <- melt(long_imp,
##                            measure.vars = c("epsi_coherence_w1", "epsi_coherence_w2", "epsi_coherence_w3", "epsi_coherence_w4"),
##                            variable.name = "time",
##                            value.name = "epsi_coherence") %>% 
##  dplyr::select(.imp, ID, time, epsi_coherence)
##
##long_imp_bfasagree$time <- plyr::revalue(long_imp_bfasagree$time, c("bfas_agreeableness_w1" = "0",
##                                                                      "bfas_agreeableness_w2" = "6",
##                                                                      "bfas_agreeableness_w3" = "13",
##                                                                      "bfas_agreeableness_w4" = "19"))
##long_imp_bfasconsci$time <- plyr::revalue(long_imp_bfasconsci$time, c("bfas_conscientiousness_w1" = "0",
##                                                                        "bfas_conscientiousness_w2" = "6",
##                                                                        "bfas_conscientiousness_w3" = "13",
##                                                                        "bfas_conscientiousness_w4" = "19"))
##long_imp_bfasextra$time <- plyr::revalue(long_imp_bfasextra$time, c("bfas_extraversion_w1" = "0",
##                                                                      "bfas_extraversion_w2" = "6",
##                                                                      "bfas_extraversion_w3" = "13",
##                                                                      "bfas_extraversion_w4" = "19"))
##long_imp_bfasneuro$time <- plyr::revalue(long_imp_bfasneuro$time, c("bfas_neuroticism_w1" = "0",
##                                                                      "bfas_neuroticism_w2" = "6",
##                                                                      "bfas_neuroticism_w3" = "13",
##                                                                      "bfas_neuroticism_w4" = "19"))
##long_imp_bfasopendo$time <- plyr::revalue(long_imp_bfasopendo$time, c("bfas_opennessdomain_w1" = "0",
##                                                                      "bfas_opennessdomain_w2" = "6",
##                                                                      "bfas_opennessdomain_w3" = "13",
##                                                                      "bfas_opennessdomain_w4" = "19"))
##long_imp_bfasassert$time <- plyr::revalue(long_imp_bfasassert$time, c("bfas_assertiveness_w1" = "0",
##                                                                        "bfas_assertiveness_w2" = "6",
##                                                                        "bfas_assertiveness_w3" = "13",
##                                                                        "bfas_assertiveness_w4" = "19"))
##long_imp_bfascompa$time <- plyr::revalue(long_imp_bfascompa$time, c("bfas_compassion_w1" = "0",
##                                                                      "bfas_compassion_w2" = "6",
##                                                                      "bfas_compassion_w3" = "13",
##                                                                      "bfas_compassion_w4" = "19"))
##long_imp_bfasenthu$time <- plyr::revalue(long_imp_bfasenthu$time, c("bfas_enthusiasm_w1" = "0",
##                                                                      "bfas_enthusiasm_w2" = "6",
##                                                                      "bfas_enthusiasm_w3" = "13",
##                                                                      "bfas_enthusiasm_w4" = "19"))
##long_imp_bfasindu$time <- plyr::revalue(long_imp_bfasindu$time, c("bfas_industriousness_w1" = "0",
##                                                                    "bfas_industriousness_w2" = "6",
##                                                                    "bfas_industriousness_w3" = "13",
##                                                                    "bfas_industriousness_w4" = "19"))
##long_imp_bfasinte$time <- plyr::revalue(long_imp_bfasinte$time, c("bfas_intellect_w1" = "0",
##                                                                    "bfas_intellect_w2" = "6",
##                                                                    "bfas_intellect_w3" = "13",
##                                                                    "bfas_intellect_w4" = "19"))
##long_imp_bfasopenas$time <- plyr::revalue(long_imp_bfasopenas$time, c("bfas_opennessaspect_w1" = "0",
##                                                                        "bfas_opennessaspect_w2" = "6",
##                                                                        "bfas_opennessaspect_w3" = "13",
##                                                                        "bfas_opennessaspect_w4" = "19"))
##long_imp_bfasorder$time <- plyr::revalue(long_imp_bfasorder$time, c("bfas_orderliness_w1" = "0",
##                                                                      "bfas_orderliness_w2" = "6",
##                                                                      "bfas_orderliness_w3" = "13",
##                                                                      "bfas_orderliness_w4" = "19"))
##long_imp_bfaspoli$time <- plyr::revalue(long_imp_bfaspoli$time, c("bfas_politeness_w1" = "0",
##                                                                    "bfas_politeness_w2" = "6",
##                                                                    "bfas_politeness_w3" = "13",
##                                                                    "bfas_politeness_w4" = "19"))
##long_imp_bfasvola$time <- plyr::revalue(long_imp_bfasvola$time, c("bfas_volatility_w1" = "0",
##                                                                    "bfas_volatility_w2" = "6",
##                                                                    "bfas_volatility_w3" = "13",
##                                                                    "bfas_volatility_w4" = "19"))
##long_imp_bfaswith$time <- plyr::revalue(long_imp_bfaswith$time, c("bfas_withdrawal_w1" = "0",
##                                                                    "bfas_withdrawal_w2" = "6",
##                                                                    "bfas_withdrawal_w3" = "13",
##                                                                    "bfas_withdrawal_w4" = "19"))
##long_imp_epsiconf$time <- plyr::revalue(long_imp_epsiconf$time, c("epsi_confusion_w1" = "0",
##                                                                    "epsi_confusion_w2" = "6",
##                                                                    "epsi_confusion_w3" = "13",
##                                                                    "epsi_confusion_w4" = "19"))
##long_imp_epsicohe$time <- plyr::revalue(long_imp_epsicohe$time, c("epsi_coherence_w1" = "0",
##                                                                    "epsi_coherence_w2" = "6",
##                                                                    "epsi_coherence_w3" = "13",
##                                                                    "epsi_coherence_w4" = "19"))
##selfl_imp <- merge(long_imp_bfasagree, long_imp_bfasconsci)
##selfl_imp <- merge(selfl_imp, long_imp_bfasextra)
##selfl_imp <- merge(selfl_imp, long_imp_bfasneuro)
##selfl_imp <- merge(selfl_imp, long_imp_bfasopendo)
##selfl_imp <- merge(selfl_imp, long_imp_bfasassert)
##selfl_imp <- merge(selfl_imp, long_imp_bfascompa)
##selfl_imp <- merge(selfl_imp, long_imp_bfasenthu)
##selfl_imp <- merge(selfl_imp, long_imp_bfasindu)
##selfl_imp <- merge(selfl_imp, long_imp_bfasinte)
##selfl_imp <- merge(selfl_imp, long_imp_bfasopenas)
##selfl_imp <- merge(selfl_imp, long_imp_bfasorder)
##selfl_imp <- merge(selfl_imp, long_imp_bfaspoli)
##selfl_imp <- merge(selfl_imp, long_imp_bfasvola)
##selfl_imp <- merge(selfl_imp, long_imp_bfaswith)
##selfl_imp <- merge(selfl_imp, long_imp_epsiconf)
##selfl_imp <- merge(selfl_imp, long_imp_epsicohe)
##
##rm(list = c("long_imp_bfasagree","long_imp_bfasconsci","long_imp_bfasextra","long_imp_bfasneuro","long_imp_bfasopendo"##,"long_imp_bfasassert",
##            "long_imp_bfascompa","long_imp_bfasenthu","long_imp_bfasindu","long_imp_bfasinte","long_imp_bfasopenas"##,"long_imp_bfasorder",
##            "long_imp_bfaspoli","long_imp_bfasvola","long_imp_bfaswith","long_imp_epsiconf","long_imp_epsicohe", "long_imp"))
##}
##
##selfl_imp$time <- as.numeric(selfl_imp$time)
##selfl_imp$time[selfl_imp$time == 1] <- 0
##selfl_imp$time[selfl_imp$time == 2] <- 6 
##selfl_imp$time[selfl_imp$time == 3] <- 13
##selfl_imp$time[selfl_imp$time == 4] <- 19
##
###create a column called .imp in original long dataset with 0
##selfl$.imp <- rep(0) 
##
###now have long dataset with 20 imputed + 1 original dataset
##selfl_imp <- rbind(selfl_imp, selfl) 
##
###create a column called .id in large long imputed dataset from 1:21350
##selfl_imp$.id <- rep(1:21756, 1)
##
###create mids object for long imputation to use with() and pool() functions later
##imp_long <- as.mids(long = selfl_imp, .imp = ".imp", .id = ".id")
##rm("selfl_imp")
}

imp_long <- panImpute(data = selfl, 
                              formula = bfas_agreeableness + bfas_assertiveness + bfas_compassion + bfas_conscientiousness + bfas_enthusiasm + bfas_extraversion + bfas_industriousness + bfas_intellect + bfas_neuroticism + bfas_opennessaspect + bfas_opennessdomain + bfas_orderliness + bfas_politeness + bfas_volatility + bfas_withdrawal + epsi_coherence + epsi_confusion  ~ time + (1+time|ID),
                              m = 20, seed = 20201028)
imp_long1 <- mitmlComplete(imp_long, print = 1)
imp_long2 <- mitmlComplete(imp_long, print = 2)
imp_long3 <- mitmlComplete(imp_long, print = 3)
imp_long4 <- mitmlComplete(imp_long, print = 4)
imp_long5 <- mitmlComplete(imp_long, print = 5)
imp_long6 <- mitmlComplete(imp_long, print = 6)
imp_long7 <- mitmlComplete(imp_long, print = 7)
imp_long8 <- mitmlComplete(imp_long, print = 8)
imp_long9 <- mitmlComplete(imp_long, print = 9)
imp_long10 <- mitmlComplete(imp_long, print = 10)
imp_long11 <- mitmlComplete(imp_long, print = 11)
imp_long12 <- mitmlComplete(imp_long, print = 12)
imp_long13 <- mitmlComplete(imp_long, print = 13)
imp_long14 <- mitmlComplete(imp_long, print = 14)
imp_long15 <- mitmlComplete(imp_long, print = 15)
imp_long16 <- mitmlComplete(imp_long, print = 16)
imp_long17 <- mitmlComplete(imp_long, print = 17)
imp_long18 <- mitmlComplete(imp_long, print = 18)
imp_long19 <- mitmlComplete(imp_long, print = 19)
imp_long20 <- mitmlComplete(imp_long, print = 20)

imp_long <- mitmlComplete(imp_long, print = 1:20)

# >> Impute wide dataset from long imputed dataset ----

#old code
{
###takes VERY LONG, load the saved RDS file instead
###imp <- selfw %>% 
###  dplyr::select(-ID)
###imp <- mice(data = selfw,
###            m = 20, maxit = 20,
###            print = FALSE,
###            seed = 20200618)
###saveRDS(imp, file = "self_imp_mids")
##
##imp <- readRDS("self_imp_mids")
##
###create 20 separate datasets from imputation
##selfw_1 <- complete(imp, 1)
##selfw_2 <- complete(imp, 2)
##selfw_3 <- complete(imp, 3)
##selfw_4 <- complete(imp, 4)
##selfw_5 <- complete(imp, 5)
##selfw_6 <- complete(imp, 6)
##selfw_7 <- complete(imp, 7)
##selfw_8 <- complete(imp, 8)
##selfw_9 <- complete(imp, 9)
##selfw_10 <- complete(imp, 10)
##selfw_11 <- complete(imp, 11)
##selfw_12 <- complete(imp, 12)
##selfw_13 <- complete(imp, 13)
##selfw_14 <- complete(imp, 14)
##selfw_15 <- complete(imp, 15)
##selfw_16 <- complete(imp, 16)
##selfw_17 <- complete(imp, 17)
##selfw_18 <- complete(imp, 18)
##selfw_19 <- complete(imp, 19)
##selfw_20 <- complete(imp, 20)
}

selfw_1 <- melt(imp_long1, id = c("ID","time")) 
selfw_1 <- dcast(selfw_1, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long1)[2:18]
names(selfw_1)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_2 <- melt(imp_long2, id = c("ID","time")) 
selfw_2 <- dcast(selfw_2, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long2)[2:18]
names(selfw_2)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_3 <- melt(imp_long3, id = c("ID","time")) 
selfw_3 <- dcast(selfw_3, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long3)[2:18]
names(selfw_3)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_4 <- melt(imp_long4, id = c("ID","time")) 
selfw_4 <- dcast(selfw_4, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long4)[2:18]
names(selfw_4)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_5 <- melt(imp_long5, id = c("ID","time")) 
selfw_5 <- dcast(selfw_5, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long5)[2:18]
names(selfw_5)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_6 <- melt(imp_long6, id = c("ID","time")) 
selfw_6 <- dcast(selfw_6, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long6)[2:18]
names(selfw_6)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_7 <- melt(imp_long7, id = c("ID","time")) 
selfw_7 <- dcast(selfw_7, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long7)[2:18]
names(selfw_7)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_8 <- melt(imp_long8, id = c("ID","time")) 
selfw_8 <- dcast(selfw_8, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long8)[2:18]
names(selfw_8)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_9 <- melt(imp_long9, id = c("ID","time")) 
selfw_9 <- dcast(selfw_9, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long9)[2:18]
names(selfw_9)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_10 <- melt(imp_long10, id = c("ID","time")) 
selfw_10 <- dcast(selfw_10, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long10)[2:18]
names(selfw_10)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_11 <- melt(imp_long11, id = c("ID","time")) 
selfw_11 <- dcast(selfw_11, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long11)[2:18]
names(selfw_11)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_12 <- melt(imp_long12, id = c("ID","time")) 
selfw_12 <- dcast(selfw_12, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long12)[2:18]
names(selfw_12)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_13 <- melt(imp_long13, id = c("ID","time")) 
selfw_13 <- dcast(selfw_13, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long13)[2:18]
names(selfw_13)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_14 <- melt(imp_long14, id = c("ID","time")) 
selfw_14 <- dcast(selfw_14, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long14)[2:18]
names(selfw_14)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_15 <- melt(imp_long15, id = c("ID","time")) 
selfw_15 <- dcast(selfw_15, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long15)[2:18]
names(selfw_15)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_16 <- melt(imp_long16, id = c("ID","time")) 
selfw_16 <- dcast(selfw_16, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long16)[2:18]
names(selfw_16)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_17 <- melt(imp_long17, id = c("ID","time")) 
selfw_17 <- dcast(selfw_17, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long17)[2:18]
names(selfw_17)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_18 <- melt(imp_long18, id = c("ID","time")) 
selfw_18 <- dcast(selfw_18, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long18)[2:18]
names(selfw_18)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_19 <- melt(imp_long19, id = c("ID","time")) 
selfw_19 <- dcast(selfw_19, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long19)[2:18]
names(selfw_19)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))

selfw_20 <- melt(imp_long20, id = c("ID","time")) 
selfw_20 <- dcast(selfw_20, ID ~ time + variable, value.var = "value")
names <- colnames(imp_long20)[2:18]
names(selfw_20)[2:69] <- paste0(names, "_w", c(rep(1:4, each = 17)))
rm("names")

## create a mitml.list object
selfw_1$imputation = 1
selfw_2$imputation = 2
selfw_3$imputation = 3
selfw_4$imputation = 4
selfw_5$imputation = 5
selfw_6$imputation = 6
selfw_7$imputation = 7
selfw_8$imputation = 8
selfw_9$imputation = 9
selfw_10$imputation = 10
selfw_11$imputation = 11
selfw_12$imputation = 12
selfw_13$imputation = 13
selfw_14$imputation = 14
selfw_15$imputation = 15
selfw_16$imputation = 16
selfw_17$imputation = 17
selfw_18$imputation = 18
selfw_19$imputation = 19
selfw_20$imputation = 20

imp_wide <- rbind(selfw_1, selfw_2, selfw_3, selfw_4, selfw_5, selfw_6, selfw_7, selfw_8, selfw_9, selfw_10,
                  selfw_11, selfw_12, selfw_13, selfw_14, selfw_15, selfw_16, selfw_17, selfw_18, selfw_19, selfw_20)

imp_wide <- split(imp_wide, imp_wide$imputation)
imp_wide <- as.mitml.list(imp_wide)


# > Peer imputation----
# >> Impute long dataset ----

#old code with mice
{
###create a dataset with all 20 imputed datasets
##long_imp_p <- complete(data = imp_p,
##                      action = "long",
##                      include = FALSE)
##
###delete .id column - unnecessary
##long_imp_p$.id <- NULL
##
###create a long dataset with all 20 imputed dataset
##{
##long_imp_p_bfasagree <- melt(long_imp_p,
##                            measure.vars = c("bfas_agreeableness_w1", "bfas_agreeableness_w2", "bfas_agreeableness_w3", ##"bfas_agreeableness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_agreeableness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_agreeableness)
##
##long_imp_p_bfasconsci <- melt(long_imp_p,
##                            measure.vars = c("bfas_conscientiousness_w1", "bfas_conscientiousness_w2", "bfas_conscientiousness_w3", ##"bfas_conscientiousness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_conscientiousness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_conscientiousness)
##
##long_imp_p_bfasextra <- melt(long_imp_p,
##                            measure.vars = c("bfas_extraversion_w1", "bfas_extraversion_w2", "bfas_extraversion_w3", ##"bfas_extraversion_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_extraversion") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_extraversion)
##
##long_imp_p_bfasneuro <- melt(long_imp_p,
##                            measure.vars = c("bfas_neuroticism_w1", "bfas_neuroticism_w2", "bfas_neuroticism_w3", ##"bfas_neuroticism_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_neuroticism") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_neuroticism)
##
##long_imp_p_bfasopendo <- melt(long_imp_p,
##                            measure.vars = c("bfas_opennessdomain_w1", "bfas_opennessdomain_w2", "bfas_opennessdomain_w3", ##"bfas_opennessdomain_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_opennessdomain") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_opennessdomain)
##
##long_imp_p_bfasassert <- melt(long_imp_p,
##                            measure.vars = c("bfas_assertiveness_w1", "bfas_assertiveness_w2", "bfas_assertiveness_w3", ##"bfas_assertiveness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_assertiveness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_assertiveness)
##
##long_imp_p_bfascompa <- melt(long_imp_p,
##                            measure.vars = c("bfas_compassion_w1", "bfas_compassion_w2", "bfas_compassion_w3", "bfas_compassion_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_compassion") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_compassion)
##
##long_imp_p_bfasenthu <- melt(long_imp_p,
##                            measure.vars = c("bfas_enthusiasm_w1", "bfas_enthusiasm_w2", "bfas_enthusiasm_w3", "bfas_enthusiasm_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_enthusiasm") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_enthusiasm)
##
##long_imp_p_bfasindu <- melt(long_imp_p,
##                            measure.vars = c("bfas_industriousness_w1", "bfas_industriousness_w2", "bfas_industriousness_w3", ##"bfas_industriousness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_industriousness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_industriousness)
##
##long_imp_p_bfasinte <- melt(long_imp_p,
##                            measure.vars = c("bfas_intellect_w1", "bfas_intellect_w2", "bfas_intellect_w3", "bfas_intellect_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_intellect") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_intellect)
##
##long_imp_p_bfasopenas <- melt(long_imp_p,
##                            measure.vars = c("bfas_opennessaspect_w1", "bfas_opennessaspect_w2", "bfas_opennessaspect_w3", ##"bfas_opennessaspect_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_opennessaspect") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_opennessaspect)
##
##long_imp_p_bfasorder <- melt(long_imp_p,
##                            measure.vars = c("bfas_orderliness_w1", "bfas_orderliness_w2", "bfas_orderliness_w3", ##"bfas_orderliness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_orderliness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_orderliness)
##
##long_imp_p_bfaspoli <- melt(long_imp_p,
##                            measure.vars = c("bfas_politeness_w1", "bfas_politeness_w2", "bfas_politeness_w3", "bfas_politeness_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_politeness") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_politeness)
##
##long_imp_p_bfasvola <- melt(long_imp_p,
##                            measure.vars = c("bfas_volatility_w1", "bfas_volatility_w2", "bfas_volatility_w3", "bfas_volatility_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_volatility") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_volatility)
##
##long_imp_p_bfaswith <- melt(long_imp_p,
##                            measure.vars = c("bfas_withdrawal_w1", "bfas_withdrawal_w2", "bfas_withdrawal_w3", "bfas_withdrawal_w4"),
##                            variable.name = "time",
##                            value.name = "bfas_withdrawal") %>% 
##  dplyr::select(.imp, ID, PeerID, time, bfas_withdrawal)
##
##long_imp_p_epsiconf <- melt(long_imp_p,
##                            measure.vars = c("epsi_confusion_w1", "epsi_confusion_w2", "epsi_confusion_w3", "epsi_confusion_w4"),
##                            variable.name = "time",
##                            value.name = "epsi_confusion") %>% 
##  dplyr::select(.imp, ID, PeerID, time, epsi_confusion)
##
##long_imp_p_epsicohe <- melt(long_imp_p,
##                            measure.vars = c("epsi_coherence_w1", "epsi_coherence_w2", "epsi_coherence_w3", "epsi_coherence_w4"),
##                            variable.name = "time",
##                            value.name = "epsi_coherence") %>% 
##  dplyr::select(.imp, ID, PeerID, time, epsi_coherence)
##
##long_imp_p_bfasagree$time <- plyr::revalue(long_imp_p_bfasagree$time, c("bfas_agreeableness_w1" = "0",
##                                                                      "bfas_agreeableness_w2" = "6",
##                                                                      "bfas_agreeableness_w3" = "13",
##                                                                      "bfas_agreeableness_w4" = "19"))
##long_imp_p_bfasconsci$time <- plyr::revalue(long_imp_p_bfasconsci$time, c("bfas_conscientiousness_w1" = "0",
##                                                                        "bfas_conscientiousness_w2" = "6",
##                                                                        "bfas_conscientiousness_w3" = "13",
##                                                                        "bfas_conscientiousness_w4" = "19"))
##long_imp_p_bfasextra$time <- plyr::revalue(long_imp_p_bfasextra$time, c("bfas_extraversion_w1" = "0",
##                                                                      "bfas_extraversion_w2" = "6",
##                                                                      "bfas_extraversion_w3" = "13",
##                                                                      "bfas_extraversion_w4" = "19"))
##long_imp_p_bfasneuro$time <- plyr::revalue(long_imp_p_bfasneuro$time, c("bfas_neuroticism_w1" = "0",
##                                                                      "bfas_neuroticism_w2" = "6",
##                                                                      "bfas_neuroticism_w3" = "13",
##                                                                      "bfas_neuroticism_w4" = "19"))
##long_imp_p_bfasopendo$time <- plyr::revalue(long_imp_p_bfasopendo$time, c("bfas_opennessdomain_w1" = "0",
##                                                                      "bfas_opennessdomain_w2" = "6",
##                                                                      "bfas_opennessdomain_w3" = "13",
##                                                                      "bfas_opennessdomain_w4" = "19"))
##long_imp_p_bfasassert$time <- plyr::revalue(long_imp_p_bfasassert$time, c("bfas_assertiveness_w1" = "0",
##                                                                        "bfas_assertiveness_w2" = "6",
##                                                                        "bfas_assertiveness_w3" = "13",
##                                                                        "bfas_assertiveness_w4" = "19"))
##long_imp_p_bfascompa$time <- plyr::revalue(long_imp_p_bfascompa$time, c("bfas_compassion_w1" = "0",
##                                                                      "bfas_compassion_w2" = "6",
##                                                                      "bfas_compassion_w3" = "13",
##                                                                      "bfas_compassion_w4" = "19"))
##long_imp_p_bfasenthu$time <- plyr::revalue(long_imp_p_bfasenthu$time, c("bfas_enthusiasm_w1" = "0",
##                                                                      "bfas_enthusiasm_w2" = "6",
##                                                                      "bfas_enthusiasm_w3" = "13",
##                                                                      "bfas_enthusiasm_w4" = "19"))
##long_imp_p_bfasindu$time <- plyr::revalue(long_imp_p_bfasindu$time, c("bfas_industriousness_w1" = "0",
##                                                                    "bfas_industriousness_w2" = "6",
##                                                                    "bfas_industriousness_w3" = "13",
##                                                                    "bfas_industriousness_w4" = "19"))
##long_imp_p_bfasinte$time <- plyr::revalue(long_imp_p_bfasinte$time, c("bfas_intellect_w1" = "0",
##                                                                    "bfas_intellect_w2" = "6",
##                                                                    "bfas_intellect_w3" = "13",
##                                                                    "bfas_intellect_w4" = "19"))
##long_imp_p_bfasopenas$time <- plyr::revalue(long_imp_p_bfasopenas$time, c("bfas_opennessaspect_w1" = "0",
##                                                                        "bfas_opennessaspect_w2" = "6",
##                                                                        "bfas_opennessaspect_w3" = "13",
##                                                                        "bfas_opennessaspect_w4" = "19"))
##long_imp_p_bfasorder$time <- plyr::revalue(long_imp_p_bfasorder$time, c("bfas_orderliness_w1" = "0",
##                                                                      "bfas_orderliness_w2" = "6",
##                                                                      "bfas_orderliness_w3" = "13",
##                                                                      "bfas_orderliness_w4" = "19"))
##long_imp_p_bfaspoli$time <- plyr::revalue(long_imp_p_bfaspoli$time, c("bfas_politeness_w1" = "0",
##                                                                    "bfas_politeness_w2" = "6",
##                                                                    "bfas_politeness_w3" = "13",
##                                                                    "bfas_politeness_w4" = "19"))
##long_imp_p_bfasvola$time <- plyr::revalue(long_imp_p_bfasvola$time, c("bfas_volatility_w1" = "0",
##                                                                    "bfas_volatility_w2" = "6",
##                                                                    "bfas_volatility_w3" = "13",
##                                                                    "bfas_volatility_w4" = "19"))
##long_imp_p_bfaswith$time <- plyr::revalue(long_imp_p_bfaswith$time, c("bfas_withdrawal_w1" = "0",
##                                                                    "bfas_withdrawal_w2" = "6",
##                                                                    "bfas_withdrawal_w3" = "13",
##                                                                    "bfas_withdrawal_w4" = "19"))
##long_imp_p_epsiconf$time <- plyr::revalue(long_imp_p_epsiconf$time, c("epsi_confusion_w1" = "0",
##                                                                    "epsi_confusion_w2" = "6",
##                                                                    "epsi_confusion_w3" = "13",
##                                                                    "epsi_confusion_w4" = "19"))
##long_imp_p_epsicohe$time <- plyr::revalue(long_imp_p_epsicohe$time, c("epsi_coherence_w1" = "0",
##                                                                    "epsi_coherence_w2" = "6",
##                                                                    "epsi_coherence_w3" = "13",
##                                                                    "epsi_coherence_w4" = "19"))
##peerl_imp <- merge(long_imp_p_bfasagree, long_imp_p_bfasconsci)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasextra)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasneuro)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasopendo)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasassert)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfascompa)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasenthu)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasindu)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasinte)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasopenas)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasorder)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfaspoli)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfasvola)
##peerl_imp <- merge(peerl_imp, long_imp_p_bfaswith)
##peerl_imp <- merge(peerl_imp, long_imp_p_epsiconf)
##peerl_imp <- merge(peerl_imp, long_imp_p_epsicohe)
##
##rm(list = c("long_imp_p_bfasagree","long_imp_p_bfasconsci","long_imp_p_bfasextra","long_imp_p_bfasneuro","long_imp_p_bfasopendo"##,"long_imp_p_bfasassert",
##            "long_imp_p_bfascompa","long_imp_p_bfasenthu","long_imp_p_bfasindu","long_imp_p_bfasinte","long_imp_p_bfasopenas"##,"long_imp_p_bfasorder",
##            "long_imp_p_bfaspoli","long_imp_p_bfasvola","long_imp_p_bfaswith","long_imp_p_epsiconf","long_imp_p_epsicohe", ##"long_imp_p"))
##}
##
##peerl_imp$time <- as.numeric(peerl_imp$time)
##peerl_imp$time[peerl_imp$time == 1] <- 0
##peerl_imp$time[peerl_imp$time == 2] <- 6 
##peerl_imp$time[peerl_imp$time == 3] <- 13
##peerl_imp$time[peerl_imp$time == 4] <- 19
##
###create a column called .imp in original long dataset with 0
##peerl$.imp <- rep(0) 
##
###now have long dataset with 20 imputed + 1 original dataset
##peerl_imp <- rbind(peerl_imp, peerl) 
##
###create a column called .id in large long imputed dataset from 1:21350
##peerl_imp$.id <- rep(1:14784, 1)
##
###create mids object for long imputation to use with() and pool() functions later
##imp_long_p <- as.mids(long = peerl_imp, .imp = ".imp", .id = ".id")
##rm("peerl_imp")
}

imp_long_p <- panImpute(data = peerl, 
                              formula = bfas_agreeableness + bfas_assertiveness + bfas_compassion + bfas_conscientiousness + bfas_enthusiasm + bfas_extraversion + bfas_industriousness + bfas_intellect + bfas_neuroticism + bfas_opennessaspect + bfas_opennessdomain + bfas_orderliness + bfas_politeness + bfas_volatility + bfas_withdrawal + epsi_coherence + epsi_confusion  ~ time + (1+time|ID),
                              m = 20, seed = 20201028)

imp_long_p1 <- mitmlComplete(imp_long_p, print = 1)
imp_long_p2 <- mitmlComplete(imp_long_p, print = 2)
imp_long_p3 <- mitmlComplete(imp_long_p, print = 3)
imp_long_p4 <- mitmlComplete(imp_long_p, print = 4)
imp_long_p5 <- mitmlComplete(imp_long_p, print = 5)
imp_long_p6 <- mitmlComplete(imp_long_p, print = 6)
imp_long_p7 <- mitmlComplete(imp_long_p, print = 7)
imp_long_p8 <- mitmlComplete(imp_long_p, print = 8)
imp_long_p9 <- mitmlComplete(imp_long_p, print = 9)
imp_long_p10 <- mitmlComplete(imp_long_p, print = 10)
imp_long_p11 <- mitmlComplete(imp_long_p, print = 11)
imp_long_p12 <- mitmlComplete(imp_long_p, print = 12)
imp_long_p13 <- mitmlComplete(imp_long_p, print = 13)
imp_long_p14 <- mitmlComplete(imp_long_p, print = 14)
imp_long_p15 <- mitmlComplete(imp_long_p, print = 15)
imp_long_p16 <- mitmlComplete(imp_long_p, print = 16)
imp_long_p17 <- mitmlComplete(imp_long_p, print = 17)
imp_long_p18 <- mitmlComplete(imp_long_p, print = 18)
imp_long_p19 <- mitmlComplete(imp_long_p, print = 19)
imp_long_p20 <- mitmlComplete(imp_long_p, print = 20)


imp_long_p <- mitmlComplete(imp_long_p, print = 1:20)

# >> Impute wide dataset from long imputed dataset ----

#old code with mice
{
###takes VERY LONG, load the saved RDS file instead
###imp_p <- peerw %>% 
###  dplyr::select(-ID, -PeerID)
###imp_p <- mice(data = peerw,
###            m = 20, maxit = 20,
###            print = FALSE,
###            seed = 20200819)
###saveRDS(imp_p, file = "peer_imp_mids")
##imp_p <- readRDS("peer_imp_mids")
##
###create 20 separate datasets from imputation
##peerw_1 <- complete(imp_p, 1)
##peerw_2 <- complete(imp_p, 2)
##peerw_3 <- complete(imp_p, 3)
##peerw_4 <- complete(imp_p, 4)
##peerw_5 <- complete(imp_p, 5)
##peerw_6 <- complete(imp_p, 6)
##peerw_7 <- complete(imp_p, 7)
##peerw_8 <- complete(imp_p, 8)
##peerw_9 <- complete(imp_p, 9)
##peerw_10 <- complete(imp_p, 10)
##peerw_11 <- complete(imp_p, 11)
##peerw_12 <- complete(imp_p, 12)
##peerw_13 <- complete(imp_p, 13)
##peerw_14 <- complete(imp_p, 14)
##peerw_15 <- complete(imp_p, 15)
##peerw_16 <- complete(imp_p, 16)
##peerw_17 <- complete(imp_p, 17)
##peerw_18 <- complete(imp_p, 18)
##peerw_19 <- complete(imp_p, 19)
##peerw_20 <- complete(imp_p, 20)
}

peerw_1 <- melt(imp_long_p1, id = c("ID","PeerID","time")) 
peerw_1 <- dcast(peerw_1, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p1)[2:18]
names(peerw_1)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_2 <- melt(imp_long_p2, id = c("ID","PeerID","time")) 
peerw_2 <- dcast(peerw_2, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p2)[2:18]
names(peerw_2)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_3 <- melt(imp_long_p3, id = c("ID","PeerID","time")) 
peerw_3 <- dcast(peerw_3, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p3)[2:18]
names(peerw_3)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_4 <- melt(imp_long_p4, id = c("ID","PeerID","time")) 
peerw_4 <- dcast(peerw_4, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p4)[2:18]
names(peerw_4)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_5 <- melt(imp_long_p5, id = c("ID","PeerID","time")) 
peerw_5 <- dcast(peerw_5, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p5)[2:18]
names(peerw_5)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_6 <- melt(imp_long_p6, id = c("ID","PeerID","time")) 
peerw_6 <- dcast(peerw_6, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p6)[2:18]
names(peerw_6)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_7 <- melt(imp_long_p7, id = c("ID","PeerID","time")) 
peerw_7 <- dcast(peerw_7, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p7)[2:18]
names(peerw_7)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_8 <- melt(imp_long_p8, id = c("ID","PeerID","time")) 
peerw_8 <- dcast(peerw_8, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p8)[2:18]
names(peerw_8)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_9 <- melt(imp_long_p9, id = c("ID","PeerID","time")) 
peerw_9 <- dcast(peerw_9, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p9)[2:18]
names(peerw_9)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_10 <- melt(imp_long_p10, id = c("ID","PeerID","time")) 
peerw_10 <- dcast(peerw_10, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p10)[2:18]
names(peerw_10)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_11 <- melt(imp_long_p11, id = c("ID","PeerID","time")) 
peerw_11 <- dcast(peerw_11, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p11)[2:18]
names(peerw_11)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_12 <- melt(imp_long_p12, id = c("ID","PeerID","time")) 
peerw_12 <- dcast(peerw_12, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p12)[2:18]
names(peerw_12)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_13 <- melt(imp_long_p13, id = c("ID","PeerID","time")) 
peerw_13 <- dcast(peerw_13, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p13)[2:18]
names(peerw_13)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_14 <- melt(imp_long_p14, id = c("ID","PeerID","time")) 
peerw_14 <- dcast(peerw_14, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p14)[2:18]
names(peerw_14)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_15 <- melt(imp_long_p15, id = c("ID","PeerID","time")) 
peerw_15 <- dcast(peerw_15, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p15)[2:18]
names(peerw_15)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_16 <- melt(imp_long_p16, id = c("ID","PeerID","time")) 
peerw_16 <- dcast(peerw_16, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p16)[2:18]
names(peerw_16)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_17 <- melt(imp_long_p17, id = c("ID","PeerID","time")) 
peerw_17 <- dcast(peerw_17, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p17)[2:18]
names(peerw_17)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_18 <- melt(imp_long_p18, id = c("ID","PeerID","time")) 
peerw_18 <- dcast(peerw_18, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p18)[2:18]
names(peerw_18)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_19 <- melt(imp_long_p19, id = c("ID","PeerID","time")) 
peerw_19 <- dcast(peerw_19, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p19)[2:18]
names(peerw_19)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))

peerw_20 <- melt(imp_long_p20, id = c("ID","PeerID","time")) 
peerw_20 <- dcast(peerw_20, ID + PeerID~ time + variable, value.var = "value")
names <- colnames(imp_long_p20)[2:18]
names(peerw_20)[3:70] <- paste0(names, "_w", c(rep(1:4, each = 17)))
rm("names")

## create a mitml.list object
peerw_1$imputation = 1
peerw_2$imputation = 2
peerw_3$imputation = 3
peerw_4$imputation = 4
peerw_5$imputation = 5
peerw_6$imputation = 6
peerw_7$imputation = 7
peerw_8$imputation = 8
peerw_9$imputation = 9
peerw_10$imputation = 10
peerw_11$imputation = 11
peerw_12$imputation = 12
peerw_13$imputation = 13
peerw_14$imputation = 14
peerw_15$imputation = 15
peerw_16$imputation = 16
peerw_17$imputation = 17
peerw_18$imputation = 18
peerw_19$imputation = 19
peerw_20$imputation = 20

imp_wide_p <- rbind(peerw_1, peerw_2, peerw_3, peerw_4, peerw_5, peerw_6, peerw_7, peerw_8, peerw_9, peerw_10,
                  peerw_11, peerw_12, peerw_13, peerw_14, peerw_15, peerw_16, peerw_17, peerw_18, peerw_19, peerw_20)

imp_wide_p <- split(imp_wide_p, imp_wide_p$imputation)
imp_wide_p <- as.mitml.list(imp_wide_p)

# ANALYSIS SELF  ==========================================

# > SELF 1a: Mean level change personality ----
# >> Linear mixed model with random intercept and random slope ----

### Agreeableness
linear.agree <- lmer(bfas_agreeableness ~ time +
                            (1 + time | ID),
                          control = lmerControl(optimizer ="Nelder_Mead"),
                          data = selfl)
summary(linear.agree)

### Assertiveness
linear.assert <- lmer(bfas_assertiveness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.assert)

### Compassion
linear.compa <- lmer(bfas_compassion ~ time +
                        (1 + time | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = selfl)
summary(linear.compa)

### Conscientiousness
linear.consci <- lmer(bfas_conscientiousness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.consci)

### Enthusiasm
linear.enthu <- lmer(bfas_enthusiasm ~ time +
                        (1 + time | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = selfl)
summary(linear.enthu)

### Extraversion
linear.extra <- lmer(bfas_extraversion ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.extra)

### Industriousness 
linear.indus <- lmer(bfas_industriousness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.indus)

### Intellect
linear.intel <- lmer(bfas_intellect ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.intel)

### Neuroticism
linear.neuro <- lmer(bfas_neuroticism ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.neuro)

### Openness Aspect
linear.opena <- lmer(bfas_opennessaspect ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.opena)

### Openness Domain
linear.opend <- lmer(bfas_opennessdomain ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.opend)

### Orderlines
linear.order <- lmer(bfas_orderliness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.order)

### Politeness
linear.polit <- lmer(bfas_politeness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.polit)

### Volatility
linear.volat <- lmer(bfas_volatility ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.volat)

### Withdrawal
linear.withd <- lmer(bfas_withdrawal ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.withd)


# >> MI - Linear mixed model random intercept and random slope ----
linear.imp.agree <- with(data = imp_long,
                         exp = lme4::lmer(bfas_agreeableness ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.agree, var.comp=TRUE)

linear.imp.assert <- with(data = imp_long,
                          exp = lme4::lmer(bfas_assertiveness ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.assert, var.comp=TRUE)

linear.imp.compa <- with(data = imp_long,
                          exp = lme4::lmer(bfas_compassion ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.compa, var.comp=TRUE)

linear.imp.consci <- with(data = imp_long,
                         exp = lme4::lmer(bfas_conscientiousness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.consci, var.comp=TRUE)

linear.imp.enthu <- with(data = imp_long,
                          exp = lme4::lmer(bfas_enthusiasm ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.enthu, var.comp=TRUE)

linear.imp.extra <- with(data = imp_long,
                         exp = lme4::lmer(bfas_extraversion ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.extra, var.comp=TRUE)

linear.imp.indus <- with(data = imp_long,
                         exp = lme4::lmer(bfas_industriousness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.indus, var.comp=TRUE)

linear.imp.intel <- with(data = imp_long,
                         exp = lme4::lmer(bfas_intellect ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.intel, var.comp=TRUE)

linear.imp.neuro <- with(data = imp_long,
                         exp = lme4::lmer(bfas_neuroticism ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.neuro, var.comp=TRUE)

linear.imp.opena <- with(data = imp_long,
                         exp = lme4::lmer(bfas_opennessaspect ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.opena, var.comp=TRUE)

linear.imp.opend <- with(data = imp_long,
                         exp = lme4::lmer(bfas_opennessdomain ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.opend, var.comp=TRUE)

linear.imp.order <- with(data = imp_long,
                         exp = lme4::lmer(bfas_orderliness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.order, var.comp=TRUE)

linear.imp.polit <- with(data = imp_long,
                         exp = lme4::lmer(bfas_politeness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.polit, var.comp=TRUE)

linear.imp.volat <- with(data = imp_long,
                         exp = lme4::lmer(bfas_volatility ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.volat, var.comp=TRUE)

linear.imp.withd <- with(data = imp_long,
                         exp = lme4::lmer(bfas_withdrawal ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.withd, var.comp=TRUE)

# >> Quadratic mixed model with random intercept and random slope, no random quad slope ----

### Agreeableness
quad.agree <- lmer(bfas_agreeableness ~ poly(time, degree = 2, raw = TRUE) +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(quad.agree)

### Assertiveness
quad.assert <- lmer(bfas_assertiveness ~ poly(time, degree = 2, raw = TRUE) +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.assert)

### Compassion
quad.compa <- lmer(bfas_compassion ~ poly(time, degree = 2, raw = TRUE)  +
                      (1 + time | ID),
                    control = lmerControl(optimizer ="Nelder_Mead"),
                    data = selfl)
summary(quad.compa)

### Conscientiousness
quad.consci <- lmer(bfas_conscientiousness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.consci)

### Enthusiasm
quad.enthu <- lmer(bfas_enthusiasm ~ poly(time, degree = 2, raw = TRUE)  +
                      (1 + time | ID),
                    control = lmerControl(optimizer ="Nelder_Mead"),
                    data = selfl)
summary(quad.enthu)

### Extraversion
quad.extra <- lmer(bfas_extraversion ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.extra)

### Industriousness
quad.indus <- lmer(bfas_industriousness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.indus)

### Intellect
quad.intel <- lmer(bfas_intellect ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.intel)

### Neuroticism
quad.neuro <- lmer(bfas_neuroticism ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.neuro)

### Openness Aspect
quad.opena <- lmer(bfas_opennessaspect ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.opena)

### Openness Domain
quad.opend <- lmer(bfas_opennessdomain ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.opend)

### Oderliness
quad.order <- lmer(bfas_orderliness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.order)

### Politeness
quad.polit <- lmer(bfas_politeness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.polit)

### Volatility
quad.volat <- lmer(bfas_volatility ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.volat)

### Withdrawal
quad.withd <- lmer(bfas_withdrawal ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.withd)

# >> Model comparison (using original data) ----

anova(linear.agree, quad.agree)
anova(linear.assert, quad.assert) #quad >
anova(linear.compa, quad.compa) #quad >
anova(linear.consci, quad.consci)
anova(linear.enthu, quad.enthu) #quad >
anova(linear.extra, quad.extra) #quad >
anova(linear.indus, quad.indus)
anova(linear.intel, quad.intel) #quad >
anova(linear.neuro, quad.neuro)
anova(linear.opena, quad.opena)
anova(linear.opend, quad.opend) #quad >
anova(linear.order, quad.order)
anova(linear.polit, quad.polit)
anova(linear.volat, quad.volat)
anova(linear.withd, quad.withd)


# >> Linear mixed model with random intercept and no random slope ----

### Agreeableness
linearint.imp.agree <- with(data = imp_long,
                     exp = lme4::lmer(bfas_agreeableness ~ time + 
                                        (1| ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))

summary(pool(linearint.imp.agree))

linearint.agree <- lmer(bfas_agreeableness ~ time +
                            (1 | ID),
                          control = lmerControl(optimizer ="Nelder_Mead"),
                          data = selfl)
summary(linearint.agree)

### Assertiveness
linearint.imp.assert <- with(data = imp_long,
                          exp = lme4::lmer(bfas_assertiveness ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.assert))

linearint.assert <- lmer(bfas_assertiveness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.assert)

### Compassion
linearint.imp.compa <- with(data = imp_long,
                          exp = lme4::lmer(bfas_compassion ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.compa))

linearint.compa <- lmer(bfas_compassion ~ time +
                        (1 | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = selfl)
summary(linearint.compa)

### Conscientiousness
linearint.imp.consci <- with(data = imp_long,
                         exp = lme4::lmer(bfas_conscientiousness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.consci))

linearint.consci <- lmer(bfas_conscientiousness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.consci)

### Enthusiasm
linearint.imp.enthu <- with(data = imp_long,
                          exp = lme4::lmer(bfas_enthusiasm ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.enthu))

linearint.enthu <- lmer(bfas_enthusiasm ~ time +
                        (1 | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = selfl)
summary(linearint.enthu)

### Extraversion
linearint.imp.extra <- with(data = imp_long,
                         exp = lme4::lmer(bfas_extraversion ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.extra))

linearint.extra <- lmer(bfas_extraversion ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.extra)

### Industriousness 
linearint.imp.indus <- with(data = imp_long,
                         exp = lme4::lmer(bfas_industriousness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.indus))

linearint.indus <- lmer(bfas_industriousness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.indus)

### Intellect
linearint.imp.intel <- with(data = imp_long,
                         exp = lme4::lmer(bfas_intellect ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.intel))

linearint.intel <- lmer(bfas_intellect ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.intel)

### Neuroticism
linearint.imp.neuro <- with(data = imp_long,
                         exp = lme4::lmer(bfas_neuroticism ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.neuro))

linearint.neuro <- lmer(bfas_neuroticism ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.neuro)

### Openness Aspect
linearint.imp.opena <- with(data = imp_long,
                         exp = lme4::lmer(bfas_opennessaspect ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.opena))

linearint.opena <- lmer(bfas_opennessaspect ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.opena)

### Openness Domain
linearint.imp.opend <- with(data = imp_long,
                         exp = lme4::lmer(bfas_opennessdomain ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.opend))

linearint.opend <- lmer(bfas_opennessdomain ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.opend)

### Orderlines
linearint.imp.order <- with(data = imp_long,
                         exp = lme4::lmer(bfas_orderliness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.order))

linearint.order <- lmer(bfas_orderliness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.order)

### Politeness
linearint.imp.polit <- with(data = imp_long,
                         exp = lme4::lmer(bfas_politeness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.polit))

linearint.polit <- lmer(bfas_politeness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.polit)

### Volatility
linearint.imp.volat <- with(data = imp_long,
                         exp = lme4::lmer(bfas_volatility ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.volat))

linearint.volat <- lmer(bfas_volatility ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.volat)

### Withdrawal
linearint.imp.withd <- with(data = imp_long,
                         exp = lme4::lmer(bfas_withdrawal ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.withd))

linearint.withd <- lmer(bfas_withdrawal ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.withd)



# > SELF 1b: Mean level change identity ----
# >> Linear mixed model with random intercept and random slope ----

linear.coher <- lmer(epsi_coherence ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.coher)

linear.confu <- lmer(epsi_confusion ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linear.confu)

# >> MI - Linear mixed model with random intercept and random slope ----
linear.imp.coher <- with(data = imp_long,
                         exp = lme4::lmer(epsi_coherence ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.coher, var.comp=TRUE)

linear.imp.confu <- with(data = imp_long,
                         exp = lme4::lmer(epsi_confusion ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.confu, var.comp=TRUE)

# >> Quadratic mixed model with random intercept and random slope, no random quad slope ----
quad.coher <- lmer(epsi_coherence ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.coher)

quad.confu <- lmer(epsi_confusion ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = selfl)
summary(quad.confu)

# >> Model comparison (using original data) ----

anova(linear.coher, quad.coher)
anova(linear.confu, quad.confu)

# >> Linear mixed model with random intercept and no random slope ----

linearint.imp.coher <- with(data = imp_long,
                         exp = lme4::lmer(epsi_coherence ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.coher))

linearint.coher <- lmer(epsi_coherence ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.coher)

linearint.imp.confu <- with(data = imp_long,
                         exp = lme4::lmer(epsi_confusion ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.confu))

linearint.confu <- lmer(epsi_confusion ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = selfl)
summary(linearint.confu)



# > SELF 2a: Rank order change personality ----

# >> Compute correlation fisher z ----

### correlation between T1 and T2
r12_agree <- cor(selfw$bfas_agreeableness_w1, selfw$bfas_agreeableness_w2, use = "pairwise.complete.obs")
r12_consci <- cor(selfw$bfas_conscientiousness_w1, selfw$bfas_conscientiousness_w2, use = "pairwise.complete.obs")
r12_extra <- cor(selfw$bfas_extraversion_w1, selfw$bfas_extraversion_w2, use = "pairwise.complete.obs")
r12_neuro <- cor(selfw$bfas_neuroticism_w1, selfw$bfas_neuroticism_w2, use = "pairwise.complete.obs")
r12_opend <- cor(selfw$bfas_opennessdomain_w1, selfw$bfas_opennessdomain_w2, use = "pairwise.complete.obs")
r12_assert <- cor(selfw$bfas_assertiveness_w1, selfw$bfas_assertiveness_w2, use = "pairwise.complete.obs")
r12_compa <- cor(selfw$bfas_compassion_w1, selfw$bfas_compassion_w2, use = "pairwise.complete.obs")
r12_enthu <- cor(selfw$bfas_enthusiasm_w1, selfw$bfas_enthusiasm_w2, use = "pairwise.complete.obs")
r12_indus <- cor(selfw$bfas_industriousness_w1, selfw$bfas_industriousness_w2, use = "pairwise.complete.obs")
r12_intel <- cor(selfw$bfas_intellect_w1, selfw$bfas_intellect_w2, use = "pairwise.complete.obs")
r12_opena <- cor(selfw$bfas_opennessaspect_w1, selfw$bfas_opennessaspect_w2, use = "pairwise.complete.obs")
r12_order <- cor(selfw$bfas_orderliness_w1, selfw$bfas_orderliness_w2, use = "pairwise.complete.obs")
r12_polit <- cor(selfw$bfas_politeness_w1, selfw$bfas_politeness_w2, use = "pairwise.complete.obs")
r12_volat <- cor(selfw$bfas_volatility_w1, selfw$bfas_volatility_w2, use = "pairwise.complete.obs")
r12_withd <- cor(selfw$bfas_withdrawal_w1, selfw$bfas_withdrawal_w2, use = "pairwise.complete.obs")

z12_agree <- fisherz(r12_agree)
z12_consci <- fisherz(r12_consci)
z12_extra <- fisherz(r12_extra)
z12_neuro <- fisherz(r12_neuro)
z12_opend <- fisherz(r12_opend)
z12_assert <- fisherz(r12_assert)
z12_compa <- fisherz(r12_compa)
z12_enthu <- fisherz(r12_enthu)
z12_indus <- fisherz(r12_indus)
z12_intel <- fisherz(r12_intel)
z12_opena <- fisherz(r12_opena)
z12_order <- fisherz(r12_order)
z12_polit <- fisherz(r12_polit)
z12_volat <- fisherz(r12_volat)
z12_withd <- fisherz(r12_withd)

### correlation between T2 and T3
r23_agree <- cor(selfw$bfas_agreeableness_w2, selfw$bfas_agreeableness_w3, use = "pairwise.complete.obs")
r23_consci <- cor(selfw$bfas_conscientiousness_w2, selfw$bfas_conscientiousness_w3, use = "pairwise.complete.obs")
r23_extra <- cor(selfw$bfas_extraversion_w2, selfw$bfas_extraversion_w3, use = "pairwise.complete.obs")
r23_neuro <- cor(selfw$bfas_neuroticism_w2, selfw$bfas_neuroticism_w3, use = "pairwise.complete.obs")
r23_opend <- cor(selfw$bfas_opennessdomain_w2, selfw$bfas_opennessdomain_w3, use = "pairwise.complete.obs")
r23_assert <- cor(selfw$bfas_assertiveness_w2, selfw$bfas_assertiveness_w3, use = "pairwise.complete.obs")
r23_compa <- cor(selfw$bfas_compassion_w2, selfw$bfas_compassion_w3, use = "pairwise.complete.obs")
r23_enthu <- cor(selfw$bfas_enthusiasm_w2, selfw$bfas_enthusiasm_w3, use = "pairwise.complete.obs")
r23_indus <- cor(selfw$bfas_industriousness_w2, selfw$bfas_industriousness_w3, use = "pairwise.complete.obs")
r23_intel <- cor(selfw$bfas_intellect_w2, selfw$bfas_intellect_w3, use = "pairwise.complete.obs")
r23_opena <- cor(selfw$bfas_opennessaspect_w2, selfw$bfas_opennessaspect_w3, use = "pairwise.complete.obs")
r23_order <- cor(selfw$bfas_orderliness_w2, selfw$bfas_orderliness_w3, use = "pairwise.complete.obs")
r23_polit <- cor(selfw$bfas_politeness_w2, selfw$bfas_politeness_w3, use = "pairwise.complete.obs")
r23_volat <- cor(selfw$bfas_volatility_w2, selfw$bfas_volatility_w3, use = "pairwise.complete.obs")
r23_withd <- cor(selfw$bfas_withdrawal_w2, selfw$bfas_withdrawal_w3, use = "pairwise.complete.obs")

z23_agree <- fisherz(r23_agree)
z23_consci <- fisherz(r23_consci)
z23_extra <- fisherz(r23_extra)
z23_neuro <- fisherz(r23_neuro)
z23_opend <- fisherz(r23_opend)
z23_assert <- fisherz(r23_assert)
z23_compa <- fisherz(r23_compa)
z23_enthu <- fisherz(r23_enthu)
z23_indus <- fisherz(r23_indus)
z23_intel <- fisherz(r23_intel)
z23_opena <- fisherz(r23_opena)
z23_order <- fisherz(r23_order)
z23_polit <- fisherz(r23_polit)
z23_volat <- fisherz(r23_volat)
z23_withd <- fisherz(r23_withd)

### correlation between T3 and T4
r34_agree <- cor(selfw$bfas_agreeableness_w3, selfw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r34_consci <- cor(selfw$bfas_conscientiousness_w3, selfw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r34_extra <- cor(selfw$bfas_extraversion_w3, selfw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r34_neuro <- cor(selfw$bfas_neuroticism_w3, selfw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r34_opend <- cor(selfw$bfas_opennessdomain_w3, selfw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r34_assert <- cor(selfw$bfas_assertiveness_w3, selfw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r34_compa <- cor(selfw$bfas_compassion_w3, selfw$bfas_compassion_w4, use = "pairwise.complete.obs")
r34_enthu <- cor(selfw$bfas_enthusiasm_w3, selfw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r34_indus <- cor(selfw$bfas_industriousness_w3, selfw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r34_intel <- cor(selfw$bfas_intellect_w3, selfw$bfas_intellect_w4, use = "pairwise.complete.obs")
r34_opena <- cor(selfw$bfas_opennessaspect_w3, selfw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r34_order <- cor(selfw$bfas_orderliness_w3, selfw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r34_polit <- cor(selfw$bfas_politeness_w3, selfw$bfas_politeness_w4, use = "pairwise.complete.obs")
r34_volat <- cor(selfw$bfas_volatility_w3, selfw$bfas_volatility_w4, use = "pairwise.complete.obs")
r34_withd <- cor(selfw$bfas_withdrawal_w3, selfw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z34_agree <- fisherz(r34_agree)
z34_consci <- fisherz(r34_consci)
z34_extra <- fisherz(r34_extra)
z34_neuro <- fisherz(r34_neuro)
z34_opend <- fisherz(r34_opend)
z34_assert <- fisherz(r34_assert)
z34_compa <- fisherz(r34_compa)
z34_enthu <- fisherz(r34_enthu)
z34_indus <- fisherz(r34_indus)
z34_intel <- fisherz(r34_intel)
z34_opena <- fisherz(r34_opena)
z34_order <- fisherz(r34_order)
z34_polit <- fisherz(r34_polit)
z34_volat <- fisherz(r34_volat)
z34_withd <- fisherz(r34_withd)

### correlation between T1 and T4
r14_agree <- cor(selfw$bfas_agreeableness_w1, selfw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r14_consci <- cor(selfw$bfas_conscientiousness_w1, selfw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r14_extra <- cor(selfw$bfas_extraversion_w1, selfw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r14_neuro <- cor(selfw$bfas_neuroticism_w1, selfw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r14_opend <- cor(selfw$bfas_opennessdomain_w1, selfw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r14_assert <- cor(selfw$bfas_assertiveness_w1, selfw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r14_compa <- cor(selfw$bfas_compassion_w1, selfw$bfas_compassion_w4, use = "pairwise.complete.obs")
r14_enthu <- cor(selfw$bfas_enthusiasm_w1, selfw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r14_indus <- cor(selfw$bfas_industriousness_w1, selfw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r14_intel <- cor(selfw$bfas_intellect_w1, selfw$bfas_intellect_w4, use = "pairwise.complete.obs")
r14_opena <- cor(selfw$bfas_opennessaspect_w1, selfw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r14_order <- cor(selfw$bfas_orderliness_w1, selfw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r14_polit <- cor(selfw$bfas_politeness_w1, selfw$bfas_politeness_w4, use = "pairwise.complete.obs")
r14_volat <- cor(selfw$bfas_volatility_w1, selfw$bfas_volatility_w4, use = "pairwise.complete.obs")
r14_withd <- cor(selfw$bfas_withdrawal_w1, selfw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z14_agree <- fisherz(r14_agree)
z14_consci <- fisherz(r14_consci)
z14_extra <- fisherz(r14_extra)
z14_neuro <- fisherz(r14_neuro)
z14_opend <- fisherz(r14_opend)
z14_assert <- fisherz(r14_assert)
z14_compa <- fisherz(r14_compa)
z14_enthu <- fisherz(r14_enthu)
z14_indus <- fisherz(r14_indus)
z14_intel <- fisherz(r14_intel)
z14_opena <- fisherz(r14_opena)
z14_order <- fisherz(r14_order)
z14_polit <- fisherz(r14_polit)
z14_volat <- fisherz(r14_volat)
z14_withd <- fisherz(r14_withd)

### correlation between T1 and T3
r13_agree <- cor(selfw$bfas_agreeableness_w3, selfw$bfas_agreeableness_w1, use = "pairwise.complete.obs")
r13_consci <- cor(selfw$bfas_conscientiousness_w3, selfw$bfas_conscientiousness_w1, use = "pairwise.complete.obs")
r13_extra <- cor(selfw$bfas_extraversion_w3, selfw$bfas_extraversion_w1, use = "pairwise.complete.obs")
r13_neuro <- cor(selfw$bfas_neuroticism_w3, selfw$bfas_neuroticism_w1, use = "pairwise.complete.obs")
r13_opend <- cor(selfw$bfas_opennessdomain_w3, selfw$bfas_opennessdomain_w1, use = "pairwise.complete.obs")
r13_assert <- cor(selfw$bfas_assertiveness_w3, selfw$bfas_assertiveness_w1, use = "pairwise.complete.obs")
r13_compa <- cor(selfw$bfas_compassion_w3, selfw$bfas_compassion_w1, use = "pairwise.complete.obs")
r13_enthu <- cor(selfw$bfas_enthusiasm_w3, selfw$bfas_enthusiasm_w1, use = "pairwise.complete.obs")
r13_indus <- cor(selfw$bfas_industriousness_w3, selfw$bfas_industriousness_w1, use = "pairwise.complete.obs")
r13_intel <- cor(selfw$bfas_intellect_w3, selfw$bfas_intellect_w1, use = "pairwise.complete.obs")
r13_opena <- cor(selfw$bfas_opennessaspect_w3, selfw$bfas_opennessaspect_w1, use = "pairwise.complete.obs")
r13_order <- cor(selfw$bfas_orderliness_w3, selfw$bfas_orderliness_w1, use = "pairwise.complete.obs")
r13_polit <- cor(selfw$bfas_politeness_w3, selfw$bfas_politeness_w1, use = "pairwise.complete.obs")
r13_volat <- cor(selfw$bfas_volatility_w3, selfw$bfas_volatility_w1, use = "pairwise.complete.obs")
r13_withd <- cor(selfw$bfas_withdrawal_w3, selfw$bfas_withdrawal_w1, use = "pairwise.complete.obs")

z13_agree <- fisherz(r13_agree)
z13_consci <- fisherz(r13_consci)
z13_extra <- fisherz(r13_extra)
z13_neuro <- fisherz(r13_neuro)
z13_opend <- fisherz(r13_opend)
z13_assert <- fisherz(r13_assert)
z13_compa <- fisherz(r13_compa)
z13_enthu <- fisherz(r13_enthu)
z13_indus <- fisherz(r13_indus)
z13_intel <- fisherz(r13_intel)
z13_opena <- fisherz(r13_opena)
z13_order <- fisherz(r13_order)
z13_polit <- fisherz(r13_polit)
z13_volat <- fisherz(r13_volat)
z13_withd <- fisherz(r13_withd)

### correlation between T2 and T4
r24_agree <- cor(selfw$bfas_agreeableness_w2, selfw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r24_consci <- cor(selfw$bfas_conscientiousness_w2, selfw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r24_extra <- cor(selfw$bfas_extraversion_w2, selfw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r24_neuro <- cor(selfw$bfas_neuroticism_w2, selfw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r24_opend <- cor(selfw$bfas_opennessdomain_w2, selfw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r24_assert <- cor(selfw$bfas_assertiveness_w2, selfw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r24_compa <- cor(selfw$bfas_compassion_w2, selfw$bfas_compassion_w4, use = "pairwise.complete.obs")
r24_enthu <- cor(selfw$bfas_enthusiasm_w2, selfw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r24_indus <- cor(selfw$bfas_industriousness_w2, selfw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r24_intel <- cor(selfw$bfas_intellect_w2, selfw$bfas_intellect_w4, use = "pairwise.complete.obs")
r24_opena <- cor(selfw$bfas_opennessaspect_w2, selfw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r24_order <- cor(selfw$bfas_orderliness_w2, selfw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r24_polit <- cor(selfw$bfas_politeness_w2, selfw$bfas_politeness_w4, use = "pairwise.complete.obs")
r24_volat <- cor(selfw$bfas_volatility_w2, selfw$bfas_volatility_w4, use = "pairwise.complete.obs")
r24_withd <- cor(selfw$bfas_withdrawal_w2, selfw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z24_agree <- fisherz(r24_agree)
z24_consci <- fisherz(r24_consci)
z24_extra <- fisherz(r24_extra)
z24_neuro <- fisherz(r24_neuro)
z24_opend <- fisherz(r24_opend)
z24_assert <- fisherz(r24_assert)
z24_compa <- fisherz(r24_compa)
z24_enthu <- fisherz(r24_enthu)
z24_indus <- fisherz(r24_indus)
z24_intel <- fisherz(r24_intel)
z24_opena <- fisherz(r24_opena)
z24_order <- fisherz(r24_order)
z24_polit <- fisherz(r24_polit)
z24_volat <- fisherz(r24_volat)
z24_withd <- fisherz(r24_withd)

# >> determine Ns for comparison ----
n_agree_12 <- sum(selfw$bfas_agreeableness_w2 != "NA", na.rm = TRUE)
n_consci_12 <- sum(selfw$bfas_conscientiousness_w2 != "NA", na.rm = TRUE)
n_extra_12 <- sum(selfw$bfas_extraversion_w2 != "NA", na.rm = TRUE)
n_neuro_12 <- sum(selfw$bfas_neuroticism_w2 != "NA", na.rm = TRUE)
n_opend_12 <- sum(selfw$bfas_opennessdomain_w2 != "NA", na.rm = TRUE)
n_assert_12 <- sum(selfw$bfas_assertiveness_w2 != "NA", na.rm = TRUE)
n_compa_12 <- sum(selfw$bfas_compassion_w2 != "NA", na.rm = TRUE)
n_enthu_12 <- sum(selfw$bfas_enthusiasm_w2 != "NA", na.rm = TRUE)
n_indus_12 <- sum(selfw$bfas_industriousness_w2 != "NA", na.rm = TRUE)
n_intel_12 <- sum(selfw$bfas_intellect_w2 != "NA", na.rm = TRUE)
n_opena_12 <- sum(selfw$bfas_opennessaspect_w2 != "NA", na.rm = TRUE)
n_order_12 <- sum(selfw$bfas_orderliness_w2 != "NA", na.rm = TRUE)
n_polit_12 <- sum(selfw$bfas_politeness_w2 != "NA", na.rm = TRUE)
n_volat_12 <- sum(selfw$bfas_volatility_w2 != "NA", na.rm = TRUE)
n_withd_12 <- sum(selfw$bfas_withdrawal_w2 != "NA", na.rm = TRUE)

n_agree_23 <- sum(selfw$bfas_agreeableness_w3 != "NA", na.rm = TRUE)
n_consci_23 <- sum(selfw$bfas_conscientiousness_w3 != "NA", na.rm = TRUE)
n_extra_23 <- sum(selfw$bfas_extraversion_w3 != "NA", na.rm = TRUE)
n_neuro_23 <- sum(selfw$bfas_neuroticism_w3 != "NA", na.rm = TRUE)
n_opend_23 <- sum(selfw$bfas_opennessdomain_w3 != "NA", na.rm = TRUE)
n_assert_23 <- sum(selfw$bfas_assertiveness_w3 != "NA", na.rm = TRUE)
n_compa_23 <- sum(selfw$bfas_compassion_w3 != "NA", na.rm = TRUE)
n_enthu_23 <- sum(selfw$bfas_enthusiasm_w3 != "NA", na.rm = TRUE)
n_indus_23 <- sum(selfw$bfas_industriousness_w3 != "NA", na.rm = TRUE)
n_intel_23 <- sum(selfw$bfas_intellect_w3 != "NA", na.rm = TRUE)
n_opena_23 <- sum(selfw$bfas_opennessaspect_w3 != "NA", na.rm = TRUE)
n_order_23 <- sum(selfw$bfas_orderliness_w3 != "NA", na.rm = TRUE)
n_polit_23 <- sum(selfw$bfas_politeness_w3 != "NA", na.rm = TRUE)
n_volat_23 <- sum(selfw$bfas_volatility_w3 != "NA", na.rm = TRUE)
n_withd_23 <- sum(selfw$bfas_withdrawal_w3 != "NA", na.rm = TRUE)

n_agree_34 <- sum(selfw$bfas_agreeableness_w4 != "NA", na.rm = TRUE)
n_consci_34 <- sum(selfw$bfas_conscientiousness_w4 != "NA", na.rm = TRUE)
n_extra_34 <- sum(selfw$bfas_extraversion_w4 != "NA", na.rm = TRUE)
n_neuro_34 <- sum(selfw$bfas_neuroticism_w4 != "NA", na.rm = TRUE)
n_opend_34 <- sum(selfw$bfas_opennessdomain_w4 != "NA", na.rm = TRUE)
n_assert_34 <- sum(selfw$bfas_assertiveness_w4 != "NA", na.rm = TRUE)
n_compa_34 <- sum(selfw$bfas_compassion_w4 != "NA", na.rm = TRUE)
n_enthu_34 <- sum(selfw$bfas_enthusiasm_w4 != "NA", na.rm = TRUE)
n_indus_34 <- sum(selfw$bfas_industriousness_w4 != "NA", na.rm = TRUE)
n_intel_34 <- sum(selfw$bfas_intellect_w4 != "NA", na.rm = TRUE)
n_opena_34 <- sum(selfw$bfas_opennessaspect_w4 != "NA", na.rm = TRUE)
n_order_34 <- sum(selfw$bfas_orderliness_w4 != "NA", na.rm = TRUE)
n_polit_34 <- sum(selfw$bfas_politeness_w4 != "NA", na.rm = TRUE)
n_volat_34 <- sum(selfw$bfas_volatility_w4 != "NA", na.rm = TRUE)
n_withd_34 <- sum(selfw$bfas_withdrawal_w4 != "NA", na.rm = TRUE)

# >> compare correlation among intervals ----
rcon12agree <- r.con(r12_agree, n_agree_12)
rcon23agree <- r.con(r12_agree, n_agree_23)
rcon34agree <- r.con(r34_agree, n_agree_34)

rcon12consci <- r.con(r12_consci, n_consci_12)
rcon23consci <- r.con(r12_consci, n_consci_23)
rcon34consci <- r.con(r34_consci, n_consci_34)

rcon12extra <- r.con(r12_extra, n_extra_12)
rcon23extra <- r.con(r12_extra, n_extra_23)
rcon34extra <- r.con(r34_extra, n_extra_34)

rcon12neuro <- r.con(r12_neuro, n_neuro_12)
rcon23neuro <- r.con(r12_neuro, n_neuro_23)
rcon34neuro <- r.con(r34_neuro, n_neuro_34)

rcon12opend <- r.con(r12_opend, n_opend_12)
rcon23opend <- r.con(r12_opend, n_opend_23)
rcon34opend <- r.con(r34_opend, n_opend_34)

rcon12assert <- r.con(r12_assert, n_assert_12)
rcon23assert <- r.con(r12_assert, n_assert_23)
rcon34assert <- r.con(r34_assert, n_assert_34)

rcon12compa <- r.con(r12_compa, n_compa_12)
rcon23compa <- r.con(r12_compa, n_compa_23)
rcon34compa <- r.con(r34_compa, n_compa_34)

rcon12enthu <- r.con(r12_enthu, n_enthu_12)
rcon23enthu <- r.con(r12_enthu, n_enthu_23)
rcon34enthu <- r.con(r34_enthu, n_enthu_34)

rcon12indus <- r.con(r12_indus, n_indus_12)
rcon23indus <- r.con(r12_indus, n_indus_23)
rcon34indus <- r.con(r34_indus, n_indus_34)

rcon12intel <- r.con(r12_intel, n_intel_12)
rcon23intel <- r.con(r12_intel, n_intel_23)
rcon34intel <- r.con(r34_intel, n_intel_34)

rcon12opena <- r.con(r12_opena, n_opena_12)
rcon23opena <- r.con(r12_opena, n_opena_23)
rcon34opena <- r.con(r34_opena, n_opena_34)

rcon12order <- r.con(r12_order, n_order_12)
rcon23order <- r.con(r12_order, n_order_23)
rcon34order <- r.con(r34_order, n_order_34)

rcon12polit <- r.con(r12_polit, n_polit_12)
rcon23polit <- r.con(r12_polit, n_polit_23)
rcon34polit <- r.con(r34_polit, n_polit_34)

rcon12volat <- r.con(r12_volat, n_volat_12)
rcon23volat <- r.con(r12_volat, n_volat_23)
rcon34volat <- r.con(r34_volat, n_volat_34)

rcon12withd <- r.con(r12_withd, n_withd_12)
rcon23withd <- r.con(r12_withd, n_withd_23)
rcon34withd <- r.con(r34_withd, n_withd_34)

agree_12_23 <- (z12_agree-z23_agree)/(sqrt((1/(n_agree_12 - 3)) + (1/(n_agree_23-3))))
agree_23_34 <- (z23_agree-z34_agree)/(sqrt((1/(n_agree_23 - 3)) + (1/(n_agree_34-3))))
agree_12_34 <- (z12_agree-z34_agree)/(sqrt((1/(n_agree_12 - 3)) + (1/(n_agree_34-3))))
p_agree_12_23 <- pnorm(abs(agree_12_23), lower.tail = FALSE)
p_agree_23_34 <-pnorm(abs(agree_23_34), lower.tail = FALSE)
p_agree_12_34 <- pnorm(abs(agree_12_34), lower.tail = FALSE)

consci_12_23 <- (z12_consci-z23_consci)/(sqrt((1/(n_consci_12 - 3)) + (1/(n_consci_23-3))))
consci_23_34 <- (z23_consci-z34_consci)/(sqrt((1/(n_consci_23 - 3)) + (1/(n_consci_34-3))))
consci_12_34 <- (z12_consci-z34_consci)/(sqrt((1/(n_consci_12 - 3)) + (1/(n_consci_34-3))))
p_consci_12_23 <- pnorm(abs(consci_12_23), lower.tail = FALSE)
p_consci_23_34 <- pnorm(abs(consci_23_34), lower.tail = FALSE)
p_consci_12_34 <- pnorm(abs(consci_12_34), lower.tail = FALSE)

extra_12_23 <- (z12_extra-z23_extra)/(sqrt((1/(n_extra_12 - 3)) + (1/(n_extra_23-3))))
extra_23_34 <- (z23_extra-z34_extra)/(sqrt((1/(n_extra_23 - 3)) + (1/(n_extra_34-3))))
extra_12_34 <- (z12_extra-z34_extra)/(sqrt((1/(n_extra_12 - 3)) + (1/(n_extra_34-3))))
p_extra_12_23 <- pnorm(abs(extra_12_23), lower.tail = FALSE)
p_extra_23_34 <- pnorm(abs(extra_23_34), lower.tail = FALSE)
p_extra_12_34 <- pnorm(abs(extra_12_34), lower.tail = FALSE)

neuro_12_23 <- (z12_neuro-z23_neuro)/(sqrt((1/(n_neuro_12 - 3)) + (1/(n_neuro_23-3))))
neuro_23_34 <- (z23_neuro-z34_neuro)/(sqrt((1/(n_neuro_23 - 3)) + (1/(n_neuro_34-3))))
neuro_12_34 <- (z12_neuro-z34_neuro)/(sqrt((1/(n_neuro_12 - 3)) + (1/(n_neuro_34-3))))
p_neuro_12_23 <- pnorm(abs(neuro_12_23), lower.tail = FALSE)
p_neuro_23_34 <- pnorm(abs(neuro_23_34), lower.tail = FALSE)
p_neuro_12_34 <- pnorm(abs(neuro_12_34), lower.tail = FALSE)

opend_12_23 <- (z12_opend-z23_opend)/(sqrt((1/(n_opend_12 - 3)) + (1/(n_opend_23-3))))
opend_23_34 <- (z23_opend-z34_opend)/(sqrt((1/(n_opend_23 - 3)) + (1/(n_opend_34-3))))
opend_12_34 <- (z12_opend-z34_opend)/(sqrt((1/(n_opend_12 - 3)) + (1/(n_opend_34-3))))
p_opend_12_23 <- pnorm(abs(opend_12_23), lower.tail = FALSE)
p_opend_23_34 <- pnorm(abs(opend_23_34), lower.tail = FALSE)
p_opend_12_34 <- pnorm(abs(opend_12_34), lower.tail = FALSE)

assert_12_23 <- (z12_assert-z23_assert)/(sqrt((1/(n_assert_12 - 3)) + (1/(n_assert_23-3))))
assert_23_34 <- (z23_assert-z34_assert)/(sqrt((1/(n_assert_23 - 3)) + (1/(n_assert_34-3))))
assert_12_34 <- (z12_assert-z34_assert)/(sqrt((1/(n_assert_12 - 3)) + (1/(n_assert_34-3))))
p_assert_12_23 <- pnorm(abs(assert_12_23), lower.tail = FALSE)
p_assert_23_34 <- pnorm(abs(assert_23_34), lower.tail = FALSE)
p_assert_12_34 <- pnorm(abs(assert_12_34), lower.tail = FALSE)

compa_12_23 <- (z12_compa-z23_compa)/(sqrt((1/(n_compa_12 - 3)) + (1/(n_compa_23-3))))
compa_23_34 <- (z23_compa-z34_compa)/(sqrt((1/(n_compa_23 - 3)) + (1/(n_compa_34-3))))
compa_12_34 <- (z12_compa-z34_compa)/(sqrt((1/(n_compa_12 - 3)) + (1/(n_compa_34-3))))
p_compa_12_23 <- pnorm(abs(compa_12_23), lower.tail = FALSE)
p_compa_23_34 <- pnorm(abs(compa_23_34), lower.tail = FALSE)
p_compa_12_34 <- pnorm(abs(compa_12_34), lower.tail = FALSE)

enthu_12_23 <- (z12_enthu-z23_enthu)/(sqrt((1/(n_enthu_12 - 3)) + (1/(n_enthu_23-3))))
enthu_23_34 <- (z23_enthu-z34_enthu)/(sqrt((1/(n_enthu_23 - 3)) + (1/(n_enthu_34-3))))
enthu_12_34 <- (z12_enthu-z34_enthu)/(sqrt((1/(n_enthu_12 - 3)) + (1/(n_enthu_34-3))))
p_enthu_12_23 <- pnorm(abs(enthu_12_23), lower.tail = FALSE)
p_enthu_23_34 <- pnorm(abs(enthu_23_34), lower.tail = FALSE)
p_enthu_12_34 <- pnorm(abs(enthu_12_34), lower.tail = FALSE)

indus_12_23 <- (z12_indus-z23_indus)/(sqrt((1/(n_indus_12 - 3)) + (1/(n_indus_23-3))))
indus_23_34 <- (z23_indus-z34_indus)/(sqrt((1/(n_indus_23 - 3)) + (1/(n_indus_34-3))))
indus_12_34 <- (z12_indus-z34_indus)/(sqrt((1/(n_indus_12 - 3)) + (1/(n_indus_34-3))))
p_indus_12_23 <- pnorm(abs(indus_12_23), lower.tail = FALSE)
p_indus_23_34 <- pnorm(abs(indus_23_34), lower.tail = FALSE)
p_indus_12_34 <- pnorm(abs(indus_12_34), lower.tail = FALSE)

intel_12_23 <- (z12_intel-z23_intel)/(sqrt((1/(n_intel_12 - 3)) + (1/(n_intel_23-3))))
intel_23_34 <- (z23_intel-z34_intel)/(sqrt((1/(n_intel_23 - 3)) + (1/(n_intel_34-3))))
intel_12_34 <- (z12_intel-z34_intel)/(sqrt((1/(n_intel_12 - 3)) + (1/(n_intel_34-3))))
p_intel_12_23 <- pnorm(abs(intel_12_23), lower.tail = FALSE)
p_intel_23_34 <- pnorm(abs(intel_23_34), lower.tail = FALSE)
p_intel_12_34 <- pnorm(abs(intel_12_34), lower.tail = FALSE)

opena_12_23 <- (z12_opena-z23_opena)/(sqrt((1/(n_opena_12 - 3)) + (1/(n_opena_23-3))))
opena_23_34 <- (z23_opena-z34_opena)/(sqrt((1/(n_opena_23 - 3)) + (1/(n_opena_34-3))))
opena_12_34 <- (z12_opena-z34_opena)/(sqrt((1/(n_opena_12 - 3)) + (1/(n_opena_34-3))))
p_opena_12_23 <- pnorm(abs(opena_12_23), lower.tail = FALSE)
p_opena_23_34 <- pnorm(abs(opena_23_34), lower.tail = FALSE)
p_opena_12_34 <- pnorm(abs(opena_12_34), lower.tail = FALSE)

order_12_23 <- (z12_order-z23_order)/(sqrt((1/(n_order_12 - 3)) + (1/(n_order_23-3))))
order_23_34 <- (z23_order-z34_order)/(sqrt((1/(n_order_23 - 3)) + (1/(n_order_34-3))))
order_12_34 <- (z12_order-z34_order)/(sqrt((1/(n_order_12 - 3)) + (1/(n_order_34-3))))
p_order_12_23 <- pnorm(abs(order_12_23), lower.tail = FALSE)
p_order_23_34 <- pnorm(abs(order_23_34), lower.tail = FALSE)
p_order_12_34 <- pnorm(abs(order_12_34), lower.tail = FALSE)

polit_12_23 <- (z12_polit-z23_polit)/(sqrt((1/(n_polit_12 - 3)) + (1/(n_polit_23-3))))
polit_23_34 <- (z23_polit-z34_polit)/(sqrt((1/(n_polit_23 - 3)) + (1/(n_polit_34-3))))
polit_12_34 <- (z12_polit-z34_polit)/(sqrt((1/(n_polit_12 - 3)) + (1/(n_polit_34-3))))
p_polit_12_23 <- pnorm(abs(polit_12_23), lower.tail = FALSE)
p_polit_23_34 <- pnorm(abs(polit_23_34), lower.tail = FALSE)
p_polit_12_34 <- pnorm(abs(polit_12_34), lower.tail = FALSE)

volat_12_23 <- (z12_volat-z23_volat)/(sqrt((1/(n_volat_12 - 3)) + (1/(n_volat_23-3))))
volat_23_34 <- (z23_volat-z34_volat)/(sqrt((1/(n_volat_23 - 3)) + (1/(n_volat_34-3))))
volat_12_34 <- (z12_volat-z34_volat)/(sqrt((1/(n_volat_12 - 3)) + (1/(n_volat_34-3))))
p_volat_12_23 <- pnorm(abs(volat_12_23), lower.tail = FALSE)
p_volat_23_34 <- pnorm(abs(volat_23_34), lower.tail = FALSE)
p_volat_12_34 <- pnorm(abs(volat_12_34), lower.tail = FALSE)

withd_12_23 <- (z12_withd-z23_withd)/(sqrt((1/(n_withd_12 - 3)) + (1/(n_withd_23-3))))
withd_23_34 <- (z23_withd-z34_withd)/(sqrt((1/(n_withd_23 - 3)) + (1/(n_withd_34-3))))
withd_12_34 <- (z12_withd-z34_withd)/(sqrt((1/(n_withd_12 - 3)) + (1/(n_withd_34-3))))
p_withd_12_23 <- pnorm(abs(withd_12_23), lower.tail = FALSE)
p_withd_23_34 <- pnorm(abs(withd_23_34), lower.tail = FALSE)
p_withd_12_34 <- pnorm(abs(withd_12_34), lower.tail = FALSE)


# >> imputations ----
# >>> Compute correlation fisher z ----

### correlation between T1 and T2
r12_agree <- with(data = imp_wide,
                   exp = cor(bfas_agreeableness_w1, bfas_agreeableness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_consci <- with(data = imp_wide,
                   exp = cor(bfas_conscientiousness_w1, bfas_conscientiousness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_extra <- with(data = imp_wide,
                   exp = cor(bfas_extraversion_w1, bfas_extraversion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_neuro <- with(data = imp_wide,
                   exp = cor(bfas_neuroticism_w1, bfas_neuroticism_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_opend <- with(data = imp_wide,
                   exp = cor(bfas_opennessdomain_w1, bfas_opennessdomain_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_assert <- with(data = imp_wide,
                   exp = cor(bfas_assertiveness_w1, bfas_assertiveness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_compa <- with(data = imp_wide,
                   exp = cor(bfas_compassion_w1, bfas_compassion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_enthu <- with(data = imp_wide,
                   exp = cor(bfas_enthusiasm_w1, bfas_enthusiasm_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_indus <- with(data = imp_wide,
                   exp = cor(bfas_industriousness_w1, bfas_industriousness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_intel <- with(data = imp_wide,
                   exp = cor(bfas_intellect_w1, bfas_intellect_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_opena <- with(data = imp_wide,
                   exp = cor(bfas_opennessaspect_w1, bfas_opennessaspect_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_order <- with(data = imp_wide,                    
                  exp = cor(bfas_orderliness_w1, bfas_orderliness_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_polit <- with(data = imp_wide,
                  exp = cor(bfas_politeness_w1, bfas_politeness_w2, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_volat <- with(data = imp_wide,
                  exp = cor(bfas_volatility_w1, bfas_volatility_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_withd <- with(data = imp_wide,   
                  exp = cor(bfas_withdrawal_w1, bfas_withdrawal_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z12_agree <- fisherz(r12_agree) %>% melt() %>% dplyr::select(value)
z12_consci <- fisherz(r12_consci) %>% melt() %>% dplyr::select(value)
z12_extra <- fisherz(r12_extra) %>% melt() %>% dplyr::select(value)
z12_neuro <- fisherz(r12_neuro) %>% melt() %>% dplyr::select(value)
z12_opend <- fisherz(r12_opend) %>% melt() %>% dplyr::select(value)
z12_assert <- fisherz(r12_assert) %>% melt() %>% dplyr::select(value)
z12_compa <- fisherz(r12_compa) %>% melt() %>% dplyr::select(value)
z12_enthu <- fisherz(r12_enthu) %>% melt() %>% dplyr::select(value)
z12_indus <- fisherz(r12_indus) %>% melt() %>% dplyr::select(value)
z12_intel <- fisherz(r12_intel) %>% melt() %>% dplyr::select(value)
z12_opena <- fisherz(r12_opena) %>% melt() %>% dplyr::select(value)
z12_order <- fisherz(r12_order) %>% melt() %>% dplyr::select(value)
z12_polit <- fisherz(r12_polit) %>% melt() %>% dplyr::select(value)
z12_volat <- fisherz(r12_volat) %>% melt() %>% dplyr::select(value)
z12_withd <- fisherz(r12_withd) %>% melt() %>% dplyr::select(value)

### correlation between T2 and T3
r23_agree <- with(data = imp_wide,              
                  exp = cor(bfas_agreeableness_w2, bfas_agreeableness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_consci <- with(data = imp_wide,                   
                   exp = cor(bfas_conscientiousness_w2, bfas_conscientiousness_w3,
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_extra <- with(data = imp_wide,                  
                  exp = cor(bfas_extraversion_w2, bfas_extraversion_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_neuro <- with(data = imp_wide,                  
                  exp = cor(bfas_neuroticism_w2, bfas_neuroticism_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_opend <- with(data = imp_wide,                  
                  exp = cor(bfas_opennessdomain_w2, bfas_opennessdomain_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_assert <- with(data = imp_wide,                 
                   exp = cor(bfas_assertiveness_w2, bfas_assertiveness_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_compa <- with(data = imp_wide,                 
                  exp = cor(bfas_compassion_w2, bfas_compassion_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_enthu <- with(data = imp_wide,                  
                  exp = cor(bfas_enthusiasm_w2, bfas_enthusiasm_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_indus <- with(data = imp_wide,                 
                  exp = cor(bfas_industriousness_w2, bfas_industriousness_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_intel <- with(data = imp_wide,                  
                  exp = cor(bfas_intellect_w2, bfas_intellect_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_opena <- with(data = imp_wide,                    
                  exp = cor(bfas_opennessaspect_w2, bfas_opennessaspect_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_order <- with(data = imp_wide,                  
                  exp = cor(bfas_orderliness_w2, bfas_orderliness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_polit <- with(data = imp_wide,                 
                  exp = cor(bfas_politeness_w2, bfas_politeness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_volat <- with(data = imp_wide,                 
                  exp = cor(bfas_volatility_w2, bfas_volatility_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_withd <- with(data = imp_wide,                 
                  exp = cor(bfas_withdrawal_w2, bfas_withdrawal_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z23_agree <- fisherz(r23_agree) %>% melt() %>% dplyr::select(value)
z23_consci <- fisherz(r23_consci) %>% melt() %>% dplyr::select(value)
z23_extra <- fisherz(r23_extra) %>% melt() %>% dplyr::select(value)
z23_neuro <- fisherz(r23_neuro) %>% melt() %>% dplyr::select(value)
z23_opend <- fisherz(r23_opend) %>% melt() %>% dplyr::select(value)
z23_assert <- fisherz(r23_assert) %>% melt() %>% dplyr::select(value)
z23_compa <- fisherz(r23_compa) %>% melt() %>% dplyr::select(value)
z23_enthu <- fisherz(r23_enthu) %>% melt() %>% dplyr::select(value)
z23_indus <- fisherz(r23_indus) %>% melt() %>% dplyr::select(value)
z23_intel <- fisherz(r23_intel) %>% melt() %>% dplyr::select(value)
z23_opena <- fisherz(r23_opena) %>% melt() %>% dplyr::select(value)
z23_order <- fisherz(r23_order) %>% melt() %>% dplyr::select(value)
z23_polit <- fisherz(r23_polit) %>% melt() %>% dplyr::select(value)
z23_volat <- fisherz(r23_volat) %>% melt() %>% dplyr::select(value)
z23_withd <- fisherz(r23_withd) %>% melt() %>% dplyr::select(value)

### correlation between T3 and T4
r34_agree <- with(data = imp_wide,                   
                  exp = cor(bfas_agreeableness_w3, bfas_agreeableness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_consci <- with(data = imp_wide,                    
                   exp = cor(bfas_conscientiousness_w3, bfas_conscientiousness_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_extra <- with(data = imp_wide,                   
                  exp = cor(bfas_extraversion_w3, bfas_extraversion_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_neuro <- with(data = imp_wide,                    
                  exp = cor(bfas_neuroticism_w3, bfas_neuroticism_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_opend <- with(data = imp_wide,                    
                  exp = cor(bfas_opennessdomain_w3, bfas_opennessdomain_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_assert <- with(data = imp_wide,                  
                   exp = cor(bfas_assertiveness_w3, bfas_assertiveness_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_compa <- with(data = imp_wide,                 
                  exp = cor(bfas_compassion_w3, bfas_compassion_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_enthu <- with(data = imp_wide,                 
                  exp = cor(bfas_enthusiasm_w3, bfas_enthusiasm_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_indus <- with(data = imp_wide,                
                  exp = cor(bfas_industriousness_w3, bfas_industriousness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_intel <- with(data = imp_wide,               
                  exp = cor(bfas_intellect_w3, bfas_intellect_w4,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_opena <- with(data = imp_wide,                
                  exp = cor(bfas_opennessaspect_w3, bfas_opennessaspect_w4,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_order <- with(data = imp_wide,                   
                  exp = cor(bfas_orderliness_w3, bfas_orderliness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_polit <- with(data = imp_wide,
                  exp = cor(bfas_politeness_w3, bfas_politeness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_volat <- with(data = imp_wide,   
                  exp = cor(bfas_volatility_w3, bfas_volatility_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_withd <- with(data = imp_wide,     
                  exp = cor(bfas_withdrawal_w3, bfas_withdrawal_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z34_agree <- fisherz(r34_agree) %>% melt() %>% dplyr::select(value)
z34_consci <- fisherz(r34_consci) %>% melt() %>% dplyr::select(value)
z34_extra <- fisherz(r34_extra) %>% melt() %>% dplyr::select(value)
z34_neuro <- fisherz(r34_neuro) %>% melt() %>% dplyr::select(value)
z34_opend <- fisherz(r34_opend) %>% melt() %>% dplyr::select(value)
z34_assert <- fisherz(r34_assert) %>% melt() %>% dplyr::select(value)
z34_compa <- fisherz(r34_compa) %>% melt() %>% dplyr::select(value)
z34_enthu <- fisherz(r34_enthu) %>% melt() %>% dplyr::select(value)
z34_indus <- fisherz(r34_indus) %>% melt() %>% dplyr::select(value)
z34_intel <- fisherz(r34_intel) %>% melt() %>% dplyr::select(value)
z34_opena <- fisherz(r34_opena) %>% melt() %>% dplyr::select(value)
z34_order <- fisherz(r34_order) %>% melt() %>% dplyr::select(value)
z34_polit <- fisherz(r34_polit) %>% melt() %>% dplyr::select(value)
z34_volat <- fisherz(r34_volat) %>% melt() %>% dplyr::select(value)
z34_withd <- fisherz(r34_withd) %>% melt() %>% dplyr::select(value)

# >>> compare correlation and p-value among intervals ----

imp_agree_12_23 <- (z12_agree-z23_agree)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_agree_23_34 <- (z23_agree-z34_agree)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_agree_12_34 <- (z12_agree-z34_agree)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_agree_12_23$p <- pnorm(abs(imp_agree_12_23$value), lower.tail = FALSE)
imp_agree_23_34$p <- pnorm(abs(imp_agree_12_23$value), lower.tail = FALSE)
imp_agree_12_34$p <- pnorm(abs(imp_agree_12_23$value), lower.tail = FALSE)

imp_consci_12_23 <- (z12_consci-z23_consci)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_consci_23_34 <- (z23_consci-z34_consci)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_consci_12_34 <- (z12_consci-z34_consci)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_consci_12_23$p <- pnorm(abs(imp_consci_12_23$value), lower.tail = FALSE)
imp_consci_23_34$p <- pnorm(abs(imp_consci_23_34$value), lower.tail = FALSE)
imp_consci_12_34$p <- pnorm(abs(imp_consci_12_34$value), lower.tail = FALSE)

imp_extra_12_23 <- (z12_extra-z23_extra)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_extra_23_34 <- (z23_extra-z34_extra)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_extra_12_34 <- (z12_extra-z34_extra)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_extra_12_23$p <- pnorm(abs(imp_extra_12_23$value), lower.tail = FALSE)
imp_extra_23_34$p <- pnorm(abs(imp_extra_23_34$value), lower.tail = FALSE)
imp_extra_12_34$p <- pnorm(abs(imp_extra_12_34$value), lower.tail = FALSE)

imp_neuro_12_23 <- (z12_neuro-z23_neuro)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_neuro_23_34 <- (z23_neuro-z34_neuro)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_neuro_12_34 <- (z12_neuro-z34_neuro)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_neuro_12_23$p <- pnorm(abs(imp_neuro_12_23$value), lower.tail = FALSE)
imp_neuro_23_34$p <- pnorm(abs(imp_neuro_23_34$value), lower.tail = FALSE)
imp_neuro_12_34$p <- pnorm(abs(imp_neuro_12_34$value), lower.tail = FALSE)

imp_opend_12_23 <- (z12_opend-z23_opend)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opend_23_34 <- (z23_opend-z34_opend)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opend_12_34 <- (z12_opend-z34_opend)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opend_12_23$p <- pnorm(abs(imp_opend_12_23$value), lower.tail = FALSE)
imp_opend_23_34$p <- pnorm(abs(imp_opend_23_34$value), lower.tail = FALSE)
imp_opend_12_34$p <- pnorm(abs(imp_opend_12_34$value), lower.tail = FALSE)

imp_assert_12_23 <- (z12_assert-z23_assert)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_assert_23_34 <- (z23_assert-z34_assert)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_assert_12_34 <- (z12_assert-z34_assert)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_assert_12_23$p <- pnorm(abs(imp_assert_12_23$value), lower.tail = FALSE)
imp_assert_23_34$p <- pnorm(abs(imp_assert_23_34$value), lower.tail = FALSE)
imp_assert_12_34$p <- pnorm(abs(imp_assert_12_34$value), lower.tail = FALSE)

imp_compa_12_23 <- (z12_compa-z23_compa)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_compa_23_34 <- (z23_compa-z34_compa)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_compa_12_34 <- (z12_compa-z34_compa)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_compa_12_23$p <- pnorm(abs(imp_compa_12_23$value), lower.tail = FALSE)
imp_compa_23_34$p <- pnorm(abs(imp_compa_23_34$value), lower.tail = FALSE)
imp_compa_12_34$p <- pnorm(abs(imp_compa_12_34$value), lower.tail = FALSE)

imp_enthu_12_23 <- (z12_enthu-z23_enthu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_enthu_23_34 <- (z23_enthu-z34_enthu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_enthu_12_34 <- (z12_enthu-z34_enthu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_enthu_12_23$p <- pnorm(abs(imp_enthu_12_23$value), lower.tail = FALSE)
imp_enthu_23_34$p <- pnorm(abs(imp_enthu_23_34$value), lower.tail = FALSE)
imp_enthu_12_34$p <- pnorm(abs(imp_enthu_12_34$value), lower.tail = FALSE)

imp_indus_12_23 <- (z12_indus-z23_indus)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_indus_23_34 <- (z23_indus-z34_indus)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_indus_12_34 <- (z12_indus-z34_indus)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_indus_12_23$p <- pnorm(abs(imp_indus_12_23$value), lower.tail = FALSE)
imp_indus_23_34$p <- pnorm(abs(imp_indus_23_34$value), lower.tail = FALSE)
imp_indus_12_34$p <- pnorm(abs(imp_indus_12_34$value), lower.tail = FALSE)

imp_intel_12_23 <- (z12_intel-z23_intel)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_intel_23_34 <- (z23_intel-z34_intel)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_intel_12_34 <- (z12_intel-z34_intel)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_intel_12_23$p <- pnorm(abs(imp_intel_12_23$value), lower.tail = FALSE)
imp_intel_23_34$p <- pnorm(abs(imp_intel_23_34$value), lower.tail = FALSE)
imp_intel_12_34$p <- pnorm(abs(imp_intel_12_34$value), lower.tail = FALSE)

imp_opena_12_23 <- (z12_opena-z23_opena)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opena_23_34 <- (z23_opena-z34_opena)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opena_12_34 <- (z12_opena-z34_opena)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_opena_12_23$p <- pnorm(abs(imp_opena_12_23$value), lower.tail = FALSE)
imp_opena_23_34$p <- pnorm(abs(imp_opena_23_34$value), lower.tail = FALSE)
imp_opena_12_34$p <- pnorm(abs(imp_opena_12_34$value), lower.tail = FALSE)

imp_order_12_23 <- (z12_order-z23_order)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_order_23_34 <- (z23_order-z34_order)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_order_12_34 <- (z12_order-z34_order)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_order_12_23$p <- pnorm(abs(imp_order_12_23$value), lower.tail = FALSE)
imp_order_23_34$p <- pnorm(abs(imp_order_23_34$value), lower.tail = FALSE)
imp_order_12_34$p <- pnorm(abs(imp_order_12_34$value), lower.tail = FALSE)

imp_polit_12_23 <- (z12_polit-z23_polit)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_polit_23_34 <- (z23_polit-z34_polit)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_polit_12_34 <- (z12_polit-z34_polit)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_polit_12_23$p <- pnorm(abs(imp_polit_12_23$value), lower.tail = FALSE)
imp_polit_23_34$p <- pnorm(abs(imp_polit_23_34$value), lower.tail = FALSE)
imp_polit_12_34$p <- pnorm(abs(imp_polit_12_34$value), lower.tail = FALSE)

imp_volat_12_23 <- (z12_volat-z23_volat)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_volat_23_34 <- (z23_volat-z34_volat)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_volat_12_34 <- (z12_volat-z34_volat)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_volat_12_23$p <- pnorm(abs(imp_volat_12_23$value), lower.tail = FALSE)
imp_volat_23_34$p <- pnorm(abs(imp_volat_23_34$value), lower.tail = FALSE)
imp_volat_12_34$p <- pnorm(abs(imp_volat_12_34$value), lower.tail = FALSE)

imp_withd_12_23 <- (z12_withd-z23_withd)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_withd_23_34 <- (z23_withd-z34_withd)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_withd_12_34 <- (z12_withd-z34_withd)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_withd_12_23$p <- pnorm(abs(imp_withd_12_23$value), lower.tail = FALSE)
imp_withd_23_34$p <- pnorm(abs(imp_withd_23_34$value), lower.tail = FALSE)
imp_withd_12_34$p <- pnorm(abs(imp_withd_12_34$value), lower.tail = FALSE)

# > SELF 2b: Rank order change identity ----
# >> Compute correlation fisher z ----
r12_confu <- cor(selfw$epsi_confusion_w1, selfw$epsi_confusion_w2, use = "pairwise.complete.obs")
r12_coher <- cor(selfw$epsi_coherence_w1, selfw$epsi_coherence_w2, use = "pairwise.complete.obs")

z12_confu <- fisherz(r12_confu)
z12_coher <- fisherz(r12_coher)


r23_confu <- cor(selfw$epsi_confusion_w2, selfw$epsi_confusion_w3, use = "pairwise.complete.obs")
r23_coher <- cor(selfw$epsi_coherence_w2, selfw$epsi_coherence_w3, use = "pairwise.complete.obs")

z23_confu <- fisherz(r23_confu)
z23_coher <- fisherz(r23_coher)

r34_confu <- cor(selfw$epsi_confusion_w3, selfw$epsi_confusion_w4, use = "pairwise.complete.obs")
r34_coher <- cor(selfw$epsi_coherence_w3, selfw$epsi_coherence_w4, use = "pairwise.complete.obs")

z34_confu <- fisherz(r34_confu)
z34_coher <- fisherz(r34_coher)

r14_confu <- cor(selfw$epsi_confusion_w1, selfw$epsi_confusion_w4, use = "pairwise.complete.obs")
r14_coher <- cor(selfw$epsi_coherence_w1, selfw$epsi_coherence_w4, use = "pairwise.complete.obs")

z14_confu <- fisherz(r14_confu)
z14_coher <- fisherz(r14_coher)

r13_confu <- cor(selfw$epsi_confusion_w1, selfw$epsi_confusion_w3, use = "pairwise.complete.obs")
r13_coher <- cor(selfw$epsi_coherence_w1, selfw$epsi_coherence_w3, use = "pairwise.complete.obs")

z13_confu <- fisherz(r13_confu)
z13_coher <- fisherz(r13_coher)

r24_confu <- cor(selfw$epsi_confusion_w2, selfw$epsi_confusion_w4, use = "pairwise.complete.obs")
r24_coher <- cor(selfw$epsi_coherence_w2, selfw$epsi_coherence_w4, use = "pairwise.complete.obs")

z24_confu <- fisherz(r24_confu)
z24_coher <- fisherz(r24_coher)

# >> determine Ns for comparison ----
n_confu_12 <- sum(selfw$epsi_confusion_w2 != "NA", na.rm = TRUE)
n_coher_12 <- sum(selfw$epsi_coherence_w2 != "NA", na.rm = TRUE)
n_confu_23 <- sum(selfw$epsi_confusion_w3 != "NA", na.rm = TRUE)
n_coher_23 <- sum(selfw$epsi_coherence_w3 != "NA", na.rm = TRUE)
n_confu_34 <- sum(selfw$epsi_confusion_w4 != "NA", na.rm = TRUE)
n_coher_34 <- sum(selfw$epsi_coherence_w4 != "NA", na.rm = TRUE)

# >> compare correlation among intervals----
rcon12confu <- r.con(r12_confu, n_confu_12)
rcon23confu <- r.con(r12_confu, n_confu_23)
rcon34confu <- r.con(r34_confu, n_confu_34)

rcon12coher <- r.con(r12_coher, n_coher_12)
rcon23coher <- r.con(r12_coher, n_coher_23)
rcon34coher <- r.con(r34_coher, n_coher_34)


confu_12_23 <- (z12_confu-z23_confu)/(sqrt((1/(n_confu_12 - 3)) + (1/(n_confu_23-3))))
confu_23_34 <- (z23_confu-z34_confu)/(sqrt((1/(n_confu_23 - 3)) + (1/(n_confu_34-3))))
confu_12_34 <- (z12_confu-z34_confu)/(sqrt((1/(n_confu_12 - 3)) + (1/(n_confu_34-3))))
p_confu_12_23 <- pnorm(abs(confu_12_23), lower.tail = FALSE)
p_confu_23_34 <- pnorm(abs(confu_23_34), lower.tail = FALSE)
p_confu_12_34 <- pnorm(abs(confu_12_34), lower.tail = FALSE)

coher_12_23 <- (z12_coher-z23_coher)/(sqrt((1/(n_coher_12 - 3)) + (1/(n_coher_23-3))))
coher_23_34 <- (z23_coher-z34_coher)/(sqrt((1/(n_coher_23 - 3)) + (1/(n_coher_34-3))))
coher_12_34 <- (z12_coher-z34_coher)/(sqrt((1/(n_coher_12 - 3)) + (1/(n_coher_34-3))))
p_coher_12_23 <- pnorm(abs(coher_12_23), lower.tail = FALSE)
p_coher_23_34 <- pnorm(abs(coher_23_34), lower.tail = FALSE)
p_coher_12_34 <- pnorm(abs(coher_12_34), lower.tail = FALSE)

# >> imputations ----
# >>> Compute correlation fisher z ----

### correlation between T1 and T2
r12_confu <- with(data = imp_wide,
                   exp = cor(epsi_confusion_w1, epsi_confusion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_coher <- with(data = imp_wide,
                   exp = cor(epsi_coherence_w1, epsi_coherence_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z12_confu <- fisherz(r12_confu) %>% melt() %>% dplyr::select(value)
z12_coher <- fisherz(r12_coher) %>% melt() %>% dplyr::select(value)

### correlation between T2 and T3
r23_confu <- with(data = imp_wide,
                   exp = cor(epsi_confusion_w2, epsi_confusion_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_coher <- with(data = imp_wide,
                   exp = cor(epsi_coherence_w2, epsi_coherence_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z23_confu <- fisherz(r23_confu) %>% melt() %>% dplyr::select(value)
z23_coher <- fisherz(r23_coher) %>% melt() %>% dplyr::select(value)

### correlation between T3 and T4
r34_confu <- with(data = imp_wide,
                   exp = cor(epsi_confusion_w3, epsi_confusion_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_coher <- with(data = imp_wide,
                   exp = cor(epsi_coherence_w3, epsi_coherence_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z34_confu <- fisherz(r34_confu) %>% melt() %>% dplyr::select(value)
z34_coher <- fisherz(r34_coher) %>% melt() %>% dplyr::select(value)

# >>> compare correlation and p-value among intervals ----
imp_confu_12_23 <- (z12_confu-z23_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_23_34 <- (z23_confu-z34_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_12_34 <- (z12_confu-z34_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_12_23$p <- pnorm(abs(imp_confu_12_23$value), lower.tail = FALSE)
imp_confu_23_34$p <- pnorm(abs(imp_confu_23_34$value), lower.tail = FALSE)
imp_confu_12_34$p <- pnorm(abs(imp_confu_12_34$value), lower.tail = FALSE)

imp_coher_12_23 <- (z12_coher-z23_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_23_34 <- (z23_coher-z34_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_12_34 <- (z12_coher-z34_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_12_23$p <- pnorm(abs(imp_coher_12_23$value), lower.tail = FALSE)
imp_coher_23_34$p <- pnorm(abs(imp_coher_23_34$value), lower.tail = FALSE)
imp_coher_12_34$p <- pnorm(abs(imp_coher_12_34$value), lower.tail = FALSE)

# > SELF 3a: Individual differences in change personality ----

### Agreeableness - intercept < slope
anova(linearint.agree, linear.agree)

### Assertiveness - intercept < slope
anova(linearint.assert, linear.assert)

### Compassion - intercept < slope
anova(linearint.compa, linear.compa)

### Conscientiousness - intercept < slope
anova(linearint.consci, linear.consci)

### Enthusiasm - intercept < slope 
anova(linearint.enthu, linear.enthu)

### Extraversion - intercept < slope
anova(linearint.extra, linear.extra)

### Industriousness - intercept < slope
anova(linearint.indus, linear.indus)

### Intellect - NO sig random slope effect
anova(linearint.intel, linear.intel)

### Neuroticism - intercept < slope
anova(linearint.neuro, linear.neuro)

### Openness Aspect - No sig random slope effect
anova(linearint.opena, linear.opena)

### Openness Domain - No sig random slope effect
anova(linearint.opend, linear.opend)

### Orderlines - intercept < slope
anova(linearint.order, linear.order)

### Politeness - intercept < slope 
anova(linearint.polit, linear.polit)

### Volatility - intercept < slope
anova(linearint.volat, linear.volat)

### Withdrawal - NO sig random slope effect
anova(linearint.withd, linear.withd)


# > SELF 3b: Individual differences in change identity ----
### Coherence - NO sig random slope effect
anova(linearint.coher, linear.coher)

### Confusion - NO sig random slope effect
anova(linearint.confu, linear.confu)


# > SELF 4a: Ipsative change personality ----

# >> merge back the computed D2, D'2, D''2 to selfw dataset -----
dp <- dp %>% 
  dplyr::select(ID, domain_dp_12:aspect_dp_all, domain_dpp_12:aspect_dpp_all)
selfw <- merge(selfw, dp)

# >> compute D2, D'2, D''2 in simulated data ----

sim.domain <- read.csv("domainsim.csv")
sim.aspect <- read.csv("aspectsim.csv")

sim.domain <- sim.domain %>% 
  mutate(domain_d2_12 = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w2)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w2)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w2)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w2)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w2)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_23 = 
           (bfas_agreeableness_w2 - bfas_agreeableness_w3)^2 +
           (bfas_conscientiousness_w2 - bfas_conscientiousness_w3)^2 +
           (bfas_extraversion_w2 - bfas_extraversion_w3)^2 +
           (bfas_neuroticism_w2 - bfas_neuroticism_w3)^2 +
           (bfas_opennessdomain_w2 - bfas_opennessdomain_w3)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_34 = 
           (bfas_agreeableness_w3 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w3 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w3 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w3 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w3 - bfas_opennessdomain_w4)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_all = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w4)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_12 = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w2)^2 +
           (bfas_compassion_w1 - bfas_compassion_w2)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w2)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w2)^2 +
           (bfas_intellect_w1 - bfas_intellect_w2)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w2)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w2)^2 +
           (bfas_politeness_w1 - bfas_politeness_w2)^2 +
           (bfas_volatility_w1 - bfas_volatility_w2)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w2)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_23 = 
           (bfas_assertiveness_w2 - bfas_assertiveness_w3)^2 +
           (bfas_compassion_w2 - bfas_compassion_w3)^2 +
           (bfas_enthusiasm_w2 - bfas_enthusiasm_w3)^2 +
           (bfas_industriousness_w2 - bfas_industriousness_w3)^2 +
           (bfas_intellect_w2 - bfas_intellect_w3)^2 +
           (bfas_opennessaspect_w2 - bfas_opennessaspect_w3)^2 +
           (bfas_orderliness_w2 - bfas_orderliness_w3)^2 +
           (bfas_politeness_w2 - bfas_politeness_w3)^2 +
           (bfas_volatility_w2 - bfas_volatility_w3)^2 +
           (bfas_withdrawal_w2 - bfas_withdrawal_w3)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_34 = 
           (bfas_assertiveness_w3 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w3 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w3 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w3 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w3 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w3 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w3 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w3 - bfas_politeness_w4)^2 +
           (bfas_volatility_w3 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w3 - bfas_withdrawal_w4)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_all = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w1 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w1 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w1 - bfas_politeness_w4)^2 +
           (bfas_volatility_w1 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w4)^2)

### domains
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                      bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_agreeableness_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_conscientiousness_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_extraversion_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                     bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_neuroticism_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                       bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_opennessdomain_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                      bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_agreeableness_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                          bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_conscientiousness_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                     bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_extraversion_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                    bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_neuroticism_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                       bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_opennessdomain_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                      bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_agreeableness_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                          bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_conscientiousness_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                     bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_extraversion_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                    bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_neuroticism_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                       bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_opennessdomain_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                      bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_agreeableness_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                          bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_conscientiousness_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                     bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_extraversion_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                    bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_neuroticism_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                       bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_opennessdomain_w4)

### aspects
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                      bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                      bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                      bfas_withdrawal_w1)/10 - bfas_assertiveness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_compassion_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_enthusiasm_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                        bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                        bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                        bfas_withdrawal_w1)/10 - bfas_industriousness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                  bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                  bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                  bfas_withdrawal_w1)/10 - bfas_intellect_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                       bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                       bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                       bfas_withdrawal_w1)/10 - bfas_opennessaspect_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                    bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                    bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                    bfas_withdrawal_w1)/10 - bfas_orderliness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_politeness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_volatility_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_withdrawal_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                      bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                      bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                      bfas_withdrawal_w2)/10 - bfas_assertiveness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_compassion_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_enthusiasm_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                        bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                        bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                        bfas_withdrawal_w2)/10 - bfas_industriousness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                  bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                  bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                  bfas_withdrawal_w2)/10 - bfas_intellect_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                       bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                       bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                       bfas_withdrawal_w2)/10 - bfas_opennessaspect_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                    bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                    bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                    bfas_withdrawal_w2)/10 - bfas_orderliness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_politeness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_volatility_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_withdrawal_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                      bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                      bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                      bfas_withdrawal_w3)/10 - bfas_assertiveness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_compassion_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_enthusiasm_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                        bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                        bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                        bfas_withdrawal_w3)/10 - bfas_industriousness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                  bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                  bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                  bfas_withdrawal_w3)/10 - bfas_intellect_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                       bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                       bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                       bfas_withdrawal_w3)/10 - bfas_opennessaspect_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                    bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                    bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                    bfas_withdrawal_w3)/10 - bfas_orderliness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_politeness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_volatility_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_withdrawal_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                      bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                      bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                      bfas_withdrawal_w4)/10 - bfas_assertiveness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_compassion_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_enthusiasm_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                        bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                        bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                        bfas_withdrawal_w4)/10 - bfas_industriousness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                  bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                  bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                  bfas_withdrawal_w4)/10 - bfas_intellect_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                       bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                       bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                       bfas_withdrawal_w4)/10 - bfas_opennessaspect_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                    bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                    bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                    bfas_withdrawal_w4)/10 - bfas_orderliness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_politeness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_volatility_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_withdrawal_w4)

### compute D'2 (name: dp) - eliminate elevation - using deviation scores computed above
sim.domain <- sim.domain %>% 
  mutate(domain_dp_12 = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w2_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w2_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w2_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w2_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w2_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_23 = 
           (bfas_agreeableness_w2_d - bfas_agreeableness_w3_d)^2 +
           (bfas_conscientiousness_w2_d - bfas_conscientiousness_w3_d)^2 +
           (bfas_extraversion_w2_d - bfas_extraversion_w3_d)^2 +
           (bfas_neuroticism_w2_d - bfas_neuroticism_w3_d)^2 +
           (bfas_opennessdomain_w2_d - bfas_opennessdomain_w3_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_34 = 
           (bfas_agreeableness_w3_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w3_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w3_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w3_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w3_d - bfas_opennessdomain_w4_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_all = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w4_d)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_12 = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w2_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w2_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w2_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w2_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w2_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w2_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w2_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w2_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w2_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w2_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_23 = 
           (bfas_assertiveness_w2_d - bfas_assertiveness_w3_d)^2 +
           (bfas_compassion_w2_d - bfas_compassion_w3_d)^2 +
           (bfas_enthusiasm_w2_d - bfas_enthusiasm_w3_d)^2 +
           (bfas_industriousness_w2_d - bfas_industriousness_w3_d)^2 +
           (bfas_intellect_w2_d - bfas_intellect_w3_d)^2 +
           (bfas_opennessaspect_w2_d - bfas_opennessaspect_w3_d)^2 +
           (bfas_orderliness_w2_d - bfas_orderliness_w3_d)^2 +
           (bfas_politeness_w2_d - bfas_politeness_w3_d)^2 +
           (bfas_volatility_w2_d - bfas_volatility_w3_d)^2 +
           (bfas_withdrawal_w2_d - bfas_withdrawal_w3_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_34 = 
           (bfas_assertiveness_w3_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w3_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w3_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w3_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w3_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w3_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w3_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w3_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w3_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w3_d - bfas_withdrawal_w4_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_all = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w4_d)^2)

### compute standardized scores
###domain
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w1_ds = bfas_agreeableness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                   bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                   bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w1_ds = bfas_conscientiousness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                            bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                            bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w1_ds = bfas_extraversion_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                  bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                  bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w1_ds = bfas_neuroticism_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w1_ds = bfas_opennessdomain_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                      bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                      bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w2_ds = bfas_agreeableness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                    bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                    bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w2_ds = bfas_conscientiousness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                            bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                            bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w2_ds = bfas_extraversion_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                  bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                  bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w2_ds = bfas_neuroticism_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w2_ds = bfas_opennessdomain_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                      bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                      bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w3_ds = bfas_agreeableness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                    bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                    bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w3_ds = bfas_conscientiousness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                            bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                            bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w3_ds = bfas_extraversion_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                  bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                  bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w3_ds = bfas_neuroticism_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w3_ds = bfas_opennessdomain_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                      bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                      bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w4_ds = bfas_agreeableness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                    bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                    bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w4_ds = bfas_conscientiousness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                            bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                            bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w4_ds = bfas_extraversion_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                  bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                  bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w4_ds = bfas_neuroticism_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w4_ds = bfas_opennessdomain_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                      bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                      bfas_opennessdomain_w4_d^2)))

### aspect
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w1_ds = bfas_assertiveness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                    bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                    bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                    bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                    bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w1_ds = bfas_compassion_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w1_ds = bfas_enthusiasm_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w1_ds = bfas_industriousness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                        bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                        bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                        bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                        bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w1_ds = bfas_intellect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                            bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                            bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                            bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                            bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w1_ds = bfas_opennessaspect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                      bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                      bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                      bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                      bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w1_ds = bfas_orderliness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w1_ds = bfas_politeness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w1_ds = bfas_volatility_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w1_ds = bfas_withdrawal_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w2_ds = bfas_assertiveness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                    bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                    bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                    bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                    bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w2_ds = bfas_compassion_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w2_ds = bfas_enthusiasm_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w2_ds = bfas_industriousness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                        bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                        bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                        bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                        bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w2_ds = bfas_intellect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                            bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                            bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                            bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                            bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w2_ds = bfas_opennessaspect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                      bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                      bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                      bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                      bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w2_ds = bfas_orderliness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w2_ds = bfas_politeness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w2_ds = bfas_volatility_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w2_ds = bfas_withdrawal_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w3_ds = bfas_assertiveness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                    bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                    bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                    bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                    bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w3_ds = bfas_compassion_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w3_ds = bfas_enthusiasm_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w3_ds = bfas_industriousness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                        bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                        bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                        bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                        bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w3_ds = bfas_intellect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                            bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                            bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                            bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                            bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w3_ds = bfas_opennessaspect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                      bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                      bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                      bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                      bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w3_ds = bfas_orderliness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w3_ds = bfas_politeness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w3_ds = bfas_volatility_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w3_ds = bfas_withdrawal_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w4_ds = bfas_assertiveness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                    bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                    bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                    bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                    bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w4_ds = bfas_compassion_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w4_ds = bfas_enthusiasm_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w4_ds = bfas_industriousness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                        bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                        bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                        bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                        bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w4_ds = bfas_intellect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                            bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                            bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                            bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                            bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w4_ds = bfas_opennessaspect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                      bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                      bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                      bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                      bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w4_ds = bfas_orderliness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w4_ds = bfas_politeness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w4_ds = bfas_volatility_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w4_ds = bfas_withdrawal_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))

### compute D''2 (name: dpp) - eliminate elevation and scatter - using standardized deviation scores computed above
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_12 = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w2_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w2_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w2_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w2_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w2_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_23 = 
           (bfas_agreeableness_w2_ds - bfas_agreeableness_w3_ds)^2 +
           (bfas_conscientiousness_w2_ds - bfas_conscientiousness_w3_ds)^2 +
           (bfas_extraversion_w2_ds - bfas_extraversion_w3_ds)^2 +
           (bfas_neuroticism_w2_ds - bfas_neuroticism_w3_ds)^2 +
           (bfas_opennessdomain_w2_ds - bfas_opennessdomain_w3_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_34 = 
           (bfas_agreeableness_w3_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w3_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w3_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w3_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w3_ds - bfas_opennessdomain_w4_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_all = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w4_ds)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_12 = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w2_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w2_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w2_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w2_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w2_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w2_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w2_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w2_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w2_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w2_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_23 = 
           (bfas_assertiveness_w2_ds - bfas_assertiveness_w3_ds)^2 +
           (bfas_compassion_w2_ds - bfas_compassion_w3_ds)^2 +
           (bfas_enthusiasm_w2_ds - bfas_enthusiasm_w3_ds)^2 +
           (bfas_industriousness_w2_ds - bfas_industriousness_w3_ds)^2 +
           (bfas_intellect_w2_ds - bfas_intellect_w3_ds)^2 +
           (bfas_opennessaspect_w2_ds - bfas_opennessaspect_w3_ds)^2 +
           (bfas_orderliness_w2_ds - bfas_orderliness_w3_ds)^2 +
           (bfas_politeness_w2_ds - bfas_politeness_w3_ds)^2 +
           (bfas_volatility_w2_ds - bfas_volatility_w3_ds)^2 +
           (bfas_withdrawal_w2_ds - bfas_withdrawal_w3_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_34 = 
           (bfas_assertiveness_w3_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w3_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w3_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w3_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w3_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w3_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w3_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w3_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w3_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w3_ds - bfas_withdrawal_w4_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_all = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w4_ds)^2)


# >> create 99.9% cutoff from simulated data ----

cutoff_domain_d2_12 <- mean(sim.domain$domain_d2_12) + 3.291*(sd(sim.domain$domain_d2_12)/sqrt(50000))
cutoff_domain_d2_23 <- mean(sim.domain$domain_d2_23) + 3.291*(sd(sim.domain$domain_d2_23)/sqrt(50000))
cutoff_domain_d2_34 <- mean(sim.domain$domain_d2_34) + 3.291*(sd(sim.domain$domain_d2_34)/sqrt(50000))
cutoff_domain_d2_all <- mean(sim.domain$domain_d2_all) + 3.291*(sd(sim.domain$domain_d2_all)/sqrt(50000))
cutoff_domain_dp_12 <- mean(sim.domain$domain_dp_12) + 3.291*(sd(sim.domain$domain_dp_12)/sqrt(50000))
cutoff_domain_dp_23 <- mean(sim.domain$domain_dp_23) + 3.291*(sd(sim.domain$domain_dp_23)/sqrt(50000))
cutoff_domain_dp_34 <- mean(sim.domain$domain_dp_34) + 3.291*(sd(sim.domain$domain_dp_34)/sqrt(50000))
cutoff_domain_dp_all <- mean(sim.domain$domain_dp_all) + 3.291*(sd(sim.domain$domain_dp_all)/sqrt(50000))
cutoff_domain_dpp_12 <- mean(sim.domain$domain_dpp_12) + 3.291*(sd(sim.domain$domain_dpp_12)/sqrt(50000))
cutoff_domain_dpp_23 <- mean(sim.domain$domain_dpp_23) + 3.291*(sd(sim.domain$domain_dpp_23)/sqrt(50000))
cutoff_domain_dpp_34 <- mean(sim.domain$domain_dpp_34) + 3.291*(sd(sim.domain$domain_dpp_34)/sqrt(50000))
cutoff_domain_dpp_all <- mean(sim.domain$domain_dpp_all) + 3.291*(sd(sim.domain$domain_dpp_all)/sqrt(50000))

cutoff_aspect_d2_12 <- mean(sim.aspect$aspect_d2_12) + 3.291*(sd(sim.aspect$aspect_d2_12)/sqrt(50000))
cutoff_aspect_d2_23 <- mean(sim.aspect$aspect_d2_23) + 3.291*(sd(sim.aspect$aspect_d2_23)/sqrt(50000))
cutoff_aspect_d2_34 <- mean(sim.aspect$aspect_d2_34) + 3.291*(sd(sim.aspect$aspect_d2_34)/sqrt(50000))
cutoff_aspect_d2_all <- mean(sim.aspect$aspect_d2_all) + 3.291*(sd(sim.aspect$aspect_d2_all)/sqrt(50000))
cutoff_aspect_dp_12 <- mean(sim.aspect$aspect_dp_12) + 3.291*(sd(sim.aspect$aspect_dp_12)/sqrt(50000))
cutoff_aspect_dp_23 <- mean(sim.aspect$aspect_dp_23) + 3.291*(sd(sim.aspect$aspect_dp_23)/sqrt(50000))
cutoff_aspect_dp_34 <- mean(sim.aspect$aspect_dp_34) + 3.291*(sd(sim.aspect$aspect_dp_34)/sqrt(50000))
cutoff_aspect_dp_all <- mean(sim.aspect$aspect_dp_all) + 3.291*(sd(sim.aspect$aspect_dp_all)/sqrt(50000))
cutoff_aspect_dpp_12 <- mean(sim.aspect$aspect_dpp_12) + 3.291*(sd(sim.aspect$aspect_dpp_12)/sqrt(50000))
cutoff_aspect_dpp_23 <- mean(sim.aspect$aspect_dpp_23) + 3.291*(sd(sim.aspect$aspect_dpp_23)/sqrt(50000))
cutoff_aspect_dpp_34 <- mean(sim.aspect$aspect_dpp_34) + 3.291*(sd(sim.aspect$aspect_dpp_34)/sqrt(50000))
cutoff_aspect_dpp_all <- mean(sim.aspect$aspect_dpp_all) + 3.291*(sd(sim.aspect$aspect_dpp_all)/sqrt(50000))


# >> create function for ipsative calculation ----
ipsative_personality <- function(data) {
# >> compute D2 in original dataset; domain, aspect ----
  data <- data %>% 
  mutate(domain_d2_12 = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w2)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w2)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w2)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w2)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w2)^2)
data <- data %>% 
  mutate(domain_d2_23 = 
           (bfas_agreeableness_w2 - bfas_agreeableness_w3)^2 +
           (bfas_conscientiousness_w2 - bfas_conscientiousness_w3)^2 +
           (bfas_extraversion_w2 - bfas_extraversion_w3)^2 +
           (bfas_neuroticism_w2 - bfas_neuroticism_w3)^2 +
           (bfas_opennessdomain_w2 - bfas_opennessdomain_w3)^2)
data <- data %>% 
  mutate(domain_d2_34 = 
           (bfas_agreeableness_w3 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w3 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w3 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w3 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w3 - bfas_opennessdomain_w4)^2)
data <- data %>% 
  mutate(domain_d2_all = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w4)^2)

data <- data %>% 
  mutate(aspect_d2_12 = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w2)^2 +
           (bfas_compassion_w1 - bfas_compassion_w2)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w2)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w2)^2 +
           (bfas_intellect_w1 - bfas_intellect_w2)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w2)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w2)^2 +
           (bfas_politeness_w1 - bfas_politeness_w2)^2 +
           (bfas_volatility_w1 - bfas_volatility_w2)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w2)^2)
data <- data %>% 
  mutate(aspect_d2_23 = 
           (bfas_assertiveness_w2 - bfas_assertiveness_w3)^2 +
           (bfas_compassion_w2 - bfas_compassion_w3)^2 +
           (bfas_enthusiasm_w2 - bfas_enthusiasm_w3)^2 +
           (bfas_industriousness_w2 - bfas_industriousness_w3)^2 +
           (bfas_intellect_w2 - bfas_intellect_w3)^2 +
           (bfas_opennessaspect_w2 - bfas_opennessaspect_w3)^2 +
           (bfas_orderliness_w2 - bfas_orderliness_w3)^2 +
           (bfas_politeness_w2 - bfas_politeness_w3)^2 +
           (bfas_volatility_w2 - bfas_volatility_w3)^2 +
           (bfas_withdrawal_w2 - bfas_withdrawal_w3)^2)
data <- data %>% 
  mutate(aspect_d2_34 = 
           (bfas_assertiveness_w3 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w3 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w3 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w3 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w3 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w3 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w3 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w3 - bfas_politeness_w4)^2 +
           (bfas_volatility_w3 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w3 - bfas_withdrawal_w4)^2)
data <- data %>% 
  mutate(aspect_d2_all = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w1 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w1 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w1 - bfas_politeness_w4)^2 +
           (bfas_volatility_w1 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w4)^2)


# >> compute deviation scores ----

dp <- data %>% 
  dplyr::select(ID, bfas_agreeableness_w1:epsi_coherence_w4)

### domains
dp <- dp %>% 
  mutate(bfas_agreeableness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                      bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_agreeableness_w1)
dp <- dp %>% 
  mutate(bfas_conscientiousness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_conscientiousness_w1)
dp <- dp %>% 
  mutate(bfas_extraversion_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_extraversion_w1)
dp <- dp %>% 
  mutate(bfas_neuroticism_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                     bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_neuroticism_w1)
dp <- dp %>% 
  mutate(bfas_opennessdomain_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                       bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_opennessdomain_w1)
dp <- dp %>% 
  mutate(bfas_agreeableness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                      bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_agreeableness_w2)
dp <- dp %>% 
  mutate(bfas_conscientiousness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                          bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_conscientiousness_w2)
dp <- dp %>% 
  mutate(bfas_extraversion_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                     bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_extraversion_w2)
dp <- dp %>% 
  mutate(bfas_neuroticism_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                    bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_neuroticism_w2)
dp <- dp %>% 
  mutate(bfas_opennessdomain_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                       bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_opennessdomain_w2)
dp <- dp %>% 
  mutate(bfas_agreeableness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                      bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_agreeableness_w3)
dp <- dp %>% 
  mutate(bfas_conscientiousness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                          bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_conscientiousness_w3)
dp <- dp %>% 
  mutate(bfas_extraversion_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                     bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_extraversion_w3)
dp <- dp %>% 
  mutate(bfas_neuroticism_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                    bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_neuroticism_w3)
dp <- dp %>% 
  mutate(bfas_opennessdomain_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                       bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_opennessdomain_w3)
dp <- dp %>% 
  mutate(bfas_agreeableness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                      bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_agreeableness_w4)
dp <- dp %>% 
  mutate(bfas_conscientiousness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                          bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_conscientiousness_w4)
dp <- dp %>% 
  mutate(bfas_extraversion_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                     bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_extraversion_w4)
dp <- dp %>% 
  mutate(bfas_neuroticism_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                    bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_neuroticism_w4)
dp <- dp %>% 
  mutate(bfas_opennessdomain_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                       bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_opennessdomain_w4)

### aspects
dp <- dp %>% 
  mutate(bfas_assertiveness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                      bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                      bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                      bfas_withdrawal_w1)/10 - bfas_assertiveness_w1)
dp <- dp %>% 
  mutate(bfas_compassion_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_compassion_w1)
dp <- dp %>% 
  mutate(bfas_enthusiasm_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_enthusiasm_w1)
dp <- dp %>% 
  mutate(bfas_industriousness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                        bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                        bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                        bfas_withdrawal_w1)/10 - bfas_industriousness_w1)
dp <- dp %>% 
  mutate(bfas_intellect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                  bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                  bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                  bfas_withdrawal_w1)/10 - bfas_intellect_w1)
dp <- dp %>% 
  mutate(bfas_opennessaspect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                       bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                       bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                       bfas_withdrawal_w1)/10 - bfas_opennessaspect_w1)
dp <- dp %>% 
  mutate(bfas_orderliness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                    bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                    bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                    bfas_withdrawal_w1)/10 - bfas_orderliness_w1)
dp <- dp %>% 
  mutate(bfas_politeness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_politeness_w1)
dp <- dp %>% 
  mutate(bfas_volatility_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_volatility_w1)
dp <- dp %>% 
  mutate(bfas_withdrawal_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_withdrawal_w1)
dp <- dp %>% 
  mutate(bfas_assertiveness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                      bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                      bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                      bfas_withdrawal_w2)/10 - bfas_assertiveness_w2)
dp <- dp %>% 
  mutate(bfas_compassion_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_compassion_w2)
dp <- dp %>% 
  mutate(bfas_enthusiasm_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_enthusiasm_w2)
dp <- dp %>% 
  mutate(bfas_industriousness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                        bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                        bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                        bfas_withdrawal_w2)/10 - bfas_industriousness_w2)
dp <- dp %>% 
  mutate(bfas_intellect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                  bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                  bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                  bfas_withdrawal_w2)/10 - bfas_intellect_w2)
dp <- dp %>% 
  mutate(bfas_opennessaspect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                       bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                       bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                       bfas_withdrawal_w2)/10 - bfas_opennessaspect_w2)
dp <- dp %>% 
  mutate(bfas_orderliness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                    bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                    bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                    bfas_withdrawal_w2)/10 - bfas_orderliness_w2)
dp <- dp %>% 
  mutate(bfas_politeness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_politeness_w2)
dp <- dp %>% 
  mutate(bfas_volatility_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_volatility_w2)
dp <- dp %>% 
  mutate(bfas_withdrawal_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_withdrawal_w2)
dp <- dp %>% 
  mutate(bfas_assertiveness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                      bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                      bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                      bfas_withdrawal_w3)/10 - bfas_assertiveness_w3)
dp <- dp %>% 
  mutate(bfas_compassion_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_compassion_w3)
dp <- dp %>% 
  mutate(bfas_enthusiasm_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_enthusiasm_w3)
dp <- dp %>% 
  mutate(bfas_industriousness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                        bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                        bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                        bfas_withdrawal_w3)/10 - bfas_industriousness_w3)
dp <- dp %>% 
  mutate(bfas_intellect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                  bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                  bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                  bfas_withdrawal_w3)/10 - bfas_intellect_w3)
dp <- dp %>% 
  mutate(bfas_opennessaspect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                       bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                       bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                       bfas_withdrawal_w3)/10 - bfas_opennessaspect_w3)
dp <- dp %>% 
  mutate(bfas_orderliness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                    bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                    bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                    bfas_withdrawal_w3)/10 - bfas_orderliness_w3)
dp <- dp %>% 
  mutate(bfas_politeness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_politeness_w3)
dp <- dp %>% 
  mutate(bfas_volatility_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_volatility_w3)
dp <- dp %>% 
  mutate(bfas_withdrawal_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_withdrawal_w3)
dp <- dp %>% 
  mutate(bfas_assertiveness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                      bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                      bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                      bfas_withdrawal_w4)/10 - bfas_assertiveness_w4)
dp <- dp %>% 
  mutate(bfas_compassion_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_compassion_w4)
dp <- dp %>% 
  mutate(bfas_enthusiasm_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_enthusiasm_w4)
dp <- dp %>% 
  mutate(bfas_industriousness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                        bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                        bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                        bfas_withdrawal_w4)/10 - bfas_industriousness_w4)
dp <- dp %>% 
  mutate(bfas_intellect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                  bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                  bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                  bfas_withdrawal_w4)/10 - bfas_intellect_w4)
dp <- dp %>% 
  mutate(bfas_opennessaspect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                       bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                       bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                       bfas_withdrawal_w4)/10 - bfas_opennessaspect_w4)
dp <- dp %>% 
  mutate(bfas_orderliness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                    bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                    bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                    bfas_withdrawal_w4)/10 - bfas_orderliness_w4)
dp <- dp %>% 
  mutate(bfas_politeness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_politeness_w4)
dp <- dp %>% 
  mutate(bfas_volatility_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_volatility_w4)
dp <- dp %>% 
  mutate(bfas_withdrawal_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_withdrawal_w4)

### compute D'2 (name: dp) - eliminate elevation - using deviation scores computed above
dp <- dp %>% 
  mutate(domain_dp_12 = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w2_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w2_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w2_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w2_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w2_d)^2)
dp <- dp %>% 
  mutate(domain_dp_23 = 
           (bfas_agreeableness_w2_d - bfas_agreeableness_w3_d)^2 +
           (bfas_conscientiousness_w2_d - bfas_conscientiousness_w3_d)^2 +
           (bfas_extraversion_w2_d - bfas_extraversion_w3_d)^2 +
           (bfas_neuroticism_w2_d - bfas_neuroticism_w3_d)^2 +
           (bfas_opennessdomain_w2_d - bfas_opennessdomain_w3_d)^2)
dp <- dp %>% 
  mutate(domain_dp_34 = 
           (bfas_agreeableness_w3_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w3_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w3_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w3_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w3_d - bfas_opennessdomain_w4_d)^2)
dp <- dp %>% 
  mutate(domain_dp_all = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w4_d)^2)

dp <- dp %>% 
  mutate(aspect_dp_12 = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w2_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w2_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w2_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w2_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w2_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w2_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w2_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w2_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w2_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w2_d)^2)
dp <- dp %>% 
  mutate(aspect_dp_23 = 
           (bfas_assertiveness_w2_d - bfas_assertiveness_w3_d)^2 +
           (bfas_compassion_w2_d - bfas_compassion_w3_d)^2 +
           (bfas_enthusiasm_w2_d - bfas_enthusiasm_w3_d)^2 +
           (bfas_industriousness_w2_d - bfas_industriousness_w3_d)^2 +
           (bfas_intellect_w2_d - bfas_intellect_w3_d)^2 +
           (bfas_opennessaspect_w2_d - bfas_opennessaspect_w3_d)^2 +
           (bfas_orderliness_w2_d - bfas_orderliness_w3_d)^2 +
           (bfas_politeness_w2_d - bfas_politeness_w3_d)^2 +
           (bfas_volatility_w2_d - bfas_volatility_w3_d)^2 +
           (bfas_withdrawal_w2_d - bfas_withdrawal_w3_d)^2)
dp <- dp %>% 
  mutate(aspect_dp_34 = 
           (bfas_assertiveness_w3_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w3_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w3_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w3_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w3_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w3_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w3_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w3_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w3_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w3_d - bfas_withdrawal_w4_d)^2)
dp <- dp %>% 
  mutate(aspect_dp_all = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w4_d)^2)


# >> compute standardized scores ----

###domain
dp <- dp %>% 
  mutate(bfas_agreeableness_w1_ds = bfas_agreeableness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                   bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                   bfas_opennessdomain_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_conscientiousness_w1_ds = bfas_conscientiousness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                            bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                            bfas_opennessdomain_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_extraversion_w1_ds = bfas_extraversion_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                  bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                  bfas_opennessdomain_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_neuroticism_w1_ds = bfas_neuroticism_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                bfas_opennessdomain_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessdomain_w1_ds = bfas_opennessdomain_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                      bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                      bfas_opennessdomain_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_agreeableness_w2_ds = bfas_agreeableness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                    bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                    bfas_opennessdomain_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_conscientiousness_w2_ds = bfas_conscientiousness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                            bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                            bfas_opennessdomain_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_extraversion_w2_ds = bfas_extraversion_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                  bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                  bfas_opennessdomain_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_neuroticism_w2_ds = bfas_neuroticism_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                bfas_opennessdomain_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessdomain_w2_ds = bfas_opennessdomain_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                      bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                      bfas_opennessdomain_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_agreeableness_w3_ds = bfas_agreeableness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                    bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                    bfas_opennessdomain_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_conscientiousness_w3_ds = bfas_conscientiousness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                            bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                            bfas_opennessdomain_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_extraversion_w3_ds = bfas_extraversion_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                  bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                  bfas_opennessdomain_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_neuroticism_w3_ds = bfas_neuroticism_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                bfas_opennessdomain_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessdomain_w3_ds = bfas_opennessdomain_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                      bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                      bfas_opennessdomain_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_agreeableness_w4_ds = bfas_agreeableness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                    bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                    bfas_opennessdomain_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_conscientiousness_w4_ds = bfas_conscientiousness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                            bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                            bfas_opennessdomain_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_extraversion_w4_ds = bfas_extraversion_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                  bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                  bfas_opennessdomain_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_neuroticism_w4_ds = bfas_neuroticism_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                bfas_opennessdomain_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessdomain_w4_ds = bfas_opennessdomain_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                      bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                      bfas_opennessdomain_w4_d^2)))

### aspect
dp <- dp %>% 
  mutate(bfas_assertiveness_w1_ds = bfas_assertiveness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                    bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                    bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                    bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                    bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_compassion_w1_ds = bfas_compassion_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_enthusiasm_w1_ds = bfas_enthusiasm_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_industriousness_w1_ds = bfas_industriousness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                        bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                        bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                        bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                        bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_intellect_w1_ds = bfas_intellect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                            bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                            bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                            bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                            bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessaspect_w1_ds = bfas_opennessaspect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                      bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                      bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                      bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                      bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_orderliness_w1_ds = bfas_orderliness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_politeness_w1_ds = bfas_politeness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_volatility_w1_ds = bfas_volatility_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_withdrawal_w1_ds = bfas_withdrawal_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
dp <- dp %>% 
  mutate(bfas_assertiveness_w2_ds = bfas_assertiveness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                    bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                    bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                    bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                    bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_compassion_w2_ds = bfas_compassion_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_enthusiasm_w2_ds = bfas_enthusiasm_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_industriousness_w2_ds = bfas_industriousness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                        bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                        bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                        bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                        bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_intellect_w2_ds = bfas_intellect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                            bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                            bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                            bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                            bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessaspect_w2_ds = bfas_opennessaspect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                      bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                      bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                      bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                      bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_orderliness_w2_ds = bfas_orderliness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_politeness_w2_ds = bfas_politeness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_volatility_w2_ds = bfas_volatility_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_withdrawal_w2_ds = bfas_withdrawal_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
dp <- dp %>% 
  mutate(bfas_assertiveness_w3_ds = bfas_assertiveness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                    bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                    bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                    bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                    bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_compassion_w3_ds = bfas_compassion_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_enthusiasm_w3_ds = bfas_enthusiasm_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_industriousness_w3_ds = bfas_industriousness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                        bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                        bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                        bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                        bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_intellect_w3_ds = bfas_intellect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                            bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                            bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                            bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                            bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessaspect_w3_ds = bfas_opennessaspect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                      bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                      bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                      bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                      bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_orderliness_w3_ds = bfas_orderliness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_politeness_w3_ds = bfas_politeness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_volatility_w3_ds = bfas_volatility_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_withdrawal_w3_ds = bfas_withdrawal_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
dp <- dp %>% 
  mutate(bfas_assertiveness_w4_ds = bfas_assertiveness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                    bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                    bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                    bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                    bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_compassion_w4_ds = bfas_compassion_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_enthusiasm_w4_ds = bfas_enthusiasm_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_industriousness_w4_ds = bfas_industriousness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                        bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                        bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                        bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                        bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_intellect_w4_ds = bfas_intellect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                            bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                            bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                            bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                            bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_opennessaspect_w4_ds = bfas_opennessaspect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                      bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                      bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                      bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                      bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_orderliness_w4_ds = bfas_orderliness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_politeness_w4_ds = bfas_politeness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_volatility_w4_ds = bfas_volatility_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
dp <- dp %>% 
  mutate(bfas_withdrawal_w4_ds = bfas_withdrawal_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))

### compute D''2 (name: dpp) - eliminate elevation and scatter - using standardized deviation scores computed above
dp <- dp %>% 
  mutate(domain_dpp_12 = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w2_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w2_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w2_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w2_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w2_ds)^2)
dp <- dp %>% 
  mutate(domain_dpp_23 = 
           (bfas_agreeableness_w2_ds - bfas_agreeableness_w3_ds)^2 +
           (bfas_conscientiousness_w2_ds - bfas_conscientiousness_w3_ds)^2 +
           (bfas_extraversion_w2_ds - bfas_extraversion_w3_ds)^2 +
           (bfas_neuroticism_w2_ds - bfas_neuroticism_w3_ds)^2 +
           (bfas_opennessdomain_w2_ds - bfas_opennessdomain_w3_ds)^2)
dp <- dp %>% 
  mutate(domain_dpp_34 = 
           (bfas_agreeableness_w3_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w3_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w3_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w3_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w3_ds - bfas_opennessdomain_w4_ds)^2)
dp <- dp %>% 
  mutate(domain_dpp_all = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w4_ds)^2)

dp <- dp %>% 
  mutate(aspect_dpp_12 = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w2_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w2_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w2_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w2_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w2_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w2_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w2_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w2_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w2_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w2_ds)^2)
dp <- dp %>% 
  mutate(aspect_dpp_23 = 
           (bfas_assertiveness_w2_ds - bfas_assertiveness_w3_ds)^2 +
           (bfas_compassion_w2_ds - bfas_compassion_w3_ds)^2 +
           (bfas_enthusiasm_w2_ds - bfas_enthusiasm_w3_ds)^2 +
           (bfas_industriousness_w2_ds - bfas_industriousness_w3_ds)^2 +
           (bfas_intellect_w2_ds - bfas_intellect_w3_ds)^2 +
           (bfas_opennessaspect_w2_ds - bfas_opennessaspect_w3_ds)^2 +
           (bfas_orderliness_w2_ds - bfas_orderliness_w3_ds)^2 +
           (bfas_politeness_w2_ds - bfas_politeness_w3_ds)^2 +
           (bfas_volatility_w2_ds - bfas_volatility_w3_ds)^2 +
           (bfas_withdrawal_w2_ds - bfas_withdrawal_w3_ds)^2)
dp <- dp %>% 
  mutate(aspect_dpp_34 = 
           (bfas_assertiveness_w3_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w3_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w3_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w3_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w3_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w3_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w3_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w3_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w3_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w3_ds - bfas_withdrawal_w4_ds)^2)
dp <- dp %>% 
  mutate(aspect_dpp_all = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w4_ds)^2)


# >> merge back the computed D2, D'2, D''2 to data dataset -----
dp <- dp %>% 
  dplyr::select(ID, domain_dp_12:aspect_dp_all, domain_dpp_12:aspect_dpp_all)
data <- merge(data, dp)

# >> compute percentage that show change greater than chance ----
data <- data %>% 
  mutate(domain_d2_12c =ifelse(
    domain_d2_12 > cutoff_domain_d2_12,
    1,
    0))
data <- data %>% 
  mutate(domain_d2_23c =ifelse(
    domain_d2_23 > cutoff_domain_d2_23,
    1,
    0))
data <- data %>% 
  mutate(domain_d2_34c =ifelse(
    domain_d2_34 > cutoff_domain_d2_34,
    1,
    0))
data <- data %>% 
  mutate(domain_d2_allc =ifelse(
    domain_d2_all > cutoff_domain_d2_all,
    1,
    0))
data <- data %>% 
  mutate(domain_dp_12c =ifelse(
    domain_dp_12 > cutoff_domain_dp_12,
    1,
    0))
data <- data %>% 
  mutate(domain_dp_23c =ifelse(
    domain_dp_23 > cutoff_domain_dp_23,
    1,
    0))
data <- data %>% 
  mutate(domain_dp_34c =ifelse(
    domain_dp_34 > cutoff_domain_dp_34,
    1,
    0))
data <- data %>% 
  mutate(domain_dp_allc =ifelse(
    domain_dp_all > cutoff_domain_dp_all,
    1,
    0))

data <- data %>% 
  mutate(domain_dpp_12c =ifelse(
    domain_dpp_12 > cutoff_domain_dpp_12,
    1,
    0))
data <- data %>% 
  mutate(domain_dpp_23c =ifelse(
    domain_dpp_23 > cutoff_domain_dpp_23,
    1,
    0))
data <- data %>% 
  mutate(domain_dpp_34c =ifelse(
    domain_dpp_34 > cutoff_domain_dpp_34,
    1,
    0))
data <- data %>% 
  mutate(domain_dpp_allc =ifelse(
    domain_dpp_all > cutoff_domain_dpp_all,
    1,
    0))

data <- data %>% 
  mutate(aspect_d2_12c =ifelse(
    aspect_d2_12 > cutoff_aspect_d2_12,
    1,
    0))
data <- data %>% 
  mutate(aspect_d2_23c =ifelse(
    aspect_d2_23 > cutoff_aspect_d2_23,
    1,
    0))
data <- data %>% 
  mutate(aspect_d2_34c =ifelse(
    aspect_d2_34 > cutoff_aspect_d2_34,
    1,
    0))
data <- data %>% 
  mutate(aspect_d2_allc =ifelse(
    aspect_d2_all > cutoff_aspect_d2_all,
    1,
    0))
data <- data %>% 
  mutate(aspect_dp_12c =ifelse(
    aspect_dp_12 > cutoff_aspect_dp_12,
    1,
    0))
data <- data %>% 
  mutate(aspect_dp_23c =ifelse(
    aspect_dp_23 > cutoff_aspect_dp_23,
    1,
    0))
data <- data %>% 
  mutate(aspect_dp_34c =ifelse(
    aspect_dp_34 > cutoff_aspect_dp_34,
    1,
    0))
data <- data %>% 
  mutate(aspect_dp_allc =ifelse(
    aspect_dp_all > cutoff_aspect_dp_all,
    1,
    0))

data <- data %>% 
  mutate(aspect_dpp_12c =ifelse(
    aspect_dpp_12 > cutoff_aspect_dpp_12,
    1,
    0))
data <- data %>% 
  mutate(aspect_dpp_23c =ifelse(
    aspect_dpp_23 > cutoff_aspect_dpp_23,
    1,
    0))
data <- data %>% 
  mutate(aspect_dpp_34c =ifelse(
    aspect_dpp_34 > cutoff_aspect_dpp_34,
    1,
    0))
data <- data %>% 
  mutate(aspect_dpp_allc =ifelse(
    aspect_dpp_all > cutoff_aspect_dpp_all,
    1,
    0))

return (data.frame(name = c("domain_d2_12", "domain_d2_23", "domain_d2_34",
                            "domain_dp_12", "domain_dp_23", "domain_dp_34",
                            "domain_dpp_12", "domain_dpp_23", "domain_dpp_34", 
                            "domain_d2_all", "domain_dp_all", "domain_dpp_all",
                            "aspect_d2_12", "aspect_d2_23", "aspect_d2_34",
                            "aspect_dp_12", "aspect_dp_23", "aspect_dp_34",
                            "aspect_dpp_12", "aspect_dpp_23", "aspect_dpp_34", 
                            "aspect_d2_all", "aspect_dp_all", "aspect_dpp_all"),
                   value = c(mean(data$domain_d2_12c, na.rm = TRUE), mean(data$domain_d2_23c, na.rm = TRUE),
                             mean(data$domain_d2_34c, na.rm = TRUE), mean(data$domain_dp_12c, na.rm = TRUE),
                             mean(data$domain_dp_23c, na.rm = TRUE), mean(data$domain_dp_34c, na.rm = TRUE),
                             mean(data$domain_dpp_12c, na.rm = TRUE), mean(data$domain_dpp_23c, na.rm = TRUE),
                             mean(data$domain_dpp_34c, na.rm = TRUE), mean(data$domain_d2_allc, na.rm = TRUE),
                             mean(data$domain_dp_allc, na.rm = TRUE), mean(data$domain_dpp_allc, na.rm = TRUE),
                             mean(data$aspect_d2_12c, na.rm = TRUE), mean(data$aspect_d2_23c, na.rm = TRUE),
                             mean(data$aspect_d2_34c, na.rm = TRUE), mean(data$aspect_dp_12c, na.rm = TRUE),
                             mean(data$aspect_dp_23c, na.rm = TRUE), mean(data$aspect_dp_34c, na.rm = TRUE),
                             mean(data$aspect_dpp_12c, na.rm = TRUE), mean(data$aspect_dpp_23c, na.rm = TRUE),
                             mean(data$aspect_dpp_34c, na.rm = TRUE), mean(data$aspect_d2_allc, na.rm = TRUE),
                             mean(data$aspect_dp_allc, na.rm = TRUE), mean(data$aspect_dpp_allc, na.rm = TRUE))))
}

# >> ipsative personality ----
ipsative_personality(selfw)

# >> imputations----

# >>> compute for each imputed dataset ----
imp_ips_pers1 <- ipsative_personality(selfw_1)
imp_ips_pers2 <- ipsative_personality(selfw_2)
imp_ips_pers3 <- ipsative_personality(selfw_3)
imp_ips_pers4 <- ipsative_personality(selfw_4)
imp_ips_pers5 <- ipsative_personality(selfw_5)
imp_ips_pers6 <- ipsative_personality(selfw_6)
imp_ips_pers7 <- ipsative_personality(selfw_7)
imp_ips_pers8 <- ipsative_personality(selfw_8)
imp_ips_pers9 <- ipsative_personality(selfw_9)
imp_ips_pers10 <- ipsative_personality(selfw_10)
imp_ips_pers11 <- ipsative_personality(selfw_11)
imp_ips_pers12 <- ipsative_personality(selfw_12)
imp_ips_pers13 <- ipsative_personality(selfw_13)
imp_ips_pers14 <- ipsative_personality(selfw_14)
imp_ips_pers15 <- ipsative_personality(selfw_15)
imp_ips_pers16 <- ipsative_personality(selfw_16)
imp_ips_pers17 <- ipsative_personality(selfw_17)
imp_ips_pers18 <- ipsative_personality(selfw_18)
imp_ips_pers19 <- ipsative_personality(selfw_19)
imp_ips_pers20 <- ipsative_personality(selfw_20)
imp_ips_pers <- rbind(imp_ips_pers1, imp_ips_pers2, imp_ips_pers3, imp_ips_pers4,
                      imp_ips_pers5, imp_ips_pers6, imp_ips_pers7, imp_ips_pers8,
                      imp_ips_pers9, imp_ips_pers10, imp_ips_pers11, imp_ips_pers12,
                      imp_ips_pers13, imp_ips_pers14, imp_ips_pers15, imp_ips_pers16,
                      imp_ips_pers17, imp_ips_pers18, imp_ips_pers19, imp_ips_pers20)

# >>> aggregating ipsative change across imputations ----
imp_ips_pers %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

# > SELF 4b: Ipsative change identity ----
# >> in simulated dataset ----

sim.identity <- read.csv("identitysim.csv")
sim.identity <- sim.identity %>% 
  mutate(identity_d2_12 = 
           (epsi_confusion_w1 - epsi_confusion_w2)^2 +
           (epsi_coherence_w1 - epsi_coherence_w2)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_23 = 
           (epsi_confusion_w2 - epsi_confusion_w3)^2 +
           (epsi_coherence_w2 - epsi_coherence_w3)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_34 = 
           (epsi_confusion_w3 - epsi_confusion_w4)^2 +
           (epsi_coherence_w3 - epsi_coherence_w4)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_all = 
           (epsi_confusion_w1 - epsi_confusion_w4)^2 +
           (epsi_coherence_w1 - epsi_coherence_w4)^2)

sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_confusion_w1)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_coherence_w1)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_confusion_w2)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_coherence_w2)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_confusion_w3)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_coherence_w3)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_confusion_w4)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_coherence_w4)


sim.identity <- sim.identity %>% 
  mutate(identity_dp_12 = 
           (epsi_confusion_w1_d - epsi_confusion_w2_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w2_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_23 = 
           (epsi_confusion_w2_d - epsi_confusion_w3_d)^2 +
           (epsi_coherence_w2_d - epsi_coherence_w3_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_34 = 
           (epsi_confusion_w3_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w3_d - epsi_coherence_w4_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_all = 
           (epsi_confusion_w1_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w4_d)^2)

sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w1_ds = epsi_confusion_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w1_ds = epsi_coherence_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w2_ds = epsi_confusion_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w2_ds = epsi_coherence_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w3_ds = epsi_confusion_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w3_ds = epsi_coherence_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w4_ds = epsi_confusion_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w4_ds = epsi_coherence_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))


sim.identity <- sim.identity %>% 
  mutate(identity_dpp_12 = 
           (epsi_confusion_w1_ds - epsi_confusion_w2_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w2_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_23 = 
           (epsi_confusion_w2_ds - epsi_confusion_w3_ds)^2 +
           (epsi_coherence_w2_ds - epsi_coherence_w3_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_34 = 
           (epsi_confusion_w3_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w3_ds - epsi_coherence_w4_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_all = 
           (epsi_confusion_w1_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w4_ds)^2)

# >> create 99.9% cutoff based on simulated data ----

cutoff_identity_d2_12 <- mean(sim.identity$identity_d2_12) + 3.291*(sd(sim.identity$identity_d2_12)/sqrt(50000))
cutoff_identity_d2_23 <- mean(sim.identity$identity_d2_23) + 3.291*(sd(sim.identity$identity_d2_23)/sqrt(50000))
cutoff_identity_d2_34 <- mean(sim.identity$identity_d2_34) + 3.291*(sd(sim.identity$identity_d2_34)/sqrt(50000))
cutoff_identity_d2_all <- mean(sim.identity$identity_d2_all) + 3.291*(sd(sim.identity$identity_d2_all)/sqrt(50000))
cutoff_identity_dp_12 <- mean(sim.identity$identity_dp_12) + 3.291*(sd(sim.identity$identity_dp_12)/sqrt(50000))
cutoff_identity_dp_23 <- mean(sim.identity$identity_dp_23) + 3.291*(sd(sim.identity$identity_dp_23)/sqrt(50000))
cutoff_identity_dp_34 <- mean(sim.identity$identity_dp_34) + 3.291*(sd(sim.identity$identity_dp_34)/sqrt(50000))
cutoff_identity_dp_all <- mean(sim.identity$identity_dp_all) + 3.291*(sd(sim.identity$identity_dp_all)/sqrt(50000))
cutoff_identity_dpp_12 <- mean(sim.identity$identity_dpp_12) + 3.291*(sd(sim.identity$identity_dpp_12)/sqrt(50000))
cutoff_identity_dpp_23 <- mean(sim.identity$identity_dpp_23) + 3.291*(sd(sim.identity$identity_dpp_23)/sqrt(50000))
cutoff_identity_dpp_34 <- mean(sim.identity$identity_dpp_34) + 3.291*(sd(sim.identity$identity_dpp_34)/sqrt(50000))
cutoff_identity_dpp_all <- mean(sim.identity$identity_dpp_all) + 3.291*(sd(sim.identity$identity_dpp_all)/sqrt(50000))

# >> create function for ipsative identity ----
ipsative_identity <- function(data) {
# >> compute D2 in original dataset ----

dp <- data %>% 
  dplyr::select(ID, bfas_agreeableness_w1:epsi_coherence_w4, epsi_confusion_w4)

data <- data %>% 
  mutate(identity_d2_12 = 
           (epsi_confusion_w1 - epsi_confusion_w2)^2 +
           (epsi_coherence_w1 - epsi_coherence_w2)^2)
data <- data %>% 
  mutate(identity_d2_23 = 
           (epsi_confusion_w2 - epsi_confusion_w3)^2 +
           (epsi_coherence_w2 - epsi_coherence_w3)^2)
data <- data %>% 
  mutate(identity_d2_34 = 
           (epsi_confusion_w3 - epsi_confusion_w4)^2 +
           (epsi_coherence_w3 - epsi_coherence_w4)^2)
data <- data %>% 
  mutate(identity_d2_all = 
           (epsi_confusion_w1 - epsi_confusion_w4)^2 +
           (epsi_coherence_w1 - epsi_coherence_w4)^2)

dp <- dp %>% 
  mutate(epsi_confusion_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_confusion_w1)
dp <- dp %>% 
  mutate(epsi_coherence_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_coherence_w1)
dp <- dp %>% 
  mutate(epsi_confusion_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_confusion_w2)
dp <- dp %>% 
  mutate(epsi_coherence_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_coherence_w2)
dp <- dp %>% 
  mutate(epsi_confusion_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_confusion_w3)
dp <- dp %>% 
  mutate(epsi_coherence_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_coherence_w3)
dp <- dp %>% 
  mutate(epsi_confusion_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_confusion_w4)
dp <- dp %>% 
  mutate(epsi_coherence_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_coherence_w4)


dp <- dp %>% 
  mutate(identity_dp_12 = 
           (epsi_confusion_w1_d - epsi_confusion_w2_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w2_d)^2)
dp <- dp %>% 
  mutate(identity_dp_23 = 
           (epsi_confusion_w2_d - epsi_confusion_w3_d)^2 +
           (epsi_coherence_w2_d - epsi_coherence_w3_d)^2)
dp <- dp %>% 
  mutate(identity_dp_34 = 
           (epsi_confusion_w3_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w3_d - epsi_coherence_w4_d)^2)
dp <- dp %>% 
  mutate(identity_dp_all = 
           (epsi_confusion_w1_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w4_d)^2)

dp <- dp %>% 
  mutate(epsi_confusion_w1_ds = epsi_confusion_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
dp <- dp %>% 
  mutate(epsi_coherence_w1_ds = epsi_coherence_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
dp <- dp %>% 
  mutate(epsi_confusion_w2_ds = epsi_confusion_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
dp <- dp %>% 
  mutate(epsi_coherence_w2_ds = epsi_coherence_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
dp <- dp %>% 
  mutate(epsi_confusion_w3_ds = epsi_confusion_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
dp <- dp %>% 
  mutate(epsi_coherence_w3_ds = epsi_coherence_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
dp <- dp %>% 
  mutate(epsi_confusion_w4_ds = epsi_confusion_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))
dp <- dp %>% 
  mutate(epsi_coherence_w4_ds = epsi_coherence_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))


dp <- dp %>% 
  mutate(identity_dpp_12 = 
           (epsi_confusion_w1_ds - epsi_confusion_w2_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w2_ds)^2)
dp <- dp %>% 
  mutate(identity_dpp_23 = 
           (epsi_confusion_w2_ds - epsi_confusion_w3_ds)^2 +
           (epsi_coherence_w2_ds - epsi_coherence_w3_ds)^2)
dp <- dp %>% 
  mutate(identity_dpp_34 = 
           (epsi_confusion_w3_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w3_ds - epsi_coherence_w4_ds)^2)
dp <- dp %>% 
  mutate(identity_dpp_all = 
           (epsi_confusion_w1_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w4_ds)^2)

dp <- dp %>% 
  dplyr::select(ID, identity_dp_12:identity_dp_all, identity_dpp_12:identity_dpp_all)
data <- merge(data, dp)


# >> compute percentage that show change greater than chance ----

data <- data %>% 
  mutate(identity_d2_12c =ifelse(
    identity_d2_12 > cutoff_identity_d2_12,
    1,
    0))
data <- data %>% 
  mutate(identity_d2_23c =ifelse(
    identity_d2_23 > cutoff_identity_d2_23,
    1,
    0))
data <- data %>% 
  mutate(identity_d2_34c =ifelse(
    identity_d2_34 > cutoff_identity_d2_34,
    1,
    0))
data <- data %>% 
  mutate(identity_d2_allc =ifelse(
    identity_d2_all > cutoff_identity_d2_all,
    1,
    0))
data <- data %>% 
  mutate(identity_dp_12c =ifelse(
    identity_dp_12 > cutoff_identity_dp_12,
    1,
    0))
data <- data %>% 
  mutate(identity_dp_23c =ifelse(
    identity_dp_23 > cutoff_identity_dp_23,
    1,
    0))
data <- data %>% 
  mutate(identity_dp_34c =ifelse(
    identity_dp_34 > cutoff_identity_dp_34,
    1,
    0))
data <- data %>% 
  mutate(identity_dp_allc =ifelse(
    identity_dp_all > cutoff_identity_dp_all,
    1,
    0))

data <- data %>% 
  mutate(identity_dpp_12c =ifelse(
    identity_dpp_12 > cutoff_identity_dpp_12,
    1,
    0))
data <- data %>% 
  mutate(identity_dpp_23c =ifelse(
    identity_dpp_23 > cutoff_identity_dpp_23,
    1,
    0))
data <- data %>% 
  mutate(identity_dpp_34c =ifelse(
    identity_dpp_34 > cutoff_identity_dpp_34,
    1,
    0))
data <- data %>% 
  mutate(identity_dpp_allc =ifelse(
    identity_dpp_all > cutoff_identity_dpp_all,
    1,
    0))
return (data.frame(name = c("identity_d2_12", "identity_d2_23", "identity_d2_34",
                            "identity_dp_12", "identity_dp_23", "identity_dp_34",
                            "identity_dpp_12", "identity_dpp_23", "identity_dpp_34", 
                            "identity_d2_all", "identity_dp_all", "identity_dpp_all"),
                   value = c(mean(data$identity_d2_12c, na.rm = TRUE), mean(data$identity_d2_23c, na.rm = TRUE),
                             mean(data$identity_d2_34c, na.rm = TRUE), mean(data$identity_dp_12c, na.rm = TRUE),
                             mean(data$identity_dp_23c, na.rm = TRUE), mean(data$identity_dp_34c, na.rm = TRUE),
                             mean(data$identity_dpp_12c, na.rm = TRUE), mean(data$identity_dpp_23c, na.rm = TRUE),
                             mean(data$identity_dpp_34c, na.rm = TRUE), mean(data$identity_d2_allc, na.rm = TRUE),
                             mean(data$identity_dp_allc, na.rm = TRUE), mean(data$identity_dpp_allc, na.rm = TRUE))))
}

# >> ipsative identity ----
ipsative_identity(selfw)
# >> imputations ----
# >>> compute for each imputed dataset ----
imp_ips_id1 <- ipsative_identity(selfw_1)
imp_ips_id2 <- ipsative_identity(selfw_2)
imp_ips_id3 <- ipsative_identity(selfw_3)
imp_ips_id4 <- ipsative_identity(selfw_4)
imp_ips_id5 <- ipsative_identity(selfw_5)
imp_ips_id6 <- ipsative_identity(selfw_6)
imp_ips_id7 <- ipsative_identity(selfw_7)
imp_ips_id8 <- ipsative_identity(selfw_8)
imp_ips_id9 <- ipsative_identity(selfw_9)
imp_ips_id10 <- ipsative_identity(selfw_10)
imp_ips_id11 <- ipsative_identity(selfw_11)
imp_ips_id12 <- ipsative_identity(selfw_12)
imp_ips_id13 <- ipsative_identity(selfw_13)
imp_ips_id14 <- ipsative_identity(selfw_14)
imp_ips_id15 <- ipsative_identity(selfw_15)
imp_ips_id16 <- ipsative_identity(selfw_16)
imp_ips_id17 <- ipsative_identity(selfw_17)
imp_ips_id18 <- ipsative_identity(selfw_18)
imp_ips_id19 <- ipsative_identity(selfw_19)
imp_ips_id20 <- ipsative_identity(selfw_20)
imp_ips_id <- rbind(imp_ips_id1, imp_ips_id2, imp_ips_id3, imp_ips_id4,
                    imp_ips_id5, imp_ips_id6, imp_ips_id7, imp_ips_id8,
                    imp_ips_id9, imp_ips_id10, imp_ips_id11, imp_ips_id12,
                    imp_ips_id13, imp_ips_id14, imp_ips_id15, imp_ips_id16,
                    imp_ips_id17, imp_ips_id18, imp_ips_id19, imp_ips_id20)

# >>> aggregating ipsative change across imputations ----
imp_ips_id %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()


# > SELF 5: Correlated change ----

slope.agree <- data.frame("id" = rownames(coef(linear.agree)$ID),
                          "slope.agree" = coef(linear.agree)$ID[,2])
slope.consci <- data.frame("id" = rownames(coef(linear.consci)$ID),
                          "slope.consci" = coef(linear.consci)$ID[,2])
slope.extra <- data.frame("id" = rownames(coef(linear.extra)$ID),
                           "slope.extra" = coef(linear.extra)$ID[,2])
slope.neuro <- data.frame("id" = rownames(coef(linear.neuro)$ID),
                          "slope.neuro" = coef(linear.neuro)$ID[,2])
slope.opend <- data.frame("id" = rownames(coef(linear.opend)$ID),
                          "slope.opend" = coef(linear.opend)$ID[,2])
slope.assert <- data.frame("id" = rownames(coef(linear.assert)$ID),
                           "slope.assert" = coef(linear.assert)$ID[,2])
slope.compa <- data.frame("id" = rownames(coef(linear.compa)$ID),
                           "slope.compa" = coef(linear.compa)$ID[,2])
slope.enthu <- data.frame("id" = rownames(coef(linear.enthu)$ID),
                          "slope.enthu" = coef(linear.enthu)$ID[,2])
slope.indus <- data.frame("id" = rownames(coef(linear.indus)$ID),
                          "slope.indus" = coef(linear.indus)$ID[,2])
slope.intel <- data.frame("id" = rownames(coef(linear.intel)$ID),
                          "slope.intel" = coef(linear.intel)$ID[,2])
slope.opena <- data.frame("id" = rownames(coef(linear.opena)$ID),
                          "slope.opena" = coef(linear.opena)$ID[,2])
slope.order <- data.frame("id" = rownames(coef(linear.order)$ID),
                          "slope.order" = coef(linear.order)$ID[,2])
slope.polit <- data.frame("id" = rownames(coef(linear.polit)$ID),
                          "slope.polit" = coef(linear.polit)$ID[,2])
slope.volat <- data.frame("id" = rownames(coef(linear.volat)$ID),
                          "slope.volat" = coef(linear.volat)$ID[,2])
slope.withd <- data.frame("id" = rownames(coef(linear.withd)$ID),
                          "slope.withd" = coef(linear.withd)$ID[,2])
slope.confu <- data.frame("id" = rownames(coef(linear.confu)$ID),
                          "slope.confu" = coef(linear.confu)$ID[,2])
slope.coher <- data.frame("id" = rownames(coef(linear.coher)$ID),
                          "slope.coher" = coef(linear.coher)$ID[,2])

slope <- merge(slope.agree, slope.consci) %>% 
  merge(slope.extra) %>% 
  merge(slope.neuro) %>% 
  merge(slope.opend) %>% 
  merge(slope.assert) %>% 
  merge(slope.compa) %>% 
  merge(slope.enthu) %>% 
  merge(slope.indus) %>% 
  merge(slope.intel) %>% 
  merge(slope.opena) %>% 
  merge(slope.order) %>% 
  merge(slope.polit) %>% 
  merge(slope.volat) %>% 
  merge(slope.withd) %>% 
  merge(slope.confu) %>% 
  merge(slope.coher)

rm(slope.agree, slope.consci, slope.extra, slope.neuro, slope.opend, slope.assert,
   slope.compa, slope.enthu, slope.indus, slope.intel, slope.opena, slope.order,
   slope.polit, slope.volat, slope.withd, slope.confu, slope.coher)

# EXPORT AND GRAPHS SELF ==========================================
# > Mean-level change ----
# >> Linear models ----
sink("linear.domain.txt")
print(summary(linear.agree))
print(summary(linear.consci))
print(summary(linear.extra))
print(summary(linear.neuro))
print(summary(linear.opend))
sink() 

sink("linear.aspect.txt")
print(summary(linear.compa))
print(summary(linear.polit))
print(summary(linear.indus))
print(summary(linear.order))
print(summary(linear.assert))
print(summary(linear.enthu))
print(summary(linear.volat))
print(summary(linear.withd))
print(summary(linear.opena))
print(summary(linear.intel))
sink() 

sink("linear.identity.txt")
print(summary(linear.confu))
print(summary(linear.coher))
sink() 

tab_model(linear.agree, linear.compa, linear.polit, file = "linear.agree.doc", digits = 3)
tab_model(linear.consci, linear.indus, linear.order, file = "linear.consci.doc", digits = 3)
tab_model(linear.extra, linear.assert, linear.enthu, file = "linear.extra.doc", digits = 3)
tab_model(linear.neuro, linear.volat, linear.withd, file = "linear.neuro.doc", digits = 3)
tab_model(linear.opend, linear.opena, linear.intel, file = "linear.opend.doc", digits = 3)
tab_model(linear.confu, linear.coher, file = "linear.epsi.doc", digits = 3)

jpeg("linear.agree.jpg", width = 400, height = 500)
plot_model(linear.agree,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Agreeableness"))
dev.off()

jpeg("linear.compa.jpg", width = 400, height = 500)
plot_model(linear.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Compassion"))
dev.off()

jpeg("linear.polit.jpg", width = 400, height = 500)
plot_model(linear.polit,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Politeness"))
dev.off()

jpeg("linear.consci.jpg", width = 400, height = 500)
plot_model(linear.consci,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Conscientiousness"))
dev.off()

jpeg("linear.indus.jpg", width = 400, height = 500)
plot_model(linear.indus,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Industriousness"))
dev.off()

jpeg("linear.order.jpg", width = 400, height = 500)
plot_model(linear.order,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Orderliness"))
dev.off()

jpeg("linear.extra.jpg", width = 400, height = 500)
plot_model(linear.extra,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Extraversion"))
dev.off()

jpeg("linear.assert.jpg", width = 400, height = 500)
plot_model(linear.assert,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Assertiveness"))
dev.off()

jpeg("linear.enthu.jpg", width = 400, height = 500)
plot_model(linear.enthu,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Enthusiasm"))
dev.off()

jpeg("linear.neuro.jpg", width = 400, height = 500)
plot_model(linear.neuro,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Neuroticism"))
dev.off()

jpeg("linear.volat.jpg", width = 400, height = 500)
plot_model(linear.volat,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Volatility"))
dev.off()

jpeg("linear.withd.jpg", width = 400, height = 500)
plot_model(linear.withd,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Withdrawal"))
dev.off()

jpeg("linear.opena.jpg", width = 400, height = 500)
plot_model(linear.opena,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Openness Aspect"))
dev.off()

jpeg("linear.opend.jpg", width = 400, height = 500)
plot_model(linear.opend,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Openness Domain"))
dev.off()

jpeg("linear.intel.jpg", width = 400, height = 500)
plot_model(linear.intel,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Intellect"))
dev.off()

jpeg("linear.confu.jpg", width = 400, height = 500)
plot_model(linear.confu,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Identity Confusion"))
dev.off()

jpeg("linear.coher.jpg", width = 400, height = 500)
plot_model(linear.coher,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Identity Coherence"))
dev.off()

# >> Quadratic models ----
sink("quad.domain.txt")
print(summary(quad.agree))
print(summary(quad.consci))
print(summary(quad.extra))
print(summary(quad.neuro))
print(summary(quad.opend))
sink() 

sink("quad.aspect.txt")
print(summary(quad.compa))
print(summary(quad.polit))
print(summary(quad.indus))
print(summary(quad.order))
print(summary(quad.assert))
print(summary(quad.enthu))
print(summary(quad.volat))
print(summary(quad.withd))
print(summary(quad.opena))
print(summary(quad.intel))
sink() 

sink("quad.identity.txt")
print(summary(quad.confu))
print(summary(quad.coher))
sink() 

tab_model(quad.agree, quad.compa, quad.polit, file = "quad.agree.doc", digits = 3)
tab_model(quad.consci, quad.indus, quad.order, file = "quad.consci.doc", digits = 3)
tab_model(quad.extra, quad.assert, quad.enthu, file = "quad.extra.doc", digits = 3)
tab_model(quad.neuro, quad.volat, quad.withd, file = "quad.neuro.doc", digits = 3)
tab_model(quad.opend, quad.opena, quad.intel, file = "quad.opend.doc", digits = 3)
tab_model(quad.confu, quad.coher, file = "quad.epsi.doc", digits = 3)

#plots of significant quadratic variables
p1 <- plot_model(quad.extra,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Extraversion"))
p2 <- plot_model(quad.opend,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Openness Domain"))
p3 <- plot_model(quad.assert,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Assertiveness"))
p4 <- plot_model(quad.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Compassion"))
p5 <- plot_model(quad.enthu,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Enthusiasm"))
p6 <- plot_model(quad.intel,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Intellect"))
jpeg("sig.quad.self.jpg", width = 700, height = 500)
cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3)
dev.off()

# >> Compare linear vs. quad ----
sink("compare.domain.txt")
print(anova(linear.agree, quad.agree))
print(anova(linear.consci, quad.consci))
print(anova(linear.extra, quad.extra))
print(anova(linear.neuro, quad.neuro))
print(anova(linear.opend, quad.opend))
sink() 

sink("compare.aspect.txt")
print(anova(linear.assert, quad.assert))
print(anova(linear.compa, quad.compa))
print(anova(linear.enthu, quad.enthu))
print(anova(linear.indus, quad.indus))
print(anova(linear.intel, quad.intel))
print(anova(linear.opena, quad.opena))
print(anova(linear.order, quad.order))
print(anova(linear.polit, quad.polit))
print(anova(linear.volat, quad.volat))
print(anova(linear.withd, quad.withd))
sink() 

sink("compare.identity.txt")
print(anova(linear.confu, quad.confu))
print(anova(linear.coher, quad.coher))
sink() 


jpeg("quad.agree.jpg", width = 400, height = 500)
plot_model(quad.agree,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.compa.jpg", width = 400, height = 500)
plot_model(quad.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.polit.jpg", width = 400, height = 500)
plot_model(quad.polit,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.consci.jpg", width = 400, height = 500)
plot_model(quad.consci,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.indus.jpg", width = 400, height = 500)
plot_model(quad.indus,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.order.jpg", width = 400, height = 500)
plot_model(quad.order,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.extra.jpg", width = 400, height = 500)
plot_model(quad.extra,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.assert.jpg", width = 400, height = 500)
plot_model(quad.assert,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.enthu.jpg", width = 400, height = 500)
plot_model(quad.enthu,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.neuro.jpg", width = 400, height = 500)
plot_model(quad.neuro,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("quad.volat.jpg", width = 400, height = 500)
plot_model(quad.volat,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("quad.withd.jpg", width = 400, height = 500)
plot_model(quad.withd,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("quad.opena.jpg", width = 400, height = 500)
plot_model(quad.opena,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.opend.jpg", width = 400, height = 500)
plot_model(quad.opend,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.intel.jpg", width = 400, height = 500)
plot_model(quad.intel,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("quad.confu.jpg", width = 400, height = 500)
plot_model(quad.confu,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("quad.coher.jpg", width = 400, height = 500)
plot_model(quad.coher,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

# >> MI linear ----
sink("MI.self.txt")
print(testEstimates(linear.imp.agree, var.comp=TRUE))
print(testEstimates(linear.imp.assert, var.comp=TRUE))
print(testEstimates(linear.imp.compa, var.comp=TRUE))
print(testEstimates(linear.imp.consci, var.comp=TRUE))
print(testEstimates(linear.imp.enthu, var.comp=TRUE))
print(testEstimates(linear.imp.extra, var.comp=TRUE))
print(testEstimates(linear.imp.indus, var.comp=TRUE))
print(testEstimates(linear.imp.intel, var.comp=TRUE))
print(testEstimates(linear.imp.neuro, var.comp=TRUE))
print(testEstimates(linear.imp.opena, var.comp=TRUE))
print(testEstimates(linear.imp.opend, var.comp=TRUE))
print(testEstimates(linear.imp.order, var.comp=TRUE))
print(testEstimates(linear.imp.polit, var.comp=TRUE))
print(testEstimates(linear.imp.volat, var.comp=TRUE))
print(testEstimates(linear.imp.withd, var.comp=TRUE))
sink() 

sink("MI.self.ID.txt")
print(testEstimates(linear.imp.confu, var.comp=TRUE))
print(testEstimates(linear.imp.coher, var.comp=TRUE))
sink()

# > Rank-order change ----
# >> Correlation between different waves ----
selfw %>% 
  dplyr::select(bfas_agreeableness_w1, bfas_agreeableness_w2, bfas_agreeableness_w3, bfas_agreeableness_w4) %>% 
  apa.cor.table(filename = 'rankagree.doc')
selfw %>% 
  dplyr::select(bfas_conscientiousness_w1, bfas_conscientiousness_w2, bfas_conscientiousness_w3, bfas_conscientiousness_w4) %>% 
  apa.cor.table(filename = 'rankconsci.doc')
selfw %>% 
  dplyr::select(bfas_extraversion_w1, bfas_extraversion_w2, bfas_extraversion_w3, bfas_extraversion_w4) %>% 
  apa.cor.table(filename = 'rankextra.doc')
selfw %>% 
  dplyr::select(bfas_neuroticism_w1, bfas_neuroticism_w2, bfas_neuroticism_w3, bfas_neuroticism_w4) %>% 
  apa.cor.table(filename = 'rankneuro.doc')
selfw %>% 
  dplyr::select(bfas_opennessdomain_w1, bfas_opennessdomain_w2, bfas_opennessdomain_w3, bfas_opennessdomain_w4) %>% 
  apa.cor.table(filename = 'rankopend.doc')
selfw %>% 
  dplyr::select(bfas_assertiveness_w1, bfas_assertiveness_w2, bfas_assertiveness_w3, bfas_assertiveness_w4) %>% 
  apa.cor.table(filename = 'rankassert.doc')
selfw %>% 
  dplyr::select(bfas_compassion_w1, bfas_compassion_w2, bfas_compassion_w3, bfas_compassion_w4) %>% 
  apa.cor.table(filename = 'rankcompa.doc')
selfw %>% 
  dplyr::select(bfas_enthusiasm_w1, bfas_enthusiasm_w2, bfas_enthusiasm_w3, bfas_enthusiasm_w4) %>% 
  apa.cor.table(filename = 'rankenthu.doc')
selfw %>% 
  dplyr::select(bfas_industriousness_w1, bfas_industriousness_w2, bfas_industriousness_w3, bfas_industriousness_w4) %>% 
  apa.cor.table(filename = 'rankindus.doc')
selfw %>% 
  dplyr::select(bfas_intellect_w1, bfas_intellect_w2, bfas_intellect_w3, bfas_intellect_w4) %>% 
  apa.cor.table(filename = 'rankintel.doc')
selfw %>% 
  dplyr::select(bfas_opennessaspect_w1, bfas_opennessaspect_w2, bfas_opennessaspect_w3, bfas_opennessaspect_w4) %>% 
  apa.cor.table(filename = 'rankopena.doc')
selfw %>% 
  dplyr::select(bfas_orderliness_w1, bfas_orderliness_w2, bfas_orderliness_w3, bfas_orderliness_w4) %>% 
  apa.cor.table(filename = 'rankorder.doc')
selfw %>% 
  dplyr::select(bfas_politeness_w1, bfas_politeness_w2, bfas_politeness_w3, bfas_politeness_w4) %>% 
  apa.cor.table(filename = 'rankpolit.doc')
selfw %>% 
  dplyr::select(bfas_volatility_w1, bfas_volatility_w2, bfas_volatility_w3, bfas_volatility_w4) %>% 
  apa.cor.table(filename = 'rankvolat.doc')
selfw %>% 
  dplyr::select(bfas_withdrawal_w1, bfas_withdrawal_w2, bfas_withdrawal_w3, bfas_withdrawal_w4) %>% 
  apa.cor.table(filename = 'rankwithd.doc')
selfw %>% 
  dplyr::select(epsi_confusion_w1, epsi_confusion_w2, epsi_confusion_w3, epsi_confusion_w4) %>% 
  apa.cor.table(filename = 'rankconfu.doc')
selfw %>% 
  dplyr::select(epsi_coherence_w1, epsi_coherence_w2, epsi_coherence_w3, epsi_coherence_w4) %>% 
  apa.cor.table(filename = 'rankcoher.doc')

# >> Rank-order over time ----
rank_domain <- data.frame(
  "T1-T2.vs.T2-T3" = c(agree_12_23, consci_12_23, extra_12_23, neuro_12_23, opend_12_23),
  "p-value" = c(p_agree_12_23, p_consci_12_23, p_extra_12_23, p_neuro_12_23, p_opend_12_23),
  "T2-T3.vs.T3-T4" = c(agree_23_34, consci_23_34, extra_23_34, neuro_23_34, opend_23_34),
  "p-value" = c(p_agree_23_34, p_consci_23_34, p_extra_23_34, p_neuro_23_34, p_opend_23_34),
  "T1-T2.vs.T3-T4" = c(agree_12_34, consci_12_34, extra_12_34, neuro_12_34, opend_12_34),
  "p-value" = c(p_agree_12_34, p_consci_12_34, p_extra_12_34, p_neuro_12_34, p_opend_12_34))

rank_aspect <- data.frame(
  "T1-T2.vs.T2-T3" = c(assert_12_23, compa_12_23, enthu_12_23, indus_12_23, intel_12_23,
                       opena_12_23, order_12_23, polit_12_23, volat_12_23, withd_12_23),
  "p-value" = c(p_assert_12_23, p_compa_12_23, p_enthu_12_23, p_indus_12_23, p_intel_12_23,
                p_opena_12_23, p_order_12_23, p_polit_12_23, p_volat_12_23, p_withd_12_23),
  "T2-T3.vs.T3-T4" = c(assert_23_34, compa_23_34, enthu_23_34, indus_23_34, intel_23_34,
                     opena_23_34, order_23_34, polit_23_34, volat_23_34, withd_23_34),
  "p-value" = c(p_assert_23_34, p_compa_23_34, p_enthu_23_34, p_indus_23_34, p_intel_23_34,
                p_opena_23_34, p_order_23_34, p_polit_23_34, p_volat_23_34, p_withd_23_34),
  "T1-T2.vs.T3-T4" = c(assert_12_34, compa_12_34, enthu_12_34, indus_12_34, intel_12_34,
                       opena_12_34, order_12_34, polit_12_34, volat_12_34, withd_12_34),
  "p-value" = c(p_assert_12_34, p_compa_12_34, p_enthu_12_34, p_indus_12_34, p_intel_12_34,
                p_opena_12_34, p_order_12_34, p_polit_12_34, p_volat_12_34, p_withd_12_34))

rank_identity <- data.frame(
  "T1-T2.vs.T2-T3" = c(coher_12_23, confu_12_23),
  "p-value" = c(p_coher_12_23, p_confu_12_23),
  "T2-T3.vs.T3-T4" = c(coher_23_34, confu_23_34),
  "p-value" = c(p_coher_23_34, p_confu_23_34),
  "T1-T2.vs.T3-T4" = c(coher_12_34, confu_12_34),
  "p-value" = c(p_coher_12_34, p_confu_12_34))

write.csv(rank_domain, "rank.domain.csv")
write.csv(rank_aspect, "rank.aspect.csv")
write.csv(rank_identity, "rank.identity.csv")

# >> Temporal decay curve ----
rank <- c(r12_agree, r12_assert, r12_compa, r12_consci, r12_enthu,
          r12_extra, r12_indus, r12_intel, r12_neuro, r12_opena,
          r12_opend, r12_order, r12_polit, r12_volat, r12_withd,
          r23_agree, r23_assert, r23_compa, r23_consci, r23_enthu,
          r23_extra, r23_indus, r23_intel, r23_neuro, r23_opena,
          r23_opend, r23_order, r23_polit, r23_volat, r23_withd,
          r34_agree, r34_assert, r34_compa, r34_consci, r34_enthu,
          r34_extra, r34_indus, r34_intel, r34_neuro, r34_opena,
          r34_opend, r34_order, r34_polit, r34_volat, r34_withd,
          r14_agree, r14_assert, r14_compa, r14_consci, r14_enthu,
          r14_extra, r14_indus, r14_intel, r14_neuro, r14_opena,
          r14_opend, r14_order, r14_polit, r14_volat, r14_withd,
          r13_agree, r13_assert, r13_compa, r13_consci, r13_enthu,
          r13_extra, r13_indus, r13_intel, r13_neuro, r13_opena,
          r13_opend, r13_order, r13_polit, r13_volat, r13_withd,
          r24_agree, r24_assert, r24_compa, r24_consci, r24_enthu,
          r24_extra, r24_indus, r24_intel, r24_neuro, r24_opena,
          r24_opend, r24_order, r24_polit, r24_volat, r24_withd)
time <- c("r12", "r12", "r12", "r12", "r12",
          "r12", "r12", "r12", "r12", "r12",
          "r12", "r12", "r12", "r12", "r12",
          "r23", "r23", "r23", "r23", "r23",
          "r23", "r23", "r23", "r23", "r23",
          "r23", "r23", "r23", "r23", "r23",
          "r34", "r34", "r34", "r34", "r34",
          "r34", "r34", "r34", "r34", "r34",
          "r34", "r34", "r34", "r34", "r34",
          "r14", "r14", "r14", "r14", "r14",
          "r14", "r14", "r14", "r14", "r14",
          "r14", "r14", "r14", "r14", "r14",
          "r13", "r13", "r13", "r13", "r13",
          "r13", "r13", "r13", "r13", "r13",
          "r13", "r13", "r13", "r13", "r13",
          "r24", "r24", "r24", "r24", "r24",
          "r24", "r24", "r24", "r24", "r24",
          "r24", "r24", "r24", "r24", "r24")
trait <- rep(c("agree", "assert", "compa", "consci", "enthu",
           "extra", "indus", "intel", "neuro", "opena",
           "opend", "order", "polit", "volat", "withd"), times = 6)
time1 <- as.factor(rep(c(1,2,3,1,1,2), each = 15))
time2 <- as.factor(rep(c(2,3,4,4,3,4), each = 15))
rank <- data.frame(time, trait, rank, time1, time2)

ggplot(data = rank[which(rank$trait == "agree"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Agreeableness")
ggsave("agree.png")

ggplot(data = rank[which(rank$trait == "consci"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Conscientiousess")
ggsave("consci.png")

ggplot(data = rank[which(rank$trait == "extra"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Extraversion")
ggsave("extra.png")

ggplot(data = rank[which(rank$trait == "neuro"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Neuroticism")
ggsave("neuro.png")

ggplot(data = rank[which(rank$trait == "opend"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Openness Domain")
ggsave("opend.png")

ggplot(data = rank[which(rank$trait == "assert"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Assertiveness")
ggsave("assert.png")

ggplot(data = rank[which(rank$trait == "compa"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Compassion")
ggsave("compa.png")

ggplot(data = rank[which(rank$trait == "enthu"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Enthusiasm")
ggsave("enthu.png")

ggplot(data = rank[which(rank$trait == "indus"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Industriousness")
ggsave("indus.png")

ggplot(data = rank[which(rank$trait == "intel"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Intellect")
ggsave("intel.png")

ggplot(data = rank[which(rank$trait == "opena"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Openness Aspect")
ggsave("opena.png")

ggplot(data = rank[which(rank$trait == "order"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Orderliness")
ggsave("order.png")

ggplot(data = rank[which(rank$trait == "polit"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Politeness")
ggsave("polit.png")

ggplot(data = rank[which(rank$trait == "volat"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Volatility")
ggsave("volat.png")

ggplot(data = rank[which(rank$trait == "withd"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Withdrawal")
ggsave("withd.png")

#plot sig temporal decay with a grid
{
g1 <- ggplot(data = rank[which(rank$trait == "agree"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Agreeableness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g2 <- ggplot(data = rank[which(rank$trait == "consci"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Conscientiousness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g3 <- ggplot(data = rank[which(rank$trait == "opend"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Openness Domain")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g4 <- ggplot(data = rank[which(rank$trait == "assert"),],
             aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Assertiveness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g5 <- ggplot(data = rank[which(rank$trait == "compa"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Compassion")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g6 <- ggplot(data = rank[which(rank$trait == "enthu"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Enthusiasm")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g7 <- ggplot(data = rank[which(rank$trait == "indus"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Industiousness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g8 <- ggplot(data = rank[which(rank$trait == "order"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Orderliness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

g9 <- ggplot(data = rank[which(rank$trait == "polit"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Politeness")+
  xlab("Time Point")+
  theme_classic() +
  theme(legend.position="none")

leg <- ggplot(data = rank[which(rank$trait == "polit"),],
                            aes(y = rank, x = time2, group = time1, color = time1)) +
                       geom_line()+
                       geom_point()+
                       ylim(0.4,1) +
                       ylab("Politeness")+
                       xlab("Time Point")+
                       theme_classic()

legend <- get_legend(leg + theme(legend.box.margin = margin(0,0,0,12)))

jpeg("sig.decay.self.jpg", width = 700, height = 700)
cowplot::plot_grid(plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,g9, nrow = 3, ncol = 3), legend, rel_widths = c(3,.4))
dev.off()

}

rank <- c(r12_confu, r12_coher, r23_confu, r23_coher, r34_confu, r34_coher,
          r14_confu, r14_coher, r13_confu, r13_coher, r24_confu, r24_coher)
time <- c("r12", "r12","r23","r23","r34", "r34",
          "r14", "r14","r13","r13","r24", "r24")
trait <- rep(c("confu", "coher"), times = 6)
time1 <- as.factor(rep(c(1,2,3,1,1,2), each = 2))
time2 <- as.factor(rep(c(2,3,4,4,3,4), each = 2))
rank <- data.frame(time, trait, rank, time1, time2)

g1 <- ggplot(data = rank[which(rank$trait == "confu"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Identity Confusion")+
  xlab("Time Point")+
  theme_classic() +
  theme(legend.position="none")
ggsave("confu.png")

g2 <- ggplot(data = rank[which(rank$trait == "coher"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Identity Coherence")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
ggsave("coher.png")

cowplot::plot_grid(plot_grid(g1,g2, nrow = 1), legend, rel_widths = c(3,.4))



# >> Forest plots ----
# >>> domains
r12 <- rbind(r12_agree, 
             r12_consci,
             r12_extra, 
             r12_neuro, 
             r12_opend) 
r34 <- rbind(r34_agree, 
             r34_consci,
             r34_extra, 
             r34_neuro, 
             r34_opend)

rcon12 <- cbind(rcon12agree, 
                rcon12consci,
                rcon12extra, 
                rcon12neuro, 
                rcon12opend)
rcon34 <- cbind(rcon34agree, 
                rcon34consci,
                rcon34extra, 
                rcon34neuro, 
                rcon34opend) 

tabletext <- cbind(
  c("Agreeableness", "Conscientiousness", "Extraversion",
    "Neuroticism", "Openness Domain"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in personality domains",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))
           

# >>> aspects
r12 <- rbind(r12_compa, r12_polit,
             r12_indus, r12_order,
             r12_assert, r12_enthu,
             r12_volat, r12_withd,
             r12_intel, r12_opena)
r34 <- rbind(r34_compa, r34_polit,
             r34_indus, r34_order,
             r34_assert, r34_enthu,
             r34_volat, r34_withd,
             r34_intel, r34_opena)


rcon12 <- cbind(rcon12compa, rcon12polit,
                rcon12indus, rcon12order,
                rcon12assert, rcon12enthu,
                rcon12volat, rcon12withd,
                rcon12intel, rcon12opena)

rcon34 <- cbind(rcon34compa, rcon34polit,
                rcon34indus, rcon34order,
                rcon34assert, rcon34enthu,
                rcon34volat, rcon34withd,
                rcon34intel, rcon34opena)

tabletext <- cbind(
  c("Agreeableness","","Conscientiousness","","Extraversion","", "Neuroticism",""),
  c("Compassion", "Politeness", "Industriousness", "Orderliness", 
    "Assertiveness","Enthusiasm","Volatility","Withdrawal","Intellect","Openness Aspect"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in personality aspects",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))

# >>> identity
r12 <- rbind(r12_coher, r12_confu)
r34 <- rbind(r34_coher, r34_confu)


rcon12 <- cbind(rcon12coher, rcon12confu)
rcon34 <- cbind(rcon34coher, rcon34confu)

tabletext <- cbind(
  c("Identity Coherence", "Identity Confusion"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in identity variables",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))

# >> MI rank-order ----
rank_domain_imp <- data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_agree_12_23$value), mean(imp_consci_12_23$value), mean(imp_extra_12_23$value), 
                       mean(imp_neuro_12_23$value), mean(imp_opend_12_23$value)),
  "p-value" = c(mean(imp_agree_12_23$p), mean(imp_consci_12_23$p), mean(imp_extra_12_23$p),
                mean(imp_neuro_12_23$p), mean(imp_opend_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_agree_23_34$value), mean(imp_consci_23_34$value), mean(imp_extra_23_34$value), 
                       mean(imp_neuro_23_34$value), mean(imp_opend_23_34$value)),
  "p-value" = c(mean(imp_agree_23_34$p), mean(imp_consci_23_34$p), mean(imp_extra_23_34$p),
                mean(imp_neuro_23_34$p), mean(imp_opend_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_agree_12_34$value), mean(imp_consci_12_34$value), mean(imp_extra_12_34$value), 
                       mean(imp_neuro_12_34$value), mean(imp_opend_12_34$value)),
  "p-value" = c(mean(imp_agree_12_34$p), mean(imp_consci_12_34$p), mean(imp_extra_12_34$p),
                mean(imp_neuro_12_34$p), mean(imp_opend_12_34$p)))


rank_aspect_imp <-  data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_assert_12_23$value), mean(imp_compa_12_23$value), 
                       mean(imp_enthu_12_23$value), mean(imp_indus_12_23$value), 
                       mean(imp_intel_12_23$value), mean(imp_opena_12_23$value),
                       mean(imp_order_12_23$value), mean(imp_polit_12_23$value), 
                       mean(imp_volat_12_23$value),mean(imp_withd_12_23$value)),
  "p-value" = c(mean(imp_assert_12_23$p), mean(imp_compa_12_23$p),
                mean(imp_enthu_12_23$p), mean(imp_indus_12_23$p), 
                mean(imp_intel_12_23$p), mean(imp_opena_12_23$p),
                mean(imp_order_12_23$p), mean(imp_polit_12_23$p), 
                mean(imp_volat_12_23$p),mean(imp_withd_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_assert_23_34$value), mean(imp_compa_23_34$value), 
                       mean(imp_enthu_23_34$value), mean(imp_indus_23_34$value), 
                       mean(imp_intel_23_34$value), mean(imp_opena_23_34$value),
                       mean(imp_order_23_34$value), mean(imp_polit_23_34$value), 
                       mean(imp_volat_23_34$value),mean(imp_withd_23_34$value)),
  "p-value" = c(mean(imp_assert_23_34$p), mean(imp_compa_23_34$p),
                mean(imp_enthu_23_34$p), mean(imp_indus_23_34$p), 
                mean(imp_intel_23_34$p), mean(imp_opena_23_34$p),
                mean(imp_order_23_34$p), mean(imp_polit_23_34$p), 
                mean(imp_volat_23_34$p),mean(imp_withd_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_assert_12_34$value), mean(imp_compa_12_34$value),
                       mean(imp_enthu_12_34$value), mean(imp_indus_12_34$value), 
                       mean(imp_intel_12_34$value), mean(imp_opena_12_34$value),
                       mean(imp_order_12_34$value), mean(imp_polit_12_34$value), 
                       mean(imp_volat_12_34$value),mean(imp_withd_12_34$value)),
  "p-value" = c(mean(imp_assert_12_34$p), mean(imp_compa_12_34$p),
                mean(imp_enthu_12_34$p), mean(imp_indus_12_34$p), 
                mean(imp_intel_12_34$p), mean(imp_opena_12_34$p),
                mean(imp_order_12_34$p), mean(imp_polit_12_34$p), 
                mean(imp_volat_12_34$p),mean(imp_withd_12_34$p)))

rank_identity_imp <- data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_coher_12_23$value), mean(imp_confu_12_23$value)),
  "p-value" = c(mean(imp_coher_12_23$p), mean(imp_confu_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_coher_23_34$value), mean(imp_confu_23_34$value)),
  "p-value" = c(mean(imp_coher_23_34$p), mean(imp_confu_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_coher_12_34$value), mean(imp_confu_12_34$value)),
  "p-value" = c(mean(imp_coher_12_34$p), mean(imp_confu_12_34$p)))

write.csv(rank_domain_imp, "rank_domain_imp.csv")
write.csv(rank_aspect_imp, "rank_aspect_imp.csv")
write.csv(rank_identity_imp, "rank_identity_imp.csv")

# > Individual differences ----

sink("indi.domain.txt")
print(anova(linearint.agree, linear.agree))
print(anova(linearint.consci, linear.consci))
print(anova(linearint.extra, linear.extra))
print(anova(linearint.neuro, linear.neuro))
print(anova(linearint.opend, linear.opend))
sink() 

sink("indi.aspect.txt")
print(anova(linearint.assert, linear.assert))
print(anova(linearint.compa, linear.compa))
print(anova(linearint.enthu, linear.enthu))
print(anova(linearint.indus, linear.indus))
print(anova(linearint.intel, linear.intel))
print(anova(linearint.opena, linear.opena))
print(anova(linearint.order, linear.order))
print(anova(linearint.polit, linear.polit))
print(anova(linearint.volat, linear.volat))
print(anova(linearint.withd, linear.withd))
sink() 

sink("indi.identity.txt")
print(anova(linearint.coher, linear.coher))
print(anova(linearint.confu, linear.confu))
sink() 

ggplot(data = selfl,
       aes(x = time, y = bfas_agreeableness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.agree.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_assertiveness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.assert.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_compassion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.compa.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_conscientiousness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.consci.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_enthusiasm, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.enthu.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_extraversion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.extra.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_industriousness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.indus.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_intellect, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.intel.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_neuroticism, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.neuro.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_opennessaspect, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.opena.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_opennessdomain, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.opend.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_orderliness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.order.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_politeness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.polit.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_volatility, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.volat.png")

ggplot(data = selfl,
       aes(x = time, y = bfas_withdrawal, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.withd.png")

ggplot(data = selfl,
       aes(x = time, y = epsi_coherence, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.coher.png")

ggplot(data = selfl,
       aes(x = time, y = epsi_confusion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("linear.confu.png")






# > Ipsative change ----
var <- rep(c("Domain","Aspect","Identity"), each = 12)
type <- rep(rep(c("D2", "D2p","D2pp"), each = 4), times = 3)
time <- rep(c("Time 1 to 2", "Time 2 to 3", "Time 3 to 4", "Overall"), times = 9)
percent <- c(43.2, 32.8, 29.4, 50.3, 43.2, 31.1, 26.6, 51.0,
             34.7, 21.5, 23.9, 34.7, 37.9, 26.0, 23.8, 50.3,
             37.4, 22.6, 24.5, 53.7, 39.5, 27.1, 27.5, 52.4,
             21.4, 25.1, 29.7, 27.3, 27.6, 24.6, 25.5, 28.7,
             17.2, 12.6, 14.2, 19.7)
      
data <- data.frame(var, type, time, percent)

write.csv(data, "profile.csv")

d2 <- data %>% 
  dplyr::filter(type == "D2")

ggplot(d2, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Raw scores: Elevation, Scatter, Shape")
ggsave("raw.png")

dp <- data %>% 
  dplyr::filter(type == "D2p")


ggplot(dp, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Deviation scores: Scatter and Shape")
ggsave("deviation.png")

dpp <- data %>% 
  dplyr::filter(type == "D2pp")


ggplot(dpp, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Standardized scores: Shape")
ggsave("stadard.png")

# >> MI ipsative ----
imp_ips_pers <-imp_ips_pers %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

write.csv(imp_ips_pers, "MI ips personality.csv")

imp_ips_id <- imp_ips_id %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

write.csv(imp_ips_id, "MI ips identity.csv")

# > Correlated change ----
slope %>% 
  dplyr::select(-id) %>% 
  apa.cor.table(filename = 'corchange.doc')

# ANALYSIS PEER  ==========================================

# > PEER 1a: Mean level change personality ----
# >> Linear mixed model with random intercept and random slope ----

### Agreeableness
linear.agree <- lmer(bfas_agreeableness ~ time +
                            (1 + time | ID),
                          control = lmerControl(optimizer ="Nelder_Mead"),
                          data = peerl)
summary(linear.agree)

### Assertiveness
linear.assert <- lmer(bfas_assertiveness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.assert)

### Compassion
linear.compa <- lmer(bfas_compassion ~ time +
                        (1 + time | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = peerl)
summary(linear.compa)

### Conscientiousness
linear.consci <- lmer(bfas_conscientiousness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.consci)

### Enthusiasm
linear.enthu <- lmer(bfas_enthusiasm ~ time +
                        (1 + time | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = peerl)
summary(linear.enthu)

### Extraversion
linear.extra <- lmer(bfas_extraversion ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.extra)

### Industriousness 
linear.indus <- lmer(bfas_industriousness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.indus)

### Intellect
linear.intel <- lmer(bfas_intellect ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.intel)

### Neuroticism
linear.neuro <- lmer(bfas_neuroticism ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.neuro)

### Openness Aspect
linear.opena <- lmer(bfas_opennessaspect ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.opena)

### Openness Domain
linear.opend <- lmer(bfas_opennessdomain ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.opend)

### Orderlines
linear.order <- lmer(bfas_orderliness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.order)

### Politeness
linear.polit <- lmer(bfas_politeness ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.polit)

### Volatility
linear.volat <- lmer(bfas_volatility ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.volat)

### Withdrawal
linear.withd <- lmer(bfas_withdrawal ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.withd)

# >> MI - Linear mixed model random intercept and random slope ----
linear.imp.agree <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_agreeableness ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.agree, var.comp=TRUE)

linear.imp.assert <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_assertiveness ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.assert, var.comp=TRUE)

linear.imp.compa <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_compassion ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.compa, var.comp=TRUE)

linear.imp.consci <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_conscientiousness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.consci, var.comp=TRUE)

linear.imp.enthu <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_enthusiasm ~ time + 
                                             (1+time | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.enthu, var.comp=TRUE)

linear.imp.extra <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_extraversion ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.extra, var.comp=TRUE)

linear.imp.indus <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_industriousness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.indus, var.comp=TRUE)

linear.imp.intel <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_intellect ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.intel, var.comp=TRUE)

linear.imp.neuro <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_neuroticism ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.neuro, var.comp=TRUE)

linear.imp.opena <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_opennessaspect ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.opena, var.comp=TRUE)

linear.imp.opend <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_opennessdomain ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.opend, var.comp=TRUE)

linear.imp.order <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_orderliness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.order, var.comp=TRUE)

linear.imp.polit <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_politeness ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.polit, var.comp=TRUE)

linear.imp.volat <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_volatility ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.volat, var.comp=TRUE)

linear.imp.withd <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_withdrawal ~ time + 
                                            (1+time | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
testEstimates(linear.imp.withd, var.comp=TRUE)

# >> Quadratic mixed model with random intercept and random slope, no random quad slope ----

### Agreeableness
quad.agree <- lmer(bfas_agreeableness ~ poly(time, degree = 2, raw = TRUE) +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(quad.agree)

### Assertiveness
quad.assert <- lmer(bfas_assertiveness ~ poly(time, degree = 2, raw = TRUE) +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.assert)

### Compassion
quad.compa <- lmer(bfas_compassion ~ poly(time, degree = 2, raw = TRUE)  +
                      (1 + time | ID),
                    control = lmerControl(optimizer ="Nelder_Mead"),
                    data = peerl)
summary(quad.compa)

### Conscientiousness
quad.consci <- lmer(bfas_conscientiousness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.consci)

### Enthusiasm
quad.enthu <- lmer(bfas_enthusiasm ~ poly(time, degree = 2, raw = TRUE)  +
                      (1 + time | ID),
                    control = lmerControl(optimizer ="Nelder_Mead"),
                    data = peerl)
summary(quad.enthu)

### Extraversion
quad.extra <- lmer(bfas_extraversion ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.extra)

### Industriousness
quad.indus <- lmer(bfas_industriousness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.indus)

### Intellect
quad.intel <- lmer(bfas_intellect ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.intel)

### Neuroticism
quad.neuro <- lmer(bfas_neuroticism ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.neuro)

### Openness Aspect
quad.opena <- lmer(bfas_opennessaspect ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.opena)

### Openness Domain
quad.opend <- lmer(bfas_opennessdomain ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.opend)

### Oderliness
quad.order <- lmer(bfas_orderliness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.order)

### Politeness
quad.polit <- lmer(bfas_politeness ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.polit)

### Volatility
quad.volat <- lmer(bfas_volatility ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.volat)

### Withdrawal
quad.withd <- lmer(bfas_withdrawal ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time | ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.withd)

# >> Model comparison (using original data) ----

anova(linear.agree, quad.agree)
anova(linear.assert, quad.assert)
anova(linear.compa, quad.compa) #quad >
anova(linear.consci, quad.consci)
anova(linear.enthu, quad.enthu)
anova(linear.extra, quad.extra) 
anova(linear.indus, quad.indus)
anova(linear.intel, quad.intel) 
anova(linear.neuro, quad.neuro)
anova(linear.opena, quad.opena)
anova(linear.opend, quad.opend)
anova(linear.order, quad.order) #quad >
anova(linear.polit, quad.polit)
anova(linear.volat, quad.volat)
anova(linear.withd, quad.withd)

# >> Linear mixed model with random intercept and no random slope ----

### Agreeableness
linearint.imp.agree <- with(data = imp_long_p,
                     exp = lme4::lmer(bfas_agreeableness ~ time + 
                                        (1| ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))

summary(pool(linearint.imp.agree))

linearint.agree <- lmer(bfas_agreeableness ~ time +
                            (1 | ID),
                          control = lmerControl(optimizer ="Nelder_Mead"),
                          data = peerl)
summary(linearint.agree)

### Assertiveness
linearint.imp.assert <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_assertiveness ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.assert))

linearint.assert <- lmer(bfas_assertiveness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.assert)

### Compassion
linearint.imp.compa <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_compassion ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.compa))

linearint.compa <- lmer(bfas_compassion ~ time +
                        (1 | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = peerl)
summary(linearint.compa)

### Conscientiousness
linearint.imp.consci <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_conscientiousness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.consci))

linearint.consci <- lmer(bfas_conscientiousness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.consci)

### Enthusiasm
linearint.imp.enthu <- with(data = imp_long_p,
                          exp = lme4::lmer(bfas_enthusiasm ~ time + 
                                             (1 | ID),
                                           control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.enthu))

linearint.enthu <- lmer(bfas_enthusiasm ~ time +
                        (1 | ID),
                      control = lmerControl(optimizer ="Nelder_Mead"),
                      data = peerl)
summary(linearint.enthu)

### Extraversion
linearint.imp.extra <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_extraversion ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.extra))

linearint.extra <- lmer(bfas_extraversion ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.extra)

### Industriousness 
linearint.imp.indus <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_industriousness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.indus))

linearint.indus <- lmer(bfas_industriousness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.indus)

### Intellect
linearint.imp.intel <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_intellect ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.intel))

linearint.intel <- lmer(bfas_intellect ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.intel)

### Neuroticism
linearint.imp.neuro <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_neuroticism ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.neuro))

linearint.neuro <- lmer(bfas_neuroticism ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.neuro)

### Openness Aspect
linearint.imp.opena <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_opennessaspect ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.opena))

linearint.opena <- lmer(bfas_opennessaspect ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.opena)

### Openness Domain
linearint.imp.opend <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_opennessdomain ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.opend))

linearint.opend <- lmer(bfas_opennessdomain ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.opend)

### Orderlines
linearint.imp.order <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_orderliness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.order))

linearint.order <- lmer(bfas_orderliness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.order)

### Politeness
linearint.imp.polit <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_politeness ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.polit))

linearint.polit <- lmer(bfas_politeness ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.polit)

### Volatility
linearint.imp.volat <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_volatility ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.volat))

linearint.volat <- lmer(bfas_volatility ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.volat)

### Withdrawal
linearint.imp.withd <- with(data = imp_long_p,
                         exp = lme4::lmer(bfas_withdrawal ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.withd))

linearint.withd <- lmer(bfas_withdrawal ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.withd)


# > PEER 1b: Mean level change identity ----
# >> Linear mixed model with random intercept and random slope ----

linear.coher <- lmer(epsi_coherence ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.coher)

linear.confu <- lmer(epsi_confusion ~ time +
                       (1 + time | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linear.confu)

# >> MI - Linear mixed model random intercept and random slope ----
linear.imp.confu <- with(data = imp_long_p,
                         exp = lme4::lmer(epsi_confusion ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.confu, var.comp=TRUE)

linear.imp.coher <- with(data = imp_long_p,
                         exp = lme4::lmer(epsi_coherence ~ time + 
                                        (1+time | ID),
                                      control = lmerControl(optimizer ="Nelder_Mead")))
testEstimates(linear.imp.coher, var.comp=TRUE)


# >> Quadratic mixed model with random intercept and random slope, no random quad slope ----
quad.coher <- lmer(epsi_coherence ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.coher)

quad.confu <- lmer(epsi_confusion ~ poly(time, degree = 2, raw = TRUE)  +
                     (1 + time| ID),
                   control = lmerControl(optimizer ="Nelder_Mead"),
                   data = peerl)
summary(quad.confu)

# >> Model comparison (using original data) ----

anova(linear.coher, quad.coher)
anova(linear.confu, quad.confu)

# >> Linear mixed model with random intercept and no random slope ----

linearint.imp.coher <- with(data = imp_long_p,
                         exp = lme4::lmer(epsi_coherence ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.coher))

linearint.coher <- lmer(epsi_coherence ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.coher)

linearint.imp.confu <- with(data = imp_long_p,
                         exp = lme4::lmer(epsi_confusion ~ time + 
                                            (1 | ID),
                                          control = lmerControl(optimizer = "Nelder_Mead")))
summary(pool(linearint.imp.confu))

linearint.confu <- lmer(epsi_confusion ~ time +
                       (1 | ID),
                     control = lmerControl(optimizer ="Nelder_Mead"),
                     data = peerl)
summary(linearint.confu)

# > PEER 2a: Rank order change personality ----
# >> Compute correlation fisher z ----

### correlation between T1 and T2
r12_agree <- cor(peerw$bfas_agreeableness_w1, peerw$bfas_agreeableness_w2, use = "pairwise.complete.obs")
r12_consci <- cor(peerw$bfas_conscientiousness_w1, peerw$bfas_conscientiousness_w2, use = "pairwise.complete.obs")
r12_extra <- cor(peerw$bfas_extraversion_w1, peerw$bfas_extraversion_w2, use = "pairwise.complete.obs")
r12_neuro <- cor(peerw$bfas_neuroticism_w1, peerw$bfas_neuroticism_w2, use = "pairwise.complete.obs")
r12_opend <- cor(peerw$bfas_opennessdomain_w1, peerw$bfas_opennessdomain_w2, use = "pairwise.complete.obs")
r12_assert <- cor(peerw$bfas_assertiveness_w1, peerw$bfas_assertiveness_w2, use = "pairwise.complete.obs")
r12_compa <- cor(peerw$bfas_compassion_w1, peerw$bfas_compassion_w2, use = "pairwise.complete.obs")
r12_enthu <- cor(peerw$bfas_enthusiasm_w1, peerw$bfas_enthusiasm_w2, use = "pairwise.complete.obs")
r12_indus <- cor(peerw$bfas_industriousness_w1, peerw$bfas_industriousness_w2, use = "pairwise.complete.obs")
r12_intel <- cor(peerw$bfas_intellect_w1, peerw$bfas_intellect_w2, use = "pairwise.complete.obs")
r12_opena <- cor(peerw$bfas_opennessaspect_w1, peerw$bfas_opennessaspect_w2, use = "pairwise.complete.obs")
r12_order <- cor(peerw$bfas_orderliness_w1, peerw$bfas_orderliness_w2, use = "pairwise.complete.obs")
r12_polit <- cor(peerw$bfas_politeness_w1, peerw$bfas_politeness_w2, use = "pairwise.complete.obs")
r12_volat <- cor(peerw$bfas_volatility_w1, peerw$bfas_volatility_w2, use = "pairwise.complete.obs")
r12_withd <- cor(peerw$bfas_withdrawal_w1, peerw$bfas_withdrawal_w2, use = "pairwise.complete.obs")

z12_agree <- fisherz(r12_agree)
z12_consci <- fisherz(r12_consci)
z12_extra <- fisherz(r12_extra)
z12_neuro <- fisherz(r12_neuro)
z12_opend <- fisherz(r12_opend)
z12_assert <- fisherz(r12_assert)
z12_compa <- fisherz(r12_compa)
z12_enthu <- fisherz(r12_enthu)
z12_indus <- fisherz(r12_indus)
z12_intel <- fisherz(r12_intel)
z12_opena <- fisherz(r12_opena)
z12_order <- fisherz(r12_order)
z12_polit <- fisherz(r12_polit)
z12_volat <- fisherz(r12_volat)
z12_withd <- fisherz(r12_withd)

### correlation between T2 and T3
r23_agree <- cor(peerw$bfas_agreeableness_w2, peerw$bfas_agreeableness_w3, use = "pairwise.complete.obs")
r23_consci <- cor(peerw$bfas_conscientiousness_w2, peerw$bfas_conscientiousness_w3, use = "pairwise.complete.obs")
r23_extra <- cor(peerw$bfas_extraversion_w2, peerw$bfas_extraversion_w3, use = "pairwise.complete.obs")
r23_neuro <- cor(peerw$bfas_neuroticism_w2, peerw$bfas_neuroticism_w3, use = "pairwise.complete.obs")
r23_opend <- cor(peerw$bfas_opennessdomain_w2, peerw$bfas_opennessdomain_w3, use = "pairwise.complete.obs")
r23_assert <- cor(peerw$bfas_assertiveness_w2, peerw$bfas_assertiveness_w3, use = "pairwise.complete.obs")
r23_compa <- cor(peerw$bfas_compassion_w2, peerw$bfas_compassion_w3, use = "pairwise.complete.obs")
r23_enthu <- cor(peerw$bfas_enthusiasm_w2, peerw$bfas_enthusiasm_w3, use = "pairwise.complete.obs")
r23_indus <- cor(peerw$bfas_industriousness_w2, peerw$bfas_industriousness_w3, use = "pairwise.complete.obs")
r23_intel <- cor(peerw$bfas_intellect_w2, peerw$bfas_intellect_w3, use = "pairwise.complete.obs")
r23_opena <- cor(peerw$bfas_opennessaspect_w2, peerw$bfas_opennessaspect_w3, use = "pairwise.complete.obs")
r23_order <- cor(peerw$bfas_orderliness_w2, peerw$bfas_orderliness_w3, use = "pairwise.complete.obs")
r23_polit <- cor(peerw$bfas_politeness_w2, peerw$bfas_politeness_w3, use = "pairwise.complete.obs")
r23_volat <- cor(peerw$bfas_volatility_w2, peerw$bfas_volatility_w3, use = "pairwise.complete.obs")
r23_withd <- cor(peerw$bfas_withdrawal_w2, peerw$bfas_withdrawal_w3, use = "pairwise.complete.obs")

z23_agree <- fisherz(r23_agree)
z23_consci <- fisherz(r23_consci)
z23_extra <- fisherz(r23_extra)
z23_neuro <- fisherz(r23_neuro)
z23_opend <- fisherz(r23_opend)
z23_assert <- fisherz(r23_assert)
z23_compa <- fisherz(r23_compa)
z23_enthu <- fisherz(r23_enthu)
z23_indus <- fisherz(r23_indus)
z23_intel <- fisherz(r23_intel)
z23_opena <- fisherz(r23_opena)
z23_order <- fisherz(r23_order)
z23_polit <- fisherz(r23_polit)
z23_volat <- fisherz(r23_volat)
z23_withd <- fisherz(r23_withd)

### correlation between T3 and T4
r34_agree <- cor(peerw$bfas_agreeableness_w3, peerw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r34_consci <- cor(peerw$bfas_conscientiousness_w3, peerw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r34_extra <- cor(peerw$bfas_extraversion_w3, peerw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r34_neuro <- cor(peerw$bfas_neuroticism_w3, peerw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r34_opend <- cor(peerw$bfas_opennessdomain_w3, peerw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r34_assert <- cor(peerw$bfas_assertiveness_w3, peerw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r34_compa <- cor(peerw$bfas_compassion_w3, peerw$bfas_compassion_w4, use = "pairwise.complete.obs")
r34_enthu <- cor(peerw$bfas_enthusiasm_w3, peerw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r34_indus <- cor(peerw$bfas_industriousness_w3, peerw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r34_intel <- cor(peerw$bfas_intellect_w3, peerw$bfas_intellect_w4, use = "pairwise.complete.obs")
r34_opena <- cor(peerw$bfas_opennessaspect_w3, peerw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r34_order <- cor(peerw$bfas_orderliness_w3, peerw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r34_polit <- cor(peerw$bfas_politeness_w3, peerw$bfas_politeness_w4, use = "pairwise.complete.obs")
r34_volat <- cor(peerw$bfas_volatility_w3, peerw$bfas_volatility_w4, use = "pairwise.complete.obs")
r34_withd <- cor(peerw$bfas_withdrawal_w3, peerw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z34_agree <- fisherz(r34_agree)
z34_consci <- fisherz(r34_consci)
z34_extra <- fisherz(r34_extra)
z34_neuro <- fisherz(r34_neuro)
z34_opend <- fisherz(r34_opend)
z34_assert <- fisherz(r34_assert)
z34_compa <- fisherz(r34_compa)
z34_enthu <- fisherz(r34_enthu)
z34_indus <- fisherz(r34_indus)
z34_intel <- fisherz(r34_intel)
z34_opena <- fisherz(r34_opena)
z34_order <- fisherz(r34_order)
z34_polit <- fisherz(r34_polit)
z34_volat <- fisherz(r34_volat)
z34_withd <- fisherz(r34_withd)

### correlation between T1 and T4
r14_agree <- cor(peerw$bfas_agreeableness_w1, peerw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r14_consci <- cor(peerw$bfas_conscientiousness_w1, peerw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r14_extra <- cor(peerw$bfas_extraversion_w1, peerw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r14_neuro <- cor(peerw$bfas_neuroticism_w1, peerw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r14_opend <- cor(peerw$bfas_opennessdomain_w1, peerw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r14_assert <- cor(peerw$bfas_assertiveness_w1, peerw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r14_compa <- cor(peerw$bfas_compassion_w1, peerw$bfas_compassion_w4, use = "pairwise.complete.obs")
r14_enthu <- cor(peerw$bfas_enthusiasm_w1, peerw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r14_indus <- cor(peerw$bfas_industriousness_w1, peerw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r14_intel <- cor(peerw$bfas_intellect_w1, peerw$bfas_intellect_w4, use = "pairwise.complete.obs")
r14_opena <- cor(peerw$bfas_opennessaspect_w1, peerw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r14_order <- cor(peerw$bfas_orderliness_w1, peerw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r14_polit <- cor(peerw$bfas_politeness_w1, peerw$bfas_politeness_w4, use = "pairwise.complete.obs")
r14_volat <- cor(peerw$bfas_volatility_w1, peerw$bfas_volatility_w4, use = "pairwise.complete.obs")
r14_withd <- cor(peerw$bfas_withdrawal_w1, peerw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z14_agree <- fisherz(r14_agree)
z14_consci <- fisherz(r14_consci)
z14_extra <- fisherz(r14_extra)
z14_neuro <- fisherz(r14_neuro)
z14_opend <- fisherz(r14_opend)
z14_assert <- fisherz(r14_assert)
z14_compa <- fisherz(r14_compa)
z14_enthu <- fisherz(r14_enthu)
z14_indus <- fisherz(r14_indus)
z14_intel <- fisherz(r14_intel)
z14_opena <- fisherz(r14_opena)
z14_order <- fisherz(r14_order)
z14_polit <- fisherz(r14_polit)
z14_volat <- fisherz(r14_volat)
z14_withd <- fisherz(r14_withd)

### correlation between T1 and T3
r13_agree <- cor(peerw$bfas_agreeableness_w3, peerw$bfas_agreeableness_w1, use = "pairwise.complete.obs")
r13_consci <- cor(peerw$bfas_conscientiousness_w3, peerw$bfas_conscientiousness_w1, use = "pairwise.complete.obs")
r13_extra <- cor(peerw$bfas_extraversion_w3, peerw$bfas_extraversion_w1, use = "pairwise.complete.obs")
r13_neuro <- cor(peerw$bfas_neuroticism_w3, peerw$bfas_neuroticism_w1, use = "pairwise.complete.obs")
r13_opend <- cor(peerw$bfas_opennessdomain_w3, peerw$bfas_opennessdomain_w1, use = "pairwise.complete.obs")
r13_assert <- cor(peerw$bfas_assertiveness_w3, peerw$bfas_assertiveness_w1, use = "pairwise.complete.obs")
r13_compa <- cor(peerw$bfas_compassion_w3, peerw$bfas_compassion_w1, use = "pairwise.complete.obs")
r13_enthu <- cor(peerw$bfas_enthusiasm_w3, peerw$bfas_enthusiasm_w1, use = "pairwise.complete.obs")
r13_indus <- cor(peerw$bfas_industriousness_w3, peerw$bfas_industriousness_w1, use = "pairwise.complete.obs")
r13_intel <- cor(peerw$bfas_intellect_w3, peerw$bfas_intellect_w1, use = "pairwise.complete.obs")
r13_opena <- cor(peerw$bfas_opennessaspect_w3, peerw$bfas_opennessaspect_w1, use = "pairwise.complete.obs")
r13_order <- cor(peerw$bfas_orderliness_w3, peerw$bfas_orderliness_w1, use = "pairwise.complete.obs")
r13_polit <- cor(peerw$bfas_politeness_w3, peerw$bfas_politeness_w1, use = "pairwise.complete.obs")
r13_volat <- cor(peerw$bfas_volatility_w3, peerw$bfas_volatility_w1, use = "pairwise.complete.obs")
r13_withd <- cor(peerw$bfas_withdrawal_w3, peerw$bfas_withdrawal_w1, use = "pairwise.complete.obs")

z13_agree <- fisherz(r13_agree)
z13_consci <- fisherz(r13_consci)
z13_extra <- fisherz(r13_extra)
z13_neuro <- fisherz(r13_neuro)
z13_opend <- fisherz(r13_opend)
z13_assert <- fisherz(r13_assert)
z13_compa <- fisherz(r13_compa)
z13_enthu <- fisherz(r13_enthu)
z13_indus <- fisherz(r13_indus)
z13_intel <- fisherz(r13_intel)
z13_opena <- fisherz(r13_opena)
z13_order <- fisherz(r13_order)
z13_polit <- fisherz(r13_polit)
z13_volat <- fisherz(r13_volat)
z13_withd <- fisherz(r13_withd)

### correlation between T2 and T4
r24_agree <- cor(peerw$bfas_agreeableness_w2, peerw$bfas_agreeableness_w4, use = "pairwise.complete.obs")
r24_consci <- cor(peerw$bfas_conscientiousness_w2, peerw$bfas_conscientiousness_w4, use = "pairwise.complete.obs")
r24_extra <- cor(peerw$bfas_extraversion_w2, peerw$bfas_extraversion_w4, use = "pairwise.complete.obs")
r24_neuro <- cor(peerw$bfas_neuroticism_w2, peerw$bfas_neuroticism_w4, use = "pairwise.complete.obs")
r24_opend <- cor(peerw$bfas_opennessdomain_w2, peerw$bfas_opennessdomain_w4, use = "pairwise.complete.obs")
r24_assert <- cor(peerw$bfas_assertiveness_w2, peerw$bfas_assertiveness_w4, use = "pairwise.complete.obs")
r24_compa <- cor(peerw$bfas_compassion_w2, peerw$bfas_compassion_w4, use = "pairwise.complete.obs")
r24_enthu <- cor(peerw$bfas_enthusiasm_w2, peerw$bfas_enthusiasm_w4, use = "pairwise.complete.obs")
r24_indus <- cor(peerw$bfas_industriousness_w2, peerw$bfas_industriousness_w4, use = "pairwise.complete.obs")
r24_intel <- cor(peerw$bfas_intellect_w2, peerw$bfas_intellect_w4, use = "pairwise.complete.obs")
r24_opena <- cor(peerw$bfas_opennessaspect_w2, peerw$bfas_opennessaspect_w4, use = "pairwise.complete.obs")
r24_order <- cor(peerw$bfas_orderliness_w2, peerw$bfas_orderliness_w4, use = "pairwise.complete.obs")
r24_polit <- cor(peerw$bfas_politeness_w2, peerw$bfas_politeness_w4, use = "pairwise.complete.obs")
r24_volat <- cor(peerw$bfas_volatility_w2, peerw$bfas_volatility_w4, use = "pairwise.complete.obs")
r24_withd <- cor(peerw$bfas_withdrawal_w2, peerw$bfas_withdrawal_w4, use = "pairwise.complete.obs")

z24_agree <- fisherz(r24_agree)
z24_consci <- fisherz(r24_consci)
z24_extra <- fisherz(r24_extra)
z24_neuro <- fisherz(r24_neuro)
z24_opend <- fisherz(r24_opend)
z24_assert <- fisherz(r24_assert)
z24_compa <- fisherz(r24_compa)
z24_enthu <- fisherz(r24_enthu)
z24_indus <- fisherz(r24_indus)
z24_intel <- fisherz(r24_intel)
z24_opena <- fisherz(r24_opena)
z24_order <- fisherz(r24_order)
z24_polit <- fisherz(r24_polit)
z24_volat <- fisherz(r24_volat)
z24_withd <- fisherz(r24_withd)

# >> determine Ns for comparison ----
n_agree_12 <- sum(peerw$bfas_agreeableness_w2 != "NA", na.rm = TRUE)
n_consci_12 <- sum(peerw$bfas_conscientiousness_w2 != "NA", na.rm = TRUE)
n_extra_12 <- sum(peerw$bfas_extraversion_w2 != "NA", na.rm = TRUE)
n_neuro_12 <- sum(peerw$bfas_neuroticism_w2 != "NA", na.rm = TRUE)
n_opend_12 <- sum(peerw$bfas_opennessdomain_w2 != "NA", na.rm = TRUE)
n_assert_12 <- sum(peerw$bfas_assertiveness_w2 != "NA", na.rm = TRUE)
n_compa_12 <- sum(peerw$bfas_compassion_w2 != "NA", na.rm = TRUE)
n_enthu_12 <- sum(peerw$bfas_enthusiasm_w2 != "NA", na.rm = TRUE)
n_indus_12 <- sum(peerw$bfas_industriousness_w2 != "NA", na.rm = TRUE)
n_intel_12 <- sum(peerw$bfas_intellect_w2 != "NA", na.rm = TRUE)
n_opena_12 <- sum(peerw$bfas_opennessaspect_w2 != "NA", na.rm = TRUE)
n_order_12 <- sum(peerw$bfas_orderliness_w2 != "NA", na.rm = TRUE)
n_polit_12 <- sum(peerw$bfas_politeness_w2 != "NA", na.rm = TRUE)
n_volat_12 <- sum(peerw$bfas_volatility_w2 != "NA", na.rm = TRUE)
n_withd_12 <- sum(peerw$bfas_withdrawal_w2 != "NA", na.rm = TRUE)

n_agree_23 <- sum(peerw$bfas_agreeableness_w3 != "NA", na.rm = TRUE)
n_consci_23 <- sum(peerw$bfas_conscientiousness_w3 != "NA", na.rm = TRUE)
n_extra_23 <- sum(peerw$bfas_extraversion_w3 != "NA", na.rm = TRUE)
n_neuro_23 <- sum(peerw$bfas_neuroticism_w3 != "NA", na.rm = TRUE)
n_opend_23 <- sum(peerw$bfas_opennessdomain_w3 != "NA", na.rm = TRUE)
n_assert_23 <- sum(peerw$bfas_assertiveness_w3 != "NA", na.rm = TRUE)
n_compa_23 <- sum(peerw$bfas_compassion_w3 != "NA", na.rm = TRUE)
n_enthu_23 <- sum(peerw$bfas_enthusiasm_w3 != "NA", na.rm = TRUE)
n_indus_23 <- sum(peerw$bfas_industriousness_w3 != "NA", na.rm = TRUE)
n_intel_23 <- sum(peerw$bfas_intellect_w3 != "NA", na.rm = TRUE)
n_opena_23 <- sum(peerw$bfas_opennessaspect_w3 != "NA", na.rm = TRUE)
n_order_23 <- sum(peerw$bfas_orderliness_w3 != "NA", na.rm = TRUE)
n_polit_23 <- sum(peerw$bfas_politeness_w3 != "NA", na.rm = TRUE)
n_volat_23 <- sum(peerw$bfas_volatility_w3 != "NA", na.rm = TRUE)
n_withd_23 <- sum(peerw$bfas_withdrawal_w3 != "NA", na.rm = TRUE)

n_agree_34 <- sum(peerw$bfas_agreeableness_w4 != "NA", na.rm = TRUE)
n_consci_34 <- sum(peerw$bfas_conscientiousness_w4 != "NA", na.rm = TRUE)
n_extra_34 <- sum(peerw$bfas_extraversion_w4 != "NA", na.rm = TRUE)
n_neuro_34 <- sum(peerw$bfas_neuroticism_w4 != "NA", na.rm = TRUE)
n_opend_34 <- sum(peerw$bfas_opennessdomain_w4 != "NA", na.rm = TRUE)
n_assert_34 <- sum(peerw$bfas_assertiveness_w4 != "NA", na.rm = TRUE)
n_compa_34 <- sum(peerw$bfas_compassion_w4 != "NA", na.rm = TRUE)
n_enthu_34 <- sum(peerw$bfas_enthusiasm_w4 != "NA", na.rm = TRUE)
n_indus_34 <- sum(peerw$bfas_industriousness_w4 != "NA", na.rm = TRUE)
n_intel_34 <- sum(peerw$bfas_intellect_w4 != "NA", na.rm = TRUE)
n_opena_34 <- sum(peerw$bfas_opennessaspect_w4 != "NA", na.rm = TRUE)
n_order_34 <- sum(peerw$bfas_orderliness_w4 != "NA", na.rm = TRUE)
n_polit_34 <- sum(peerw$bfas_politeness_w4 != "NA", na.rm = TRUE)
n_volat_34 <- sum(peerw$bfas_volatility_w4 != "NA", na.rm = TRUE)
n_withd_34 <- sum(peerw$bfas_withdrawal_w4 != "NA", na.rm = TRUE)
# >> compare correlation among intervals ----
rcon12agree <- r.con(r12_agree, n_agree_12)
rcon23agree <- r.con(r12_agree, n_agree_23)
rcon34agree <- r.con(r34_agree, n_agree_34)

rcon12consci <- r.con(r12_consci, n_consci_12)
rcon23consci <- r.con(r12_consci, n_consci_23)
rcon34consci <- r.con(r34_consci, n_consci_34)

rcon12extra <- r.con(r12_extra, n_extra_12)
rcon23extra <- r.con(r12_extra, n_extra_23)
rcon34extra <- r.con(r34_extra, n_extra_34)

rcon12neuro <- r.con(r12_neuro, n_neuro_12)
rcon23neuro <- r.con(r12_neuro, n_neuro_23)
rcon34neuro <- r.con(r34_neuro, n_neuro_34)

rcon12opend <- r.con(r12_opend, n_opend_12)
rcon23opend <- r.con(r12_opend, n_opend_23)
rcon34opend <- r.con(r34_opend, n_opend_34)

rcon12assert <- r.con(r12_assert, n_assert_12)
rcon23assert <- r.con(r12_assert, n_assert_23)
rcon34assert <- r.con(r34_assert, n_assert_34)

rcon12compa <- r.con(r12_compa, n_compa_12)
rcon23compa <- r.con(r12_compa, n_compa_23)
rcon34compa <- r.con(r34_compa, n_compa_34)

rcon12enthu <- r.con(r12_enthu, n_enthu_12)
rcon23enthu <- r.con(r12_enthu, n_enthu_23)
rcon34enthu <- r.con(r34_enthu, n_enthu_34)

rcon12indus <- r.con(r12_indus, n_indus_12)
rcon23indus <- r.con(r12_indus, n_indus_23)
rcon34indus <- r.con(r34_indus, n_indus_34)

rcon12intel <- r.con(r12_intel, n_intel_12)
rcon23intel <- r.con(r12_intel, n_intel_23)
rcon34intel <- r.con(r34_intel, n_intel_34)

rcon12opena <- r.con(r12_opena, n_opena_12)
rcon23opena <- r.con(r12_opena, n_opena_23)
rcon34opena <- r.con(r34_opena, n_opena_34)

rcon12order <- r.con(r12_order, n_order_12)
rcon23order <- r.con(r12_order, n_order_23)
rcon34order <- r.con(r34_order, n_order_34)

rcon12polit <- r.con(r12_polit, n_polit_12)
rcon23polit <- r.con(r12_polit, n_polit_23)
rcon34polit <- r.con(r34_polit, n_polit_34)

rcon12volat <- r.con(r12_volat, n_volat_12)
rcon23volat <- r.con(r12_volat, n_volat_23)
rcon34volat <- r.con(r34_volat, n_volat_34)

rcon12withd <- r.con(r12_withd, n_withd_12)
rcon23withd <- r.con(r12_withd, n_withd_23)
rcon34withd <- r.con(r34_withd, n_withd_34)

agree_12_23 <- (z12_agree-z23_agree)/(sqrt((1/(n_agree_12 - 3)) + (1/(n_agree_23-3))))
agree_23_34 <- (z23_agree-z34_agree)/(sqrt((1/(n_agree_23 - 3)) + (1/(n_agree_34-3))))
agree_12_34 <- (z12_agree-z34_agree)/(sqrt((1/(n_agree_12 - 3)) + (1/(n_agree_34-3))))
p_agree_12_23 <- pnorm(abs(agree_12_23), lower.tail = FALSE)
p_agree_23_34 <-pnorm(abs(agree_23_34), lower.tail = FALSE)
p_agree_12_34 <- pnorm(abs(agree_12_34), lower.tail = FALSE)

consci_12_23 <- (z12_consci-z23_consci)/(sqrt((1/(n_consci_12 - 3)) + (1/(n_consci_23-3))))
consci_23_34 <- (z23_consci-z34_consci)/(sqrt((1/(n_consci_23 - 3)) + (1/(n_consci_34-3))))
consci_12_34 <- (z12_consci-z34_consci)/(sqrt((1/(n_consci_12 - 3)) + (1/(n_consci_34-3))))
p_consci_12_23 <- pnorm(abs(consci_12_23), lower.tail = FALSE)
p_consci_23_34 <- pnorm(abs(consci_23_34), lower.tail = FALSE)
p_consci_12_34 <- pnorm(abs(consci_12_34), lower.tail = FALSE)

extra_12_23 <- (z12_extra-z23_extra)/(sqrt((1/(n_extra_12 - 3)) + (1/(n_extra_23-3))))
extra_23_34 <- (z23_extra-z34_extra)/(sqrt((1/(n_extra_23 - 3)) + (1/(n_extra_34-3))))
extra_12_34 <- (z12_extra-z34_extra)/(sqrt((1/(n_extra_12 - 3)) + (1/(n_extra_34-3))))
p_extra_12_23 <- pnorm(abs(extra_12_23), lower.tail = FALSE)
p_extra_23_34 <- pnorm(abs(extra_23_34), lower.tail = FALSE)
p_extra_12_34 <- pnorm(abs(extra_12_34), lower.tail = FALSE)

neuro_12_23 <- (z12_neuro-z23_neuro)/(sqrt((1/(n_neuro_12 - 3)) + (1/(n_neuro_23-3))))
neuro_23_34 <- (z23_neuro-z34_neuro)/(sqrt((1/(n_neuro_23 - 3)) + (1/(n_neuro_34-3))))
neuro_12_34 <- (z12_neuro-z34_neuro)/(sqrt((1/(n_neuro_12 - 3)) + (1/(n_neuro_34-3))))
p_neuro_12_23 <- pnorm(abs(neuro_12_23), lower.tail = FALSE)
p_neuro_23_34 <- pnorm(abs(neuro_23_34), lower.tail = FALSE)
p_neuro_12_34 <- pnorm(abs(neuro_12_34), lower.tail = FALSE)

opend_12_23 <- (z12_opend-z23_opend)/(sqrt((1/(n_opend_12 - 3)) + (1/(n_opend_23-3))))
opend_23_34 <- (z23_opend-z34_opend)/(sqrt((1/(n_opend_23 - 3)) + (1/(n_opend_34-3))))
opend_12_34 <- (z12_opend-z34_opend)/(sqrt((1/(n_opend_12 - 3)) + (1/(n_opend_34-3))))
p_opend_12_23 <- pnorm(abs(opend_12_23), lower.tail = FALSE)
p_opend_23_34 <- pnorm(abs(opend_23_34), lower.tail = FALSE)
p_opend_12_34 <- pnorm(abs(opend_12_34), lower.tail = FALSE)

assert_12_23 <- (z12_assert-z23_assert)/(sqrt((1/(n_assert_12 - 3)) + (1/(n_assert_23-3))))
assert_23_34 <- (z23_assert-z34_assert)/(sqrt((1/(n_assert_23 - 3)) + (1/(n_assert_34-3))))
assert_12_34 <- (z12_assert-z34_assert)/(sqrt((1/(n_assert_12 - 3)) + (1/(n_assert_34-3))))
p_assert_12_23 <- pnorm(abs(assert_12_23), lower.tail = FALSE)
p_assert_23_34 <- pnorm(abs(assert_23_34), lower.tail = FALSE)
p_assert_12_34 <- pnorm(abs(assert_12_34), lower.tail = FALSE)

compa_12_23 <- (z12_compa-z23_compa)/(sqrt((1/(n_compa_12 - 3)) + (1/(n_compa_23-3))))
compa_23_34 <- (z23_compa-z34_compa)/(sqrt((1/(n_compa_23 - 3)) + (1/(n_compa_34-3))))
compa_12_34 <- (z12_compa-z34_compa)/(sqrt((1/(n_compa_12 - 3)) + (1/(n_compa_34-3))))
p_compa_12_23 <- pnorm(abs(compa_12_23), lower.tail = FALSE)
p_compa_23_34 <- pnorm(abs(compa_23_34), lower.tail = FALSE)
p_compa_12_34 <- pnorm(abs(compa_12_34), lower.tail = FALSE)

enthu_12_23 <- (z12_enthu-z23_enthu)/(sqrt((1/(n_enthu_12 - 3)) + (1/(n_enthu_23-3))))
enthu_23_34 <- (z23_enthu-z34_enthu)/(sqrt((1/(n_enthu_23 - 3)) + (1/(n_enthu_34-3))))
enthu_12_34 <- (z12_enthu-z34_enthu)/(sqrt((1/(n_enthu_12 - 3)) + (1/(n_enthu_34-3))))
p_enthu_12_23 <- pnorm(abs(enthu_12_23), lower.tail = FALSE)
p_enthu_23_34 <- pnorm(abs(enthu_23_34), lower.tail = FALSE)
p_enthu_12_34 <- pnorm(abs(enthu_12_34), lower.tail = FALSE)

indus_12_23 <- (z12_indus-z23_indus)/(sqrt((1/(n_indus_12 - 3)) + (1/(n_indus_23-3))))
indus_23_34 <- (z23_indus-z34_indus)/(sqrt((1/(n_indus_23 - 3)) + (1/(n_indus_34-3))))
indus_12_34 <- (z12_indus-z34_indus)/(sqrt((1/(n_indus_12 - 3)) + (1/(n_indus_34-3))))
p_indus_12_23 <- pnorm(abs(indus_12_23), lower.tail = FALSE)
p_indus_23_34 <- pnorm(abs(indus_23_34), lower.tail = FALSE)
p_indus_12_34 <- pnorm(abs(indus_12_34), lower.tail = FALSE)

intel_12_23 <- (z12_intel-z23_intel)/(sqrt((1/(n_intel_12 - 3)) + (1/(n_intel_23-3))))
intel_23_34 <- (z23_intel-z34_intel)/(sqrt((1/(n_intel_23 - 3)) + (1/(n_intel_34-3))))
intel_12_34 <- (z12_intel-z34_intel)/(sqrt((1/(n_intel_12 - 3)) + (1/(n_intel_34-3))))
p_intel_12_23 <- pnorm(abs(intel_12_23), lower.tail = FALSE)
p_intel_23_34 <- pnorm(abs(intel_23_34), lower.tail = FALSE)
p_intel_12_34 <- pnorm(abs(intel_12_34), lower.tail = FALSE)

opena_12_23 <- (z12_opena-z23_opena)/(sqrt((1/(n_opena_12 - 3)) + (1/(n_opena_23-3))))
opena_23_34 <- (z23_opena-z34_opena)/(sqrt((1/(n_opena_23 - 3)) + (1/(n_opena_34-3))))
opena_12_34 <- (z12_opena-z34_opena)/(sqrt((1/(n_opena_12 - 3)) + (1/(n_opena_34-3))))
p_opena_12_23 <- pnorm(abs(opena_12_23), lower.tail = FALSE)
p_opena_23_34 <- pnorm(abs(opena_23_34), lower.tail = FALSE)
p_opena_12_34 <- pnorm(abs(opena_12_34), lower.tail = FALSE)

order_12_23 <- (z12_order-z23_order)/(sqrt((1/(n_order_12 - 3)) + (1/(n_order_23-3))))
order_23_34 <- (z23_order-z34_order)/(sqrt((1/(n_order_23 - 3)) + (1/(n_order_34-3))))
order_12_34 <- (z12_order-z34_order)/(sqrt((1/(n_order_12 - 3)) + (1/(n_order_34-3))))
p_order_12_23 <- pnorm(abs(order_12_23), lower.tail = FALSE)
p_order_23_34 <- pnorm(abs(order_23_34), lower.tail = FALSE)
p_order_12_34 <- pnorm(abs(order_12_34), lower.tail = FALSE)

polit_12_23 <- (z12_polit-z23_polit)/(sqrt((1/(n_polit_12 - 3)) + (1/(n_polit_23-3))))
polit_23_34 <- (z23_polit-z34_polit)/(sqrt((1/(n_polit_23 - 3)) + (1/(n_polit_34-3))))
polit_12_34 <- (z12_polit-z34_polit)/(sqrt((1/(n_polit_12 - 3)) + (1/(n_polit_34-3))))
p_polit_12_23 <- pnorm(abs(polit_12_23), lower.tail = FALSE)
p_polit_23_34 <- pnorm(abs(polit_23_34), lower.tail = FALSE)
p_polit_12_34 <- pnorm(abs(polit_12_34), lower.tail = FALSE)

volat_12_23 <- (z12_volat-z23_volat)/(sqrt((1/(n_volat_12 - 3)) + (1/(n_volat_23-3))))
volat_23_34 <- (z23_volat-z34_volat)/(sqrt((1/(n_volat_23 - 3)) + (1/(n_volat_34-3))))
volat_12_34 <- (z12_volat-z34_volat)/(sqrt((1/(n_volat_12 - 3)) + (1/(n_volat_34-3))))
p_volat_12_23 <- pnorm(abs(volat_12_23), lower.tail = FALSE)
p_volat_23_34 <- pnorm(abs(volat_23_34), lower.tail = FALSE)
p_volat_12_34 <- pnorm(abs(volat_12_34), lower.tail = FALSE)

withd_12_23 <- (z12_withd-z23_withd)/(sqrt((1/(n_withd_12 - 3)) + (1/(n_withd_23-3))))
withd_23_34 <- (z23_withd-z34_withd)/(sqrt((1/(n_withd_23 - 3)) + (1/(n_withd_34-3))))
withd_12_34 <- (z12_withd-z34_withd)/(sqrt((1/(n_withd_12 - 3)) + (1/(n_withd_34-3))))
p_withd_12_23 <- pnorm(abs(withd_12_23), lower.tail = FALSE)
p_withd_23_34 <- pnorm(abs(withd_23_34), lower.tail = FALSE)
p_withd_12_34 <- pnorm(abs(withd_12_34), lower.tail = FALSE)
# >> imputations ----
# >>> Compute correlation fisher z ----

### correlation between T1 and T2
r12_agree <- with(data = imp_wide_p,
                   exp = cor(bfas_agreeableness_w1, bfas_agreeableness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_consci <- with(data = imp_wide_p,
                   exp = cor(bfas_conscientiousness_w1, bfas_conscientiousness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_extra <- with(data = imp_wide_p,
                   exp = cor(bfas_extraversion_w1, bfas_extraversion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_neuro <- with(data = imp_wide_p,
                   exp = cor(bfas_neuroticism_w1, bfas_neuroticism_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_opend <- with(data = imp_wide_p,
                   exp = cor(bfas_opennessdomain_w1, bfas_opennessdomain_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_assert <- with(data = imp_wide_p,
                   exp = cor(bfas_assertiveness_w1, bfas_assertiveness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_compa <- with(data = imp_wide_p,
                   exp = cor(bfas_compassion_w1, bfas_compassion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_enthu <- with(data = imp_wide_p,
                   exp = cor(bfas_enthusiasm_w1, bfas_enthusiasm_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_indus <- with(data = imp_wide_p,
                   exp = cor(bfas_industriousness_w1, bfas_industriousness_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_intel <- with(data = imp_wide_p,
                   exp = cor(bfas_intellect_w1, bfas_intellect_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_opena <- with(data = imp_wide_p,
                   exp = cor(bfas_opennessaspect_w1, bfas_opennessaspect_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_order <- with(data = imp_wide_p,                    
                  exp = cor(bfas_orderliness_w1, bfas_orderliness_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_polit <- with(data = imp_wide_p,
                  exp = cor(bfas_politeness_w1, bfas_politeness_w2, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_volat <- with(data = imp_wide_p,
                  exp = cor(bfas_volatility_w1, bfas_volatility_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r12_withd <- with(data = imp_wide_p,   
                  exp = cor(bfas_withdrawal_w1, bfas_withdrawal_w2,
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z12_agree <- fisherz(r12_agree) %>% melt() %>% dplyr::select(value)
z12_consci <- fisherz(r12_consci) %>% melt() %>% dplyr::select(value)
z12_extra <- fisherz(r12_extra) %>% melt() %>% dplyr::select(value)
z12_neuro <- fisherz(r12_neuro) %>% melt() %>% dplyr::select(value)
z12_opend <- fisherz(r12_opend) %>% melt() %>% dplyr::select(value)
z12_assert <- fisherz(r12_assert) %>% melt() %>% dplyr::select(value)
z12_compa <- fisherz(r12_compa) %>% melt() %>% dplyr::select(value)
z12_enthu <- fisherz(r12_enthu) %>% melt() %>% dplyr::select(value)
z12_indus <- fisherz(r12_indus) %>% melt() %>% dplyr::select(value)
z12_intel <- fisherz(r12_intel) %>% melt() %>% dplyr::select(value)
z12_opena <- fisherz(r12_opena) %>% melt() %>% dplyr::select(value)
z12_order <- fisherz(r12_order) %>% melt() %>% dplyr::select(value)
z12_polit <- fisherz(r12_polit) %>% melt() %>% dplyr::select(value)
z12_volat <- fisherz(r12_volat) %>% melt() %>% dplyr::select(value)
z12_withd <- fisherz(r12_withd) %>% melt() %>% dplyr::select(value)

### correlation between T2 and T3
r23_agree <- with(data = imp_wide_p,              
                  exp = cor(bfas_agreeableness_w2, bfas_agreeableness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_consci <- with(data = imp_wide_p,                   
                   exp = cor(bfas_conscientiousness_w2, bfas_conscientiousness_w3,
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_extra <- with(data = imp_wide_p,                  
                  exp = cor(bfas_extraversion_w2, bfas_extraversion_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_neuro <- with(data = imp_wide_p,                  
                  exp = cor(bfas_neuroticism_w2, bfas_neuroticism_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_opend <- with(data = imp_wide_p,                  
                  exp = cor(bfas_opennessdomain_w2, bfas_opennessdomain_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_assert <- with(data = imp_wide_p,                 
                   exp = cor(bfas_assertiveness_w2, bfas_assertiveness_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_compa <- with(data = imp_wide_p,                 
                  exp = cor(bfas_compassion_w2, bfas_compassion_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_enthu <- with(data = imp_wide_p,                  
                  exp = cor(bfas_enthusiasm_w2, bfas_enthusiasm_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_indus <- with(data = imp_wide_p,                 
                  exp = cor(bfas_industriousness_w2, bfas_industriousness_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_intel <- with(data = imp_wide_p,                  
                  exp = cor(bfas_intellect_w2, bfas_intellect_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_opena <- with(data = imp_wide_p,                    
                  exp = cor(bfas_opennessaspect_w2, bfas_opennessaspect_w3, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_order <- with(data = imp_wide_p,                  
                  exp = cor(bfas_orderliness_w2, bfas_orderliness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_polit <- with(data = imp_wide_p,                 
                  exp = cor(bfas_politeness_w2, bfas_politeness_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_volat <- with(data = imp_wide_p,                 
                  exp = cor(bfas_volatility_w2, bfas_volatility_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r23_withd <- with(data = imp_wide_p,                 
                  exp = cor(bfas_withdrawal_w2, bfas_withdrawal_w3,
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z23_agree <- fisherz(r23_agree) %>% melt() %>% dplyr::select(value)
z23_consci <- fisherz(r23_consci) %>% melt() %>% dplyr::select(value)
z23_extra <- fisherz(r23_extra) %>% melt() %>% dplyr::select(value)
z23_neuro <- fisherz(r23_neuro) %>% melt() %>% dplyr::select(value)
z23_opend <- fisherz(r23_opend) %>% melt() %>% dplyr::select(value)
z23_assert <- fisherz(r23_assert) %>% melt() %>% dplyr::select(value)
z23_compa <- fisherz(r23_compa) %>% melt() %>% dplyr::select(value)
z23_enthu <- fisherz(r23_enthu) %>% melt() %>% dplyr::select(value)
z23_indus <- fisherz(r23_indus) %>% melt() %>% dplyr::select(value)
z23_intel <- fisherz(r23_intel) %>% melt() %>% dplyr::select(value)
z23_opena <- fisherz(r23_opena) %>% melt() %>% dplyr::select(value)
z23_order <- fisherz(r23_order) %>% melt() %>% dplyr::select(value)
z23_polit <- fisherz(r23_polit) %>% melt() %>% dplyr::select(value)
z23_volat <- fisherz(r23_volat) %>% melt() %>% dplyr::select(value)
z23_withd <- fisherz(r23_withd) %>% melt() %>% dplyr::select(value)

### correlation between T3 and T4
r34_agree <- with(data = imp_wide_p,                   
                  exp = cor(bfas_agreeableness_w3, bfas_agreeableness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_consci <- with(data = imp_wide_p,                    
                   exp = cor(bfas_conscientiousness_w3, bfas_conscientiousness_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_extra <- with(data = imp_wide_p,                   
                  exp = cor(bfas_extraversion_w3, bfas_extraversion_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_neuro <- with(data = imp_wide_p,                    
                  exp = cor(bfas_neuroticism_w3, bfas_neuroticism_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_opend <- with(data = imp_wide_p,                    
                  exp = cor(bfas_opennessdomain_w3, bfas_opennessdomain_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_assert <- with(data = imp_wide_p,                  
                   exp = cor(bfas_assertiveness_w3, bfas_assertiveness_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_compa <- with(data = imp_wide_p,                 
                  exp = cor(bfas_compassion_w3, bfas_compassion_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_enthu <- with(data = imp_wide_p,                 
                  exp = cor(bfas_enthusiasm_w3, bfas_enthusiasm_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_indus <- with(data = imp_wide_p,                
                  exp = cor(bfas_industriousness_w3, bfas_industriousness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_intel <- with(data = imp_wide_p,               
                  exp = cor(bfas_intellect_w3, bfas_intellect_w4,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_opena <- with(data = imp_wide_p,                
                  exp = cor(bfas_opennessaspect_w3, bfas_opennessaspect_w4,
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_order <- with(data = imp_wide_p,                   
                  exp = cor(bfas_orderliness_w3, bfas_orderliness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_polit <- with(data = imp_wide_p,
                  exp = cor(bfas_politeness_w3, bfas_politeness_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_volat <- with(data = imp_wide_p,   
                  exp = cor(bfas_volatility_w3, bfas_volatility_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()
r34_withd <- with(data = imp_wide_p,     
                  exp = cor(bfas_withdrawal_w3, bfas_withdrawal_w4, 
                            use = "pairwise.complete.obs")) %>% as.data.frame()

z34_agree <- fisherz(r34_agree) %>% melt() %>% dplyr::select(value)
z34_consci <- fisherz(r34_consci) %>% melt() %>% dplyr::select(value)
z34_extra <- fisherz(r34_extra) %>% melt() %>% dplyr::select(value)
z34_neuro <- fisherz(r34_neuro) %>% melt() %>% dplyr::select(value)
z34_opend <- fisherz(r34_opend) %>% melt() %>% dplyr::select(value)
z34_assert <- fisherz(r34_assert) %>% melt() %>% dplyr::select(value)
z34_compa <- fisherz(r34_compa) %>% melt() %>% dplyr::select(value)
z34_enthu <- fisherz(r34_enthu) %>% melt() %>% dplyr::select(value)
z34_indus <- fisherz(r34_indus) %>% melt() %>% dplyr::select(value)
z34_intel <- fisherz(r34_intel) %>% melt() %>% dplyr::select(value)
z34_opena <- fisherz(r34_opena) %>% melt() %>% dplyr::select(value)
z34_order <- fisherz(r34_order) %>% melt() %>% dplyr::select(value)
z34_polit <- fisherz(r34_polit) %>% melt() %>% dplyr::select(value)
z34_volat <- fisherz(r34_volat) %>% melt() %>% dplyr::select(value)
z34_withd <- fisherz(r34_withd) %>% melt() %>% dplyr::select(value)

# >>> compare correlation and p-value among intervals ----

imp_agree_12_23 <- (z12_agree-z23_agree)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_agree_23_34 <- (z23_agree-z34_agree)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_agree_12_34 <- (z12_agree-z34_agree)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_agree_12_23$p <- pnorm(abs(imp_agree_12_23$value), lower.tail = FALSE)
imp_agree_23_34$p <- pnorm(abs(imp_agree_23_34$value), lower.tail = FALSE)
imp_agree_12_34$p <- pnorm(abs(imp_agree_12_34$value), lower.tail = FALSE)

imp_consci_12_23 <- (z12_consci-z23_consci)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_consci_23_34 <- (z23_consci-z34_consci)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_consci_12_34 <- (z12_consci-z34_consci)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_consci_12_23$p <- pnorm(abs(imp_consci_12_23$value), lower.tail = FALSE)
imp_consci_23_34$p <- pnorm(abs(imp_consci_23_34$value), lower.tail = FALSE)
imp_consci_12_34$p <- pnorm(abs(imp_consci_12_34$value), lower.tail = FALSE)

imp_extra_12_23 <- (z12_extra-z23_extra)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_extra_23_34 <- (z23_extra-z34_extra)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_extra_12_34 <- (z12_extra-z34_extra)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_extra_12_23$p <- pnorm(abs(imp_extra_12_23$value), lower.tail = FALSE)
imp_extra_23_34$p <- pnorm(abs(imp_extra_23_34$value), lower.tail = FALSE)
imp_extra_12_34$p <- pnorm(abs(imp_extra_12_34$value), lower.tail = FALSE)

imp_neuro_12_23 <- (z12_neuro-z23_neuro)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_neuro_23_34 <- (z23_neuro-z34_neuro)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_neuro_12_34 <- (z12_neuro-z34_neuro)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_neuro_12_23$p <- pnorm(abs(imp_neuro_12_23$value), lower.tail = FALSE)
imp_neuro_23_34$p <- pnorm(abs(imp_neuro_23_34$value), lower.tail = FALSE)
imp_neuro_12_34$p <- pnorm(abs(imp_neuro_12_34$value), lower.tail = FALSE)

imp_opend_12_23 <- (z12_opend-z23_opend)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opend_23_34 <- (z23_opend-z34_opend)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opend_12_34 <- (z12_opend-z34_opend)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opend_12_23$p <- pnorm(abs(imp_opend_12_23$value), lower.tail = FALSE)
imp_opend_23_34$p <- pnorm(abs(imp_opend_23_34$value), lower.tail = FALSE)
imp_opend_12_34$p <- pnorm(abs(imp_opend_12_34$value), lower.tail = FALSE)

imp_assert_12_23 <- (z12_assert-z23_assert)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_assert_23_34 <- (z23_assert-z34_assert)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_assert_12_34 <- (z12_assert-z34_assert)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_assert_12_23$p <- pnorm(abs(imp_assert_12_23$value), lower.tail = FALSE)
imp_assert_23_34$p <- pnorm(abs(imp_assert_23_34$value), lower.tail = FALSE)
imp_assert_12_34$p <- pnorm(abs(imp_assert_12_34$value), lower.tail = FALSE)

imp_compa_12_23 <- (z12_compa-z23_compa)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_compa_23_34 <- (z23_compa-z34_compa)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_compa_12_34 <- (z12_compa-z34_compa)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_compa_12_23$p <- pnorm(abs(imp_compa_12_23$value), lower.tail = FALSE)
imp_compa_23_34$p <- pnorm(abs(imp_compa_23_34$value), lower.tail = FALSE)
imp_compa_12_34$p <- pnorm(abs(imp_compa_12_34$value), lower.tail = FALSE)

imp_enthu_12_23 <- (z12_enthu-z23_enthu)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_enthu_23_34 <- (z23_enthu-z34_enthu)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_enthu_12_34 <- (z12_enthu-z34_enthu)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_enthu_12_23$p <- pnorm(abs(imp_enthu_12_23$value), lower.tail = FALSE)
imp_enthu_23_34$p <- pnorm(abs(imp_enthu_23_34$value), lower.tail = FALSE)
imp_enthu_12_34$p <- pnorm(abs(imp_enthu_12_34$value), lower.tail = FALSE)

imp_indus_12_23 <- (z12_indus-z23_indus)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_indus_23_34 <- (z23_indus-z34_indus)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_indus_12_34 <- (z12_indus-z34_indus)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_indus_12_23$p <- pnorm(abs(imp_indus_12_23$value), lower.tail = FALSE)
imp_indus_23_34$p <- pnorm(abs(imp_indus_23_34$value), lower.tail = FALSE)
imp_indus_12_34$p <- pnorm(abs(imp_indus_12_34$value), lower.tail = FALSE)

imp_intel_12_23 <- (z12_intel-z23_intel)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_intel_23_34 <- (z23_intel-z34_intel)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_intel_12_34 <- (z12_intel-z34_intel)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_intel_12_23$p <- pnorm(abs(imp_intel_12_23$value), lower.tail = FALSE)
imp_intel_23_34$p <- pnorm(abs(imp_intel_23_34$value), lower.tail = FALSE)
imp_intel_12_34$p <- pnorm(abs(imp_intel_12_34$value), lower.tail = FALSE)

imp_opena_12_23 <- (z12_opena-z23_opena)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opena_23_34 <- (z23_opena-z34_opena)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opena_12_34 <- (z12_opena-z34_opena)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_opena_12_23$p <- pnorm(abs(imp_opena_12_23$value), lower.tail = FALSE)
imp_opena_23_34$p <- pnorm(abs(imp_opena_23_34$value), lower.tail = FALSE)
imp_opena_12_34$p <- pnorm(abs(imp_opena_12_34$value), lower.tail = FALSE)


imp_order_12_23 <- (z12_order-z23_order)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_order_23_34 <- (z23_order-z34_order)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_order_12_34 <- (z12_order-z34_order)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_order_12_23$p <- pnorm(abs(imp_order_12_23$value), lower.tail = FALSE)
imp_order_23_34$p <- pnorm(abs(imp_order_23_34$value), lower.tail = FALSE)
imp_order_12_34$p <- pnorm(abs(imp_order_12_34$value), lower.tail = FALSE)

imp_polit_12_23 <- (z12_polit-z23_polit)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_polit_23_34 <- (z23_polit-z34_polit)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_polit_12_34 <- (z12_polit-z34_polit)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_polit_12_23$p <- pnorm(abs(imp_polit_12_23$value), lower.tail = FALSE)
imp_polit_23_34$p <- pnorm(abs(imp_polit_23_34$value), lower.tail = FALSE)
imp_polit_12_34$p <- pnorm(abs(imp_polit_12_34$value), lower.tail = FALSE)

imp_volat_12_23 <- (z12_volat-z23_volat)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_volat_23_34 <- (z23_volat-z34_volat)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_volat_12_34 <- (z12_volat-z34_volat)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_volat_12_23$p <- pnorm(abs(imp_volat_12_23$value), lower.tail = FALSE)
imp_volat_23_34$p <- pnorm(abs(imp_volat_23_34$value), lower.tail = FALSE)
imp_volat_12_34$p <- pnorm(abs(imp_volat_12_34$value), lower.tail = FALSE)

imp_withd_12_23 <- (z12_withd-z23_withd)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_withd_23_34 <- (z23_withd-z34_withd)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_withd_12_34 <- (z12_withd-z34_withd)/(sqrt((1/(176 - 3)) + (1/(176-3))))
imp_withd_12_23$p <- pnorm(abs(imp_withd_12_23$value), lower.tail = FALSE)
imp_withd_23_34$p <- pnorm(abs(imp_withd_23_34$value), lower.tail = FALSE)
imp_withd_12_34$p <- pnorm(abs(imp_withd_12_34$value), lower.tail = FALSE)

# > PEER 2b: Rank order change identity ----
# >> Compute correlation fisher z ----
r12_confu <- cor(peerw$epsi_confusion_w1, peerw$epsi_confusion_w2, use = "pairwise.complete.obs")
r12_coher <- cor(peerw$epsi_coherence_w1, peerw$epsi_coherence_w2, use = "pairwise.complete.obs")

z12_confu <- fisherz(r12_confu)
z12_coher <- fisherz(r12_coher)


r23_confu <- cor(peerw$epsi_confusion_w2, peerw$epsi_confusion_w3, use = "pairwise.complete.obs")
r23_coher <- cor(peerw$epsi_coherence_w2, peerw$epsi_coherence_w3, use = "pairwise.complete.obs")

z23_confu <- fisherz(r23_confu)
z23_coher <- fisherz(r23_coher)

r34_confu <- cor(peerw$epsi_confusion_w3, peerw$epsi_confusion_w4, use = "pairwise.complete.obs")
r34_coher <- cor(peerw$epsi_coherence_w3, peerw$epsi_coherence_w4, use = "pairwise.complete.obs")

z34_confu <- fisherz(r34_confu)
z34_coher <- fisherz(r34_coher)

r14_confu <- cor(peerw$epsi_confusion_w1, peerw$epsi_confusion_w4, use = "pairwise.complete.obs")
r14_coher <- cor(peerw$epsi_coherence_w1, peerw$epsi_coherence_w4, use = "pairwise.complete.obs")

z14_confu <- fisherz(r14_confu)
z14_coher <- fisherz(r14_coher)

r13_confu <- cor(peerw$epsi_confusion_w1, peerw$epsi_confusion_w3, use = "pairwise.complete.obs")
r13_coher <- cor(peerw$epsi_coherence_w1, peerw$epsi_coherence_w3, use = "pairwise.complete.obs")

z13_confu <- fisherz(r13_confu)
z13_coher <- fisherz(r13_coher)

r24_confu <- cor(peerw$epsi_confusion_w2, peerw$epsi_confusion_w4, use = "pairwise.complete.obs")
r24_coher <- cor(peerw$epsi_coherence_w2, peerw$epsi_coherence_w4, use = "pairwise.complete.obs")

z24_confu <- fisherz(r24_confu)
z24_coher <- fisherz(r24_coher)
# >> determine Ns for comparison ----
n_confu_12 <- sum(peerw$epsi_confusion_w2 != "NA", na.rm = TRUE)
n_coher_12 <- sum(peerw$epsi_coherence_w2 != "NA", na.rm = TRUE)
n_confu_23 <- sum(peerw$epsi_confusion_w3 != "NA", na.rm = TRUE)
n_coher_23 <- sum(peerw$epsi_coherence_w3 != "NA", na.rm = TRUE)
n_confu_34 <- sum(peerw$epsi_confusion_w4 != "NA", na.rm = TRUE)
n_coher_34 <- sum(peerw$epsi_coherence_w4 != "NA", na.rm = TRUE)
# >> compare correlation among intervals ----
rcon12confu <- r.con(r12_confu, n_confu_12)
rcon23confu <- r.con(r12_confu, n_confu_23)
rcon34confu <- r.con(r34_confu, n_confu_34)

rcon12coher <- r.con(r12_coher, n_coher_12)
rcon23coher <- r.con(r12_coher, n_coher_23)
rcon34coher <- r.con(r34_coher, n_coher_34)

confu_12_23 <- (z12_confu-z23_confu)/(sqrt((1/(n_confu_12 - 3)) + (1/(n_confu_23-3))))
confu_23_34 <- (z23_confu-z34_confu)/(sqrt((1/(n_confu_23 - 3)) + (1/(n_confu_34-3))))
confu_12_34 <- (z12_confu-z34_confu)/(sqrt((1/(n_confu_12 - 3)) + (1/(n_confu_34-3))))
p_confu_12_23 <- pnorm(abs(confu_12_23), lower.tail = FALSE)
p_confu_23_34 <- pnorm(abs(confu_23_34), lower.tail = FALSE)
p_confu_12_34 <- pnorm(abs(confu_12_34), lower.tail = FALSE)

coher_12_23 <- (z12_coher-z23_coher)/(sqrt((1/(n_coher_12 - 3)) + (1/(n_coher_23-3))))
coher_23_34 <- (z23_coher-z34_coher)/(sqrt((1/(n_coher_23 - 3)) + (1/(n_coher_34-3))))
coher_12_34 <- (z12_coher-z34_coher)/(sqrt((1/(n_coher_12 - 3)) + (1/(n_coher_34-3))))
p_coher_12_23 <- pnorm(abs(coher_12_23), lower.tail = FALSE)
p_coher_23_34 <- pnorm(abs(coher_23_34), lower.tail = FALSE)
p_coher_12_34 <- pnorm(abs(coher_12_34), lower.tail = FALSE)
# >> imputations ----
# >>> compute correlation fisher z ----

### correlation between T1 and T2
r12_confu <- with(data = imp_wide_p,
                   exp = cor(epsi_confusion_w1, epsi_confusion_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r12_coher <- with(data = imp_wide_p,
                   exp = cor(epsi_coherence_w1, epsi_coherence_w2, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z12_confu <- fisherz(r12_confu) %>% melt() %>% dplyr::select(value)
z12_coher <- fisherz(r12_coher) %>% melt() %>% dplyr::select(value)

### correlation between T2 and T3
r23_confu <- with(data = imp_wide_p,
                   exp = cor(epsi_confusion_w2, epsi_confusion_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r23_coher <- with(data = imp_wide_p,
                   exp = cor(epsi_coherence_w2, epsi_coherence_w3, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z23_confu <- fisherz(r23_confu) %>% melt() %>% dplyr::select(value)
z23_coher <- fisherz(r23_coher) %>% melt() %>% dplyr::select(value)

### correlation between T3 and T4
r34_confu <- with(data = imp_wide_p,
                   exp = cor(epsi_confusion_w3, epsi_confusion_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()
r34_coher <- with(data = imp_wide_p,
                   exp = cor(epsi_coherence_w3, epsi_coherence_w4, 
                             use = "pairwise.complete.obs")) %>% as.data.frame()

z34_confu <- fisherz(r34_confu) %>% melt() %>% dplyr::select(value)
z34_coher <- fisherz(r34_coher) %>% melt() %>% dplyr::select(value)

# >>> compare correlation and p-value among intervals ----
imp_confu_12_23 <- (z12_confu-z23_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_23_34 <- (z23_confu-z34_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_12_34 <- (z12_confu-z34_confu)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_confu_12_23$p <- pnorm(abs(imp_confu_12_23$value), lower.tail = FALSE)
imp_confu_23_34$p <- pnorm(abs(imp_confu_23_34$value), lower.tail = FALSE)
imp_confu_12_34$p <- pnorm(abs(imp_confu_12_34$value), lower.tail = FALSE)

imp_coher_12_23 <- (z12_coher-z23_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_23_34 <- (z23_coher-z34_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_12_34 <- (z12_coher-z34_coher)/(sqrt((1/(259 - 3)) + (1/(259-3))))
imp_coher_12_23$p <- pnorm(abs(imp_coher_12_23$value), lower.tail = FALSE)
imp_coher_23_34$p <- pnorm(abs(imp_coher_23_34$value), lower.tail = FALSE)
imp_coher_12_34$p <- pnorm(abs(imp_coher_12_34$value), lower.tail = FALSE)
# > PEER 3a: Individual differences in change personality ----

### Agreeableness
anova(linearint.agree, linear.agree)

### Assertiveness
anova(linearint.assert, linear.assert)

### Compassion
anova(linearint.compa, linear.compa)

### Conscientiousness
anova(linearint.consci, linear.consci)

### Enthusiasm
anova(linearint.enthu, linear.enthu)

### Extraversion
anova(linearint.extra, linear.extra)

### Industriousness
anova(linearint.indus, linear.indus)

### Intellect
anova(linearint.intel, linear.intel)

### Neuroticism
anova(linearint.neuro, linear.neuro)

### Openness Aspect
anova(linearint.opena, linear.opena)

### Openness Domain
anova(linearint.opend, linear.opend)

### Orderliness
anova(linearint.order, linear.order)

### Politeness
anova(linearint.polit, linear.polit)

### Volatility
anova(linearint.volat, linear.volat)

### Withdrawal
anova(linearint.withd, linear.withd)

# > PEER 3b: Individual differences in change identity ----
### Coherence
anova(linearint.coher, linear.coher)

### Confusion
anova(linearint.confu, linear.confu)
# > PEER 4a: Ipsative change personality ----

# >> compute D2, D'2, D''2 in simulated data ----

sim.domain <- read.csv("domainsim_peer.csv")
sim.aspect <- read.csv("aspectsim_peer.csv")

sim.domain <- sim.domain %>% 
  mutate(domain_d2_12 = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w2)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w2)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w2)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w2)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w2)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_23 = 
           (bfas_agreeableness_w2 - bfas_agreeableness_w3)^2 +
           (bfas_conscientiousness_w2 - bfas_conscientiousness_w3)^2 +
           (bfas_extraversion_w2 - bfas_extraversion_w3)^2 +
           (bfas_neuroticism_w2 - bfas_neuroticism_w3)^2 +
           (bfas_opennessdomain_w2 - bfas_opennessdomain_w3)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_34 = 
           (bfas_agreeableness_w3 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w3 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w3 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w3 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w3 - bfas_opennessdomain_w4)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_d2_all = 
           (bfas_agreeableness_w1 - bfas_agreeableness_w4)^2 +
           (bfas_conscientiousness_w1 - bfas_conscientiousness_w4)^2 +
           (bfas_extraversion_w1 - bfas_extraversion_w4)^2 +
           (bfas_neuroticism_w1 - bfas_neuroticism_w4)^2 +
           (bfas_opennessdomain_w1 - bfas_opennessdomain_w4)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_12 = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w2)^2 +
           (bfas_compassion_w1 - bfas_compassion_w2)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w2)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w2)^2 +
           (bfas_intellect_w1 - bfas_intellect_w2)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w2)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w2)^2 +
           (bfas_politeness_w1 - bfas_politeness_w2)^2 +
           (bfas_volatility_w1 - bfas_volatility_w2)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w2)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_23 = 
           (bfas_assertiveness_w2 - bfas_assertiveness_w3)^2 +
           (bfas_compassion_w2 - bfas_compassion_w3)^2 +
           (bfas_enthusiasm_w2 - bfas_enthusiasm_w3)^2 +
           (bfas_industriousness_w2 - bfas_industriousness_w3)^2 +
           (bfas_intellect_w2 - bfas_intellect_w3)^2 +
           (bfas_opennessaspect_w2 - bfas_opennessaspect_w3)^2 +
           (bfas_orderliness_w2 - bfas_orderliness_w3)^2 +
           (bfas_politeness_w2 - bfas_politeness_w3)^2 +
           (bfas_volatility_w2 - bfas_volatility_w3)^2 +
           (bfas_withdrawal_w2 - bfas_withdrawal_w3)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_34 = 
           (bfas_assertiveness_w3 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w3 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w3 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w3 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w3 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w3 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w3 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w3 - bfas_politeness_w4)^2 +
           (bfas_volatility_w3 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w3 - bfas_withdrawal_w4)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_d2_all = 
           (bfas_assertiveness_w1 - bfas_assertiveness_w4)^2 +
           (bfas_compassion_w1 - bfas_compassion_w4)^2 +
           (bfas_enthusiasm_w1 - bfas_enthusiasm_w4)^2 +
           (bfas_industriousness_w1 - bfas_industriousness_w4)^2 +
           (bfas_intellect_w1 - bfas_intellect_w4)^2 +
           (bfas_opennessaspect_w1 - bfas_opennessaspect_w4)^2 +
           (bfas_orderliness_w1 - bfas_orderliness_w4)^2 +
           (bfas_politeness_w1 - bfas_politeness_w4)^2 +
           (bfas_volatility_w1 - bfas_volatility_w4)^2 +
           (bfas_withdrawal_w1 - bfas_withdrawal_w4)^2)

### domains
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                      bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_agreeableness_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_conscientiousness_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                          bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_extraversion_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                     bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_neuroticism_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w1_d = (bfas_agreeableness_w1 + bfas_conscientiousness_w1 + bfas_extraversion_w1 + 
                                       bfas_neuroticism_w1 + bfas_opennessdomain_w1)/5 - bfas_opennessdomain_w1)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                      bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_agreeableness_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                          bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_conscientiousness_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                     bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_extraversion_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                    bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_neuroticism_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w2_d = (bfas_agreeableness_w2 + bfas_conscientiousness_w2 + bfas_extraversion_w2 + 
                                       bfas_neuroticism_w2 + bfas_opennessdomain_w2)/5 - bfas_opennessdomain_w2)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                      bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_agreeableness_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                          bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_conscientiousness_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                     bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_extraversion_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                    bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_neuroticism_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w3_d = (bfas_agreeableness_w3 + bfas_conscientiousness_w3 + bfas_extraversion_w3 + 
                                       bfas_neuroticism_w3 + bfas_opennessdomain_w3)/5 - bfas_opennessdomain_w3)
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                      bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_agreeableness_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                          bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_conscientiousness_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                     bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_extraversion_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                    bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_neuroticism_w4)
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w4_d = (bfas_agreeableness_w4 + bfas_conscientiousness_w4 + bfas_extraversion_w4 + 
                                       bfas_neuroticism_w4 + bfas_opennessdomain_w4)/5 - bfas_opennessdomain_w4)

### aspects
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                      bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                      bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                      bfas_withdrawal_w1)/10 - bfas_assertiveness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_compassion_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_enthusiasm_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                        bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                        bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                        bfas_withdrawal_w1)/10 - bfas_industriousness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                  bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                  bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                  bfas_withdrawal_w1)/10 - bfas_intellect_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                       bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                       bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                       bfas_withdrawal_w1)/10 - bfas_opennessaspect_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                    bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                    bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                    bfas_withdrawal_w1)/10 - bfas_orderliness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_politeness_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_volatility_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w1_d = (bfas_assertiveness_w1 + bfas_compassion_w1 + bfas_enthusiasm_w1 + 
                                   bfas_industriousness_w1 + bfas_intellect_w1 + bfas_opennessaspect_w1 +
                                   bfas_orderliness_w1 + bfas_politeness_w1 + bfas_volatility_w1 + 
                                   bfas_withdrawal_w1)/10 - bfas_withdrawal_w1)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                      bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                      bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                      bfas_withdrawal_w2)/10 - bfas_assertiveness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_compassion_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_enthusiasm_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                        bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                        bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                        bfas_withdrawal_w2)/10 - bfas_industriousness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                  bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                  bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                  bfas_withdrawal_w2)/10 - bfas_intellect_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                       bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                       bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                       bfas_withdrawal_w2)/10 - bfas_opennessaspect_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                    bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                    bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                    bfas_withdrawal_w2)/10 - bfas_orderliness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_politeness_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_volatility_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w2_d = (bfas_assertiveness_w2 + bfas_compassion_w2 + bfas_enthusiasm_w2 + 
                                   bfas_industriousness_w2 + bfas_intellect_w2 + bfas_opennessaspect_w2 +
                                   bfas_orderliness_w2 + bfas_politeness_w2 + bfas_volatility_w2 + 
                                   bfas_withdrawal_w2)/10 - bfas_withdrawal_w2)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                      bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                      bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                      bfas_withdrawal_w3)/10 - bfas_assertiveness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_compassion_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_enthusiasm_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                        bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                        bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                        bfas_withdrawal_w3)/10 - bfas_industriousness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                  bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                  bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                  bfas_withdrawal_w3)/10 - bfas_intellect_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                       bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                       bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                       bfas_withdrawal_w3)/10 - bfas_opennessaspect_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                    bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                    bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                    bfas_withdrawal_w3)/10 - bfas_orderliness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_politeness_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_volatility_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w3_d = (bfas_assertiveness_w3 + bfas_compassion_w3 + bfas_enthusiasm_w3 + 
                                   bfas_industriousness_w3 + bfas_intellect_w3 + bfas_opennessaspect_w3 +
                                   bfas_orderliness_w3 + bfas_politeness_w3 + bfas_volatility_w3 + 
                                   bfas_withdrawal_w3)/10 - bfas_withdrawal_w3)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                      bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                      bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                      bfas_withdrawal_w4)/10 - bfas_assertiveness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_compassion_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_enthusiasm_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                        bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                        bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                        bfas_withdrawal_w4)/10 - bfas_industriousness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                  bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                  bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                  bfas_withdrawal_w4)/10 - bfas_intellect_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                       bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                       bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                       bfas_withdrawal_w4)/10 - bfas_opennessaspect_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                    bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                    bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                    bfas_withdrawal_w4)/10 - bfas_orderliness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_politeness_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_volatility_w4)
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w4_d = (bfas_assertiveness_w4 + bfas_compassion_w4 + bfas_enthusiasm_w4 + 
                                   bfas_industriousness_w4 + bfas_intellect_w4 + bfas_opennessaspect_w4 +
                                   bfas_orderliness_w4 + bfas_politeness_w4 + bfas_volatility_w4 + 
                                   bfas_withdrawal_w4)/10 - bfas_withdrawal_w4)

### compute D'2 (name: dp) - eliminate elevation - using deviation scores computed above
sim.domain <- sim.domain %>% 
  mutate(domain_dp_12 = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w2_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w2_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w2_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w2_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w2_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_23 = 
           (bfas_agreeableness_w2_d - bfas_agreeableness_w3_d)^2 +
           (bfas_conscientiousness_w2_d - bfas_conscientiousness_w3_d)^2 +
           (bfas_extraversion_w2_d - bfas_extraversion_w3_d)^2 +
           (bfas_neuroticism_w2_d - bfas_neuroticism_w3_d)^2 +
           (bfas_opennessdomain_w2_d - bfas_opennessdomain_w3_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_34 = 
           (bfas_agreeableness_w3_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w3_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w3_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w3_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w3_d - bfas_opennessdomain_w4_d)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dp_all = 
           (bfas_agreeableness_w1_d - bfas_agreeableness_w4_d)^2 +
           (bfas_conscientiousness_w1_d - bfas_conscientiousness_w4_d)^2 +
           (bfas_extraversion_w1_d - bfas_extraversion_w4_d)^2 +
           (bfas_neuroticism_w1_d - bfas_neuroticism_w4_d)^2 +
           (bfas_opennessdomain_w1_d - bfas_opennessdomain_w4_d)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_12 = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w2_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w2_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w2_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w2_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w2_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w2_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w2_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w2_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w2_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w2_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_23 = 
           (bfas_assertiveness_w2_d - bfas_assertiveness_w3_d)^2 +
           (bfas_compassion_w2_d - bfas_compassion_w3_d)^2 +
           (bfas_enthusiasm_w2_d - bfas_enthusiasm_w3_d)^2 +
           (bfas_industriousness_w2_d - bfas_industriousness_w3_d)^2 +
           (bfas_intellect_w2_d - bfas_intellect_w3_d)^2 +
           (bfas_opennessaspect_w2_d - bfas_opennessaspect_w3_d)^2 +
           (bfas_orderliness_w2_d - bfas_orderliness_w3_d)^2 +
           (bfas_politeness_w2_d - bfas_politeness_w3_d)^2 +
           (bfas_volatility_w2_d - bfas_volatility_w3_d)^2 +
           (bfas_withdrawal_w2_d - bfas_withdrawal_w3_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_34 = 
           (bfas_assertiveness_w3_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w3_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w3_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w3_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w3_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w3_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w3_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w3_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w3_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w3_d - bfas_withdrawal_w4_d)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dp_all = 
           (bfas_assertiveness_w1_d - bfas_assertiveness_w4_d)^2 +
           (bfas_compassion_w1_d - bfas_compassion_w4_d)^2 +
           (bfas_enthusiasm_w1_d - bfas_enthusiasm_w4_d)^2 +
           (bfas_industriousness_w1_d - bfas_industriousness_w4_d)^2 +
           (bfas_intellect_w1_d - bfas_intellect_w4_d)^2 +
           (bfas_opennessaspect_w1_d - bfas_opennessaspect_w4_d)^2 +
           (bfas_orderliness_w1_d - bfas_orderliness_w4_d)^2 +
           (bfas_politeness_w1_d - bfas_politeness_w4_d)^2 +
           (bfas_volatility_w1_d - bfas_volatility_w4_d)^2 +
           (bfas_withdrawal_w1_d - bfas_withdrawal_w4_d)^2)

### compute standardized scores
###domain
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w1_ds = bfas_agreeableness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                   bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                   bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w1_ds = bfas_conscientiousness_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                            bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                            bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w1_ds = bfas_extraversion_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                  bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                  bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w1_ds = bfas_neuroticism_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w1_ds = bfas_opennessdomain_w1_d/(sqrt(bfas_agreeableness_w1_d^2 + bfas_conscientiousness_w1_d^2 + 
                                                                      bfas_extraversion_w1_d^2 + bfas_neuroticism_w1_d^2 + 
                                                                      bfas_opennessdomain_w1_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w2_ds = bfas_agreeableness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                    bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                    bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w2_ds = bfas_conscientiousness_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                            bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                            bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w2_ds = bfas_extraversion_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                  bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                  bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w2_ds = bfas_neuroticism_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w2_ds = bfas_opennessdomain_w2_d/(sqrt(bfas_agreeableness_w2_d^2 + bfas_conscientiousness_w2_d^2 + 
                                                                      bfas_extraversion_w2_d^2 + bfas_neuroticism_w2_d^2 + 
                                                                      bfas_opennessdomain_w2_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w3_ds = bfas_agreeableness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                    bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                    bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w3_ds = bfas_conscientiousness_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                            bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                            bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w3_ds = bfas_extraversion_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                  bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                  bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w3_ds = bfas_neuroticism_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w3_ds = bfas_opennessdomain_w3_d/(sqrt(bfas_agreeableness_w3_d^2 + bfas_conscientiousness_w3_d^2 + 
                                                                      bfas_extraversion_w3_d^2 + bfas_neuroticism_w3_d^2 + 
                                                                      bfas_opennessdomain_w3_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_agreeableness_w4_ds = bfas_agreeableness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                    bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                    bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_conscientiousness_w4_ds = bfas_conscientiousness_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                            bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                            bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_extraversion_w4_ds = bfas_extraversion_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                  bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                  bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_neuroticism_w4_ds = bfas_neuroticism_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                bfas_opennessdomain_w4_d^2)))
sim.domain <- sim.domain %>% 
  mutate(bfas_opennessdomain_w4_ds = bfas_opennessdomain_w4_d/(sqrt(bfas_agreeableness_w4_d^2 + bfas_conscientiousness_w4_d^2 + 
                                                                      bfas_extraversion_w4_d^2 + bfas_neuroticism_w4_d^2 + 
                                                                      bfas_opennessdomain_w4_d^2)))

### aspect
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w1_ds = bfas_assertiveness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                    bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                    bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                    bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                    bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w1_ds = bfas_compassion_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w1_ds = bfas_enthusiasm_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w1_ds = bfas_industriousness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                        bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                        bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                        bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                        bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w1_ds = bfas_intellect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                            bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                            bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                            bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                            bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w1_ds = bfas_opennessaspect_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                      bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                      bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                      bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                      bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w1_ds = bfas_orderliness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                                bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                                bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                                bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                                bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w1_ds = bfas_politeness_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w1_ds = bfas_volatility_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w1_ds = bfas_withdrawal_w1_d/(sqrt(bfas_assertiveness_w1_d^2 + bfas_compassion_w1_d^2 + 
                                                              bfas_enthusiasm_w1_d^2 + bfas_industriousness_w1_d^2 + 
                                                              bfas_intellect_w1_d^2 + bfas_opennessaspect_w1_d^2 +
                                                              bfas_orderliness_w1_d^2 + bfas_politeness_w1_d^2 +
                                                              bfas_volatility_w1_d^2 + bfas_withdrawal_w1_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w2_ds = bfas_assertiveness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                    bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                    bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                    bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                    bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w2_ds = bfas_compassion_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w2_ds = bfas_enthusiasm_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w2_ds = bfas_industriousness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                        bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                        bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                        bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                        bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w2_ds = bfas_intellect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                            bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                            bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                            bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                            bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w2_ds = bfas_opennessaspect_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                      bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                      bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                      bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                      bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w2_ds = bfas_orderliness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                                bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                                bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                                bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                                bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w2_ds = bfas_politeness_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w2_ds = bfas_volatility_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w2_ds = bfas_withdrawal_w2_d/(sqrt(bfas_assertiveness_w2_d^2 + bfas_compassion_w2_d^2 + 
                                                              bfas_enthusiasm_w2_d^2 + bfas_industriousness_w2_d^2 + 
                                                              bfas_intellect_w2_d^2 + bfas_opennessaspect_w2_d^2 +
                                                              bfas_orderliness_w2_d^2 + bfas_politeness_w2_d^2 +
                                                              bfas_volatility_w2_d^2 + bfas_withdrawal_w2_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w3_ds = bfas_assertiveness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                    bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                    bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                    bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                    bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w3_ds = bfas_compassion_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w3_ds = bfas_enthusiasm_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w3_ds = bfas_industriousness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                        bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                        bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                        bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                        bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w3_ds = bfas_intellect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                            bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                            bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                            bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                            bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w3_ds = bfas_opennessaspect_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                      bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                      bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                      bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                      bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w3_ds = bfas_orderliness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                                bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                                bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                                bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                                bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w3_ds = bfas_politeness_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w3_ds = bfas_volatility_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w3_ds = bfas_withdrawal_w3_d/(sqrt(bfas_assertiveness_w3_d^2 + bfas_compassion_w3_d^2 + 
                                                              bfas_enthusiasm_w3_d^2 + bfas_industriousness_w3_d^2 + 
                                                              bfas_intellect_w3_d^2 + bfas_opennessaspect_w3_d^2 +
                                                              bfas_orderliness_w3_d^2 + bfas_politeness_w3_d^2 +
                                                              bfas_volatility_w3_d^2 + bfas_withdrawal_w3_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_assertiveness_w4_ds = bfas_assertiveness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                    bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                    bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                    bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                    bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_compassion_w4_ds = bfas_compassion_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_enthusiasm_w4_ds = bfas_enthusiasm_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_industriousness_w4_ds = bfas_industriousness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                        bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                        bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                        bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                        bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_intellect_w4_ds = bfas_intellect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                            bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                            bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                            bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                            bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_opennessaspect_w4_ds = bfas_opennessaspect_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                      bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                      bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                      bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                      bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_orderliness_w4_ds = bfas_orderliness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                                bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                                bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                                bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                                bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_politeness_w4_ds = bfas_politeness_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_volatility_w4_ds = bfas_volatility_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))
sim.aspect <- sim.aspect %>% 
  mutate(bfas_withdrawal_w4_ds = bfas_withdrawal_w4_d/(sqrt(bfas_assertiveness_w4_d^2 + bfas_compassion_w4_d^2 + 
                                                              bfas_enthusiasm_w4_d^2 + bfas_industriousness_w4_d^2 + 
                                                              bfas_intellect_w4_d^2 + bfas_opennessaspect_w4_d^2 +
                                                              bfas_orderliness_w4_d^2 + bfas_politeness_w4_d^2 +
                                                              bfas_volatility_w4_d^2 + bfas_withdrawal_w4_d^2)))

### compute D''2 (name: dpp) - eliminate elevation and scatter - using standardized deviation scores computed above
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_12 = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w2_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w2_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w2_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w2_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w2_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_23 = 
           (bfas_agreeableness_w2_ds - bfas_agreeableness_w3_ds)^2 +
           (bfas_conscientiousness_w2_ds - bfas_conscientiousness_w3_ds)^2 +
           (bfas_extraversion_w2_ds - bfas_extraversion_w3_ds)^2 +
           (bfas_neuroticism_w2_ds - bfas_neuroticism_w3_ds)^2 +
           (bfas_opennessdomain_w2_ds - bfas_opennessdomain_w3_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_34 = 
           (bfas_agreeableness_w3_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w3_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w3_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w3_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w3_ds - bfas_opennessdomain_w4_ds)^2)
sim.domain <- sim.domain %>% 
  mutate(domain_dpp_all = 
           (bfas_agreeableness_w1_ds - bfas_agreeableness_w4_ds)^2 +
           (bfas_conscientiousness_w1_ds - bfas_conscientiousness_w4_ds)^2 +
           (bfas_extraversion_w1_ds - bfas_extraversion_w4_ds)^2 +
           (bfas_neuroticism_w1_ds - bfas_neuroticism_w4_ds)^2 +
           (bfas_opennessdomain_w1_ds - bfas_opennessdomain_w4_ds)^2)

sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_12 = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w2_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w2_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w2_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w2_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w2_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w2_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w2_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w2_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w2_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w2_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_23 = 
           (bfas_assertiveness_w2_ds - bfas_assertiveness_w3_ds)^2 +
           (bfas_compassion_w2_ds - bfas_compassion_w3_ds)^2 +
           (bfas_enthusiasm_w2_ds - bfas_enthusiasm_w3_ds)^2 +
           (bfas_industriousness_w2_ds - bfas_industriousness_w3_ds)^2 +
           (bfas_intellect_w2_ds - bfas_intellect_w3_ds)^2 +
           (bfas_opennessaspect_w2_ds - bfas_opennessaspect_w3_ds)^2 +
           (bfas_orderliness_w2_ds - bfas_orderliness_w3_ds)^2 +
           (bfas_politeness_w2_ds - bfas_politeness_w3_ds)^2 +
           (bfas_volatility_w2_ds - bfas_volatility_w3_ds)^2 +
           (bfas_withdrawal_w2_ds - bfas_withdrawal_w3_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_34 = 
           (bfas_assertiveness_w3_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w3_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w3_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w3_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w3_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w3_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w3_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w3_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w3_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w3_ds - bfas_withdrawal_w4_ds)^2)
sim.aspect <- sim.aspect %>% 
  mutate(aspect_dpp_all = 
           (bfas_assertiveness_w1_ds - bfas_assertiveness_w4_ds)^2 +
           (bfas_compassion_w1_ds - bfas_compassion_w4_ds)^2 +
           (bfas_enthusiasm_w1_ds - bfas_enthusiasm_w4_ds)^2 +
           (bfas_industriousness_w1_ds - bfas_industriousness_w4_ds)^2 +
           (bfas_intellect_w1_ds - bfas_intellect_w4_ds)^2 +
           (bfas_opennessaspect_w1_ds - bfas_opennessaspect_w4_ds)^2 +
           (bfas_orderliness_w1_ds - bfas_orderliness_w4_ds)^2 +
           (bfas_politeness_w1_ds - bfas_politeness_w4_ds)^2 +
           (bfas_volatility_w1_ds - bfas_volatility_w4_ds)^2 +
           (bfas_withdrawal_w1_ds - bfas_withdrawal_w4_ds)^2)

# >> create 99.9% cutoff from simulated data ----

cutoff_domain_d2_12 <- mean(sim.domain$domain_d2_12) + 3.291*(sd(sim.domain$domain_d2_12)/sqrt(50000))
cutoff_domain_d2_23 <- mean(sim.domain$domain_d2_23) + 3.291*(sd(sim.domain$domain_d2_23)/sqrt(50000))
cutoff_domain_d2_34 <- mean(sim.domain$domain_d2_34) + 3.291*(sd(sim.domain$domain_d2_34)/sqrt(50000))
cutoff_domain_d2_all <- mean(sim.domain$domain_d2_all) + 3.291*(sd(sim.domain$domain_d2_all)/sqrt(50000))
cutoff_domain_dp_12 <- mean(sim.domain$domain_dp_12) + 3.291*(sd(sim.domain$domain_dp_12)/sqrt(50000))
cutoff_domain_dp_23 <- mean(sim.domain$domain_dp_23) + 3.291*(sd(sim.domain$domain_dp_23)/sqrt(50000))
cutoff_domain_dp_34 <- mean(sim.domain$domain_dp_34) + 3.291*(sd(sim.domain$domain_dp_34)/sqrt(50000))
cutoff_domain_dp_all <- mean(sim.domain$domain_dp_all) + 3.291*(sd(sim.domain$domain_dp_all)/sqrt(50000))
cutoff_domain_dpp_12 <- mean(sim.domain$domain_dpp_12) + 3.291*(sd(sim.domain$domain_dpp_12)/sqrt(50000))
cutoff_domain_dpp_23 <- mean(sim.domain$domain_dpp_23) + 3.291*(sd(sim.domain$domain_dpp_23)/sqrt(50000))
cutoff_domain_dpp_34 <- mean(sim.domain$domain_dpp_34) + 3.291*(sd(sim.domain$domain_dpp_34)/sqrt(50000))
cutoff_domain_dpp_all <- mean(sim.domain$domain_dpp_all) + 3.291*(sd(sim.domain$domain_dpp_all)/sqrt(50000))

cutoff_aspect_d2_12 <- mean(sim.aspect$aspect_d2_12) + 3.291*(sd(sim.aspect$aspect_d2_12)/sqrt(50000))
cutoff_aspect_d2_23 <- mean(sim.aspect$aspect_d2_23) + 3.291*(sd(sim.aspect$aspect_d2_23)/sqrt(50000))
cutoff_aspect_d2_34 <- mean(sim.aspect$aspect_d2_34) + 3.291*(sd(sim.aspect$aspect_d2_34)/sqrt(50000))
cutoff_aspect_d2_all <- mean(sim.aspect$aspect_d2_all) + 3.291*(sd(sim.aspect$aspect_d2_all)/sqrt(50000))
cutoff_aspect_dp_12 <- mean(sim.aspect$aspect_dp_12) + 3.291*(sd(sim.aspect$aspect_dp_12)/sqrt(50000))
cutoff_aspect_dp_23 <- mean(sim.aspect$aspect_dp_23) + 3.291*(sd(sim.aspect$aspect_dp_23)/sqrt(50000))
cutoff_aspect_dp_34 <- mean(sim.aspect$aspect_dp_34) + 3.291*(sd(sim.aspect$aspect_dp_34)/sqrt(50000))
cutoff_aspect_dp_all <- mean(sim.aspect$aspect_dp_all) + 3.291*(sd(sim.aspect$aspect_dp_all)/sqrt(50000))
cutoff_aspect_dpp_12 <- mean(sim.aspect$aspect_dpp_12) + 3.291*(sd(sim.aspect$aspect_dpp_12)/sqrt(50000))
cutoff_aspect_dpp_23 <- mean(sim.aspect$aspect_dpp_23) + 3.291*(sd(sim.aspect$aspect_dpp_23)/sqrt(50000))
cutoff_aspect_dpp_34 <- mean(sim.aspect$aspect_dpp_34) + 3.291*(sd(sim.aspect$aspect_dpp_34)/sqrt(50000))
cutoff_aspect_dpp_all <- mean(sim.aspect$aspect_dpp_all) + 3.291*(sd(sim.aspect$aspect_dpp_all)/sqrt(50000))

# >> ipsative personality ----
ipsative_personality(peerw)

# >>> compute for each imputed dataset ----
imp_ips_pers1 <- ipsative_personality(peerw_1)
imp_ips_pers2 <- ipsative_personality(peerw_2)
imp_ips_pers3 <- ipsative_personality(peerw_3)
imp_ips_pers4 <- ipsative_personality(peerw_4)
imp_ips_pers5 <- ipsative_personality(peerw_5)
imp_ips_pers6 <- ipsative_personality(peerw_6)
imp_ips_pers7 <- ipsative_personality(peerw_7)
imp_ips_pers8 <- ipsative_personality(peerw_8)
imp_ips_pers9 <- ipsative_personality(peerw_9)
imp_ips_pers10 <- ipsative_personality(peerw_10)
imp_ips_pers11 <- ipsative_personality(peerw_11)
imp_ips_pers12 <- ipsative_personality(peerw_12)
imp_ips_pers13 <- ipsative_personality(peerw_13)
imp_ips_pers14 <- ipsative_personality(peerw_14)
imp_ips_pers15 <- ipsative_personality(peerw_15)
imp_ips_pers16 <- ipsative_personality(peerw_16)
imp_ips_pers17 <- ipsative_personality(peerw_17)
imp_ips_pers18 <- ipsative_personality(peerw_18)
imp_ips_pers19 <- ipsative_personality(peerw_19)
imp_ips_pers20 <- ipsative_personality(peerw_20)
imp_ips_pers <- rbind(imp_ips_pers1, imp_ips_pers2, imp_ips_pers3, imp_ips_pers4,
                      imp_ips_pers5, imp_ips_pers6, imp_ips_pers7, imp_ips_pers8,
                      imp_ips_pers9, imp_ips_pers10, imp_ips_pers11, imp_ips_pers12,
                      imp_ips_pers13, imp_ips_pers14, imp_ips_pers15, imp_ips_pers16,
                      imp_ips_pers17, imp_ips_pers18, imp_ips_pers19, imp_ips_pers20)

# >>> aggregating ipsative change across imputations ----
imp_ips_pers %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()


# > PEER 4b: Ipsative change identity ----
# >> in simulated dataset ----

sim.identity <- read.csv("identitysim_peer.csv")
sim.identity <- sim.identity %>% 
  mutate(identity_d2_12 = 
           (epsi_confusion_w1 - epsi_confusion_w2)^2 +
           (epsi_coherence_w1 - epsi_coherence_w2)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_23 = 
           (epsi_confusion_w2 - epsi_confusion_w3)^2 +
           (epsi_coherence_w2 - epsi_coherence_w3)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_34 = 
           (epsi_confusion_w3 - epsi_confusion_w4)^2 +
           (epsi_coherence_w3 - epsi_coherence_w4)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_d2_all = 
           (epsi_confusion_w1 - epsi_confusion_w4)^2 +
           (epsi_coherence_w1 - epsi_coherence_w4)^2)

sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_confusion_w1)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w1_d = (epsi_confusion_w1 + epsi_coherence_w1)/2 - epsi_coherence_w1)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_confusion_w2)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w2_d = (epsi_confusion_w2 + epsi_coherence_w2)/2 - epsi_coherence_w2)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_confusion_w3)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w3_d = (epsi_confusion_w3 + epsi_coherence_w3)/2 - epsi_coherence_w3)
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_confusion_w4)
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w4_d = (epsi_confusion_w4 + epsi_coherence_w4)/2 - epsi_coherence_w4)


sim.identity <- sim.identity %>% 
  mutate(identity_dp_12 = 
           (epsi_confusion_w1_d - epsi_confusion_w2_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w2_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_23 = 
           (epsi_confusion_w2_d - epsi_confusion_w3_d)^2 +
           (epsi_coherence_w2_d - epsi_coherence_w3_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_34 = 
           (epsi_confusion_w3_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w3_d - epsi_coherence_w4_d)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dp_all = 
           (epsi_confusion_w1_d - epsi_confusion_w4_d)^2 +
           (epsi_coherence_w1_d - epsi_coherence_w4_d)^2)

sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w1_ds = epsi_confusion_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w1_ds = epsi_coherence_w1_d/(sqrt(epsi_confusion_w1_d^2 + epsi_coherence_w1_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w2_ds = epsi_confusion_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w2_ds = epsi_coherence_w2_d/(sqrt(epsi_confusion_w2_d^2 + epsi_coherence_w2_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w3_ds = epsi_confusion_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w3_ds = epsi_coherence_w3_d/(sqrt(epsi_confusion_w3_d^2 + epsi_coherence_w3_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_confusion_w4_ds = epsi_confusion_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))
sim.identity <- sim.identity %>% 
  mutate(epsi_coherence_w4_ds = epsi_coherence_w4_d/(sqrt(epsi_confusion_w4_d^2 + epsi_coherence_w4_d^2)))


sim.identity <- sim.identity %>% 
  mutate(identity_dpp_12 = 
           (epsi_confusion_w1_ds - epsi_confusion_w2_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w2_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_23 = 
           (epsi_confusion_w2_ds - epsi_confusion_w3_ds)^2 +
           (epsi_coherence_w2_ds - epsi_coherence_w3_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_34 = 
           (epsi_confusion_w3_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w3_ds - epsi_coherence_w4_ds)^2)
sim.identity <- sim.identity %>% 
  mutate(identity_dpp_all = 
           (epsi_confusion_w1_ds - epsi_confusion_w4_ds)^2 +
           (epsi_coherence_w1_ds - epsi_coherence_w4_ds)^2)

# >> create 99.9% cutoff based on simulated data ----

cutoff_identity_d2_12 <- mean(sim.identity$identity_d2_12) + 3.291*(sd(sim.identity$identity_d2_12)/sqrt(50000))
cutoff_identity_d2_23 <- mean(sim.identity$identity_d2_23) + 3.291*(sd(sim.identity$identity_d2_23)/sqrt(50000))
cutoff_identity_d2_34 <- mean(sim.identity$identity_d2_34) + 3.291*(sd(sim.identity$identity_d2_34)/sqrt(50000))
cutoff_identity_d2_all <- mean(sim.identity$identity_d2_all) + 3.291*(sd(sim.identity$identity_d2_all)/sqrt(50000))
cutoff_identity_dp_12 <- mean(sim.identity$identity_dp_12) + 3.291*(sd(sim.identity$identity_dp_12)/sqrt(50000))
cutoff_identity_dp_23 <- mean(sim.identity$identity_dp_23) + 3.291*(sd(sim.identity$identity_dp_23)/sqrt(50000))
cutoff_identity_dp_34 <- mean(sim.identity$identity_dp_34) + 3.291*(sd(sim.identity$identity_dp_34)/sqrt(50000))
cutoff_identity_dp_all <- mean(sim.identity$identity_dp_all) + 3.291*(sd(sim.identity$identity_dp_all)/sqrt(50000))
cutoff_identity_dpp_12 <- mean(sim.identity$identity_dpp_12) + 3.291*(sd(sim.identity$identity_dpp_12)/sqrt(50000))
cutoff_identity_dpp_23 <- mean(sim.identity$identity_dpp_23) + 3.291*(sd(sim.identity$identity_dpp_23)/sqrt(50000))
cutoff_identity_dpp_34 <- mean(sim.identity$identity_dpp_34) + 3.291*(sd(sim.identity$identity_dpp_34)/sqrt(50000))
cutoff_identity_dpp_all <- mean(sim.identity$identity_dpp_all) + 3.291*(sd(sim.identity$identity_dpp_all)/sqrt(50000))

# >> ipsative identity ----
ipsative_identity(peerw)
# >> imputations ----
# >>> compute for each imputed dataset ----
imp_ips_id1 <- ipsative_identity(peerw_1)
imp_ips_id2 <- ipsative_identity(peerw_2)
imp_ips_id3 <- ipsative_identity(peerw_3)
imp_ips_id4 <- ipsative_identity(peerw_4)
imp_ips_id5 <- ipsative_identity(peerw_5)
imp_ips_id6 <- ipsative_identity(peerw_6)
imp_ips_id7 <- ipsative_identity(peerw_7)
imp_ips_id8 <- ipsative_identity(peerw_8)
imp_ips_id9 <- ipsative_identity(peerw_9)
imp_ips_id10 <- ipsative_identity(peerw_10)
imp_ips_id11 <- ipsative_identity(peerw_11)
imp_ips_id12 <- ipsative_identity(peerw_12)
imp_ips_id13 <- ipsative_identity(peerw_13)
imp_ips_id14 <- ipsative_identity(peerw_14)
imp_ips_id15 <- ipsative_identity(peerw_15)
imp_ips_id16 <- ipsative_identity(peerw_16)
imp_ips_id17 <- ipsative_identity(peerw_17)
imp_ips_id18 <- ipsative_identity(peerw_18)
imp_ips_id19 <- ipsative_identity(peerw_19)
imp_ips_id20 <- ipsative_identity(peerw_20)
imp_ips_id <- rbind(imp_ips_id1, imp_ips_id2, imp_ips_id3, imp_ips_id4,
                    imp_ips_id5, imp_ips_id6, imp_ips_id7, imp_ips_id8,
                    imp_ips_id9, imp_ips_id10, imp_ips_id11, imp_ips_id12,
                    imp_ips_id13, imp_ips_id14, imp_ips_id15, imp_ips_id16,
                    imp_ips_id17, imp_ips_id18, imp_ips_id19, imp_ips_id20)

# >>> aggregating ipsative change across imputations ----
imp_ips_id %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

# > PEER 5: Correlated change ----
# >> In peer only ----
slope.agree_p <- data.frame("id" = rownames(coef(linear.agree)$ID),
                          "slope.agree_p" = coef(linear.agree)$ID[,2])
slope.consci_p <- data.frame("id" = rownames(coef(linear.consci)$ID),
                          "slope.consci_p" = coef(linear.consci)$ID[,2])
slope.extra_p <- data.frame("id" = rownames(coef(linear.extra)$ID),
                           "slope.extra_p" = coef(linear.extra)$ID[,2])
slope.neuro_p <- data.frame("id" = rownames(coef(linear.neuro)$ID),
                          "slope.neuro_p" = coef(linear.neuro)$ID[,2])
slope.opend_p <- data.frame("id" = rownames(coef(linear.opend)$ID),
                          "slope.opend_p" = coef(linear.opend)$ID[,2])
slope.assert_p <- data.frame("id" = rownames(coef(linear.assert)$ID),
                           "slope.assert_p" = coef(linear.assert)$ID[,2])
slope.compa_p <- data.frame("id" = rownames(coef(linear.compa)$ID),
                           "slope.compa_p" = coef(linear.compa)$ID[,2])
slope.enthu_p <- data.frame("id" = rownames(coef(linear.enthu)$ID),
                          "slope.enthu_p" = coef(linear.enthu)$ID[,2])
slope.indus_p <- data.frame("id" = rownames(coef(linear.indus)$ID),
                          "slope.indus_p" = coef(linear.indus)$ID[,2])
slope.intel_p <- data.frame("id" = rownames(coef(linear.intel)$ID),
                          "slope.intel_p" = coef(linear.intel)$ID[,2])
slope.opena_p <- data.frame("id" = rownames(coef(linear.opena)$ID),
                          "slope.opena_p" = coef(linear.opena)$ID[,2])
slope.order_p <- data.frame("id" = rownames(coef(linear.order)$ID),
                          "slope.order_p" = coef(linear.order)$ID[,2])
slope.polit_p <- data.frame("id" = rownames(coef(linear.polit)$ID),
                          "slope.polit_p" = coef(linear.polit)$ID[,2])
slope.volat_p <- data.frame("id" = rownames(coef(linear.volat)$ID),
                          "slope.volat_p" = coef(linear.volat)$ID[,2])
slope.withd_p <- data.frame("id" = rownames(coef(linear.withd)$ID),
                          "slope.withd_p" = coef(linear.withd)$ID[,2])
slope.confu_p <- data.frame("id" = rownames(coef(linear.confu)$ID),
                          "slope.confu_p" = coef(linear.confu)$ID[,2])
slope.coher_p <- data.frame("id" = rownames(coef(linear.coher)$ID),
                          "slope.coher_p" = coef(linear.coher)$ID[,2])

slope_peer <- merge(slope.agree_p, slope.consci_p) %>% 
  merge(slope.extra_p) %>% 
  merge(slope.neuro_p) %>% 
  merge(slope.opend_p) %>% 
  merge(slope.assert_p) %>% 
  merge(slope.compa_p) %>% 
  merge(slope.enthu_p) %>% 
  merge(slope.indus_p) %>% 
  merge(slope.intel_p) %>% 
  merge(slope.opena_p) %>% 
  merge(slope.order_p) %>% 
  merge(slope.polit_p) %>% 
  merge(slope.volat_p) %>% 
  merge(slope.withd_p) %>% 
  merge(slope.confu_p) %>% 
  merge(slope.coher_p)

# >> Between self and peer ----

rm(slope.agree_p, slope.consci_p, slope.extra_p, slope.neuro_p, slope.opend_p, 
   slope.assert_p, slope.compa_p, slope.enthu_p, slope.indus_p, slope.intel_p, 
   slope.opena_p, slope.order_p, slope.polit_p, slope.volat_p, slope.withd_p, 
   slope.confu_p, slope.coher_p)

slope_total <- merge(slope, slope_peer)

# EXPORT AND GRAPHS PEER ==========================================
# > Mean-level change ----
# >> Linear models ----
sink("peer.linear.domain.txt")
print(summary(linear.agree))
print(summary(linear.consci))
print(summary(linear.extra))
print(summary(linear.neuro))
print(summary(linear.opend))
sink() 

sink("peer.linear.aspect.txt")
print(summary(linear.compa))
print(summary(linear.polit))
print(summary(linear.indus))
print(summary(linear.order))
print(summary(linear.assert))
print(summary(linear.enthu))
print(summary(linear.volat))
print(summary(linear.withd))
print(summary(linear.opena))
print(summary(linear.intel))
sink() 

sink("peer.linear.identity.txt")
print(summary(linear.confu))
print(summary(linear.coher))
sink() 

tab_model(linear.agree, linear.compa, linear.polit, file = "linear.agree.doc", digits = 3)
tab_model(linear.consci, linear.indus, linear.order, file = "linear.consci.doc", digits = 3)
tab_model(linear.extra, linear.assert, linear.enthu, file = "linear.extra.doc", digits = 3)
tab_model(linear.neuro, linear.volat, linear.withd, file = "linear.neuro.doc", digits = 3)
tab_model(linear.opend, linear.opena, linear.intel, file = "linear.opend.doc", digits = 3)
tab_model(linear.confu, linear.coher, file = "linear.epsi.doc", digits = 3)

jpeg("peer.linear.agree.jpg", width = 400, height = 500)
plot_model(linear.agree,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Agreeableness"))
dev.off()

jpeg("peer.linear.compa.jpg", width = 400, height = 500)
plot_model(linear.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Compassion"))
dev.off()

jpeg("peer.linear.polit.jpg", width = 400, height = 500)
plot_model(linear.polit,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Politeness"))
dev.off()

jpeg("peer.linear.consci.jpg", width = 400, height = 500)
plot_model(linear.consci,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Conscientiousness"))
dev.off()

jpeg("peer.linear.indus.jpg", width = 400, height = 500)
plot_model(linear.indus,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Industriousness"))
dev.off()

jpeg("peer.linear.order.jpg", width = 400, height = 500)
plot_model(linear.order,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Orderliness"))
dev.off()

jpeg("peer.linear.extra.jpg", width = 400, height = 500)
plot_model(linear.extra,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Extraversion"))
dev.off()

jpeg("peer.linear.assert.jpg", width = 400, height = 500)
plot_model(linear.assert,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Assertiveness"))
dev.off()

jpeg("peer.linear.enthu.jpg", width = 400, height = 500)
plot_model(linear.enthu,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Enthusiasm"))
dev.off()

jpeg("peer.linear.neuro.jpg", width = 400, height = 500)
plot_model(linear.neuro,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Neuroticism"))
dev.off()

jpeg("peer.linear.volat.jpg", width = 400, height = 500)
plot_model(linear.volat,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Volatility"))
dev.off()

jpeg("peer.linear.withd.jpg", width = 400, height = 500)
plot_model(linear.withd,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Withdrawal"))
dev.off()

jpeg("peer.linear.opena.jpg", width = 400, height = 500)
plot_model(linear.opena,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Openness Aspect"))
dev.off()

jpeg("peer.linear.opend.jpg", width = 400, height = 500)
plot_model(linear.opend,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Openness Domain"))
dev.off()

jpeg("peer.linear.intel.jpg", width = 400, height = 500)
plot_model(linear.intel,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Intellect"))
dev.off()

jpeg("peer.linear.confu.jpg", width = 400, height = 500)
plot_model(linear.confu,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4),
           title = "",
           axis.title = c("Time","Identity Confusion"))
dev.off()

jpeg("peer.linear.coher.jpg", width = 400, height = 500)
plot_model(linear.coher,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           title = "",
           axis.title = c("Time","Identity Coherence"))
dev.off()

# >> Quadratic models ----
sink("peer.quad.domain.txt")
print(summary(quad.agree))
print(summary(quad.consci))
print(summary(quad.extra))
print(summary(quad.neuro))
print(summary(quad.opend))
sink() 

sink("peer.quad.aspect.txt")
print(summary(quad.compa))
print(summary(quad.polit))
print(summary(quad.indus))
print(summary(quad.order))
print(summary(quad.assert))
print(summary(quad.enthu))
print(summary(quad.volat))
print(summary(quad.withd))
print(summary(quad.opena))
print(summary(quad.intel))
sink() 

sink("peer.quad.identity.txt")
print(summary(quad.confu))
print(summary(quad.coher))
sink() 

tab_model(quad.agree, quad.compa, quad.polit, file = "peer.quad.agree.doc")
tab_model(quad.consci, quad.indus, quad.order, file = "peer.quad.consci.doc")
tab_model(quad.extra, quad.assert, quad.enthu, file = "peer.quad.extra.doc")
tab_model(quad.neuro, quad.volat, quad.withd, file = "peer.quad.neuro.doc")
tab_model(quad.opend, quad.opena, quad.intel, file = "peer.quad.opend.doc")
tab_model(quad.confu, quad.coher, file = "peer.quad.epsi.doc")

#plots of significant quadratic variables
p1 <- plot_model(quad.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           axis.title = c("Time", "Compassion"),
           title = "")
p2 <- plot_model(quad.order,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5),
           axis.title = c("Time","Orderliness"),
           title = "")

jpeg("sig.quad.peer.jpg", width = 700, height = 500)
cowplot::plot_grid(p1,p2)
dev.off()


# >> Compare linear vs. quad ----
sink("peer.compare.domain.txt")
print(anova(linear.agree, quad.agree))
print(anova(linear.consci, quad.consci))
print(anova(linear.extra, quad.extra))
print(anova(linear.neuro, quad.neuro))
print(anova(linear.opend, quad.opend))
sink() 

sink("peer.compare.aspect.txt")
print(anova(linear.assert, quad.assert))
print(anova(linear.compa, quad.compa))
print(anova(linear.enthu, quad.enthu))
print(anova(linear.indus, quad.indus))
print(anova(linear.intel, quad.intel))
print(anova(linear.opena, quad.opena))
print(anova(linear.order, quad.order))
print(anova(linear.polit, quad.polit))
print(anova(linear.volat, quad.volat))
print(anova(linear.withd, quad.withd))
sink() 

sink("peer.compare.identity.txt")
print(anova(linear.confu, quad.confu))
print(anova(linear.coher, quad.coher))
sink() 


jpeg("peer.quad.agree.jpg", width = 400, height = 500)
plot_model(quad.agree,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.compa.jpg", width = 400, height = 500)
plot_model(quad.compa,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.polit.jpg", width = 400, height = 500)
plot_model(quad.polit,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.consci.jpg", width = 400, height = 500)
plot_model(quad.consci,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.indus.jpg", width = 400, height = 500)
plot_model(quad.indus,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.order.jpg", width = 400, height = 500)
plot_model(quad.order,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.extra.jpg", width = 400, height = 500)
plot_model(quad.extra,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.assert.jpg", width = 400, height = 500)
plot_model(quad.assert,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.enthu.jpg", width = 400, height = 500)
plot_model(quad.enthu,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.neuro.jpg", width = 400, height = 500)
plot_model(quad.neuro,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("peer.quad.volat.jpg", width = 400, height = 500)
plot_model(quad.volat,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("peer.quad.withd.jpg", width = 400, height = 500)
plot_model(quad.withd,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("peer.quad.opena.jpg", width = 400, height = 500)
plot_model(quad.opena,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.opend.jpg", width = 400, height = 500)
plot_model(quad.opend,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.intel.jpg", width = 400, height = 500)
plot_model(quad.intel,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

jpeg("peer.quad.confu.jpg", width = 400, height = 500)
plot_model(quad.confu,
           type = "pred",
           terms = "time",
           axis.lim = c(2,4))
dev.off()

jpeg("peer.quad.coher.jpg", width = 400, height = 500)
plot_model(quad.coher,
           type = "pred",
           terms = "time",
           axis.lim = c(3,5))
dev.off()

# >> MI linear ----
sink("MI.peer.txt")
print(testEstimates(linear.imp.agree, var.comp=TRUE))
print(testEstimates(linear.imp.assert, var.comp=TRUE))
print(testEstimates(linear.imp.compa, var.comp=TRUE))
print(testEstimates(linear.imp.consci, var.comp=TRUE))
print(testEstimates(linear.imp.enthu, var.comp=TRUE))
print(testEstimates(linear.imp.extra, var.comp=TRUE))
print(testEstimates(linear.imp.indus, var.comp=TRUE))
print(testEstimates(linear.imp.intel, var.comp=TRUE))
print(testEstimates(linear.imp.neuro, var.comp=TRUE))
print(testEstimates(linear.imp.opena, var.comp=TRUE))
print(testEstimates(linear.imp.opend, var.comp=TRUE))
print(testEstimates(linear.imp.order, var.comp=TRUE))
print(testEstimates(linear.imp.polit, var.comp=TRUE))
print(testEstimates(linear.imp.volat, var.comp=TRUE))
print(testEstimates(linear.imp.withd, var.comp=TRUE))
sink() 

sink("MI.peer.ID.txt")
print(testEstimates(linear.imp.confu, var.comp=TRUE))
print(testEstimates(linear.imp.coher, var.comp=TRUE))
sink()

# > Rank-order change ----
# >> Correlation between different waves ----
peerw %>% 
  dplyr::select(bfas_agreeableness_w1, bfas_agreeableness_w2, bfas_agreeableness_w3, bfas_agreeableness_w4) %>% 
  apa.cor.table(filename = 'peer.rankagree.doc')
peerw %>% 
  dplyr::select(bfas_conscientiousness_w1, bfas_conscientiousness_w2, bfas_conscientiousness_w3, bfas_conscientiousness_w4) %>% 
  apa.cor.table(filename = 'peer.rankconsci.doc')
peerw %>% 
  dplyr::select(bfas_extraversion_w1, bfas_extraversion_w2, bfas_extraversion_w3, bfas_extraversion_w4) %>% 
  apa.cor.table(filename = 'peer.rankextra.doc')
peerw %>% 
  dplyr::select(bfas_neuroticism_w1, bfas_neuroticism_w2, bfas_neuroticism_w3, bfas_neuroticism_w4) %>% 
  apa.cor.table(filename = 'peer.rankneuro.doc')
peerw %>% 
  dplyr::select(bfas_opennessdomain_w1, bfas_opennessdomain_w2, bfas_opennessdomain_w3, bfas_opennessdomain_w4) %>% 
  apa.cor.table(filename = 'peer.rankopend.doc')
peerw %>% 
  dplyr::select(bfas_assertiveness_w1, bfas_assertiveness_w2, bfas_assertiveness_w3, bfas_assertiveness_w4) %>% 
  apa.cor.table(filename = 'peer.rankassert.doc')
peerw %>% 
  dplyr::select(bfas_compassion_w1, bfas_compassion_w2, bfas_compassion_w3, bfas_compassion_w4) %>% 
  apa.cor.table(filename = 'peer.rankcompa.doc')
peerw %>% 
  dplyr::select(bfas_enthusiasm_w1, bfas_enthusiasm_w2, bfas_enthusiasm_w3, bfas_enthusiasm_w4) %>% 
  apa.cor.table(filename = 'peer.rankenthu.doc')
peerw %>% 
  dplyr::select(bfas_industriousness_w1, bfas_industriousness_w2, bfas_industriousness_w3, bfas_industriousness_w4) %>% 
  apa.cor.table(filename = 'peer.rankindus.doc')
peerw %>% 
  dplyr::select(bfas_intellect_w1, bfas_intellect_w2, bfas_intellect_w3, bfas_intellect_w4) %>% 
  apa.cor.table(filename = 'peer.rankintel.doc')
peerw %>% 
  dplyr::select(bfas_opennessaspect_w1, bfas_opennessaspect_w2, bfas_opennessaspect_w3, bfas_opennessaspect_w4) %>% 
  apa.cor.table(filename = 'peer.rankopena.doc')
peerw %>% 
  dplyr::select(bfas_orderliness_w1, bfas_orderliness_w2, bfas_orderliness_w3, bfas_orderliness_w4) %>% 
  apa.cor.table(filename = 'peer.rankorder.doc')
peerw %>% 
  dplyr::select(bfas_politeness_w1, bfas_politeness_w2, bfas_politeness_w3, bfas_politeness_w4) %>% 
  apa.cor.table(filename = 'peer.rankpolit.doc')
peerw %>% 
  dplyr::select(bfas_volatility_w1, bfas_volatility_w2, bfas_volatility_w3, bfas_volatility_w4) %>% 
  apa.cor.table(filename = 'peer.rankvolat.doc')
peerw %>% 
  dplyr::select(bfas_withdrawal_w1, bfas_withdrawal_w2, bfas_withdrawal_w3, bfas_withdrawal_w4) %>% 
  apa.cor.table(filename = 'peer.rankwithd.doc')
peerw %>% 
  dplyr::select(epsi_confusion_w1, epsi_confusion_w2, epsi_confusion_w3, epsi_confusion_w4) %>% 
  apa.cor.table(filename = 'peer.rankconfu.doc')
peerw %>% 
  dplyr::select(epsi_coherence_w1, epsi_coherence_w2, epsi_coherence_w3, epsi_coherence_w4) %>% 
  apa.cor.table(filename = 'peer.rankcoher.doc')

# >> Rank-order over time ----
rank_domain <- data.frame(
  "T1-T2.vs.T2-T3" = c(agree_12_23, consci_12_23, extra_12_23, neuro_12_23, opend_12_23),
  "p-value" = c(p_agree_12_23, p_consci_12_23, p_extra_12_23, p_neuro_12_23, p_opend_12_23),
  "T2-T3.vs.T3-T4" = c(agree_23_34, consci_23_34, extra_23_34, neuro_23_34, opend_23_34),
  "p-value" = c(p_agree_23_34, p_consci_23_34, p_extra_23_34, p_neuro_23_34, p_opend_23_34),
  "T1-T2.vs.T3-T4" = c(agree_12_34, consci_12_34, extra_12_34, neuro_12_34, opend_12_34),
  "p-value" = c(p_agree_12_34, p_consci_12_34, p_extra_12_34, p_neuro_12_34, p_opend_12_34))

rank_aspect <- data.frame(
  "T1-T2.vs.T2-T3" = c(assert_12_23, compa_12_23, enthu_12_23, indus_12_23, intel_12_23,
                       opena_12_23, order_12_23, polit_12_23, volat_12_23, withd_12_23),
  "p-value" = c(p_assert_12_23, p_compa_12_23, p_enthu_12_23, p_indus_12_23, p_intel_12_23,
                p_opena_12_23, p_order_12_23, p_polit_12_23, p_volat_12_23, p_withd_12_23),
  "T2-T3.vs.T3-T4" = c(assert_23_34, compa_23_34, enthu_23_34, indus_23_34, intel_23_34,
                     opena_23_34, order_23_34, polit_23_34, volat_23_34, withd_23_34),
  "p-value" = c(p_assert_23_34, p_compa_23_34, p_enthu_23_34, p_indus_23_34, p_intel_23_34,
                p_opena_23_34, p_order_23_34, p_polit_23_34, p_volat_23_34, p_withd_23_34),
  "T1-T2.vs.T3-T4" = c(assert_12_34, compa_12_34, enthu_12_34, indus_12_34, intel_12_34,
                       opena_12_34, order_12_34, polit_12_34, volat_12_34, withd_12_34),
  "p-value" = c(p_assert_12_34, p_compa_12_34, p_enthu_12_34, p_indus_12_34, p_intel_12_34,
                p_opena_12_34, p_order_12_34, p_polit_12_34, p_volat_12_34, p_withd_12_34))

rank_identity <- data.frame(
  "T1-T2.vs.T2-T3" = c(coher_12_23, confu_12_23),
  "p-value" = c(p_coher_12_23, p_confu_12_23),
  "T2-T3.vs.T3-T4" = c(coher_23_34, confu_23_34),
  "p-value" = c(p_coher_23_34, p_confu_23_34),
  "T1-T2.vs.T3-T4" = c(coher_12_34, confu_12_34),
  "p-value" = c(p_coher_12_34, p_confu_12_34))

write.csv(rank_domain, "rank.domain.csv")
write.csv(rank_aspect, "rank.aspect.csv")
write.csv(rank_identity, "rank.identity.csv")

# >> Temporal decay curve ----
rank <- c(r12_agree, r12_assert, r12_compa, r12_consci, r12_enthu,
          r12_extra, r12_indus, r12_intel, r12_neuro, r12_opena,
          r12_opend, r12_order, r12_polit, r12_volat, r12_withd,
          r23_agree, r23_assert, r23_compa, r23_consci, r23_enthu,
          r23_extra, r23_indus, r23_intel, r23_neuro, r23_opena,
          r23_opend, r23_order, r23_polit, r23_volat, r23_withd,
          r34_agree, r34_assert, r34_compa, r34_consci, r34_enthu,
          r34_extra, r34_indus, r34_intel, r34_neuro, r34_opena,
          r34_opend, r34_order, r34_polit, r34_volat, r34_withd,
          r14_agree, r14_assert, r14_compa, r14_consci, r14_enthu,
          r14_extra, r14_indus, r14_intel, r14_neuro, r14_opena,
          r14_opend, r14_order, r14_polit, r14_volat, r14_withd,
          r13_agree, r13_assert, r13_compa, r13_consci, r13_enthu,
          r13_extra, r13_indus, r13_intel, r13_neuro, r13_opena,
          r13_opend, r13_order, r13_polit, r13_volat, r13_withd,
          r24_agree, r24_assert, r24_compa, r24_consci, r24_enthu,
          r24_extra, r24_indus, r24_intel, r24_neuro, r24_opena,
          r24_opend, r24_order, r24_polit, r24_volat, r24_withd)
time <- c("r12", "r12", "r12", "r12", "r12",
          "r12", "r12", "r12", "r12", "r12",
          "r12", "r12", "r12", "r12", "r12",
          "r23", "r23", "r23", "r23", "r23",
          "r23", "r23", "r23", "r23", "r23",
          "r23", "r23", "r23", "r23", "r23",
          "r34", "r34", "r34", "r34", "r34",
          "r34", "r34", "r34", "r34", "r34",
          "r34", "r34", "r34", "r34", "r34",
          "r14", "r14", "r14", "r14", "r14",
          "r14", "r14", "r14", "r14", "r14",
          "r14", "r14", "r14", "r14", "r14",
          "r13", "r13", "r13", "r13", "r13",
          "r13", "r13", "r13", "r13", "r13",
          "r13", "r13", "r13", "r13", "r13",
          "r24", "r24", "r24", "r24", "r24",
          "r24", "r24", "r24", "r24", "r24",
          "r24", "r24", "r24", "r24", "r24")
trait <- rep(c("agree", "assert", "compa", "consci", "enthu",
           "extra", "indus", "intel", "neuro", "opena",
           "opend", "order", "polit", "volat", "withd"), times = 6)
time1 <- as.factor(rep(c(1,2,3,1,1,2), each = 15))
time2 <- as.factor(rep(c(2,3,4,4,3,4), each = 15))
rank <- data.frame(time, trait, rank, time1, time2)

ggplot(data = rank[which(rank$trait == "agree"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Agreeableness")
ggsave("peer.agree.png")

ggplot(data = rank[which(rank$trait == "consci"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Conscientiousess")
ggsave("peer.consci.png")

ggplot(data = rank[which(rank$trait == "extra"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Extraversion")
ggsave("peer.extra.png")

ggplot(data = rank[which(rank$trait == "neuro"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Neuroticism")
ggsave("peer.neuro.png")

ggplot(data = rank[which(rank$trait == "opend"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Openness Domain")
ggsave("peer.opend.png")

ggplot(data = rank[which(rank$trait == "assert"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Assertiveness")
ggsave("peer.assert.png")

ggplot(data = rank[which(rank$trait == "compa"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Compassion")
ggsave("peer.compa.png")

ggplot(data = rank[which(rank$trait == "enthu"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Enthusiasm")
ggsave("peer.enthu.png")

ggplot(data = rank[which(rank$trait == "indus"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Industriousness")
ggsave("peer.indus.png")

ggplot(data = rank[which(rank$trait == "intel"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Intellect")
ggsave("peer.intel.png")

ggplot(data = rank[which(rank$trait == "opena"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Openness Aspect")
ggsave("peer.opena.png")

ggplot(data = rank[which(rank$trait == "order"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Orderliness")
ggsave("peer.order.png")

ggplot(data = rank[which(rank$trait == "polit"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Politeness")
ggsave("peer.polit.png")

ggplot(data = rank[which(rank$trait == "volat"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Volatility")
ggsave("peer.volat.png")

ggplot(data = rank[which(rank$trait == "withd"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")+
  ylim(0.4,1) +
  ylab("Correlation")+
  xlab("Time Point")+
  theme_classic() +
  ggtitle("Temporal Decay Curve - Withdrawal")
ggsave("peer.withd.png")

#plot significant rank-order increase with grid
g1 <- ggplot(data = rank[which(rank$trait == "agree"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Agreeableness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
  
g2 <- ggplot(data = rank[which(rank$trait == "opend"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Openness Domain")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
  
g3 <- ggplot(data = rank[which(rank$trait == "compa"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Compassion")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
  
g4 <- ggplot(data = rank[which(rank$trait == "indus"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Industriousness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
  
  
g5 <- ggplot(data = rank[which(rank$trait == "opena"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Openess Aspect")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
  
g6 <- ggplot(data = rank[which(rank$trait == "polit"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Politeness")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")

cowplot::plot_grid(plot_grid(g1,g2,g3,g4,g5,g6, nrow = 2), legend, rel_widths = c(3,.4))

rank <- c(r12_confu, r12_coher, r23_confu, r23_coher, r34_confu, r34_coher,
          r14_confu, r14_coher, r13_confu, r13_coher, r24_confu, r24_coher)
time <- c("r12", "r12","r23","r23","r34", "r34",
          "r14", "r14","r13","r13","r24", "r24")
trait <- rep(c("confu", "coher"), times = 6)
time1 <- as.factor(rep(c(1,2,3,1,1,2), each = 2))
time2 <- as.factor(rep(c(2,3,4,4,3,4), each = 2))
rank <- data.frame(time, trait, rank, time1, time2)

g1 <- ggplot(data = rank[which(rank$trait == "confu"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point() +
  ylim(0.4,1) +
  ylab("Identity Confusion")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
ggsave("peer.confu.png")

g2 <- ggplot(data = rank[which(rank$trait == "coher"),],
       aes(y = rank, x = time2, group = time1, color = time1)) +
  geom_line()+
  geom_point()+
  ylim(0.4,1) +
  ylab("Identity Coherence")+
  xlab("Time Point")+
  theme_classic()+
  theme(legend.position="none")
ggsave("peer.conher.png")

cowplot::plot_grid(plot_grid(g1,g2, nrow = 1), legend, rel_widths = c(3,.4))


# >> Forest plot ----
# >>> domains
r12 <- rbind(r12_agree, 
             r12_consci,
             r12_extra, 
             r12_neuro, 
             r12_opend) 
r34 <- rbind(r34_agree, 
             r34_consci,
             r34_extra, 
             r34_neuro, 
             r34_opend)

rcon12 <- cbind(rcon12agree, 
                rcon12consci,
                rcon12extra, 
                rcon12neuro, 
                rcon12opend)
rcon34 <- cbind(rcon34agree, 
                rcon34consci,
                rcon34extra, 
                rcon34neuro, 
                rcon34opend) 

tabletext <- cbind(
  c("Agreeableness", "Conscientiousness", "Extraversion",
    "Neuroticism", "Openness Domain"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in personality domains - Peer reports",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))
           

# >>> aspects
r12 <- rbind(r12_compa, r12_polit,
             r12_indus, r12_order,
             r12_assert, r12_enthu,
             r12_volat, r12_withd,
             r12_intel, r12_opena)
r34 <- rbind(r34_compa, r34_polit,
             r34_indus, r34_order,
             r34_assert, r34_enthu,
             r34_volat, r34_withd,
             r34_intel, r34_opena)


rcon12 <- cbind(rcon12compa, rcon12polit,
                rcon12indus, rcon12order,
                rcon12assert, rcon12enthu,
                rcon12volat, rcon12withd,
                rcon12intel, rcon12opena)

rcon34 <- cbind(rcon34compa, rcon34polit,
                rcon34indus, rcon34order,
                rcon34assert, rcon34enthu,
                rcon34volat, rcon34withd,
                rcon34intel, rcon34opena)

tabletext <- cbind(
  c("Agreeableness","","Conscientiousness","","Extraversion","", "Neuroticism",""),
  c("Compassion", "Politeness", "Industriousness", "Orderliness", 
    "Assertiveness","Enthusiasm","Volatility","Withdrawal","Intellect","Openness Aspect"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in personality aspects - Peer reports",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))

# >>> identity
r12 <- rbind(r12_coher, r12_confu)
r34 <- rbind(r34_coher, r34_confu)


rcon12 <- cbind(rcon12coher, rcon12confu)
rcon34 <- cbind(rcon34coher, rcon34confu)

tabletext <- cbind(
  c("Identity Coherence", "Identity Confusion"))


forestplot(tabletext, 
           mean = cbind(r12, r34),
           lower = cbind(rcon12[1,], rcon34[1,]),
           upper = cbind(rcon12[2,], rcon34[2,]),
           clip = c(0.5, 1.0),
           zero = mean(r12),
           lty.ci = c(1, 2),
           legend = c("Time 1-2", "Time 3-4"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           col=fpColors(box=c("blue", "darkred")),
           xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
           xlab = "Between-wave correlation in identity variables - Peer reports",
           txt_gp = fpTxtGp(ticks = gpar(fontfamily = "", cex = 0.75),
                            xlab = gpar(fontfamily = "", cex = 1)))

# >> MI rank-order ----
rank_domain_imp_p <- data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_agree_12_23$value), mean(imp_consci_12_23$value), mean(imp_extra_12_23$value), 
                       mean(imp_neuro_12_23$value), mean(imp_opend_12_23$value)),
  "p-value" = c(mean(imp_agree_12_23$p), mean(imp_consci_12_23$p), mean(imp_extra_12_23$p),
                mean(imp_neuro_12_23$p), mean(imp_opend_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_agree_23_34$value), mean(imp_consci_23_34$value), mean(imp_extra_23_34$value), 
                       mean(imp_neuro_23_34$value), mean(imp_opend_23_34$value)),
  "p-value" = c(mean(imp_agree_23_34$p), mean(imp_consci_23_34$p), mean(imp_extra_23_34$p),
                mean(imp_neuro_23_34$p), mean(imp_opend_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_agree_12_34$value), mean(imp_consci_12_34$value), mean(imp_extra_12_34$value), 
                       mean(imp_neuro_12_34$value), mean(imp_opend_12_34$value)),
  "p-value" = c(mean(imp_agree_12_34$p), mean(imp_consci_12_34$p), mean(imp_extra_12_34$p),
                mean(imp_neuro_12_34$p), mean(imp_opend_12_34$p)))


rank_aspect_imp_p <-  data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_assert_12_23$value), mean(imp_compa_12_23$value), 
                       mean(imp_enthu_12_23$value), mean(imp_indus_12_23$value), 
                       mean(imp_intel_12_23$value), mean(imp_opena_12_23$value),
                       mean(imp_order_12_23$value), mean(imp_polit_12_23$value), 
                       mean(imp_volat_12_23$value),mean(imp_withd_12_23$value)),
  "p-value" = c(mean(imp_assert_12_23$p), mean(imp_compa_12_23$p),
                mean(imp_enthu_12_23$p), mean(imp_indus_12_23$p), 
                mean(imp_intel_12_23$p), mean(imp_opena_12_23$p),
                mean(imp_order_12_23$p), mean(imp_polit_12_23$p), 
                mean(imp_volat_12_23$p),mean(imp_withd_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_assert_23_34$value), mean(imp_compa_23_34$value), 
                       mean(imp_enthu_23_34$value), mean(imp_indus_23_34$value), 
                       mean(imp_intel_23_34$value), mean(imp_opena_23_34$value),
                       mean(imp_order_23_34$value), mean(imp_polit_23_34$value), 
                       mean(imp_volat_23_34$value),mean(imp_withd_23_34$value)),
  "p-value" = c(mean(imp_assert_23_34$p), mean(imp_compa_23_34$p),
                mean(imp_enthu_23_34$p), mean(imp_indus_23_34$p), 
                mean(imp_intel_23_34$p), mean(imp_opena_23_34$p),
                mean(imp_order_23_34$p), mean(imp_polit_23_34$p), 
                mean(imp_volat_23_34$p),mean(imp_withd_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_assert_12_34$value), mean(imp_compa_12_34$value),
                       mean(imp_enthu_12_34$value), mean(imp_indus_12_34$value), 
                       mean(imp_intel_12_34$value), mean(imp_opena_12_34$value),
                       mean(imp_order_12_34$value), mean(imp_polit_12_34$value), 
                       mean(imp_volat_12_34$value),mean(imp_withd_12_34$value)),
  "p-value" = c(mean(imp_assert_12_34$p), mean(imp_compa_12_34$p),
                mean(imp_enthu_12_34$p), mean(imp_indus_12_34$p), 
                mean(imp_intel_12_34$p), mean(imp_opena_12_34$p),
                mean(imp_order_12_34$p), mean(imp_polit_12_34$p), 
                mean(imp_volat_12_34$p),mean(imp_withd_12_34$p)))

rank_identity_imp_p <- data.frame(
  "T1-T2.vs.T2-T3" = c(mean(imp_coher_12_23$value), mean(imp_confu_12_23$value)),
  "p-value" = c(mean(imp_coher_12_23$p), mean(imp_confu_12_23$p)),
  "T2-T3.vs.T3-T4" = c(mean(imp_coher_23_34$value), mean(imp_confu_23_34$value)),
  "p-value" = c(mean(imp_coher_23_34$p), mean(imp_confu_23_34$p)),
  "T1-T2.vs.T3-T4" = c(mean(imp_coher_12_34$value), mean(imp_confu_12_34$value)),
  "p-value" = c(mean(imp_coher_12_34$p), mean(imp_confu_12_34$p)))

write.csv(rank_domain_imp_p, "rank_domain_imp_p.csv")
write.csv(rank_aspect_imp_p, "rank_aspect_imp_p.csv")
write.csv(rank_identity_imp_p, "rank_identity_imp_p.csv")

# > Individual differences ----

sink("peer.indi.domain.txt")
print(anova(linearint.agree, linear.agree))
print(anova(linearint.consci, linear.consci))
print(anova(linearint.extra, linear.extra))
print(anova(linearint.neuro, linear.neuro))
print(anova(linearint.opend, linear.opend))
sink() 

sink("peer.indi.aspect.txt")
print(anova(linearint.assert, linear.assert))
print(anova(linearint.compa, linear.compa))
print(anova(linearint.enthu, linear.enthu))
print(anova(linearint.indus, linear.indus))
print(anova(linearint.intel, linear.intel))
print(anova(linearint.opena, linear.opena))
print(anova(linearint.order, linear.order))
print(anova(linearint.polit, linear.polit))
print(anova(linearint.volat, linear.volat))
print(anova(linearint.withd, linear.withd))
sink() 

sink("peer.indi.identity.txt")
print(anova(linearint.coher, linear.coher))
print(anova(linearint.confu, linear.confu))
sink() 

ggplot(data = peerl,
       aes(x = time, y = bfas_agreeableness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.agree.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_assertiveness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.assert.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_compassion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.compa.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_conscientiousness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.consci.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_enthusiasm, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.enthu.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_extraversion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.extra.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_industriousness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.indus.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_intellect, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.intel.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_neuroticism, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.neuro.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_opennessaspect, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.opena.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_opennessdomain, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.opend.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_orderliness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.order.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_politeness, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.polit.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_volatility, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.volat.png")

ggplot(data = peerl,
       aes(x = time, y = bfas_withdrawal, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.withd.png")

ggplot(data = peerl,
       aes(x = time, y = epsi_coherence, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.coher.png")

ggplot(data = peerl,
       aes(x = time, y = epsi_confusion, group = ID))+
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 0.5) + 
  stat_smooth(aes(group = 1), method = "lm", 
              formula = y ~ x * I(x > 1), se = TRUE) + 
  stat_summary(aes(group = 1), fun.y = mean, geom = "point", size = 3) + 
  theme(legend.position="none")
ggsave("peer.linear.confu.png")

var <- rep(c("Domain","Aspect","Identity"), each = 12)
type <- rep(rep(c("D2", "D2p","D2pp"), each = 4), times = 3)
time <- rep(c("Time 1 to 2", "Time 2 to 3", "Time 3 to 4", "Overall"), times = 9)
percent <- c(46.2, 36.8, 44.0, 54.2, 44.1, 48.5, 40.0, 50.8,
             32.3, 29.4, 24.0, 39.0, #domains
             39.8, 38.2, 44.0, 49.2, 43.0, 39.7, 36.0, 50.8, 
             36.6, 33.8, 32.0, 47.5, #aspects
             33.3, 22.7, 32.8, 48.1, 30.2, 33.3, 25.9, 33.3, 
             6.7, 2.8, 3.5, 9.4) #identity
      
data <- data.frame(var, type, time, percent)
write.csv(data, "profile.csv")

d2 <- data %>% 
  dplyr::filter(type == "D2")

ggplot(d2, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Raw scores: Elevation, Scatter, Shape")
ggsave("raw.png")

dp <- data %>% 
  dplyr::filter(type == "D2p")


ggplot(dp, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Deviation scores: Scatter and Shape")
ggsave("deviation.png")

dpp <- data %>% 
  dplyr::filter(type == "D2pp")


ggplot(dpp, aes(fill=time, y=percent, x=var)) + 
    geom_bar(position="dodge", stat="identity") +
  ylim(0,80) +
  theme_classic() +
  ggtitle("Standardized scores: Shape")
ggsave("stadard.png")
# > Ipsative change ----
profile.peer <- rbind(ipsative_personality(peerw),
                      ipsative_identity(peerw))
write.csv(profile.peer, "profile.peer.csv")

#>> MI ipsative ----
imp_ips_pers <-imp_ips_pers %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

write.csv(imp_ips_pers, "MI ips personality peer.csv")

imp_ips_id <- imp_ips_id %>% group_by(name) %>% 
  summarise(mean=mean(value)) %>% 
  as.data.frame()

write.csv(imp_ips_id, "MI ips identity peer.csv")

# > Correlated change ----
slope_peer %>% 
  dplyr::select(-id) %>% 
  apa.cor.table(filename = 'corchange.doc')

slope_total %>% 
  dplyr::select(-id) %>% 
  apa.cor.table(filename = 'corchange.total.doc')

# >> bivariate correlations between self and peer ----
selfwave1 <- selfl %>% filter(time == "0")
peerwave1 <- peerl %>% filter(time == "0")
names <- names(peerwave1)[4:20]
names(peerwave1)[4:20] <- paste(names, "_p")

selfwave2 <- selfl %>% filter(time == "6")
peerwave2 <- peerl %>% filter(time == "6")
names <- names(peerwave2)[4:20]
names(peerwave2)[4:20] <- paste(names, "_p")

selfwave3 <- selfl %>% filter(time == "13")
peerwave3 <- peerl %>% filter(time == "13")
names <- names(peerwave3)[4:20]
names(peerwave3)[4:20] <- paste(names, "_p")

selfwave4 <- selfl %>% filter(time == "19")
peerwave4 <- peerl %>% filter(time == "19")
names <- names(peerwave4)[4:20]
names(peerwave4)[4:20] <- paste(names, "_p")

rm(names)

wave1 <- merge(selfwave1,peerwave1)
wave1 %>% dplyr::select(-ID, -PeerID, -time) %>% 
  apa.cor.table(file = "wave1cor.doc")

wave2 <- merge(selfwave2,peerwave2)
wave2 %>% dplyr::select(-ID, -PeerID, -time) %>% 
  apa.cor.table(file = "wave2cor.doc")

wave3 <- merge(selfwave3,peerwave3)
wave3 %>% dplyr::select(-ID, -PeerID, -time) %>% 
  apa.cor.table(file = "wave3cor.doc")

wave4 <- merge(selfwave4,peerwave4)
wave4 %>% dplyr::select(-ID, -PeerID, -time) %>% 
  apa.cor.table(file = "wave4cor.doc")

rm(wave1, wave2, wave3, wave4, selfwave1, peerwave1, selfwave2,peerwave2,
   selfwave3,peerwave3, selfwave4,peerwave4)