# library(MplusAutomation)
# library(jmRtools)
# library(readr)
# library(tidyverse)
#
# d <- read_csv("~/Google Drive/SCHMIDTLAB/PSE/data/iMUScLE/iMUScLE-esm.csv")
#
# d$cognitive_engagement<-composite_mean_maker(d, IMPTYOU, IMPTGOAL)
# d$affective_engagement<-composite_mean_maker(d, ENJOY, INTEREST)
# d$behavioral_engagement<-composite_mean_maker(d, CONCEN, WRKHARD)
#
# dd <- select(d, contains("engagement"))
#
# dd <- dd[complete.cases(dd), ]
#
# dd <- sample_frac(dd, .5)
#
# x <- Rmixmod::mixmodCluster(dd, nbCluster = 3)
# x
# x <- Rmixmod::mixmodCluster(dd, nbCluster = 4, strategy = new("Strategy", initMethod = "random",
#                                                               nbTryInInit = 300))
# # x
#
# dd <- as.matrix(dd)
# dd <- as.data.frame(dd)
# dd <- dd[complete.cases(dd), ]
#
# prepareMplusData(dd, "test.dat")
#
# # Model 1
# runModels(target = "/Users/joshuarosenberg/Dropbox/1_Research/tidymixmod/")
#
# m1 <- readModels("imuscle-test.out")
#
# m1
# hist(dd$cognitive_engagement)
