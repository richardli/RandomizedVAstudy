##
## Physician versus Computer Coding of Verbal Autopsies, 
##    a Randomised Control Trial
##
## @author Richard Li
## @date Feb 9, 2016


##                                                               ##
## ------------------------Task 1 - 4 -------------------------- ##
##                                                               ##

## read data (.c4, .c17)
## - raw.india: PHMRC india
## - raw.all: PHMRC all
## - raw.esl: test data
raw.india.c4 <- read.csv("../FilesSent_9Feb2016/PHMRC_India_12-69_4CODCategories.csv")
raw.india.c17 <- read.csv("../FilesSent_9Feb2016/PHMRC_India_12-69_17CODCategories.csv")
raw.all.c4 <- read.csv("../FilesSent_9Feb2016/PHMRC_AllSites_12-69_4CODCategories.csv")
raw.all.c17 <- read.csv("../FilesSent_9Feb2016/PHMRC_AllSites_12-69_17CODCategories.csv")
raw.esl <- read.csv("../FilesSent_9Feb2016/ESL_Amravati_12-69.csv")

## function to process data into ("Y", "", ".")
## 1st col: ID; 2nd col: cause
formatData <- function(dat){
  id <- dat[, 1]
  cause <- dat[, 2]
  dat <- dat[, -c(1, 2)]
  new <- matrix("", dim(dat)[1], dim(dat)[2])
  for(i in 1:dim(dat)[2]){
    new[which(dat[, i] == 1), i] <- "Y"
    new[which(dat[, i] == 99), i] <- "."
  } 
  new <- data.frame(new)
  new <- cbind(id, cause, new)
  colnames(new) <- c("ID", "gs", colnames(dat))
  return(new)
}

data.india.c4 <- formatData(raw.india.c4)
data.india.c17 <- formatData(raw.india.c17)
data.all.c4 <- formatData(raw.all.c4)
data.all.c17 <- formatData(raw.all.c17)
data.esl <- formatData(raw.esl)

fit <- NULL
out <- NULL
method_name <- "UWTariff"
## ------------  Tariff Experiment 1  ---------- ##
experiment <- 1
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.india.c4, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 2  ---------- ##
experiment <- 2
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.india.c17, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 3  ---------- ##
experiment <- 3
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.all.c4, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 4  ---------- ##
experiment <- 4
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.all.c17, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)



save.image("../FilesSent_9Feb2016/results/Feb9_tariff.rdata")



##                                                               ##
## ------------------------Task 5 - 6 -------------------------- ##
##    

raw.80.c4 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD4Cat_P1_P2_FinalICD_train80Per.csv")
raw.80.c17 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD17Cat_P1_P2_FinalICD_train80Per.csv")
raw.20.c4 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD4Cat_P1_P2_FinalICD_test20Per.csv")
raw.20.c17 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD17Cat_P1_P2_FinalICD_test20Per.csv")

## function to process data into ("Y", "", ".")
## 1st col: ID; 2nd col: cause
formatData2 <- function(dat){
  id <- dat[, 1]
  cause <- dat[, 2]
  dat <- dat[, -c(1, 2, 3, 4)]
  new <- matrix("", dim(dat)[1], dim(dat)[2])
  for(i in 1:dim(dat)[2]){
    new[which(dat[, i] == 1), i] <- "Y"
    new[which(dat[, i] == 99), i] <- "."
  } 
  new <- data.frame(new)
  new <- cbind(id, cause, new)
  colnames(new) <- c("ID", "gs", colnames(dat))
  return(new)
}

data.80.c4 <- formatData2(raw.80.c4)
data.80.c17 <- formatData2(raw.80.c17)
data.20.c4 <- formatData2(raw.20.c4)
data.20.c17 <- formatData2(raw.20.c17)



load("../FilesSent_9Feb2016/results/Feb9_tariff.rdata")
# install.packages("~/Dropbox/VA-related/VA-codebase/VerbalAutopsy-git/Tariff_1.0.tar.gz", repos = NULL, type = "source")
library(Tariff)

## ------------  Tariff Experiment 5  ---------- ##
experiment <- 5
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.80.c4, 
                      symps.test = data.20.c4)
summary(fit[[experiment]])
out[[experiment]]<-data.frame(deathId=fit[[experiment]]$causes.test[, 1],
                              COD = fit[[experiment]]$causes.test[, 2],
                              stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_18Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 6  ---------- ##
experiment <- 6
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.80.c17, 
                      symps.test = data.20.c17)
summary(fit[[experiment]])
out[[experiment]]<-data.frame(deathId=fit[[experiment]]$causes.test[, 1],
                              COD = fit[[experiment]]$causes.test[, 2],
                              stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_18Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)
save.image("../FilesSent_18Feb2016/results/Feb18_tariff.rdata")



##                                                               ##
## ------------------------Task 7 - 10 -------------------------- ##
##                                                               ##

## read data (.c4, .c17)
## - raw.india: PHMRC india
## - raw.all: PHMRC all
## - raw.esl: test data
raw.india.c4 <- read.csv("../FilesSent_29Feb2016/PHMRC_India_12andOver_4CODCategories.csv")
raw.india.c17 <- read.csv("../FilesSent_29Feb2016/PHMRC_India_12andOver_17CODCategories.csv")
raw.all.c4 <- read.csv("../FilesSent_29Feb2016/PHMRC_AllSites_12andOver_4CODCategories.csv")
raw.all.c17 <- read.csv("../FilesSent_29Feb2016/PHMRC_AllSites_12andOver_17CODCategories.csv")
raw.esl <- read.csv("../FilesSent_29Feb2016/ESL_Amravati_12andOver.csv")

## function to process data into ("Y", "", ".")
## 1st col: ID; 2nd col: cause
formatData <- function(dat){
  id <- dat[, 1]
  cause <- dat[, 2]
  dat <- dat[, -c(1, 2)]
  new <- matrix("", dim(dat)[1], dim(dat)[2])
  for(i in 1:dim(dat)[2]){
    new[which(dat[, i] == 1), i] <- "Y"
    new[which(dat[, i] == 99), i] <- "."
  } 
  new <- data.frame(new)
  new <- cbind(id, cause, new)
  colnames(new) <- c("ID", "gs", colnames(dat))
  return(new)
}

data.india.c4 <- formatData(raw.india.c4)
data.india.c17 <- formatData(raw.india.c17)
data.all.c4 <- formatData(raw.all.c4)
data.all.c17 <- formatData(raw.all.c17)
data.esl <- formatData(raw.esl)

fit <- NULL
out <- NULL
method_name <- "UWTariff"
## ------------  Tariff Experiment 7  ---------- ##
experiment <- 7
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.india.c4, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 8  ---------- ##
experiment <- 8
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.india.c17, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 9  ---------- ##
experiment <- 9
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.all.c4, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  Tariff Experiment 10  ---------- ##
experiment <- 10
fit[[experiment]] <- tariff(causes.train = "gs", 
                      symps.train = data.all.c17, 
                      symps.test = data.esl)
pick <- fit[[experiment]]$causes.test[, 2]
out[[experiment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)



save.image("../FilesSent_29Feb2016/results/Feb29_tariff.rdata")

