##
## Physician versus Computer Coding of Verbal Autopsies, 
##    a Randomised Control Trial
##
## @author Richard Li
## @date Feb 9, 2016


## install recent version of packages
install.packages("~/Bitbucket-repos/InSilicoVA-beta/InSilicoVA_1.1.tar.gz", repos = NULL, type = "source")
library(InSilicoVA)

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

which <- 1
type <- c("fixed", "quantile")[which]
strength <- c(1, 0.01)[which]
method_name <- c("InSilicoVA_noInj", "InSilicoVA_v2_noInj")[which]
causes4 <- (1:4)[-3]
causes17 <- (1:17)[-c(13, 14, 16)]

## ------------  InSilico Experiment 1  ---------- ##
experiment <- 1
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c4, 
                cause = "gs", 
                cause.table = causes4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
			          auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 2  ---------- ##
experiment <- 2
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c17, 
                cause = "gs", 
                cause.table = causes17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 3  ---------- ##
experiment <- 3
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c4, 
                cause = "gs", 
                cause.table = causes4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 4  ---------- ##
experiment <- 4
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c17, 
                cause = "gs", 
                cause.table = causes17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_9Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)


save.image(paste0("../FilesSent_9Feb2016/results/Feb9_", method_name, ".rdata"))


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

which <- 1
type <- c("fixed", "quantile")[which]
strength <- c(1, 0.01)[which]
method_name <- c("InSilicoVA_noInj", "InSilicoVA_v2_noInj")[which]
load(paste0("../FilesSent_9Feb2016/results/Feb9_", method_name, ".rdata"))
library(InSilicoVA)

## ------------  InSilico Experiment 5  ---------- ##
experiment <- 5
fit[[experiment]] <- insilico.train(
                data = data.20.c4, 
                train = data.80.c4, 
                cause = "gs", 
                cause.table = causes4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_18Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 6  ---------- ##
experiment <- 6
fit[[experiment]] <- insilico.train(
                data = data.20.c17, 
                train = data.80.c17, 
                cause = "gs", 
                cause.table = causes17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_18Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

save.image(paste0("../FilesSent_18Feb2016/results/Feb18_", method_name, ".rdata"))






##                                                               ##
## ------------------------Task 7 - 10 ------------------------- ##
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

which <- 1
type <- c("fixed", "quantile")[which]
strength <- c(1, 0.01)[which]
method_name <- c("InSilicoVA_noInj", "InSilicoVA_v2_noInj")[which]
causes4 <- (1:4)[-3]
causes17 <- (1:17)[-c(13, 14, 16)]

fit <- NULL

## ------------  InSilico Experiment 7  ---------- ##
experiment <- 7
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c4, 
                cause = "gs", 
                causes.table = causes4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 8  ---------- ##
experiment <- 8
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c17, 
                cause = "gs", 
                causes.table = causes17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 9  ---------- ##
experiment <- 9
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c4, 
                cause = "gs", 
                causes.table = causes4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)

## ------------  InSilico Experiment 10  ---------- ##
experiment <- 10
fit[[experiment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c17, 
                cause = "gs", 
                causes.table = causes17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = strength, type = type,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[experiment]])
probs <- fit[[experiment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[experiment]] <- data.frame(deathId = rownames(probs),
                               COD = pick, stringsAsFactors = FALSE)
write.csv(out[[experiment]], 
  file = paste0("../FilesSent_29Feb2016/results/Experiment", experiment, "_12-69_", method_name,".csv"), row.names = FALSE)


save.image(paste0("../FilesSent_29Feb2016/results/Feb29_", method_name, ".rdata"))