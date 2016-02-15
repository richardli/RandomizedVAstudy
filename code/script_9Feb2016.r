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
data.all.c4 <- formatData(raw.india.c4)
data.all.c17 <- formatData(raw.all.c17)
data.esl <- formatData(raw.esl)

fit <- NULL
out <- NULL


test <- insilico.train(
                data = data.esl, 
                train = data.india.c4, 
                cause = "gs", 
                cause.table = 1:4, 
                length.sim = 100, burnin = 50, thin = 10 , seed = 1,
                levels.strength = 0.01,
                updateCondProb = FALSE,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)

## ------------  InSilico Experiment 1  ---------- ##
expriment <- 1
fit[[expriment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c4, 
                cause = "gs", 
                cause.table = 1:4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = 0.01,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
			          auto.length = FALSE)
summary(fit[[expriment]])
probs <- fit[[expriment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[expriment]] <- data.frame(deathId = rownames(probs),
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment1_12-69_InSilicoVA.csv", row.names = FALSE)

## ------------  InSilico Experiment 2  ---------- ##
expriment <- 2
fit[[expriment]] <- insilico.train(
                data = data.esl, 
                train = data.india.c17, 
                cause = "gs", 
                cause.table = 1:17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = 0.01,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[expriment]])
probs <- fit[[expriment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[expriment]] <- data.frame(deathId = rownames(probs),
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment2_12-69_InSilicoVA.csv", row.names = FALSE)

## ------------  InSilico Experiment 3  ---------- ##
expriment <- 3
fit[[expriment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c4, 
                cause = "gs", 
                cause.table = 1:4, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = 0.01,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[expriment]])
probs <- fit[[expriment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[expriment]] <- data.frame(deathId = rownames(probs),
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment3_12-69_InSilicoVA.csv", row.names = FALSE)

## ------------  InSilico Experiment 4  ---------- ##
expriment <- 4
fit[[expriment]] <- insilico.train(
                data = data.esl, 
                train = data.all.c17, 
                cause = "gs", 
                cause.table = 1:17, 
                length.sim = 10000, burnin = 5000, thin = 10 , seed = 1,
                levels.strength = 0.01,
                trunc.min = 1e-6, trunc.max = 1-1e-6,
                auto.length = FALSE)
summary(fit[[expriment]])
probs <- fit[[expriment]]$indiv.prob
pick <- colnames(probs)[apply(probs, 1, which.max)]
out[[expriment]] <- data.frame(deathId = rownames(probs),
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment4_12-69_InSilicoVA.csv", row.names = FALSE)


fit01 <- Tariff(causes.train = "gs", 
                symps.train = data.all.c17, 
                symps.test = data.india.c17)
pick <- fit01$causes.test[, 2]
table(as.numeric(pick), data.india.c17$gs)
length(which(pick == data.india.c17$gs)) / length(pick)




save.image("../FilesSent_9Feb2016/results/Feb9_insilico.rdata")


