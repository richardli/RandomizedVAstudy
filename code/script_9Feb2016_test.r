# experiment for predicting India death using all sites
#install.packages("../packages/InSilicoVA_1.1.tar.gz", repos = NULL, type = "source")
library(InSilicoVA)
source("Tariff.r")

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


set.seed(1)

test_run<- function(test, train, causes.table){
  csmf.test <- (table(c(causes.table, test$gs)) - 1) / length(test$gs)
  csmf.test <- csmf.test[csmf.test > 0]
  exist.cause <- names(csmf.test)

  fit0 <- insilico.train(
                  data = test, 
                  train = train, 
                  cause = "gs", 
                  cause.table = causes.table,
                  type = "fixed", 
                  length.sim = 5000, burnin = 2500, thin = 10 , 
                  seed = 1, #updateCondProb=FALSE,
                  levels.strength = 1, 
                  trunc.min = 1e-6, trunc.max = 1-1e-6,
                  auto.length = FALSE)
  probs <- fit0$indiv.prob
  pick <- colnames(probs)[apply(probs, 1, which.max)]
  acc.ins <- length(which(pick == test$gs)) / length(pick)
  # csmf <- (table(c(causes.table, pick)) - 1) / length(pick)
  csmf <- summary(fit0)$csmf
  csmf <- csmf[match(exist.cause, rownames(csmf)), 1]
  csmf.ins <- 1 - sum(abs(csmf - csmf.test)) / 2 / (1-min(csmf.test))

  # tariff
  fit1 <- Tariff(causes.train = "gs", 
                symps.train = train, 
                symps.test = test,
                causes.table = causes.table)
  pick.tar <- fit1$causes.test[, 2]
  acc.tar <- length(which(pick.tar == test$gs)) / length(pick.tar)
  csmf <- (table(c(exist.cause, pick.tar)) - 1) / length(pick.tar)
  csmf <- csmf[match(exist.cause, names(csmf))]
  csmf.tar <- 1 - sum(abs(csmf - csmf.test)) / 2 / (1-min(csmf.test))

  return(c(acc.ins, acc.tar, csmf.ins, csmf.tar))
}


out.table <- matrix(0, 4, 4)
## 1. all -> India
out.table[1, ] <- test_run(data.india.c4, data.all.c4, 1:4)
out.table[2, ] <- test_run(data.india.c17, data.all.c17, 1:17)
## 2. (all - india) -> India
data.except.c4 <- data.all.c4[-which(data.all.c4$ID %in% data.india.c4$ID), ]
data.except.c17 <- data.all.c17[-which(data.all.c17$ID %in% data.india.c17$ID), ]
out.table[3, ] <- test_run(data.india.c4, data.except.c4, 1:4)
out.table[4, ] <- test_run(data.india.c17, data.except.c17, 1:17)

colnames(out.table) <- c("InSilicoVA Top Cause", "Tariff Top Cause", 
                         "InSilicoVA CSMF", "Tariff CSMF")
rownames(out.table) <- c("4-cause all sites", "17-cause all sites", 
                         "4-cause other sites", "17-cause other sites")

xtable(out.table)