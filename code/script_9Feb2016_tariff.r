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
data.all.c4 <- formatData(raw.india.c4)
data.all.c17 <- formatData(raw.all.c17)
data.esl <- formatData(raw.esl)

fit <- NULL
out <- NULL

## ------------  InSilico Experiment 1  ---------- ##
expriment <- 1
fit[[expriment]] <- Tariff(causes.train = "gs", 
                      symps.train = data.india.c4, 
                      symps.test = data.esl)
pick <- fit[[expriment]]$causes.test[, 2]
out[[expriment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment1_12-69_UWTariff.csv",
  row.names = FALSE)

## ------------  InSilico Experiment 2  ---------- ##
expriment <- 2
fit[[expriment]] <- Tariff(causes.train = "gs", 
                      symps.train = data.india.c17, 
                      symps.test = data.esl)
pick <- fit[[expriment]]$causes.test[, 2]
out[[expriment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment2_12-69_UWTariff.csv",
  row.names = FALSE)

## ------------  InSilico Experiment 3  ---------- ##
expriment <- 3
fit[[expriment]] <- Tariff(causes.train = "gs", 
                      symps.train = data.all.c4, 
                      symps.test = data.esl)
pick <- fit[[expriment]]$causes.test[, 2]
out[[expriment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment3_12-69_UWTariff.csv",
  row.names = FALSE)

## ------------  InSilico Experiment 4  ---------- ##
expriment <- 4
fit[[expriment]] <- Tariff(causes.train = "gs", 
                      symps.train = data.all.c17, 
                      symps.test = data.esl)
pick <- fit[[expriment]]$causes.test[, 2]
out[[expriment]] <- data.frame(deathId = data.esl$ID,
                               COD = pick)
write.csv(out[[expriment]], 
  file = "../FilesSent_9Feb2016/results/Experiment4_12-69_UWTariff.csv",
  row.names = FALSE)



save.image("../FilesSent_9Feb2016/results/Feb9_tariff.rdata")


