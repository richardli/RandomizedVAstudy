# experiment for predicting India death using all sites
#install.packages("../packages/InSilicoVA_1.1.tar.gz", repos = NULL, type = "source")
library(InSilicoVA)
library(Tariff)

raw.80.c4 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD4Cat_P1_P2_FinalICD_train80Per.csv")
raw.80.c17 <- read.csv("../FilesSent_18Feb2016/ESL_Amravati_12-69_COD17Cat_P1_P2_FinalICD_train80Per.csv")

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



set.seed(1)

test_run <- function(test, train, causes.table){
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
  fit1 <- tariff(causes.train = "gs", 
                symps.train = train, 
                symps.test = test,
                cause.table = causes.table)
  pick.tar <- fit1$causes.test[, 2]
  acc.tar <- length(which(pick.tar == test$gs)) / length(pick.tar)
  csmf <- (table(c(exist.cause, pick.tar)) - 1) / length(pick.tar)
  csmf <- csmf[match(exist.cause, names(csmf))]
  csmf.tar <- 1 - sum(abs(csmf - csmf.test)) / 2 / (1-min(csmf.test))

  return(c(acc.ins, acc.tar, csmf.ins, csmf.tar))
}

#
set.seed(1)
N <- dim(data.80.c4)[1]
Nsim <- 50
#
data <- data.80.c4
causes.table <- 1:4
out4 <- matrix(0, Nsim, 4)
#
for(i in 1:Nsim){
  train.index <- sample(1:N, round(N * .8))
  test.index <- (1:N)[-train.index]
  train <- data[train.index, ]
  test <- data[test.index, ]
  out4[i, ] <- test_run(test, train, causes.table)
}

data <- data.80.c17
causes.table <- 1:17
out17 <- matrix(0, Nsim, 4)
for(i in 1:Nsim){
  train.index <- sample(1:N, round(N * .8))
  test.index <- (1:N)[-train.index]
  train <- data[train.index, ]
  test <- data[test.index, ]
  out17[i, ] <- test_run(test, train, causes.table)
}

out <- list(out4 = out4, out17 = out17)
save(out, file = "../FilesSent_18Feb2016/results/cv.rdata")