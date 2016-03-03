# experiment performed on cluster
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

# experiment <- 1
set.seed(1)
Nsim <- 100
acc.ins <- acc.tar <- rep(NA, Nsim)
csmf.ins <- csmf.tar <- rep(NA, Nsim)
####################################################################
# experiment 1-4: quantile prior
# 
if(experiment == 1){
	N <- dim(data.india.c4)[1]
	train.all <- data.all.c4
	test.all <- data.india.c4
	causes.table <- 1:4
	strength <- 0.01
	resample <- "train"	
	type <- "quantile"
}
####################################################################
if(experiment == 2){
	N <- dim(data.india.c4)[1]
	train.all <- data.all.c17
	test.all <- data.india.c17
	causes.table <- 1:17
	strength <- 0.01
	resample <- "train"	
	type <- "quantile"
}####################################################################
if(experiment == 3){
	N <- dim(data.india.c4)[1]
	train.all <- data.india.c4
	test.all <- data.all.c4
	causes.table <- 1:4
	strength <- 0.01
	resample <- "test"		
	type <- "quantile"
}####################################################################
if(experiment == 4){
	N <- dim(data.india.c4)[1]
	train.all <- data.india.c17
	test.all <- data.all.c17
	causes.table <- 1:17
	strength <- 0.01
	resample <- "test"
	type <- "quantile"
}
####################################################################
# experiment 5-8: fixed prior
# 
if(experiment == 5){
	N <- dim(data.india.c4)[1]
	train.all <- data.all.c4
	test.all <- data.india.c4
	causes.table <- 1:4
	strength <- 1
	resample <- "train"	
	type <- "fixed"
}
####################################################################
if(experiment == 6){
	N <- dim(data.india.c4)[1]
	train.all <- data.all.c17
	test.all <- data.india.c17
	causes.table <- 1:17
	strength <- 1
	resample <- "train"	
	type <- "fixed"
}####################################################################
if(experiment == 7){
	N <- dim(data.india.c4)[1]
	train.all <- data.india.c4
	test.all <- data.all.c4
	causes.table <- 1:4
	strength <- 1
	resample <- "test"		
	type <- "fixed"
}####################################################################
if(experiment == 8){
	N <- dim(data.india.c4)[1]
	train.all <- data.india.c17
	test.all <- data.all.c17
	causes.table <- 1:17
	strength <- 1
	resample <- "test"
	type <- "fixed"
}


for(i in 1:Nsim){
	if(resample == "train"){
		train <- train.all[sample(1:dim(train.all)[1], N), ]
		test <- test.all
	}else{
		test <- test.all[sample(1:dim(test.all)[1], N), ]
		train <- train.all
	}
	csmf.test <- (table(c(causes.table, test$gs)) - 1) / length(test$gs)
	csmf.test <- csmf.test[csmf.test > 0]
	exist.cause <- names(csmf.test)

	fit0 <- insilico.train(
	                data = test, 
	                train = train, 
	                cause = "gs", 
	                cause.table = causes.table,
	                type = type, 
	                length.sim = 1000, burnin = 500, thin = 10 , 
	                seed = i, #updateCondProb=FALSE,
	                levels.strength = strength, 
	                trunc.min = 1e-6, trunc.max = 1-1e-6,
	                auto.length = FALSE)
	probs <- fit0$indiv.prob
	pick <- colnames(probs)[apply(probs, 1, which.max)]
	acc.ins[i] <- length(which(pick == test$gs)) / length(pick)
	# csmf <- (table(c(causes.table, pick)) - 1) / length(pick)
	csmf <- summary(fit0)$csmf
	csmf <- csmf[match(exist.cause, rownames(csmf)), 1]
	csmf.ins[i] <- 1 - sum(abs(csmf - csmf.test)) / 2 / (1-min(csmf.test))

	# tariff
	fit1 <- Tariff(causes.train = "gs", 
                symps.train = train, 
                symps.test = test,
                causes.table = causes.table)
	pick.tar <- fit1$causes.test[, 2]
	acc.tar[i] <- length(which(pick.tar == test$gs)) / length(pick.tar)
	csmf <- (table(c(exist.cause, pick.tar)) - 1) / length(pick.tar)
	csmf <- csmf[match(exist.cause, names(csmf))]
	csmf.tar[i] <- 1 - sum(abs(csmf - csmf.test)) / 2 / (1-min(csmf.test))

	cat(paste("itr", i, 
			  "InSilico", round(mean(acc.ins[1:i]), 4), 
			  "Tariff", round(mean(acc.tar[1:i]), 4), "\n"))
}

out <- list(acc.ins = acc.ins, acc.tar = acc.tar,
			csmf.ins = csmf.ins, csmf.tar = csmf.tar)
save(out, file = paste0("experiment", experiment, ".rda"))	

