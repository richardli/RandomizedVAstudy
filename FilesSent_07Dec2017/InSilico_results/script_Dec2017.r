###################################################################################
## This script perform the experiments 7 and 8 for the new data
##    from 3 sites in India
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("amravati", "anand", "mansa")
counter <- 1
fitnames <- rep(NA, 18)
fit <- out1 <- out2 <- out3 <- vector("list", 18)
Nitr <- 10000

###################################################################################
## Experiment 1: adult PHMRC-India ~ [Amravati, Anand, Mansa]
for(site in sites){
	raw <-  read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_adult_split80.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_adult_split20.csv"))
	# colnames(raw.esl)[52] <- colnames(raw)[52]
	# colnames(raw.esl)[65] <- colnames(raw)[65]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:18)
	fit[[counter]] <- codeVA(data = test,
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE)
	summary(fit[[counter]])

	csmf <- getCSMF(fit[[counter]])[,1]
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
	out1[[counter]] <- getTopCOD(fit[[counter]])
	out2[[counter]] <- cbind(Population = csmf, 
				   Sample = prob)
	out3[[counter]] <- getIndivProb(fit[[counter]])

	write.csv(out2[[counter]],
		file = paste0("../FilesSent_07Dec2017/InSilico_results/Experiment", 7, "_", site, "_InSilicoVA_format2.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment7")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}
###################################################################################
## Experiment 8
for(site in sites){
	raw <-  read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_child_split80.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_child_split20.csv"))
	# colnames(raw.esl)[52] <- colnames(raw)[52]
	# colnames(raw.esl)[65] <- colnames(raw)[65]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:18)
	fit[[counter]] <- codeVA(data = test,
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE, 
                    	jump.scale = 0.5)
	summary(fit[[counter]])

	csmf <- getCSMF(fit[[counter]])[,1]
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
	out1[[counter]] <- getTopCOD(fit[[counter]])
	out2[[counter]] <- cbind(Population = csmf, 
				   Sample = prob)
	out3[[counter]] <- getIndivProb(fit[[counter]])

	write.csv(out2[[counter]],
		file = paste0("../FilesSent_07Dec2017/InSilico_results/Experiment", 8, "_", site, "_InSilicoVA_format2.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment8")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}
###################################################################################
## Experiment 8 v2 joint run
for(site in sites){
	raw <-  read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_child_split80.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_07Dec2017/", site, "/ESL_", site, "_child_split20.csv"))
	# colnames(raw.esl)[52] <- colnames(raw)[52]
	# colnames(raw.esl)[65] <- colnames(raw)[65]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:18)
	fit[[counter]] <- codeVA(data = rbind(train, test),
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE, 
                    	jump.scale = 0.2)
	summary(fit[[counter]])

	indiv.prob <- fit[[counter]]$indiv.prob
	indiv.prob <- indiv.prob[which(rownames(indiv.prob) %in% test[, 1]), ]	
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
	out2[[counter]] <- cbind(Population = NA, 
				   Sample = prob)

	write.csv(out2[[counter]],
		file = paste0("../FilesSent_07Dec2017/InSilico_results/Experiment", 8, "_", site, "_InSilicoVA_v2_format2.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment8")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}