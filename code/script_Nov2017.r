###################################################################################
## This script perform the 4 experiments for the new data
##    from 3 sites in India
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
counter <- 1
fitnames <- rep(NA, 18)
fit <- out1 <- out2 <- out3 <- vector("list", 18)
Nitr <- 10000

###################################################################################
## Experiment 1: adult PHMRC-India ~ [Amravati, Anand, Mansa]
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_25Nov2017/PHMRC_", phmrc_name, "/PHMRC_IHME_allSites_Adult_12-69yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_25Nov2017/", site, "/ESL_", site, "_Adult_12-69.csv"))
	colnames(raw.esl)[52] <- colnames(raw)[52]
	colnames(raw.esl)[65] <- colnames(raw)[65]

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

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 1, "_", site, "_12-69_InSilicoVA_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_25Nov2017/InSilico_results/Experiment", 1, "_", site, "_12-69_InSilicoVA_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 1, "_", site, "_12-69_InSilicoVA_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment1")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}
###################################################################################
## Experiment 2: adult PHMRC-all ~ [Amravati, Anand, Mansa]
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_25Nov2017/PHMRC_", phmrc_name, "/PHMRC_IHME_India_Adult_12-69yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_25Nov2017/", site, "/ESL_", site, "_Adult_12-69.csv"))
	colnames(raw.esl)[52] <- colnames(raw)[52]
	colnames(raw.esl)[65] <- colnames(raw)[65]

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

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 2, "_", site, "_12-69_InSilicoVA_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_25Nov2017/InSilico_results/Experiment", 2, "_", site, "_12-69_InSilicoVA_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 2, "_", site, "_12-69_InSilicoVA_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment2")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1
}


###################################################################################
## Experiment 3: child PHMRC-India ~ [Amravati, Anand, Mansa], 10 COD
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_25Nov2017/PHMRC_", phmrc_name, "/PHMRC_IHME_allSites_Child_28days-11yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_25Nov2017/", site, "/ESL_", site, "_Child_28days-11yrs.csv"))
	colnames(raw.esl)[89] <- colnames(raw)[89]
	colnames(raw.esl)[90] <- colnames(raw)[90]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:10)
	subpop <- c(rep("train", dim(train)[1]), rep("test", dim(test)[1]))
	fit[[counter]] <- codeVA(data = rbind(train, test),
						subpop = subpop,
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE)
	summary(fit[[counter]])

	indiv.prob <- fit[[counter]]$indiv.prob
	indiv.prob <- indiv.prob[which(rownames(indiv.prob) %in% test[, 1]), ]	
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)

	out1[[counter]] <- getTopCOD(fit[[counter]])
	out1[[counter]] <- out1[[counter]][which((out1[[counter]][,1]) %in% test[, 1]), ]
	out2[[counter]] <- cbind(Population = getCSMF(fit[[counter]])$test[, 1], 
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 3, "_", site, "_12-69_InSilicoVA_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_25Nov2017/InSilico_results/Experiment", 3, "_", site, "_12-69_InSilicoVA_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 3, "_", site, "_12-69_InSilicoVA_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment3")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}
###################################################################################
## Experiment 4: child PHMRC-all ~ [Amravati, Anand, Mansa]
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_25Nov2017/PHMRC_", phmrc_name, "/PHMRC_IHME_India_Child_28days-11yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_25Nov2017/", site, "/ESL_", site, "_Child_28days-11yrs.csv"))
	colnames(raw.esl)[89] <- colnames(raw)[89]
	colnames(raw.esl)[90] <- colnames(raw)[90]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:10)
	subpop <- c(rep("train", dim(train)[1]), rep("test", dim(test)[1]))
	fit[[counter]] <- codeVA(data = rbind(train, test),
						subpop = subpop,
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE)
	summary(fit[[counter]])

	indiv.prob <- fit[[counter]]$indiv.prob
	indiv.prob <- indiv.prob[which(rownames(indiv.prob) %in% test[, 1]), ]	
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)

	out1[[counter]] <- getTopCOD(fit[[counter]])
	out1[[counter]] <- out1[[counter]][which((out1[[counter]][,1]) %in% test[, 1]), ]
	out2[[counter]] <- cbind(Population = getCSMF(fit[[counter]])$test[, 1], 
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 4, "_", site, "_12-69_InSilicoVA_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_25Nov2017/InSilico_results/Experiment", 4, "_", site, "_12-69_InSilicoVA_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_25Nov2017/results/Experiment", 4, "_", site, "_12-69_InSilicoVA_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment4")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}

