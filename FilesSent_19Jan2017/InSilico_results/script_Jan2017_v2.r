###################################################################################
## This script perform the 18 experiments for the new data
##    from 3 sites in India
## Avoid specifying separate CSMF for training and testing in small n problems
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
counter <- 1
fitnames <- rep(NA, 12)
fit <- out1 <- out2 <- out3 <- vector("list", 12)
Nitr <- 10000

###################################################################################
## Experiment 3: child PHMRC-India ~ [Amravati, Anand, Mansa], 10 COD
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_19Jan2017/PHMRC_", phmrc_name, "/PHMRC_IHME_allSites_Child_28days-11yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_19Jan2017/", site, "/ESL_", site, "_Child_28days-11yrs.csv"))
	colnames(raw.esl)[89] <- colnames(raw)[89]
	colnames(raw.esl)[90] <- colnames(raw)[90]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:10)
	fit[[counter]] <- codeVA(data = rbind(train, test),
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
	out2[[counter]] <- cbind(Population = NA,
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 3, "_", site, "_12-69_InSilicoVA_v2_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 3, "_", site, "_12-69_InSilicoVA_v2_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 3, "_", site, "_12-69_InSilicoVA_v2_format3.csv"), row.names = TRUE)
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
	raw <-  read.csv(paste0("../FilesSent_19Jan2017/PHMRC_", phmrc_name, "/PHMRC_IHME_India_Child_28days-11yrs.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_19Jan2017/", site, "/ESL_", site, "_Child_28days-11yrs.csv"))
	colnames(raw.esl)[89] <- colnames(raw)[89]
	colnames(raw.esl)[90] <- colnames(raw)[90]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:10)
	fit[[counter]] <- codeVA(data = rbind(train, test),
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
	out2[[counter]] <- cbind(Population = NA,
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 4, "_", site, "_12-69_InSilicoVA_v2_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 4, "_", site, "_12-69_InSilicoVA_v2_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 4, "_", site, "_12-69_InSilicoVA_v2_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment4")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}



###################################################################################
## Experiment 5: neonate PHMRC-India ~ [Amravati, Anand, Mansa], 6 COD
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_19Jan2017/PHMRC_", phmrc_name, "/PHMRC_IHME_allSites_Neo_u28days.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_19Jan2017/", site, "/ESL_", site, "_Neo_u28days.csv"))
	colnames(raw.esl)[48] <- colnames(raw)[48]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:6)
	fit[[counter]] <- codeVA(data = rbind(train, test),
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
	out2[[counter]] <- cbind(Population = NA,
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 5, "_", site, "_12-69_InSilicoVA_v2_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 5, "_", site, "_12-69_InSilicoVA_v2_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 5, "_", site, "_12-69_InSilicoVA_v2_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment5")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}
###################################################################################
## Experiment 6: neonate PHMRC-all ~ [Amravati, Anand, Mansa]
for(site in sites){
	if(site != "Amravati"){
		phmrc_name <- "AnandMansa"
	}else{
		phmrc_name <- site
	}
	raw <-  read.csv(paste0("../FilesSent_19Jan2017/PHMRC_", phmrc_name, "/PHMRC_IHME_India_Neo_u28days.csv"))
	# need to fix the double ".csv" in some files
	raw.esl <- read.csv(paste0("../FilesSent_19Jan2017/", site, "/ESL_", site, "_Neo_u28days.csv"))
	colnames(raw.esl)[48] <- colnames(raw)[48]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.esl, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	CODs <- paste("cause", 1:6)
	fit[[counter]] <- codeVA(data = rbind(train, test),
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
	out2[[counter]] <- cbind(Population = NA, 
				   Sample = prob)
	out3[[counter]] <- indiv.prob

	write.csv(out1[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 6, "_", site, "_12-69_InSilicoVA_v2_format1.csv"), row.names = TRUE)
	write.csv(out2[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 6, "_", site, "_12-69_InSilicoVA_v2_format2.csv"), row.names = TRUE)
	write.csv(out3[[counter]],
		file = paste0("../FilesSent_19Jan2017/results/Experiment", 6, "_", site, "_12-69_InSilicoVA_v2_format3.csv"), row.names = TRUE)
	fitnames[[counter]] <- paste0(site, "_experiment6")
	names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
	counter <- counter + 1

}