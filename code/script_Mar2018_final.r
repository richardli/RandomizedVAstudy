remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
counter <- 1
fit <- out1 <- out2 <- out3 <- vector("list", 9)
Nitr <- 10000

##########################################################
## Experiment: InSilico-NT
for(site in sites){
	files <- list.files(paste0("../Final/Data_Feb2018/", site ))
	for(file in files){
		filename = gsub(".csv", "", file)
		raw <- read.csv(paste0("../Final/Data_Feb2018/", site, "/", file))
		data <- ConvertData(raw, yesLabel = "yes", noLabel = "no", missLabel = "missing")
		fit[[counter]] <- codeVA(data = data,
						data.type = "WHO", 
						model = "InSilicoVA",
                    	Nsim=Nitr, auto.length = FALSE, 
                    	jump.scale = 0.05)
		csmf <- getCSMF(fit[[counter]])[,1]
		prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
		out1[[counter]] <- getTopCOD(fit[[counter]])
		out2[[counter]] <- cbind(Population = csmf, 
					   Sample = prob)
		out3[[counter]] <- getIndivProb(fit[[counter]])

		write.csv(out2[[counter]],
			file = paste0("../Final/Data_Feb2018/InSilico_results/", site, "/", filename, "_InSilicoVA_format2.csv"), row.names = TRUE)
		write.csv(out1[[counter]],
			file = paste0("../Final/Data_Feb2018/InSilico_results/", site, "/", filename, "_InSilicoVA_format2_indiv.csv"), row.names = TRUE)
		counter <- counter + 1
	}
}

##########################################################
## Experiment with training data

remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
counter <- 1
fitnames <- NULL
fit <- out1 <- out2 <- out3 <- NULL
Nitr <- 10000

##########################################################
## Experiment: InSilico training
for(site in sites){
	for(training in c("allSites", "India")){
	files <- list.files(paste0("../Final/DataFilesSent_19Jan2017/", site ))
	for(file in files){
		filename = gsub(".csv", "", file)
		agename = gsub(paste0("ESL_", site, "_"), "", filename)
		agename <- gsub("12-69", "12-69yrs", agename)

		raw <- read.csv(paste0("../Final/DataFilesSent_19Jan2017/", site, "/", file))
		if(site != "Amravati"){
			raw0 <- read.csv(paste0("../Final/DataFilesSent_19Jan2017/PHMRC_AnandMansa/PHMRC_IHME_", training,"_",agename, ".csv"))
		}else{
			raw0 <- read.csv(paste0("../Final/DataFilesSent_19Jan2017/PHMRC_Amravati/PHMRC_IHME_", training,"_",agename, ".csv"))
		}
		colnames(raw0) <- colnames(raw)
		train <- ConvertData(raw0, yesLabel = "1", noLabel = "0", missLabel = "99")
		test <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")

		# just making sure
		train$cause <- paste("cause", train$cause)
		CODs <- sort(unique(train$cause))
		if(agename != "Adult_12-69yrs"){
			dat <- rbind(train, test)
		}else{
			dat <- test
		}
		fit[[counter]] <- codeVA(data = dat,
							data.type = "customize", 
							model = "InSilicoVA",
							data.train = train, 
							causes.train = "cause",
							onvert.type = "fixed",
							causes.table = CODs,
		                	Nsim=Nitr, 
		                	auto.length = FALSE, 
		                	jump.scale = 0.05)
		csmf <- getCSMF(fit[[counter]])[,1]
		indiv.prob <- fit[[counter]]$indiv.prob
		indiv.prob <- indiv.prob[which(rownames(indiv.prob) %in% test[, 1]), ]	
		prob <- apply(fit[[counter]]$indiv.prob, 2, mean)

		# remove CSMF estimates for population in child and neonates
		if(agename != "Adult_12-69yrs"){
			csmf <- NA
		} 

		out1[[counter]] <- getTopCOD(fit[[counter]])
		out1[[counter]] <- out1[[counter]][which((out1[[counter]][,1]) %in% test[, 1]), ]
		out2[[counter]] <- cbind(Population = csmf, 
					   Sample = prob)

		write.csv(out2[[counter]],
			file = paste0("../Final/DataFilesSent_19Jan2017/InSilico_results/", training, "/", site, "/", filename, "_InSilicoVA_format2.csv"), row.names = TRUE)
		write.csv(out1[[counter]],
			file = paste0("../Final/DataFilesSent_19Jan2017/InSilico_results/", training, "/", site, "/", filename, "_InSilicoVA_format2_indiv.csv"), row.names = TRUE)
		counter <- counter + 1
	}
	}
}