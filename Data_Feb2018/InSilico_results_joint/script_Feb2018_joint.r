###################################################################################
## This script perform the experiments 7 and 8 for the new data
##    from 3 sites in India
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
ages <- c("_12_69_v2.csv", 
			   "_child_29days-11yrs_v2.csv", 
			   "_Neo_u28days_v2.csv")
counter <- 1
fitnames <- rep(NA, 9)
fit <- out1 <- out2 <- out3 <- vector("list", 9)
Nitr <- 10000

###################################################################################
## Experiment 1: adult PHMRC-India ~ [Amravati, Anand, Mansa]
for(age in ages){
	raw <- NULL
	for(site in sites){
		tmp <- read.csv(paste0("../", site, "/InterVA_ESL_", site, age))
		# fix coding
		colnames(tmp)[51] <- "bl_cough"
		tmp$site <- site
		raw <- rbind(raw, tmp)
	}
	data <- ConvertData(raw, yesLabel = "yes", noLabel = "no", missLabel = "missing")
	fit[[counter]] <- codeVA(data = data,
					data.type = "WHO", 
					model = "InSilicoVA",
                	Nsim=Nitr, auto.length = FALSE, 
                	subpop = list("site"))

	# csmf <- getCSMF(fit[[counter]])[,1]
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
	out2[[counter]] <- cbind(Population = NA, 
				   Sample = prob)

	write.csv(out2[[counter]],
		file = paste0("../InSilico_results_joint/Three_sites", age), row.names = TRUE)
	counter <- counter + 1
}

for(age in ages){
	raw <- NULL
	for(site in sites[-1]){
		tmp <- read.csv(paste0("../", site, "/InterVA_ESL_", site, age))
		# fix coding
		colnames(tmp)[51] <- "bl_cough"
		tmp$site <- site
		raw <- rbind(raw, tmp)
	}
	data <- ConvertData(raw, yesLabel = "yes", noLabel = "no", missLabel = "missing")
	fit[[counter]] <- codeVA(data = data,
					data.type = "WHO", 
					model = "InSilicoVA",
                	Nsim=Nitr, auto.length = FALSE, 
                	subpop = list("site"))

	# csmf <- getCSMF(fit[[counter]])[,1]
	prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
	out2[[counter]] <- cbind(Population = NA, 
				   Sample = prob)

	write.csv(out2[[counter]],
		file = paste0("../InSilico_results_joint/AnandMansa", age), row.names = TRUE)
	counter <- counter + 1
}
	