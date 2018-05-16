###################################################################################
## This script perform the experiments 7 and 8 for the new data
##    from 3 sites in India
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
counter <- 1
fitnames <- rep(NA, 9)
fit <- out1 <- out2 <- out3 <- vector("list", 9)
Nitr <- 10000

###################################################################################
## Experiment 1: adult PHMRC-India ~ [Amravati, Anand, Mansa]
for(site in sites){
	files <- list.files(paste0("../Data_Feb2018/", site ))
	for(file in files){
		filename = gsub(".csv", "", file)
		raw <- read.csv(paste0("../Data_Feb2018/", site, "/", file))
		data <- ConvertData(raw, yesLabel = "yes", noLabel = "no", missLabel = "missing")
		fit[[counter]] <- codeVA(data = data,
						data.type = "WHO", 
						model = "InSilicoVA",
                    	Nsim=Nitr, auto.length = FALSE)
		csmf <- getCSMF(fit[[counter]])[,1]
		prob <- apply(fit[[counter]]$indiv.prob, 2, mean)
		out1[[counter]] <- getTopCOD(fit[[counter]])
		out2[[counter]] <- cbind(Population = csmf, 
					   Sample = prob)
		out3[[counter]] <- getIndivProb(fit[[counter]])

		write.csv(out2[[counter]],
			file = paste0("../Data_Feb2018/InSilico_results/", site, "/", filename, "_InSilicoVA_format2.csv"), row.names = TRUE)
		fitnames[[counter]] <- paste0(site, "_experiment7")
		names(fit)[counter] <- names(out1)[counter] <- names(out2)[counter] <- names(out3)[counter] <- fitnames[[counter]]
		counter <- counter + 1
	}
}