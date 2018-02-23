###################################################################################
## This script perform the 18 experiments for the new data
##    from 3 sites in India
###################################################################################
remove(list = ls())
library(openVA)
sites <- c("Amravati", "Anand", "Mansa")
groups <- c("adult", "child", "neo")
Nitr <- 10000

###################################################################################
## Experiment 7, 8 ,9
for(site in sites){
	for(group in groups){
	name <- tolower(site)
	
	raw <-  read.csv(paste0("../", name, "/esl_", name, "_", group, "_split80.csv"))
	# need to fix the double ".csv" in some files
	raw.test <- read.csv(paste0("../", name, "/esl_", name, "_", group, "_split20.csv"))
	# colnames(raw.test)[52] <- colnames(raw)[52]
	# colnames(raw.test)[65] <- colnames(raw)[65]

	train <- ConvertData(raw, yesLabel = "1", noLabel = "0", missLabel = "99")
	test <- ConvertData(raw.test, yesLabel = "1", noLabel = "0", missLabel = "99")

	# just making sure
	train$cause <- paste("cause", train$cause)
	if(group == "adult"){
		CODs <- paste("cause", 1:18)
		number <- 7

	}else if(group == "child"){
		CODs <- paste("cause", 1:10)
		number <- 8

	}else if(group == "neo"){
		CODs <- paste("cause", 1:6)
		number <- 9

	}
	fit <- codeVA(data = test,
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train, 
						causes.train = "cause",
						convert.type = "fixed",
						causes.table = CODs,
                    	Nsim=Nitr, auto.length = FALSE)
	summary(fit)

	csmf <- getCSMF(fit)[,1]
	prob <- apply(fit$indiv.prob, 2, mean)
	out <- cbind(Population = csmf,  Sample = prob)
	write.csv(out,
		file = paste0("results/Experiment", number, "_", name, "_", group, "InSilicoVA_format2.csv"), row.names = TRUE)
	}
}
