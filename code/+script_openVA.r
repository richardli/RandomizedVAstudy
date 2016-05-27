# Script to run the new dataset with openVA package
# Richard Li
# Apr 14, 2016

library(openVA)

fit.insilico <- NULL
fit.tariff <- NULL
out.insilico <- NULL
out.tariff <- NULL

##############################################################
#--------------------- Task 1 - 4 ---------------------------#
############################################################### 

## read data
raw.india.c4 <- read.csv("../FilesSent_14Apr2016/PHMRC_India_12-69_4CODCategories.csv")
raw.india.c17 <- read.csv("../FilesSent_14Apr2016/PHMRC_India_12-69_17CODCategories_14April2016.csv")
raw.all.c4 <- read.csv("../FilesSent_14Apr2016/PHMRC_AllSites_12-69_4CODCategories.csv")
raw.all.c17 <- read.csv("../FilesSent_14Apr2016/PHMRC_AllSites_12-69_17CODCategories_14April2016.csv")
raw.esl <- read.csv("../FilesSent_14Apr2016/ESL_Amravati_12-69.csv")

## change data
train.task1 <- ConvertData(raw.india.c4, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task2 <- ConvertData(raw.india.c17, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task3 <- ConvertData(raw.all.c4, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task4 <- ConvertData(raw.all.c17, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
test.task1 <- ConvertData(raw.esl, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

#--------------------- Task 1 ---------------------------#
experiment <- 1
CODs <- c(1:3)
fit.insilico[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task1, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment1_12-69_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task1, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment1_12-69_Tariff.csv", row.names = TRUE)




#--------------------- Task 2 ---------------------------#
experiment <- 2
CODs <- c(1:17)[-c(2, 12, 15)]
fit.insilico[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task2, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment2_12-69_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task2, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment2_12-69_Tariff.csv", row.names = TRUE)


#--------------------- Task 3 ---------------------------#
experiment <- 3
CODs <- c(1:3)
fit.insilico[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task3, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment3_12-69_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task3, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment3_12-69_Tariff.csv", row.names = TRUE)




#--------------------- Task 4 ---------------------------#
experiment <- 4
CODs <- c(1:17)[-c(2, 12, 15)]
fit.insilico[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task4, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment4_12-69_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task1, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task4, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment4_12-69_Tariff.csv", row.names = TRUE)


save.image("../FilesSent_14Apr2016/results/workspace.rdata")



##############################################################
#--------------------- Task 5 - 6  ---------------------------#
############################################################## 


load("../FilesSent_14Apr2016/results/workspace.rdata")

## read data
raw0.80 <- read.csv("../FilesSent_14Apr2016/ESL_Amravati_12-69_COD4Cat_P1_P2_FinalICD_train80Per.csv")
raw0.20 <- read.csv("../FilesSent_14Apr2016/ESL_Amravati_12-69_COD4Cat_P1_P2_FinalICD_test20Per.csv")

raw.80 <- read.csv("../FilesSent_14Apr2016/train80Per_ESL_Amravati_12-69_COD17Cat_P1_P2_FinalICD_14April2016.csv")
raw.20 <- read.csv("../FilesSent_14Apr2016/test20Per_ESL_Amravati_12-69_COD17Cat_P1_P2_FinalICD_14April2016.csv")


## change data
train.task5 <- ConvertData(raw0.80, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
test.task5 <- ConvertData(raw0.20, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task5 <- train.task5[, -c(3, 4)]

train.task6 <- ConvertData(raw.80, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
test.task6 <- ConvertData(raw.20, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task6 <- train.task6[, -c(3, 4)]

#--------------------- Task 5 ---------------------------#
experiment <- 5
CODs <- c(1:4) 
fit.insilico[[experiment]] <- codeVA(data = test.task5, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task5, 
						causes.train = "final_cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment5_12-69_InSilicoVA.csv", row.names = TRUE)

## additional results per request
indiv <- getTopCOD(fit.insilico[[experiment]])
write.csv(indiv, 
	file = "../FilesSent_14Apr2016/results/Experiment5_12-69_InSilicoVA_indiv.csv", row.names = FALSE)



fit.tariff[[experiment]] <- codeVA(data = test.task5, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task5, 
						causes.train = "final_cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment5_12-69_Tariff.csv", row.names = TRUE)
## additional results per request
indiv <- getTopCOD(fit.tariff[[experiment]])
write.csv(indiv, 
	file = "../FilesSent_14Apr2016/results/Experiment5_12-69_Tariff_indiv.csv", row.names = FALSE)



#--------------------- Task 6 ---------------------------#
experiment <- 6
CODs <- c(1:17)[-c(2, 7, 12)]
fit.insilico[[experiment]] <- codeVA(data = test.task6, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task6, 
						causes.train = "final_cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment6_12-69_InSilicoVA.csv", row.names = TRUE)

## additional results per request
indiv <- getTopCOD(fit.insilico[[experiment]])
write.csv(indiv, 
	file = "../FilesSent_14Apr2016/results/Experiment6_12-69_InSilicoVA_indiv.csv", row.names = FALSE)



fit.tariff[[experiment]] <- codeVA(data = test.task6, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task6, 
						causes.train = "final_cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_14Apr2016/results/Experiment6_12-69_Tariff.csv", row.names = TRUE)
## additional results per request
indiv <- getTopCOD(fit.tariff[[experiment]])
write.csv(indiv, 
	file = "../FilesSent_14Apr2016/results/Experiment6_12-69_Tariff_indiv.csv", row.names = FALSE)


save.image("../FilesSent_14Apr2016/results/workspace2.rdata")



##############################################################
#---------------- Task 7,8,9,10,13,14  ----------------------#
############################################################## 

## ------
##  Additional results added because sample size in test data is too small
## ------
load("../FilesSent_14Apr2016/results/workspace2.rdata")
raw0.child.india.c4 <- read.csv("../FilesSent_9May2016/Child/PHMRC_IHME_India_Child_28days-11yrs_4CODcategories.csv")
raw0.child.india.c17 <- read.csv("../FilesSent_9May2016/Child/PHMRC_IHME_India_Child_28days-11yrs_15CODcategories.csv")
raw0.child.all.c4 <- read.csv("../FilesSent_9May2016/Child/PHMRC_IHME_allSites_Child_28days-11yrs_4CODCategories.csv")
raw0.child.all.c17 <- read.csv("../FilesSent_9May2016/Child/PHMRC_IHME_allSites_Child_28days-11yrs_15CODCategories.csv")

raw0.neo.india <- read.csv("../FilesSent_9May2016/Neonates/PHMRC_IHME_india_Neo_u28days.csv")
raw0.neo.all <- read.csv("../FilesSent_9May2016/Neonates/PHMRC_IHME_allSites_Neo_u28days.csv")

test.child <- read.csv("../FilesSent_9May2016/Child/ESL_Amravati_Child_28days-11yrs_15CODCategories.csv")
test.neo <- read.csv("../FilesSent_9May2016/Neonates/ESL_Amravati_u28days.csv")



train.task7 <- ConvertData(raw0.child.india.c4, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task8 <- ConvertData(raw0.child.india.c17, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task9 <- ConvertData(raw0.child.all.c4, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task10 <- ConvertData(raw0.child.all.c17, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task13 <- ConvertData(raw0.neo.india, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task14 <- ConvertData(raw0.neo.all, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

test.task7 <- ConvertData(test.child, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

test.task13 <- ConvertData(test.neo, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

fit.insilico.combine <- NULL

#--------------------- Task 7 ---------------------------#
experiment <- 7
CODs <- c(1:3)
fit.insilico[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task7, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task7, test.task7), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task7, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task7[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment7_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task7, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment7_28days-11yrs_Tariff.csv", row.names = TRUE)


#--------------------- Task 8 ---------------------------#
experiment <- 8
CODs <- c(1, 3, 5, 11, 13, 14)
fit.insilico[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task8, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task8, test.task7), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task8, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task7[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment8_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task8, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment8_28days-11yrs_Tariff.csv", row.names = TRUE)



#--------------------- Task 9 ---------------------------#
experiment <- 9
CODs <- c(1:3)
fit.insilico[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task9, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task9, test.task7), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task9, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task7[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment9_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task9, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment9_28days-11yrs_Tariff.csv", row.names = TRUE)



#--------------------- Task 10 ---------------------------#
experiment <- 10
CODs <- c(1, 3, 5, 11, 13, 14)
fit.insilico[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task10, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task10, test.task7), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task10, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task7[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment10_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task7, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task10, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment10_28days-11yrs_Tariff.csv", row.names = TRUE)





#--------------------- Task 13 ---------------------------#
experiment <- 13
CODs <- c(1:2)
fit.insilico[[experiment]] <- codeVA(data = test.task13, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task13, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task13, test.task13), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task13, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task13[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment13_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task13, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task13, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment13_28days-11yrs_Tariff.csv", row.names = TRUE)





#--------------------- Task 14 ---------------------------#
experiment <- 14
CODs <- c(1:2)
fit.insilico[[experiment]] <- codeVA(data = test.task13, 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task14, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)

csmf <- getCSMF(fit.insilico[[experiment]])[,1]
prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task14, test.task13), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task14, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task13[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment14_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task13, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task14, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_9May2016/results/Experiment14_28days-11yrs_Tariff.csv", row.names = TRUE)

save.image("../FilesSent_9May2016/results/workspace3.rdata")


##############################################################
#---------------- Task 11, 12, 15      ----------------------#
############################################################## 

## ------
##  Additional results added because sample size in test data is too small
## ------
load("../FilesSent_9May2016/results/workspace3.rdata")
raw0.child.80.c4 <- read.csv("../FilesSent_12May2016/Child/train80Per_ESL_Amravati_Child_28days-11yrs_4CODCategories.csv")
raw0.child.80.c17 <- read.csv("../FilesSent_12May2016/Child/train80Per_ESL_Amravati_Child_28days-11yrs_15CODCategories.csv")

raw0.neo.80 <- read.csv("../FilesSent_12May2016/Neonate/train80Per_ESL_Amravati_u28days.csv")

test.child.20 <- read.csv("../FilesSent_12May2016/Child/test20Per_ESL_Amravati_Child_28days-11yrs_4CODCategories.csv")
test.neo.20 <- read.csv("../FilesSent_12May2016/Neonate/test20Per_ESL_Amravati_u28days.csv")



train.task11 <- ConvertData(raw0.child.80.c4, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task12 <- ConvertData(raw0.child.80.c17, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")
train.task15 <- ConvertData(raw0.neo.80, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

test.task11 <- ConvertData(test.child.20, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")

test.task15 <- ConvertData(test.neo.20, 
			   yesLabel = "1", noLabel = "0", missLabel = "99")


#--------------------- Task 11 ---------------------------#

experiment <- 11
CODs <- c(1, 2, 4)
# fit.insilico[[experiment]] <- codeVA(data = test.task11, 
# 						data.type = "customize", 
# 						model = "InSilicoVA",
# 						data.train = train.task11, 
# 						causes.train = "cause",
# 						onvert.type = "fixed",
# 						causes.table = CODs,
#                     	Nsim=10000, auto.length = FALSE)

# csmf <- getCSMF(fit.insilico[[experiment]])[,1]
# prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
# out.insilico[[experiment]] <- cbind(Population = csmf, 
# 				   Sample = prob)
csmf <- NULL
prob <- NULL

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task11, test.task11), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task11, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task11[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment11_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task11, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task11, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment11_28days-11yrs_Tariff.csv", row.names = TRUE)

#--------------------- Task 12 ---------------------------#

experiment <- 12
CODs <- c(1, 3, 5, 8, 11, 12, 14, 15)
# fit.insilico[[experiment]] <- codeVA(data = test.task11, 
# 						data.type = "customize", 
# 						model = "InSilicoVA",
# 						data.train = train.task11, 
# 						causes.train = "cause",
# 						onvert.type = "fixed",
# 						causes.table = CODs,
#                     	Nsim=10000, auto.length = FALSE)

# csmf <- getCSMF(fit.insilico[[experiment]])[,1]
# prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
# out.insilico[[experiment]] <- cbind(Population = csmf, 
# 				   Sample = prob)
csmf <- NULL
prob <- NULL

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task12, test.task11), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task12, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task11[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment12_28days-11yrs_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task11, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task12, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment12_28days-11yrs_Tariff.csv", row.names = TRUE)


#--------------------- Task 15 ---------------------------#
experiment <- 15
CODs <- c(1, 2)
# fit.insilico[[experiment]] <- codeVA(data = test.task11, 
# 						data.type = "customize", 
# 						model = "InSilicoVA",
# 						data.train = train.task11, 
# 						causes.train = "cause",
# 						onvert.type = "fixed",
# 						causes.table = CODs,
#                     	Nsim=10000, auto.length = FALSE)

# csmf <- getCSMF(fit.insilico[[experiment]])[,1]
# prob <- apply(fit.insilico[[experiment]]$indiv.prob, 2, mean)
# out.insilico[[experiment]] <- cbind(Population = csmf, 
# 				   Sample = prob)
csmf <- NULL
prob <- NULL

fit.insilico.combine[[experiment]] <- codeVA(
						data = rbind(train.task15, test.task15), 
						data.type = "customize", 
						model = "InSilicoVA",
						data.train = train.task15, 
						causes.train = "cause",
						onvert.type = "fixed",
						causes.table = CODs,
                    	Nsim=10000, auto.length = FALSE)
allprob <- fit.insilico.combine[[experiment]]$indiv.prob
testprob <- allprob[which(rownames(allprob) %in% test.task15[, 1]), ]
prob2 <- apply(testprob, 2, mean)
out.insilico[[experiment]] <- cbind(Population = csmf, 
				   Sample = prob, Sample_combined_estimates = prob2)

write.csv(out.insilico[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment15_u28days_InSilicoVA.csv", row.names = TRUE)


fit.tariff[[experiment]] <- codeVA(data = test.task15, 
						data.type = "customize", 
						model = "Tariff",
						data.train = train.task15, 
						causes.train = "cause",
						causes.table = CODs)
csmf <- getCSMF(fit.tariff[[experiment]])
out.tariff[[experiment]] <- data.frame(Tariff=csmf[as.character(CODs)]) 
write.csv(out.tariff[[experiment]], 
	file = "../FilesSent_12May2016/results/Experiment15_u28days_Tariff.csv", row.names = TRUE)
save.image("../FilesSent_12May2016/results/workspace4.rdata")
