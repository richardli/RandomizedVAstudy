## Experiment 8
cod.8.nbc <- read.csv("../CSMF_accuracy/Experiment8_28days-11yrs_CGHRnbc.csv")
cod.8.ins <- read.csv("../CSMF_accuracy/Experiment8_28days-11yrs_InSilicoVA.csv")
cod.8.true <- read.csv("../CSMF_accuracy/Experiment8_28days-11yrs_eVA.csv")


cod.8 <- sort(unique(cod.8.true$COD))
csmf.8.nbc <- table(c(cod.8, cod.8.nbc$COD)) - 1
csmf.8.nbc <- csmf.8.nbc / sum(csmf.8.nbc)
csmf.8.true <- table(c(cod.8, cod.8.true$COD)) - 1
csmf.8.true <- csmf.8.true / sum(csmf.8.true)
csmf.8.ins <- csmf.8.true * 0
csmf.8.ins[as.character(cod.8.ins$COD)] <- cod.8.ins$CSMFPop

acc.8.ins <- 1 - sum(abs(csmf.8.ins - csmf.8.true))/2/(1-min(csmf.8.true ))
acc.8.nbc <- 1 - sum(abs(csmf.8.nbc - csmf.8.true))/2/(1-min(csmf.8.true ))


## Experiment 10
cod.10.nbc <- read.csv("../CSMF_accuracy/Experiment10_28days-11yrs_CGHRnbc.csv")
cod.10.ins <- read.csv("../CSMF_accuracy/Experiment10_28days-11yrs_InSilicoVA.csv")
cod.10.true <- read.csv("../CSMF_accuracy/Experiment10_28days-11yrs_eVA.csv")


cod.10 <- sort(unique(cod.10.true$COD))
csmf.10.nbc <- table(c(cod.10, cod.10.nbc$COD)) - 1
csmf.10.nbc <- csmf.10.nbc / sum(csmf.10.nbc)
csmf.10.true <- table(c(cod.10, cod.10.true$COD)) - 1
csmf.10.true <- csmf.10.true / sum(csmf.10.true)
csmf.10.ins <- csmf.10.true * 0
csmf.10.ins[as.character(cod.10.ins$COD)] <- cod.10.ins$CSMFPop

acc.10.ins <- 1 - sum(abs(csmf.10.ins - csmf.10.true))/2/(1-min(csmf.10.true ))
acc.10.nbc <- 1 - sum(abs(csmf.10.nbc - csmf.10.true))/2/(1-min(csmf.10.true ))
