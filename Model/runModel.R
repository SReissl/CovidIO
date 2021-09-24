
params<-as.numeric(read.csv("estimatedParams.csv",header=TRUE))
rounds=53
source("ModelFinal2020.R")
IO<-readRDS("IO_spese.rds")
measures<-readRDS("measures_spese.rds")
cons_coeff<- readRDS("coefficienti_consumo.RDS")
mobility<-as.data.frame(read.csv("weeklyMobility.csv",header=TRUE,row.names = 1))
mobility<-mobility[,2:21]
ExpService<-as.data.frame(read.csv("ExportsServices2020.csv",header = FALSE))
ExpSectoral<-as.data.frame(read.csv("ExportsSectoral2020_deflated.csv",header = TRUE))
LD<-read.csv("shocks_fra_estimation.csv")
LD2<-read.csv("shocks_fra_scenario.csv")
scenario=1


result<-runModel(IO,measures,cons_coeff,params,rounds,LD,LD2,scenario,ExpService,ExpSectoral,mobility)

