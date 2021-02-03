library(agricolae)
library(SuppDists)
library(RODBC)
library(openxlsx)

dir(pattern = "xls")

canal <- odbcConnectExcel("Formato II Mama&BB- Quilcas.xls")
datos <- sqlFetch(canal, "mama")

dat = na.omit(datos)
colnames(dat)

(attach(dat))

(comparison <- friedman(judge, trt, MV, alpha = 0.05, group = TRUE, main = NULL))
(comparison <- friedman(judge, trt, MM, alpha = 0.05, group = TRUE, main = NULL))
(comparison <- friedman(judge, trt, MT, alpha = 0.05, group = TRUE, main = NULL))


###
SheetName <- c("Chota", "Cutervo", "Leopata", "Apacheta", "Paucara", "LV", "Chulec", "Pataz", "Chugay")


for(i in 1:8){
  
dat <- read.xlsx("STC almacenamiento 2019-2020.xlsx", sheet = SheetName[i])
dfr = na.omit(dat)

(comp <- friedman(judge = dfr$REP, trt = dfr$INSTN, evaluation = dfr$SCORE_GLOBAL, alpha = 0.05, group = TRUE, main = NULL))

write.csv(comp$statistics, paste0("statistics_", SheetName[i], ".csv"))
write.csv(comp$means, paste0("means_", SheetName[i], ".csv"))
write.csv(comp$groups, paste0("groups_", SheetName[i], ".csv"))

}



