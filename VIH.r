#abrir archivo con variables transformadas

library(lubridate)
datos<- readRDS(file = "tablaif.rds")
library(readxl)
CMBD_CRUZADO_VIH <- read_excel("CMBD CRUZADO VIH.xls")

datossociodemograficos<- as.data.frame(CMBD_CRUZADO_VIH)
edad<- datossociodemograficos[,3]
ymd(edad)
edadreal<- floor(time_length(ymd(Sys.Date()) - ymd(edad), unit = "year"))
historial<-datossociodemograficos[,2]
sexo<-as.factor(datossociodemograficos[,4])
vihElisa<-as.factor(datossociodemograficos[,173])
vihsino<- as.factor(as.numeric(is.na(vihElisa)))
final<- cbind(historial,sexo,edadreal,datos,vihElisa,vihsino)
table(final[,27])
#Hacer el modelo de regresion logistica multinivel
paste(nombres, collapse = " + ")
library(lme4)
library(rcompanion)#para nagelkerke?¿?¿
library(car)
modelo<- glmer(vihsino ~ (1 | historial) + sexo + edadreal + IF1 + IF2 + IF3 + IF4 + IF5 + IF6 + IF7 + IF8 + IF9 + IF10 + IF11 + IF12 + IF13 + IF14 + IF15 + IF16 + IF17 + IF18 + IF19 + IF20 + IF21 + IF22,data=final, family="binomial")
summary(modelo)
car::vif(modelo)
#quito las variables no significativas:  IF4, IF5, IF7, IF9, IF12, IF15, IF16, IF17

modelo1<- glmer(vihsino ~ (1 | historial) + sexo + edadreal + IF1 + IF2 + IF3 + IF6 + IF8 + IF10 + IF11 + IF13 + IF14 + IF18 + IF19 + IF20 + IF21 + IF22,data=final, family="binomial")
summary(modelo1)
car::vif(modelo1)
#quito las variables no significativas IF11 y IF13
modelo2<-glmer(vihsino ~ (1 | historial) + sexo + edadreal + IF1 + IF2 + IF3 + IF6 + IF8 + IF10 + IF14 + IF18 + IF19 + IF20 + IF21 + IF22,data=final, family="binomial")
summary(modelo2)


anova(modelo2, modelo1, modelo)#obtenemos igual devianza en los tres modelos por lo que con el principio de parsimonia nos quedamos con el de menor variables
library(performance)
r2(modelo2)
for (i in 4:25){
  print(paste("IF",i-3,sep=""))
  print(table(final[,i]))
}

table(final[,2])



#imputacion con mice
library(mice)
install.packages("mice")
remove.packages("mice")
library(reshape)
library(mitml)
library(lme4)
library(ez);#para hacer ANOVAs de medidas repetidas (al menos un factor)
library(lavaan)
library(ggplot2)
library(matlib)
library(lm.beta)
library(brms)
#regresi?n lineal por el m?todo cl?sico

#Imputaci?n mice
final1<-final[,1:26]

#quito las variables no significativas del modelo
variablesquitadas<- c("IF4", "IF5", "IF7", "IF9","IF11", "IF12", "IF13", "IF15", "IF16", "IF17")
final1<-final1[, !(names(final1) %in% variablesquitadas)]
final2<-final[,1:26]
saveRDS(final1,file = "datossantos.RDS")
str(final2)
final2[,26]<-as.character(final2[,26])
final2[,2]<- as.numeric(final2[,2])
ini<- mice::mice(data=final2,maxit = 0, method = "polyreg")
ini$predictorMatrix[,1]<--2
ini$predictorMatrix[26,]<-2
ini$predictorMatrix[26,1]<--2
ini$predictorMatrix[26,26]<-0
ini$predictorMatrix[26,2:3]<-1
predi<-ini$predictorMatrix
imp<- mice::mice(data = final2, m=5, pred=predi)
summary(imp)
complete(imp)
#--------------------------------------------------------------------------------------------------


library(parallel)


fit <- with(imp, brm(vihElisa ~  sexo + edadreal + IF1 + IF2 + IF3 + IF4 + IF5 + IF6 + IF7 + IF8 + IF9 + IF10 + IF11 + IF12 + IF13 + IF14 + IF15 + IF16 + IF17 + IF18 + IF19 + IF20 + IF21 + IF22 + (1 | historial),data=final2, family=categorical(), chains = nCores, cores = nCores, iter = 500))

nCores<-detectCores()

prueba<-brm(vihElisa ~  sexo + edadreal + IF1 + IF2 + IF3 + IF4 + IF5 + IF6 + IF7 + IF8 + IF9 + IF10 + IF11 + IF12 + IF13 + IF14 + IF15 + IF16 + IF17 + IF18 + IF19 + IF20 + IF21 + IF22 + (1 | historial),data=final2, family=categorical(),chains = nCores, cores = nCores)
summary(prueba)
r2(fit$analyses[[1]])
historial<- as.factor(historial)
table(vihElisa)
testEstimates(fit, extra.pars = TRUE)
table(historial)
table(final2[,4])
summary(pool(fit))
pool.r.squared(fit, adjusted = TRUE)


#pasos seguidos para que el modleo en brms corra. Habia algún problema con la version de rstan y Stanheaders

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")


install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


example(stan_model, package = "rstan", run.dontrun = TRUE)

implist<- mids2mitml.list(imp)

library(performance)
install.packages("miceadds")
library(mitml)
library(miceadds)






