library(psych)
library(readxl)
library(dplyr)
CMBD_CRUZADO_VIH <- read_excel("C:/Users/SANTO/Desktop/master/practicas/CMBD CRUZADO VIH.xls")
datos<-data.frame(CMBD_CRUZADO_VIH[,166:173])
pruebavih<-vector()

table(is.na(datos$VIH.Ag..Ac..ELISA.))

#variables sociodemograficas
historia<-CMBD_CRUZADO_VIH[,2]
fecnac<-CMBD_CRUZADO_VIH[,3]
sexo<-CMBD_CRUZADO_VIH[,4]

#transformando las variables enfermedades en DSM-IV
C1<-CMBD_CRUZADO_VIH[,"C1"]
C2<-CMBD_CRUZADO_VIH[,"C2"]
C3<-CMBD_CRUZADO_VIH[,"C3"]
C4<-CMBD_CRUZADO_VIH[,"C4"]
C5<-CMBD_CRUZADO_VIH[,"C5"]
C6<-CMBD_CRUZADO_VIH[,"C6"]
C7<-CMBD_CRUZADO_VIH[,"C7"]
C8<-CMBD_CRUZADO_VIH[,"C8"]
C9<-CMBD_CRUZADO_VIH[,"C9"]
C10<-CMBD_CRUZADO_VIH[,"C10"]
C11<-CMBD_CRUZADO_VIH[,"C11"]
C12<-CMBD_CRUZADO_VIH[,"C12"]
C13<-CMBD_CRUZADO_VIH[,"C13"]
C14<-CMBD_CRUZADO_VIH[,"C14"]
C15<-CMBD_CRUZADO_VIH[,"C15"]
C16<-CMBD_CRUZADO_VIH[,"C16"]
C17<-CMBD_CRUZADO_VIH[,"C17"]
C18<-CMBD_CRUZADO_VIH[,"C18"]
C19<-CMBD_CRUZADO_VIH[,"C19"]
C20<-CMBD_CRUZADO_VIH[,"C20"]
datoscie<-cbind(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20)


filas<-dim(C1)


for (i in 1:filas){
  for (j in 1:20){
  datoscie[i,j]<-substr(datoscie[i,j], 0, 3)
  }
}
dim(columna1)
columna1<-table(datoscie[,1])
columna2<-table(datoscie[,2])

mezcla<-columna1 + columna2
dim(columna2)

vector1<-LETTERS
vector2<-as.character(c(0:9))
vector3<-as.character(c(0:9))

vector1*vector2

posibles<-vector()
#para datos originales
for (i in 1:10){
  for (j in 1:10){
    for (h in 1:26){
      posibles<-c(posibles,paste(vector1[h],vector2[j],vector3[i],sep = ""))
    }
  }
}

#crear tabla de frecuencias con datos originales
posibles<-sort(posibles)
length(posibles)
recuento<-as.integer(rep(0, length(posibles)))

tabla<-as.data.frame(posibles)
tabla$recuento<-recuento
#meter datos en la tabla de frecuencias
for (i in 1:length(posibles)){
  tabla[i,2]<-sum(datoscie==tabla[i,1],na.rm = T)
}
#eliminar los recuentos 0
tabla2<-subset(tabla,!tabla[,2]==0)

table(tabla2)
table(tabla)


clasdatoscie<-datoscie
clasdatoscie[]<-0
dim(datoscie)
#transformar CIE a categorias de 1 a 22 (minimizado el for)
for (i in 1:15424){
  for (j in 1:20){
    if (datoscie[i,j] %in% tabla[1:200,1]){
      clasdatoscie[i,j]<-1
      }
    if (datoscie[i,j] %in% tabla[201:350,1]){
        clasdatoscie[i,j]<-2
        }
    if (datoscie[i,j] %in% tabla[351:400,1]){
          clasdatoscie[i,j]<-3
          }
    if (datoscie[i,j] %in% tabla[401:500,1]){
            clasdatoscie[i,j]<-4
            }
    if (datoscie[i,j] %in% tabla[501:600,1]){
              clasdatoscie[i,j]<-5
              }
    if (datoscie[i,j] %in% tabla[601:700,1]){
                clasdatoscie[i,j]<-6
                }
    if (datoscie[i,j] %in% tabla[701:760,1]){
                  clasdatoscie[i,j]<-7
                  }
    if (datoscie[i,j] %in% tabla[761:800,1]){
                    clasdatoscie[i,j]<-8
                    }
    if (datoscie[i,j] %in% tabla[801:900,1]){
                      clasdatoscie[i,j]<-9
                      }
    if (datoscie[i,j] %in% tabla[901:1000,1]){
                        clasdatoscie[i,j]<-10
                        }
    if (datoscie[i,j] %in% tabla[1001:1100,1]){
                          clasdatoscie[i,j]<-11
                          }
    if (datoscie[i,j] %in% tabla[1101:1200,1]){
                            clasdatoscie[i,j]<-12
                            }
    if (datoscie[i,j] %in% tabla[1201:1300,1]){
                              clasdatoscie[i,j]<-13
                              }
    if (datoscie[i,j] %in% tabla[1301:1400,1]){
                                clasdatoscie[i,j]<-14
                                }
    if (datoscie[i,j] %in% tabla[1401:1500,1]){
                                  clasdatoscie[i,j]<-15
                                  }
    if (datoscie[i,j] %in% tabla[1501:1600,1]){
                                    clasdatoscie[i,j]<-16
                                    }
    if (datoscie[i,j] %in% tabla[1601:1700,1]){
                                      clasdatoscie[i,j]<-17
                                      }
    if (datoscie[i,j] %in% tabla[1701:1800,1]){
                                        clasdatoscie[i,j]<-18
                                        }
    if (datoscie[i,j] %in% tabla[1801:2000,1]){
                                          clasdatoscie[i,j]<-19
                                          }
    if (datoscie[i,j] %in% tabla[2001:2100,1]){
                                            clasdatoscie[i,j]<-20
                                            }
    if (datoscie[i,j] %in% tabla[2101:2500,1]){
                                              clasdatoscie[i,j]<-21
                                              }
    if (datoscie[i,j] %in% tabla[2501:2600,1]){
                                                clasdatoscie[i,j]<-22
                                              }
  }
  }

#rellenar tabla categorias con recuentos

tablacat<-as.data.frame(c(1:22))
colnames(tablacat)<-"Categorías"
tablacat$Recuento<-rep(0,22)

for (i in 1:22){
  tablacat[i,2]<-sum(clasdatoscie==tablacat[i,1],na.rm = T)
}
#crear las variables de si existe cada categoria en cada linea de nombre IF y el numero de la categoria

tablaif<-datoscie
tablaif[]<-0
colnames(tablaif)<-c(paste("IF",1:20,sep=""))
tablaif$IF21<-rep(0,15424) 
tablaif$IF22<-rep(0,15424)  
head(tablaif)

for (j in 1:22) {
  for (i in 1:15424){
    ifelse (j %in% clasdatoscie[i,], tablaif[i,j]<-1, tablaif[i,j]<-0)
    }
  }

#----------------------------------------------------------------------------------------


#if CMBD_CRUZADO_VIH[,"VIH Ag, Ac (ELISA)"]=="NEGATIVO",
#rempsyc::nice_table pa sacar tablas en formato apa y poder modificar en word


