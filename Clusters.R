library(data.table)

setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Plan Vital - Fuga/Ultimos modelos")

fuga=fread("Plan_vital_pbb_fuga_y_variables.csv", sep=";", header = T)
clientes=fread("CLIENTES.csv", sep=",", header = T)
names(fuga)

#######################################################
################# SEPARACIÓN DE TABLAS ################
#######################################################

roco_chupalo <- subset(chuto, edad < 35)
dataset_id_90 = dataset_90[, c(1,4,10,11)]

dataset <- merge(dataset_90,clientes, by = "id_persona", all.x = T)

write.table(dataset, "dataset_filtros.csv", row.names = F, sep=";")

##### ARREGLO EN EXCEL ####

dataset_id=fread("dataset_mc.csv", sep=",", header = T)

dataset = dataset_id[, c(4,2,3)]

head(dataset)

#######################################################
################### MÉTODO DEL CODO ###################
#######################################################

set.seed(30118)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}

library(ggplot2)
ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

#######################################################
####################### K-MEDIAS ######################
#######################################################

kmeans <- kmeans(dataset, 5, iter.max = 1000, nstart = 10)

dataset$cluster <- kmeans$cluster

library(rgl)

##### GRÁFICO 3D #####
plot3d(dataset$region, dataset$genero, dataset$edad, col=as.integer(dataset$cluster)) 

#View(dataset)
#######################################################
########## AGREGAR VARIABLES Y CLUSTER AL ID ##########
#######################################################

kmeans_90_final=cbind(dataset_id,kmeans$cluster)
names(kmeans_90_final)
colnames(kmeans_90_final)[5]<-"cluster"
#View(kmeans_90_final)

write.table(kmeans_90_final, "Kmeans_k5.csv", row.names = F, sep=";")


#######################################################
############## SEGMENTACIONES OPCIONALES ##############
#######################################################

#dataset_95 <- subset(fuga, pbb_fuga > 0.95)
#dataset_90 <- subset(fuga, pbb_fuga > 0.90)
#dataset_85 <- subset(fuga, pbb_fuga > 0.85)
#dataset_80 <- subset(fuga, pbb_fuga > 0.80)
#dataset_75 <- subset(fuga, pbb_fuga > 0.75)
#dataset_25 <- subset(fuga, pbb_fuga < 0.25)
#dataset_20 <- subset(fuga, pbb_fuga < 0.20)
#dataset_15 <- subset(fuga, pbb_fuga < 0.15)
#dataset_10 <- subset(fuga, pbb_fuga < 0.10)
#dataset_05 <- subset(fuga, pbb_fuga < 0.05)
#dim(dataset_95)
#dim(dataset_90)
#dim(dataset_85)
#dim(dataset_80)
#dim(dataset_75)
#dim(dataset_25)
#dim(dataset_20)
#dim(dataset_15)
#dim(dataset_10)
#dim(dataset_05)