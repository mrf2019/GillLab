setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("Invertebrate abundance.csv")

dim(data)
head(data)
colnames(data)

boxplot(data$Abundance~data$U_L, xlab = "Site", ylab = "# Individuals", main = "Number of Invertebrates per Site")


#################Rank abundance of species
LowerAB<-data$Abundance[1:24]
UpperAB<-data$Abundance[25:48]

LowerAB_sort<-sort(LowerAB, decreasing=T)
UpperAB_sort<-sort(UpperAB, decreasing=T)
LowerAB_sort_sum<-cumsum(LowerAB_sort)
UpperAB_sort_sum<-cumsum(UpperAB_sort)
xs<-1:24

plot(xs, LowerAB_sort_sum, col='red', ylim=c(0,100), xaxt='n', ylab = "Number of Individuals",
     main = "Rank Abundance of Species")
points(xs, UpperAB_sort_sum, col='blue')
legend("topleft", c("Lower Site", "Upper Site"), col=c("red", "blue"), 
       pch=c(16,20), bty="n")

#####Relative abundance of species

setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates/data")
data<- read.csv("invert2.csv")

#Earthworm
data$X
rownames(data)<-data$X
data<-data[,c(2:49)]
EarthwormRelAb<-data["Lumbricidae",]/colSums(data, na.rm=T)
EarthwormRelAb<-as.matrix(EarthwormRelAb)
EarthwormRelAb[is.nan(EarthwormRelAb)]<-NA
EarthwormRelAb<-t(as.vector(EarthwormRelAb))



setwd("~/Desktop/Summer_Science_Gill_2021")
master_data<- read.csv("Summer2021MasterData.csv")
