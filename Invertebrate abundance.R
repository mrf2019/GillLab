setwd("~/Desktop/Summer_Science_Gill_2021/Invertebrates")
data<- read.csv("Invertebrate abundance.csv")

dim(data)
head(data)
colnames(data)

boxplot(data$Abundance~data$U_L)

barplot(tapply(data$Abundance, data$U_L, mean, na.rm=T), ylim=c(0,18), xlab = 
"Site", ylab = "# Individuals", main = "Abundance of Invertebrates per Site")

mean<-tapply(data$Abundance,data$U_L,  FUN=mean, na.rm=T)
se<-tapply(data$Abundance,data$U_L,  FUN=sd, na.rm=T)/sqrt(5)
upper<- mean+se
lower<- mean-se
arrows(barx, mean, barx, upper, lwd=1.5, angle=90, length=0.1)
arrows(barx, mean, barx, lower, lwd=1.5, angle=90, length=0.1)

fit<-lm(data$Abundance~data$U_L)
anova(fit)

fit<-aov(data$Abundance~data$U_L)
summary(fit)
anova(fit)


