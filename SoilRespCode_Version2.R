##### Script to process soil respiration fluxes
setwd("~/Desktop/Summer_Science_Gill_2021/Flux data") # Update to match individual computer directories

install.packages("lubridate")  # need to run this line once to install lubridate R package. After you run it once, you can just use the library command
library(lubridate)

filenames <- list.files("Flux measurement 1", pattern="*.csv", full.names=TRUE) 
ldf <- lapply(filenames, read.csv)

### Bind all the relevent files in the "CSV files" folder into one big table. 
##### This step will take several minutes to run if there are a lot of files.
load_data <- function(path) { 
   files <- dir("Flux measurement 1", pattern = '\\.csv', full.names = TRUE)
   tables <- lapply(files, read.csv)
   do.call(rbind, tables)
}
data <- load_data("Flux measurement 1")
dim(data)

data <- read.csv("20210615_LowerSite_SoilRespiration.csv")
dim(data)
head(data)
colnames(data)


################ Flux calculation Collar 1_1
Collar1_1<-subset(data, subset = data$Plot_No=="1_1")
dim(Collar1_1)

plot(Collar1_1$Rec_No, Collar1_1$CO2)

Collar1_1_short <- Collar1_1[c(8:60), ]
plot(Collar1_1_short$Rec_No, Collar1_1_short$CO2)
Collar1_1_short[,"Rec_No"]
Collar1_1_short$Rec_No


##### n = RT/PV
n = ((Collar1_1$Pressure[1]/1000)*2)/((0.08314)*(Collar1_1$Tair[1]+273.15))
Collar1_1_short$CO2_umol_m2 <- (Collar1_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar1_1_short$Rec_No,Collar1_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 1_1")
#lm(y~x)
fit1<-lm(Collar1_1_short$CO2_umol_m2~Collar1_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_1_1<-(summary(lm(Collar1_1_short$CO2_umol_m2~Collar1_1_short$Rec_No))$coefficients[2])
R1_1<-summary(lm(Collar1_1_short$CO2_umol_m2~Collar1_1_short$Rec_No))$adj.r.squared


################ Flux calculation Collar 1_2
Collar1_2<-subset(data, subset = data$Plot_No=="1_2")
dim(Collar1_2)

plot(Collar1_2$Rec_No, Collar1_2$CO2)

Collar1_2_short <- Collar1_2[c(15:60), ]
plot(Collar1_2_short$Rec_No, Collar1_2_short$CO2)
Collar1_2_short[,"Rec_No"]
Collar1_2_short$Rec_No


##### n = RT/PV
n = ((Collar1_2$Pressure[1]/1000)*2)/((0.08314)*(Collar1_2$Tair[1]+273.15))
Collar1_2_short$CO2_umol_m2 <- (Collar1_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar1_2_short$Rec_No,Collar1_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", xlab = "Rec No", main= "Co2 Flux Collar 1_2")
#lm(y~x)
fit1<-lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_1_2<-(summary(lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No))$coefficients[2])
R1_2<-summary(lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No))$adj.r.squared

####################################################################################################
################ Flux calculation Collar 1_2
Collar1_2<-subset(data, subset = data$Plot_No=="1_2")
dim(Collar1_2)

plot(Collar1_2$Rec_No, Collar1_2$CO2)

Collar1_2_short <- Collar1_2[c(15:60), ]
plot(Collar1_2_short$Rec_No, Collar1_2_short$CO2)
Collar1_2_short[,"Rec_No"]
Collar1_2_short$Rec_No


##### n = RT/PV
n = ((Collar1_2$Pressure[1]/1000)*2)/((0.08314)*(Collar1_2$Tair[1]+273.15))
Collar1_2_short$CO2_umol_m2 <- (Collar1_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar1_2_short$Rec_No,Collar1_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 2_1")
#lm(y~x)
fit1<-lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_1_2<-(summary(lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No))$coefficients[2])
R2_1<-summary(lm(Collar1_2_short$CO2_umol_m2~Collar1_2_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 2_1
Collar2_1<-subset(data, subset = data$Plot_No=="2_1")
dim(Collar2_1)

plot(Collar2_1$Rec_No, Collar2_1$CO2)

Collar2_1_short <- Collar2_1[c(8:60), ]
plot(Collar2_1_short$Rec_No, Collar2_1_short$CO2)
Collar2_1_short[,"Rec_No"]
Collar2_1_short$Rec_No


##### n = RT/PV
n = ((Collar2_1$Pressure[1]/1000)*2)/((0.08314)*(Collar2_1$Tair[1]+273.15))
Collar2_1_short$CO2_umol_m2 <- (Collar2_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar2_1_short$Rec_No,Collar2_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main= "Co2 Flux Collar 2_1")
#lm(y~x)
fit1<-lm(Collar2_1_short$CO2_umol_m2~Collar2_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_2_1<-(summary(lm(Collar2_1_short$CO2_umol_m2~Collar2_1_short$Rec_No))$coefficients[2])
R2_2<-summary(lm(Collar2_1_short$CO2_umol_m2~Collar2_1_short$Rec_No))$adj.r.squared

################ Flux calculation Collar 2_2
Collar2_2<-subset(data, subset = data$Plot_No=="2_2")
dim(Collar2_2)

plot(Collar2_2$Rec_No, Collar2_2$CO2)

Collar2_2_short <- Collar2_2[c(16:60), ]
plot(Collar2_2_short$Rec_No, Collar2_2_short$CO2)
Collar2_2_short[,"Rec_No"]
Collar2_2_short$Rec_No


##### n = RT/PV
n = ((Collar2_2$Pressure[1]/1000)*2)/((0.08314)*(Collar2_2$Tair[1]+273.15))
Collar2_2_short$CO2_umol_m2 <- (Collar2_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar2_2_short$Rec_No,Collar2_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 2_2")
#lm(y~x)
fit1<-lm(Collar2_2_short$CO2_umol_m2~Collar2_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_2_2<-(summary(lm(Collar2_2_short$CO2_umol_m2~Collar2_2_short$Rec_No))$coefficients[2])
R3_1<-summary(lm(Collar2_2_short$CO2_umol_m2~Collar2_2_short$Rec_No))$adj.r.squared


################ Flux calculation Collar 3_1
Collar3_1<-subset(data, subset = data$Plot_No=="3_1")
dim(Collar3_1)

plot(Collar3_1$Rec_No, Collar3_1$CO2)

Collar3_1_short <- Collar3_1[c(8:60), ]
plot(Collar3_1_short$Rec_No, Collar3_1_short$CO2)
Collar3_1_short[,"Rec_No"]
Collar3_1_short$Rec_No


##### n = RT/PV
n = ((Collar3_1$Pressure[1]/1000)*2)/((0.08314)*(Collar3_1$Tair[1]+273.15))
Collar3_1_short$CO2_umol_m2 <- (Collar3_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar3_1_short$Rec_No,Collar3_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 3_1")
#lm(y~x)
fit1<-lm(Collar3_1_short$CO2_umol_m2~Collar3_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_3_1<-(summary(lm(Collar3_1_short$CO2_umol_m2~Collar3_1_short$Rec_No))$coefficients[2])
R3_2<-summary(lm(Collar3_1_short$CO2_umol_m2~Collar3_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 3_2
Collar3_2<-subset(data, subset = data$Plot_No=="3_2")
dim(Collar3_2)

plot(Collar3_2$Rec_No, Collar3_2$CO2)

Collar3_2_short <- Collar3_2[c(8:60), ]
plot(Collar3_2_short$Rec_No, Collar3_2_short$CO2)
Collar3_2_short[,"Rec_No"]
Collar3_2_short$Rec_No


##### n = RT/PV
n = ((Collar3_2$Pressure[1]/1000)*2)/((0.08314)*(Collar3_2$Tair[1]+273.15))
Collar3_2_short$CO2_umol_m2 <- (Collar3_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar3_2_short$Rec_No,Collar3_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 3_2")
#lm(y~x)
fit1<-lm(Collar3_2_short$CO2_umol_m2~Collar3_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_3_2<-(summary(lm(Collar3_2_short$CO2_umol_m2~Collar3_2_short$Rec_No))$coefficients[2])
R3_2<-summary(lm(Collar3_2_short$CO2_umol_m2~Collar3_2_short$Rec_No))$adj.r.squared



###########Flux calculation Collar 4_1
Collar4_1<-subset(data, subset = data$Plot_No=="4_1")
dim(Collar4_1)

plot(Collar4_1$Rec_No, Collar4_1$CO2)

Collar4_1_short <- Collar4_1[c(10:60), ]
plot(Collar4_1_short$Rec_No, Collar4_1_short$CO2)
Collar4_1_short[,"Rec_No"]
Collar4_1_short$Rec_No


##### n = RT/PV
n = ((Collar4_1$Pressure[1]/1000)*2)/((0.08314)*(Collar4_1$Tair[1]+273.15))
Collar4_1_short$CO2_umol_m2 <- (Collar4_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar4_1_short$Rec_No,Collar4_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 4_1")
#lm(y~x)
fit1<-lm(Collar4_1_short$CO2_umol_m2~Collar4_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_4_1<-(summary(lm(Collar4_1_short$CO2_umol_m2~Collar4_1_short$Rec_No))$coefficients[2])
R4_1<-summary(lm(Collar4_1_short$CO2_umol_m2~Collar4_1_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 4_2
Collar4_2<-subset(data, subset = data$Plot_No=="4_2")
dim(Collar4_2)

plot(Collar4_2$Rec_No, Collar4_2$CO2)

Collar4_2_short <- Collar4_2[c(8:60), ]
plot(Collar4_2_short$Rec_No, Collar4_2_short$CO2)
Collar4_2_short[,"Rec_No"]
Collar4_2_short$Rec_No


##### n = RT/PV
n = ((Collar4_2$Pressure[1]/1000)*2)/((0.08314)*(Collar4_2$Tair[1]+273.15))
Collar4_2_short$CO2_umol_m2 <- (Collar4_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar4_2_short$Rec_No,Collar4_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 4_2")
#lm(y~x)
fit1<-lm(Collar4_2_short$CO2_umol_m2~Collar4_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_4_2<-(summary(lm(Collar4_2_short$CO2_umol_m2~Collar4_2_short$Rec_No))$coefficients[2])
R4_2<-summary(lm(Collar4_2_short$CO2_umol_m2~Collar4_2_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 5_1
Collar5_1<-subset(data, subset = data$Plot_No=="5_1")
dim(Collar5_1)

plot(Collar5_1$Rec_No, Collar5_1$CO2)

Collar5_1_short <- Collar5_1[c(8:60), ]
plot(Collar5_1_short$Rec_No, Collar5_1_short$CO2)
Collar5_1_short[,"Rec_No"]
Collar5_1_short$Rec_No


##### n = RT/PV
n = ((Collar5_1$Pressure[1]/1000)*2)/((0.08314)*(Collar5_1$Tair[1]+273.15))
Collar5_1_short$CO2_umol_m2 <- (Collar5_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar5_1_short$Rec_No,Collar5_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 5_1")
#lm(y~x)
fit1<-lm(Collar5_1_short$CO2_umol_m2~Collar5_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_5_1<-(summary(lm(Collar5_1_short$CO2_umol_m2~Collar5_1_short$Rec_No))$coefficients[2])
R5_1<-summary(lm(Collar5_1_short$CO2_umol_m2~Collar5_1_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 5_2
Collar5_2<-subset(data, subset = data$Plot_No=="5_2")
dim(Collar5_2)

plot(Collar5_2$Rec_No, Collar5_2$CO2)

Collar5_2_short <- Collar5_2[c(10:60), ]
plot(Collar5_2_short$Rec_No, Collar5_2_short$CO2)
Collar5_2_short[,"Rec_No"]
Collar5_2_short$Rec_No


##### n = RT/PV
n = ((Collar5_2$Pressure[1]/1000)*2)/((0.08314)*(Collar5_2$Tair[1]+273.15))
Collar5_2_short$CO2_umol_m2 <- (Collar5_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar5_2_short$Rec_No,Collar5_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 5_2")
#lm(y~x)
fit1<-lm(Collar5_2_short$CO2_umol_m2~Collar5_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_5_2<-(summary(lm(Collar5_2_short$CO2_umol_m2~Collar5_2_short$Rec_No))$coefficients[2])
R5_2<-summary(lm(Collar5_2_short$CO2_umol_m2~Collar5_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 6_1
Collar6_1<-subset(data, subset = data$Plot_No=="6_1")
dim(Collar6_1)

plot(Collar6_1$Rec_No, Collar6_1$CO2)

Collar6_1_short <- Collar6_1[c(9:60), ]
plot(Collar6_1_short$Rec_No, Collar6_1_short$CO2)
Collar6_1_short[,"Rec_No"]
Collar6_1_short$Rec_No


##### n = RT/PV
n = ((Collar6_1$Pressure[1]/1000)*2)/((0.08314)*(Collar6_1$Tair[1]+273.15))
Collar6_1_short$CO2_umol_m2 <- (Collar6_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar6_1_short$Rec_No,Collar6_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 6_1")
#lm(y~x)
fit1<-lm(Collar6_1_short$CO2_umol_m2~Collar6_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_6_1<-(summary(lm(Collar6_1_short$CO2_umol_m2~Collar6_1_short$Rec_No))$coefficients[2])
R6_1<-summary(lm(Collar6_1_short$CO2_umol_m2~Collar6_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 6_2
Collar6_2<-subset(data, subset = data$Plot_No=="6_2")
dim(Collar6_2)

plot(Collar6_2$Rec_No, Collar6_2$CO2)

Collar6_2_short <- Collar6_2[c(10:60), ]
plot(Collar6_2_short$Rec_No, Collar6_2_short$CO2)
Collar6_2_short[,"Rec_No"]
Collar6_2_short$Rec_No


##### n = RT/PV
n = ((Collar6_2$Pressure[1]/1000)*2)/((0.08314)*(Collar6_2$Tair[1]+273.15))
Collar6_2_short$CO2_umol_m2 <- (Collar6_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar6_2_short$Rec_No,Collar6_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 6_2")
#lm(y~x)
fit1<-lm(Collar6_2_short$CO2_umol_m2~Collar6_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_6_2<-(summary(lm(Collar6_2_short$CO2_umol_m2~Collar6_2_short$Rec_No))$coefficients[2])
R6_2<-summary(lm(Collar6_2_short$CO2_umol_m2~Collar6_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 7_1
Collar7_1<-subset(data, subset = data$Plot_No=="7_1")
dim(Collar7_1)

plot(Collar7_1$Rec_No, Collar7_1$CO2)

Collar7_1_short <- Collar7_1[c(8:60), ]
plot(Collar7_1_short$Rec_No, Collar7_1_short$CO2)
Collar7_1_short[,"Rec_No"]
Collar7_1_short$Rec_No


##### n = RT/PV
n = ((Collar7_1$Pressure[1]/1000)*2)/((0.08314)*(Collar7_1$Tair[1]+273.15))
Collar7_1_short$CO2_umol_m2 <- (Collar7_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar7_1_short$Rec_No,Collar7_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 7_1")
#lm(y~x)
fit1<-lm(Collar7_1_short$CO2_umol_m2~Collar7_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_7_1<-(summary(lm(Collar7_1_short$CO2_umol_m2~Collar7_1_short$Rec_No))$coefficients[2])
R7_1<-summary(lm(Collar7_1_short$CO2_umol_m2~Collar7_1_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 7_2
Collar7_2<-subset(data, subset = data$Plot_No=="7_2")
dim(Collar7_2)

plot(Collar7_2$Rec_No, Collar7_2$CO2)

Collar7_2_short <- Collar7_2[c(15:60), ]
plot(Collar7_2_short$Rec_No, Collar7_2_short$CO2)
Collar7_2_short[,"Rec_No"]
Collar7_2_short$Rec_No


##### n = RT/PV
n = ((Collar7_2$Pressure[1]/1000)*2)/((0.08314)*(Collar7_2$Tair[1]+273.15))
Collar7_2_short$CO2_umol_m2 <- (Collar7_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar7_2_short$Rec_No,Collar7_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 7_2")
#lm(y~x)
fit1<-lm(Collar7_2_short$CO2_umol_m2~Collar7_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_7_2<-(summary(lm(Collar7_2_short$CO2_umol_m2~Collar7_2_short$Rec_No))$coefficients[2])
R7_2<-summary(lm(Collar7_2_short$CO2_umol_m2~Collar7_2_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 8_1
Collar8_1<-subset(data, subset = data$Plot_No=="8_1")
dim(Collar8_1)

plot(Collar8_1$Rec_No, Collar8_1$CO2)

Collar8_1_short <- Collar8_1[c(8:60), ]
plot(Collar8_1_short$Rec_No, Collar8_1_short$CO2)
Collar8_1_short[,"Rec_No"]
Collar8_1_short$Rec_No


##### n = RT/PV
n = ((Collar8_1$Pressure[1]/1000)*2)/((0.08314)*(Collar8_1$Tair[1]+273.15))
Collar8_1_short$CO2_umol_m2 <- (Collar8_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar8_1_short$Rec_No,Collar8_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 8_1")
#lm(y~x)
fit1<-lm(Collar8_1_short$CO2_umol_m2~Collar8_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_8_1<-(summary(lm(Collar8_1_short$CO2_umol_m2~Collar8_1_short$Rec_No))$coefficients[2])
R8_1<-summary(lm(Collar8_1_short$CO2_umol_m2~Collar8_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 8_2
Collar8_2<-subset(data, subset = data$Plot_No=="8_2")
dim(Collar8_2)

plot(Collar8_2$Rec_No, Collar8_2$CO2)

Collar8_2_short <- Collar8_2[c(8:60), ]
plot(Collar8_2_short$Rec_No, Collar8_2_short$CO2)
Collar8_2_short[,"Rec_No"]
Collar8_2_short$Rec_No


##### n = RT/PV
n = ((Collar8_2$Pressure[1]/1000)*2)/((0.08314)*(Collar8_2$Tair[1]+273.15))
Collar8_2_short$CO2_umol_m2 <- (Collar8_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar8_2_short$Rec_No,Collar8_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 8_2")
#lm(y~x)
fit1<-lm(Collar8_2_short$CO2_umol_m2~Collar8_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_8_2<-(summary(lm(Collar8_2_short$CO2_umol_m2~Collar8_2_short$Rec_No))$coefficients[2])
R8_2<-summary(lm(Collar8_2_short$CO2_umol_m2~Collar8_2_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 9_1
Collar9_1<-subset(data, subset = data$Plot_No=="9_1")
dim(Collar9_1)

plot(Collar9_1$Rec_No, Collar9_1$CO2)

Collar9_1_short <- Collar9_1[c(8:60), ]
plot(Collar9_1_short$Rec_No, Collar9_1_short$CO2)
Collar9_1_short[,"Rec_No"]
Collar9_1_short$Rec_No


##### n = RT/PV
n = ((Collar9_1$Pressure[1]/1000)*2)/((0.08314)*(Collar9_1$Tair[1]+273.15))
Collar9_1_short$CO2_umol_m2 <- (Collar9_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar9_1_short$Rec_No,Collar9_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 9_1")
#lm(y~x)
fit1<-lm(Collar9_1_short$CO2_umol_m2~Collar9_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_9_1<-(summary(lm(Collar9_1_short$CO2_umol_m2~Collar9_1_short$Rec_No))$coefficients[2])
R9_1<-summary(lm(Collar9_1_short$CO2_umol_m2~Collar9_1_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 9_2
Collar9_2<-subset(data, subset = data$Plot_No=="9_2")
dim(Collar9_2)

plot(Collar9_2$Rec_No, Collar9_2$CO2)

Collar9_2_short <- Collar9_2[c(15:60), ]
plot(Collar9_2_short$Rec_No, Collar9_2_short$CO2)
Collar9_2_short[,"Rec_No"]
Collar9_2_short$Rec_No


##### n = RT/PV
n = ((Collar9_2$Pressure[1]/1000)*2)/((0.08314)*(Collar9_2$Tair[1]+273.15))
Collar9_2_short$CO2_umol_m2 <- (Collar9_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar9_2_short$Rec_No,Collar9_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 9_2")
#lm(y~x)
fit1<-lm(Collar9_2_short$CO2_umol_m2~Collar9_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_9_2<-(summary(lm(Collar9_2_short$CO2_umol_m2~Collar9_2_short$Rec_No))$coefficients[2])
R9_2<-summary(lm(Collar9_2_short$CO2_umol_m2~Collar9_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 10_1
Collar10_1<-subset(data, subset = data$Plot_No=="10_1")
dim(Collar10_1)

plot(Collar10_1$Rec_No, Collar10_1$CO2)

Collar10_1_short <- Collar10_1[c(8:60), ]
plot(Collar10_1_short$Rec_No, Collar10_1_short$CO2)
Collar10_1_short[,"Rec_No"]
Collar10_1_short$Rec_No


##### n = RT/PV
n = ((Collar10_1$Pressure[1]/1000)*2)/((0.08314)*(Collar10_1$Tair[1]+273.15))
Collar10_1_short$CO2_umol_m2 <- (Collar10_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar10_1_short$Rec_No,Collar10_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 10_1")
#lm(y~x)
fit1<-lm(Collar10_1_short$CO2_umol_m2~Collar10_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_10_1<-(summary(lm(Collar10_1_short$CO2_umol_m2~Collar10_1_short$Rec_No))$coefficients[2])
R10_1<-summary(lm(Collar10_1_short$CO2_umol_m2~Collar10_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 10_2
Collar10_2<-subset(data, subset = data$Plot_No=="10_2")
dim(Collar10_2)

plot(Collar10_2$Rec_No, Collar10_2$CO2)

Collar10_2_short <- Collar10_2[c(12:60), ]
plot(Collar10_2_short$Rec_No, Collar10_2_short$CO2)
Collar10_2_short[,"Rec_No"]
Collar10_2_short$Rec_No


##### n = RT/PV
n = ((Collar10_2$Pressure[1]/1000)*2)/((0.08314)*(Collar10_2$Tair[1]+273.15))
Collar10_2_short$CO2_umol_m2 <- (Collar10_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar10_2_short$Rec_No,Collar10_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 10_2")
#lm(y~x)
fit1<-lm(Collar10_2_short$CO2_umol_m2~Collar10_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_10_2<-(summary(lm(Collar10_2_short$CO2_umol_m2~Collar10_2_short$Rec_No))$coefficients[2])
R10_2<-summary(lm(Collar10_2_short$CO2_umol_m2~Collar10_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 11_1
Collar11_1<-subset(data, subset = data$Plot_No=="11_1")
dim(Collar11_1)

plot(Collar11_1$Rec_No, Collar11_1$CO2)

Collar11_1_short <- Collar11_1[c(8:60), ]
plot(Collar11_1_short$Rec_No, Collar11_1_short$CO2)
Collar11_1_short[,"Rec_No"]
Collar11_1_short$Rec_No


##### n = RT/PV
n = ((Collar11_1$Pressure[1]/1000)*2)/((0.08314)*(Collar11_1$Tair[1]+273.15))
Collar11_1_short$CO2_umol_m2 <- (Collar11_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar11_1_short$Rec_No,Collar11_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 11_1")
#lm(y~x)
fit1<-lm(Collar11_1_short$CO2_umol_m2~Collar11_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_11_1<-(summary(lm(Collar11_1_short$CO2_umol_m2~Collar11_1_short$Rec_No))$coefficients[2])
R11_1<-summary(lm(Collar11_1_short$CO2_umol_m2~Collar11_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 11_2
Collar11_2<-subset(data, subset = data$Plot_No=="11_2")
dim(Collar11_2)

plot(Collar11_2$Rec_No, Collar11_2$CO2)

Collar11_2_short <- Collar11_2[c(8:60), ]
plot(Collar11_2_short$Rec_No, Collar11_2_short$CO2)
Collar11_2_short[,"Rec_No"]
Collar11_2_short$Rec_No


##### n = RT/PV
n = ((Collar11_2$Pressure[1]/1000)*2)/((0.08314)*(Collar11_2$Tair[1]+273.15))
Collar11_2_short$CO2_umol_m2 <- (Collar11_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar11_2_short$Rec_No,Collar11_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 11_2")
#lm(y~x)
fit1<-lm(Collar11_2_short$CO2_umol_m2~Collar11_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_11_2<-(summary(lm(Collar11_2_short$CO2_umol_m2~Collar11_2_short$Rec_No))$coefficients[2])
R11_2<-summary(lm(Collar11_2_short$CO2_umol_m2~Collar11_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 12_1
Collar12_1<-subset(data, subset = data$Plot_No=="12_1")
dim(Collar12_1)

plot(Collar12_1$Rec_No, Collar12_1$CO2)

Collar12_1_short <- Collar12_1[c(8:60), ]
plot(Collar12_1_short$Rec_No, Collar12_1_short$CO2)
Collar12_1_short[,"Rec_No"]
Collar12_1_short$Rec_No


##### n = RT/PV
n = ((Collar12_1$Pressure[1]/1000)*2)/((0.08314)*(Collar12_1$Tair[1]+273.15))
Collar12_1_short$CO2_umol_m2 <- (Collar12_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar12_1_short$Rec_No,Collar12_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 12_1")
#lm(y~x)
fit1<-lm(Collar12_1_short$CO2_umol_m2~Collar12_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_12_1<-(summary(lm(Collar12_1_short$CO2_umol_m2~Collar12_1_short$Rec_No))$coefficients[2])
R12_1<-summary(lm(Collar12_1_short$CO2_umol_m2~Collar12_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 12_2
Collar12_2<-subset(data, subset = data$Plot_No=="12_2")
dim(Collar12_2)

plot(Collar12_2$Rec_No, Collar12_2$CO2)

Collar12_2_short <- Collar12_2[c(20:60), ]
plot(Collar12_2_short$Rec_No, Collar12_2_short$CO2)
Collar12_2_short[,"Rec_No"]
Collar12_2_short$Rec_No


##### n = RT/PV
n = ((Collar12_2$Pressure[1]/1000)*2)/((0.08314)*(Collar12_2$Tair[1]+273.15))
Collar12_2_short$CO2_umol_m2 <- (Collar12_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar12_2_short$Rec_No,Collar12_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 12_2")
#lm(y~x)
fit1<-lm(Collar12_2_short$CO2_umol_m2~Collar12_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_12_2<-(summary(lm(Collar12_2_short$CO2_umol_m2~Collar12_2_short$Rec_No))$coefficients[2])
R12_2<-summary(lm(Collar12_2_short$CO2_umol_m2~Collar12_2_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 13_1
Collar13_1<-subset(data, subset = data$Plot_No=="13_1")
dim(Collar13_1)

plot(Collar13_1$Rec_No, Collar13_1$CO2)

Collar13_1_short <- Collar13_1[c(8:60), ]
plot(Collar13_1_short$Rec_No, Collar13_1_short$CO2)
Collar13_1_short[,"Rec_No"]
Collar13_1_short$Rec_No


##### n = RT/PV
n = ((Collar13_1$Pressure[1]/1000)*2)/((0.08314)*(Collar13_1$Tair[1]+273.15))
Collar13_1_short$CO2_umol_m2 <- (Collar13_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar13_1_short$Rec_No,Collar13_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 13_1")
#lm(y~x)
fit1<-lm(Collar13_1_short$CO2_umol_m2~Collar13_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_13_1<-(summary(lm(Collar13_1_short$CO2_umol_m2~Collar13_1_short$Rec_No))$coefficients[2])
R13_1<-summary(lm(Collar13_1_short$CO2_umol_m2~Collar13_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 13_2
Collar13_2<-subset(data, subset = data$Plot_No=="13_2")
dim(Collar13_2)

plot(Collar13_2$Rec_No, Collar13_2$CO2)

Collar13_2_short <- Collar13_2[c(14:60), ]
plot(Collar13_2_short$Rec_No, Collar13_2_short$CO2)
Collar13_2_short[,"Rec_No"]
Collar13_2_short$Rec_No


##### n = RT/PV
n = ((Collar13_2$Pressure[1]/1000)*2)/((0.08314)*(Collar13_2$Tair[1]+273.15))
Collar13_2_short$CO2_umol_m2 <- (Collar13_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar13_2_short$Rec_No,Collar13_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 13_2")
#lm(y~x)
fit1<-lm(Collar13_2_short$CO2_umol_m2~Collar13_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_13_2<-(summary(lm(Collar13_2_short$CO2_umol_m2~Collar13_2_short$Rec_No))$coefficients[2])
R13_2<-summary(lm(Collar13_2_short$CO2_umol_m2~Collar13_2_short$Rec_No))$adj.r.squared


###########Flux calculation Collar 14_1
Collar14_1<-subset(data, subset = data$Plot_No=="14_1")
dim(Collar14_1)

plot(Collar14_1$Rec_No, Collar14_1$CO2)

Collar14_1_short <- Collar14_1[c(8:60), ]
plot(Collar14_1_short$Rec_No, Collar14_1_short$CO2)
Collar14_1_short[,"Rec_No"]
Collar14_1_short$Rec_No


##### n = RT/PV
n = ((Collar14_1$Pressure[1]/1000)*2)/((0.08314)*(Collar14_1$Tair[1]+273.15))
Collar14_1_short$CO2_umol_m2 <- (Collar14_1_short$CO2*n/0.008)

#plot(x,y)
plot(Collar14_1_short$Rec_No,Collar14_1_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 14_1")
#lm(y~x)
fit1<-lm(Collar14_1_short$CO2_umol_m2~Collar14_1_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_14_1<-(summary(lm(Collar14_1_short$CO2_umol_m2~Collar14_1_short$Rec_No))$coefficients[2])
R14_1<-summary(lm(Collar14_1_short$CO2_umol_m2~Collar14_1_short$Rec_No))$adj.r.squared

###########Flux calculation Collar 14_2
Collar14_2<-subset(data, subset = data$Plot_No=="14_2")
dim(Collar14_2)

plot(Collar14_2$Rec_No, Collar14_2$CO2)

Collar14_2_short <- Collar14_2[c(8:60), ]
plot(Collar14_2_short$Rec_No, Collar14_2_short$CO2)
Collar14_2_short[,"Rec_No"]
Collar14_2_short$Rec_No


##### n = RT/PV
n = ((Collar14_2$Pressure[1]/1000)*2)/((0.08314)*(Collar14_2$Tair[1]+273.15))
Collar14_2_short$CO2_umol_m2 <- (Collar14_2_short$CO2*n/0.008)

#plot(x,y)
plot(Collar14_2_short$Rec_No,Collar14_2_short$CO2_umol_m2, ylab="Co2 Flux umol/m2", 
     xlab = "Rec No", main = "Co2 Flux Collar 14_2")
#lm(y~x)
fit1<-lm(Collar14_2_short$CO2_umol_m2~Collar14_2_short$Rec_No)
abline(fit1, col="red")
summary(fit1)

slope_14_2<-(summary(lm(Collar14_2_short$CO2_umol_m2~Collar14_2_short$Rec_No))$coefficients[2])
R14_2<-summary(lm(Collar14_2_short$CO2_umol_m2~Collar14_2_short$Rec_No))$adj.r.squared



slopes<-c(slope_1_1, slope_1_2, slope_2_1, slope_2_2, slope_3_1, slope_3_2, 
          slope_4_1, slope_4_2, slope_5_1, slope_5_2, slope_6_1, slope_6_2,
          slope_7_1, slope_7_2, slope_8_1, slope_8_2, slope_9_1, slope_9_2, slope_10_1,
          slope_10_2, slope_11_1, slope_11_2, slope_12_1, slope_12_2, 
          slope_13_1, slope_13_2, slope_14_1, slope_14_2)
R2<-c(R1_1, R1_2, R2_1, R2_2, R3_1, R3_2, R4_1, R4_2, R5_1, R5_2, R6_1, R6_2, R7_1, 
      R7_2, R8_1, R8_2, R9_1, R9_2, R10_1, R10_2, R11_1, R11_2, R12_1, R12_2, 
      R13_1, R13_2, R14_1, R14_2)

collars<-c("collar1_1", "collar1_2", "collar2_1", "collar2_2", "collar3_1", "collar3_2", 
           "collar4_1", "collar4_2", "collar5_1", "collar5_2", "collar6_1", "collar6_2",
           "collar7_1", "collar7_2", "collar8_1", "collar8_2", "collar9_1", "collar9_2", "collar10_1",
           "collar10_2", "collar11_1", "collar11_2", "collar12_1", "collar12_2", 
           "collar13_1", "collar13_2", "collar14_1","collar14_2")

CollarNumber<-c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14)
Replicate<-rep(c(1,2),14)


new.data<-as.matrix(cbind(collars, slopes, R2, CollarNumber, Replicate))
write.csv(new.data,"20210615_SummaryRespirationData.csv")

flux<-read.csv("20210615_SummaryRespirationData.csv")
head(flux)










