


the.data <- as.matrix(read.csv("LZIsData.csv", header = FALSE, sep = ","))
my.data <- the.data [sample(1:4464,1500),c(1:4)]
write.table(my.data,"Muhammad-Ahsan-LzMyData.txt")


#Q.1

#1.1)

#Air Temperature Histogram
hist(my.data[,1], main = 'Air Temperature Frequency Chart',xlab = 'Air Temperature')

#Air Pressure
hist(my.data[,4],main = 'Air Pressure Frequency Chart',xlab = 'Air Pressure')

#1.2)

#parallel box plots in r

temp = c('Air Temperature','Windspeed')
boxplot(my.data[,1],my.data[,3],names = temp, main = 'Air Temperature & Wind Speed', ylab = 'Degree')

#summary measure of Air Temperature
summary(my.data[,1])

#Summary measure of Wind Speed
summary(my.data[,3])

#1.3)

#Hist of 'Humidity'
hist(my.data[,2],main = 'Histogram of Humidity',xlab = 'Humidity')

#Summary measure of Humidity
summary(my.data[,2]) #Decide which summary measure is best

#1.4)

#Creating two variables
Air_1000 <- my.data[1:1000,1]
Humid_1000 <- my.data[1:1000,2]

#Fitting a liner regression model
reg1 <- lm(Humid_1000~Air_1000)

#Getting the summary statistics of the model
summary(reg1)

#Plotting the scatter plot
plot(Air_1000,Humid_1000,main = 'Scatter plot of Air Temp vs humidity',xlab = 'Air Temperature',ylab = 'Humidity')
#Fitting the regression line
abline(reg1)

#Findinf the correlation between two variables
cor(Air_1000,Humid_1000)

############################################################################################################
#Q.5)

#d)

library(Bolstad)

# mu = 5 and s.d = 0.25 as mentioned in the question

mu = seq(4,6,by = 0.05)

mu.prior = rep(0,length(mu))
mu.prior[mu <= 4.75 ] = -16/3 + (4*mu[mu<=4.75])/3
mu.prior[mu > 4.75] = 24/5 + (-4*mu[mu > 4.75])/5

results = normgcp(5,0.25,density = "user",mu = mu, mu.prior = mu.prior)
plot(results,overlay = TRUE,which = 1:3)




############################################################################################################

#Q.6

#6.1

zz <- read.table("IOCdata2020.txt")
zz <- as.matrix(zz)

#a)
#scatterplot of zz
plot(zz, main = "Scatter Pot of IOC Data")

#b) refer to document

#c)
cl <- kmeans(zz,4,nstart = 25)
plot(zz,col = cl$cluster,main = "Scatter Pot of IOC Data")

#d)
totwss = array(,c(20,1))
for (i in 2:20) { print(i) 
  totwss[i,1] = (kmeans(zz,centers = i))$tot.withins }
plot(totwss,xlab = "Number of clusters ",ylab = "Total within Sum of Squares",main = "Total within sum of squares (totwss) with different k value")

#6.2

#Spectral Clustering 
library(kernlab)
spec <- specc(zz,centers = 3)
plot(zz,col = spec,main = 'Spectral Clustering')

############################################################################################################

#Q.7

library(mixtools)
library(MASS)
library(bbmle)
library(TSstudio)

WTempdata <- as.matrix(read.csv("HeronIslandWaterTemp.csv", header = TRUE, sep = ","))

#7.1

plot.ts(WTempdata,main='Time Series plot Water Temperature')

#7.2

hist(WTempdata,main = 'Histogram of Water Temperature',xlab = 'Water Temperature')


#7.3)

#Fitting 

fitFunc=fitdistr(WTempdata, densfun = "normal")
fitFunc

hist(WTempdata,pch=20,breaks = 25,prob=TRUE,
     main="Water Temperature",
     xlab="Temperature",
     col="lightblue")

curve(dnorm(x,fitFunc$estimate[1],fitFunc$estimate[2]),col="red", lwd=2,add=T)


#7.4

mixmdl = normalmixEM(WTempdata) #default k=2 components
mixmdl
summary(mixmdl)


#7.5

plot(mixmdl,which=2)
lines(density(WTempdata), lty=2, lwd=2)

#7.6

mixmdl$lambda
mixmdl$mu
mixmdl$sigma
plot(mixmdl$all.loglik)

