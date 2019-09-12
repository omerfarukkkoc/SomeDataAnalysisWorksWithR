#Ömer Faruk KOÇ
#23.10.2018


ViewAndSummary <- function(dataset) {
  View(dataset) 
  summary(dataset)
}

BoxPlott <- function(){
  par(mfrow=c(1,9)); 
  boxplot(Facebook$Total.Reach, col = c("red"), xlab="Total Reach") 
  boxplot(Facebook$Total.Impressions, col = c("blue"), xlab="Total Impressions") 
  boxplot(Facebook$Engaged.Users, col = c("green"), xlab="Engaged Users") 
  boxplot(Facebook$Before.Post, col = c("red"), xlab="Before Post") 
  boxplot(Facebook$After.Post, col = c("blue"), xlab="After Post") 
  boxplot(Facebook$comment, col = c("green"), xlab="Comment") 
  boxplot(Facebook$like, col = c("red"), xlab="Like") 
  boxplot(Facebook$share, col = c("Blue"), xlab="Share") 
  boxplot(Facebook$Total.Interactions, col = c("Green"), xlab="Total Interactions") 
  par(mfrow=c(1,1))
}

NormalityTest <- function(feature){
  result <- shapiro.test(feature)
  if (result$p.value > 0.05){
    cat("p value",result$p.value," > 0.05 feature is normally distributed")
  }else{
    cat("p value",result$p.value," < 0.05 feature is not normally distributed")
  }
  x <- feature;h<-hist(x,breaks=10, col="blue", xlab="Feature", main="Histogram")
  xfit<-seq(min(x),max(x),length=40);yfit<-dnorm(xfit,mean=mean(x),sd=sd(x));yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, type="l", col="red", lwd=2)
}

library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("psych")



Facebook <- read.csv("C:/Users/omerfarukkoc/Desktop/Facebook.csv", sep=";")
ViewAndSummary(Facebook)

psych::describeBy(Facebook)

BoxPlott()

plot(Facebook$Total.Reach, Facebook$Total.Impressions, type="p", main="ScatterPlot", xlab="Total Reach", ylab="Total Impressions")
#lines(Facebook$Total.Reach, Facebook$Total.Impressions, type="l", col="red", lwd=2)


########## Missing Values
sum(is.na(Facebook))
apply(Facebook, 2, function(x)sum(is.na(x)))


LikeMean = round(mean(Facebook[!is.na(Facebook$like),]$like),0); LikeMean
LikeIndex <- which(is.na(Facebook$like)); LikeIndex
Facebook$like[LikeIndex] = LikeMean


TotalInteractionsMean = round(mean(Facebook[!is.na(Facebook$Total.Interactions),]$Total.Interactions),0); TotalInteractionsMean
TotalInteractionsIndex <- which(is.na(Facebook$Total.Interactions)); TotalInteractionsIndex
Facebook$Total.Interactions[TotalInteractionsIndex] = TotalInteractionsMean

sum(is.na(Facebook))
apply(Facebook, 2, function(x)sum(is.na(x)))



#########Normality Test
NormalityTest(Facebook$Total.Reach)
NormalityTest(Facebook$Total.Impressions)
NormalityTest(Facebook$Engaged.Users)
NormalityTest(Facebook$Before.Post)
NormalityTest(Facebook$After.Post)
NormalityTest(Facebook$comment)
NormalityTest(Facebook$like)
NormalityTest(Facebook$share)
NormalityTest(Facebook$Total.Interactions)




########### Factor Analysis
cor(Facebook, method = c("pearson"))
install.packages("REdaS")
REdaS::KMOS(Facebook)
bartlett.test(Facebook)
REdaS::bart_spher(Facebook)
screeplot(princomp(Facebook, scores = TRUE, cor = TRUE), type="line", main = "Scree Plot")
factanal(Facebook,factors = 3, rotation = "varimax")




NormalityTest(fuelConsumptionData$hp)
hpOutlierValueIndex <- which.max(fuelConsumptionData$hp); max(fuelConsumptionData$hp)
print(fuelConsumptionData[hpOutlierValueIndex,c(1,2,3,4,5)])
fuelConsumptionData <- fuelConsumptionData[-hpOutlierValueIndex,];ViewAndSummary(fuelConsumptionData); BoxPlott()

NormalityTest(fuelConsumptionData$mpg)
mpgOutlierValueIndex <- which.max(fuelConsumptionData$mpg); max(fuelConsumptionData$mpg)
print(fuelConsumptionData[mpgOutlierValueIndex,c(1,2,3,4,5)])
fuelConsumptionData <- fuelConsumptionData[-mpgOutlierValueIndex,]; ViewAndSummary(fuelConsumptionData);BoxPlott()


NormalityTest(fuelConsumptionData$hp)
NormalityTest(fuelConsumptionData$mpg)


plot(fuelConsumptionData$hp, fuelConsumptionData$mpg, xlab ="HP", ylab ="MPG", main="Cars", col="blue", pch=19, cex=1) 

NormalityTest(Facebook$Total.Reach)


transform(table(Facebook$Total.Reach))
