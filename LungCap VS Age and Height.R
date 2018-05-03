library(readxl)
LungCapData <- read_excel("LungCapData.xls")
attach(LungCapData)
LungCapData

#checking variable relations
#independent-Age,Height   Dependent-LungCap 

par(mfrow=c(1,2))
plot(LungCap~Age,data=LungCapData,main="Scatterplot of LungCap vs Age")
abline(lm(LungCap~Age,data=LungCapData))
plot(LungCap~Height,data=LungCapData,main="Scatterplot of LungCap vs Height")
abline(lm(LungCap~Height,data=LungCapData))

#checking independent variables relations
plot(Age~Height,data=LungCapData,main="Scatterplot of Age vs Height")
abline(lm(Age~Height,data=LungCapData))

#Minimizing data columns

data<-LungCapData[1:725,1:3]
data

#check the correlation matrix

cor(data,use="complete.obs")

#Building Multiple regression Model
summary(lm(LungCap~Age,data=LungCapData))
summary(lm(LungCap~Height,data=LungCapData))

summary(lm(LungCap~Age+Height,data=LungCapData))

#Selecting Best Model Based on the AIC value
null=lm(LungCap~1,data=LungCapData)
full=lm(LungCap~.,data=LungCapData)
step(null,scope=list(lower=null,upper=full),direction="forward")
step(full,direction="backward")
step(null,scope=list(upper=full),data=LungCapData,direction="both")

#Check the Normality and Randomness of the residuals
qqnorm(resid(lm(LungCap~Age+Height,data=LungCapData)))
qqline(resid(lm(LungCap~Age+Height,data=LungCapData)))

plot(fitted(lm(LungCap~Age+Height,data=LungCapData)),resid(lm(LungCap~Age+Height,data=LungCapData)),main="Randomness of residuals")
