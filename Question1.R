           #Control L ~ Clear Console
           #rm(x,r,z)

#Install Package
library(ggplot2)


Data <- read.csv("~/Desktop/Taiwan/3rd Semester/GLM/Canada_Fuel_Consumption_Rating.csv")
#removing "NA" data
Data <- na.omit(Data)


#Train_Test_Dataset
i <- nrow(Data) - 20
Data_Train <- Data[1: i, ]     #Extract first i obs
Data_Test <- tail(Data, n = 20)   #Extract last 20 obs
rm(i)

#Extract Variable
Engine <- Data_Train$ENGINE_SIZE
Cylinders <- Data_Train$CYLINDERS
Fuel <- Data_Train$FUEL_TYPE
City <- Data_Train$CITY
HWY  <- Data_Train$HWY
COMB <- Data_Train$COMB
COMB_mpg   <- Data_Train$COMB_mpg
CO2_Emission   <- Data_Train$CO2_EMISSIONS


#Pre-processing
func <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  b <- boxplot(x, plot = FALSE)
  x[ x %in% b$out]  <- quantiles[2]
  x
}

par(mfrow = c(3,3))  
boxplot(Engine, main = "Engine", col = "steelblue3")
boxplot(City, main = "C.City", col = "steelblue3")
boxplot(CO2_Emission, main = "CO2_Emission", col = "steelblue3")
boxplot(COMB,main = "C.COMB", col = "steelblue3")
boxplot(Cylinders, main = "Cylinders", col = "steelblue3")
boxplot(COMB_mpg, main = "C.COMB_mpg", col = "steelblue3")
boxplot(HWY, main = "C.HWY", col = "steelblue3")

City <- func(City)
CO2_Emission <- func(CO2_Emission)
COMB <- func(COMB)
Cylinders <- func(Cylinders)
COMB_mpg <- func(COMB_mpg)
HWY <- func(HWY)


par(mfrow = c(3,3))  
boxplot(Engine, main = "Engine", col = "steelblue3")
boxplot(City, main = "C.City", col = "steelblue3")
boxplot(CO2_Emission, main = "CO2_Emission", col = "steelblue3")
boxplot(COMB, main = "C.COMB", col = "steelblue3")
boxplot(Cylinders, main = "Cylinders", col = "steelblue3")
boxplot(COMB_mpg, main = "C.COMB_mpg", col = "steelblue3")
boxplot(HWY, main = "C.HWY", col = "steelblue3")

#Descriptive Statistic
cor(Data_Train[-3])
summary(CO2_Emission); 
sd(CO2_Emission)

#Plot
par(mfrow = c(1,1))
plot(CO2_Emission,col = 'palevioletred')
pairs(formula = CO2_Emission~Engine +
        Cylinders +
        City +
        HWY +
        COMB +
        COMB_mpg,
      col = 'slategrey')


#Regression
a <- lm(CO2_Emission ~ Engine  + Cylinders + COMB + Fuel, 
        data = Data_Train)
summary(a)
anova(a)

#Diagnostic
par(mfrow = c(1,1))
ggplot(a, aes( a$residuals, a$fitted.values)) + geom_point(col='steelblue3') + xlab("Fitted") + ylab("Residuals")

qqnorm(a$residuals, col = 'paleturquoise4')
qqline(a$residuals, col = 'sienna1')  

#SSE
sse <- sum( (Data$CO2_EMISSIONS - fitted(a))^2)
sse
#find ssr
ssr <- sum((fitted(a) - mean(Data$CO2_EMISSIONS))^2)
ssr
# find SST
sst <- sse + ssr
sst
#Accurancy

