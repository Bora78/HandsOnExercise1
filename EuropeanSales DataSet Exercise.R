#################################################################
## Marketing analytics                                         ##
## Lecture 2 Exercise in Market Response Models				         ##
## EuropeanSales Exercise                                      ##
##                                                             ## 
#################################################################

# free memory if need be
#rm(list = ls())
#gc()

# Get the working directory. If needed, you can set the working directory to another folder.
getwd()
#setwd("C:\")


# Read the Data files from a directory  
EuropeanSalesDataFrame <-read.csv("C:/Users/Bora/Desktop/EuropeanSales.csv", header=T)

#Show attributes  
attributes(EuropeanSalesDataFrame)

# draw Yearly sales
plot(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$SalesPerCapita, ylab="GDPperHead", xlab="SalesPerCapita")
plot(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$ComputerSales, ylab="GDPperHead", xlab="ComputerSales")
plot(EuropeanSalesDataFrame) 

#Correlation between attributes
cor(EuropeanSalesDataFrame$UnemploymentRate,EuropeanSalesData$SalesPerCapita)
cor(EuropeanSalesDataFrame$EducationSpending,EuropeanSalesData$SalesPerCapita) 
cor(EuropeanSalesDataFrame$GDPperHead,EuropeanSalesData$SalesPerCapita)
cor(EuropeanSalesDataFrame$Population,EuropeanSalesData$SalesPerCapita)

cor(EuropeanSalesDataFrame$UnemploymentRate,EuropeanSalesDataFrame$ComputerSales)
cor(EuropeanSalesDataFrame$EducationSpending,EuropeanSalesDataFrame$ComputerSales) 
cor(EuropeanSalesDataFrame$GDPperHead,EuropeanSalesDataFrame$ComputerSales)
cor(EuropeanSalesDataFrame$Population,EuropeanSalesDataFrame$ComputerSales)



#Correlation for all attributes
cor(EuropeanSalesDataFrame[,])

# Fit the Data to the model
mymodel <- lm(SalesPerCapita ~ EducationSpending + GDPperHead, data=EuropeanSalesDataFrame)
summary(mymodel)

#Model attributes and coefficients  
attributes(mymodel)
mymodel$coefficients

#Find the SalesPerCapita for EducationSpending 8 and GDPperHead 50 
SalespcapitaED8GDP50 <- mymodel$coefficients[[1]]+mymodel$coefficients[[2]]*8+mymodel$coefficients[[3]]*50


#SalespcapitaED10GDP50000 = 200.4201
SalespcapitaED8GDP50

#A simpler Model without GDPperHead
model <- lm(SalesPerCapita ~ EducationSpending, data=EuropeanSalesDataFrame)
summary(model)


# differences between observed values and fitted values
residuals(model) 


# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(model)

# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(model)

# Compare Different Models
model1 <- lm(SalesPerCapita ~ EducationSpending + GDPperHead, data=EuropeanSalesDataFrame)
summary(model1)


model2 <- lm(SalesPerCapita ~ EducationSpending , data=EuropeanSalesDataFrame)
summary(model2)

# Fit the Data to the model
model <- lm(ComputerSales ~ Population, data=EuropeanSalesDataFrame)
summary(model)

#Model attributes and coefficients  
attributes(model)
model$coefficients


# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(model)


#Find the ComputerSales for Population 40  
ComputerSalesPop40 <- model$coefficients[[1]]+model$coefficients[[2]]*40


#ComputerSalesPop40= 3519.062
ComputerSalesPop40
