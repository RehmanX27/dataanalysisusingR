setwd("F:/pgdm/Term 4/DAR/R exam test/Mid term")
getwd()

install.packages("readxl") #The package is for data import-excel
library(readxl)

#Packages for regression analysis
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)
install.packages("lm.beta")
library(lm.beta)
install.packages("Hmisc")
library(Hmisc)
install.packages("psych")
library(psych)

#Data Import
House_rent=read_excel(file.choose(), sheet ="House_Rent_Dataset")
attach(House_rent)
names(House_rent)
str(House_rent)
fix(House_rent)
summary(House_rent)

#Dummy variables
House_rent$D_Carpetarea=ifelse(House_rent$`Area Type`=="Carpet Area",1,0)
House_rent$D_Superarea=ifelse(House_rent$`Area Type`=="Super Area",1,0)
House_rent$D_Hyderabad=ifelse(House_rent$City=="Hyderabad",1,0)
House_rent$D_Chennai=ifelse(House_rent$City=="Chennai",1,0)
House_rent$D_Delhi=ifelse(House_rent$City=="Delhi",1,0)
House_rent$D_Mumbai=ifelse(House_rent$City=="Mumbai",1,0)
House_rent$D_Kolkata=ifelse(House_rent$City=="Kolkata",1,0)
House_rent$D_Furnished=ifelse(House_rent$`Furnishing Status`=="Furnished",1,0)
House_rent$D_Semifurnished=ifelse(House_rent$`Furnishing Status`=="Semi-Furnished",1,0)
House_rent$D_Bachelors=ifelse(House_rent$`Tenant Preferred`=="Bachelors",1,0)
House_rent$D_Family=ifelse(House_rent$`Tenant Preferred`=="Family",1,0)
House_rent$D_Agent=ifelse(House_rent$`Point of Contact`=="Contact Agent",1,0)
House_rent$D_Owner=ifelse(House_rent$`Point of Contact`=="Contact Owner",1,0)

#Model building
House_rent_reg=lm(Rent~BHK+Size+Bathroom+D_Carpetarea+D_Superarea+D_Hyderabad+D_Chennai+D_Delhi+D_Mumbai+D_Kolkata+D_Furnished+D_Semifurnished+D_Bachelors+D_Family+D_Agent+D_Owner,data=House_rent)
summary(House_rent_reg)

#Rebuilding the model
House_rent_reg1=lm(Rent~Size+Bathroom+D_Hyderabad+D_Chennai+D_Delhi+D_Mumbai+D_Furnished+D_Family,data=House_rent)
summary(House_rent_reg1)

#Rebuilding the model
House_rent_reg2=lm(Rent~Size+Bathroom+D_Mumbai+D_Delhi+D_Hyderabad+D_Furnished,data=House_rent)
summary(House_rent_reg2)

#Assumptions testing
#1.average residul is zero
mean(House_rent_reg2$residuals)
#2.Constant variance
#H0: Constant variance assumption is satisfied
#H1: Constant variance assumption is not-satisfied
bptest(House_rent_reg2)
#The assumption of constant variance is not satisfied
#3.Uncorrelated errors
#H0: Uncorrelated errors
#H1: Correlated errors
durbinWatsonTest(House_rent_reg2) # Durbin watson test
#4.Residuals are normally distributed
#H0: Residuals are normally distributed
#H1: Residuals are non-normally distributed
shapiro.test(House_rent_reg2$residuals)
#5. Regressors are statistically independent
vif(House_rent_reg2)# VIF stands for variance inflation factor. It used for checking the 


# The model cannot be used as the assumptions are not satisfied
# Hence, we need to get into diagnosis
# Diagnosis can be done in three ways
#1. Using the right transformations
#2. Using alternative regression models
#3. Checking for leverage values, influential observations, or outliers


library(VGAM)
library(sandwich)
library(nortest)
library(bestNormalize)
library(fitdistrplus)
library(pracma)
library(MLmetrics)


#1. Using the right transformations
names(House_rent)
House_rent$log_Rent=log(House_rent$Rent,exp(1))

library(VGAM)
library(sandwich)
library(nortest)
library(bestNormalize)
library(fitdistrplus)
library(pracma)
library(MLmetrics)

#Transformations
#1. Regular Transformations
#2. Box-Cox transformation
#3. Yeo-Johnson transformation
#Box-Cox Transformation
bc_Houserent=boxCox(House_rent_reg2)
bc_Houserent
lambda_Houserent=bc_Houserent$x[which.max(bc_Houserent$y)]
lambda_Houserent
House_rent$bc_Houserent=(Rent^lambda_Houserent-1)/lambda_Houserent
House_rent_reg2_bc=lm(bc_Houserent~Size+Bathroom+D_Chennai+D_Delhi+D_Hyderabad+D_Mumbai+D_Furnished,data=House_rent)
summary(House_rent_reg2)
avPlots(House_rent_reg2)
shapiro.test(House_rent_reg$residuals)
qqnorm(House_rent_reg2$residuals)
qqline(House_rent_reg2$residuals)

#Yeo Johnson Transformation
yeojohnson(nthroot(House_rent$Rent,1))
yeo_conv_Houserent=yeo.johnson(nthroot(House_rent$Rent,1),-0.3432769)
hist(House_rent$Rent)
hist(yeo_conv_Houserent)
names(House_rent)

#rebuilding the model
House_rent_reg2=lm(yeo_conv_Houserent~Size+Bathroom+D_Chennai+D_Delhi+D_Hyderabad+D_Mumbai+D_Furnished,data=House_rent)
summary(House_rent_reg2)

#Leverage points
hatvalues(House_rent_reg2)
plot(hatvalues(House_rent_reg2), type='h')
dim(House_rent)
#hat value>2*(p/n), p is number of regression coefficients, n is the sample size
p=length(House_rent_reg2$coefficients)
p
dim(House_rent)
n=dim(House_rent)[1]
n
2*(p/n)
#p=k+1
#(2k+2)/n
Lev_Houserent=which(hatvalues(House_rent_reg2)>2*(p/n))
Lev_Houserent
length(Lev_Houserent)
#Selecting the leverage data from the original data set
View(House_rent
     [which(hatvalues(House_rent_reg2)>2*(p/n)),])

#Leverage plots
leveragePlots(House_rent_reg2,layout=c(2,2))


#Influential Observations
cooks.distance(House_rent_reg2)
as.vector(cooks.distance(House_rent_reg2))
cook_pelicon=round(cooks.distance(House_rent_reg2),4)
cooks.distance(House_rent_reg2)[which.max(cooks.distance(House_rent_reg2))]
plot(House_rent_reg2,which=4)

plot(cooks.distance(House_rent_reg2),type="b",pch=18,col="blue")
N = 4746
k = 10
cutoff = 4/ (N-k-1)
cutoff
abline(h=cutoff,lty=2)
House_rent[which(hatvalues(House_rent_reg2)>2*(p/n)),]
Houserent_inf=House_rent[which(cooks.distance(House_rent_reg2)>(4/(N-k-1))),]
View(Houserent_inf)
library(broom)
View(augment(House_rent_reg2))

# Outliers
qqPlot(House_rent_reg2, labels=row.names(City), simulate = TRUE
       ,main="QQ Plot")
outlierTest(House_rent_reg2)
fitted(House_rent_reg2)[4077]

library(VGAM)
library(bestNormalize)
library(pracma)
library(MLmetrics)
install.packages("InformationValue")
library(InformationValue)
library(sandwich)

#Generalized least squares estimator
install.packages("nlme")
library(nlme)
House_rent_reg2_gls=gls(Rent~Size+Bathroom+D_Chennai+D_Delhi+D_Hyderabad+D_Mumbai+D_Furnished,data=House_rent)
summary(House_rent_reg2_gls)$sigma

#Robust regression
install.packages("MASS")
library(MASS)
House_rent_robust_reg=rlm(Rent~Size+Bathroom+D_Chennai+D_Delhi+D_Hyderabad+D_Mumbai+D_Furnished,data=House_rent,method="MM")
summary(House_rent_robust_reg)
coeftest(House_rent_robust_reg,vcov. = vcovHC(House_rent_robust_reg,"HC1"))
summary(House_rent_robust_reg)$sigma
confint.default(House_rent_robust_reg)
install.packages("robustbase")
library(robustbase)

Houserent_rob=lmrob(Rent~Size+Bathroom+D_Chennai+D_Delhi+D_Hyderabad+D_Mumbai+D_Furnished,data=House_rent,method="MM")
summary(Houserent_rob)

confint(Houserent_rob)
confint.default(Houserent_rob)

RMSE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100
100-RMSE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100

MAE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100
100-MAE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100

RMSLE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100    
100-RMSLE(Houserent_rob$fitted.values,Rent)/mean(Rent)*100   
