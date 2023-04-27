#We can choose the csv file which we are interested to work with.
z=read.csv(file.choose())

#Extracting the income from the file.
x=z$Income

#Converting the data into time series data.
xt=ts(x,frequency=12,start=c(2000,1))

#Plotting the time profile to observe if there is any time series components.
plot.ts(xt,main="Plot of original series")
cat("\ From the graph we can observe that trend is present in the given data and hence the series is not stationary \n")

#We are using variance differencing method to make the series stationary.
v=var(xt)
cat("\n Variance of original series \n")
print(v)
d1=diff(xt)
v1=var(d1)
cat("\n Variance of first differenced series \n")
print(v1)
if(v<v1)
cat("\n Original series is stationary \n")else
cat("\n Original series is not stationary \n")
d2=diff(d1)
v2=var(d2)
cat("\n Variance of second differenced series \n")
print(v2)
if(v1<v2)
cat("\n First differenced series is stationary \n")else
cat("\n First differenced series is not stationary \n")
cat("\n Since variance of first differenced series is less than variance of second differenced series, we consider first differenced series as stationary series \n")

#At which point the series becomes stationary, that differenced series order is considered as order of d.
d=1

#Finding length of the series.
n=length(xt)

#Finding the lag length.
k=round(2*sqrt(n))

#Plotting ACF and PACF for the stationary series
par(mfrow=c(1,2))
acf(d1,main="ACF of stationary series")
pacf(d1,main="PACF of stationary series")

#From ACF plot we can observe that first 3 lags are significant. Hence value of q is 3.
q1=3

#From PACF plot we can observe that first 2 lags are significant. Hence value of p is 2.
p1=2

#Fitting ARIMA model.
A1=arima(xt,order=c(p1,d,q1))

#Extracting residuals from the fitted model.
e1=A1$residual

#Applying Ljung-Box test to test the serial uncorrelation of the residuals.
LB1=Box.test(e1,lag=k,type="Ljung-Box")

#Applying Shapiro-Wilk test to test the normality of the residuals.
SW1=shapiro.test(e1)

#AKAIKE Information criteria.
aic1=A1$aic

#To identify the appropriate model for the given data we fit model for different combinations of p and q keeping d constant. For each combinatons we apply uncorrelaton test, normality test and AIC.
q2=1
p2=1
A2=arima(xt,order=c(p2,d,q2))
e2=A2$residual
LB2=Box.test(e2,lag=k,type="Ljung-Box")
SW2=shapiro.test(e2)
aic2=A2$aic

q3=2
p3=2
A3=arima(xt,order=c(p3,d,q3))
e3=A3$residual
LB3=Box.test(e3,lag=k,type="Ljung-Box")
SW3=shapiro.test(e3)
aic3=A3$aic

q4=1
p4=0
A4=arima(xt,order=c(p4,d,q4))
e4=A4$residual
LB4=Box.test(e4,lag=k,type="Ljung-Box")
SW4=shapiro.test(e4)
aic4=A4$aic

q5=2
p5=1
A5=arima(xt,order=c(p5,d,q5))
e5=A5$residual
LB5=Box.test(e5,lag=k,type="Ljung-Box")
SW5=shapiro.test(e5)
aic5=A5$aic

#Creating a table for values of Ljung-Box, Shapiro-Wilk and AIC for different combinations of ARIMA(p,d,q) model
Model=c(1,2,3,4,5)
P=c(p1,p2,p3,p4,p5)
Q=c(q1,q2,q3,q4,q5)
D=rep(1,5)
LB=c(round(LB1$p.value,4),round(LB2$p.value,4),round(LB3$p.value,4),round(LB4$p.value,4),round(LB5$p.value,4))
SW=c(SW1$p.value,SW2$p.value,SW3$p.value,SW4$p.value,SW5$p.value)
AIC=c(aic1,aic2,aic3,aic4,aic5)

#Appropriate model is choosed based on maximun p-values of Ljung-Box and Shapiro-Wilk tests and for minimun value of AIC.
T=data.frame(Model,P,D,Q,LB,SW,AIC)
cat("\n Table for values of Ljung-Box, Shapiro-Wilk and AIC for different combinations of ARIMA(p,d,q) model \n")
print(T)
cat("\n From the table we can observe that ARIMA(2,1,3) model has maximum p-value for Ljung-Box, Shapiro-Wilk test and it has minimum AIC than other model, hence we consider this model as good fit for the data \n")
 

#Based on the appropriate model choosed we predict the future income of the company.
PR=predict(A1,n.ahead=10)
Pre=PR$pred
cat("\n Predicted Income of the company  for 2019 \n")
print(Pre)