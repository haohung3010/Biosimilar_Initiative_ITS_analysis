library("nlme")
library("car")

View(IJD_total_cost_ITS)
View(ISD_total_cost_ITS)
View(IBD_total_cost_ITS)

#write.csv(IJD_total_cost_ITS, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/IJD_total_cost.csv", row.names = FALSE)
#write.csv(ISD_total_cost_ITS, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/ISD_total_cost.csv", row.names = FALSE)
#write.csv(IBD_total_cost_ITS, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/IBD_total_cost.csv", row.names = FALSE)

total_IJD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/IJD_total_cost_ITS.csv")
total_IBD$date = as.Date(paste(total_IBD$year,total_IBD$month, "01", sep = "-"))

#total_ISD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/ISD_total_cost_ITS.csv")

total_IJD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/IJD_total_cost_ITS.csv")
total_IJD$date = as.Date(paste(total_IJD$year,total_IJD$month, "01", sep = "-"))

##########################################################################################


modeldat<-total_IBD[total_IBD$period != 3 & total_IBD$period  != 5,]

model_ols_total_IBD = lm(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, 
                         data = modeldat)
summary(model_ols_total_IBD)
dwt(model_ols_total_IBD, max.lag = 12, alternative = "two.sided")

par(mfrow = c(1,2))
acf(residuals(model_ols_total_IBD)) # p = 1
acf(residuals(model_ols_total_IBD), type = "partial") # not significant
par(mfrow = c(1,1))
# does not show AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# significant at p = 1

residuals = residuals(model_ols_total_IBD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# Residuals plot converged at the end
# residuals are not normally distributed (QQ plot)
# bell-shaped histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_total_IBD = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, 
                      data = modeldat, method = "ML")

model_total_IBD_q1 = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, 
                         data = modeldat,
                         correlation = corARMA(q=1, form = ~ Time),
                         method = "ML")
model_total_IBD_p1 = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, 
                         data = modeldat,
                         correlation = corARMA(p=1, form = ~ Time),
                         method = "ML")

anova(model_total_IBD, model_total_IBD_p1) 
anova(model_total_IBD, model_total_IBD_q1) 
#choose RA (1) model

final_model<- model_total_IBD_p1
summary(final_model)
confint(final_model)


y1<-fitted(final_model)[57]
y2<-fitted(final_model)[69]
x1<-modeldat$Time[57]
x2<-modeldat$Time[69]
slope1<-(y2-y1)/(x2-x1)
inter1<-fitted(final_model)[57]
newt<-seq(0,33,1)
newx<-total_IBD$date[63:96]
newy<-inter1+(slope1*newt)

length(newx)

plot(total_IBD$date, total_IBD$total_cost,
     ylim = c(0,1500),
     ylab = "IBD Total Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-09-05"), 0, as.Date("2020-03-05"), 1000000, col = "#00000011", border = NA)
lines(total_IBD$date, total_IBD$percent_bio, col = "lightblue")
abline(v=as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06")), lty=2)

lines(modeldat$date[1:56], fitted(final_model)[1:56], col = "blue", lwd = 2)
lines(modeldat$date[57:69], fitted(final_model)[57:69], col = "blue", lwd = 2)
lines(modeldat$date[70:84], fitted(final_model)[70:84], col = "blue", lwd = 2)
segments(as.Date("2020-03-01"), final_model$coef[1] + final_model$coef[2]*63,
         as.Date("2021-03-01"), final_model$coef[1] + final_model$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(newx[20],newy[20],newx[34],newy[34], lty = 1, col = "red", lwd = "2")
title("IBD Total Cost")


###########################################################################################
modeldat_IJD<-total_IJD[total_IJD$period != 3 & total_IJD$period  != 5,]

model_ols_total_IJD = lm(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, 
                         data = modeldat_IJD)
summary(model_ols_total_IJD)
dwt(model_ols_total_IJD, max.lag = 12, alternative = "two.sided") #no significant at lag 1-4

par(mfrow = c(1,2))
acf(residuals(model_ols_total_IJD)) 
acf(residuals(model_ols_total_IJD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_total_IJD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# Residuals plot converged at the end
# residuals are normally distributed (QQ plot)
# lightly skewed histogram shows moderate model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_total_IJD = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, data = modeldat_IJD,
                   method = "ML")
summary(model_total_IJD)
confint(model_total_IJD)

model_total_IJD_q1 = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, data = modeldat_IJD,
                      correlation = corARMA(q=1, form = ~ Time),
                      method = "ML")
model_total_IJD_p1 = gls(total_cost ~ Time + INF.ETA_Trend_adj + INF.ETA_Level_adj + ADA_Trend_adj + ADA_Level_adj, data = modeldat_IJD,
                      correlation = corARMA(p=1, form = ~ Time),
                      method = "ML")
anova(model_total_IJD, model_total_IJD_q1) 
anova(model_total_IJD, model_total_IJD_p1) 
#choose model with no autocorrelation structure

final_model<-model_total_IJD

plot(total_IJD$date, total_IJD$total_cost,
     ylim = c(0,1500),
     ylab = "IJD Total Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(total_IJD$date, total_IJD$total_cost, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)

lines(modeldat_IJD$date[1:53], fitted(final_model)[1:53], col = "blue", lwd = 2)
lines(modeldat_IJD$date[54:67], fitted(final_model)[54:67], col = "blue", lwd = 2)
lines(modeldat_IJD$date[68:84], fitted(final_model)[68:84], col = "blue", lwd = 2)


y1<-fitted(final_model)[54]
y2<-fitted(final_model)[67]
x1<-modeldat_IJD$Time[54]
x2<-modeldat_IJD$Time[67]
slope1<-(y2-y1)/(x2-x1)
inter1<-fitted(final_model)[54]
newt<-seq(0,33,1)
newx<-total_IJD$date[63:96]
newy<-inter1+(slope1*newt)


segments(as.Date("2020-02-01"),final_model$coef[1] + final_model$coef[2]*62,
         as.Date("2021-03-01"), final_model$coef[1] + final_model$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(newx[22],newy[22],newx[34],newy[34], lty = 1, col = "red", lwd = "2")
#segments(as.Date("2021-10-01"), final_model$coef[1] + final_model$coef[2]*82 + final_model$coef[3]*29 + final_model$coef[4],
#         as.Date("2022-12-01"), final_model$coef[1] + final_model$coef[2]*96 + final_model$coef[3]*43 + final_model$coef[4],
#         lty = 1, col = "red", lwd = "2")
title("IJD Total Cost")


#

##########################################################################################
total_ISD$date = as.Date(paste(total_ISD$year,total_ISD$month, "01", sep = "-"))
model_ols_total_ISD = lm(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD)
summary(model_ols_total_ISD)
dwt(model_ols_total_ISD, max.lag = 12, alternative = "two.sided") #no significant at lag 1-4

par(mfrow = c(1,2))
acf(residuals(model_ols_total_ISD)) 
acf(residuals(model_ols_total_ISD), type = "partial") 
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)


residuals = residuals(model_ols_total_ISD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# Residuals plot converged at the end
# residuals are normally distributed (QQ plot)
# lightly skewed histogram shows moderate model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_total_ISD = gls(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD,
                      method = "ML")
summary(model_total_ISD)
confint(model_total_ISD)
model_total_ISD_p1 = gls(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD,
                         correlation = corARMA(p=1, form = ~ Time),
                         method = "ML")
model_total_ISD_q1 = gls(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD,
                         correlation = corARMA(q=1, form = ~ Time),
                         method = "ML")
model_total_ISD_q2 = gls(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD,
                         correlation = corARMA(q=2, form = ~ Time),
                         method = "ML")
model_total_ISD_p3 = gls(total_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = total_ISD,
                         correlation = corARMA(p=3, form = ~ Time),
                         method = "ML")
anova(model_total_ISD, model_total_ISD_p1) # p = 0.4096
anova(model_total_ISD, model_total_ISD_q1) # p = 0.329
anova(model_total_ISD, model_total_ISD_q2) # p = 0.1385
anova(model_total_ISD, model_total_ISD_p3) # p = 0.3811
#choose model with no correlation for consistency with IJD

plot(total_ISD$date, total_ISD$total_cost,
     ylim = c(0,1500),
     ylab = "ISD Total Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(total_ISD$date, total_ISD$total_cost, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(total_ISD$date[1:53], fitted(model_total_ISD)[1:53], col = "blue", lwd = 2)
lines(total_ISD$date[60:75], fitted(model_total_ISD)[60:75], col = "blue", lwd = 2)
lines(total_ISD$date[82:96], fitted(model_total_ISD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_total_ISD$coef[1] + model_total_ISD$coef[2]*60,
         as.Date("2021-03-01"), model_total_ISD$coef[1] + model_total_ISD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_total_ISD$coef[1] + model_total_ISD$coef[2]*82 + model_total_ISD$coef[3]*29 + model_total_ISD$coef[4],
         as.Date("2022-12-01"), model_total_ISD$coef[1] + model_total_ISD$coef[2]*96 + model_total_ISD$coef[3]*43 + model_total_ISD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("ISD Total Cost")

