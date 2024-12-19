library("nlme")
library("car")

View(avg_msp_IJD_cost)
View(avg_msp_ISD_cost)
View(avg_msp_IBD_cost)

write.csv(avg_msp_IJD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_IJD_cost.csv", row.names = FALSE)
write.csv(avg_msp_ISD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_ISD_cost.csv", row.names = FALSE)
write.csv(avg_msp_IBD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_IBD_cost.csv", row.names = FALSE)

msp_IJD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_IJD_cost_ITS.csv")
msp_ISD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_ISD_cost_ITS.csv")
msp_IBD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_msp_IBD_cost_ITS.csv")

##########################################################################################
msp_IBD$date = as.Date(paste(msp_IBD$year,msp_IBD$month, "01", sep = "-"))
model_ols_msp_IBD = lm(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IBD)
summary(model_ols_msp_IBD)
dwt(model_ols_msp_IBD, max.lag = 12, alternative = "two.sided") #no significant lag

par(mfrow = c(1,2))
acf(residuals(model_ols_msp_IBD))
acf(residuals(model_ols_msp_IBD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_msp_IBD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# no pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# bell-shape histogram shows good model fitting with OLS

# Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_msp_IBD = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IBD,
                    method = "ML")
summary(model_msp_IBD)
confint(model_msp_IBD)

model_msp_IBD_q1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IBD,
                       correlation = corARMA(q=1, form = ~ Time),
                       method = "ML")
model_msp_IBD_p1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IBD,
                       correlation = corARMA(p=1, form = ~ Time),
                       method = "ML")
anova(model_msp_IBD, model_msp_IBD_q1) # p = 0.2942
anova(model_msp_IBD, model_msp_IBD_p1) # p = 0.3316
#choose model with no autocorrelation structure


plot(msp_IBD$date, msp_IBD$avg_cost_msp,
     ylim = c(0,200),
     ylab = "IBD MSP Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-09-05"), 0, as.Date("2020-03-05"), 1000000, col = "#00000011", border = NA)
lines(msp_IBD$date, msp_IBD$avg_cost_msp, col = "lightblue")
abline(v=as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06")), lty=2)
lines(msp_IBD$date[1:56], fitted(model_msp_IBD)[1:56], col = "blue", lwd = 2)
lines(msp_IBD$date[63:75], fitted(model_msp_IBD)[63:75], col = "blue", lwd = 2)
lines(msp_IBD$date[82:96], fitted(model_msp_IBD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2020-03-01"), model_msp_IBD$coef[1] + model_msp_IBD$coef[2]*63,
         as.Date("2021-03-01"), model_msp_IBD$coef[1] + model_msp_IBD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_msp_IBD$coef[1] + model_msp_IBD$coef[2]*82 + model_msp_IBD$coef[3]*26 + model_msp_IBD$coef[4],
         as.Date("2022-12-01"), model_msp_IBD$coef[1] + model_msp_IBD$coef[2]*96 + model_msp_IBD$coef[3]*40 + model_msp_IBD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IBD MSP Cost")
###########################################################################################
msp_IJD$date = as.Date(paste(msp_IJD$year,msp_IJD$month, "01", sep = "-"))

model_ols_msp_IJD = lm(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IJD)
summary(model_ols_msp_IJD)
dwt(model_ols_msp_IJD, max.lag = 12, alternative = "two.sided") #no significant lag

par(mfrow = c(1,2))
acf(residuals(model_ols_msp_IJD))
acf(residuals(model_ols_msp_IJD), type = "partial") 
par(mfrow = c(1,1)) 
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_msp_IJD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# no pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_msp_IJD = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IJD,
                       method = "ML")
summary(model_msp_IJD)
confint(model_msp_IJD)

model_msp_IJD_q1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IJD,
                         correlation = corARMA(q=1, form = ~ Time),
                         method = "ML")
model_msp_IJD_p1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_IJD,
                       correlation = corARMA(p=1, form = ~ Time),
                       method = "ML")
anova(model_msp_IJD, model_msp_IJD_q1) # p = 0.2844
anova(model_msp_IJD, model_msp_IJD_p1) # p = 0.3873
#choose model with no autocorrelation structure

#Plotting the results
plot(msp_IJD$date, msp_IJD$avg_cost_msp,
     ylim = c(0,200),
     ylab = "IJD MSP Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(msp_IJD$date, msp_IJD$avg_cost_msp, col = "lightblue");
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(msp_IJD$date[1:53], fitted(model_msp_IJD)[1:53], col = "blue", lwd = 2)
lines(msp_IJD$date[60:75], fitted(model_msp_IJD)[60:75], col = "blue", lwd = 2)
lines(msp_IJD$date[82:96], fitted(model_msp_IJD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_msp_IJD$coef[1] + model_msp_IJD$coef[2]*60,
         as.Date("2021-03-01"), model_msp_IJD$coef[1] + model_msp_IJD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_msp_IJD$coef[1] + model_msp_IJD$coef[2]*82 + model_msp_IJD$coef[3]*29 + model_msp_IJD$coef[4],
         as.Date("2022-12-01"), model_msp_IJD$coef[1] + model_msp_IJD$coef[2]*96 + model_msp_IJD$coef[3]*43 + model_msp_IJD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IJD MSP Cost")

##########################################################################################
msp_ISD$date = as.Date(paste(msp_ISD$year,msp_ISD$month, "01", sep = "-"))
model_ols_msp_ISD = lm(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_ISD)
summary(model_ols_msp_ISD)
dwt(model_ols_msp_ISD, max.lag = 12, alternative = "two.sided") # no significant lag

par(mfrow = c(1,2))
acf(residuals(model_ols_msp_ISD))
acf(residuals(model_ols_msp_ISD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_msp_ISD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# no pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# bell-shape histogram shows good model fitting with OLS

# Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_msp_ISD = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_ISD,
                    method = "ML")
summary(model_msp_ISD)
confint(model_msp_ISD)

model_msp_ISD_q1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_ISD,
                       correlation = corARMA(q=1, form = ~ Time),
                       method = "ML")
model_msp_ISD_p1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_ISD,
                       correlation = corARMA(p=1, form = ~ Time),
                       method = "ML")
model_msp_ISD_p1q1 = gls(avg_cost_msp ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = msp_ISD,
                       correlation = corARMA(p=1,q=1, form = ~ Time),
                       method = "ML")
anova(model_msp_ISD, model_msp_ISD_q1) # p = 0.0482
anova(model_msp_ISD, model_msp_ISD_p1) # p = 0.0685
anova(model_msp_ISD, model_msp_ISD_p1q1) # p = 0.1355
#choose model with no Q1 structure, to stay consisting with IJD

model_msp_ISD<-model_msp_ISD_q1
summary(model_msp_ISD)

plot(msp_ISD$date, msp_ISD$avg_cost_msp,
     ylim = c(0,200),
     ylab = "ISD MSP Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(msp_ISD$date, msp_ISD$avg_cost_msp, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(msp_ISD$date[1:53], fitted(model_msp_ISD)[1:53], col = "blue", lwd = 2)
lines(msp_ISD$date[60:75], fitted(model_msp_ISD)[60:75], col = "blue", lwd = 2)
lines(msp_ISD$date[82:96], fitted(model_msp_ISD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_msp_ISD$coef[1] + model_msp_ISD$coef[2]*60,
         as.Date("2021-03-01"), model_msp_ISD$coef[1] + model_msp_ISD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_msp_ISD$coef[1] + model_msp_ISD$coef[2]*82 + model_msp_ISD$coef[3]*29 + model_msp_ISD$coef[4],
         as.Date("2022-12-01"), model_msp_ISD$coef[1] + model_msp_ISD$coef[2]*96 + model_msp_ISD$coef[3]*43 + model_msp_ISD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("ISD MSP Cost")

