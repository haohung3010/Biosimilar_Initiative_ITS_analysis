library("nlme")
library("car")

View(avg_daded_IJD_cost)
View(avg_daded_ISD_cost)
View(avg_daded_IBD_cost)

write.csv(avg_daded_IJD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_IJD_cost.csv", row.names = FALSE)
write.csv(avg_daded_ISD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_ISD_cost.csv", row.names = FALSE)
write.csv(avg_daded_IBD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_IBD_cost.csv", row.names = FALSE)

daded_IJD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_IJD_cost_ITS.csv")
daded_ISD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_ISD_cost_ITS.csv")
daded_IBD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_daded_IBD_cost_ITS.csv")

##########################################################################################
daded_IBD$date = as.Date(paste(daded_IBD$year,daded_IBD$month, "01", sep = "-"))
model_ols_daded_IBD = lm(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IBD)
summary(model_ols_daded_IBD)
dwt(model_ols_daded_IBD, max.lag = 12, alternative = "two.sided") #significant at lag 1,3

par(mfrow = c(1,2))
acf(residuals(model_ols_daded_IBD)) 
acf(residuals(model_ols_daded_IBD), type = "partial") 
par(mfrow = c(1,1))
# does not show AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# significant at p = 1

residuals = residuals(model_ols_daded_IBD)
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
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_daded_IBD = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IBD,
                      method = "ML")

model_daded_IBD_p1 = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IBD,
                         correlation = corARMA(p=1, form = ~ Time),
                         method = "ML")
anova(model_daded_IBD, model_daded_IBD_p1) # p = 0.0075
#choose AR (1) model
summary(model_daded_IBD_p1)
confint(model_daded_IBD_p1)

plot(daded_IBD$date, daded_IBD$avg_daded_cost,
     ylim = c(0,1500),
     ylab = "IBD DAD/ED Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-09-05"), 0, as.Date("2020-03-05"), 1000000, col = "#00000011", border = NA)
lines(daded_IBD$date, daded_IBD$avg_daded_cost, col = "lightblue")
abline(v=as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06")), lty=2)
lines(daded_IBD$date[1:56], fitted(model_daded_IBD_p1)[1:56], col = "blue", lwd = 2)
lines(daded_IBD$date[63:75], fitted(model_daded_IBD_p1)[63:75], col = "blue", lwd = 2)
lines(daded_IBD$date[82:96], fitted(model_daded_IBD_p1)[82:96], col = "blue", lwd = 2)
segments(as.Date("2020-03-01"), model_daded_IBD_p1$coef[1] + model_daded_IBD_p1$coef[2]*63,
         as.Date("2021-03-01"), model_daded_IBD_p1$coef[1] + model_daded_IBD_p1$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_daded_IBD_p1$coef[1] + model_daded_IBD_p1$coef[2]*82 + model_daded_IBD_p1$coef[3]*26 + model_daded_IBD_p1$coef[4],
         as.Date("2022-12-01"), model_daded_IBD_p1$coef[1] + model_daded_IBD_p1$coef[2]*96 + model_daded_IBD_p1$coef[3]*40 + model_daded_IBD_p1$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IBD DAD/ED Cost")

###########################################################################################
daded_IJD$date = as.Date(paste(daded_IJD$year,daded_IJD$month, "01", sep = "-"))
model_ols_daded_IJD = lm(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IJD)
summary(model_ols_daded_IJD)
dwt(model_ols_daded_IJD, max.lag = 12, alternative = "two.sided") #no significant from lag 1-4

par(mfrow = c(1,2))
acf(residuals(model_ols_daded_IJD))
acf(residuals(model_ols_daded_IJD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_daded_IJD)
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
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_daded_IJD = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IJD,
                    method = "ML")
summary(model_daded_IJD)
confint(model_daded_IJD)

model_daded_IJD_q1 = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IJD,
                       correlation = corARMA(q=1, form = ~ Time),
                       method = "ML")
model_daded_IJD_p1 = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_IJD,
                       correlation = corARMA(p=1, form = ~ Time),
                       method = "ML")
anova(model_daded_IJD, model_daded_IJD_q1) # p = 0.2609
anova(model_daded_IJD, model_daded_IJD_p1) # p = 0.3702
#choose model with no autocorrelation structure

plot(daded_IJD$date, daded_IJD$avg_daded_cost,
     ylim = c(0,1500),
     ylab = "IJD DAD/ED Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(daded_IJD$date, daded_IJD$avg_daded_cost, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(daded_IJD$date[1:53], fitted(model_daded_IJD)[1:53], col = "blue", lwd = 2)
lines(daded_IJD$date[60:75], fitted(model_daded_IJD)[60:75], col = "blue", lwd = 2)
lines(daded_IJD$date[82:96], fitted(model_daded_IJD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_daded_IJD$coef[1] + model_daded_IJD$coef[2]*60,
         as.Date("2021-03-01"), model_daded_IJD$coef[1] + model_daded_IJD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_daded_IJD$coef[1] + model_daded_IJD$coef[2]*82 + model_daded_IJD$coef[3]*29 + model_daded_IJD$coef[4],
         as.Date("2022-12-01"), model_daded_IJD$coef[1] + model_daded_IJD$coef[2]*96 + model_daded_IJD$coef[3]*43 + model_daded_IJD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IJD DAD/ED Cost")

##########################################################################################
daded_ISD$date = as.Date(paste(daded_ISD$year,daded_ISD$month, "01", sep = "-"))
model_ols_daded_ISD = lm(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_ISD)
summary(model_ols_daded_ISD)
dwt(model_ols_daded_ISD, max.lag = 12, alternative = "two.sided") #significant at lag 3

par(mfrow = c(1,2))
acf(residuals(model_ols_daded_ISD))
acf(residuals(model_ols_daded_ISD), type = "partial")
par(mfrow = c(1,1))
# does not show AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# significant at p = 3; q = 2, but could be due to sample size

residuals = residuals(model_ols_daded_ISD)
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
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_daded_ISD = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_ISD,
                      method = "ML")
summary(model_daded_ISD)
confint(model_daded_ISD)

model_daded_ISD_q2 = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_ISD,
                         correlation = corARMA(q=2, form = ~ Time),
                         method = "ML")
model_daded_ISD_p3 = gls(avg_daded_cost ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = daded_ISD,
                         correlation = corARMA(p=3, form = ~ Time),
                         method = "ML")
anova(model_daded_ISD, model_daded_ISD_q2) # p = 0.0058
anova(model_daded_ISD, model_daded_ISD_p3) # p = 0.0169
#choose model with no correlation for consistency with IJD

plot(daded_ISD$date, daded_ISD$avg_daded_cost,
     ylim = c(0,1500),
     ylab = "ISD DAD/ED Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(daded_ISD$date, daded_ISD$avg_daded_cost, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(daded_ISD$date[1:53], fitted(model_daded_ISD)[1:53], col = "blue", lwd = 2)
lines(daded_ISD$date[60:75], fitted(model_daded_ISD)[60:75], col = "blue", lwd = 2)
lines(daded_ISD$date[82:96], fitted(model_daded_ISD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_daded_ISD$coef[1] + model_daded_ISD$coef[2]*60,
         as.Date("2021-03-01"), model_daded_ISD$coef[1] + model_daded_ISD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_daded_ISD$coef[1] + model_daded_ISD$coef[2]*82 + model_daded_ISD$coef[3]*29 + model_daded_ISD$coef[4],
         as.Date("2022-12-01"), model_daded_ISD$coef[1] + model_daded_ISD$coef[2]*96 + model_daded_ISD$coef[3]*43 + model_daded_ISD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("ISD DAD/ED Cost")


