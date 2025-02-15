library("nlme")
library("car")

View(avg_pn_IJD_cost)
View(avg_pn_ISD_cost)
View(avg_pn_IBD_cost)

write.csv(avg_pn_IJD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_IJD_cost.csv", row.names = FALSE)
write.csv(avg_pn_ISD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_ISD_cost.csv", row.names = FALSE)
write.csv(avg_pn_IBD_cost, file = "R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_IBD_cost.csv", row.names = FALSE)

pn_IJD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_IJD_cost_ITS.csv")
pn_ISD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_ISD_cost_ITS.csv")
pn_IBD = read.csv("R:/working/hdang_AnalysisFiles/R_Code/Official Codes/Output/avg_pn_IBD_cost_ITS.csv")

##########################################################################################
pn_IBD$date = as.Date(paste(pn_IBD$year,pn_IBD$month, "01", sep = "-"))
model_ols_pn_IBD = lm(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IBD)
summary(model_ols_pn_IBD)
dwt(model_ols_pn_IBD, max.lag = 12, alternative = "two.sided") #no significant for lag 1-4

par(mfrow = c(1,2))
acf(residuals(model_ols_pn_IBD)) 
acf(residuals(model_ols_pn_IBD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# no significant

residuals = residuals(model_ols_pn_IBD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# seasonal pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# lightly skewed histogram shows moderate model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_pn_IBD = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IBD,
                   method = "ML")
summary(model_pn_IBD)
confint(model_pn_IBD)

model_pn_IBD_q1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IBD,
                      correlation = corARMA(q=1, form = ~ Time),
                      method = "ML")
model_pn_IBD_p1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IBD,
                      correlation = corARMA(p=1, form = ~ Time),
                      method = "ML")
anova(model_pn_IBD, model_pn_IBD_q1) # p = 0.2324
anova(model_pn_IBD, model_pn_IBD_p1) # p = 0.1799
#choose model with no autocorrelation structure

plot(pn_IBD$date, pn_IBD$avg_cost_pn,
     ylim = c(0,100),
     ylab = "IBD PN Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-09-05"), 0, as.Date("2020-03-05"), 1000000, col = "#00000011", border = NA)
lines(pn_IBD$date, pn_IBD$avg_cost_pn, col = "lightblue")
abline(v=as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06")), lty=2)
lines(pn_IBD$date[1:56], fitted(model_pn_IBD)[1:56], col = "blue", lwd = 2)
lines(pn_IBD$date[63:75], fitted(model_pn_IBD)[63:75], col = "blue", lwd = 2)
lines(pn_IBD$date[82:96], fitted(model_pn_IBD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2020-03-01"), model_pn_IBD$coef[1] + model_pn_IBD$coef[2]*63,
         as.Date("2021-03-01"), model_pn_IBD$coef[1] + model_pn_IBD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_pn_IBD$coef[1] + model_pn_IBD$coef[2]*82 + model_pn_IBD$coef[3]*26 + model_pn_IBD$coef[4],
         as.Date("2022-12-01"), model_pn_IBD$coef[1] + model_pn_IBD$coef[2]*96 + model_pn_IBD$coef[3]*40 + model_pn_IBD$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IBD PN Cost")

###########################################################################################
pn_IJD$date = as.Date(paste(pn_IJD$year,pn_IJD$month, "01", sep = "-"))
model_ols_pn_IJD = lm(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD)
summary(model_ols_pn_IJD)
dwt(model_ols_pn_IJD, max.lag = 12, alternative = "two.sided") #significant at lag 1-2

par(mfrow = c(1,2))
acf(residuals(model_ols_pn_IJD))
acf(residuals(model_ols_pn_IJD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# significant at p = q = 4

residuals = residuals(model_ols_pn_IJD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# seasonal pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_pn_IJD = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      method = "ML")
model_pn_IJD_q1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(q=1, form = ~ Time),
                      method = "ML")
model_pn_IJD_q2 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(q=2, form = ~ Time),
                      method = "ML")
model_pn_IJD_q3 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(q=3, form = ~ Time),
                      method = "ML")
model_pn_IJD_q4 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                         correlation = corARMA(q=4, form = ~ Time),
                         method = "ML")

model_pn_IJD_p1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(p=1, form = ~ Time),
                      method = "ML")
model_pn_IJD_p2 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(p=2, form = ~ Time),
                      method = "ML")
model_pn_IJD_p3 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(p=3, form = ~ Time),
                      method = "ML")
model_pn_IJD_p4 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                         correlation = corARMA(p=4, form = ~ Time),
                         method = "ML")

model_pn_IJD_p1q1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_IJD,
                      correlation = corARMA(p=1,q=1, form = ~ Time),
                      method = "ML")
anova(model_pn_IJD, model_pn_IJD_q1) # p = .0082
anova(model_pn_IJD, model_pn_IJD_q2) # p = .0016
anova(model_pn_IJD, model_pn_IJD_q3) # p = .005
anova(model_pn_IJD, model_pn_IJD_q4) # p = .012

anova(model_pn_IJD, model_pn_IJD_p1) # p = 0.0021
anova(model_pn_IJD, model_pn_IJD_p2) # p = 0.0049
anova(model_pn_IJD, model_pn_IJD_p3) # p = 0.0042
anova(model_pn_IJD, model_pn_IJD_p4) # p = 0.0043

anova(model_pn_IJD, model_pn_IJD_p1q1) # p = 0.007

anova(model_pn_IJD_q2, model_pn_IJD_p1) # p = 0.0653

#choose AR2 model- slightly lower AIC
summary(model_pn_IJD_q2)
confint(model_pn_IJD_q2)

plot(pn_IJD$date, pn_IJD$avg_cost_pn,
     ylim = c(0,100),
     ylab = "IJD PN Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(pn_IJD$date, pn_IJD$avg_cost_pn, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(pn_IJD$date[1:53], fitted(model_pn_IJD_q2)[1:53], col = "blue", lwd = 2)
lines(pn_IJD$date[60:75], fitted(model_pn_IJD_q2)[60:75], col = "blue", lwd = 2)
lines(pn_IJD$date[82:96], fitted(model_pn_IJD_q2)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_pn_IJD_q2$coef[1] + model_pn_IJD_q2$coef[2]*60,
         as.Date("2021-03-01"), model_pn_IJD_q2$coef[1] + model_pn_IJD_q2$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_pn_IJD_q2$coef[1] + model_pn_IJD_q2$coef[2]*82 + model_pn_IJD_q2$coef[3]*29 + model_pn_IJD_q2$coef[4],
         as.Date("2022-12-01"), model_pn_IJD_q2$coef[1] + model_pn_IJD_q2$coef[2]*96 + model_pn_IJD_q2$coef[3]*31 + model_pn_IJD_q2$coef[4],
         lty = 1, col = "red", lwd = "2")
title("IJD PN Cost")

##########################################################################################
pn_ISD$date = as.Date(paste(pn_ISD$year,pn_ISD$month, "01", sep = "-"))
model_ols_pn_ISD = lm(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD)
summary(model_ols_pn_ISD)
dwt(model_ols_pn_ISD, max.lag = 12, alternative = "two.sided") #significant at lag 2

par(mfrow = c(1,2))
acf(residuals(model_ols_pn_ISD))
acf(residuals(model_ols_pn_ISD), type = "partial")
par(mfrow = c(1,1))
# does not show clear AR, MA, or ARMA process (no gradual decline in both ACF and PACF)
# significant at p = q = 2

residuals = residuals(model_ols_pn_ISD)
std_residuals = (residuals - mean(residuals)) / sd(residuals)
par(mfrow = c(1,3))
plot(std_residuals, type = "l", xlab = "Time", ylab = "Standardized Residuals", main = "Standardized Residuals over Time")
hist(std_residuals, probability = TRUE, breaks = "Sturges", xlab = "Standardized Residuals", main = "Histogram of Standardized Residuals")
curve(dnorm(x, mean = mean(std_residuals), sd = sd(std_residuals)), add = TRUE, col = "blue", lwd = 2)
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "blue", lwd = 2)
par(mfrow = c(1,1)) 
# seasonal pattern in Residuals plot
# residuals are normally distributed (QQ plot)
# bell-shape histogram shows good model fitting with OLS

#Fit the ARMA regression model using Generalized Least-Squares function, testing different correlation structures by likelihood-ratio tests 
model_pn_ISD = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                   method = "ML")
model_pn_ISD_q1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(q=1, form = ~ Time),
                      method = "ML")
model_pn_ISD_q2 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(q=2, form = ~ Time),
                      method = "ML")
model_pn_ISD_q3 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(q=3, form = ~ Time),
                      method = "ML")
model_pn_ISD_q4 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(q=4, form = ~ Time),
                      method = "ML")

model_pn_ISD_p1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(p=1, form = ~ Time),
                      method = "ML")
model_pn_ISD_p2 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(p=2, form = ~ Time),
                      method = "ML")
model_pn_ISD_p3 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(p=3, form = ~ Time),
                      method = "ML")
model_pn_ISD_p4 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                      correlation = corARMA(p=4, form = ~ Time),
                      method = "ML")

model_pn_ISD_p1q1 = gls(avg_cost_pn ~ Time + INF.ETA_Trend + INF.ETA_Level + ADA_Trend + ADA_Level, data = pn_ISD,
                        correlation = corARMA(p=1,q=1, form = ~ Time),
                        method = "ML")
anova(model_pn_ISD, model_pn_ISD_q1) # p = .0002
anova(model_pn_ISD, model_pn_ISD_q2) # p = <0.0001
anova(model_pn_ISD, model_pn_ISD_q3) # p = 0.0001
anova(model_pn_ISD, model_pn_ISD_q4) # p = <0.0001  #lowest AIC

anova(model_pn_ISD, model_pn_ISD_p1) # p = <0.0001
anova(model_pn_ISD, model_pn_ISD_p2) # p = 0.0001
anova(model_pn_ISD, model_pn_ISD_p3) # p = 0.0001
anova(model_pn_ISD, model_pn_ISD_p4) # p = <0.0001

anova(model_pn_ISD, model_pn_IJD_p1q1) # p = 0.007

anova(model_pn_ISD_q4, model_pn_ISD_p4) # 


#choose MA(4) model-lower AIC
summary(model_pn_ISD_q4)
confint(model_pn_ISD_q4)

plot(pn_ISD$date, pn_ISD$avg_cost_pn,
     ylim = c(0,100),
     ylab = "ISD PN Cost",
     xlab = "Year",
     pch = 20, col = "lightblue")
rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(pn_ISD$date, pn_ISD$avg_cost_pn, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(pn_ISD$date[1:53], fitted(model_pn_ISD_q4)[1:53], col = "blue", lwd = 2)
lines(pn_ISD$date[60:75], fitted(model_pn_ISD_q4)[60:75], col = "blue", lwd = 2)
lines(pn_ISD$date[82:96], fitted(model_pn_ISD_q4)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_pn_ISD_q4$coef[1] + model_pn_ISD_q4$coef[2]*60,
         as.Date("2021-03-01"), model_pn_ISD_q4$coef[1] + model_pn_ISD_q4$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_pn_ISD_q4$coef[1] + model_pn_ISD_q4$coef[2]*82 + model_pn_ISD_q4$coef[3]*29 + model_pn_ISD_q4$coef[4],
         as.Date("2022-12-01"), model_pn_ISD_q4$coef[1] + model_pn_ISD_q4$coef[2]*96 + model_pn_ISD_q4$coef[3]*31 + model_pn_ISD_q4$coef[4],
         lty = 1, col = "red", lwd = "2")
title("ISD PN Cost")

