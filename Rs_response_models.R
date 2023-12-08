# Remove all Working variables
rm(list=ls())

# load packages
library(ggplot2)
library(ggpubr)
library(grid)
library(base)
library(lubridate)
library(egg)  
library(data.table)

# load csv file
flux_int_og = read.csv('/Users/jessicamontes/Documents/ThesisData/0_ThesisPaper/Final_Paper/JGR_Biogeosciences/Rs_Reco_5day.csv')
#View(flux_int_og)

# Remove Reco and Ratio columns from analysis 
# Note: this is to avoid removing  Rs values when removing NAs, since there are less values for Reco and Ratio
flux_int_og <- flux_int_og[,-c(3,11)]

# Remove outliers
NA -> flux_int_og$Rs[2] 
NA -> flux_int_og$Rs[11] 
NA -> flux_int_og$Rs[33] 
NA -> flux_int_og$Rs[43]
NA -> flux_int_og$Rs[49]

# Create squared SWC and Ts
flux_int_og$SWC2 <- (flux_int_og$SWC)^2
flux_int_og$Ts2 <- (flux_int_og$Ts)^2
flux_int_og$SWC3 <- (flux_int_og$SWC)^3
flux_int_og$Ts3 <- (flux_int_og$Ts)^3

# Remove NAs
flux_int <- na.omit(flux_int_og)

View(flux_int)

############################################################################################################
# Rs response to Ts partioned by soil water content levels
############################################################################################################

# Create groups of different SWC levels
flux_int$SWC_Level <- as.factor(ifelse(flux_int$SWC<=5.5, 'Verydry',
                                       ifelse(flux_int$SWC>5.5 & flux_int$SWC<=9, 'Dry',
                                              ifelse(flux_int$SWC>9, 'Wet', 0))))

# Specify order of SWC levels
flux_int$SWC_Level <- factor(flux_int$SWC_Level, levels = c("Wet", "Dry", "Verydry"))

### Exponential (Very dry)
Rs_Ts_exp_verydry_SWC <- lm(log(Rs) ~ Ts, data=subset(flux_int, SWC_Level=="Verydry"))

# look at residuals
Rs_Ts_exp_verydry_SWC.resid <- resid(Rs_Ts_exp_verydry_SWC)
Rs_Ts_exp_verydry_SWC.fitted <- fitted(Rs_Ts_exp_verydry_SWC)
Rs_Ts_exp_verydry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_exp_verydry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_exp_verydry_SWC.fitted, Rs_Ts_exp_verydry_SWC.resid, main='resid, Rs_Ts_exp_verydry_SWC')
plot(Rs_Ts_exp_verydry_SWC.fitted, Rs_Ts_exp_verydry_SWC.sqrt, main='sqrt resid, Rs_Ts_exp_verydry_SWC')
qqnorm(Rs_Ts_exp_verydry_SWC.resid, main = 'Rs_Ts_exp_verydry_SWC')
qqline(Rs_Ts_exp_verydry_SWC.resid)
hist(Rs_Ts_exp_verydry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_exp_verydry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.39434    0.19241  -2.050   0.0863 .
#Ts          -0.01812    0.01240  -1.462   0.1941  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.174 on 6 degrees of freedom
#Multiple R-squared:  0.2626,	Adjusted R-squared:  0.1397 
#F-statistic: 2.137 on 1 and 6 DF,  p-value: 0.1941

r.squaredGLMM(Rs_Ts_exp_verydry_SWC)
#R2m       R2c
#[1,] 0.2338558 0.2338558

AIC(Rs_Ts_exp_verydry_SWC)
# -1.57373

### Linear (Very dry)
Rs_Ts_linear_verydry_SWC <- lm(Rs ~ Ts, data=subset(flux_int, SWC_Level=="Verydry"))

# look at residuals
Rs_Ts_linear_verydry_SWC.resid <- resid(Rs_Ts_linear_verydry_SWC)
Rs_Ts_linear_verydry_SWC.fitted <- fitted(Rs_Ts_linear_verydry_SWC)
Rs_Ts_linear_verydry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_linear_verydry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_linear_verydry_SWC.fitted, Rs_Ts_linear_verydry_SWC.resid, main='resid, Rs_Ts_linear_verydry_SWC')
plot(Rs_Ts_linear_verydry_SWC.fitted, Rs_Ts_linear_verydry_SWC.sqrt, main='sqrt resid, Rs_Ts_linear_verydry_SWC')
qqnorm(Rs_Ts_linear_verydry_SWC.resid, main = 'Rs_Ts_linear_verydry_SWC')
qqline(Rs_Ts_linear_verydry_SWC.resid)
hist(Rs_Ts_linear_verydry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_linear_verydry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.673804   0.102614   6.566 0.000598 ***
#  Ts          -0.010147   0.006613  -1.534 0.175817    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.09282 on 6 degrees of freedom
#Multiple R-squared:  0.2818,	Adjusted R-squared:  0.1621 
#F-statistic: 2.355 on 1 and 6 DF,  p-value: 0.1758

r.squaredGLMM(Rs_Ts_linear_verydry_SWC)
#R2m       R2c
#[1,] 0.2517002 0.2517002

AIC(Rs_Ts_linear_verydry_SWC)
# -11.63174


### Quadratic (Very dry)
Rs_Ts_quad_verydry_SWC <- lm(Rs ~ Ts + Ts2, data=subset(flux_int, SWC_Level=="Verydry"))

# look at residuals
Rs_Ts_quad_verydry_SWC.resid <- resid(Rs_Ts_quad_verydry_SWC)
Rs_Ts_quad_verydry_SWC.fitted <- fitted(Rs_Ts_quad_verydry_SWC)
Rs_Ts_quad_verydry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_quad_verydry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_quad_verydry_SWC.fitted, Rs_Ts_quad_verydry_SWC.resid, main='resid, Rs_Ts_quad_verydry_SWC')
plot(Rs_Ts_quad_verydry_SWC.fitted, Rs_Ts_quad_verydry_SWC.sqrt, main='sqrt resid, Rs_Ts_quad_verydry_SWC')
qqnorm(Rs_Ts_quad_verydry_SWC.resid, main = 'Rs_Ts_quad_verydry_SWC')
qqline(Rs_Ts_quad_verydry_SWC.resid)
hist(Rs_Ts_quad_verydry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_quad_verydry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.647447   0.309562   2.091   0.0907 .
#Ts          -0.005898   0.047067  -0.125   0.9052  
#Ts2         -0.000150   0.001642  -0.091   0.9307  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1016 on 5 degrees of freedom
#Multiple R-squared:  0.283,	Adjusted R-squared:  -0.003765 
#F-statistic: 0.9869 on 2 and 5 DF,  p-value: 0.4353

r.squaredGLMM(Rs_Ts_quad_verydry_SWC)
#R2m       R2c
#[1,] 0.2199463 0.2199463

AIC(Rs_Ts_quad_verydry_SWC)
# -9.645089


### Exponential (Dry)
Rs_Ts_exp_dry_SWC <- lm(log(Rs) ~ Ts, data=subset(flux_int, SWC_Level=="Dry"))

# look at residuals
Rs_Ts_exp_dry_SWC.resid <- resid(Rs_Ts_exp_dry_SWC)
Rs_Ts_exp_dry_SWC.fitted <- fitted(Rs_Ts_exp_dry_SWC)
Rs_Ts_exp_dry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_exp_dry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_exp_dry_SWC.fitted, Rs_Ts_exp_dry_SWC.resid, main='resid, Rs_Ts_exp_dry_SWC')
plot(Rs_Ts_exp_dry_SWC.fitted, Rs_Ts_exp_dry_SWC.sqrt, main='sqrt resid, Rs_Ts_exp_dry_SWC')
qqnorm(Rs_Ts_exp_dry_SWC.resid, main = 'Rs_Ts_exp_dry_SWC')
qqline(Rs_Ts_exp_dry_SWC.resid)
hist(Rs_Ts_exp_dry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_exp_dry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.36359    0.22267  -1.633    0.119  
#Ts           0.02634    0.01065   2.474    0.023 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3839 on 19 degrees of freedom
#Multiple R-squared:  0.2437,	Adjusted R-squared:  0.2039 
#F-statistic: 6.122 on 1 and 19 DF,  p-value: 0.02295

r.squaredGLMM(Rs_Ts_exp_dry_SWC)
#R2m       R2c
#[1,] 0.2343533 0.2343533

AIC(Rs_Ts_exp_dry_SWC)
# 23.28805

### Linear (Dry)
Rs_Ts_linear_dry_SWC <- lm(Rs ~ Ts, data=subset(flux_int, SWC_Level=="Dry"))

# look at residuals
Rs_Ts_linear_dry_SWC.resid <- resid(Rs_Ts_linear_dry_SWC)
Rs_Ts_linear_dry_SWC.fitted <- fitted(Rs_Ts_linear_dry_SWC)
Rs_Ts_linear_dry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_linear_dry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_linear_dry_SWC.fitted, Rs_Ts_linear_dry_SWC.resid, main='resid, Rs_Ts_linear_dry_SWC')
plot(Rs_Ts_linear_dry_SWC.fitted, Rs_Ts_linear_dry_SWC.sqrt, main='sqrt resid, Rs_Ts_linear_dry_SWC')
qqnorm(Rs_Ts_linear_dry_SWC.resid, main = 'Rs_Ts_linear_dry_SWC')
qqline(Rs_Ts_linear_dry_SWC.resid)
hist(Rs_Ts_linear_dry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_linear_dry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.78437    0.24549   3.195  0.00477 **
#  Ts           0.02417    0.01174   2.059  0.05342 . 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4233 on 19 degrees of freedom
#Multiple R-squared:  0.1825,	Adjusted R-squared:  0.1395 
#F-statistic: 4.241 on 1 and 19 DF,  p-value: 0.05342

r.squaredGLMM(Rs_Ts_linear_dry_SWC)
#R2m       R2c
#[1,] 0.1749617 0.1749617

AIC(Rs_Ts_linear_dry_SWC)
# 27.3849


### Quadratic (Dry)
Rs_Ts_quad_dry_SWC <- lm(Rs ~ Ts + Ts2, data=subset(flux_int, SWC_Level=="Dry"))

# look at residuals
Rs_Ts_quad_dry_SWC.resid <- resid(Rs_Ts_quad_dry_SWC)
Rs_Ts_quad_dry_SWC.fitted <- fitted(Rs_Ts_quad_dry_SWC)
Rs_Ts_quad_dry_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_quad_dry_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_quad_dry_SWC.fitted, Rs_Ts_quad_dry_SWC.resid, main='resid, Rs_Ts_quad_dry_SWC')
plot(Rs_Ts_quad_dry_SWC.fitted, Rs_Ts_quad_dry_SWC.sqrt, main='sqrt resid, Rs_Ts_quad_dry_SWC')
qqnorm(Rs_Ts_quad_dry_SWC.resid, main = 'Rs_Ts_quad_dry_SWC')
qqline(Rs_Ts_quad_dry_SWC.resid)
hist(Rs_Ts_quad_dry_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_quad_dry_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.0911843  0.2420125  -4.509 0.000272 ***
#  Ts           0.2957112  0.0316207   9.352 2.48e-08 ***
#  Ts2         -0.0077410  0.0008888  -8.710 7.15e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1904 on 18 degrees of freedom
#Multiple R-squared:  0.8432,	Adjusted R-squared:  0.8258 
#F-statistic: 48.41 on 2 and 18 DF,  p-value: 5.722e-08

r.squaredGLMM(Rs_Ts_quad_dry_SWC)
#R2m       R2c
#[1,] 0.8287851 0.8287851

AIC(Rs_Ts_quad_dry_SWC)
# -5.295232


### Exponential (Wet)
Rs_Ts_exp_wet_SWC <- lm(log(Rs) ~ Ts, data=subset(flux_int, SWC_Level=="Wet"))

# look at residuals
Rs_Ts_exp_wet_SWC.resid <- resid(Rs_Ts_exp_wet_SWC)
Rs_Ts_exp_wet_SWC.fitted <- fitted(Rs_Ts_exp_wet_SWC)
Rs_Ts_exp_wet_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_exp_wet_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_exp_wet_SWC.fitted, Rs_Ts_exp_wet_SWC.resid, main='resid, Rs_Ts_exp_wet_SWC')
plot(Rs_Ts_exp_wet_SWC.fitted, Rs_Ts_exp_wet_SWC.sqrt, main='sqrt resid, Rs_Ts_exp_wet_SWC')
qqnorm(Rs_Ts_exp_wet_SWC.resid, main = 'Rs_Ts_exp_wet_SWC')
qqline(Rs_Ts_exp_wet_SWC.resid)
hist(Rs_Ts_exp_wet_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_exp_wet_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.07090    0.11379   0.623  0.54150   
#Ts           0.04216    0.01135   3.713  0.00173 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2123 on 17 degrees of freedom
#Multiple R-squared:  0.4479,	Adjusted R-squared:  0.4154 
#F-statistic: 13.79 on 1 and 17 DF,  p-value: 0.001727

r.squaredGLMM(Rs_Ts_exp_wet_SWC)
#R2m       R2c
#[1,] 0.4337695 0.4337695

AIC(Rs_Ts_exp_wet_SWC)
#-1.087041


### Linear (Wet)
Rs_Ts_linear_wet_SWC <- lm(Rs ~ Ts, data=subset(flux_int, SWC_Level=="Wet"))

# look at residuals
Rs_Ts_linear_wet_SWC.resid <- resid(Rs_Ts_linear_wet_SWC)
Rs_Ts_linear_wet_SWC.fitted <- fitted(Rs_Ts_linear_wet_SWC)
Rs_Ts_linear_wet_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_linear_wet_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_linear_wet_SWC.fitted, Rs_Ts_linear_wet_SWC.resid, main='resid, Rs_Ts_linear_wet_SWC')
plot(Rs_Ts_linear_wet_SWC.fitted, Rs_Ts_linear_wet_SWC.sqrt, main='sqrt resid, Rs_Ts_linear_wet_SWC')
qqnorm(Rs_Ts_linear_wet_SWC.resid, main = 'Rs_Ts_linear_wet_SWC')
qqline(Rs_Ts_linear_wet_SWC.resid)
hist(Rs_Ts_linear_wet_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_linear_wet_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.99778    0.19110   5.221 6.91e-05 ***
#  Ts           0.07004    0.01907   3.673  0.00189 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3565 on 17 degrees of freedom
#Multiple R-squared:  0.4424,	Adjusted R-squared:  0.4096 
#F-statistic: 13.49 on 1 and 17 DF,  p-value: 0.001886

r.squaredGLMM(Rs_Ts_linear_wet_SWC)
#R2m       R2c
#[1,] 0.4283944 0.4283944

AIC(Rs_Ts_linear_wet_SWC)
# 18.61423


### Quadratic (Wet)
Rs_Ts_quad_wet_SWC <- lm(Rs ~ Ts + Ts2, data=subset(flux_int, SWC_Level=="Wet"))

# look at residuals
Rs_Ts_quad_wet_SWC.resid <- resid(Rs_Ts_quad_wet_SWC)
Rs_Ts_quad_wet_SWC.fitted <- fitted(Rs_Ts_quad_wet_SWC)
Rs_Ts_quad_wet_SWC.sqrt <- sqrt(abs(resid(Rs_Ts_quad_wet_SWC)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_quad_wet_SWC.fitted, Rs_Ts_quad_wet_SWC.resid, main='resid, Rs_Ts_quad_wet_SWC')
plot(Rs_Ts_quad_wet_SWC.fitted, Rs_Ts_quad_wet_SWC.sqrt, main='sqrt resid, Rs_Ts_quad_wet_SWC')
qqnorm(Rs_Ts_quad_wet_SWC.resid, main = 'Rs_Ts_quad_wet_SWC')
qqline(Rs_Ts_quad_wet_SWC.resid)
hist(Rs_Ts_quad_wet_SWC.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_quad_wet_SWC)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.9146555  0.3645898   2.509   0.0233 *
#  Ts           0.0899773  0.0762307   1.180   0.2551  
#Ts2         -0.0009706  0.0035857  -0.271   0.7901  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.3667 on 16 degrees of freedom
#Multiple R-squared:  0.445,	Adjusted R-squared:  0.3756 
#F-statistic: 6.414 on 2 and 16 DF,  p-value: 0.009004

r.squaredGLMM(Rs_Ts_quad_wet_SWC)
#R2m       R2c
#[1,] 0.4161172 0.4161172

AIC(Rs_Ts_quad_wet_SWC)
# 20.52743


### Exponential (All data)
Rs_Ts_exp_alldata <- lm(log(Rs) ~ Ts, data=flux_int)

# look at residuals
Rs_Ts_exp_alldata.resid <- resid(Rs_Ts_exp_alldata)
Rs_Ts_exp_alldata.fitted <- fitted(Rs_Ts_exp_alldata)
Rs_Ts_exp_alldata.sqrt <- sqrt(abs(resid(Rs_Ts_exp_alldata)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_exp_alldata.fitted, Rs_Ts_exp_alldata.resid, main='resid, Rs_Ts_exp_alldata')
plot(Rs_Ts_exp_alldata.fitted, Rs_Ts_exp_alldata.sqrt, main='sqrt resid, Rs_Ts_exp_alldata')
qqnorm(Rs_Ts_exp_alldata.resid, main = 'Rs_Ts_exp_alldata')
qqline(Rs_Ts_exp_alldata.resid)
hist(Rs_Ts_exp_alldata.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_exp_alldata)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -0.023763   0.159859  -0.149    0.882
#Ts           0.009269   0.009299   0.997    0.324

#Residual standard error: 0.5231 on 45 degrees of freedom
#Multiple R-squared:  0.0216,	Adjusted R-squared:  -0.0001377 
#F-statistic: 0.9937 on 1 and 45 DF,  p-value: 0.3242

r.squaredGLMM(Rs_Ts_exp_alldata)
#R2m        R2c
#[1,] 0.02114473 0.02114473

AIC(Rs_Ts_exp_alldata)
#76.42016


### Linear (All data)
Rs_Ts_linear_alldata <- lm(Rs ~ Ts, data=flux_int)

# look at residuals
Rs_Ts_linear_alldata.resid <- resid(Rs_Ts_linear_alldata)
Rs_Ts_linear_alldata.fitted <- fitted(Rs_Ts_linear_alldata)
Rs_Ts_linear_alldata.sqrt <- sqrt(abs(resid(Rs_Ts_linear_alldata)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_linear_alldata.fitted, Rs_Ts_linear_alldata.resid, main='resid, Rs_Ts_linear_alldata')
plot(Rs_Ts_linear_alldata.fitted, Rs_Ts_linear_alldata.sqrt, main='sqrt resid, Rs_Ts_linear_alldata')
qqnorm(Rs_Ts_linear_alldata.resid, main = 'Rs_Ts_linear_alldata')
qqline(Rs_Ts_linear_alldata.resid)
hist(Rs_Ts_linear_alldata.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_linear_alldata)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.119297   0.176962   6.325 1.03e-07 ***
#  Ts          0.009818   0.010294   0.954    0.345    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.579 on 45 degrees of freedom
#Multiple R-squared:  0.01981,	Adjusted R-squared:  -0.001969 
#F-statistic: 0.9096 on 1 and 45 DF,  p-value: 0.3453

r.squaredGLMM(Rs_Ts_linear_alldata)
#R2m        R2c
#[1,] 0.01939062 0.01939062

AIC(Rs_Ts_linear_alldata)
# 85.97474


### Quadratic (All data)
Rs_Ts_quad_alldata <- lm(Rs ~ Ts + Ts2, data=flux_int)

# look at residuals
Rs_Ts_quad_alldata.resid <- resid(Rs_Ts_quad_alldata)
Rs_Ts_quad_alldata.fitted <- fitted(Rs_Ts_quad_alldata)
Rs_Ts_quad_alldata.sqrt <- sqrt(abs(resid(Rs_Ts_quad_alldata)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_Ts_quad_alldata.fitted, Rs_Ts_quad_alldata.resid, main='resid, Rs_Ts_quad_alldata')
plot(Rs_Ts_quad_alldata.fitted, Rs_Ts_quad_alldata.sqrt, main='sqrt resid, Rs_Ts_quad_alldata')
qqnorm(Rs_Ts_quad_alldata.resid, main = 'Rs_Ts_quad_alldata')
qqline(Rs_Ts_quad_alldata.resid)
hist(Rs_Ts_quad_alldata.resid)
par(mfrow=c(1,1))

summary(Rs_Ts_quad_alldata)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.646146   0.322462   2.004   0.0513 .
#Ts           0.087418   0.045741   1.911   0.0625 .
#Ts2         -0.002366   0.001360  -1.739   0.0890 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.5664 on 44 degrees of freedom
#Multiple R-squared:  0.08286,	Adjusted R-squared:  0.04117 
#F-statistic: 1.988 on 2 and 44 DF,  p-value: 0.1491

r.squaredGLMM(Rs_Ts_quad_alldata)
#R2m        R2c
#[1,] 0.07954427 0.07954427

AIC(Rs_Ts_quad_alldata)
# 84.85001



############################################################################################################
# Rs response to SWC partioned by soil temperature levels
############################################################################################################

# Create groups of different Ts levels
flux_int$Temp_Level <- as.factor(ifelse(flux_int$Ts > 20, 'High',
                                        ifelse(flux_int$Ts <= 20 & flux_int$Ts > 10, 'Medium',
                                               ifelse(flux_int$Ts <= 10, 'Low', 0))))
# Specify order of SWC levels
flux_int$Temp_Level <- factor(flux_int$Temp_Level, levels = c("High", "Medium", "Low"))


### Linear (High)
Rs_SWC_linear_high <- lm(Rs ~ SWC, data=subset(flux_int, Temp_Level=="High"))

# look at residuals
Rs_SWC_linear_high.resid <- resid(Rs_SWC_linear_high)
Rs_SWC_linear_high.fitted <- fitted(Rs_SWC_linear_high)
Rs_SWC_linear_high.sqrt <- sqrt(abs(resid(Rs_SWC_linear_high)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_linear_high.fitted, Rs_SWC_linear_high.resid, main='resid, Rs_SWC_linear_high')
plot(Rs_SWC_linear_high.fitted, Rs_SWC_linear_high.sqrt, main='sqrt resid, Rs_SWC_linear_high')
qqnorm(Rs_SWC_linear_high.resid, main = 'Rs_SWC_linear_high')
qqline(Rs_SWC_linear_high.resid)
hist(Rs_SWC_linear_high.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_linear_high)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.18471    0.27561  -7.927 7.13e-06 ***
#  SWC          0.55443    0.04362  12.711 6.43e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1081 on 11 degrees of freedom
#Multiple R-squared:  0.9363,	Adjusted R-squared:  0.9305 
#F-statistic: 161.6 on 1 and 11 DF,  p-value: 6.426e-08


r.squaredGLMM(Rs_SWC_linear_high)
#R2m       R2c
#[1,] 0.9308597 0.9308597

AIC(Rs_SWC_linear_high)
#-17.10992



### Quadratic (High)
Rs_SWC_quad_high <- lm(Rs ~ SWC +SWC2, data=subset(flux_int, Temp_Level=="High"))

# look at residuals
Rs_SWC_quad_high.resid <- resid(Rs_SWC_quad_high)
Rs_SWC_quad_high.fitted <- fitted(Rs_SWC_quad_high)
Rs_SWC_quad_high.sqrt <- sqrt(abs(resid(Rs_SWC_quad_high)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_quad_high.fitted, Rs_SWC_quad_high.resid, main='resid, Rs_SWC_quad_high')
plot(Rs_SWC_quad_high.fitted, Rs_SWC_quad_high.sqrt, main='sqrt resid, Rs_SWC_quad_high')
qqnorm(Rs_SWC_quad_high.resid, main = 'Rs_SWC_quad_high')
qqline(Rs_SWC_quad_high.resid)
hist(Rs_SWC_quad_high.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_quad_high)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -5.04955    1.94307  -2.599   0.0265 *
#  SWC          1.50276    0.63869   2.353   0.0404 *
#  SWC2        -0.07744    0.05204  -1.488   0.1676  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1026 on 10 degrees of freedom
#Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9374 
#F-statistic:  90.8 on 2 and 10 DF,  p-value: 3.872e-07


r.squaredGLMM(Rs_SWC_quad_high)
#R2m       R2c
#[1,] 0.9380177 0.9380177

AIC(Rs_SWC_quad_high)
#-17.70983


### Cubic (High)
Rs_SWC_cubic_high<- lm(Rs ~ SWC +SWC2+SWC3, data=subset(flux_int, Temp_Level=="High"))

# look at residuals
Rs_SWC_cubic_high.resid <- resid(Rs_SWC_cubic_high)
Rs_SWC_cubic_high.fitted <- fitted(Rs_SWC_cubic_high)
Rs_SWC_cubic_high.sqrt <- sqrt(abs(resid(Rs_SWC_cubic_high)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_cubic_high.fitted, Rs_SWC_cubic_high.resid, main='resid, Rs_SWC_cubic_high')
plot(Rs_SWC_cubic_high.fitted, Rs_SWC_cubic_high.sqrt, main='sqrt resid, Rs_SWC_cubic_high')
qqnorm(Rs_SWC_cubic_high.resid, main = 'Rs_SWC_cubic_high')
qqline(Rs_SWC_cubic_high.resid)
hist(Rs_SWC_cubic_high.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_cubic_high)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  43.00110    6.83281   6.293 0.000142 ***
#  SWC         -22.92118    3.45924  -6.626 9.63e-05 ***
#  SWC2          4.01425    0.57824   6.942 6.74e-05 ***
#  SWC3         -0.22607    0.03193  -7.081 5.79e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0422 on 9 degrees of freedom
#Multiple R-squared:  0.9921,	Adjusted R-squared:  0.9894 
#F-statistic: 374.7 on 3 and 9 DF,  p-value: 9.145e-10

r.squaredGLMM(Rs_SWC_cubic_high)
#R2m       R2c
#[1,] 0.9894379 0.9894379

AIC(Rs_SWC_cubic_high)
# -40.18459


# Linear (Medium)
Rs_SWC_linear_medium <- lm(Rs ~ SWC, data=subset(flux_int, Temp_Level=="Medium"))

# look at residuals
Rs_SWC_linear_medium.resid <- resid(Rs_SWC_linear_medium)
Rs_SWC_linear_medium.fitted <- fitted(Rs_SWC_linear_medium)
Rs_SWC_linear_medium.sqrt <- sqrt(abs(resid(Rs_SWC_linear_medium)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_linear_medium.fitted, Rs_SWC_linear_medium.resid, main='resid, Rs_SWC_linear_medium')
plot(Rs_SWC_linear_medium.fitted, Rs_SWC_linear_medium.sqrt, main='sqrt resid, Rs_SWC_linear_medium')
qqnorm(Rs_SWC_linear_medium.resid, main = 'Rs_SWC_linear_medium')
qqline(Rs_SWC_linear_medium.resid)
hist(Rs_SWC_linear_medium.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_linear_medium)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.59831    0.16286  -3.674  0.00281 ** 
#  SWC          0.25926    0.01946  13.326 5.89e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2016 on 13 degrees of freedom
#Multiple R-squared:  0.9318,	Adjusted R-squared:  0.9265 
#F-statistic: 177.6 on 1 and 13 DF,  p-value: 5.888e-09

r.squaredGLMM(Rs_SWC_linear_medium)
#R2m       R2c
#[1,] 0.9269235 0.9269235

AIC(Rs_SWC_linear_medium)
# -1.621824


### Quadratic (Medium)
Rs_SWC_quad_medium <- lm(Rs ~ SWC +SWC2, data=subset(flux_int, Temp_Level=="Medium"))

# look at residuals
Rs_SWC_quad_medium.resid <- resid(Rs_SWC_quad_medium)
Rs_SWC_quad_medium.fitted <- fitted(Rs_SWC_quad_medium)
Rs_SWC_quad_medium.sqrt <- sqrt(abs(resid(Rs_SWC_quad_medium)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_quad_medium.fitted, Rs_SWC_quad_medium.resid, main='resid, Rs_SWC_quad_medium')
plot(Rs_SWC_quad_medium.fitted, Rs_SWC_quad_medium.sqrt, main='sqrt resid, Rs_SWC_quad_medium')
qqnorm(Rs_SWC_quad_medium.resid, main = 'Rs_SWC_quad_medium')
qqline(Rs_SWC_quad_medium.resid)
hist(Rs_SWC_quad_medium.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_quad_medium)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.480718   0.428606  -3.455 0.004763 ** 
#  SWC          0.506286   0.114365   4.427 0.000825 ***
#  SWC2        -0.015369   0.007035  -2.185 0.049480 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1775 on 12 degrees of freedom
#Multiple R-squared:  0.9512,	Adjusted R-squared:  0.9431 
#F-statistic: 116.9 on 2 and 12 DF,  p-value: 1.351e-08

r.squaredGLMM(Rs_SWC_quad_medium)
#R2m       R2c
#[1,] 0.9435228 0.9435228

AIC(Rs_SWC_quad_medium)
#  -4.644504


# Cubic (Medium)
Rs_SWC_cubic_medium <- lm(Rs ~ SWC +SWC2+SWC3, data=subset(flux_int, Temp_Level=="Medium"))

# look at residuals
Rs_SWC_cubic_low.resid <- resid(Rs_SWC_cubic_low)
Rs_SWC_cubic_low.fitted <- fitted(Rs_SWC_cubic_low)
Rs_SWC_cubic_low.sqrt <- sqrt(abs(resid(Rs_SWC_cubic_low)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_quad_low.fitted, Rs_SWC_quad_low.resid, main='resid, Rs_SWC_quad_low')
plot(Rs_SWC_quad_low.fitted, Rs_SWC_quad_low.sqrt, main='sqrt resid, Rs_SWC_quad_low')
qqnorm(Rs_SWC_quad_low.resid, main = 'Rs_SWC_quad_low')
qqline(Rs_SWC_quad_low.resid)
hist(Rs_SWC_quad_low.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_cubic_medium)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -2.627555   2.339382  -1.123    0.285
#SWC          0.974393   0.945031   1.031    0.325
#SWC2        -0.073768   0.117197  -0.629    0.542
#SWC3         0.002275   0.004556   0.499    0.627

#Residual standard error: 0.1833 on 11 degrees of freedom
#Multiple R-squared:  0.9523,	Adjusted R-squared:  0.9393 
#F-statistic: 73.17 on 3 and 11 DF,  p-value: 1.495e-07

r.squaredGLMM(Rs_SWC_cubic_medium)
#R2m       R2c
#[1,] 0.9400439 0.9400439

AIC(Rs_SWC_cubic_medium)
# -2.980599

#Linear (Low)
Rs_SWC_linear_low <- lm(Rs ~ SWC, data=subset(flux_int, Temp_Level=="Low"))

# look at residuals
Rs_SWC_linear_low.resid <- resid(Rs_SWC_linear_low)
Rs_SWC_linear_low.fitted <- fitted(Rs_SWC_linear_low)
Rs_SWC_linear_low.sqrt <- sqrt(abs(resid(Rs_SWC_linear_low)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_linear_low.fitted, Rs_SWC_linear_low.resid, main='resid, Rs_SWC_linear_low')
plot(Rs_SWC_linear_low.fitted, Rs_SWC_linear_low.sqrt, main='sqrt resid, Rs_SWC_linear_low')
qqnorm(Rs_SWC_linear_low.resid, main = 'Rs_SWC_linear_low')
qqline(Rs_SWC_linear_low.resid)
hist(Rs_SWC_linear_low.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_linear_low)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.176440   0.136514   1.292    0.213    
#SWC         0.076350   0.009963   7.663  4.5e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2396 on 18 degrees of freedom
#Multiple R-squared:  0.7654,	Adjusted R-squared:  0.7524 
#F-statistic: 58.73 on 1 and 18 DF,  p-value: 4.495e-07

r.squaredGLMM(Rs_SWC_linear_low)
#R2m       R2c
#[1,] 0.7555586 0.7555586

AIC(Rs_SWC_linear_low)
# 3.495031



### Quadratic (Low)
Rs_SWC_quad_low <- lm(Rs ~ SWC +SWC2, data=subset(flux_int, Temp_Level=="Low"))

# look at residuals
Rs_SWC_quad_low.resid <- resid(Rs_SWC_quad_low)
Rs_SWC_quad_low.fitted <- fitted(Rs_SWC_quad_low)
Rs_SWC_quad_low.sqrt <- sqrt(abs(resid(Rs_SWC_quad_low)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_quad_low.fitted, Rs_SWC_quad_low.resid, main='resid, Rs_SWC_quad_low')
plot(Rs_SWC_quad_low.fitted, Rs_SWC_quad_low.sqrt, main='sqrt resid, Rs_SWC_quad_low')
qqnorm(Rs_SWC_quad_low.resid, main = 'Rs_SWC_quad_low')
qqline(Rs_SWC_quad_low.resid)
hist(Rs_SWC_quad_low.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_quad_low)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.043952   0.343400  -0.128   0.8997  
#SWC          0.120968   0.064416   1.878   0.0777 .
#SWC2        -0.001821   0.002597  -0.701   0.4926  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.243 on 17 degrees of freedom
#Multiple R-squared:  0.772,	Adjusted R-squared:  0.7452 
#F-statistic: 28.78 on 2 and 17 DF,  p-value: 3.487e-06

r.squaredGLMM(Rs_SWC_quad_low)
#R2m       R2c
#[1,] 0.7518362 0.7518362

AIC(Rs_SWC_quad_low)
#  4.924547

# Cubic (Low)
Rs_SWC_cubic_low <- lm(Rs ~ SWC +SWC2+SWC3, data=subset(flux_int, Temp_Level=="Low"))

# look at residuals
Rs_SWC_cubic_low.resid <- resid(Rs_SWC_cubic_low)
Rs_SWC_cubic_low.fitted <- fitted(Rs_SWC_cubic_low)
Rs_SWC_cubic_low.sqrt <- sqrt(abs(resid(Rs_SWC_cubic_low)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_SWC_cubic_low.fitted, Rs_SWC_cubic_low.resid, main='resid, Rs_SWC_cubic_low')
plot(Rs_SWC_cubic_low.fitted, Rs_SWC_cubic_low.sqrt, main='sqrt resid, Rs_SWC_cubic_low')
qqnorm(Rs_SWC_cubic_low.resid, main = 'Rs_SWC_cubic_low')
qqline(Rs_SWC_cubic_low.resid)
hist(Rs_SWC_cubic_low.resid)
par(mfrow=c(1,1))

summary(Rs_SWC_cubic_low)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.1453462  1.3125899   0.111    0.913
#SWC          0.0659156  0.3735537   0.176    0.862
#SWC2         0.0027617  0.0307194   0.090    0.929
#SWC3        -0.0001158  0.0007730  -0.150    0.883

#Residual standard error: 0.2503 on 16 degrees of freedom
#Multiple R-squared:  0.7723,	Adjusted R-squared:  0.7296 
#F-statistic: 18.09 on 3 and 16 DF,  p-value: 2.153e-05

r.squaredGLMM(Rs_SWC_cubic_low)
#R2m       R2c
#[1,] 0.7407017 0.7407017

AIC(Rs_SWC_cubic_low)
# 6.896532


###################################################################################################################################
###### Create graphs of Rs response to Ts and SWC

# Read as data table
flux_int.graph <- as.data.table(flux_int)

# Rs response to Ts under different levels of SWC
flux_int.graph[SWC_Level=="Verydry",':='(level.vwc="Verydry")] 

flux_int.graph[SWC_Level=="Dry",
               ':='(Ts.response.thresholds.quad =-1.0911843+0.2957112*Ts-0.0077410*Ts2,
                    level.vwc="Dry")] 

flux_int.graph[SWC_Level=="Wet",
               ':='(Ts.response.thresholds.exp = 1.073474*exp(0.04216*Ts),
                    level.vwc="Wet")] 

flux_int.graph[SWC_Level=="Wet",
               ':='(Ts.response.thresholds.linear = 0.93048+(0.07496*Ts),
                    level.vwc="Wet")] 

flux_int.graph[SWC_Level=="Wet",
               ':='(Ts.response.thresholds.quad = 1.113317+(0.031427*Ts)+(0.002073*Ts2),
                    level.vwc="Wet")] 

# Specify order of SWC levels
flux_int.graph$level.vwc <- factor(flux_int.graph$level.vwc, levels = c("Wet", "Dry", "Verydry"))

# Graph (Fig. 3a)
Ts_response_thresholds <- ggplot(flux_int.graph, aes(x=Ts))+
  geom_point(aes(y=Rs, colour = level.vwc))+
  geom_smooth(aes(y=Ts.response.thresholds.exp, colour = level.vwc), size=1, linetype="dotted", se=F) +
  geom_smooth(aes(y=Ts.response.thresholds.quad, colour = level.vwc), size=0.7, linetype="dotdash", se=F) +
  scale_color_manual(values=c("Wet"="blue","Dry" ="cyan3",
                              "Verydry"="green2"),
                     labels=c(">9%","6-9%","4-6%"),
                     name="SWC")+
  labs (y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1})),
        x=(expression("Soil temperature ("~degree~"C)")),
        subtitle="") +
  ylim(0,3)+
  theme(plot.title = element_text(color = "black", size = 12)) +
  theme(axis.text.y =  element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.justification = c(1,1),
        legend.position = c(1,1),
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm")) 

# Rs response to SWC under different Ts levels 
flux_int.graph[Temp_Level=="High",
               ':='(SWC.response.thresholds.quad = -5.04955+1.50276*SWC-0.07744*SWC2,
                    level.ts="High")]

flux_int.graph[Temp_Level=="High",
               ':='(SWC.response.thresholds.linear = -2.18471+0.55443*SWC,
                    level.ts="High")]

flux_int.graph[Temp_Level=="High",
               ':='(SWC.response.thresholds.cubic = 43.00110-(22.92118*SWC)+(4.01425*SWC2)-(0.22607*SWC3),
                    level.ts="High")]

flux_int.graph[Temp_Level=="Medium",
               ':='(SWC.response.thresholds.quad =-1.480718+0.506286*SWC-0.015369*SWC2,
                    level.ts="Medium")] 

flux_int.graph[Temp_Level=="Medium",
               ':='(SWC.response.thresholds.linear =-0.59831+0.25926*SWC,
                    level.ts="Medium")] 

flux_int.graph[Temp_Level=="Medium",
               ':='(SWC.response.thresholds.cubic = -2.627555+(0.974393*SWC)-(0.073768*SWC2)+(0.002275*SWC3),
                    level.ts="Medium")] 

flux_int.graph[Temp_Level=="Low",
               ':='(SWC.response.thresholds.linear = 0.176440+0.076350*SWC,
                    level.ts="Low")] 

flux_int.graph[Temp_Level=="Low",
               ':='(SWC.response.thresholds.cubic = 0.1453462+0.0659156*SWC+0.0027617*SWC2-0.0001158*SWC3,
                    level.ts="Low")] 

flux_int.graph[Temp_Level=="Low",
               ':='(SWC.response.thresholds.quad = -0.043952+0.120968*SWC-0.001821*SWC2,
                    level.ts="Low")]

# Graph (Fig. 3b)
SWC_response_thresholds <- ggplot(flux_int.graph, aes(x=SWC))+
  geom_point(aes(y=Rs, colour = Temp_Level))+
  geom_smooth(data=subset(flux_int.graph, Temp_Level=="Low"), aes(y=SWC.response.thresholds.linear, colour = Temp_Level), 
              size=0.7,se=F, linetype="solid") +
  geom_smooth(data=subset(flux_int.graph, Temp_Level=="Medium"|Temp_Level=="High"), aes(y=SWC.response.thresholds.cubic, colour = Temp_Level), 
              size=0.7,se=F, linetype="dashed") +
  scale_colour_manual(values=c("High"="red3","Medium" ="darkorange2",
                               "Low"="palevioletred1"),
                      breaks=c("High", "Medium", "Low"),
                      labels=c(">20°C","<=20 - >10°C","<=10°C"),
                      name="Ts")+
  labs(y="",
       x=(expression("Soil water content (%)")),
       subtitle="") +
  ylim(0,3)+
  theme(plot.title = element_text(color = "black", size = 12)) +
  theme(axis.text.y =  element_blank(), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.ticks.length=unit(-1.5,"mm"),
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.justification = c(1,1),
        legend.position = c(0.38,1),
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm")) 

# Arrange graphs of Rs response to Ts and SWC
ggarrange(Ts_response_thresholds, SWC_response_thresholds + x.axis, 
          labels = c("A", "B"),
          ncol = 2)

ggarrange(Ts_response_thresholds, SWC_response_thresholds, 
          labels = c("A", "B"),
          ncol = 2)


ggarrange(Ts_response_thresholds, 
          SWC_response_thresholds + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank()),
          labels = c("A", "B"),
          nrow = 1)
