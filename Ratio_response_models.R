# Remove all Working variables
rm(list=ls())

# load packages
library(MuMIn) # for r.squaredGLMM() 
library(ggpmisc) # for stat_poly_eq() 

# load csv file
flux_int_og = read.csv('/Users/jessicamontes/Documents/ThesisData/0_ThesisPaper/Final_Paper/JGR_Biogeosciences/Rs_Reco_5day.csv')
#View(flux_int)

# Create squared and cubic values of SWC
flux_int_og$SWC2 <- (flux_int_og$SWC)^2
flux_int_og$SWC3 <- (flux_int_og$SWC)^3

# remove NAs
flux_int <- na.omit(flux_int_og)

# specify order of factors
flux_int_og$season <- factor(flux_int_og$season, levels = c("summer", "fall", "winter", "spring"))
flux_int_og$seasons <- factor(flux_int_og$seasons, levels = c("dry", "wet"))

##############################################################################################################
# SOIL WATER CONTENT 
##############################################################################################################

# DRY SEASON (linear fit)
Ratio_SWC_linear_dry <- lm(Ratio ~ SWC, data=subset(flux_int, seasons=="dry"))

# look at residuals
Ratio_SWC_linear_dry.resid <- resid(Ratio_SWC_linear_dry)
Ratio_SWC_linear_dry.fitted <- fitted(Ratio_SWC_linear_dry)
Ratio_SWC_linear_dry.sqrt <- sqrt(abs(resid(Ratio_SWC_linear_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_SWC_linear_dry.fitted, Ratio_SWC_linear_dry.resid, main='resid, Ratio_SWC_linear_dry')
plot(Ratio_SWC_linear_dry.fitted, Ratio_SWC_linear_dry.sqrt, main='sqrt resid, Ratio_SWC_linear_dry')
qqnorm(Ratio_SWC_linear_dry.resid, main = 'Ratio_SWC_linear_dry')
qqline(Ratio_SWC_linear_dry.resid)
hist(Ratio_SWC_linear_dry.resid)
par(mfrow=c(1,1))

summary(Ratio_SWC_linear_dry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.33262    0.16793  -1.981 0.066276 .  
#SWC          0.13995    0.02813   4.975 0.000166 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1111 on 15 degrees of freedom
#Multiple R-squared:  0.6226,	Adjusted R-squared:  0.5975 
#F-statistic: 24.75 on 1 and 15 DF,  p-value: 0.0001663

r.squaredGLMM(Ratio_SWC_linear_dry)
#R2m       R2c
#[1,] 0.6073395 0.6073395

# WET SEASON (linear fit)
Ratio_SWC_linear_wet <- lm(Ratio ~ SWC, data=subset(flux_int, seasons=="wet"))

# look at residuals
Ratio_SWC_linear_wet.resid <- resid(Ratio_SWC_linear_wet)
Ratio_SWC_linear_wet.fitted <- fitted(Ratio_SWC_linear_wet)
Ratio_SWC_linear_wet.sqrt <- sqrt(abs(resid(Ratio_SWC_linear_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_SWC_linear_wet.fitted, Ratio_SWC_linear_wet.resid, main='resid, Ratio_SWC_linear_wet')
plot(Ratio_SWC_linear_wet.fitted, Ratio_SWC_linear_wet.sqrt, main='sqrt resid, Ratio_SWC_linear_wet')
qqnorm(Ratio_SWC_linear_wet.resid, main = 'Ratio_SWC_linear_wet')
qqline(Ratio_SWC_linear_wet.resid)
hist(Ratio_SWC_linear_wet.resid)
par(mfrow=c(1,1))

summary(Ratio_SWC_linear_wet)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.610814   0.387910   1.575    0.256
#SWC         0.005489   0.022025   0.249    0.826

#Residual standard error: 0.1002 on 2 degrees of freedom
#Multiple R-squared:  0.03012,	Adjusted R-squared:  -0.4548 
#F-statistic: 0.06211 on 1 and 2 DF,  p-value: 0.8265

r.squaredGLMM(Ratio_SWC_linear_wet)
#R2m        R2c
#[1,] 0.02028215 0.02028215


# Figure 8b:Rs/Reco response to SWC grouped by season following linear fit
Ratio_SWC_seasonal <- ggplot(data=flux_int, aes(x=SWC, y=Ratio, color=seasons)) +
  geom_point() +
  geom_smooth(data=subset(flux_int, seasons=="dry"), 
              aes(x=SWC, y=Ratio,color=seasons), method="lm", se=F, size=0.5)+
  labs(y=expression(""),
       x=expression("Soil water content (%)"),
       title=bquote(bold("B"))) +
  stat_poly_eq(formula = y ~ x, 
               #aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "left",
               label.y.npc = "top",
               vstep = 0.09,
               size=3) +
  ylim(0.15,1) +
  theme(plot.title = element_text(color = "black", size = 12)) +
  theme(axis.text.y = element_blank(), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.ticks.length=unit(-1.5,"mm"),
        #legend.key.size=unit(4,"mm"),
        #legend.key = element_rect(fill=NA, colour=NA),
        #legend.title = element_text(size=9),
        #legend.text = element_text(size=9),
        #legend.justification = c(1,1),
        #legend.position = c(1,1),
        legend.position = "none",
        legend.background = element_rect(colour="black"),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm")) 


# ALL DATA (cubic fit)
Ratio_SWC_cubic_interval <- lm(Ratio ~ SWC + SWC2 + SWC3, data=flux_int)

# look at residuals
Ratio_SWC_cubic_interval.resid <- resid(Ratio_SWC_cubic_interval)
Ratio_SWC_cubic_interval.fitted <- fitted(Ratio_SWC_cubic_interval)
Ratio_SWC_cubic_interval.sqrt <- sqrt(abs(resid(Ratio_SWC_cubic_interval)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_SWC_cubic_interval.fitted, Ratio_SWC_cubic_interval.resid, main='resid, Ratio_SWC_cubic_interval')
plot(Ratio_SWC_cubic_interval.fitted, Ratio_SWC_cubic_interval.sqrt, main='sqrt resid, Ratio_SWC_cubic_interval')
qqnorm(Ratio_SWC_cubic_interval.resid, main = 'Ratio_SWC_cubic_interval')
qqline(Ratio_SWC_cubic_interval.resid)
hist(Ratio_SWC_cubic_interval.resid)
par(mfrow=c(1,1))

summary(Ratio_SWC_cubic_interval)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -1.1910592  0.3970763  -3.000  0.00806 **
#  SWC          0.4706243  0.1249879   3.765  0.00154 **
#  SWC2        -0.0359576  0.0117784  -3.053  0.00720 **
#  SWC3         0.0008647  0.0003378   2.560  0.02030 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1077 on 17 degrees of freedom
#Multiple R-squared:  0.7012,	Adjusted R-squared:  0.6485 
#F-statistic:  13.3 on 3 and 17 DF,  p-value: 0.000102

r.squaredGLMM(Ratio_SWC_cubic_interval)
#R2m       R2c
#[1,] 0.6660996 0.6660996


# Figure 8a:Rs/Reco response to SWC including all data following cubic fit
flux_int$SWC.response.cubic <- -1.1910592 + 0.4706243*(flux_int$SWC) - 0.0359576*(flux_int$SWC2) + 0.0008647*(flux_int$SWC3)

Ratio_SWC_alldata <- ggplot(data=flux_int, aes(x=SWC, y=Ratio)) +
  geom_point() +
  #geom_smooth(method="lm",color="black", size=0.5, se=F )+
  #geom_smooth(aes(y=SWC.response.quad), color="black", size=0.5, linetype="dotdash")+
  geom_smooth(aes(y=SWC.response.cubic), color="black", size=0.7, se=F, linetype="dashed")+
  #labs (y=expression(R[s]/R[eco]),
  labs (y=expression(Rs/Reco),
        x=expression("Soil water content (%)"),
        title=bquote(bold("A"))) +
  ylim(0.15,1)+
  theme(plot.title = element_text(color = "black", size = 12)) +
  theme(axis.text.y = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.ticks.length=unit(-1.5,"mm"),
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


# arrange Figure 8a and 8b together
ggarrange(Ratio_SWC_alldata, Ratio_SWC_seasonal, ncol=2, nrow=1)


##############################################################################################################
# SOIL TEMPERATURE 
##############################################################################################################

# DRY SEASON
Ratio_Ts_linear_dry <- lm(Ratio ~ Ts, data=subset(flux_int, seasons=="dry"))

# look at residuals
Ratio_Ts_linear_dry.resid <- resid(Ratio_Ts_linear_dry)
Ratio_Ts_linear_dry.fitted <- fitted(Ratio_Ts_linear_dry)
Ratio_Ts_linear_dry.sqrt <- sqrt(abs(resid(Ratio_Ts_linear_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Ts_linear_dry.fitted, Ratio_Ts_linear_dry.resid, main='resid, Ratio_Ts_linear_dry')
plot(Ratio_Ts_linear_dry.fitted, Ratio_Ts_linear_dry.sqrt, main='sqrt resid, Ratio_Ts_linear_dry')
qqnorm(Ratio_Ts_linear_dry.resid, main = 'Ratio_Ts_linear_dry')
qqline(Ratio_Ts_linear_dry.resid)
hist(Ratio_Ts_linear_dry.resid)
par(mfrow=c(1,1))

summary(Ratio_Ts_linear_dry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.195946   0.198161   0.989    0.338
#Ts          0.012957   0.008487   1.527    0.148

#Residual standard error: 0.1682 on 15 degrees of freedom
#Multiple R-squared:  0.1345,	Adjusted R-squared:  0.07678 
#F-statistic: 2.331 on 1 and 15 DF,  p-value: 0.1477

r.squaredGLMM(Ratio_Ts_linear_dry)
#R2m       R2c
#[1,] 0.1271483 0.1271483


# WET SEASON
Ratio_Ts_linear_wet <- lm(Ratio ~ Ts, data=subset(flux_int, seasons=="wet"))

# look at residuals
Ratio_Ts_linear_wet.resid <- resid(Ratio_Ts_linear_wet)
Ratio_Ts_linear_wet.fitted <- fitted(Ratio_Ts_linear_wet)
Ratio_Ts_linear_wet.sqrt <- sqrt(abs(resid(Ratio_Ts_linear_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Ts_linear_wet.fitted, Ratio_Ts_linear_wet.resid, main='resid, Ratio_Ts_linear_wet')
plot(Ratio_Ts_linear_wet.fitted, Ratio_Ts_linear_wet.sqrt, main='sqrt resid, Ratio_Ts_linear_wet')
qqnorm(Ratio_Ts_linear_wet.resid, main = 'Ratio_Ts_linear_wet')
qqline(Ratio_Ts_linear_wet.resid)
hist(Ratio_Ts_linear_wet.resid)
par(mfrow=c(1,1))

summary(Ratio_Ts_linear_wet)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.59198    0.11328   5.226   0.0347 *
#  Ts           0.01674    0.01545   1.084   0.3918  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.08073 on 2 degrees of freedom
#Multiple R-squared:  0.3699,	Adjusted R-squared:  0.05491 
#F-statistic: 1.174 on 1 and 2 DF,  p-value: 0.3918

r.squaredGLMM(Ratio_Ts_linear_wet)
#R2m      R2c
#[1,] 0.281314 0.281314


##############################################################################################################
# AIR TEMPERATURE 
##############################################################################################################

# DRY SEASON
Ratio_AirT_linear_dry <- lm(Ratio ~ AirT, data=subset(flux_int, seasons=="dry"))

# look at residuals
Ratio_AirT_linear_dry.resid <- resid(Ratio_AirT_linear_dry)
Ratio_AirT_linear_dry.fitted <- fitted(Ratio_AirT_linear_dry)
Ratio_AirT_linear_dry.sqrt <- sqrt(abs(resid(Ratio_AirT_linear_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_AirT_linear_dry.fitted, Ratio_AirT_linear_dry.resid, main='resid, Ratio_AirT_linear_dry')
plot(Ratio_AirT_linear_dry.fitted, Ratio_AirT_linear_dry.sqrt, main='sqrt resid, Ratio_AirT_linear_dry')
qqnorm(Ratio_AirT_linear_dry.resid, main = 'Ratio_AirT_linear_dry')
qqline(Ratio_AirT_linear_dry.resid)
hist(Ratio_AirT_linear_dry.resid)
par(mfrow=c(1,1))

summary(Ratio_AirT_linear_dry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) 0.388777   0.217634   1.786   0.0943 .
#AirT        0.004724   0.009760   0.484   0.6354  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1794 on 15 degrees of freedom
#Multiple R-squared:  0.01538,	Adjusted R-squared:  -0.05026 
#F-statistic: 0.2343 on 1 and 15 DF,  p-value: 0.6354

r.squaredGLMM(Ratio_AirT_linear_dry)
#R2m        R2c
#[1,] 0.01443147 0.01443147


# WET SEASON
Ratio_AirT_linear_wet <- lm(Ratio ~ AirT, data=subset(flux_int, seasons=="wet"))

# look at residuals
Ratio_AirT_linear_wet.resid <- resid(Ratio_AirT_linear_wet)
Ratio_AirT_linear_wet.fitted <- fitted(Ratio_AirT_linear_wet)
Ratio_AirT_linear_wet.sqrt <- sqrt(abs(resid(Ratio_AirT_linear_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_AirT_linear_wet.fitted, Ratio_AirT_linear_wet.resid, main='resid, Ratio_AirT_linear_wet')
plot(Ratio_AirT_linear_wet.fitted, Ratio_AirT_linear_wet.sqrt, main='sqrt resid, Ratio_AirT_linear_wet')
qqnorm(Ratio_AirT_linear_wet.resid, main = 'Ratio_AirT_linear_wet')
qqline(Ratio_AirT_linear_wet.resid)
hist(Ratio_AirT_linear_wet.resid)
par(mfrow=c(1,1))

summary(Ratio_AirT_linear_wet)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.52838    0.12293   4.298   0.0501 .
#AirT         0.01944    0.01286   1.512   0.2697  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.06947 on 2 degrees of freedom
#Multiple R-squared:  0.5334,	Adjusted R-squared:    0.3 
#F-statistic: 2.286 on 1 and 2 DF,  p-value: 0.2697

r.squaredGLMM(Ratio_AirT_linear_wet)
#R2m       R2c
#[1,] 0.4324643 0.4324643


##############################################################################################################
# AVERAGE RAINFALL 
##############################################################################################################

# DRY SEASON
Ratio_Rain_linear_dry <- lm(Ratio ~ Rainfall_avg, data=subset(flux_int, seasons=="dry"))

# look at residuals
Ratio_Rain_linear_dry.resid <- resid(Ratio_Rain_linear_dry)
Ratio_Rain_linear_dry.fitted <- fitted(Ratio_Rain_linear_dry)
Ratio_Rain_linear_dry.sqrt <- sqrt(abs(resid(Ratio_Rain_linear_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Rain_linear_dry.fitted, Ratio_Rain_linear_dry.resid, main='resid, Ratio_Rain_linear_dry')
plot(Ratio_Rain_linear_dry.fitted, Ratio_Rain_linear_dry.sqrt, main='sqrt resid, Ratio_Rain_linear_dry')
qqnorm(Ratio_Rain_linear_dry.resid, main = 'Ratio_Rain_linear_dry')
qqline(Ratio_Rain_linear_dry.resid)
hist(Ratio_Rain_linear_dry.resid)
par(mfrow=c(1,1))

summary(Ratio_Rain_linear_dry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.49387    0.04682  10.549 2.46e-08 ***
#  Rainfall_avg -0.02620    0.22935  -0.114    0.911    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1807 on 15 degrees of freedom
#Multiple R-squared:  0.000869,	Adjusted R-squared:  -0.06574 
#F-statistic: 0.01305 on 1 and 15 DF,  p-value: 0.9106

r.squaredGLMM(Ratio_Rain_linear_dry)
#R2m          R2c
#[1,] 0.0008147257 0.0008147257


# WET SEASON
Ratio_Rain_linear_wet <- lm(Ratio ~ Rainfall_avg, data=subset(flux_int, seasons=="wet"))

# look at residuals
Ratio_Rain_linear_wet.resid <- resid(Ratio_Rain_linear_wet)
Ratio_Rain_linear_wet.fitted <- fitted(Ratio_Rain_linear_wet)
Ratio_Rain_linear_wet.sqrt <- sqrt(abs(resid(Ratio_Rain_linear_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Rain_linear_wet.fitted, Ratio_Rain_linear_wet.resid, main='resid, Ratio_Rain_linear_wet')
plot(Ratio_Rain_linear_wet.fitted, Ratio_Rain_linear_wet.sqrt, main='sqrt resid, Ratio_Rain_linear_wet')
qqnorm(Ratio_Rain_linear_wet.resid, main = 'Ratio_Rain_linear_wet')
qqline(Ratio_Rain_linear_wet.resid)
hist(Ratio_Rain_linear_wet.resid)
par(mfrow=c(1,1))

summary(Ratio_Rain_linear_wet)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   0.73506    0.04368  16.829  0.00351 **
#  Rainfall_avg -0.01463    0.01140  -1.283  0.32806   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07532 on 2 degrees of freedom
#Multiple R-squared:  0.4515,	Adjusted R-squared:  0.1773 
#F-statistic: 1.646 on 1 and 2 DF,  p-value: 0.3281

r.squaredGLMM(Ratio_Rain_linear_wet)
#R2m       R2c
#[1,] 0.3543339 0.3543339


##############################################################################################################
# ACCUMULATED RAINFALL 
##############################################################################################################

# DRY SEASON
Ratio_Rain_sum_linear_dry <- lm(Ratio ~ Rainfall_sum, data=subset(flux_int, seasons=="dry"))

# look at residuals
Ratio_Rain_sum_linear_dry.resid <- resid(Ratio_Rain_sum_linear_dry)
Ratio_Rain_sum_linear_dry.fitted <- fitted(Ratio_Rain_sum_linear_dry)
Ratio_Rain_sum_linear_dry.sqrt <- sqrt(abs(resid(Ratio_Rain_sum_linear_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Rain_sum_linear_dry.fitted, Ratio_Rain_sum_linear_dry.resid, main='resid, Ratio_Rain_sum_linear_dry')
plot(Ratio_Rain_sum_linear_dry.fitted, Ratio_Rain_sum_linear_dry.sqrt, main='sqrt resid, Ratio_Rain_sum_linear_dry')
qqnorm(Ratio_Rain_sum_linear_dry.resid, main = 'Ratio_Rain_sum_linear_dry')
qqline(Ratio_Rain_sum_linear_dry.resid)
hist(Ratio_Rain_sum_linear_dry.resid)
par(mfrow=c(1,1))

summary(Ratio_Rain_sum_linear_dry)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.49387    0.04682  10.549 2.46e-08 ***
#  Rainfall_avg -0.02620    0.22935  -0.114    0.911    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1807 on 15 degrees of freedom
#Multiple R-squared:  0.000869,	Adjusted R-squared:  -0.06574 
#F-statistic: 0.01305 on 1 and 15 DF,  p-value: 0.9106

r.squaredGLMM(Ratio_Rain_sum_linear_dry)
#R2m          R2c
#[1,] 0.0008147257 0.0008147257


# WET SEASON
Ratio_Rain_sum_linear_wet <- lm(Ratio ~ Rainfall_sum, data=subset(flux_int, seasons=="wet"))

# look at residuals
Ratio_Rain_sum_linear_wet.resid <- resid(Ratio_Rain_sum_linear_wet)
Ratio_Rain_sum_linear_wet.fitted <- fitted(Ratio_Rain_sum_linear_wet)
Ratio_Rain_sum_linear_wet.sqrt <- sqrt(abs(resid(Ratio_Rain_sum_linear_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Ratio_Rain_sum_linear_wet.fitted, Ratio_Rain_sum_linear_wet.resid, main='resid, Ratio_Rain_sum_linear_wet')
plot(Ratio_Rain_sum_linear_wet.fitted, Ratio_Rain_sum_linear_wet.sqrt, main='sqrt resid, Ratio_Rain_sum_linear_wet')
qqnorm(Ratio_Rain_sum_linear_wet.resid, main = 'Ratio_Rain_sum_linear_wet')
qqline(Ratio_Rain_sum_linear_wet.resid)
hist(Ratio_Rain_sum_linear_wet.resid)
par(mfrow=c(1,1))

summary(Ratio_Rain_sum_linear_wet)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   0.735061   0.043677  16.829  0.00351 **
#  Rainfall_sum -0.002926   0.002281  -1.283  0.32806   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07532 on 2 degrees of freedom
#Multiple R-squared:  0.4515,	Adjusted R-squared:  0.1773 
#F-statistic: 1.646 on 1 and 2 DF,  p-value: 0.3281

r.squaredGLMM(Ratio_Rain_sum_linear_wet)
#R2m       R2c
#[1,] 0.3543339 0.3543339

##############################################################################################################
#   Air - soil temperature
##############################################################################################################

# Figure 9: Rs/Reco linear response to difference between air and soil temperature
ggplot(data=flux_int, aes(x=Tair_Ts, y=Ratio, color=seasons)) +
  geom_point() +
  geom_smooth(data=subset(flux_int, seasons=="dry"), 
              aes(x=Tair_Ts, y=Ratio,color=seasons), method="lm", se=F, size=0.5)+
  #labs(y=expression(R[soil]/R[eco]),
  labs(y=expression(Rs/Reco),
       x=expression("Air temperature - Soil temperature (°C)"),
       title=bquote(bold("C"))) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "left",
               label.y.npc = "top",
               vstep = 0.09,
               size=3) +
  ylim(0.15,1) +
  theme(plot.title = element_text(color = "black", size = 12)) +
  theme(axis.text.y = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin:adjust text distance from axis
        axis.text.x = element_text(size=10,margin=unit(c(2,2,2,2),"mm")), # margin: (?, tick text, ?, legend title)
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(size=10),
        #axis.ticks.length=unit(-1.5,"mm"),
        legend.key.size=unit(4,"mm"),
        legend.key = element_rect(fill=NA, colour=NA),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        legend.justification = c(1,1),
        #legend.position = c(1,1),
        legend.position = "none",
        legend.background = element_rect(colour="black"),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))



