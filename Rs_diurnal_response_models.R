# load packages
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(emmeans)

# load cvs file
Rs_diurnal_all = read.csv('/Users/jessicamontes/Documents/ThesisData/0_ThesisPaper/Final_Paper/JGR_Biogeosciences/Rs_Ts_SWC_diurnal.csv')
#View(Rs_diurnal_all)

# set order of months
Rs_diurnal_all$month <- as.factor(Rs_diurnal_all$month)
Rs_diurnal_all$month <- factor(Rs_diurnal_all$month, levels = c("Jun (SWC = 6.6%)", "Jul (SWC = 6%)", "Aug (SWC = 5.7%)", "Oct (SWC = 4.7%)", "Nov (SWC = 9.3%)",
                                                                "Dec (SWC = 5.5%)", "Jan (SWC = 14%)", "Feb (SWC = 19%)", "Mar (SWC = 16 %)",  "Apr (SWC = 11.7%)", "May (SWC = 7.5%)"))


# Averages for 5-day periods of each month
Rs_diurnal_monthly_avg <- group_by(Rs_diurnal_all, month, season)
Rs_diurnal_monthly_avg <- summarize(Rs_diurnal_monthly_avg, Flux_mean=mean(Flux, na.rm=T),
                                    Ts_mean=mean(Ts, na.rm=T), 
                                    SWC_mean=mean(SWC, na.rm=T))
Rs_diurnal_monthly_avg

#month             Flux_mean Ts_mean SWC_mean
#<fct>                 <dbl>   <dbl>    <dbl>
#  1 Jun (SWC = 6.6%)      1.58    22.4      6.64
#2 Jul (SWC = 6%)        1.09    25.3      6.01
#3 Aug (SWC = 5.7%)      0.799   30.6      5.68
#4 Oct (SWC = 4.7%)      0.476   20.0      4.74
#5 Nov (SWC = 9.3%)      1.62     8.99     9.31
#6 Dec (SWC = 5.5%)      0.474    6.21     5.54
#7 Jan (SWC = 14%)       1.27     7.51    14.0 
#8 Feb (SWC = 19%)       1.22     8.17    19.2 
#9 Mar (SWC = 16 %)      1.62     8.69    16.1 
#10 Apr (SWC = 11.7%)     2.34    14.4     11.7 
#11 May (SWC = 7.5%)      1.59    17.2      7.45


# Hourly averages for selected 5-day periods of each month
Rs_diurnal_monthly <- group_by(Rs_diurnal_all, hour, month, season)
Rs_diurnal_monthly <- summarize(Rs_diurnal_monthly, Flux_mean=mean(Flux, na.rm=T),
                                Ts_mean=mean(Ts, na.rm=T), 
                                SWC_mean=mean(SWC, na.rm=T))
Rs_diurnal_monthly

# Create new dataframe of hourly averages
Rs_diurnal_monthly <- data.frame(Rs_diurnal_monthly)

# Linear mixed model
diurnal_Ts_model <- lm(Flux_mean~Ts_mean*month, data=Rs_diurnal_monthly)
summary(diurnal_Ts_model)
anova(diurnal_Ts_model)
#Analysis of Variance Table

#Response: Flux_mean
#Df Sum Sq Mean Sq F value    Pr(>F)    
#Ts_mean         1  0.069  0.0686   19.38 1.608e-05 ***
#  month          10 77.845  7.7845 2198.75 < 2.2e-16 ***
#  Ts_mean:month  10  4.162  0.4162  117.55 < 2.2e-16 ***
#  Residuals     242  0.857  0.0035                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# month had a significant effect
# there is a significant interaction between month & soil temperature

emmeans(diurnal_Ts_model, pairwise ~ month)

# Fig 4. Diurnal relationship between Rs and Ts during 5-day periods for each month (except September)
ggplot(data=Rs_diurnal_monthly, aes(x=Ts_mean, y=Flux_mean, color=month)) +
  #geom_point(aes(colour=month, shape=season)) +
  geom_point(shape=17, size=2) +
  #geom_smooth(data=subset(Rs_diurnal_monthly, month=="Jul (SWC = 6%)"|month=="Aug (SWC = 5.7%)"|month=="Oct (SWC = 4.7%)"|month=="Nov (SWC = 9.3%)"|month=="Dec (SWC = 5.5%)"|month=="Jan (SWC = 14%)"|month=="Feb (SWC = 19%)"|month=="Mar (SWC = 16 %)"|month=="Apr (SWC = 11.7%)"|month=="May (SWC = 7.5%)"), 
  #method='lm', se=F, size=0.7)+
  geom_smooth(data=subset(Rs_diurnal_monthly, month=="Jul (SWC = 6%)"|month=="Aug (SWC = 5.7%)"|month=="Oct (SWC = 4.7%)"|month=="Nov (SWC = 9.3%)"|month=="Dec (SWC = 5.5%)"|month=="Jan (SWC = 14%)"|month=="Feb (SWC = 19%)"|month=="Mar (SWC = 16 %)"|month=="Apr (SWC = 11.7%)"|month=="May (SWC = 7.5%)"), 
              method='lm', se=F, size=0.5)+
  labs(x="Soil temperature (°C)",y=expression(Soil~respiration~(mu~mol~CO[2]~m^{-2}~s^{-1}))) + 
  stat_poly_eq(formula = y ~ x, 
               #aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.04,
               size=3) + # sets vertical spacing 
  xlim(0,50) +
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
        #legend.position = c(3,1),
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm")) 

# Diurnal relationship between Rs and SWC during 5-day periods for each month (except September)
ggplot(data=Rs_diurnal_monthly, aes(x=SWC_mean, y=Flux_mean, color=month)) +
  geom_point(aes(colour=month, shape=season)) +
  geom_smooth(data=subset(Rs_diurnal_monthly, month=="Jul (SWC = 6%)"|month=="Aug (SWC = 5.7%)"|month=="Oct (SWC = 4.7%)"|month=="Nov (SWC = 9.3%)"|month=="Dec (SWC = 5.5%)"|month=="Jan (SWC = 14%)"|month=="Feb (SWC = 19%)"|month=="Mar (SWC = 16 %)"|month=="Apr (SWC = 11.7%)"|month=="May (SWC = 7.5%)"), 
              method='lm', se=F, size=0.7)+
  labs(x="Soil temperature (°C)",y=expression(Soil~respiration~(mu~mol~CO[2]~m^{-2}~s^{-1}))) + 
  stat_poly_eq(formula = y ~ x, 
               #aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.04,
               size=3) + # sets vertical spacing 
  # xlim(0,50) +
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
        #legend.position = c(3,1),
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm")) 
