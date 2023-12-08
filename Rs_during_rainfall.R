# clean brain
rm(list=ls())

library(ggpubr)

# load csv file
Rc_rs = read.csv('/Users/jessicamontes/Documents/ThesisData/0_ThesisPaper/Final_Paper/JGR_Biogeosciences/Rc_rs.csv')
#View(Rc_rs) 

# set categorical factors
Rc_rs$seasons <- as.factor(Rc_rs$seasons)
Rc_rs$rain_time <- as.factor(Rc_rs$rain_time)

# remove outliers of dry season
Rc_rs$Rs[1] <- NA
Rc_rs$Rs[2] <- NA
Rc_rs$Rs[1] <- NA
Rc_rs$Rs[2] <- NA
Rc_rs$Ts[1] <- NA
Rc_rs$Ts[2] <- NA
Rc_rs$SWC[1] <- NA
Rc_rs$SWC[2] <- NA

############################# Differences between soil respiration before/after rainfall #############################
## Linear mixed models 
# the date before the rainfall event as a random factor and the timing of the rainfall event (before/after) as a fixed factor

# Dry season
Rs_raintime_dry <- lmer(Rs ~ rain_time +
                          (1|Date_Before_Rain), 
                        data = subset(Rc_rs, seasons=="dry"))

# look at residuals
Rs_raintime_dry.resid <- resid(Rs_raintime_dry)
Rs_raintime_dry.fitted <- fitted(Rs_raintime_dry)
Rs_raintime_dry.sqrt <- sqrt(abs(resid(Rs_raintime_dry)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_raintime_dry.fitted, Rs_raintime_dry.resid, main='resid, Rs_raintime_dry')
plot(Rs_raintime_dry.fitted, Rs_raintime_dry.sqrt, main='sqrt resid, Rs_raintime_dry')
qqnorm(Rs_raintime_dry.resid, main = 'Rs_raintime_dry')
qqline(Rs_raintime_dry.resid)
hist(Rs_raintime_dry.resid)
par(mfrow=c(1,1))

# search for residual outliers
View(Rs_raintime_dry.resid)
Rc_rs$Rs[13] <- NA

# R squared value
r.squaredGLMM(Rs_raintime_dry)
#R2m       R2c
#[1,] 0.6513958 0.7311134

summary(Rs_raintime_dry)
#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)      0.65766    0.08056 6.50583   8.164 0.000119 ***
#  rain_timeDuring  0.41789    0.09545 4.50673   4.378 0.009096 ** 


# Wet season
Rs_raintime_wet <- lmer(Rs ~ rain_time +
                          (1|Date_Before_Rain), 
                        data = subset(Rc_rs, seasons=="wet"))

# look at residuals
Rs_raintime_wet.resid <- resid(Rs_raintime_wet)
Rs_raintime_wet.fitted <- fitted(Rs_raintime_wet)
Rs_raintime_wet.sqrt <- sqrt(abs(resid(Rs_raintime_wet)))

# graph
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(Rs_raintime_wet.fitted, Rs_raintime_wet.resid, main='resid, Rs_raintime_wet')
plot(Rs_raintime_wet.fitted, Rs_raintime_wet.sqrt, main='sqrt resid, Rs_raintime_wet')
qqnorm(Rs_raintime_wet.resid, main = 'Rs_raintime_wet')
qqline(Rs_raintime_wet.resid)
hist(Rs_raintime_wet.resid)
par(mfrow=c(1,1))

# search for residual outliers
View(Rs_raintime_wet.resid)
Rc_rs$Rs[11] <- NA

# R squared value
r.squaredGLMM(Rs_raintime_wet)
#R2m       R2c
#[1,] 0.01527298 0.8046357

summary(Rs_raintime_wet)
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)      1.32968    0.13704 11.09900   9.703  9.3e-07 ***
#  rain_timeDuring  0.10687    0.07943 16.42756   1.346    0.197 


### Figure S1. Box plots of soil respiration before/after rainfall during dry and wet season
# dry season
Rs_rain_times_dry <- ggplot(subset(Rc_rs, seasons=="dry"), aes(x=rain_time, y=Rs, fill=rain_time)) + 
  #geom_bar(stat="identity") +
  geom_boxplot()+
  labs(y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1})),
       x="", subtitle="(a) dry season") +
  scale_fill_grey(start = 0.8, end = 0.4) +
  ylim(0,2.2) +
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
        #legend.justification = c(1,1),
        legend.position = "none",
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))

# wet season
Rs_rain_times_wet <- ggplot(subset(Rc_rs, seasons=="wet"), aes(x=rain_time, y=Rs, fill=rain_time)) + 
  #geom_bar(stat="identity") +
  geom_boxplot()+
  #labs(y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1})),
  labs(y=expression(),
       x="", subtitle="(b) wet season") +
  scale_fill_grey(start = 0.8, end = 0.4) +
  ylim(0,2.2) +
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
        #legend.justification = c(1,1),
        legend.position = "none",
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))

ggarrange(Rs_rain_times_dry, Rs_rain_times_wet + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank()), 
          ncol = 2, nrow = 1)


########################## Relative change of soil respiration during rainfall #########################
# Text if the relative change of soil respiration (Rcrs) differs significantly between dry and wet season
t.test(Rcrs ~ seasons, data = subset(Rc_rs, rain_time=="During"))
#Welch Two Sample t-test

#data:  Rcrs by seasons
#t = 2.2784, df = 18.787, p-value = 0.03458
#alternative hypothesis: true difference in means between group dry and group wet is not equal to 0
#95 percent confidence interval:
#  0.02595818 0.61758952
#sample estimates:
#  mean in group dry mean in group wet 
#0.5267502         0.2049764

# Figure 6. Box plot of Rcrs during rainfall in the dry and wet season
ggplot(subset(Rc_rs, rain_time=="During"), aes(x=seasons, y=Rcrs, fill=seasons)) + 
  #geom_bar(stat="identity") 
  labs(x="Season", y="Relative change of soil respiration")+
  geom_boxplot()+
  #ylim(0,2.2) +
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
        legend.position = "none",
        #legend.position = c(1,1),
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))


####################################### Effect of change of SWC on Rcrs ###################################
# Daily means of Rs during rainfall responding to SWC, Ts, accumulated rainfall. 

# Fig 5a. Soil respiration response to SWC during days with rainfall
Rs_response_SWC <- ggplot(subset(Rc_rs, rain_time=="During"), aes(x=SWC, y=Rs, color=seasons)) +
  #ggplot(subset(Rc_rs, rain_time=="During"), aes(x=Rain, y=Total_Rs_upscaled)) +
  geom_smooth(data=subset(Rc_rs, seasons=="dry"), method="lm", se=F, size=0.7) +
  geom_point()  +
  labs(x="Soil water content (%)", y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1})), subtitle=bquote(bold("(a)"))) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label..,  sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               label.y.npc = "bottom",
               vstep = 0.05,
               size=3) + 
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
        #legend.justification = c(1,1),
        legend.position = "none",
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))

# Soil respiration response to Ts during days with rainfall 
ggplot(subset(Rc_rs, rain_time=="During"), aes(x=Ts, y=Rs, color=seasons)) +
  #ggplot(subset(Rc_rs, rain_time=="During"), aes(x=Rain, y=Total_Rs_upscaled)) +
  #geom_smooth(data=subset(Rc_rs, seasons=="dry"), method="lm", se=F, size=0.7) +
  geom_point()  +
  labs(x=expression("Soil temperature ("~degree~"C)"), y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1}))) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label..,..rr.label.., ..p.value.label..,  sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               label.y.npc = "top",
               vstep = 0.05,
               size=3) + 
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
        #legend.justification = c(1,1),
        legend.position = "none",
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))

# Fig 5a. Soil respiration response to accumulated rainfall during days with rainfall
Rs_response_rainfall <- ggplot(subset(Rc_rs, rain_time=="During"), aes(x=Rain, y=Rs, color=seasons)) +
  #ggplot(subset(Rc_rs, rain_time=="During"), aes(x=Rain, y=Total_Rs_upscaled)) +
  geom_smooth(data=subset(Rc_rs, seasons=="dry"), method="lm", se=F, size=0.7) +
  geom_point()  +
  labs(x="Accumulated rainfall (mm)", y=expression(Soil~respiration~(g~C~m^{-2}~d^{-1})), subtitle=bquote(bold("(b)"))) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label..,  sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               #label.y.npc = "bottom",
               vstep = 0.05,
               size=3) + 
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
        #legend.justification = c(1,1),
        legend.position = "none",
        legend.background = element_blank(),
        strip.text = element_text(size=11),
        strip.background = element_rect(colour="white",fill="white"),
        panel.background = element_rect(colour="black",fill="white", size=unit(0.25,"mm")),
        panel.border = element_rect(colour="black", fill=NA),
        panel.grid = element_blank(),
        plot.margin=unit(c(2,1,2,1),"mm"))


ggarrange(Rs_response_SWC, Rs_response_rainfall + 
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank()), 
          ncol = 2, nrow = 1)
