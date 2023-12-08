# load packages
library(dplyr)
library(plotrix)
library(lubridate)
library(ggplot2)

# load csv file
Rsoil_chambers = read.csv('/Users/jessicamontes/Documents/ThesisData/0_ThesisPaper/Final_Paper/JGR_Biogeosciences/Rs_daily_upscaled.csv')

####################################### ANNUAL AVERAGES OF SOIL RESPIRATION ####################################### 

############ REDSHANK
### CHAMBER #1 
# average
mean(Rsoil_chambers$Flux_AS1, na.rm=T)
# 1.736267

# standard error
std.error(Rsoil_chambers$Flux_AS1, na.rm=T)
# 0.04036293

### CHAMBER #2 
# average
mean(Rsoil_chambers$Flux_AS2, na.rm=T)
# 1.315601

# standard error
std.error(Rsoil_chambers$Flux_AS2, na.rm=T)
# 0.02855011

### CHAMBER #3 
# average
mean(Rsoil_chambers$Flux_AS3, na.rm=T)
# 1.353066

# standard error
std.error(Rsoil_chambers$Flux_AS3, na.rm=T)
# 0.03418589

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AS_chambers <- c(1.74,1.32, 1.35)
mean(AS_chambers) 
# 1.47
std.error(AS_chambers)
# 0.1352775

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AS_cv <- sd(AS_chambers) / mean(AS_chambers) * 100
AS_cv
#15.93929

############ CHAMISE
### CHAMBER #1
# average
mean(Rsoil_chambers$Flux_AF1, na.rm=T)
# 1.234641

# standard error
std.error(Rsoil_chambers$Flux_AF1, na.rm=T)
# 0.04442007

### CHAMBER #2 
# average
mean(Rsoil_chambers$Flux_AF2, na.rm=T)
# 1.281247

# standard error
std.error(Rsoil_chambers$Flux_AF2, na.rm=T)
# 0.05669695

### CHAMBER #3 
# average
mean(Rsoil_chambers$Flux_AF3, na.rm=T)
# 1.182925

# standard error
std.error(Rsoil_chambers$Flux_AF3, na.rm=T)
# 0.04346424

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AF_chambers <- c(1.23,1.28, 1.18)
mean(AF_chambers) 
# 1.23
std.error(BARE_chambers)
# 0.03511885

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AF_cv <- sd(AF_chambers) / mean(AF_chambers) * 100
AF_cv
#4.065041

############ BARE
### CHAMBER #1
# average
mean(Rsoil_chambers$Flux_BARE1.x, na.rm=T)
# 0.9714221

# standard error
std.error(Rsoil_chambers$Flux_BARE1.x, na.rm=T)
# 0.02767122

### CHAMBER #2
# average
mean(Rsoil_chambers$Flux_BARE2.x, na.rm=T)
# 0.9830694

# standard error
std.error(Rsoil_chambers$Flux_BARE2.x, na.rm=T)
# 0.02673014

### CHAMBER #3 
# average
mean(Rsoil_chambers$Flux_BARE3.x, na.rm=T)
# 0.8732479

# standard error
std.error(Rsoil_chambers$Flux_BARE3.x, na.rm=T)
# 0.02330408

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
BARE_chambers <- c(0.97,0.98, 0.87)
mean(BARE_chambers) 
# 0.94
std.error(BARE_chambers)
# 0.03511885

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
BARE_cv <- sd(BARE_chambers) / mean(BARE_chambers) * 100
BARE_cv
#6.471024

###### UPSCALED SOIL RESPIRATION
# average
mean(Rsoil_chambers$Rs_upscaled, na.rm=T)
# 1.302978

# standard error
std.error(Rsoil_chambers$Rs_upscaled, na.rm=T)
# 0.03633857

# max
max(Rsoil_chambers$Rs_upscaled, na.rm=T)
#2.74452

# min
min(Rsoil_chambers$Rs_upscaled, na.rm=T)
#0.4122023

####################################### ANNUAL AVERAGES OF SOIL TEMPERATURE ####################################### 

############ REDSHANK
### REDSHANK #1 
# average
mean(Rsoil_chambers$Ts_AS1, na.rm=T)
# 15.91743

# standard error
std.error(Rsoil_chambers$Ts_AS1, na.rm=T)
# 0.4335941

### REDSHANK #2 
# average
mean(Rsoil_chambers$Ts_AS2, na.rm=T)
# 16.11838

# standard error
std.error(Rsoil_chambers$Ts_AS2, na.rm=T)
# 0.4663902

### REDSHANK #3 
# average
mean(Rsoil_chambers$Ts_AS3, na.rm=T)
# 15.20163

# standard error
std.error(Rsoil_chambers$Ts_AS3, na.rm=T)
# 0.4627849

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AS_Ts_chambers <- c(15.92,16.12, 15.20)
mean(AS_Ts_chambers) 
# 15.74667
std.error(AS_Ts_chambers)
# 0.2793644

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AS_cv <- sd(AS_Ts_chambers) / mean(AS_Ts_chambers) * 100
AS_cv
# 3.072862

############ CHAMISE
### CHAMISE #1 
# average
mean(Rsoil_chambers$Ts_AF1, na.rm=T)
# 16.07786

# standard error
std.error(Rsoil_chambers$Ts_AF1, na.rm=T)
# 0.4609431

### CHAMISE #2 
# average
mean(Rsoil_chambers$Ts_AF2, na.rm=T)
# 16.47092

# standard error
std.error(Rsoil_chambers$Ts_AF2, na.rm=T)
# 0.4561467

### CHAMISE #3 
# average
mean(Rsoil_chambers$Ts_AF3, na.rm=T)
# 15.0297

# standard error
std.error(Rsoil_chambers$Ts_AF3, na.rm=T)
# 0.4645329

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AF_Ts_chambers <- c(16.08,16.47, 15.03)
mean(AF_Ts_chambers) 
# 15.86
std.error(AF_Ts_chambers)
# 0.43

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AF_cv <- sd(AF_Ts_chambers) / mean(AF_Ts_chambers) * 100
AF_cv
# 4.695976

############ BARE
### BARE #1 
# average
mean(Rsoil_chambers$Ts_BARE1, na.rm=T)
# 15.94956

# standard error
std.error(Rsoil_chambers$Ts_BARE1, na.rm=T)
# 0.496083

### BARE #2
# average
mean(Rsoil_chambers$Ts_BARE2, na.rm=T)
# 16.09478

# standard error
std.error(Rsoil_chambers$Ts_BARE2, na.rm=T)
# 0.5000633

### BARE #3
# average
mean(Rsoil_chambers$Ts_BARE3, na.rm=T)
# 16.15351

# standard error
std.error(Rsoil_chambers$Ts_BARE3, na.rm=T)
# 0.4414511

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
BARE_Ts_chambers <- c(15.95,16.09, 16.15)
mean(BARE_Ts_chambers) 
# 16.06333
std.error(BARE_Ts_chambers)
# 0.05925463

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
BARE_cv <- sd(BARE_Ts_chambers) / mean(BARE_Ts_chambers) * 100
BARE_cv
# 0.6389211

###### UPSCALED SOIL TEMPERATURE
# average
mean(Rsoil_chambers$Ts_upscaled, na.rm=T)
# 15.89187

# standard error
std.error(Rsoil_chambers$Ts_upscaled, na.rm=T)
# 0.4632213

# max
max(Rsoil_chambers$Ts_upscaled, na.rm=T)
#31.76141

# min
min(Rsoil_chambers$Ts_upscaled, na.rm=T)
#-0.03756391


####################################### ANNUAL AVERAGES OF SOIL WATER CONTENT ####################################### 

############ REDSHANK
### REDSHANK #1 
# average
mean(Rsoil_chambers$SWC_AS1, na.rm=T)
# 6.199636

# standard error
std.error(Rsoil_chambers$SWC_AS1, na.rm=T)
# 0.1863994

### REDSHANK #2
# average
mean(Rsoil_chambers$SWC_AS2, na.rm=T)
# 6.507207

# standard error
std.error(Rsoil_chambers$SWC_AS2, na.rm=T)
# 0.2371685

### REDSHANK #3
# average
mean(Rsoil_chambers$SWC_AS3, na.rm=T)
# 10.08862

# standard error
std.error(Rsoil_chambers$SWC_AS3, na.rm=T)
# 0.2872961

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AS_SWC_chambers <- c(6.20,6.51, 10.09)
mean(AS_SWC_chambers) 
# 7.6
std.error(AS_SWC_chambers)
# 1.248212

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AS_cv <- sd(AS_SWC_chambers) / mean(AS_SWC_chambers) * 100
AS_cv
# 28.44693

############ CHAMISE
### CHAMISE #1 
# average
mean(Rsoil_chambers$SWC_AF1, na.rm=T)
# 10.51269

# standard error
std.error(Rsoil_chambers$SWC_AF1, na.rm=T)
# 0.2412244

### CHAMISE #2
# average
mean(Rsoil_chambers$SWC_AF2, na.rm=T)
# 9.802013

# standard error
std.error(Rsoil_chambers$SWC_AF2, na.rm=T)
# 0.2795618

### CHAMISE #3
# average
mean(Rsoil_chambers$SWC_AF3, na.rm=T)
# 10.51977

# standard error
std.error(Rsoil_chambers$SWC_AF3, na.rm=T)
# 0.3057173

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
AF_SWC_chambers <- c(10.51,9.80, 10.52)
mean(AF_SWC_chambers) 
# 10.27667
std.error(AF_SWC_chambers)
# 0.2383508

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
AF_cv <- sd(AF_SWC_chambers) / mean(AF_SWC_chambers) * 100
AF_cv
# 4.017214

############ BARE
### BARE #1 
# average
mean(Rsoil_chambers$SWC_BARE1.x, na.rm=T)
# 7.439642

# standard error
std.error(Rsoil_chambers$SWC_BARE1.x, na.rm=T)
# 0.2225682

### BARE #2
# average
mean(Rsoil_chambers$SWC_BARE2.x, na.rm=T)
# 9.48314

# standard error
std.error(Rsoil_chambers$SWC_BARE2.x, na.rm=T)
# 0.2832695

### BARE #3
# average
mean(Rsoil_chambers$SWC_BARE3.x, na.rm=T)
# 9.452664

# standard error
std.error(Rsoil_chambers$SWC_BARE3.x, na.rm=T)
# 0.2556942

### AVERAGE AND STANDARD ERROR OF THREE CHAMBERS
BARE_SWC_chambers <- c(7.44,9.48, 9.45)
mean(BARE_SWC_chambers) 
# 8.79
std.error(BARE_SWC_chambers)
# 0.6750556

### COEFFICIENT OF VARIANCE OF THREE CHAMBERS
BARE_cv <- sd(BARE_SWC_chambers) / mean(BARE_SWC_chambers) * 100
BARE_cv
# 13.30183

###### UPSCALED SOIL WATER CONTENT
# average
mean(Rsoil_chambers$SWC_upscaled, na.rm=T)
# 9.205484

# standard error
std.error(Rsoil_chambers$SWC_upscaled, na.rm=T)
# 0.2495158

# max
max(Rsoil_chambers$SWC_upscaled, na.rm=T)
# 21.997

# min
min(Rsoil_chambers$SWC_upscaled, na.rm=T)
# 4.41719


