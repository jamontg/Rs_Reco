# Rs_Reco

GENERAL INFORMATION

1. Title of Dataset: Soil Respiration, Ecosystem Respiration, and Meteorological Variables in Chaparral Shrublands

2. Author Information
	A. Principal Investigator Contact Information
		Name: Walter Oechel
		Institution: San Diego State University
		Address: 
		Email: woechel@sdsu.edu

	B. Author Contact Information
		Name: Jessica Montes
		Institution: San Diego State University
		Address: 
		Email: jamontes@sdsu.edu


3. Date of data collection (single date, range, approximate date) <suggested format YYYY-MM-DD>: 
   2020-06-01 to 2021-5-21 

4. Geographic location of data collection <latitude, longiute, or city/region, State, Country, as appropriate>: 

This study was conducted at the San Diego State University Sky Oaks Field Station near Warner Springs, CA (33°23′N, 116°38′W, Elevation: 1420 m)

6. Citation for this dataset:
   Montes, J. (2023). Soil Respiration, Ecosystem Respiration, and Meteorological Variables in Chaparral Shrublands (Version V1). Zenodo. https://doi.org/10.5281/zenodo.10424614

8. DATA & FILE OVERVIEW

1. File List: 
Rs_Ts_SWC_daily.csv
	This file contains daily mean values of soil respiration (Rs), soil temperature (Ts, measured at 0 cm), soil water content (SWC, measured as average of 0-30 cm), and upscaled soil respiration (Rs_upscaled). 

Rs_Reco_5day.csv
	This file contains 5-day mean values of ecosystem respiration (Reco), upscaled Rs, Ts, SWC, air temperature (AirT), average rainfall (Rain_avg), accumulated rainfall (Rain_sum), and the difference between soil and air temperature (Tair_Ts). 
  
  Rs_Ts_SWC_diurnal.csv
  	This file contains hourly 5-day periods for each month (except September).
 
 Rs_Ts_SWC_annual_means.R
	R code to calculate the annual means of daily Rs, Ts, and SWC values for each microsite: under redshank canopy (AS), under chamise canopy (AF), and inter-canopy soil (BARE). Coefficient of variacion was calculated for the three replicates of each microsite. 

 Rs_response_models.R
	R code to reproduce and create figures of Rs response models to Ts and SWC. 

 Ratio_response_models.R
 	R code to reproduce and create figures of Rs/Reco ratios response models to Ts, SWC, AirT, Rain_avg, Rain_sum, and Tair_Ts.  
  
  Rs_diurnal_response_models.R
  	R code to reproduce and create figures of diurnal Rs response to Ts for each month (except September). 

2. Relationship between files, if important:
   	The annual averages and coefficient of variance for the three replicates of each microsite were calculated with the files Rs_Ts_SWC_daily.csv and Rs_Ts_SWC_annual_means.R.
	5-day averages of Rs and Reco (Rs_Reco_5day.csv) were used to create Rs and Rs/Reco ratios response models to environmental variables (Rs_response_models.R and Ratio_response_models.R).
   	Diurnal relationships between Rs and Ts for each month were analyzed with the files Rs_Ts_SWC_diurnal.csv and Rs_diurnal_response_models.R.

