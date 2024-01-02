# Rs_Reco

GENERAL INFORMATION

1. Title of Dataset: Soil Respiration, Ecosystem Respiration, and Meteorological Variables in Chaparral Shrublands

2. Author Information
	A. Principal Investigator Contact Information
		Name: Walter Oechel
		Institution: San Diego State University
		Address: 
		Email: woechel@sdsu.edu

	B. Graduate Student and Author Contact Information
		Name: Jessica Montes
		Institution: San Diego State University
		Address: 
		Email: jamontes@sdsu.edu


3. Date of data collection (single date, range, approximate date) <suggested format YYYY-MM-DD>: 
   2020-06-01 to 2021-5-21 

4. Geographic location of data collection <latitude, longiute, or city/region, State, Country, as appropriate>: 

This study was conducted at the San Diego State University Sky Oaks Field Station near Warner Springs, CA (33°23′N, 116°38′W, Elevation: 1420 m)

5. Information about funding sources that supported the collection of the data: 

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: Please contact authors for authorship if using data for publication

2. Links to publications that cite or use the data: All files contained here were used for manuscript currently under review

3. Links to other publicly accessible locations of the data: 

4. Links/relationships to ancillary data sets: 

5. Was data derived from another source? NO
	A. If yes, list source(s): 

6. Recommended citation for this dataset:
   Montes, J. (2023). Soil Respiration, Ecosystem Respiration, and Meteorological Variables in Chaparral Shrublands (Version V1). Zenodo. https://doi.org/10.5281/zenodo.10424614

8. DATA & FILE OVERVIEW

1. File List: 
Rs_Ts_SWC_daily.csv
	This file contains daily mean values of soil respiration (Rs), soil temperature (Ts, measured at 0 cm), soil water content (SWC, measured as average of 0-30 cm), and upscaled soil respiration (Rs_upscaled). 

Rs_Reco_5day.csv
	This file contains 5-day mean values of ecosystem respiration (Reco), upscaled Rs, Ts, SWC, air temperature (AirT), average rainfall (Rain_avg), accumulated rainfall (Rain_sum), and the difference between soil and air temperature (Tair_Ts). 

 Rs_during_rainfall.csv
 	This file contains the daily Rs, Ts, SWC, and accumulated rainfall values for days with rainfall only. rain_time indicates the timing of the rainfall event (before or after). The Date_Before indicates the day before the rainfall occurred. The relative change of soil respiration during rainfall (Rcrs) is also included. 
  
  Rs_Ts_SWC_diurnal.csv
  	This file contains hourly 5-day periods for each month (except September).
 
 Rs_Ts_SWC_annual_means.R
	R code to calculate the annual means of daily Rs, Ts, and SWC values for each microsite: under redshank canopy (AS), under chamise canopy (AF), and inter-canopy soil (BARE). Coefficient of variacion was calculated for the three replicates of each microsite. 

 Rs_response_models.R
	R code to reproduce and create figures of Rs response models to Ts and SWC. 

 Ratio_response_models.R
 	R code to reproduce and create figures of Rs/Reco ratios response models to Ts, SWC, AirT, Rain_avg, Rain_sum, and Tair_Ts.  
  
 Rs_during_rainfall.R
 	R code to observe and statisically test the differences in daily Rs values before and during rainfall events during the wet and dry season.
  
  Rs_diurnal_response_models.R
  	R code to reproduce and create figures of diurnal Rs response to Ts for each month (except September). 

2. Relationship between files, if important:
   	The annual averages and coefficient of variance for the three replicates of each microsite were calculated with the files Rs_Ts_SWC_daily.csv and Rs_Ts_SWC_annual_means.R.
	5-day averages of Rs and Reco (Rs_Reco_5day.csv) were used to create Rs and Rs/Reco ratios response models to environmental variables (Rs_response_models.R and Ratio_response_models.R).
   	Differences in daily Rs before and during rainfall were observed and analyzed with files Rs_during_rainfall.csv and Rs_during_rainfall.R.
   	Diurnal relationships between Rs and Ts for each month were analyzed with the files Rs_Ts_SWC_diurnal.csv and Rs_diurnal_response_models.R.

