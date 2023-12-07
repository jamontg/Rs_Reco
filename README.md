# Rs_Reco

GENERAL INFORMATION

1. Title of Dataset: EFFECTS OF SOIL TEMPERATURE, SOIL WATER CONTENT, AND RAINFALL ON SOIL RESPIRATION AND ITS CONTRIBUTION TO ECOSYSTEM RESPIRATION IN CHAPARRAL SHRUBLANDS

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

7. DATA & FILE OVERVIEW

1. File List: 
Rs_Ts_SWC_daily.csv
	This file contains daily mean values of soil respiration (Rs), soil temperature (Ts, measured at 0 cm), soil water content (SWC, measured as average of 0-30 cm), and upscaled soil respiration (Rs_upscaled). 

Rs_Reco_5day.csv
	This file contains 5-day mean values of upscaled ecosystem respiration (Reco), Rs, Ts, SWC, air temperature (AirT), average rainfall (Rain_avg), and accumulated rainfall (Rain_sum). 

Rs_Ts_SWC_annual_means.R
	R code to reproduce and create figures of Rs response models to Ts and SWC. Linear, quadratic, and cubic fits were included. 

 Rs_response_models.R
 	R code to reproduce and create figures of Reco response models to Ts, SWC, air temperature (AirT), rainfall (Rain_sum), and difference between soil and air temperature. 


2. Relationship between files, if important: 
	5-day averages of Rs and Reco (Rs_Reco_5day.csv) were used to create response models to environmental variables (Rs_Ts_SWC_annual_means.R and Rs_response_models.R). 

