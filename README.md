# Rs_Reco

GENERAL INFORMATION

1. Title of Dataset: EFFECTS OF SOIL TEMPERATURE, SOIL WATER CONTENT, AND RAINFALL ON SOIL RESPIRATION AND ITS CONTRIBUTION TO ECOSYSTEM RESPIRATION IN CHAPARRAL SHRUBLANDS

2. Author Information
	A. Principal Investigator Contact Information
		Name: Walter Oechel
		Institution: San Diego State University
		Address: 
		Email: woechel@sdsu.edu

	B. Graduate Student and Co-Author Contact Information
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

2. Links to publications that cite or use the data: 

3. Links to other publicly accessible locations of the data: 

4. Links/relationships to ancillary data sets: 

5. Was data derived from another source? NO
	A. If yes, list source(s): 

6. Recommended citation for this dataset:

7. DATA & FILE OVERVIEW

1. File List: 
SMER_FluxData_Daily_RtRhRa.csv
	This file contains daily mean values of total respiration (Rt), heterotrophic respiration (Rh), and autotrophic respiration (Ra) calculate from the paired Rt-Rh treatments. 

SMER_FluxData_Daily_RtRhTempVWC.csv
	This file contains daily mean values of total respiration (Rt), heterotrophic respiration (Rh), soil temperature (integrated 0-5cm) and soil volumetric water content (integrated 0-5cm and calibrated to gravimetric water content). This data is organized with each individual observation in a row with VWC and soil temperature measurements matched with every flux measurement. 

Mauritz_Lipson_Oecologia_StatsGraphCode.R
	R code to reproduce figures and statistical analyses for a manuscript under review, using the data as included here. 


2. Relationship between files, if important: 
The Rt and Rh values in SMER_FluxData_Daily_RtRhRa.csv are identical to SMER_FluxData_Daily_RtRhTempVWC.csv and both files contain the unique collar IDs as well as a pairing ID that allowed calculation of Ra. The difference is that for SMER_FluxData_Daily_RtRhRa.csv, Rt, Rh, Ra are organized as columns and include Ra. For SMER_FluxData_Daily_RtRhTempVWC.csv each observation is in a separate row and includes VWC and soil temperature data with no Ra. 


