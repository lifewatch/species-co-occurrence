# LifeWatch Species Co-occurrence

Herein this repository were codes used to analyze species co-occurrence using available Passive Acoustic Monitoring (PAM) and Acoustic Telemetry (AT) data from the European Tracking Network (ETN). We created hourly presence-absence matrices for each species of interest and applied some of the most common analyses previously used to study species co-occurrence. 

# Preparation of data

a. Raw data was extracted from https://lifewatch.be/etn

b. Cleaning, organizing and merging of data is found in organising_data.Rmd. Hourly presence-absence matrices were made for each species which were then merged. This code results to the final data set used for the subsequent analyses. A distinction was made between true absence (DPH=0) and uncertainty of presence/absence (DPH = NA) by creating a dataframe of all hours when CPODs/receivers were both active and fish had active acoustic transmitters.

# Data visualization

Plotting of available data for each station, mapping out the stations and plotting of heat maps for species detected from the BPAN. 

# Data analyses

a. MonthlyOccupancy.R: Pairwise species monthly occupancy using the cooccur package

b. CScore.R: Quantification of association between species pairs based on the number of shared stations using the EcoSimR package 

c. DielOverlap.R: Graphing of hourly overlap of presence of species and calculation of overlap estimates using the overlap package

d. sb_porp_glmm.R, cod_porp_glmm.R, dol_porp_glmm.R:  Generalized Linear Mixed Effects/Generalized Linear Models for each species pair 










