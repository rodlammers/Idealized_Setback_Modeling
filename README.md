# Idealized_Setback_Modeling
This repository includes code and data for modeling idealized levee setbacks using HEC-RAS. This analysis is described in more detail in the manuscript "Modeling the effects of levee setbacks on flood hydraulics" submitted to the Journal of Flood Risk Management. The specific files included in this repository are included below. These data and code are sufficient to replicate the analysis in this paper. We do not provide all HEC-RAS model files and results because of the sheer number and size of these files. Please direct any questions to Rod Lammers (rodlammers@gmail.com).

# R Code
R code was used to set up all HEC-RAS simulations and read and analyze results. 

"HEC-RAS Automation Functions.R" - This script contains functions for creating HEC-RAS input files, reading results, and creating plots. These functions are called by various other scripts.
"Idealized_sims_V_results.R" - This script reads in velocity results from all simulations, summarizes the data, and produces plots that are included in the manuscript.
"Idealized_sims_spatial_configs_updated.R" - This script performs the analysis of how different configurations of levee setbacks affects hydraulics. This code creates the HEC-RAS input files for these simulations, reads and analyzes results, and creates plots.
"Idealized_sims_updated.R" - This script performs the main analysis of the paper. The code creates HEC-RAS input files for all modeled scenarios. After the HEC-RAS models are run (using the provided Excel VBA code), this script reads in the results, analyzes the data, and produces the figures included in the paper.
"Plot Functions.R" - This script has some basic plotting functions used in other code.
"Sediment Transport Capacity Analysis.R" - This script runs the sediment transport analysis based on the full simulation results.
"Wabash Results_Updated.R" - This script reads in the HEC-RAS results for the Wabash River models and compares them to the idealized simulation results. Note that the full Wabash HEC-RAS output files are not provided because of their size, but the summary results are so that the manuscript figures can be reproduced.

#Runs
This folder contains baseline HEC-RAS input files that are modified by the R code to produce HEC-RAS files for each simulation. A "Run_RAS.xlsm" file is also provided which is used to do batch HEC-RAS runs. This code just needs a list of all HEC-RAS .prj file paths to be run (supplied as a link to a text file). This text file is produced by the R scripts described above. Once the HEC-RAS models have been run, the rest of the R scripts are used to analyze results.

#Runs/Results
Summary results are provided in this folder to allow for all manuscript figures to be replicated. Results files are provided in R binary file format (.Rds).
"Attenuation_results2022-07-11.Rds" - Results of floodwave attenuation from all idealized simulations. These data are used to produce Figure S6 (end of "Idealized_sims_updated.R" file).
"Sed_results_v42022-06-29.Rds" - Results of sediment transport analysis. These data are used to produce Figures 8, S9, S10, and S12 ("Sediment Transport Capacity Analysis.R" file).
"Summary_WSE2022-06-09.Rds" - Main summary results of changes in water surface elevation with setbacks. These data are used to produce Figures 2-6 ("Idealized_sims_updated.R" file).
"Velocity_results_max_v42022-06-29.R" - Results of change in peak channel velocity with setbacks. These data are used to produce Figures 7, S7-S8, and S11 ("Idealized_sims_V_results.R" file).
"WSE_diff_2022-06-29.R" - Results of change in waster surface elevation with setbacks. These full data are used to calculate the metrics in "Summary_WSE2022-06-09.Rds".

#Runs/Wabash
This folder contains results from the Wabash modeling analysis (see "Wabash Results_Updated.R" file).
"Bed_z.csv" - Channel bed elevation (profile) for the Wabash River. These data are used for Figure S5.
"Wabash_max_WSE_03292023.Rds" - These is the peak water surface elevation profile for all Wabash River simulations. These data are used for Figure S5.
"Wabash_summary.csv" - Summary data of changes in water surface elevation with setbacks for the Wabash River simulations. These data are compared to results from idealized simulations and are included in Figure 6.
