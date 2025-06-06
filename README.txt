STEPS TO REPLICATE THIS ANALYSIS --------------------------------------------------------------
- Download PSID data    
    - Search public data carts at: Go to https://simba.isr.umich.edu/DC/c.aspx
    - Enter amandabuechele2029@u.northwestern.edu to search my carts
    - Select public cart name "348092	-	20250529DOL"
    - Download data as STATA files
    - Create fam_ind as a subfolder of the 0_data folder
    - Save the .do and .txt files in the 0_data/fam_ind folder
- Set your working directory on the first line of build_panel.r
- Folders 1-5 each contain an R script and output folder
- Run code within each folder in order


FILE OVERVIEW ---------------------------------------------------------------------------------
functions: Functions to run all code
0_data: Contains all raw 0_data
0_data_info: Gives the codebook for PSID 0_data
0_functions: Run before other files
1_build_panel: Maps PSID variables across years and constructs an individual-year file
2_clean_panel: Harmonizes and cleans variables
3_create_households: Converts data from individual-years to household-years
4_create_outcomes: Adds household-level outcome variables, gives data for Appendix Table A.1
5_analysis: All code for analysis, including all tables and figures
