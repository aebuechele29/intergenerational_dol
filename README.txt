To replicate this analysis:
- Download data from the PSID and save within its own folder 0_data/fam_ind
    - Search public cart name "348092	-	20250529DOL"
    - Download data as STATA files
    - Save the .do and .txt files to the 0_data/fam_ind folder
- Folders 1-5 each contain an R script and output folder
- Run code within each folder in order

functions: Functions to run all code
0_data: Contains all raw 0_data
0_data_info: Gives the codebook for PSID 0_data
1_build_panel: Maps PSID variables across years and constructs an individual-year file
2_clean_panel: Harmonizes and cleans variables
3_create_households: Converts data from individual-years to household-years
4_create_outcomes: Adds household-level outcome variables, gives data for Appendix Table A.1
5_analysis: All code for analysis, including all tables and figures
