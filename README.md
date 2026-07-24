## 2026 REU Rainforest hydraulics project

### scripts

01 - wrangling data products into tidy format

02 - making dataframes, calculating further response variables

03 - initial visualizations of data/looking for patterns in manual data

04 - making "final"" figures for manual data

05 - looking at canopy level differences in sap velocity

06 - looking at how water potential and tree-level metrics relate

07 - relating tree level metrics to each other

### data

All data types have their own folder. Within these folders, are 1) loose
files with cleaned data products that you'd want to read in somewhere else, 
and 2) more folder(s) containing raw/slightly cleaned data files for ease 
of indexing.

**dendro_data**/all_dendros.csv - all cleaned dendrometer data, concatenated 
together, labelled by tree. Written out in local time (MST, America/Phoenix).

**dendro_data**/clean_dendros - cleaned dendrometer data (jumps fixed), used to 
create all_dendros.csv

**dendro_data**/lambda_twd_min.csv - coefficients for tree wise regression 
between tree water deficit and predawn water potential (|λTWD[min]|).

**dendro_data**/raw_dendros - uncleaned dendrometer data, used in 
01-fix-dendro-jumps.R

**licor600_data**/all_licor600.csv - all li600 files concatenated together, 
corrected dt.

**licor600_data**/clean_all_licor600.csv - same as all_licor600.csv but filtered 
for correct config, labelled by instrument.

**licor600_data**/licor600_by_canopy.csv - all the cleaned li600 data, but 
summarized by canopy level (upper/lower).

**licor600_data**/licor600_by_individual - all the cleaned li600 data, but 
summarized by individual.

**licor600_data**/licor600_raw_data - all raw li600 files straight from the 
machine.

**licor600_data**/test_data - test data, used when troubleshooting the machine 
mostly.

**met_data**

**sap_flow**/lambda_sv_norm.csv - coefficients for the tree wise regression 
between sap velocity and midday water potential (λG[c,norm]).

**sapflow**/raw_sapflow - all sap flow data files (cleaned files are loose 
within this folder and labelled "postprocess", and there is a nested folder 
containing the postprocess files for R1171).

**sapflow**/Sap_Flow_MetaData.xlsx - meta data for all sap flow files.

**sapflow**/sv_all.csv - all cleaned sap flow files, concatenated together and 
labelled by tree + location. 

**water_potential**/all_wp.csv - all water potential data, cleaned up.

**water_potential**/wp_PDMD_long.csv - summarized water potential values to 
predawns and middays (mean for each tree + canopy level), in long format (column 
for period and wp_m).

**water_potential**/wp_PDMD.csv - summarized water potential values to 
predawns and middays (mean for each tree + canopy level), in wide format (column 
for PD_m and MD_m with sds).

### figs

some saved figures
