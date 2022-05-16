This readme accompanies the paper "Floral phenology of an Andean bellflower and pollination by Buff-tailed Sicklebill hummingbird" (Boehm MMA, Guevara-Apaza D, Jankowski JE, Cronk QCB, 2022). The following is an ordered, annotated list of .R files briefly describing their contribution to the manuscript. If reproducing the analyses, I recommend opening the .Rproj file in RStudio and using the package `here`; this way, all scripts will run without needing to modify any of the working directories. Scripts are ordered alpha-numerically. Scripts that share the same prefix number are related, but seemed too long to put into one .R file. The R Project is hosted at Zenodo. 

Following the R script descriptions is information describing the data used the analysis. Within the RStudio Project, the "data" folder contains two subfolders: the original raw data used in this study ("raw_data"), and several saved datasets that allow you to reproduce some individual results without having to re-do all previous steps in R ("derived_data"). Data that are not the products of any other scripts are in the "raw_data' subfolder. All variable descriptions are outlined (following the R script descriptions) in the metadata.txt file. The data are hosted at Dryad. 

Please feel free to direct any questions to mannfred.boehm@ubc.ca. 



# -------------------

/Rscripts

1_data_inspection.R
	This script imports "means_melted.csv", which provides the mean number of days taken to reach a developmental stage of flowering. The means were computed manually (i.e. not in R) from the files "Bird_cage_*.csv" and "No_treat_*.csv", which were extracted from the raw data "Bird_exclusion_exp_2017.xslx". The purpose of the first section of this script is to visualize and compare development of flowers between controls and those with pollinators excluded. In the second and third sections we import the data "No_treat_compiled.csv" and "Bird_cage_compiled.csv". These files are amaglamations of "Bird_cage_*.csv" and "No_treat_*.csv" (see above). We use this data to determine if pollinator exclusion affected flowering time, and to estimate mean and variance of days taken to reach each developmental stage. 


1_graphical_abstract.R
	This script imports landmark data ("graphical_abstract.TPS") from photos of flowers in the dorsiventral view (see Figure 1). It then fits cubic splines to the ventral side of the flower and computes total curvature. 


2_development_controls.R
2_development_excluded.R
	These scripts import "No_treat_compiled.csv" or "Bird_cage_compiled.csv", respectively. First, the development of each flower is arranged as a string of numbers representing developmental stage. Second, a for loop evaluates each developmental trajectory to determine if it is censored (0) or not (1). At this point, two RDS files are exported for later use: "survdata_controls.rds" and "survdata_pollexcluded.rds". We then manually correct some flowers assigned as "censored" that only completed stage 1, but which we strongly suspect had reached the maximum duration of stage 1 before the study ended. We then fit a parametric survival curve to each developmental trajectory, and compute the median duration of each stage. Each script outputs "medians_controls.rds" or "medians_pollexcluded.rds", respectively. 

2_boxplot_alpha.R
2_ggplot_theme.R
2_development_plotting.R
	This script takes the outputs (median stage durations) of the previous scripts and plots them for visualization purposes (used as Figure 4 in the manuscript). To plot, the file "2_ggplot_theme.R" and "2_boxplot_alpha.R" need to be run. 

3_fruit_medians.R
	This script imports "survdata_controls.rds" to estimate the median duration of fruit development in control flowers. 

4_peru_map.R
	This script uses `tmap` to plot an elevational heatmap of Peru, with our study site marked (see: Figure 2 in the manuscript_.

5_flowering_rate.R
	This script imports "No_treat_compiled.csv" and "Bird_cage_compiled.csv" (see "1_data_inspection.R) to compute the number of days between anthesis events for each flower in the study. It then uses `broom` to fit linear models to the anthesis rate and plots these lines (see: Figure 5 in the manuscript). We then use `lin.eval` to estimate linearity. 

6_sicklebill_visits.R
	This script creates numeric vectors storing counts of Sicklebill visits extracted from inspection of camera trap data (see: Dryad repo). 




# ----------------------------------
The following contains information about variable names in the '/data/raw_data' and '/data/derived_data' directories.


# /data/raw_data 

/Table_S1_monitoredlocations.csv
	name = C. granulosus individual ID
	latitude = latitude in decimal degrees
	longitude = longitude in decimal degrees
	elevation = elevation above sea level in metres

/Table_S2_developmentstages.csv
	Stage = developmental stage of flower (A to H)
	Description = qualitative and quantitative traits that define the stage
	Median duration (days) = median duration of the stage as computed in "2_development_controls.R" and "2_development_excluded.R"

/Table_S3_observationseutoxeres.csv
/Table_S4_observationsschistes.csv
	Date_observed = date of pollinator visit
	time_of_day = time of pollinator visit (24 hour clock)
	feeding_mode = hovering or perching
	visit_duration_seconds = duration of pollinator visit (seconds)
	temperature_celcius = temperature during pollinator visit (Celcius)
	centropogon_ID = C. granulosus individual ID
	flower_ID = flower ID per C. granulosus individual

/camera_trap_eutoxeres
	containts photos (.jpg) taken by camera traps deployed near Centropogon granulosus and Heliconia aemygdiana. 

/landmarking
	contains photos (.png) of flowers and hummingbirds used for landmarking and estimation of curvature. Photos are used with permission from owners. 

/landmarking/graphical_abstract.tps
	LM = number of landmarks
	IMAGE = image ID from associated .jpg file
	ID = tpsDig ID
	SCALE = pixels to mm conversion

/Bird_exclusion_exp_2017.xslx
	first column (blank) = date of record (year 2017)
	F* = developmental stage of flower (1 to 8)
	d = fruit dropped
	d+c = fruit dropped and collected by MMAB


# ----------------------------------
# /data/derived_data 

/Bird_cage_*.csv
/No_treat_*.csv
	first column (blank) = date of record (year 2017)
	F* = developmental stage of flower (1 to 8)

/means_melted.csv
/means_bird_cage.csv
/means_no_treat.csv
	group = control (a) or pollinators excluded (b)
	stage = developmental stage	
	days = mean days in current stage

/Bird_cage_compiled.csv
/No_treat_compiled.csv
	indiv_flower = plant individual ID and flower ID, separated by an underscore
	date = month/day/year
	stage = developmental stage

/survdata_controls.rds
/survdata_pollexcluded.rds
	stage = developmental stage
	duration = days in current stage
	indiv = plant individual ID and flower ID, separated by an underscore
	status = censored (0) or uncensored (1)

/medians_controls.rds
/medians_pollexcluded.rds
	est = estimate of the median duration of the developmental stage
	lcl = lower confidence limit of the median
	ucl = upper confidence limit of the median
	stage = developmental stage
	cummed = cumuluative days to current stage (median)
	CI = confidence interval of cummed
	CIprop = confidence interval with error propagated
	lclprop = lower confidence limit with error propagated
	uclprop = upper confidence limit with error propagated







	


	
