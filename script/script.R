library(readr)
library(forcats)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(scales)
library(tidyr)
library(openxlsx)
library(readxl)
library(broom)
library(patchwork)
library(stringr) 
library(data.table)
library(curl)
library(zoo)
library(stringr)
library(zoom)
library(tsibble)
library(ggpubr)
library(ggspatial)
##library(janitor)
library(zoo)
##library(flextable)
##library(officer)
library(eq5d)
##install.packages("readxl")
library(readxl)
library(sandwich)
library(lmtest)
library(haven)
library(Synth)

file_path <- "/Users/huisong/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofLiverpool/Health Inequalities Policy Research Team - PHIRST/1. Research/1. Allocated Projects/06. PHIRST_5_Trauma Informed Schools Pilot/5. Data and Research Related/PHASE 2/quant"

sub_folder <- "outputs"
lsoa11 <- read_csv(paste0(file_path,"/input data/geo/PCD_OA_LSOA_MSOA_LAD_AUG22_UK_LU.csv"))
IMD <- read_xlsx(paste0(file_path, "/input data/IMD.xlsx"))
full_path <- file.path(file_path, sub_folder)

##input all England schools
######link for absence:
#https://explore-education-statistics.service.gov.uk/data-tables/pupil-absence-in-schools-in-england

##link for exclusion 
##https://explore-education-statistics.service.gov.uk/data-tables/suspensions-and-permanent-exclusions-in-england. 

###input data
absence<- fread(paste0(file_path, "/input data/school level absence data/pupil-absence-in-schools-in-england_2023-24/data/1a_absence_3term_school.csv"))
exclusion <- fread(paste0(file_path, "/input data/suspensions-and-permanent-exclusions-in-england_2022-23/data/exc_school.csv"))

#####for academic year 2023-2024 we only have autumn and spring data,so use the average score
exclusion_2023 <- fread(paste0(file_path,"/input data/suspensions-and-permanent-exclusions-in-england_2023-24-spring-term/data/exc_school.csv"))
exclusion_2023 <-exclusion_2023[time_period=="202324"]
exclusion_2023[, perm_excl := mean(perm_excl, na.rm = TRUE), by = school_urn]
exclusion_2023[, perm_excl_rate := mean(perm_excl_rate, na.rm = TRUE), by = school_urn]
exclusion_2023[, suspension := mean(suspension, na.rm = TRUE), by = school_urn]
exclusion_2023[, susp_rate := mean(susp_rate, na.rm = TRUE), by = school_urn]
exclusion_2023[, one_plus_susp := mean(one_plus_susp, na.rm = TRUE), by = school_urn]
exclusion_2023[, one_plus_susp_rate := mean(one_plus_susp_rate, na.rm = TRUE), by = school_urn]
exclusion_2023[, headcount := mean(headcount, na.rm = TRUE), by = school_urn]
exclusion_2023 <- unique(exclusion_2023, by = "school_urn")
exclusion <- rbindlist(list(exclusion,exclusion_2023),fill=T)

absence <- unique(absence)
exclusion <- unique(exclusion)

##read other variables in dta file
other <- read_dta(paste0(file_path, "/input data/extract_Huihui.dta"))

##using school_urn and time_period to merge two dataset
abs_exc <- merge(absence, exclusion, by=c("school_urn", "time_period"),all=TRUE)
##panel data with missing values
abs_exc$period <- paste0(
  substring(abs_exc$time_period, 1, 4), "_", 
  as.numeric(substring(abs_exc$time_period, 3, 4)) + 2001
)
final <- merge(abs_exc, other, by.x = c("school_urn", "period"), by.y = c("urn", "period"),all = TRUE)
outcome_vars <- c("sess_overall_percent", "sess_overall_percent_pa_10_exact", 
                  "perm_excl_rate", "susp_rate")


#####lsoa we want
nafill_str <- function(x) {
  x <- as.factor(x)                        
  levels <- levels(x)                       
  x <- as.numeric(x)                       
  x <- nafill(x, type = 'locf')             # Fill forward first
  x <- nafill(x, type = 'nocb')             # Fill backward
  x <- factor(x, labels = levels)           
  return(x)
}
final[, la_name.x:=nafill_str(la_name.x), school_urn]
setcolorder(final, c("school_urn", "la_name.x","la_name"))
lsoa <- c("Walsall", "Rochdale", "Oldham", "Tameside", "Bury", "Derby", "Kirklees", 
          "Middlesbrough", "Blackburn with Darwen", "Stoke-on-Trent", "Doncaster", 
          "Bradford", "Dudley", "Telford and Wrekin", "Rotherham", "Bolton")

final<-final[la_name.x %in% lsoa]

table(final$period)
##select variables we want 
##sess_overall_percent absence rate
##sess_overall_percent_pa_10_exact  Overall absence rate (persistent absentees only)
##perm_excl_rate Permanent exclusions (rate)
##susp_rate Suspension (rate)
table(final$sixth_form)

##during the period 2017-2024
table(final$period)
final <- final[period!="2013_2014"& period!="2014_2015"&period!="2006_2007"
               &period!="2007_2008"& period!="2008_2009"& period!="2009_2010"
               &period!="2010_2011"&period!="2011_2012"&period!="2012_2013"&period!="2015_2016"
               &period!="2016_2017",]
##fill the missing values
setorder(final, school_urn, period)


####################sixth form
##1.assume six form does not change
final[, sixth_form:=nafill_str(sixth_form), school_urn]
final[, sixth_form := {
  sixth_form_num <- as.numeric(as.factor(sixth_form)) 
  sixth_form_num <- nafill(sixth_form_num, type = 'locf') 
  sixth_form_num <- nafill(sixth_form_num, type = 'nocb') 
  factor(sixth_form_num, labels = levels(as.factor(sixth_form)))
}, by = school_urn]

####still quite a lot missing values for sixth_form, lets use 2022-2023 school information to speculate
ifsix<- fread(paste0(file_path, "/input data/school level information data/2022-2023_england_school_information.csv"))
ifsix <- ifsix[, .(URN,ISPRIMARY,ISSECONDARY,ISPOST16)]
final <- merge(final,ifsix, by.x = c("school_urn"), by.y = c("URN"),all.x = TRUE)
setcolorder(final, c("school_urn", "ISPOST16", "ISSECONDARY", "sixth_form"))
final[, sixth_form:=nafill_str(sixth_form), school_urn]
final[, ISPOST16:=nafill(ISPOST16), school_urn]
table(final$sixth_form)

six_final <- final[sixth_form == "Yes"|ISPOST16== "1", ]
six_final[,sixth:=1]
table(six_final$period)

##intervention school name: Thornleigh Salesian College

#####fill other missing values (matching criteria)
##1. free-meal percentage
num_na_pct_fsm <- sum(is.na(six_final$pct_fsm))
num_na_pct_fsm ##143
##use the average to fill the missing values
six_final[, pct_fsm := fifelse(is.na(pct_fsm), mean(pct_fsm, na.rm = TRUE), pct_fsm), by = school_urn]
##still have 15 NAs without any observation during this period
# Replace any remaining NA with the overall mean
overall_mean <- mean(six_final$pct_fsm, na.rm = TRUE)
six_final[is.na(pct_fsm), pct_fsm := overall_mean]
#####now no missing values for pct_fsm

##2.use postcode to match IMD score
num_na_imd <- sum(is.na(six_final$postcode))
num_na_imd ##182
six_final[, postcode:=nafill_str(postcode), school_urn]
###14 missing values and do it by hand
six_final[school_urn == 105280, postcode := "BL7 9AB"]
six_final[school_urn == 106818, postcode := "DN2 6AY"]
six_final[school_urn == 107796, postcode := "WF14 0DQ"]
six_final[school_urn == 113044, postcode := "DE22 3BH"]
six_final[school_urn == 136360, postcode := "DE73 5UB"]
six_final[school_urn == 139051, postcode := "DE24 0AN"]
six_final[, postcode:=nafill_str(postcode), school_urn]
six_final <- merge(six_final, lsoa11, by.x = "postcode", by.y = "pcds", all.x = TRUE)
setcolorder(six_final, c("postcode", "lsoa11cd","year"))
num_na_imd <- sum(is.na(six_final$lsoa11cd))
num_na_imd ##0 missing values for postcode

six_final<- merge(six_final, IMD, by.x = "lsoa11cd", by.y = "LSOA code (2011)", all.x = TRUE)
num_na_imd <- sum(is.na(six_final$`Overall Index of Multiple Deprivation (IMD) Score`))
num_na_imd ##0

##3.percentage of students receiving Special Educational Needs (SEN) 
six_final[, pct_sen := fifelse(is.na(pct_sen), mean(pct_sen, na.rm = TRUE), pct_sen), by = school_urn]
num_na_pct_sen <- sum(is.na(six_final$pct_sen))
num_na_pct_sen ##2
overall_mean <- mean(six_final$pct_sen, na.rm = TRUE)
six_final[is.na(pct_sen), pct_sen := overall_mean]
####no missing values


###4.school type
table(six_final$school_type)
six_final[, school_type:=nafill_str(school_type), school_urn]
setcolorder(six_final, c("school_urn", "school_type"))
six_final <- six_final[school_type=="state-funded secondary"]


#######running the synthetic control 
##generate academic start year
six_final[, year := NULL]
six_final[, year := substr(period, 1, 4)]
table(six_final$year)

final_regression <- six_final[, 
  .(school_name.x, school_urn, year, sess_overall_percent, sess_overall_percent_pa_10_exact, 
    perm_excl_rate, susp_rate, pct_fsm, `Overall Index of Multiple Deprivation (IMD) Score`, pct_sen)
]
final_regression[, school_name.x:=nafill_str(school_name.x), school_urn]

###make sure the data is the panel data
final_regression[, number := .N, by = school_urn]
table(final_regression$number)
final_regression <- final_regression[number==7, ]
##1 treatment unit and 95 comparison unit

treated_unit <- "Thornleigh Salesian College"
treatment_year <- c(2022,2023)
pre_treat_years <- c(2017, 2018, 2019, 2020, 2021)
predictors <- c("pct_fsm", "Overall Index of Multiple Deprivation (IMD) Score", "pct_sen")

final_regression[, year := as.integer(as.numeric(year))]
final_regression[, pct_fsm := as.numeric(pct_fsm)] 
final_regression[, `Overall Index of Multiple Deprivation (IMD) Score` := as.numeric(`Overall Index of Multiple Deprivation (IMD) Score`)]
final_regression[, `pct_sen` := as.numeric(`pct_sen`)]
final_regression[, `perm_excl_rate` := as.numeric(`perm_excl_rate`)]
final_regression[, `sess_overall_percent_pa_10_exact` := as.numeric(`sess_overall_percent_pa_10_exact`)]
final_regression[, `susp_rate` := as.numeric(`susp_rate`)]
final_regression[, `sess_overall_percent` := as.numeric(`sess_overall_percent`)]
final_regression[, school_name.x:=nafill_str(school_name.x), school_urn]

table(final_regression$year)
##mark as missing values for dependent variable
final_regression$sess_overall_percent[final_regression$year == 2019] <- NA

##some controls have missing records
final_regression2 <- final_regression[school_name.x!="Astrea Academy Woodfields"]
final_regression2 <- final_regression2[school_name.x!="The Holy Family Catholic School"]
final_regression2 <- final_regression2[school_name.x!="Hanson School"]
treated_data <- final_regression2[school_name.x == treated_unit]
control_data <- final_regression2[school_name.x != treated_unit]

dataprep_out <- dataprep(
  foo = as.data.frame(final_regression2),  
  predictors = predictors,               
  predictors.op = "mean",                
  dependent = "sess_overall_percent",                   
  unit.variable = "school_urn",         
  time.variable = "year",                
  treatment.identifier = treated_data$school_urn[1], 
  controls.identifier = unique(control_data$school_urn), 
  time.predictors.prior = pre_treat_years[pre_treat_years != 2019],  
  time.optimize.ssr = pre_treat_years[pre_treat_years != 2019],     
  unit.names.variable = "school_name.x",
  time.plot = c(2017, 2018, 2020, 2021, 2022,2023)  
)

synth_out <- synth(dataprep_out)
synth_tables1 <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
print(synth_tables1$tab.pred)

dataprep_out$time.plot <- dataprep_out$tag$time.plot

residuals <- dataprep_out$Y1plot - (dataprep_out$Y0plot %*% synth_out$solution.w)
ci_sd <- sd(residuals, na.rm = TRUE)  

ci_upper_treated <- dataprep_out$Y1plot + 1.96 * ci_sd  
ci_lower_treated <- dataprep_out$Y1plot - 1.96 * ci_sd

synth_values <- dataprep_out$Y0plot %*% synth_out$solution.w
ci_upper_synth <- synth_values + 1.96 * ci_sd
ci_lower_synth <- synth_values - 1.96 * ci_sd

# Academic year labels
# Full year ticks including 2019
# 1. X values corresponding to actual data points (excluding 2019)
x_coords <- c(2017, 2018, 2020, 2021, 2022, 2023)

# 2. Full x-axis tick marks for labelling (including 2019 for visual continuity)
year_ticks <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
school_years <- paste0(year_ticks, "/", year_ticks + 1)

# 3. Export to high-resolution PNG
png("absence_without_2019_label_added.png", width = 8, height =5.6, units = "in", res = 300)

# 4. Graphical parameters
par(mar = c(6, 5, 3, 2),
    family = "sans",
    cex.lab = 1.4,
    cex.axis = 1.2,
    font.lab = 2,
    bty = "n")

# 5. Initialize an empty plot using only available data years
plot(x_coords,
     dataprep_out$Y1plot,
     type = "n",
     ylim = range(c(ci_lower_treated, ci_upper_treated, ci_lower_synth, ci_upper_synth), na.rm = TRUE),
     xlab = "",
     ylab = "Absence Rate",
     xaxt = "n",
     bty = "n")

# 6. Shaded confidence intervals
polygon(c(x_coords, rev(x_coords)),
        c(ci_upper_treated, rev(ci_lower_treated)),
        col = rgb(0.2, 0.4, 0.8, 0.2), border = NA)

polygon(c(x_coords, rev(x_coords)),
        c(ci_upper_synth, rev(ci_lower_synth)),
        col = rgb(0.9, 0.3, 0.3, 0.2), border = NA)

# 7. Plot the main trend lines
lines(x_coords, dataprep_out$Y1plot, col = "#1f78b4", lwd = 3)
lines(x_coords, synth_values, col = "#e31a1c", lwd = 3)

# 8. Add vertical line to indicate the start of treatment
abline(v = 2022, col = "darkgray", lty = 2, lwd = 2)

# 9. Draw x-axis tick marks (including 2019) without labels
axis(1, at = year_ticks, labels = FALSE)

# 10. Add rotated academic year labels aligned with ticks
text(x = year_ticks,
     y = par("usr")[3] - 0.5,
     labels = school_years,
     srt = 45, adj = 1, xpd = TRUE, cex = 0.9, font = 2)

# 11. Add legend
legend("topleft",
       legend = c("Treated Unit", "Synthetic Control", "Treatment Start (2022)"),
       col = c("#1f78b4", "#e31a1c", "darkgray"),
       lty = c(1, 1, 2),
       lwd = 3,
       bty = "n",
       text.font = 2,
       cex = 1.1)

# 12. Add x-axis label
mtext("Academic Year", side = 1, line = 4.5, cex = 1.4, font = 2)

# 13. Close the graphics device
dev.off()

########################################################  
  # Outcome 2: sess_overall_percent_pa_10_exact  
##some controls have missing records
treated_data <- final_regression2[school_name.x == treated_unit]
control_data <- final_regression2[school_name.x != treated_unit]

  dataprep_out <- dataprep(
    foo = as.data.frame(final_regression2),  
    predictors = predictors,               
    predictors.op = "mean",                
    dependent = "sess_overall_percent_pa_10_exact",                   
    unit.variable = "school_urn",         
    time.variable = "year",                
    treatment.identifier = treated_data$school_urn[1], 
    controls.identifier = unique(control_data$school_urn), 
    time.predictors.prior = pre_treat_years[pre_treat_years != 2019], 
    time.optimize.ssr = pre_treat_years[pre_treat_years != 2019],     
    unit.names.variable = "school_name.x",
    time.plot = c(2017, 2018, 2020, 2021, 2022,2023)
  )
  
  synth_out <- synth(dataprep_out)
  synth.tables1 <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  print(synth.tables1$tab.pred)
  
  # Plot Results
  dataprep_out$time.plot <- sort(unique(final_regression$year))
  residuals <- dataprep_out$Y1plot - (dataprep_out$Y0plot %*% synth_out$solution.w)
  ci_sd <- sd(residuals, na.rm = TRUE)  
  
  # Confidence intervals for treated unit
  ci_upper_treated <- dataprep_out$Y1plot + 1.96 * ci_sd  # 95% CI
  ci_lower_treated <- dataprep_out$Y1plot - 1.96 * ci_sd
  
  # Confidence intervals for synthetic control
  synth_values <- dataprep_out$Y0plot %*% synth_out$solution.w
  ci_upper_synth <- synth_values + 1.96 * ci_sd
  ci_lower_synth <- synth_values - 1.96 * ci_sd
  
  # X values used in model output (no 2019)
  x_coords <- c(2017, 2018, 2020, 2021, 2022, 2023)
  
  # Full ticks for axis labeling (includes 2019)
  year_ticks <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
  school_years <- paste0(year_ticks, "/", year_ticks + 1)
  
  # Export to high-resolution PNG
  png("persistent_absentees_only_with_2019_label.png", 
      width = 8, height = 5.6, units = "in", res = 300)
  
  # Graphical settings
  par(mar = c(6, 5, 3, 2),
      family = "sans",
      cex.lab = 1.4,
      cex.axis = 1.2,
      font.lab = 2,
      bty = "n")
  
  # Empty plot with x_coords for data only (no 2019)
  plot(x_coords,
       dataprep_out$Y1plot,
       type = "n",
       ylim = range(c(ci_lower_treated, ci_upper_treated, ci_lower_synth, ci_upper_synth)),
       xlab = "",
       ylab = "Persistent Absence Rate",
       xaxt = "n",
       bty = "n")
  
  # Draw full x-axis ticks (including 2019) without labels
  axis(1, at = year_ticks, labels = FALSE)
  
  # Add tilted academic year labels (includes 2019)
  text(x = year_ticks,
       y = par("usr")[3] - 0.5,
       labels = school_years,
       srt = 45, adj = 1, xpd = TRUE, cex = 0.9, font = 2)
  
  # Confidence interval shading
  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_treated, rev(ci_lower_treated)),
          col = rgb(0.2, 0.4, 0.8, 0.2), border = NA)
  
  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_synth, rev(ci_lower_synth)),
          col = rgb(0.9, 0.3, 0.3, 0.2), border = NA)
  
  # Plot main lines
  lines(x_coords, dataprep_out$Y1plot, col = "#1f78b4", lwd = 3)
  lines(x_coords, synth_values, col = "#e31a1c", lwd = 3)
  
  # Treatment intervention marker
  abline(v = 2022, col = "darkgray", lty = 2, lwd = 2)
  
  # Legend
  legend("topleft",
         legend = c("Treated Unit", "Synthetic Control", "Treatment Start (2022)"),
         col = c("#1f78b4", "#e31a1c", "darkgray"),
         lty = c(1, 1, 2),
         lwd = 3,
         bty = "n",
         text.font = 2,
         cex = 1.1)
  
  # X-axis label
  mtext("Academic Year", side = 1, line = 4.5, cex = 1.4, font = 2)
  
  # Close device
  dev.off()
  
  ########################################################  
  # Outcome 3: perm_excl_rate 
  final_regression$perm_excl_rate <- as.numeric(final_regression$perm_excl_rate)
  setcolorder(final_regression, c("school_urn", "perm_excl_rate","susp_rate"))
  dataprep_out <- dataprep(
    foo = as.data.frame(final_regression2),  
    predictors = predictors,               
    predictors.op = "mean",                
    dependent = "perm_excl_rate",                   
    unit.variable = "school_urn",         
    time.variable = "year",                
    treatment.identifier = treated_data$school_urn[1], 
    controls.identifier = unique(control_data$school_urn), 
    time.predictors.prior = pre_treat_years, 
    time.optimize.ssr = pre_treat_years,     
    unit.names.variable = "school_name.x",
    time.plot = 2017:2023
  )
  
  synth_out <- synth(dataprep_out)
  synth.tables1 <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  print(synth.tables1$tab.pred)
  
  # Plot Results
  dataprep_out$time.plot <- sort(unique(final_regression$year))
  residuals <- dataprep_out$Y1plot - (dataprep_out$Y0plot %*% synth_out$solution.w)
  ci_sd <- sd(residuals, na.rm = TRUE)  
  
  # Confidence intervals for treated unit
  ci_upper_treated <- dataprep_out$Y1plot + 1.96 * ci_sd  # 95% CI
  ci_lower_treated <- dataprep_out$Y1plot - 1.96 * ci_sd
  
  # Confidence intervals for synthetic control
  synth_values <- dataprep_out$Y0plot %*% synth_out$solution.w
  ci_upper_synth <- synth_values + 1.96 * ci_sd
  ci_lower_synth <- synth_values - 1.96 * ci_sd
  
  # Academic year labels and x-axis
  school_years <- paste0(2017:2023, "/", 2018:2024)
  x_coords <- dataprep_out$time.plot
  
  # Output to high-resolution PNG
  png("permanent_exclusion_rate_2017_2023.png", 
      width = 8, height = 5.6, units = "in", res = 300)
  
  par(mar = c(6, 5, 3, 2),
      family = "sans",
      cex.lab = 1.4,
      cex.axis = 1.2,
      font.lab = 2,
      bty = "n")
  
  plot(x_coords,
       dataprep_out$Y1plot,
       type = "n",
       ylim = range(c(ci_lower_treated, ci_upper_treated, ci_lower_synth, ci_upper_synth)),
       xlab = "",
       ylab = "Permanent Exclusion Rate",
       xaxt = "n",
       bty = "n")
  
  # Custom x-axis with tilted academic years
  axis(1, at = x_coords, labels = FALSE)
  text(x = x_coords, y = par("usr")[3]-0.05 ,
       labels = school_years, srt = 45, adj = 1, xpd = TRUE, cex = 0.9, font = 2)
  
  # Confidence interval shading
  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_treated, rev(ci_lower_treated)),
          col = rgb(0.2, 0.4, 0.8, 0.2), border = NA)
  
  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_synth, rev(ci_lower_synth)),
          col = rgb(0.9, 0.3, 0.3, 0.2), border = NA)
  
  # Main lines
  lines(x_coords, dataprep_out$Y1plot, col = "#1f78b4", lwd = 3)
  lines(x_coords, synth_values, col = "#e31a1c", lwd = 3)
  
  # Vertical line for treatment
  abline(v = 2022, col = "darkgray", lty = 2, lwd = 2)
  
  # Legend
  legend("topleft",
         legend = c("Treated Unit", "Synthetic Control", "Treatment Start (2022)"),
         col = c("#1f78b4", "#e31a1c", "darkgray"),
         lty = c(1, 1, 2),
         lwd = 3,
         bty = "n",
         text.font = 2,
         cex = 1.1)
  
  # X-axis label
  mtext("Academic Year", side = 1, line = 4.5, cex = 1.4, font = 2)
  
  dev.off()
  
  ########################################################  
  # Outcome 4: susp_rate 
  dataprep_out <- dataprep(
    foo = as.data.frame(final_regression2),  
    predictors = predictors,               
    predictors.op = "mean",                
    dependent = "susp_rate",                   
    unit.variable = "school_urn",         
    time.variable = "year",                
    treatment.identifier = treated_data$school_urn[1], 
    controls.identifier = unique(control_data$school_urn), 
    time.predictors.prior = pre_treat_years, 
    time.optimize.ssr = pre_treat_years,     
    unit.names.variable = "school_name.x",
    time.plot = 2017:2023
  )
  
  synth_out <- synth(dataprep_out)
  synth.tables1 <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  print(synth.tables1$tab.pred)
  
  # Plot Results
  dataprep_out$time.plot <- sort(unique(final_regression$year))
  residuals <- dataprep_out$Y1plot - (dataprep_out$Y0plot %*% synth_out$solution.w)
  ci_sd <- sd(residuals, na.rm = TRUE)  
  
  # Confidence intervals for treated unit
  ci_upper_treated <- dataprep_out$Y1plot + 1.96 * ci_sd  # 95% CI
  ci_lower_treated <- dataprep_out$Y1plot - 1.96 * ci_sd
  
  # Confidence intervals for synthetic control
  synth_values <- dataprep_out$Y0plot %*% synth_out$solution.w
  ci_upper_synth <- synth_values + 1.96 * ci_sd
  ci_lower_synth <- synth_values - 1.96 * ci_sd
  

  school_years <- paste0(2017:2023, "/", 2018:2024)
  x_coords <- dataprep_out$time.plot
  

  png("synthetic_control_suspension_rate.png", 
      width = 8,           
      height = 5.6,           
      units = "in",        
      res = 300)            

  par(mar = c(6, 5, 3, 2),      
      family = "sans",        
      cex.lab = 1.4,              
      cex.axis = 1.2,            
      font.lab = 2,                 
      bty = "n")                     
  

  plot(x_coords,
       dataprep_out$Y1plot,
       type = "n",
       ylim = range(c(ci_lower_treated, ci_upper_treated, ci_lower_synth, ci_upper_synth)),
       xlab = "",  
       ylab = "Suspension Rate",
       xaxt = "n",
       bty = "n")
  

  axis(1, at = x_coords, labels = FALSE)  
  text(x = x_coords, y = par("usr")[3] - 1.5, 
       labels = school_years, srt = 45, adj = 1, xpd = TRUE, cex = 0.9, font = 2)
  

  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_treated, rev(ci_lower_treated)),
          col = rgb(0.2, 0.4, 0.8, 0.2), border = NA)
  
  polygon(c(x_coords, rev(x_coords)),
          c(ci_upper_synth, rev(ci_lower_synth)),
          col = rgb(0.9, 0.3, 0.3, 0.2), border = NA)
  

  lines(x_coords, dataprep_out$Y1plot, col = "#1f78b4", lwd = 3)
  lines(x_coords, synth_values, col = "#e31a1c", lwd = 3)
  
  abline(v = 2022, col = "darkgray", lty = 2, lwd = 2)
  

  legend("topleft",
         legend = c("Treated Unit", "Synthetic Control", "Treatment Start (2022)"),
         col = c("#1f78b4", "#e31a1c", "darkgray"),
         lty = c(1, 1, 2),
         lwd = 3,
         bty = "n",              
         text.font = 2,        
         cex = 1.1)
  

  mtext("Academic Year", side = 1, line = 4.5, cex = 1.4, font = 2)
  
  dev.off() 
