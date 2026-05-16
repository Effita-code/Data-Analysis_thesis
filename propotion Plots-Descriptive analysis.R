rm(list=ls())

###==========================================================================
#Create a file path to the data cleaning code
###==========================================================================
source("Adila_DataCleaning2025May19.R")

##==============================================================================
##libraries (put them in a loop later)
##==============================================================================
lod.pckgs2<- c("hrbrthemes", "plotly", "patchwork", "babynames", "viridis",
              "binom", "broom","dplyr","ggplot2", "janitor","lubridate")


lod.pckgs2
for(x in 1:length(lod.pckgs2))
  library(package = lod.pckgs2[x], character.only = TRUE)

##================================================================######
#Code for table 1 (demographic and clinical characteristics)
##======================================================================

###Remove the trial surveys (experimental surveys)

table(merged_df$surveynumber)

##Create a data frame for Prevalence table scripts

analysis_df_prevalence_data <- merged_df %>%  
  filter(!(surveynumber %in%(c(15,17))))
        
## create dataframe for proportion plots

analysis_df <- merged_df %>%  
 filter(!(surveynumber %in% c(15,17)))
        #, 
    #Department != "Obstetrics and Gynaecology")

#### fix a redundancy/ survey 1 was showing in February

## create a vector 

month_vector <- c(
  "1" = "January",
  "2" = "February",
  "3" = "March",
  "4" = "April",
  "5" = "May",
  "6" = "June",
  "7" = "July",
  "8" = "August",
  "9" = "September",
  "10" = "October",
  "11" = "November",
  "12" = "December",
  "13" = "January",
  "14" = "February",
  "16" = "March",
  "18" = "April"
)

analysis_df$Month <- month_vector[as.character(analysis_df$surveynumber)]

### introduce new variable with year and month

analysis_df$year_month <- paste(analysis_df$Year, analysis_df$Month, sep = "-")


## Select variables and calculate proportions

proportion_data1 <- analysis_df %>% 
  mutate(
    HIVstatus = ifelse(is.na(HIVstatus), 2, HIVstatus),
    HIVstatus = factor(HIVstatus,
    levels = c(0, 1, 2), labels = c("Negative", "Positive", "Unknown"))) %>%
  dplyr::select(Agegroups, Sex, HIVstatus, AnyMorbidity, 
                  WardType, WardActivity, HAI_Patient) %>% 
  pivot_longer(cols = c(Agegroups, Sex, HIVstatus, AnyMorbidity, WardType,
                      WardActivity, HAI_Patient),
              names_to = "Variable", values_to = "Category") %>% 
  filter(!is.na(Category))



# Proportions using the total count of each unique Variable as the denominator
proportion_summary <- proportion_data1 %>%
  group_by(Variable, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(Total_Variable_Count = sum(Count), 
         Percentage = (Count / Total_Variable_Count) * 100) %>%
  select(Variable, Category, Count, Percentage)

# results
print(proportion_summary)


####Pust

write.csv(proportion_summary, file = "firsttable.csv")

###===========================================================================
##Create a function to stratify table per department
###===========================================================================


# Define the function
department_summary <- function(analysis_df, department_name) {
  analysis_df %>%
    filter(Department == department_name) %>%
    mutate(HIVstatus = factor(HIVstatus,
                              levels = c(0, 1, 2),
                              labels = c("Negative", "Positive", "Unknown"))) %>%
    dplyr::select(Agegroups, Sex, HIVstatus, AnyMorbidity, 
                  WardType, WardActivity, HAI_Patient) %>% 
    pivot_longer(cols = c(Agegroups, Sex, HIVstatus, AnyMorbidity, 
                          WardType, WardActivity, HAI_Patient),
                 names_to = "Variable", values_to = "Category") %>% 
    filter(!is.na(Category)) %>%
    group_by(Variable, Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Variable) %>%
    mutate(Total_Variable_Count = sum(Count), 
           Percentage = (Count / Total_Variable_Count) * 100) %>%
    select(Variable, Category, Count, Percentage)
}

##========Pull out summaries 
Med_prop_data <- department_summary(analysis_df, "Medical")
Surg_prop_data <- department_summary(analysis_df, "Surgical")
Peads_prop_data  <- department_summary(analysis_df, "Paediatrics")
Obs_prop_data <- department_summary(analysis_df, "Obstetrics and Gynaecology")
icu_prop_data <- department_summary(analysis_df, "ICU")
##check output

Surg_prop_data
Obs_prop_data
Peads_prop_data
Med_prop_data
icu_prop_data

write.csv(Surg_prop_data, file = "surgerytable.csv")
write.csv(Obs_prop_data, file = "ObsGYanetable.csv")
write.csv(Peads_prop_data, file = "Paedstable.csv")
write.csv(Med_prop_data, file = "Medicinetable.csv")
write.csv(icu_prop_data, file = "icu.csv")
###===========================================================================
#Visually show the trends of ABU over one year - Main trend
###===========================================================================

# number of patients on antibiotics per month
patients_on_antibiotics <- analysis_df %>% dplyr::select(year_month,Ward,Patients) %>%
  group_by(year_month,) %>%
  summarise(total_on_antibiotics = n())

# Sum the unique total patients per ward per month
total_patients_per_month <- analysis_df %>% dplyr::select(year_month,Ward,Patients) %>% 
  group_by(year_month, Ward) %>%
  summarise(total_patients = unique(Patients)) %>%
  group_by(year_month,) %>%
  summarise(total_patients = sum(total_patients)) 

# Merging both summaries and calculate the proportion
trenddata <- left_join(patients_on_antibiotics, total_patients_per_month, by = "year_month") %>%
  mutate(proportion_on_antibiotics = total_on_antibiotics / total_patients * 100,
         Department="All")

print(trenddata)
##============================================================================================================
## Formating the month-year variable
##============================================================================================================
##month and year 

months_2024 <- paste("2024", month.name, sep = "-")
months_2025 <- paste("2025", month.name[1:4], sep = "-")  

#convert to character 

trenddata$year_month <- as.character(trenddata$year_month)

###order the timeline 

timeline_order <- c(months_2024, months_2025)

trenddata$year_month <- factor(trenddata$year_month, levels = timeline_order, ordered = TRUE)

levels(trenddata$year_month)

##shorten the x axis labels and order the months for ggplot 

trenddata <- trenddata[order(as.Date(paste0(trenddata$year_month, "-01"), "%Y-%B-%d")), ]

trenddata$month_short <- format(as.Date(paste0(trenddata$year_month, "-01"), "%Y-%B-%d"), "%b %Y")

trenddata$month_short <- factor(trenddata$month_short, levels = unique(trenddata$month_short), ordered = TRUE)


##check
levels(trenddata$month_short)

print(trenddata)


###calculate Binomial Confidence intervals

trenddata_CIs <- trenddata %>% 
             mutate(
            CI_results = binom::binom.wilson(x = total_on_antibiotics, n = total_patients, conf.level = 0.95),
            Lower_CI = CI_results$lower * 100,
            Upper_CI = CI_results$upper * 100) 
          
trenddata_with_CIs <- trenddata_CIs %>%  select(-CI_results)

print(trenddata_with_CIs)


### Line plot with Binomial confidence intervals

plot1_CIs <- ggplot(trenddata_with_CIs, aes(x = month_short, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color="darkseagreen", lwd = 1 ) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI),
                width = 0.2, 
                color = "darkgreen",
                size = 0.8) +
  ylab("Proportion(%)") +
  xlab("Month") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 50)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, 
                               face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14),
    aspect.ratio = 0.5,
    axis.line = element_line(size = 1.2, colour = "black"),
    axis.ticks = element_line(size = 1.2),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  )

plot1_CIs


ggsave("lineplotCI2.png", plot = plot1_CIs, bg= "white", width = 12, height = 8)


###check the intervals 

# Display the proportion and CI widths for each month
trenddata_CIs %>%
  mutate(CI_Width = Upper_CI - Lower_CI) %>%
  select(year_month, proportion_on_antibiotics, Lower_CI, Upper_CI, CI_Width, total_patients) %>%
  arrange(CI_Width) 

ggsave("lineplotCI.png", plot = plot1_CIs, bg= "white", width = 8, height = 5)


###============================================================================
## format data for Line plot of beds, vs admissions vs number on antibiotics
##=============================================================================

#####create summary data 

bed_patient_abu <- analysis_df %>% select(Patients,Ward, Beds, year_month, Department)

monthly_summary <- bed_patient_abu %>% 
  distinct(year_month,Patients,Ward, Beds, year_month) %>%
    group_by(year_month) %>% 
    summarise(
      Beds = sum( Beds, na.rm = TRUE), 
      Admissions =sum(Patients, na.rm = TRUE),
      observ = n(), 
      .groups = "drop"
    )  %>% left_join(
      bed_patient_abu %>%
        count(year_month, name = "Observations"),  
      by = "year_month"
    )

###order the timeline of year_month

summaryline_order <- c(months_2024, months_2025)

monthly_summary$year_month <- factor(monthly_summary$year_month, levels = summaryline_order, ordered = TRUE)

levels(monthly_summary$year_month)

##shorten the x axis labels and order the months for ggplot 

monthly_summary <- monthly_summary[order(as.Date(paste0(monthly_summary$year_month, "-01"), "%Y-%B-%d")), ]

monthly_summary$month_short <- format(as.Date(paste0(monthly_summary$year_month, "-01"), "%Y-%B-%d"), "%b %Y")

monthly_summary$month_short <- factor(monthly_summary$month_short, levels = unique(monthly_summary$month_short), ordered = TRUE)


############# Try easier way of presenting bed occupancy 

monthly_occupancy <- bed_patient_abu %>%
  distinct(year_month, Ward, Patients, Beds) %>%   # one row per ward
  group_by(year_month) %>%
  summarise(
    Total_Beds = sum(Beds, na.rm = TRUE),
    Total_Patients = sum(Patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date = dmy(paste("01",
                     word(year_month, 2, sep = "-"),
                     word(year_month, 1, sep = "-"))), 
    Occupancy_percent = (Total_Patients / Total_Beds) * 100
  )%>%
  arrange(date) %>%                               # chronological order
  mutate(
    month_label = format(date, "%b-%y") 
  )


occupancy <- ggplot(monthly_occupancy, aes(x = date, y = Occupancy_percent, )) +
  geom_line(linewidth = 1, color = "#1f78b4") +
  geom_point(size = 2, color = "#1f78b4") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  geom_hline(yintercept = 85, linetype = "dashed") +
  scale_y_continuous(
    limits = c(50, 100),
    breaks = seq(50, 100, by = 10)
  ) +
  labs(
    x = "Month",
    y = "Bed occupancy (%)"
  ) +
  theme_minimal(base_size = 14) +
theme(
  axis.line = element_line(linewidth = 1.2, color = "black"),  
  axis.title = element_text(face = "bold", size = 16),    
  axis.text = element_text(face = "bold", size = 14),      # bold axis tick labels
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.ticks = element_line(linewidth = 1, color = "black"),
  axis.ticks.length = unit(0.3, "cm")
)

ggsave("occupancy.png", plot = occupancy, bg= "white", height = 8, width = 12)

### Plot occupancy by department

##format data 
dept_monthly_occupancy <- bed_patient_abu %>%
  filter(Department != "ICU") %>% 
  distinct(year_month, Department, Ward, Patients, Beds) %>%
  group_by(year_month, Department) %>%
  summarise(
    Total_Beds = sum(Beds, na.rm = TRUE),
    Total_Patients = sum(Patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date = dmy(paste("01",
                     word(year_month, 2, sep = "-"),
                     word(year_month, 1, sep = "-"))),
    Occupancy_percent = round((Total_Patients / Total_Beds) * 100, 1)
  ) %>%
  arrange(Department, date) %>%
  mutate(
    month_label = format(date, "%b-%y"),
    month_label = factor(month_label, levels = unique(month_label))
  )

## plot

##1. Line plot
dept_occupancy <- ggplot(dept_monthly_occupancy,
                         aes(x = date, y = Occupancy_percent, colour = Department)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 85, linetype = "dashed") +
  facet_wrap(~ Department) +
  labs(
    x = "Month",
    y = "Bed occupancy (%)") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  scale_y_continuous(limits = c(25, 110), breaks = seq(25, 110, 25)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(linewidth = 1.2, color = "black"),  
    axis.title = element_text(face = "bold", size = 16),    
    axis.text = element_text(face = "bold", size = 14),      # bold axis tick labels
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_line(linewidth = 1, color = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )


ggsave("occupancy-department.png", plot = dept_occupancy, bg= "white", height = 8, width = 12)



##==============================================================================
## Create stratified plot using a function 
##==============================================================================

PlotData.fn1<-function(dept1) { 
  #Data filtering 
  analysis_df2 <-analysis_df %>% filter(Department%in% dept1)
  dim(analysis_df ); dim( analysis_df2)
  
#number of patients on antibiotics per month
patients_on_antibiotics <- analysis_df2%>% dplyr::select(year_month,Ward,Patients) %>%
  group_by(year_month) %>%
  summarise(total_on_antibiotics = n())

# Sum the unique total patients per ward per month
total_patients_per_month <- analysis_df2 %>% dplyr::select(year_month,Ward,Patients) %>% 
  group_by(year_month, Ward) %>%
  summarise(total_patients = unique(Patients)) %>%
  group_by(year_month) %>%
  summarise(total_patients = sum(total_patients))  

# Step 3: Merge both summaries and calculate the proportion
trenddata <- left_join(patients_on_antibiotics, total_patients_per_month, by = "year_month") %>%
  mutate(proportion_on_antibiotics = total_on_antibiotics / total_patients * 100,
         Department=dept1)
  

return(trenddata)
} 

#Application of the PlotData.fn1 function (USE THIS TO DEVELOP SEPARATE PLOTS AND MERGE WITH PATCHWORK)

Medical <- PlotData.fn1(dept1 = "Medical")
Surgical <- PlotData.fn1(dept1 = "Surgical")
Paeds <- PlotData.fn1(dept1 = "Paediatrics")
Obs_gynae <- PlotData.fn1(dept1 = "Obstetrics and Gynaecology")


#### start from here tommorrow

Departments <- unique(analysis_df$Department)

dept_plot_df <- lapply (Departments, function(x) PlotData.fn1(dept1 = x)) %>%
  purrr::reduce(rbind.data.frame)

glimpse(dept_plot_df)
dim(trenddata); dim(dept_plot_df)

##Append with total data 

dept_trend_data <- bind_rows(trenddata, dept_plot_df)

dim(dept_trend_data)
str(dept_trend_data)

#fix chronolgical order of months 

month_levels <- dept_trend_data %>%
  distinct(year_month) %>%
  mutate(date = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d")) %>%
  arrange(date) %>%
  pull(year_month)


#### Create a monthly character variable that is short 
### Since the month is looking funky on plots, try to use numbers as strings 

dept_trend_data <- dept_trend_data %>%
  mutate(year_month = factor(year_month, levels = month_levels), 
         date_full = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"),
         month_short = format(date_full, "%b-%y")) %>%
  arrange(date_full) %>%
  mutate(month_short = factor(month_short, levels = unique(month_short)), 
         month_num = as.numeric(month_short), 
         month_charac = factor(as.character(month_num), levels = as.character(1:16), ordered = TRUE))


print(dept_trend_data)

table(dept_trend_data$month_num , dept_trend_data$month_charac)

# Create stratified line plot
## THIS SCETION WAS REVISED TO ADD LINEAR MODELS 
##==============================================================================
## Plot side by side using Patchwork
##==============================================================================

#1. Medical department plot 
print(Medical)

##fix dates in chron0logical order 

Medical_data <- Medical %>%
  mutate(month_year = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"))

view(Medical_data)

### internal medicine line plot 

p1 <- ggplot(Medical_data, aes(x = month_year, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color="#69b3a2", lwd = 1 ) +
  geom_point(size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(x = "Month", y = "Proportion(%)", title = "Internal Medicine") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 0.5, 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50))

p1

##2. Pediatric department 

print(Paeds)

Paeds_data <- Paeds %>%
  mutate(month_year = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"))

### Pediatric department line plot 

p2 <- ggplot(Paeds_data, aes(x = month_year, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color="#69b3a2", lwd = 1 ) +
  geom_point(size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(x = "Month", y = "Proportion(%)", title = "Paediatrics") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 0.5, 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50))

p2


##3.. Surgery department plot 

print(Surgical)

surgical_data <- Surgical %>%
  mutate(month_year = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"))

### surgery department line plot 

p3 <- ggplot(surgical_data, aes(x = month_year, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color="#69b3a2", lwd = 1 ) +
  geom_point(size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(x = "Month", y = "Proportion(%)", title = "Surgery") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 0.5, 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50))

p3

##3.. Surgery department plot 

print(Obs_gynae)

obs_data <- Obs_gynae %>%
  mutate(month_year = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"))

### surgery department line plot 

p4 <- ggplot(obs_data, aes(x = month_year, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color="#69b3a2", lwd = 1 ) +
  geom_point(size=1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(x = "Month", y = "Proportion(%)", title = "Obstetrics & Gynaecology") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 0.5, 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 50))


##==============================================================================
## merge plots with patchwork
##==============================================================================

## patch them 

(p1 + p2) / (p3 + p4)

## remove months labels up there 

  
 # Create a custom theme to hide the x-axis
  theme_no_x_axis <- theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank() 
  )
  
  
  # Apply the theme to the top two plots (p1 and p2)
  
  p1_modified <- p1 +
    theme_no_x_axis +
    labs(x = NULL)
  
  p2_modified <- p2 +
    theme_no_x_axis +
    labs(x = NULL, y = NULL)
  
  p3_modified <- p3 
  
  
  p3_modified
  
p4_modified <- p4 + 
    labs( y = NULL)
  
# Combine the plots using patchwork

p5 <- (p1_modified + p2_modified) / (p3_modified + p4_modified)

p5

ggsave("Departments.png", plot = p5, bg= "white", height = 6,
       width = 10, units = "in", dpi = 300)
##==============================================================================
### Change colors  (dO THIS LATER)
##==============================================================================
### get the palette 
library(RColorBrewer)

my_colors <- brewer.pal(n = 4, name = "Dark2")
print(my_colors)

# Print the hex codes to see the colors
print(my_colors)

## color each plot 

p1_colored <- p1_modified +
  geom_line(color = my_colors[1]) +
  geom_point(color = my_colors[1])

p2_colored <- p2_modified +
  geom_line(color = my_colors[2]) +
  geom_point(color = my_colors[2])

p3_colored <- p3_modified +
  geom_line(color = my_colors[3]) +
  geom_point(color =my_colors[3])

p4_colored <- p4_modified+
  geom_line(color = my_colors[4]) +
  geom_point(color = my_colors[4])

p6 <- (p1_colored + p2_colored) / (p3_colored + p4_colored)

p6

# Add a single title for the entire plot.


##============================================================================
### Linear Models  
##===========================================================================
## List of data sets 

Medical 
Surgical 
Paeds
Obs_gynae

## Medical department
#1.convert month-year variable to time 
#Medical$month_year <- parse_date_time(Medical$year_month, orders = "Y-b")
#2. Run the Linear model 
#medmodel <- lm(proportion_on_antibiotics ~ time_index, data = Medical)
#3. summary
#summary(medmodel)

### Create a function to automate the steps for the linear model 

department_lm <- function(dept_data) {
  
  # Convert year_month to date
  dept_data$month_year <- parse_date_time(dept_data$year_month, orders = "Y-b")
  
  # Create time index
  if(!"time_index" %in% names(dept_data)){
    dept_data <- dept_data[order(dept_data$month_year), ]
    dept_data$time_index <- 1:nrow(dept_data)
  }
  
  # Run linear model
  model <- lm(proportion_on_antibiotics ~ time_index, data = dept_data)
  
  # Print summary
  print(summary(model))
  
  # Return model object if needed for further analysis
  return(model)
}

medical_lm <- department_lm(Medical)
surgical_lm <-  department_lm(Surgical) 
Paeds_lm <- department_lm(Paeds)
Obs_gynae_lm <- department_lm(Obs_gynae)

##
install.packages("modelsummary")
library(modelsummary)

modelsummary(
  list(
    "Medical" = medical_lm,
    "Surgical" = surgical_lm,
    "Paediatrics" = Paeds_lm,
    "Obs & Gynae" = Obs_gynae_lm
  ),
  statistic = "({std.error})",
  stars = TRUE
)

##=============================================================================
#### Format model outputs into one table 
##=============================================================================

# Create the summary table
dept_summary <- data.frame(
  Department = c("Medical", "Surgical", "Paediatrics", "Obs/Gynae"),
  Intercept = c(24.5846, 22.6487, 22.80775, 8.5413),
  Time_trend = c(0.4542, -0.7905, 0.23548, 0.5199),
  p_value = c(0.1244, 0.00208, 0.02604, 0.010002),
  R2 = c(0.1603, 0.5035, 0.3067, 0.3876)
)

# Add interpretation column
dept_summary$Interpretation <- with(dept_summary, ifelse(
  p_value < 0.05 & Time_trend > 0, "Significant increase",
  ifelse(p_value < 0.05 & Time_trend < 0, "Significant decline", 
         "No significant trend")
))

# Round numeric columns for clean display
dept_summary$Intercept <- round(dept_summary$Intercept, 1)
dept_summary$Time_trend <- round(dept_summary$Time_trend, 2)
dept_summary$p_value <- round(dept_summary$p_value, 3)
dept_summary$R2 <- round(dept_summary$R2, 2)

# Add + / – sign to time trends for clarity
dept_summary$Time_trend <- ifelse(dept_summary$Time_trend > 0,
                                  paste0("+", dept_summary$Time_trend),
                                  as.character(dept_summary$Time_trend))

# Print the table
dept_summary
# Export the table to a CSV file
write.csv(dept_summary, "department_linear_models_summary.csv", row.names = FALSE)

###============================================================================
## Linear model with time as a categoriacl variable and Likelihood ratio test 
##============================================================================

### create seasonal variable in the department data sets 

create_season <- function(data, month_var = "year_month") {
  data %>%
    mutate(
      month_year = parse_date_time(.data[[month_var]], orders = "Y-b"),
      month_num = month(month_year),
      season = case_when(
        month_num %in% c(11,12,1,2,3,4) ~ "Rainy",
        month_num %in% 5:8 ~ "Cool-dry",
        month_num %in% 9:10 ~ "Hot-dry"
      ),
      season = factor(season, levels = c("Rainy","Cool-dry","Hot-dry"))
    )
}

#create new data frames for each department with season
Medical_season <- create_season(Medical)
Surgical_season <- create_season(Surgical)
Paeds_season <- create_season(Paeds)
ObsGynae_season <- create_season(Obs_gynae)

###===================== LINEAR model (season) with LIKELIHOOD TEST

dep_cat_lm <- function(dept_data) {
  
  # Ensure time_index exists
  if(!"time_index" %in% names(dept_data)){
    dept_data <- dept_data %>%
      arrange(month_year) %>%
      mutate(time_index = 1:nrow(dept_data))
  }
  
  # Continuous-time model
  lm_time <- lm(proportion_on_antibiotics ~ time_index, data = dept_data)
  
  # Categorical (season) model
  if(!"season" %in% names(dept_data)){
    stop("Season variable not found. Please run create_season() first.")
  }
  lm_season <- lm(proportion_on_antibiotics ~ season, data = dept_data)
  
  #Likelihood ratio test
  LRT <- anova(lm_time, lm_season)
  
  #Summarize key results
  summary_table <- data.frame(
    Model = c("Linear time", "Seasonal"),
    Intercept = c(coef(lm_time)[1], coef(lm_season)[1]),
    Effect = c(coef(lm_time)[2], paste(names(coef(lm_season))[-1], round(coef(lm_season)[-1], 2), collapse=", ")),
    R_squared = c(summary(lm_time)$r.squared, summary(lm_season)$r.squared),
    stringsAsFactors = FALSE
  )
  
  #Print output
  cat("=== Linear-time model ===\n")
  print(summary(lm_time))
  
  cat("\n=== Seasonal model ===\n")
  print(summary(lm_season))
  
  cat("\n=== Likelihood ratio test: Linear vs Seasonal ===\n")
  print(LRT, width = 100)
  
  cat("\n=== Information Criteria (AIC/BIC) ===\n")
  print(AIC(lm_time, lm_season))
  print(BIC(lm_time, lm_season))
  #Return everything
  return(list(
    lm_time = lm_time,
    lm_season = lm_season,
    LRT = LRT,
    summary_table = summary_table
  ))
}


### ==========================================================================
#Apply function for lm and LRT
##============================================================================
# Run categorical LM + LRT
Medical_results <- dep_cat_lm(Medical_season)
Paediatrics_results <- dep_cat_lm(Paeds_season)
Surgical_results <- dep_cat_lm(Surgical_season)
OBG_results <- dep_cat_lm(ObsGynae_season)
#===============================================================================
## EXTRACT MODEL OUTPUT AND COMBINE RESULTS
##==============================================================================


extract_seasonal_coeffs <- function(res_obj, dept_name) {
  
  broom::tidy(res_obj$lm_season) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value) %>%
    dplyr::mutate(Department = dept_name)
}

seasonal_coeff_table <- bind_rows(
  lapply(names(all_results), function(x)
    extract_seasonal_coeffs(all_results[[x]], x)
  )
)

#export to CSV
write.csv(seasonal_coeff_table, "seasonal_lm.csv", row.names = FALSE)

##==============================================================================
## seasonality tests
##==============================================================================

#1) Kruskal Wallis test on Medical department

Medical$month_year <- parse_date_time(Medical$year_month, orders = "Y-b")

# Extract month as a factor
Medical$month <- format(Medical$month_year, "%m")

# Kruskal–Wallis test
kruskal_med <- kruskal.test(proportion_on_antibiotics ~ factor(month), data = Medical)

# Print results
kruskal_med

##============ FUNCTION

# Function to run Kruskal-Wallis
kruskal_fn <- function(df, value_var = "proportion_on_antibiotics", date_var = "year_month") {
  
  # Convert the date column to proper Date format
  df[[date_var]] <- parse_date_time(df[[date_var]], orders = "Y-b")
  
  # Extract month as a factor
  df$month <- format(df[[date_var]], "%m")
  
  # Run Kruskal-Wallis test
  kw_result <- kruskal.test(df[[value_var]] ~ factor(df$month))
  
  return(kw_result)
}

## test fn 
kruskal_fn(Medical)
kruskal_fn(Surgical)
kruskal_fn(Paeds)
kruskal_fn(Obs_gynae)


    
