rm(list = ls())

####============================================================================
### Copy file path
###=============================================================================

source("Adila_DataCleaning2025May19.R")

##==============================================================================
### ###Remove the trial surveys (experimental surveys)
#================================================================================

table(merged_df$surveynumber)

##Create a data frame for Prevalence table scripts

prevalence_df <- merged_df %>%  
  filter(!(surveynumber %in% c(15, 17))) # & Department != "Obstetrics and Gynaecology")


##==============================================================================
## Create year_month variable 
##==============================================================================
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

prevalence_df$Month <- month_vector[as.character(prevalence_df$surveynumber)]

### introduce new variable with year and month

prevalence_df$year_month <- paste(prevalence_df$Year, prevalence_df$Month, sep = "-")

##================================================================================
# calculate total prevalence
##================================================================================
#1. create a data frame with unique ward-date denominators
ward_denominators <- prevalence_df %>%
  select(Ward, year_month, Department, Patients) %>%
  distinct(Ward, year_month, .keep_all = TRUE)

view(ward_denominators)


#2. Overall prevalence

overall_prevalence <-  prevalence_df %>%
  summarise(On_antibiotics = n()) %>% 
  bind_cols(
    Total_patients = sum(ward_denominators$Patients)
  ) %>%
 rowwise() %>%
  mutate(
    ci = list(binom.confint(On_antibiotics, Total_patients, method = "wilson")),
    Prevalence_percent = round(On_antibiotics / Total_patients * 100, 1),
    CI_lower = round(ci$lower * 100, 1),
    CI_upper = round(ci$upper * 100, 1)
  ) %>%
  ungroup() %>%
  select(-ci)

print(overall_prevalence)
##=============================================================================
### calculate prevalence by department 
#==============================================================================

##1.. department data

department_prevalence_data <- prevalence_df %>%
  select(Ward, year_month, Department, Patients) 

view(department_prevalence_data)

##2. denomonator-department 

ward_denominators_unique <- department_prevalence_data %>%
  distinct(Ward, year_month, .keep_all = TRUE) %>%  
  group_by(Department) %>%
  summarise(Total_patients = sum(Patients), .groups = "drop")



#3. department prevalence

department_prevalence <- department_prevalence_data %>%
  group_by(Department) %>%
  summarise(On_antibiotics = n(), .groups = "drop") %>%
  left_join(ward_denominators_unique, by = "Department") %>%
  rowwise() %>%
  mutate(
    ci = list(binom.confint(On_antibiotics, Total_patients, method = "wilson")),
    Prevalence_percent = round(On_antibiotics / Total_patients * 100, 1),
    CI_lower = round(ci$lower * 100, 1),
    CI_upper = round(ci$upper * 100, 1)
  ) %>%
  ungroup() %>%
  select(-ci)


print(department_prevalence)

main_prevalence_table <- bind_rows(overall_prevalence, department_prevalence)

print(main_prevalence_table)

write.csv(main_prevalence_table, file = "prevalence.csv", row.names = FALSE)

#### Chi square test for Prevalence

##1. Create variable of patients not on antibiotics and then filter overall prevalence

chi_df <- department_prevalence %>% 
         mutate(
           Not_antibiotics = Total_patients- On_antibiotics
         ) %>% 
  filter( !is.na(Department))

print(chi_df)


# Create matrix from data frame
data_matrix <- chi_df %>%
  select(On_antibiotics, Not_antibiotics) %>%
  as.matrix()

print(data_matrix)
# Set department names as row names
rownames(data_matrix) <- chi_df$Department

print(data_matrix)

## Chi-square test 
chisq.test(data_matrix)


#### Calculate the effect size; Cramers V 

library(vcd)

# Calculate association statistics
assoc_stats <- assocstats(data_matrix)

# Print Cramer's V
print(assoc_stats$cramer)
