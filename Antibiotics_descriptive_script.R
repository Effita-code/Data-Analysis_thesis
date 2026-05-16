rm(list=ls())

###==========================================================================
#Create a file path to the data cleaning code
###==========================================================================
source("Adila_DataCleaning2025May19.R")


###============================================================================
### Antibiotic classes per month
##=============================================================================

## Create a new data frame 

Antibiotic_df <- merged_abx.df %>% 
  transmute( PID, Month = Month, Year = Year, Survey = surveynumber, Ward=Ward, Comorbidity = AnyMorbidity, 
        Multimorbidity =MultMorbidity,WardType = WardType, WardActivity = WardActivity,
    Patients = Patients, Beds = Beds, Department = Department, Gender = Sex, 
    Age_yrs = Age_yrs, Age_groups = Agegroups, Biomarker = BiomarkerBasedTreatment,
    Antibiotic = AntibioticName, Start_date = ABX_StartDate,Dose_unit = DoseUint, 
    Dosage = Dosage, Frequency = DoseFrequency, Route = Route, Indication = Indication, 
       Stopdate_Info = StopdateInfo, Antibiotic_class= AntibioticClass,
    AWaRe_Class = AntioticAwareClass)

### remove ICU, survey 15 and 17

Antibiotic_df <- Antibiotic_df %>%  
  filter(!(Survey %in%(c(15,17))), 
         Department != "ICU") %>%
  mutate(AwaRe = case_when(
    AWaRe_Class == "Access group" ~ "Access",
    AWaRe_Class == "Watch group" ~ "Watch",
    AWaRe_Class == "Reserve group" ~ "Reserve", 
    TRUE ~ NA_character_
  ))
  
  
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

Antibiotic_df$Month <- month_vector[as.character(Antibiotic_df$Survey)]

### create yea_month variable
Antibiotic_df$year_month <- paste(Antibiotic_df$Year, Antibiotic_df$Month, sep = "-")

setdiff(unique(Antibiotic_df$Survey), as.integer(names(month_vector)))

### put the variable year-month in chronological order 

Antibiotic_df <- Antibiotic_df %>%
  mutate(
    date_full = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"),
    year_month = factor(year_month, levels = unique(year_month[order(date_full)])),
    year_month_short = format(date_full, "%b-%Y"),
    month_short = factor(year_month_short, levels = unique(year_month_short[order(date_full)]))
  )

##check if it worked
levels(Antibiotic_df$year_month)
levels(Antibiotic_df$month_short)
##Create new variables to calculate daily prescribed dose and DDD

##Clean observations abit 

Antibiotic_df <- Antibiotic_df %>%
  filter(Antibiotic != "")

Antibiotic_df <- Antibiotic_df %>% 
  filter(!is.na(Dosage) & Frequency != "")

### new Frequency and dosage variables

Antibiotic_df<- Antibiotic_df %>%
  mutate(Frequency_num = case_when(
    Frequency == "BD" ~ 2,
    Frequency == "OD" ~ 1,
    Frequency == "Other" ~ 1,
    Frequency == "QID" ~ 4,
    Frequency == "TDS" ~ 3,
    TRUE ~ NA_integer_  
  )) %>% 
  relocate(Dosage, Frequency_num, .after = last_col())

## Check IU dose units for antibiotics to be converted to grams 

International_units <- Antibiotic_df %>% filter(Dose_unit == "IU")

table(International_units$Antibiotic)

MU_units <-Antibiotic_df %>%
 filter(Dose_unit == "MU") %>%
 distinct(Antibiotic)

table(MU_units$Antibiotic)
#Conversion factors using ; https://mypharmatools.com/othertools/iu
#Benzylpenicillin/Xpen (1 IU = 0.0000006 g)
#Benzathine benzylpenicillin (1 IU = 0.0000015 g)
#Gentamicin (1 IU = 0.0000016 g)
#Vancomycin sulfate (1 IU = 0.0000010 g)
## Ceftriaxone, Amikacin, Amikacin, Flucoxacillin were wrongly dosed... we will use Benzly factor 

##==============================================================================
#troubleshooting dose issues (some doses are outreageously high)
##==============================================================================

#check why Gentamicin has high doses 

Genta <- Antibiotic_df %>% filter(
  Antibiotic == "Gentamycin"
)

table(Genta$Dosage, Genta$Dose_unit)

 
###create a dose column in grams
   
Antibiotic_df <- Antibiotic_df %>% 
  mutate(
    Dose_grams = case_when(
    Dose_unit == "g"   ~ Dosage,  
    Dose_unit == "mg"  ~ Dosage / 1000, 
    Dose_unit == "mcg" ~ Dosage / 1e6,   
    Dose_unit == "IU"  ~ case_when(
      Antibiotic == "Benzathine benzylpenicillin" ~ Dosage * 0.0000015,  
      Antibiotic == "Benzyl penicillin" ~ Dosage * 0.0000006, 
      Antibiotic == "Gentamycin"         ~ Dosage * 0.0000016,
      Antibiotic == "Vancomycin"         ~ Dosage * 0.0000010, 
      Antibiotic == "Erythromycin"       ~ Dosage*  0.000001,
      Antibiotic == "Doxycline"         ~ Dosage * 0.0000011,
      Antibiotic ==  "Xpen"          ~ Dosage * 0.0000006,
      Antibiotic %in% c("Ceftriaxone","Imipenem", "Amikacin", "Flucloxacillin") ~ Dosage * 0.0000006,  
      TRUE ~ NA_real_ 
    ),
    Dose_unit == "MU"  ~ case_when(
      Antibiotic == "Benzyl penicillin" ~ (Dosage * 1e6) * 0.0000006,  
      Antibiotic == "Benzathine penicillin" ~ (Dosage * 1e6) * 0.0000006, 
      TRUE ~ NA_real_   
    )))



### Calculate prescribed daily dose 

Antibiotic_df <- Antibiotic_df %>%
  mutate(Pres_dailydose = round(Frequency_num * Dose_grams, 2))

### Check prescribed daily doses per month

Prescribed_daily_dose <- Antibiotic_df %>% 
  dplyr::select(month_short, Ward, Department,Pres_dailydose)


##Calculate total PDD per month

##check if February is not inflated 


PDD_per_month <- Prescribed_daily_dose%>%
  group_by(month_short) %>%
  summarise(Total_PDD = sum(Pres_dailydose, na.rm = TRUE))

print(PDD_per_month)

### AWARE by percentage

Aware2 <- Antibiotic_df %>%
  count(AwaRe) %>%                               # count each category
  mutate(perc = n / sum(n) * 100) %>%            # calculate percentages
  ggplot(aes(x = AwaRe, y = perc)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", perc)),
            vjust = -0.5, size = 5) +
  labs(       x = "AWARE category",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14)

###AWare  PLOT

Aware3 <- Aware2 <- Antibiotic_df %>%
  count(AwaRe) %>%                              
  mutate(perc = round(n / sum(n) * 100)) %>%          # round percentages
  ggplot(aes(x = perc, y = reorder(AwaRe, perc))) +
  geom_segment(aes(x = 0, xend = perc,
                   y = reorder(AwaRe, perc),
                   yend = reorder(AwaRe, perc),
                   color = AwaRe),
               linewidth = 1.2, 
               show.legend = FALSE) +
  geom_point(aes(color = AwaRe), size = 6,
             show.legend = FALSE)+
  geom_text(aes(label = paste0(perc, "%")),
            hjust = -0.3, size = 5) +
  scale_color_manual(values = c(
    "Access" = "green3",
    "Watch" = "gold",
    "Reserve" = "red3"
  ))#

## Add labels
 
Aware3 <- Aware3 +
  labs(y = "Category",
       x = "Percentage") +
  coord_cartesian(xlim = c(0, max(Antibiotic_df %>% count(AWaRe_Class) %>% 
                                    mutate(perc = round(n/sum(n)*100)) %>% pull(perc)) + 10)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(linewidth = 1.2, colour = "black"),  # bold axis lines
    axis.ticks = element_line(linewidth = 1.2),                   # bold tick marks
    axis.title.x = element_text(face = "bold", size = 18),   # bold x label
    axis.title.y = element_text(face = "bold", size = 18), # bold y label
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 14)
  )
 
Aware3

ggsave("Aware3.png", plot= Aware3, bg = "white", height = 8, width = 14)

##### version 2 of the above

# 1. Create a clean summary dataframe
Aware_Data <- Antibiotic_df %>%
  count(AWaRe_Class) %>%
  mutate(perc = n / sum(n) * 100)

# 2. Calculate the Actual CIs on that dataframe
n_total <- sum(Aware_Data$n)

Aware_Data <- Aware_Data %>%
  mutate(
    p = perc / 100,
    se_p = sqrt((p * (1 - p)) / n_total),
    perc_lb = (p - (1.96 * se_p)) * 100,
    perc_ub = (p + (1.96 * se_p)) * 100
  )

# 3.build  the plot 
aware_perc_plot <- ggplot(Aware_Data, aes(x = perc, y = reorder(AWaRe_Class, perc), color = AWaRe_Class)) +
  geom_segment(aes(x = 0, xend = perc, yend = reorder(AWaRe_Class, perc)), 
               linewidth = 1.5, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = perc_lb, xmax = perc_ub), height = 0.2, color = "black") + 
  geom_point(size = 8, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", perc)), 
           hjust = -0.6, size = 5, fontface = "bold", color = "black") +
  scale_color_manual(values = c(
    "Access" = "green4", 
    "Watch" = "gold", 
    "Reserve" = "red3"
  )) +
  labs(x = "Percentage (%)", y = "AWaRe") +
  theme_minimal(base_size = 14)

# 4. Sync the aesthetics
aware_perc_plot <- aware_perc_plot + 
  theme(
    axis.line = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1.2, color = "black"),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    axis.title = element_text(face = "bold", size =14),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.4)))

aware_perc_plot


ggsave("Aware4.png", plot= aware_perc_plot, bg = "white", height = 8, width = 14)
