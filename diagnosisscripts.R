rm(list = ls())

## script for infection patterns 

#==============================================================================
### source
#==============================================================================

source("Adila_DataCleaning2025May19.R")

####======load data 

diagnosis <- read.csv("Diagnosisdf.csv")

### remove survey 15 and 17
Diagnosis_df <- diagnosis %>%  
  filter(!(surveynumber %in% c(15,17)))
    
###create month variable, 

month_vector <- c(
  "1" = "January", "2" = "February", "3" = "March", "4" = "April",
  "5" = "May", "6" = "June", "7" = "July", "8" = "August",
  "9" = "September", "10" = "October", "11" = "November", "12" = "December",
  "13" = "January", "14" = "February", "16" = "March", "18" = "April")


Diagnosis_df <- Diagnosis_df %>%
      mutate(
        month = month_vector[as.character(surveynumber)],
        year = case_when(
          surveynumber %in% 1:12 ~ 2024,
          surveynumber %in% c(13, 14, 16, 18) ~ 2025,
          TRUE ~ NA_real_
        )
      )
  
#### fix a redundancy/ survey 1 was showing in February


## create a vector 

month_fixed <- c(
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

Diagnosis_df$month <- month_fixed[as.character(Diagnosis_df$surveynumber)]

### create yea_month variable

Diagnosis_df$year_month <- paste(Diagnosis_df$year, Diagnosis_df$month, sep = "-")

setdiff(unique(Diagnosis_df$surveynumber), as.integer(names(month)))


### put the variable year-month in chronological order 

Diagnosis_df <- Diagnosis_df %>%
  mutate(
    date_full = as.Date(paste0(year_month, "-01"), format = "%Y-%B-%d"),
    year_month = factor(year_month, levels = unique(year_month[order(date_full)])),
    year_month_short = format(date_full, "%b-%Y"),
    month_short = factor(year_month_short, levels = unique(year_month_short[order(date_full)]))
  )


###Plot infections 

##filter out NCDs 

Diagnosis_data_without_NDC <- Diagnosis_df %>%  filter(
  !Infection_category %in% c(
    "Non Communicable Disease", 
    "HIV",
    "Malaria"
))

#### try line plot
### create a data frame for this 

#Diag_data_withnumberedmonths <- Diagnosis_data_without_NDC %>%
 # arrange(date_full) %>% 
  #mutate(month_sequence_num = as.numeric(factor(date_full, levels = unique(date_full), ordered = TRUE))) %>%
  #mutate(month_plot_label = factor(as.character(month_sequence_num),
                                #   levels = as.character(unique(month_sequence_num)),
                                #   ordered = TRUE))

#### try line plot

Diag_df <- Diagnosis_data_without_NDC %>% 
  mutate(
    Infection_short = case_when(
      Infection_category == "Central Nervous System Infection" ~ "CNS", 
      Infection_category == "Gastrointestinal Tract Infection" ~ "GIT",
      Infection_category == "Genitourinary Tract Infection" ~ "UTI",
      Infection_category == "Obstetric" ~ "Obstetric", 
      Infection_category == "Respiratory Tract Infection" ~ "RTI", 
      Infection_category == "Sepsis" ~ "Sepsis", 
      Infection_category == "Skin, soft tissue and bone" ~ "SSTI"))


##=================================================================================
## Infection plot with proportions
##=================================================================================

Diag_df_count <- Diag_df%>%
  group_by(month_short, Infection_short) %>%
  summarise(freq = n(), .groups = "drop") 


diag_count_plot <- ggplot(
  Diag_df_count, aes(x = month_short, y = freq, group = Infection_short, color = Infection_short)) +
  geom_line( size= 1) +
  geom_point() +
  ylim(0, 80) +
  theme_minimal()+ 
  theme( axis.text = element_text(face = "bold", size = 12),
         axis.title = element_text(face = "bold", size = 14))+
  facet_wrap(~ Infection_short) +
  labs(y = "Frequency", x = "Month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        strip.text = element_text(face = "bold", size = 14),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "none")

ggsave("diagnosis-freq.png", plot = diag_count_plot, bg = "white", height = 8, width = 12)


####===========================================================================
## New infection plots
##==============================================================================
#1.create new variable

New_diag_df <- Diag_df %>% 
  mutate(
    Infection_cat_short = case_when(
    Infection_category == "Central Nervous System Infection" ~ "CNS", 
    Infection_category == "Gastrointestinal Tract Infection" ~ "GIT",
    Infection_category == "Genitourinary Tract Infection" ~ "G-UTI",
    Infection_category == "Obstetric" ~ "Obstetric", 
    Infection_category == "Respiratory Tract Infection" ~ "RTI", 
    Infection_category == "Sepsis" ~ "Sepsis", 
    Infection_category == "Skin, soft tissue and bone" ~ "SSTI"), 
    Agegroups=cut(age, right=FALSE,
                  breaks = c(0, 5,18,50, 100),
                  labels=c("<5", "5-17", "18-49", "≥50"))
    )
  

#2) TRY A BOX PLOT 

Boxplot <- ggplot(New_diag_df, aes(x = Infection_short , y = age, fill =  Infection_short)) +
  geom_boxplot( width = 0.5) +
  labs(
    x = "Infection",
    y = "Age"
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), 
    axis.text.y = element_text(face = "bold"), 
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    text = element_text(family = "Arial", size = 16)
  ) + 
  guides(fill = "none")

Boxplot

###diagnosis  plot
ggsave("infection_box_plot.png", plot = Boxplot, bg = "white", width = 16, height = 10)

#3) Basic Bar plot 

Infection_barplot <- ggplot(
  New_diag_df, aes ( x= fct_reorder(Infection_short, Infection_short, .desc = TRUE, .fun = length), fill = Infection_short)) +
  geom_bar( width = 0.5) +
  ylab("Number of diagnoses") +
  xlab("Infection category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16)
                                    ) +
                     guides(fill = "none")
  

Infection_barplot 

###diagnosis  plot
ggsave("Infection_barplot2.png", plot = Infection_barplot, bg = "white", width = 16, height = 10)

#4) Infections by age and gender


infection_histogram <- ggplot(New_diag_df, aes(x = age)) +
  geom_histogram(
    binwidth = 5, 
    fill = "skyblue",
    color = "black",
  ) + 
  facet_wrap(~ Infection_short, scales = "free_y", ncol = 3) +
  labs(
    x = "Age",
  y = "Number of diagnoses"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        strip.text = element_text(face = "bold", size = 14),
axis.text.y = element_text(face = "bold", size = 14),
axis.text.x = element_text(face = "bold", size = 14),
axis.title.x = element_text(face = "bold", size = 14),
axis.title.y = element_text(face = "bold", size = 14))

###diagnosis  plot
ggsave("Infection_histogram .png", plot =infection_histogram, bg = "white", width = 16, height = 10)



###=============================================================================================================

##==============================================================================================================






