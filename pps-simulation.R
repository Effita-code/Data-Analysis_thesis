rm(list = ls())

#============================================================================
source("Antibiotics_descriptive_script.R")
#=============================================================================

simulation_df <- Antibiotic_df %>%
                 select(PID, Month, Year, Survey, Ward,month_short,
                        Patients)
glimpse(simulation_df)

##Make one row per patient, per ward per survey

simulation_df2 <-simulation_df %>%
  distinct(PID, Survey, Ward, .keep_all = TRUE)

##Ward-level dataframe 

simulation_df3 <- simulation_df2 %>%
  group_by(Survey, month_short, Ward) %>%
  summarise(
    n_abx = n(),
    n_total = first(Patients),
    .groups = "drop"
  ) %>%
  mutate(
    prevalence = n_abx / n_total
  )

##========================================================================
## Monte carlo simulation 
##=======================================================================
set.seed(123) ## for reprodicibility 

##======================Simulations  

# ---- Parameters ----
pps_scenarios <- c(1, 2, 3, 4, 6, 12)  # PPS frequencies
n_iterations <- 10000                    # Number of Monte Carlo iterations

months <- unique(pps_data$month_short)   # Available months in dataset

mc_results_freq <- data.frame()

for (n_survey in pps_scenarios) {
  
  mc_prev <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    
    # Randomly pick n_survey months for this iteration
    sampled_months <- sample(months, n_survey, replace = TRUE)
    
    sampled_data <- pps_data %>% filter(month_short %in% sampled_months)
    
    # Hospital-wide prevalence
    mc_prev[i] <- sum(sampled_data$n_abx) / sum(sampled_data$n_total)
  }
  
  # Summarize
  mc_summary <- data.frame(
    n_surveys = n_survey,
    mean_prev = mean(mc_prev),
    ci_lower = quantile(mc_prev, 0.025),
    ci_upper = quantile(mc_prev, 0.975),
    sd_prev = sd(mc_prev),
    ci_width = quantile(mc_prev, 0.975) - quantile(mc_prev, 0.025)
  )
  
  mc_results_freq <- bind_rows(mc_results_freq, mc_summary)
}

# Convert to percentages
mc_results_freq <- mc_results_freq %>%
  mutate(across(c(mean_prev, ci_lower, ci_upper, ci_width), ~ round(.x*100,1)))

print(mc_results_freq)

write.csv(mc_results_freq, file = "montecarlotable.csv", row.names = FALSE)

#### PLOt of diminishing returns 


# Order n_surveys for plotting
mc_results_freq <- mc_results_freq %>% 
  arrange(n_surveys)

# Diminishing returns plot
 
sim_plot <- ggplot(mc_results_freq, aes(x = n_surveys, y = ci_width)) +
  geom_line(color = "#2c7fb8", linewidth = 1.2) +
  geom_point(size = 3, color = "#2c7fb8") +
  geom_text(aes(label = paste0(ci_width, "%")), 
            vjust = -0.8, size = 4) +
   geom_hline(yintercept = 4, 
             linetype = "dashed", 
             color = "darkblue", 
             size = 1) +
  
  annotate("text", x = 6, y = 4.2, 
           label = "Optimal CI width", 
           color = "darkblue", size = 4) +
  theme_minimal() +
  scale_x_continuous(breaks = mc_results_freq$n_surveys) +
  labs(
    x = "Number of surveys",
    y = "95% CI width (%)",
     ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


## aesthetics 

simplot2 <- sim_plot + 
  theme(
  axis.text.x = element_text(# hjust = 1, 
                             face = "bold", size = 12),
  axis.text.y = element_text(face = "bold", size = 12),
  aspect.ratio = 0.5,
  axis.line = element_line(linewidth = 1.2, colour = "black"),
  axis.ticks = element_line(linewidth = 1.2),
  axis.title.x = element_text(face = "bold", size = 14),
  axis.title.y = element_text(face = "bold", size = 14)
)

ggsave("montecarloplot.png", plot = simplot2, bg= "white", height = 8, width = 10)

##########Bootstrap simulation==================================================
## Data preparations 

#1.create a monthly identifier 

bootstrap_df <- simulation_df2 %>%
  mutate(month_id = paste(Year, Month, sep = "-"))

#2. ward level aggregation 

bootstrap_df <- bootstrap_df %>%
group_by(month_id, Ward) %>%
  summarise(
    n_abx = n_distinct(PID),      # count unique patients per ward
    n_total = first(Patients),    # total patients in ward
    .groups = "drop"
  )

#Aggregate hospital data by combining wards

BS_df <-   bootstrap_df %>%
  group_by(month_id) %>%
  summarise(
    patients_on_abx = sum(n_abx),
    total_patients = sum(n_total),
    prevalence = patients_on_abx / total_patients,
    .groups = "drop"
  )

############RUN the Bootstrap
set.seed(123)  # reproducible

n_boot <- 10000  # number of bootstrap replications
survey_scenarios <- 12  

# Create empty list to store results
results_list <- list()
all_bootstrap_distributions <- list()  # optional, for plotting

for (n_surveys in 1:max_surveys) {
  
 # Run bootstrap
  bootstrap_prev <- replicate(n_boot, {
    sampled <- monthly_simulation_df[sample(1:nrow(monthly_simulation_df), n_surveys, replace = TRUE), ]
    sum(sampled$patients_on_abx) / sum(sampled$total_patients)
  })
  
  # Store summary stats
  results_list[[n_surveys]] <- data.frame(
    n_surveys = n_surveys,
    mean = mean(bootstrap_prev),
    sd = sd(bootstrap_prev),
    cv = sd(bootstrap_prev)/mean(bootstrap_prev),
    ci_lower = quantile(bootstrap_prev, 0.025),
    ci_upper = quantile(bootstrap_prev, 0.975),
    ci_width = quantile(bootstrap_prev, 0.975) - quantile(bootstrap_prev, 0.025)
  )
  
  # Store full distribution (optional)
  all_bootstrap_distributions[[n_surveys]] <- data.frame(
    n_surveys = n_surveys,
    prevalence = bootstrap_prev
  )
}

# Combine summary results
results_df <- do.call(rbind, results_list)
results_df 


##bootstrap plot 
cv_at_3 <- results_df$cv[results_df$n_surveys == 3]

#plot

bootplot<- ggplot(results_df, aes(x = n_surveys, y = cv)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(aes(color = n_surveys == 3), size = 3) +
  geom_hline(yintercept = cv_at_3, linetype = "dashed", color = "black") +
  labs(x = "Number of surveys",
       y = "Coefficient of variation (%)") +
  scale_color_manual(values = c("steelblue", "darkblue"), guide = "none") +
  scale_x_continuous(breaks = 1:12) +  
  scale_y_continuous(labels = function(x) x * 100)+ 
  theme_minimal()

## aesthetics 

bootplot2 <- bootplot + 
  theme(
    axis.text.x = element_text( 
      face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    aspect.ratio = 0.5,
    axis.line = element_line(linewidth = 1.2, colour = "black"),
    axis.ticks = element_line(linewidth = 1.2),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

ggsave("bootstrapplot.png", plot = bootplot2, bg= "white", height = 8, width = 10)


#####================Sample distribution

# Function to calculate mean
bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices]  # select the bootstrap sample
  return(mean(sample_data))
}

# Set number of bootstrap resamples
n_boot <- 10000

# Run bootstrap
boot_results <- boot(data = bootstrap_vector, statistic = bootstrap_mean, R = n_boot)

# The bootstrap means are stored in:
bootstrap_means <- boot_results$t


### Plot for the means and std deviation 

bootmeans <- ggplot(data.frame(bootstrap_means), aes(x = bootstrap_means)) +
  geom_density(fill = "skyblue", alpha = 0.4, color = "skyblue", size = 1) +
  labs(x = "Bootstrap means (%)",
       y = "Density") +
  scale_x_continuous(labels = function(x) paste0(round(x*100,1))) +
  theme_minimal(base_size = 14)

boot_means_plot <- bootmeans + 
  theme(
    axis.text.x = element_text( 
      face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    aspect.ratio = 0.5,
    axis.line = element_line(linewidth = 1.2, colour = "black"),
    axis.ticks = element_line(linewidth = 1.2),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

ggsave("bootstrapplot2.png", plot = boot_means_plot, bg= "white", height = 8, width = 10)


