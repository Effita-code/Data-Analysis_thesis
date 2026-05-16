#### Script for stratifiying plots

rm(list=ls())

###==========================================================================
#Create a file path to the data cleaning code
###==========================================================================
source("propotion Plots-Descriptive analysis.R")
##=======================================================================
## use patch work to plot department plots 
##=========================================================================
 ##1) Internal medicine plots

print(Medical)

#fix chronolgical order of months 

# Convert the 'year_month' column to a date object and then sort the data frame
Medical_df <- Medical %>%
  arrange( ym(year_month))

Medical_df <- Medical_df %>%
  mutate(
    month_short = format(as.Date(paste0(year_month, "-01"), "%Y-%B-%d"), "%b-%y"), 
    month_short = factor(month_short, levels = month_short), 
    ci = binom.confint(total_on_antibiotics,total_patients, method = "wilson"),
    lower = ci$lower * 100,
    upper = ci$upper * 100) 

##### Medical plot with cleaner aesthetic 

Medical_plot <- ggplot(Medical_df, 
                        aes(x = month_short, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color = "#762a83", size = 1) +
  geom_point(color = "#762a83", size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25,
                color = "#762a83",
                linewidth = 0.7) +
  labs(y = "Proportion (%)", x= NULL, title = "Medical") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, face ="bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title   = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    panel.border = element_blank(),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"), 
    plot.background   = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.bottom = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.top = element_line(color = "black", linewidth = 0.8) 
  )

p1<- Medical_plot

##1) Paediatrics plots with a clean background 

print(Paeds)

# Convert the 'year_month' column to a date object and then sort the data frame
Paeds_df <- Paeds %>%
  arrange( ym(year_month))

Paeds_df <- Paeds_df %>%
  mutate(
    month_short = format(as.Date(paste0(year_month, "-01"), "%Y-%B-%d"), "%b-%y"), 
    month_short = factor(month_short, levels = month_short), 
    ci = binom.confint(total_on_antibiotics,
                       total_patients,
                       method = "wilson"),
    lower = ci$lower * 100,
    upper = ci$upper * 100
  )


##### Paeds plot with cleaner aesthetic 

Paeds_plot <- ggplot(Paeds_df, 
                        aes(x = month_short, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color = "#9660a5", size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25,
                color = "#762a83",
                linewidth = 0.7) +
  geom_point(color = "#9660a5", size = 1.5) +
  labs(y = NULL, x= NULL, title = "Paediatrics") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text = element_text(face = "bold"),
    axis.title   = element_text(face = "bold"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    panel.border = element_blank(),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"), 
    plot.background   = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.bottom = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.top = element_line(color = "black", linewidth = 0.8) 
  )

p2 <- Paeds_plot

##### Surgical with cleaner aesthetic 

# Convert the 'year_month' column to a date object and then sort the data frame
surgical_df <- Surgical%>%
  arrange( ym(year_month))

surgical_df <- surgical_df %>%
  mutate(
    month_short = format(as.Date(paste0(year_month, "-01"), "%Y-%B-%d"), "%b-%y"), 
    month_short = factor(month_short, levels = month_short),
    ci = binom.confint(total_on_antibiotics,
                       total_patients,
                       method = "wilson"),
    lower = ci$lower * 100,
    upper = ci$upper * 100
  )

surgical_plot <- ggplot(surgical_df, 
                      aes(x = month_short, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color = "#912a76", size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25,
                color = "#912a76",
                linewidth = 0.7) +
  geom_point(color = "#912a76", size = 1.5) +
  labs(y ="Proportion (%)", x= "Month", title = "Surgical") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text = element_text(face = "bold"),
    axis.title   = element_text(face = "bold"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    panel.border = element_blank(),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"), 
    plot.background   = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.bottom = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.top = element_line(color = "black", linewidth = 0.8) 
  )

p3<- surgical_plot2

##### Surgical with cleaner aesthetic 

# Convert the 'year_month' column to a date object and then sort the data frame
obg_df <- Obs_gynae%>%
  arrange( ym(year_month))

obg_df <- obg_df %>%
  mutate(
    month_short = format(as.Date(paste0(year_month, "-01"), "%Y-%B-%d"), "%b-%y"), 
    month_short = factor(month_short, levels = month_short), 
    ci = binom.confint(total_on_antibiotics,
                       total_patients,
                       method = "wilson"),
    lower = ci$lower * 100,
    upper = ci$upper * 100
  )



obg_plot <- ggplot(obg_df, 
                         aes(x = month_short, y = proportion_on_antibiotics, group = 1)) +
  geom_line(color = "#5c1f69", size = 1) +
  geom_point(color = "#5c1f69", size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.25,
                color = "#5c1f69",
                linewidth = 0.7) +
  labs(y= NULL, x= "Month", title = "Obstetrics") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text = element_text(face = "bold"),
    axis.title   = element_text(face = "bold"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    panel.border = element_blank(),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"), 
    plot.background   = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.bottom = element_line(color = "black", linewidth  = 0.8),
    axis.line.x.top = element_line(color = "black", linewidth = 0.8) 
  )

p4 <- obg_plot

###### merge with Patchwork


p5 <- (p1+p2)/(p3+p4)

p5

ggsave("patchwork.png", plot=p5, bg="white", height = 8, width = 10)

ggsave("patchwork-CI.png", plot=p5, bg="white", height = 8, width = 10)

