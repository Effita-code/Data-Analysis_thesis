
rm(list = ls())

#=================================================================

##Load packages

gam.pckgs <- c("sf", "mgcv", "cols4all", "cowplot", "statmod", "tweedie", "broom")

gam.pckgs
for(x in 1:length(gam.pckgs))
  library(package = gam.pckgs[x], character.only = TRUE)

## Load data 

gam_df <- read.csv("Antibiotic_ddd_dot.csv")


##============================================================================
## Does the probability of a patient receiving an antibiotic change overtime? 
##===========================================================================

## create df and format it for a poisson model 

pos_df <- gam_df %>% select(Department, Patients, month_short)

#1. Convert the date string month_short to a proper Date object

pos_df$date <- as.Date(paste0("01-", pos_df$month_short), format = "%d-%b-%Y")

#2. Assign a numeric time index for month

pos_df <- pos_df %>%
          arrange(date) %>%
          mutate(time_index = as.numeric(factor(date)))

#3. aggregate data frame for poisson model

poisson_model_df <- pos_df %>%
  group_by(Department, date, time_index) %>%
  summarise(
    abx_count = n(), 
    # take first 'Patients' column value as the denominator
    denominator = sum(unique(Patients)),
    .groups = 'drop'
  )

### Poisson model with ward as an offset

model_poisson <- glm(abx_count ~ time_index + as.factor(Department) + 
                       offset(log(denominator)), 
                     family = poisson(link = "log"), 
                     data = poisson_model_df)

summary(model_poisson)

## Run a quasi-poisson model to fix over dispersion 

# Re-run with Quasi-Poisson to fix the over dispersion
model_dept_quasi <- glm(abx_count ~ time_index + as.factor(Department) + 
                          offset(log(denominator)), 
                        family = quasipoisson(link = "log"), 
                        data = poisson_model_df)

quasi_summary <- summary(model_dept_quasi)

quasi_summary

### format to a cleaner output 

pos_model_table <- tidy(model_dept_quasi, conf.int = TRUE) %>%
  mutate(
    IRR = exp(estimate),
    CI_low = exp(conf.low),
    CI_high = exp(conf.high)
  ) %>%
  select(term, estimate, std.error, IRR, CI_low, CI_high, p.value)

pos_model_table

write.csv(pos_model_table, file= "quasi-poisson.csv")


#==============================================================================
## Gamma regression 
##==============================================================================

#filter out adult patients 

DDD_df <- gam_df %>%
          filter( WardType == "Adult wards") %>% 
          select(Department, Comorbidity, Gender, Age_yrs, month_short,DDD)

## clean zeros 

DDD_df <- DDD_df[DDD_df$DDD > 0, ]


#create time-index 
#1. Convert the date string month_short to a proper Date object

DDD_df$date <- as.Date(paste0("01-", DDD_df$month_short), format = "%d-%b-%Y")

#2. Assign a numeric time index for month

DDD_df <- DDD_df %>%
  arrange(date) %>%
  mutate(time_index = as.numeric(factor(date)))


# Comparing models 

# Create a density plot of DDD values
DDD_density <- ggplot(DDD_df, aes(x = DDD)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef") +
  theme_minimal() +
  labs(
    x = "Defined Daily Dose",
    y = "Density"
  )

### Improved density plot 

DDD_plot <- DDD_density + 
  scale_x_continuous(
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.5),
    expand = c(0, 0)
    ) +
  theme(
  axis.title   = element_text(face = "bold", size = 14),
  axis.text = element_text(face = "bold", size = 14),
  axis.line.y.left = element_line(color = "black", linewidth  = 0.8),
  axis.line.x.bottom = element_line(color = "black", linewidth  = 0.8),
  axis.line.x.top = element_line(color = "black", linewidth = 0.8) 
)

ggsave("DDD-density.png", plot = DDD_plot, bg= "white", height = 8, width = 12)
  
  
# 1. Linear Model ========================================================
# fit a model of DDD and time 


fit_lm <- glm(DDD ~ time_index, 
              family = Gamma(link = "log"),
              data = DDD_df) 

fit_lm %>% summary()


## Clean model outputs

#1. extract co-efficients 

raw_coefs <- coef(fit_lm)
raw_cis <- confint(fit_lm)

#.2 exponentiation 

mean_ratios <- exp(raw_coefs)
mr_cis <- exp(raw_cis)

#3 Combine into a clean summary table
lm_table <- data.frame(
  Predictor = names(raw_coefs),
  Mean_Ratio = round(mean_ratios, 3),
  CI_Lower = round(mr_cis[, 1], 3),
  CI_Upper = round(mr_cis[, 2], 3),
  P_Value = format.pval(summary(fit_lm)$coefficients[, 4], eps = 0.001)
)

write.csv(lm_table, file = "time_lm.csv", row.names = FALSE)


### DDD with time, gender, age, comorbidity. 
## Use Gamma distribution as data is heavily skewed

fit_lm5 <- glm(DDD ~ time_index + Age_yrs + Gender + Comorbidity + Department, 
              family = Gamma(link = "log"),
              data = DDD_df) 

summary(fit_lm5)

### simpler model without Department 
fit_lm6 <- glm(DDD ~ time_index + Age_yrs + Gender + Comorbidity, 
               family = Gamma(link = "log"),
               data = DDD_df) 

summary(fit_lm6)


## model outputs=================================================================

#GLM 5
#Get tidy model output
tidy_fit <- broom::tidy(fit_lm5, conf.int = TRUE, exponentiate = TRUE)

# 2. Select only columns I want
tidy_fit_selected <- tidy_fit[, c("term", "estimate", "std.error", "conf.low", "conf.high", "p.value")]

#3 Rename columns
colnames(tidy_fit_selected) <- c("Predictor", "exp(Estimate)", "Std.Error", "CI_low", "CI_high", "p_value")

write.csv(tidy_fit_selected, file = "model1.csv", row.names = FALSE)

##GLM 6 
#Get tidy model output
tidy_fit6 <- broom::tidy(fit_lm6, conf.int = TRUE, exponentiate = TRUE)

# 2. Select only columns I want
tidy_fit_selected6 <- tidy_fit[, c("term", "estimate", "std.error", "conf.low", "conf.high", "p.value")]

#3 Rename columns
colnames(tidy_fit_selected6) <- c("Predictor", "exp(Estimate)", "Std.Error", "CI_low", "CI_high", "p_value")

write.csv(tidy_fit_selected6, file = "model2.csv", row.names = FALSE)



#### Comparing GLMs====================================================================

# Save to current working directory

png("AIC_Gamma_Models.png", width = 1200, height = 800, res = 150)


gammamodels <- list(
  "Model 1" = fit_lm,
  "Model 2" = fit_lm5,
  "Model 3" = fit_lm6
)

barplot(
  sapply(gammamodels, AIC), 
 # main = "AIC for Candidate Gamma GLMs", 
  col = "lightblue",
  ylab = "AIC",
  ylim = c(0, 3500)# makes x-axis labels vertical for readability
)

# Close the device to save the file
dev.off()

#===============Residual deviance

# Residual deviance
sapply(gammamodels, function(x) deviance(x))

sapply(gammamodels, function(x) c(ResidualDeviance = deviance(x),
                                  df = df.residual(x),
                                  DeviancePerDF = deviance(x)/df.residual(x)))


#==============================================================================
# 2. Quadratic Model (Time + Time^2)
##=============================================================================

#a. plain model with time and  Time^2

first_quad <- glm(DDD ~ time_index + I(time_index^2), 
                family = Gamma(link = "log"),
                data = DDD_df)

summary(first_quad)

#b. (Time + Time^2) and Covariates

fit_quad <- glm(DDD ~ time_index + I(time_index^2) + Age_yrs + Gender + Comorbidity + Department, 
               family = Gamma(link = "log"),
                data = DDD_df)

fit_quad %>% summary()

#Get tidy model output
tidy_quad_fit <- broom::tidy(fit_quad, conf.int = TRUE, exponentiate = TRUE)

# 2. Select only columns I want
tidy_fit_quad_selected <- tidy_quad_fit[, c("term", "estimate", "std.error", "conf.low", "conf.high", "p.value")]

#3 Rename columns
colnames(tidy_fit_quad_selected) <- c("Predictor", "exp(Estimate)", "Std.Error", "CI_low", "CI_high", "p_value")

write.csv(tidy_fit_quad_selected, file = "model2.csv", row.names = FALSE)

#### Model diagnostics

#1.Compare AIC
# Lower AIC indicates a better balance of fit and parsimony

aic_compare <- data.frame(
  Model = c("first_quad", "fit_quad"),
  AIC = c(AIC(first_quad), AIC(fit_quad))
)
print(aic_compare)

anova_result <- anova(first_quad, fit_quad, test = "Chi")
print(anova_result)

##==============================================================================
# 3. Generalized Additive Model
##==============================================================================

#a.  GAM of DDD and time 

first_gam <- gam(DDD ~ s(time_index), 
                       family = Gamma(link = "log"), 
                       data = DDD_df,
                       method = "REML")
summary(first_gam)
#plot model output iN Base R 

#Start the graphics device

tiff("GAM_Plot_Thesis.tiff", width = 8, height = 5, units = 'in', res = 300)

par(mar = c(5, 6, 2, 2))

plot(first_gam,
     shade = TRUE, 
     shade.col = "lightblue",   # confidence band
     col = "blue",             # smooth line
     lwd = 2,  
     xlab = "Month",
     ylab = "",
     cex.lab = 1.0,     # bigger axis labels
     font.lab = 2,
     xaxt = "n",
     yaxt = "n")

# 4. Use title() to place the label exactly where you want it
# Adjust 'line' (4.5) to push it further left or right
title(ylab = "Estimated effect on DDD", line = 4.5, font.lab = 2, cex.lab = 1.0)

axis(1, at = 1:16, labels = 1:16,
     cex.axis = 1.2,    # size of tick labels
     font.axis = 2)

axis(2, 
     cex.axis = 1.2,    # Matches your X-axis size
     font.axis = 2,     # Makes it bold
     las = 1)

dev.off()




### Plot predictions on response scale 

fit_gam <- gam(DDD ~ s(time_index) + s(Age_yrs) + Gender + Comorbidity + Department, 
               family = Gamma(link = "log"), 
               data = DDD_df,
               method = "REML")

fit_gam %>% summary()

plot(fit_gam)

###Forest plot 

AIC(first_gam, fit_gam)

anova_testgam <- anova(first_gam, fit_gam, test = "Chisq")
print(anova_testgam)

# 4. Compare AICs to see which model is objectively "best"

AIC(fit_lm5, fit_quad, fit_gam)

AIC(fit_lm, first_quad, first_gam)

AIC(fit_gam,fit_lm6)
#===============================================================================
## Multivariable regression 
#==============================================================================

adult_df <- gam_df %>%
  filter(WardType != "Paediatric") %>%
  group_by(month_short) %>%
  summarise(
    Total_DDD = sum(DDD, na.rm = TRUE),
    Date_Ref = min(as.Date(date_full)), #date
    .groups = "drop"
  ) %>%
  arrange(Date_Ref) %>%
  mutate(Time_Index = row_number())

####Visualize data 
adult_df %>%
  ggplot(aes(x = Time_Index, y = Total_DDD)) +
  geom_point(alpha = 0.5) +          
  geom_smooth() +
  labs(
       x = "Month",
       y = "Total DDD") +
  theme_minimal()


### Model 1 (Predictor = time, Outcome = DDD)
gam_time_model <-gam(Total_DDD ~ s(Time_Index),
                     data = adult_df,
                     method = "REML")
    

summary(gam_time_model)


##Visualisation 

plot(gam_time_model, 
     select = 1, 
     shade = TRUE, 
     residuals = TRUE, 
     pch = 1, 
     main = "Association of Time and DDD")

##==============================================================================
##Model 2 ()
##==============================================================================
## A little data formatting here 

aware_ddd <- gam_df %>%
  group_by(year_month, AWaRe_Class) %>% 
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

aware_ddd$AWaRe_Class <- factor(aware_ddd$AWaRe_Class)

aware_ddd$time_index <- as.numeric(factor(aware_ddd$year_month, levels = unique(aware_ddd$year_month)))

#### time + aware model 

aware_model <- gam(total_DDD ~ s(time_index, by = AWaRe_Class) + AWaRe_Class,
                 data = aware_ddd, 
                 method = "REML")

summary(aware_model)

##========== Create function for all stratification

stratified_gam <- function(data, strat_var = NULL, method = "REML") {
  
  if (is.null(strat_var)) {
    
      strata_df <- data %>%
      group_by(year_month) %>%
      summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
      ungroup()
    
  } else {
    
      strata_df <- data %>%
      group_by(year_month, !!sym(strat_var)) %>%
      summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
      ungroup()
    
    ## converting the characters to a factor
    strata_df[[strat_var]] <- factor(strata_df[[strat_var]])
  }
  
  # creating time index
  strata_df$time_index <- as.numeric(
    factor(strata_df$year_month, 
           levels = unique(strata_df$year_month))
  )
  
  # formula
  if (is.null(strat_var)) {
    
    # simple time  model
    formula <- total_DDD ~ s(time_index)
    
  } else {
    
    # stratified smooths + group main effect
    formula <- as.formula(
      paste0("total_DDD ~ s(time_index, by = ", strat_var, ") + ", strat_var)
    )
  }
  
  # Fit model
  model <- gam(formula,
               data = strata_df,
               method = "REML")
  
  # create list of outputs
  list(
    data = strata_df,
    model = model,
    summary = summary(model)
  )
}

### Testing the function

#1.  By aWARE class 

aware_gam <- stratified_gam(gam_df, strat_var = "AWaRe_Class")

aware_gam$summary

#2.  By ward 

ward_gam <- stratified_gam(gam_df, strat_var = "Ward")
ward_gam$summary

###===========================================================================
### Quadratic regression
##=============================================================================




