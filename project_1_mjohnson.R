#### OPAN 6602 - Project 1 ####

# Mike Johnson - SAXA

###############################

### Set up ----

# Load Libraries
library(tidyverse)
library(broom)
library(GGally)
library(mvrsquared)
library(ggridges)
library(caret)
library(car)
library(lmtest)
library(forcats)

# Set a random seed for reproducibility
set.seed(206)

# Set data viz theme
theme_set(theme_classic())

# Load data
bike_data = read.csv('Capital Bike Sharing data by hour.csv')


# Convert specified columns to factors
bike_data <- bike_data %>% 
  mutate_at(vars(season, yr, mnth, hr, holiday, weekday, workingday, weathersit), as.factor)

# Check the structure of the data to confirm the conversion
str(bike_data)

  

###############################

### Phase 1: Exploratory Data Analysis

## Create train/test split ----

# Divide 30% of data to test set

test_indices = createDataPartition(1:nrow(bike_data),
                                   times = 1,
                                   p = 0.3)

# Create training set

bike_train = bike_data[-test_indices[[1]], ]

# Create test set

bike_test = bike_data[test_indices[[1]], ]

## Data Exploration ----

bikes_eda = 
  bike_train %>% 
  select(
    !instant &
    !dteday &
    !casual & # Exclude since used to calculate dependent variable, 'cnt'.
    !registered # Exclude since used to calculate dependent variable, 'cnt'.
  )

# Create pairs plot.
# There are challenges producing the viz. 
# Saved an image for future use just in case this fails to re-run.

#bikes_eda %>% ggpairs()

# Summary Statistics

summary(bikes_eda)

## Verify the distribution of the dependent variable

bikes_eda %>% 
  ggplot(aes(cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 
  
  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "Unadjusted")

# cnt is right skewed. Apply transformations to achieve normal distribution.

## Dependent variable transformations

bikes_eda =
  bikes_eda %>% 
  mutate(log_cnt = log(cnt),
         sqrt_cnt = sqrt(cnt),
         cubert_cnt = cnt^(1/3),
         recip_cnt = 1/cnt)

# Visualize log_cnt
# Data becomes left skewed. Not a good candidate.

bikes_eda %>% 
  ggplot(aes(log_cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 
  
  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "Log Transformed")

# Visualize sqrt_cnt
# Data becomes bionomial. Not a good candidate

bikes_eda %>% 
  ggplot(aes(sqrt_cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 
  
  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "Square Root Transformed")

# Visualize cubert_cnt
# Data becomes slightly skewed left. Best candidate out of the transformations

bikes_eda %>% 
  ggplot(aes(cubert_cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 
  
  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "Cube Root Transformed")

# Visualize recip_cnt
# Still right skewed. Not a good candiate.

bikes_eda %>% 
  ggplot(aes(recip_cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 
  
  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "Reciprocal Transformed")


# Identify any correlations between the independent and dependent variable.

# Select only numeric columns
numeric_columns = sapply(bikes_eda, is.numeric)
bikes_eda_numeric = bikes_eda[, numeric_columns]


cor_matrix = round(cor(bikes_eda_numeric),2)

cor_df = 
  cor_matrix %>% 
  as.data.frame() %>% # Convert matrix to data frame
  rownames_to_column(var = "Var") %>% 
  select(
    Var,
    cnt,
    log_cnt,
    sqrt_cnt,
    cubert_cnt,
    recip_cnt
  ) %>% 
  filter(!grepl("cnt",Var)) %>%  # Remove dependent variables
  arrange(desc(cnt))

# Check for potential multicollinearity.
# Identified variables: (season & mnth) & (temp & atemp)

multicol_vars = 
  cor_matrix %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Var1") %>% 
  select(
    -cnt,
    -log_cnt,
    -sqrt_cnt,
    -cubert_cnt,
    -recip_cnt
  ) %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%  # Unpivot data
  filter(
    Var1 != Var2
    & !grepl("cnt",Var1)
    & (r > 0.5 | r < -0.5)
  )

## Explore variables

# season vs cnt

bikes_eda %>% 
  group_by(season) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = season, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  
  # Labels
  labs(x = "Season",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Hour")

# weekday vs cnt

bikes_eda %>% 
  group_by(weekday) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = weekday, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  
  # Labels
  labs(x = "Season",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Hour")

# hr vs cnt

bikes_eda %>% 
  group_by(hr) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = hr, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  
  # Labels
  labs(x = "Hour",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Hour")

# hr facet

bikes_eda %>% 
  group_by(hr, workingday) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = hr, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  facet_wrap(~ workingday)
  
  # Labels
  labs(x = "Hour",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Hour")

# Add peak_hr variable to account for rentals between 7-20 hrs

bikes_eda = 
  bikes_eda %>% 
  mutate(peak_hr = ifelse(hr %in% 7:20, TRUE, FALSE))

cor(bikes_eda$peak_hr, bikes_eda$cnt)

# temp vs cnt
# As temperature increases, total rentals increase.

bikes_eda %>% 
  group_by(temp) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = temp, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  
  # Labels
  labs(x = "Hourly Temperature in Celcius (Normalized)",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Temperature")

# Add peak_temp to account for leveling of rentals at 0.75

bikes_eda = 
  bikes_eda %>% 
  mutate(peak_temp = ifelse(temp > 0.75, TRUE, FALSE))

cor(bikes_eda$peak_temp, bikes_eda$cnt)

# hum vs cnt
# Increases in humidity result in a decrease in total rentals.

bikes_eda %>% 
  mutate(denorm_hum = hum * 100) %>%  # Denormalize humidity
  group_by(denorm_hum) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = denorm_hum, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  geom_smooth(method = "lm", se = FALSE) +
  
  # Labels
  labs(x = "Relative Humidity",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Relative Humidity")

bikes_eda %>% 
  #mutate(denorm_hum = hum * 100) %>%  # Denormalize humidity
  group_by(hum) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = hum, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  
  # Labels
  labs(x = "Relative Humidity",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Relative Humidity")

# Add peak_temp to account for leveling of rentals at 0.75

bikes_eda = 
  bikes_eda %>% 
  mutate(peak_hum = ifelse(temp > 0.19, TRUE, FALSE))

cor(bikes_eda$peak_hum, bikes_eda$cnt)

# windspeed vs cnt

bikes_eda %>% 
  mutate(denorm_windspeed = windspeed * 67) %>%  # Denormalize humidity
  group_by(denorm_windspeed, weathersit) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>% 
  ggplot(aes(x = denorm_windspeed, y = avg_cnt)) + # Dimensions
  geom_col(fill = "darkcyan") + # Viz
  facet_wrap(~ weathersit) +
  
  # Labels
  labs(x = "Windspeed",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Windspeed")

# Apply identified transformations used in EDA to training set to use in model

bike_train = 
  bike_train %>% 
  mutate(cubert_cnt = cnt^(1/3),
         peak_hr = ifelse(hr %in% 7:20, TRUE, FALSE),
         peak_temp = ifelse(temp > 0.75, TRUE, FALSE),
         peak_hum = ifelse(temp > 0.19, TRUE, FALSE))

### 3. Regression Model ----

## Everything Model (Unadjusted cnt)
# Excluding atemp due to multicollinearity with temp

f1 = 
  lm(
    cnt ~
      season +
      mnth +
      yr +
      hr +
      holiday +
      weekday +
      workingday +
      weathersit +
      temp +
      hum +
      windspeed +
      peak_hr+
      peak_temp+
      peak_hum,
    data = bike_train
  )

# Summary of Everything Model

summary(f1)

plot(f1)

# Evidence of Homoscedasticity
# Evidence of non-linear relationship
# Evidence of kurtosis

## Backward Elimination

f_back =
  step(object = f1,
       direction = "back"
       )

# No improvement from Everything Model
summary(f_back)

plot(f_back)

## Stepwise Selection

f_step = step(object = f1,
              direction = "both")

# No improvement from Everything Model

summary(f_step)

plot(f_step)

## Forward Selection

f_forward = 
  step(
    object = lm(cnt ~ 1,
                data = bike_train %>% select(-instant,
                                             -dteday,
                                             -casual,
                                             -registered,
                                             -mnth,
                                             -atemp,
                                             -cubert_cnt)),
    direction = "forward",
    scope = cnt ~
      season +
      yr + 
      hr +
      holiday +
      weekday +
      workingday +
      weathersit +
      temp +
      hum +
      windspeed +
      peak_hr +
      peak_temp+
      peak_hum
  )

# No improvement from everything model.

summary(f_forward)

plot(f_forward)

## Cube Root Model (cubert_cnt)
# Excluding mnth due to multicollinearity with season
# Excluding atemp due to multicollinearity with temp

f2 = 
  lm(
    cubert_cnt ~
      season +
      mnth +
      yr +
      hr +
      holiday +
      weekday +
      workingday +
      weathersit +
      temp +
      hum +
      windspeed +
      peak_hr +
      peak_temp+
      peak_hum
    ,
    data = bike_train
  )

# Summary of Cube Root Model

summary(f2)

plot(f2)
# Residuals vs Fitted looks good
# Q-Q Plot suggests non-normal residuals
# Scale-location looks good

vif(f2)

## Stepwise Selection

f2_step =
  step(object = f2,
       direction = "both")

summary(f2_step)



### 4. Assumption Validation and Influential Observation Detection ----

summary(f2_step)

plot(f2_step)
# Residuals vs Fitted looks good
# Q-Q Plots show non-linear relationships
# Scale-Location has a minor curve indicating non-linear relationship

vif(f2_step)

# Additional model tests
bptest(f2_step)
dwtest(f2_step)

### 5. Model Revision ----

# Address autocorrelation

bike_train = 
  bike_train %>% 
  arrange(dteday) %>% 
  mutate(cubert_cnt_lag = lag(cubert_cnt, 1))

# F4 Model with Lag
# remove mnth due to high p-values
# remove weekday due to high p-values
# remove weathersit due to high p-values in 4
# Remove Holiday due to high leverage
# Remove Humidity due to high p-value

f4 = 
  lm(
    cubert_cnt ~
      season +
      #mnth + 
      yr +
      hr +
      holiday +
      #weekday +
      #weathersit +
      temp +
      #hum +
      windspeed +
      #peak_hr +
      peak_temp+
      #peak_hum +
      cubert_cnt_lag
    ,
    data = bike_train
  )

summary(f4)

plot(f4)

# Additional model tests
bptest(f4)
dwtest(f4)
vif(f4)

## F5 Model

f5 = 
  lm(
    cubert_cnt ~
      season +
      yr +
      hr +
      temp +
      peak_temp +
      cubert_cnt_lag
    ,
    data = bike_train
  )

summary(f5)

plot(f5)

# Additional model tests
bptest(f5)
dwtest(f5)

vif(f5)

### 6. Model Evaluation ----

# Get fold indices

k_fold_indices = 
  createFolds(
    1:nrow(bike_train),
    k = 10
  )

# Declare a function to fit the model

model_function <- function(dat) {
  lm(
    cubert_cnt ~
      season +
      yr +
      hr +
      temp +
      peak_temp +
      cubert_cnt_lag
    ,
    data = dat
  )

}

# Declare a function to get out-of-sample evaluations

evaluation_function = function(model, new_data) {
  
  preds = predict(model, new_data)
  
  oos_r2 = cor(new_data$cubert_cnt, preds) ^ 2
  
  oos_rmse = ((new_data$cubert_cnt - preds) ^ 2) %>% 
    mean() %>% 
    sqrt()
  
  tibble(r2 = oos_r2,
         rmse = oos_rmse)
}

# Loop over folds and apply

k_fold_results =
  k_fold_indices %>% 
  map(function(fold) {
    train = bike_train[-fold, ]
    test = bike_train[fold, ]
    f = model_function(train)
    coefs = tidy(f)
    evals = evaluation_function(model = f, new_data = test)
    
    list(
      evals = evals,
      coefs = coefs
    )
  })

cv_evals = 
  k_fold_results %>% 
  map(function(x) x$evals) %>% 
  bind_rows()

cv_coefs = 
  k_fold_results %>% 
  map(function(x) x$coefs)

for (j in seq_along(cv_coefs)) {
  cv_coefs[[j]]$fold = j
}

cv_coefs = bind_rows(cv_coefs)

# Plot results

cv_coefs %>% 
  filter(
    term != "(Intercept)"
  ) %>% 
  ggplot(aes(x = estimate, y = term, fill = term)) +
  geom_density_ridges() + 
  theme_ridges() + 
  theme(legend.position = "none")

### Step 7: Predictions and Conclusions ----

# Apply transformations to test set

bike_test = 
  bike_test %>% 
  mutate(cubert_cnt = cnt^(1/3),
         peak_hr = ifelse(hr %in% 7:20, TRUE, FALSE),
         cubert_cnt_lag = lag(cubert_cnt, 1),
         peak_temp = ifelse(temp > 0.75, TRUE, FALSE))

test_predictions = 
  predict(f5, newdata = bike_test)

test_results = 
  tibble(
    r2 = calc_rsquared(y = bike_test$cubert_cnt[! is.na(test_predictions)],
                       yhat = test_predictions[! is.na(test_predictions)]),
    rmse = ((bike_test$cubert_cnt - test_predictions) ^ 2) %>% 
              mean(na.rm = TRUE) %>% 
              sqrt()
  )

tibble(
  test_predictions,
  actual = bike_test$cubert_cnt) %>% 
  ggplot(aes(x = actual, y = test_predictions)) +
  geom_point()

### Univariate Summaries ----

# Summary Statistics

summary_stats_function = function(x) {
  data.frame(
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Sd = sd(x, na.rm = TRUE)
  )
}

summary_df =
  bike_data %>% 
  select(
    -instant,
    -dteday,
    -casual,
    -registered
  ) %>% 
  summarise(across(where(is.numeric), summary_stats_function)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable") %>% 
  unnest_wider(value)

bike_data %>% 
  select(
    -instant,
    -dteday,
    -casual,
    -registered
  ) %>% 
  summary()

# Pair Plots

bike_data %>% 
  select(    
    -instant,
    -dteday,
    -casual,
    -registered) %>% 
  ggpairs()

# cnt distribution

bike_data %>% 
  ggplot(aes(cnt)) + # Dimensions
  geom_histogram(fill = "darkcyan") + # Viz 

  # Labels
  labs(x = "Total Rentals",
       y = "Count",
       title = "Total Rentals Distribution",
       subtitle = "")

summary(bike_data$cnt)

# Identify any correlations between the independent and dependent variable.

bike_data =
  bike_data %>% 
  mutate(log_cnt = log(cnt),
         sqrt_cnt = sqrt(cnt),
         cubert_cnt = cnt^(1/3),
         recip_cnt = 1/cnt,
         peak_hr = ifelse(hr %in% 7:20, TRUE, FALSE),
         cubert_cnt_lag = lag(cubert_cnt, 1),
         peak_temp = ifelse(temp > 0.75, TRUE, FALSE))

# Select only numeric columns
numeric_columns = sapply(bike_data, is.numeric)
bikes_eda_numeric = bike_data[, numeric_columns]

bikes_eda_numeric = 
  bikes_eda_numeric %>% 
  select(-casual,
         -registered)


cor_matrix_all = round(cor(bikes_eda_numeric),2)

cor_df_all = 
  cor_matrix_all %>% 
  as.data.frame() %>% # Convert matrix to data frame
  rownames_to_column(var = "Var") %>% 
  select(
    Var,
    cnt,
    log_cnt,
    sqrt_cnt,
    cubert_cnt,
    recip_cnt
  ) %>% 
  filter(!grepl("cnt",Var)) %>%  # Remove dependent variables
  arrange(desc(cnt))

# Check for potential multicollinearity.
# Identified variables: (season & mnth) & (temp & atemp)

multicol_vars_all = 
  cor_matrix_all %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Var1") %>% 
  select(
    -cnt,
    -log_cnt,
    -sqrt_cnt,
    -cubert_cnt,
    -recip_cnt
  ) %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%  # Unpivot data
  filter(
    Var1 != Var2
    & !grepl("cnt",Var1)
    & (r > 0.5 | r < -0.5)
  )

# temp vs cnt
# As temperature increases, total rentals increase.

bike_data %>% 
  group_by(temp) %>% 
  summarise(avg_cnt = mean(cnt, na.rm = TRUE)) %>%
  mutate(peak_temp = ifelse(temp > 0.75, "Above 0.75", "0.75 and Below")) %>% # Create a descriptive color column
  ggplot(aes(x = temp, y = avg_cnt, fill = peak_temp)) + # Dimensions
  geom_col() + # Viz
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  
  # Labels
  labs(x = "Hourly Temperature in Celcius (Normalized)",
       y = "Average Total Rentals",
       title = "Average Total Rentals by Temperature") +
  scale_fill_manual(values = c("0.75 and Below" = "darkcyan", "Above 0.75" = "grey"),
                    name = "Peak Temperature")

# Extract model stats using tidy()

model_tidy = tidy(f5)

# Visualize marginal effects

model_tidy %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_col(fill = "darkcyan") +
  geom_text(aes(label = round(estimate, 2)),  # Add labels with rounded estimates
            hjust = -0.2,  # Adjust horizontal position of the labels
            color = "black") +  # Set the color of the labels
  labs(x = "Estimate", y = "Variable", title = "Model Coefficients")

## Summary of final model

summary(f5) # Model results

bptest(f5) # Breusch-Pagan Test
dwtest(f5) # Durbin-Watson Test
vif(f5) #Generalized Variance Inflation Factor

plot(f5)

model_tidy

