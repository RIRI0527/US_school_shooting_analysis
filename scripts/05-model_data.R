#### Preamble ####
# Purpose:Fit the Logictics Regression Models
# Author: Ruizi Liu
# Date: 3 December 2024
# Contact: ruizi.liu@mail.utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: Run `02-download_data.R` and `03-clean_data.R` first
# Any other information needed? Make sure you are in the `school_shooting_analysis` rproj


#### Workspace setup ####
library(brms)
library(dplyr)

#### Read data ####
data <- read.csv(here::here("data/02-analysis_data/analysis_data.csv"))

# Prepare the data
data <- data %>%
  mutate(
    school_shooting = as.factor(school_shooting),
    state = as.factor(state),
    school_type = as.factor(school_type)
  )

# Fit the models
model_bayes <- brm(
  formula = school_shooting ~ year + school_type + top_1_races + ulocale_desc,
  family = bernoulli(link = "logit"),
  data = data,
  prior = c(
    prior(normal(0, 10), class = "b"),          # Prior for coefficients
    prior(cauchy(0, 2), class = "Intercept")    # Prior for intercept
  ),
  chains = 4,    # Number of Markov Chains
  iter = 2000,   # Number of iterations per chain
  warmup = 500   # Number of warm-up samples
)


# Bayesian logistic regression
model_bayes <- brm(
  formula = school_shooting ~ year + school_type + state,
  family = bernoulli(link = "logit"),
  data = analysis_data,
  prior = c(
    prior(normal(0, 10), class = "b"),          # Priors for coefficients
    prior(cauchy(0, 2), class = "Intercept")    # Prior for intercept
  ),
  chains = 4,    # Number of Markov Chains
  iter = 2000,   # Number of iterations per chain
  warmup = 500   # Number of warm-up samples
)

# Summarize the model
summary(model_bayes)

# Plot posterior distributions
plot(model_bayes)

# Bayesian logistic regression
model_bayes_state <- brm(
  formula = school_shooting ~ year + school_type + state,
  family = bernoulli(link = "logit"),
  data = data,
  prior = c(
    prior(normal(0, 10), class = "b"),          # Priors for coefficients
    prior(cauchy(0, 2), class = "Intercept")    # Prior for intercept
  ),
  chains = 4,    # Number of Markov Chains
  iter = 2000,   # Number of iterations per chain
  warmup = 500   # Number of warm-up samples
)

# Summarize the model
summary(model_bayes_state)

# Plot posterior distributions
plot(model_bayes_state)

# Model comparison (if you have other Bayesian models)
# library(loo)
# loo_compare(model_bayes, another_bayesian_model)

# Save the model as an .rds file
saveRDS(model_bayes, file = here::here("models/model_bayes.rds"))
saveRDS(model_bayes_state, file = here::here("models/model_bayes_state.rds"))
# Print confirmation message
cat("The logistic regression model has been saved as 'logistic_regression_model.rds'.")

