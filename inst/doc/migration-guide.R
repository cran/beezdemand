## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  dev = "png",
  dpi = 144,
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)
library(beezdemand)
library(dplyr)

## ----fitcurves-example, eval = FALSE------------------------------------------
# # Old approach
# results <- FitCurves(
#   dat = apt,
#   equation = "hs",
#   k = 2,
#   xcol = "x",
#   ycol = "y",
#   idcol = "id"
# )
# 
# # results is a data.frame with all parameters
# head(results)

## ----fit-demand-fixed-example-------------------------------------------------
# New approach
fit <- fit_demand_fixed(
  data = apt,
  equation = "hs",
  k = 2,
  x_var = "x",
  y_var = "y",
  id_var = "id"
)

# fit is a structured S3 object
print(fit)

## ----extract-old, eval = FALSE------------------------------------------------
# # FitCurves returns a data frame directly
# results <- FitCurves(apt, "hs", k = 2)
# q0_values <- results$Q0d
# alpha_values <- results$Alpha

## ----extract-new--------------------------------------------------------------
# Use tidy() for a tibble of coefficients
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
coefs <- tidy(fit)
head(coefs)

# Or access the results data frame directly
head(fit$results)

## ----stats-old, eval = FALSE--------------------------------------------------
# # Manual calculation required
# results <- FitCurves(apt, "hs", k = 2)
# n_converged <- sum(!is.na(results$Alpha))
# mean_r2 <- mean(results$R2, na.rm = TRUE)

## ----stats-new----------------------------------------------------------------
# Use glance() for model-level statistics
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)
glance(fit)

# Or use summary() for comprehensive output
summary(fit)

## ----pred-old, eval = FALSE---------------------------------------------------
# # Required detailed = TRUE and manual extraction
# results <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
# # Predictions stored in list element
# predictions <- results$newdats  # List of data frames

## ----pred-new-----------------------------------------------------------------
# Use predict() method
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)

# Predict at new prices
new_prices <- data.frame(x = c(0, 0.5, 1, 2, 5, 10))
preds <- predict(fit, newdata = new_prices)
head(preds)

## ----plot-old, eval = FALSE---------------------------------------------------
# # Used separate PlotCurves() function
# results <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
# PlotCurves(results)

## ----plot-new, fig.width = 6, fig.height = 4----------------------------------
# Use plot() method directly on the fit object
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)

# Plot first 5 subjects
plot(fit, ids = unique(apt$id)[1:5], facet = TRUE)

## ----post-fit-workflow--------------------------------------------------------
fit <- fit_demand_fixed(apt, equation = "hs", k = 2)

augment(fit) |> head()
check_demand_model(fit)
plot_residuals(fit)$fitted

## ----systematicity-wrapper, eval = FALSE--------------------------------------
# sys <- check_systematic_demand(apt)
# head(sys$results)

## ----agg-example--------------------------------------------------------------
# Fit to mean data
fit_mean <- fit_demand_fixed(apt, equation = "hs", k = 2, agg = "Mean")
tidy(fit_mean)

## ----param-space--------------------------------------------------------------
# Fit in log10 space
fit_log <- fit_demand_fixed(
  apt,
  equation = "hs",
  k = 2,
  param_space = "log10"
)

# tidy() can report in either scale
tidy(fit_log, report_space = "log10") |> head()
tidy(fit_log, report_space = "natural") |> head()

## ----suppress-warning, eval = FALSE-------------------------------------------
# # Suppress deprecation warning temporarily
# rlang::with_options(
#   lifecycle_verbosity = "quiet",
#   FitCurves(apt, "hs", k = 2)
# )

## ----session-info-------------------------------------------------------------
sessionInfo()

