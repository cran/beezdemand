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
data(apt)

## ----check-systematic---------------------------------------------------------
# Check for systematic demand
systematic_check <- check_systematic_demand(apt)
head(systematic_check$results)

# Filter to systematic data only (those that pass all criteria)
systematic_ids <- systematic_check$results |>
  filter(systematic) |>
  dplyr::pull(id)

apt_clean <- apt |>
  filter(as.character(id) %in% systematic_ids)

cat("Systematic participants:", n_distinct(apt_clean$id),
    "of", n_distinct(apt$id), "\n")

## ----fixed-example------------------------------------------------------------
# Fit individual demand curves using the Hursh & Silberberg equation
fit_fixed <- fit_demand_fixed(
  data = apt,
  equation = "hs",
  k = 2
)

# Print summary
print(fit_fixed)

# Get tidy coefficient table
tidy(fit_fixed) |> head()

# Get model-level statistics
glance(fit_fixed)

## ----fixed-plot, fig.cap="Individual demand curves for two example participants."----
# Plot individual curves
plot(fit_fixed, type = "individual", ids = c("19", "51"))

## ----fixed-diagnostics--------------------------------------------------------
# Basic diagnostics
check_demand_model(fit_fixed)

## ----ll4-transform, eval=FALSE------------------------------------------------
# # Transform consumption using LL4
# apt_ll4 <- apt |>
#   dplyr::mutate(y_ll4 = ll4(y))
# 
# # View the transformation
# apt_ll4 |>
#   dplyr::filter(id == 19) |>
#   dplyr::select(id, x, y, y_ll4)

## ----mixed-example, eval=FALSE------------------------------------------------
# # Fit mixed-effects model
# fit_mixed <- fit_demand_mixed(
#   data = apt_ll4,
#   y_var = "y_ll4",
#   x_var = "x",
#   id_var = "id",
#   equation_form = "zben"
# )
# 
# # Print summary
# print(fit_mixed)
# summary(fit_mixed)
# 
# # Basic diagnostics
# check_demand_model(fit_mixed)
# plot_residuals(fit_mixed)$fitted

## ----emmeans-example, eval=FALSE----------------------------------------------
# # For a model with factors (example with ko dataset):
# data(ko)
# 
# # Prepare data with LL4 transformation
# # (Note: the ko dataset already includes a y_ll4 column, but we
# # recreate it here to demonstrate the transformation workflow)
# ko_ll4 <- ko |>
#   dplyr::mutate(y_ll4 = ll4(y))
# 
# fit <- fit_demand_mixed(
#   data = ko_ll4,
#   y_var = "y_ll4",
#   x_var = "x",
#   id_var = "monkey",
#   factors = c("drug", "dose"),
#   equation_form = "zben"
# )
# 
# # Get estimated marginal means for Q0 and alpha across drug levels
# emms <- get_demand_param_emms(fit, factors_in_emm = "drug", include_ev = TRUE)
# 
# # Pairwise comparisons of drug conditions
# comps <- get_demand_comparisons(fit, compare_specs = ~drug, contrast_type = "pairwise")

## ----hurdle-example, eval=FALSE-----------------------------------------------
# # Fit hurdle model with 3 random effects
# fit_hurdle <- fit_demand_hurdle(
#   data = apt,
#   y_var = "y",
#   x_var = "x",
#   id_var = "id",
#   random_effects = c("zeros", "q0", "alpha")
# )
# 
# # Summary
# summary(fit_hurdle)
# 
# # Population demand curve
# plot(fit_hurdle, type = "demand")
# 
# # Probability of zero consumption
# plot(fit_hurdle, type = "probability")
# 
# # Basic diagnostics
# check_demand_model(fit_hurdle)
# plot_residuals(fit_hurdle)$fitted
# plot_qq(fit_hurdle)

## ----hurdle-compare, eval=FALSE-----------------------------------------------
# # Fit full model (3 random effects)
# fit_hurdle3 <- fit_demand_hurdle(
#   data = apt,
#   y_var = "y",
#   x_var = "x",
#   id_var = "id",
#   random_effects = c("zeros", "q0", "alpha")
# )
# 
# # Fit simplified model (2 random effects)
# fit_hurdle2 <- fit_demand_hurdle(
#   data = apt,
#   y_var = "y",
#   x_var = "x",
#   id_var = "id",
#   random_effects = c("zeros", "q0")  # No alpha random effect
# )
# 
# # Compare models
# compare_hurdle_models(fit_hurdle3, fit_hurdle2)
# 
# # Unified model comparison (AIC/BIC + LRT when appropriate)
# compare_models(fit_hurdle3, fit_hurdle2)

## ----k-example, eval=FALSE----------------------------------------------------
# # Different k specifications
# fit_k2 <- fit_demand_fixed(apt, k = 2)          # Fixed at 2
# fit_kind <- fit_demand_fixed(apt, k = "ind")    # Individual
# fit_kfit <- fit_demand_fixed(apt, k = "fit")    # Free parameter
# fit_kshare <- fit_demand_fixed(apt, k = "share") # Shared across participants

