## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",   # R console style comments
  dev = "png",
  dpi = 144,
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,  # Suppress warnings for cleaner output
  message = FALSE,  # Suppress messages for cleaner output
  cache = TRUE,     # Cache for faster rebuilds
  cache.path = "mixed-demand_cache/",
  autodep = TRUE
)

library(beezdemand)
library(dplyr)
library(ggplot2)

data("apt", package = "beezdemand", envir = environment())
data("ko", package = "beezdemand", envir = environment())

cache_key_object <- function(x) {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(x, tmp)
  on.exit(unlink(tmp), add = TRUE)
  unname(tools::md5sum(tmp))
}

# Invalidate cache when the package data changes.
knitr::opts_chunk$set(
  cache.extra = list(
    beezdemand_version = as.character(utils::packageVersion("beezdemand")),
    apt = cache_key_object(apt),
    ko = cache_key_object(ko)
  )
)

## -----------------------------------------------------------------------------
quick_nlme_control <- nlme::nlmeControl(
  msMaxIter = 100,
  niterEM = 20,
  maxIter = 100, # Low iterations for speed
  pnlsTol = 0.1,
  tolerance = 1e-4, # Looser tolerance
  opt = "nlminb",
  msVerbose = FALSE
)

## ----apt_zben_fit-------------------------------------------------------------
apt_ll4 <- apt |>
  mutate(y_ll4 = ll4(y))

fit_apt_zben <- fit_demand_mixed(
  data = apt_ll4,
  y_var = "y_ll4",
  x_var = "x",
  id_var = "id",
  equation_form = "zben",
  nlme_control = quick_nlme_control,
  start_value_method = "heuristic"
)
print(fit_apt_zben)

## ----apt_zben_plot, cache = FALSE---------------------------------------------
plot(
  fit_apt_zben,
  inv_fun = ll4_inv,
  y_trans = "pseudo_log",
  x_trans = "pseudo_log",
  show_pred_lines = c("population", "individual")
) +
  facet_wrap(
    ~id
  )

## ----apt_simplified_fit-------------------------------------------------------
fit_apt_simplified <- fit_demand_mixed(
  data = apt_ll4,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  equation_form = "simplified",
  nlme_control = quick_nlme_control,
  start_value_method = "heuristic"
)
print(fit_apt_simplified)

## ----apt_simplified_plot, cache = FALSE---------------------------------------
plot(
  fit_apt_simplified,
  x_trans = "pseudo_log",
  show_pred_lines = c("population", "individual")
) +
  facet_wrap(
    ~id
  )

## ----apt_exponentiated_fit----------------------------------------------------
fit_apt_exponentiated <- fit_demand_mixed(
  data = apt,
  y_var = "y",
  x_var = "x",
  id_var = "id",
  equation_form = "exponentiated",
  k = NULL,
  nlme_control = quick_nlme_control,
  start_value_method = "heuristic"
)
print(fit_apt_exponentiated)

## ----inspect_fit--------------------------------------------------------------
glance(fit_apt_zben)
tidy(fit_apt_zben) |> head()
augment(fit_apt_zben) |> head()

## ----diagnostics--------------------------------------------------------------
check_demand_model(fit_apt_zben)
plot_residuals(fit_apt_zben)$fitted

## -----------------------------------------------------------------------------
# Make sure a similar 'fit_no_factors' was created successfully in your environment
# For the vignette, let's create one that is more likely to converge quickly
# by using only Alfentanil data, which is less complex than the full dataset.
ko_alf <- ko[ko$drug == "Alfentanil", ]

fit_no_factors_vignette <- fit_demand_mixed(
  data = ko_alf,
  y_var = "y_ll4",
  x_var = "x",
  id_var = "monkey",
  equation_form = "zben",
  nlme_control = quick_nlme_control, # Use quicker control for vignette
  start_value_method = "heuristic" # Heuristic is faster for simple model
)
print(fit_no_factors_vignette)

## -----------------------------------------------------------------------------
fit_one_factor_dose <- fit_demand_mixed(
  data = ko_alf,
  y_var = "y_ll4",
  x_var = "x",
  id_var = "monkey",
  factors = "dose",
  equation_form = "zben",
  nlme_control = quick_nlme_control,
  start_value_method = "heuristic"
)
print(fit_one_factor_dose)

## -----------------------------------------------------------------------------
# Summary
summary(fit_one_factor_dose)

# Fixed effects
coef(fit_one_factor_dose, type = "fixed")

# Random effects (deviations from fixed)
head(coef(fit_one_factor_dose, type = "random"))

# Subject-specific coefficients (fixed + random)
head(coef(fit_one_factor_dose, type = "combined"))

# Access nlme fixef/ranef directly
nlme::fixef(fit_one_factor_dose)
utils::head(nlme::ranef(fit_one_factor_dose))

# Start values that were used for the NLME fit
fit_one_factor_dose$start_values_used

## -----------------------------------------------------------------------------
# Population-level predictions (log10 scale for 'zben')
preds_pop_log <- predict(fit_one_factor_dose, level = 0)
head(preds_pop_log)

# Population-level predictions (natural scale, back-transformed)
preds_pop_natural <- predict(
  fit_one_factor_dose,
  level = 0,
  inv_fun = ll4_inv
)
head(preds_pop_natural)

# Group-level predictions for first few data points
sample_newdata <- fit_one_factor_dose$data[1:5, ]
preds_group_log <- predict(fit_one_factor_dose, newdata = sample_newdata, level = 1)
preds_group_log

## ----plot_one_factor, cache = FALSE, fig.cap="Demand curves for Alfentanil by dose (population level). y-axis on natural scale."----
plot(
  fit_one_factor_dose,
  inv_fun = ll4_inv,
  color_by = "dose",
  shape_by = "dose",
  observed_point_alpha = 0.7,
  title = "Alfentanil Demand by Dose (Population Fit)"
)

## ----plot_one_factor_individual, cache = FALSE--------------------------------
plot(
  fit_one_factor_dose,
  show_pred_lines = "individual",
  inv_fun = ll4_inv,
  color_by = "dose",
  observed_point_alpha = 0.4,
  y_trans = "pseudo_log",
  ind_line_alpha = .5,
  title = "Alfentanil Demand by Dose (Subject-Specific Fits)"
) +
  ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(~monkey)

## ----plot_one_factor_axes, cache = FALSE--------------------------------------
plot(
  fit_one_factor_dose,
  inv_fun = ll4_inv,
  color_by = "dose",
  x_trans = "pseudo_log",
  y_trans = "pseudo_log",
  title = "Alfentanil Demand (Log10 Price Scale)"
)

