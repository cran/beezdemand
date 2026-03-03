## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "png",
  dpi = 144,
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE,
  cache = TRUE,
  cache.path = "fixed-demand_cache/",
  autodep = TRUE
)

library(beezdemand)
library(dplyr)
library(ggplot2)

data("apt", package = "beezdemand", envir = environment())

cache_key_object <- function(x) {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(x, tmp)
  on.exit(unlink(tmp), add = TRUE)
  unname(tools::md5sum(tmp))
}

knitr::opts_chunk$set(
  cache.extra = list(
    beezdemand_version = as.character(utils::packageVersion("beezdemand")),
    apt = cache_key_object(apt)
  )
)

## ----fit_hs-------------------------------------------------------------------
fit_hs <- fit_demand_fixed(apt, equation = "hs", k = 2)
fit_hs

## ----fit_koff-----------------------------------------------------------------
fit_koff <- fit_demand_fixed(apt, equation = "koff", k = 2)
fit_koff

## ----fit_simplified-----------------------------------------------------------
fit_simplified <- fit_demand_fixed(apt, equation = "simplified")
fit_simplified

## ----fit_k_options, eval = FALSE----------------------------------------------
# ## Fixed k (default)
# fit_demand_fixed(apt, equation = "hs", k = 2)
# 
# ## Individual k per subject
# fit_demand_fixed(apt, equation = "hs", k = "ind")
# 
# ## Shared k across subjects
# fit_demand_fixed(apt, equation = "hs", k = "share")
# 
# ## Fitted k as free parameter
# fit_demand_fixed(apt, equation = "hs", k = "fit")

## ----fit_log10, eval = FALSE--------------------------------------------------
# fit_demand_fixed(apt, equation = "hs", k = 2, param_space = "log10")

## ----tidy---------------------------------------------------------------------
tidy(fit_hs)

## ----glance-------------------------------------------------------------------
glance(fit_hs)

## ----augment------------------------------------------------------------------
augment(fit_hs)

## ----confint------------------------------------------------------------------
confint(fit_hs)

## ----summary------------------------------------------------------------------
summary(fit_hs)

## ----alpha_star---------------------------------------------------------------
tidy(fit_hs) |>
  filter(term == "alpha_star") |>
  select(id, term, estimate, std.error)

## ----plot_basic, cache = FALSE------------------------------------------------
plot(fit_hs)

## ----plot_facet, cache = FALSE, fig.height = 8--------------------------------
plot(fit_hs, facet = TRUE)

## ----plot_transforms, cache = FALSE-------------------------------------------
plot(fit_hs, x_trans = "pseudo_log", y_trans = "pseudo_log")

## ----diagnostics--------------------------------------------------------------
check_demand_model(fit_hs)

## ----residuals, cache = FALSE-------------------------------------------------
plot_residuals(fit_hs)$fitted

## ----predict_default----------------------------------------------------------
predict(fit_hs)

## ----predict_custom-----------------------------------------------------------
new_prices <- data.frame(x = c(0, 0.5, 1, 2, 5, 10, 20))
predict(fit_hs, newdata = new_prices)

## ----agg_mean-----------------------------------------------------------------
fit_mean <- fit_demand_fixed(apt, equation = "hs", k = 2, agg = "Mean")
fit_mean

## ----agg_mean_plot, cache = FALSE---------------------------------------------
plot(fit_mean)

## ----agg_pooled---------------------------------------------------------------
fit_pooled <- fit_demand_fixed(apt, equation = "hs", k = 2, agg = "Pooled")
fit_pooled

## ----agg_pooled_plot, cache = FALSE-------------------------------------------
plot(fit_pooled)

