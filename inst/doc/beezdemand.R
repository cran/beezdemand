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

## ----cran-install, eval = FALSE-----------------------------------------------
# install.packages("beezdemand")
# 
# library(beezdemand)

## ----git-install, eval = FALSE------------------------------------------------
# install.packages("devtools")
# 
# devtools::install_github("brentkaplan/beezdemand", build_vignettes = TRUE)
# 
# library(beezdemand)

## ----gitdev-install, eval = FALSE---------------------------------------------
# devtools::install_github("brentkaplan/beezdemand@develop")

## ----packages, include = FALSE, echo = FALSE----------------------------------
# Package dependencies are specified in DESCRIPTION
# They should already be installed when building vignettes
library(dplyr)
library(tidyr)
library(ggplot2)
library(beezdemand)

## ----example-data-set, echo=FALSE, results='asis'-----------------------------
knitr::kable(
    apt[c(1:10, 17:26), ],
    caption = "Example APT (Alcohol Purchase Task) data in long format"
)

## ----example-wide-------------------------------------------------------------
## the following code takes the apt data, which are in long format, and converts
## to a wide format that might be seen from data collection software
wide <- as.data.frame(tidyr::pivot_wider(apt, names_from = x, values_from = y))
colnames(wide) <- c("id", paste0("price_", seq(1, 16, by = 1)))
knitr::kable(
    wide[1:5, 1:10],
    caption = "Example data in wide format (first 5 participants, first 10 prices)"
)

## ----example-pivot------------------------------------------------------------
long <- pivot_demand_data(
  wide,
  format = "long",
  x_values = c(0, 0.5, 1, 1.50, 2, 2.50, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20)
)
knitr::kable(
    head(long),
    caption = "Wide to long conversion using pivot_demand_data()"
)

## ----example-wide-manual------------------------------------------------------
## make a copy for the manual approach
wide_manual <- wide
newcolnames <- c("id", "0", "0.5", "1", "1.50", "2", "2.50", "3",
                 "4", "5", "6", "7", "8", "9", "10", "15", "20")
colnames(wide_manual) <- newcolnames

## ----example-w2l-manual-------------------------------------------------------
long_manual <- tidyr::pivot_longer(wide_manual, -id,
                                   names_to = "price", values_to = "consumption")
long_manual <- dplyr::arrange(long_manual, id)
colnames(long_manual) <- c("id", "x", "y")
long_manual$x <- as.numeric(long_manual$x)
long_manual$y <- as.numeric(long_manual$y)

## ----descriptive--------------------------------------------------------------
desc <- get_descriptive_summary(apt)
knitr::kable(
    desc$statistics,
    caption = "Descriptive statistics by price point",
    digits = 2
)

## ----descriptive-plot, fig.width=7, fig.height=5------------------------------
plot(desc)

## ----change-data, eval = FALSE------------------------------------------------
# ChangeData(apt, nrepl = 1, replnum = 0.01, rem0 = FALSE, remq0e = FALSE, replfree = NULL)

## ----unsystematic, eval=FALSE-------------------------------------------------
# check_systematic_demand(
#   data = apt,
#   trend_threshold = 0.025,
#   bounce_threshold = 0.1,
#   max_reversals = 0,
#   consecutive_zeros = 2
# )

## ----unsystematic-output, echo=FALSE, results='asis'--------------------------
unsys <- check_systematic_demand(
  data = apt,
  trend_threshold = 0.025,
  bounce_threshold = 0.1,
  max_reversals = 0,
  consecutive_zeros = 2
)
knitr::kable(
    head(unsys$results, 5),
    caption = "Systematicity check results (first 5 participants)"
)

## ----empirical, eval=FALSE----------------------------------------------------
# get_empirical_measures(apt)

## ----empirical-output, echo=FALSE, results='asis'-----------------------------
knitr::kable(
    head(get_empirical_measures(apt)$measures, 5),
    caption = "Empirical demand measures (first 5 participants)",
    digits = 3
)

## ----zero-warning, eval=FALSE-------------------------------------------------
# Warning message:
# Zeros found in data not compatible with equation! Dropping zeros!

## ----hs, eval=FALSE-----------------------------------------------------------
# fit_demand_fixed(data = apt, equation = "hs", k = 2)

## ----hs-setup, include=FALSE--------------------------------------------------
fit_hs <- fit_demand_fixed(apt, equation = "hs", k = 2)
hs_tidy <- tidy(fit_hs)
hs_glance <- glance(fit_hs)
hs_confint <- confint(fit_hs)
hs_aug <- augment(fit_hs)
hs_diag <- check_demand_model(fit_hs)

## ----hs-output, echo=FALSE, results='asis'------------------------------------
knitr::kable(head(hs_tidy, 10), caption = "Parameter estimates (`tidy()`, first 10 rows)")
knitr::kable(hs_glance, caption = "Model summary (`glance()`)")
knitr::kable(head(hs_confint, 10), caption = "Confidence intervals (`confint()`, first 10 rows)")
knitr::kable(head(hs_aug, 10), caption = "Fitted values and residuals (`augment()`, first 10 rows)")

## ----hs-diagnostics-----------------------------------------------------------
hs_diag

## ----hs-plot, fig.width=7, fig.height=4---------------------------------------
plot(fit_hs)

## ----hs-residuals, fig.width=7, fig.height=4----------------------------------
plot_residuals(fit_hs)$fitted

## ----alpha-star, echo=TRUE----------------------------------------------------
## alpha_star is included in tidy() output for HS and Koff equations
hs_tidy[hs_tidy$term == "alpha_star", c("id", "term", "estimate", "std.error")]

## ----koff, eval=FALSE---------------------------------------------------------
# fit_demand_fixed(data = apt, equation = "koff", k = 2)

## ----simplified, eval=FALSE---------------------------------------------------
# fit_demand_fixed(data = apt, equation = "simplified")

## ----agg-mean, eval = FALSE---------------------------------------------------
# fit_demand_fixed(data = apt, equation = "hs", k = 2, agg = "Mean")

## ----agg-pooled, eval = FALSE-------------------------------------------------
# fit_demand_fixed(data = apt, equation = "hs", k = 2, agg = "Pooled")

## ----share, eval=FALSE--------------------------------------------------------
# fit_demand_fixed(data = apt, equation = "hs", k = "share")

## ----share-setup, include=FALSE-----------------------------------------------
fit_share <- fit_demand_fixed(apt, equation = "hs", k = "share")
share_tidy <- tidy(fit_share)
share_glance <- glance(fit_share)

## ----share-output, echo=FALSE, results='asis'---------------------------------
knitr::kable(head(share_tidy, 10), caption = "Parameter estimates (`tidy()`, first 10 rows)")
knitr::kable(share_glance, caption = "Model summary (`glance()`)")

## ----learn, eval=FALSE--------------------------------------------------------
# ## Modern interface (recommended)
# ?check_systematic_demand
# 
# ## Legacy interface (still available)
# ?CheckUnsystematic

