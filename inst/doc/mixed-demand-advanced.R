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
  cache.path = "mixed-demand-advanced_cache/",
  autodep = TRUE
)

library(beezdemand)
library(dplyr)
library(ggplot2)

data("apt", package = "beezdemand", envir = environment())
data("ko", package = "beezdemand", envir = environment())

# Set `BEEZDEMAND_VIGNETTE_MODE=full` to run the slower / less stable examples.
render_fast <- tolower(Sys.getenv("BEEZDEMAND_VIGNETTE_MODE", "fast")) != "full"

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

# Optional fits used later (kept NULL in fast renders).
fit_two_factors_add <- NULL
fit_two_factors_int <- NULL
fit_simplified_example <- NULL
fit_collapsed_same <- NULL
fit_collapsed_asymmetric <- NULL
fit_drug_slope_random <- NULL
fit_age_add <- NULL
fit_age_rhs <- NULL

## ----shared-setup-------------------------------------------------------------
quick_nlme_control <- nlme::nlmeControl(
  msMaxIter = 100,
  niterEM = 20,
  maxIter = 100, # Low iterations for speed
  pnlsTol = 0.1,
  tolerance = 1e-4, # Looser tolerance
  opt = "nlminb",
  msVerbose = FALSE
)

# Prepare data subsets used throughout
ko_alf <- ko[ko$drug == "Alfentanil", ]

# Fit the one-factor model used as a baseline/fallback in later sections
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

## ----two_factor_add_note, echo=FALSE, results='asis'--------------------------
if (render_fast) {
  cat(
    "*This example is computationally intensive and is not run during standard vignette building. ",
    "To run it, set `BEEZDEMAND_VIGNETTE_MODE=full` as an environment variable before building. ",
    "When successful, the output shows fixed-effect estimates for each drug and dose combination ",
    "on Q0 and alpha, plus random-effects variance components.*\n"
  )
}

## ----two_factor_add, eval = !render_fast, include = !render_fast--------------
# # This model might be slow or fail to converge with the small 'ko' and quick_nlme_control
# # In a real vignette, use a dataset known to converge or increase iterations.
# # For demonstration, we'll proceed.
# fit_two_factors_add <- try(
#   fit_demand_mixed(
#     data = ko,
#     y_var = "y_ll4",
#     x_var = "x",
#     id_var = "monkey",
#     factors = c("drug", "dose"),
#     factor_interaction = FALSE, # Additive effects
#     equation_form = "zben",
#     nlme_control = quick_nlme_control,
#     start_value_method = "pooled_nls"
#   ),
#   silent = TRUE
# )
# 
# if (!inherits(fit_two_factors_add, "try-error")) {
#   print(fit_two_factors_add)
# } else {
#   cat(
#     "Two-factor additive model failed to converge with current settings for the vignette.\n"
#   )
#   fit_two_factors_add <- NULL # Ensure it's NULL for later checks
# }

## ----two_factor_int_note, echo=FALSE, results='asis'--------------------------
if (render_fast) {
  cat(
    "*This example is computationally intensive and is not run during standard vignette building. ",
    "The interaction model allows the effect of dose to differ by drug (and vice versa), ",
    "producing drug:dose interaction terms in the fixed-effects table.*\n"
  )
}

## ----two_factor_int, eval = !render_fast, include = !render_fast--------------
# fit_two_factors_int <- try(
#   fit_demand_mixed(
#     data = ko,
#     y_var = "y_ll4",
#     x_var = "x",
#     id_var = "monkey",
#     factors = c("drug", "dose"),
#     factor_interaction = TRUE, # Interaction effect
#     equation_form = "zben",
#     nlme_control = quick_nlme_control,
#     start_value_method = "pooled_nls"
#   ),
#   silent = TRUE
# )
# 
# if (!inherits(fit_two_factors_int, "try-error")) {
#   print(fit_two_factors_int)
# } else {
#   cat(
#     "Two-factor interaction model failed to converge with current settings for the vignette.\n"
#   )
#   fit_two_factors_int <- NULL
# }

## ----simplified_equation_note, echo=FALSE, results='asis'---------------------
if (render_fast) {
  cat(
    "*This example uses `equation_form = \"simplified\"` with raw consumption values (no LL4 ",
    "transformation needed). The simplified equation handles zeros natively. When this example ",
    "runs, the output resembles the `zben` model output but with `y` as the dependent variable.*\n"
  )
}

## ----simplified_equation, eval = !render_fast, include = !render_fast---------
# better_nlme_control <- nlme::nlmeControl(
#   msMaxIter = 100, # Max iterations for nlmeStruct optimization
#   niterEM = 50, # Max EM iterations
#   maxIter = 100, # Max iterations for the optim call for fixed effects
#   pnlsTol = 1e-4, # Tolerance for PNLS convergence
#   tolerance = 1e-7, # Tolerance for outer optimization loop
#   opt = "nlminb",
#   msVerbose = TRUE # Set to TRUE to see optimization steps
# )
# 
# fit_simplified_example <- try(
#   fit_demand_mixed(
#     data = ko_alf, # Using simpler Alfentanil data
#     y_var = "y", # Raw consumption
#     x_var = "x",
#     id_var = "monkey",
#     factors = "dose",
#     equation_form = "simplified",
#     nlme_control = quick_nlme_control,
#     start_value_method = "pooled_nls"
#   ),
#   silent = TRUE
# )
# 
# if (!inherits(fit_simplified_example, "try-error")) {
#   print(fit_simplified_example)
# } else {
#   cat(
#     "Simplified equation model failed to converge with current settings for the vignette.\n"
#   )
#   fit_simplified_example <- NULL
# }

## -----------------------------------------------------------------------------
# Ensure levels to collapse are present in ko_alf$dose
# levels(ko_alf$dose) are "0.001", "0.003", "3e-04"
fit_collapsed_same <- try(
  fit_demand_mixed(
    data = ko_alf,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "monkey",
    factors = "dose",
    collapse_levels = list(
      Q0 = list(
        dose = list(low_doses = c("3e-04", "0.001"), high_dose = "0.003")
      ),
      alpha = list(
        dose = list(low_doses = c("3e-04", "0.001"), high_dose = "0.003")
      )
    ),
    equation_form = "zben",
    nlme_control = quick_nlme_control
  ),
  silent = TRUE
)

if (
  !is.null(fit_collapsed_same) &&
    !inherits(fit_collapsed_same, "try-error") &&
    !is.null(fit_collapsed_same$model)
) {
  print(fit_collapsed_same)
  cat("\nQ0 params:", fit_collapsed_same$param_info$num_params_Q0, "\n")
  cat("alpha params:", fit_collapsed_same$param_info$num_params_alpha, "\n")
} else {
  cat("Collapsed levels model failed to converge.\n")
}

## -----------------------------------------------------------------------------
# Q0: keep 2 collapsed levels (low vs high)
# alpha: collapse all to 1 level (intercept only)
fit_collapsed_asymmetric <- try(
  fit_demand_mixed(
    data = ko_alf,
    y_var = "y_ll4",
    x_var = "x",
    id_var = "monkey",
    factors = "dose",
    collapse_levels = list(
      Q0 = list(
        dose = list(low_doses = c("3e-04", "0.001"), high_dose = "0.003")
      ),
      alpha = list(dose = list(all_doses = c("3e-04", "0.001", "0.003")))
    ),
    equation_form = "zben",
    nlme_control = quick_nlme_control
  ),
  silent = TRUE
)

if (
  !is.null(fit_collapsed_asymmetric) &&
    !inherits(fit_collapsed_asymmetric, "try-error") &&
    !is.null(fit_collapsed_asymmetric$model)
) {
  print(fit_collapsed_asymmetric)
  cat("\nQ0 params:", fit_collapsed_asymmetric$param_info$num_params_Q0, "\n")
  cat(
    "alpha params:",
    fit_collapsed_asymmetric$param_info$num_params_alpha,
    "\n"
  )
  cat(
    "\nQ0 formula:",
    fit_collapsed_asymmetric$formula_details$fixed_effects_formula_str_Q0,
    "\n"
  )
  cat(
    "alpha formula:",
    fit_collapsed_asymmetric$formula_details$fixed_effects_formula_str_alpha,
    "\n"
  )
} else {
  cat("Asymmetric collapsed levels model failed to converge.\n")
}

## -----------------------------------------------------------------------------
# Using the asymmetric collapsed model from above (if it converged)
if (
  !is.null(fit_collapsed_asymmetric) &&
    !inherits(fit_collapsed_asymmetric, "try-error") &&
    !is.null(fit_collapsed_asymmetric$model)
) {
  cat("--- EMMs with collapsed factors ---\n")
  # EMMs will show collapsed Q0 levels but single alpha value
  collapsed_emms <- get_demand_param_emms(
    fit_obj = fit_collapsed_asymmetric,
    factors_in_emm = "dose", # Use original factor name
    include_ev = TRUE
  )
  print(collapsed_emms)

  cat("\n--- Comparisons with collapsed factors ---\n")
  # Q0 will have comparisons (2 levels), alpha will have none (1 level)
  collapsed_comparisons <- get_demand_comparisons(
    fit_obj = fit_collapsed_asymmetric,
    compare_specs = ~dose, # Use original factor name
    params_to_compare = c("Q0", "alpha")
  )

  cat("\nQ0 contrasts (collapsed levels):\n")
  print(collapsed_comparisons$Q0$contrasts_log10)

  cat("\nalpha contrasts (intercept-only, no comparisons):\n")
  print(collapsed_comparisons$alpha$contrasts_log10)
} else {
  cat("Asymmetric collapsed model not available for EMM demonstration.\n")
}

## ----plot_two_factor_faceting, cache = FALSE----------------------------------
# Assuming fit_two_factors_add was successfully created earlier
# If it failed, this chunk won't produce a plot.
active_two_factor_fit <- if (
  !is.null(fit_two_factors_add) && !is.null(fit_two_factors_add$model)
) {
  fit_two_factors_add
} else {
  # Fallback to a simpler model if the two-factor one failed in the vignette
  if (!is.null(fit_one_factor_dose$model)) fit_one_factor_dose else NULL
}

if (
  !is.null(active_two_factor_fit$model) &&
    !is.null(active_two_factor_fit$param_info$factors) &&
    length(active_two_factor_fit$param_info$factors) >= 1
) {
  # Determine factors for aesthetics based on what's in active_two_factor_fit
  color_factor <- if ("dose" %in% active_two_factor_fit$param_info$factors) {
    "dose"
  } else {
    NULL
  }
  facet_factor_name <- if (
    "drug" %in% active_two_factor_fit$param_info$factors
  ) {
    "drug"
  } else {
    # If only 'dose' is available from fit_one_factor_dose, we can't facet by 'drug'
    # So, maybe facet by dose instead, or don't facet.
    if (
      "dose" %in%
        active_two_factor_fit$param_info$factors &&
        is.null(color_factor)
    ) {
      color_factor <- "dose" # color by dose if not faceting by it
      NULL # No faceting
    } else {
      NULL
    }
  }

  facet_formula_plot <- if (!is.null(facet_factor_name)) {
    stats::as.formula(paste("~", facet_factor_name))
  } else {
    NULL
  }

  plot(
    active_two_factor_fit,
    inv_fun = ll4_inv,
    color_by = color_factor,
    # linetype_by = if("dose" %in% active_two_factor_fit$param_info$factors) "dose" else NULL, # Example
    facet_formula = facet_formula_plot,
    title = "Demand Curves (Population Fit)",
    observed_point_alpha = 0.5,
    ind_line_alpha = .5
  )
} else {
  cat(
    "A suitable two-factor or one-factor model object not available for this plotting example.\n"
  )
}

## ----plot_simplified_model, cache = FALSE-------------------------------------
# Assuming fit_simplified_example converged earlier
if (
  !is.null(fit_simplified_example) && !is.null(fit_simplified_example$model)
) {
  plot(
    fit_simplified_example,
    inv_fun = identity, # Predictions are already on raw y scale
    color_by = "dose",
    shape_by = "dose",
    title = "Demand Model ('simplified' equation, Raw Y)"
  )
} else {
  cat("fit_simplified_example model object not available for plotting.\n")
}

## -----------------------------------------------------------------------------
# We'll use a model with factors.
# If fit_two_factors_add converged, use it. Otherwise, use fit_one_factor_dose.
# For the vignette, let's ensure we use one that is likely available.
# If fit_two_factors_add is NULL (failed to converge in example), this will use fit_one_factor_dose
emm_model_to_use <- if (
  !is.null(fit_two_factors_add) && !is.null(fit_two_factors_add$model)
) {
  fit_two_factors_add
} else if (!is.null(fit_one_factor_dose$model)) {
  fit_one_factor_dose
} else {
  NULL
}

if (!is.null(emm_model_to_use)) {
  cat(
    "--- EMMs for model with factors:",
    paste(emm_model_to_use$param_info$factors, collapse = ", "),
    "---\n"
  )

  factors_for_emms <- emm_model_to_use$param_info$factors

  demand_emms_output <- get_demand_param_emms(
    fit_obj = emm_model_to_use,
    factors_in_emm = factors_for_emms, # Use factors from the model
    include_ev = TRUE
  )
  print(demand_emms_output)

  cat("\n--- EMMs for observed factor combinations only: ---\n")
  # This is useful if the EMM grid includes combinations not in data
  observed_demand_emms <- get_observed_demand_param_emms(
    fit_obj = emm_model_to_use,
    factors_in_emm = factors_for_emms,
    include_ev = TRUE
  )
  print(observed_demand_emms)
} else {
  cat(
    "No suitable model with factors converged for EMM analysis in the vignette.\n"
  )
}

## -----------------------------------------------------------------------------
# Using the same emm_model_to_use
if (
  !is.null(emm_model_to_use) && length(emm_model_to_use$param_info$factors) > 0
) {
  factors_present <- emm_model_to_use$param_info$factors

  if ("dose" %in% factors_present) {
    cat(
      "--- Pairwise comparisons for 'dose' (averaging over other factors if any): ---\n"
    )
    comparisons_dose <- get_demand_comparisons(
      fit_obj = emm_model_to_use,
      compare_specs = ~dose,
      contrast_type = "pairwise",
      adjust = "fdr",
      report_ratios = TRUE
    )
    print(comparisons_dose)
  }

  if (all(c("drug", "dose") %in% factors_present)) {
    cat(
      "\n--- Pairwise comparisons for 'drug' within each level of 'dose': ---\n"
    )
    # EMMs calculated over drug*dose, then contrast drug within each dose
    comparisons_drug_by_dose <- get_demand_comparisons(
      fit_obj = emm_model_to_use,
      compare_specs = ~ drug * dose,
      contrast_type = "pairwise",
      contrast_by = "dose", # Compare 'drug' levels, holding 'dose' constant
      adjust = "fdr",
      report_ratios = TRUE
    )
    print(comparisons_drug_by_dose)
  }
} else {
  cat(
    "No suitable model with factors converged for comparisons in the vignette.\n"
  )
}

## ----advanced_topics_note, echo=FALSE, results='asis'-------------------------
if (render_fast) {
  cat(
    "**Performance Note**: The code examples in the sections below are computationally intensive ",
    "and are not evaluated during standard vignette building. The code is shown for reference. ",
    "To run these examples, set the environment variable `BEEZDEMAND_VIGNETTE_MODE=full` before ",
    "building vignettes.\n"
  )
}

## ----complex_random_effects_fit, eval = !render_fast, include = !render_fast----
# robust_nlme_control <- nlme::nlmeControl(
#   msMaxIter = 100,
#   niterEM = 50,
#   maxIter = 100,
#   pnlsTol = 1e-4,
#   tolerance = 1e-7,
#   opt = "nlminb",
#   msVerbose = TRUE # Set to FALSE for less output once working
# )
# 
# fit_drug_slope_random <- try(
#   fit_demand_mixed(
#     data = ko,
#     y_var = "y_ll4",
#     x_var = "x",
#     id_var = "monkey", # This is your 'monkey' identifier
#     factors = c("drug", "dose"),
#     factor_interaction = FALSE, # Additive fixed effects as requested
#     equation_form = "zben",
#     random_effects = Q0 + alpha ~ drug, # Random slopes by drug
#     covariance_structure = "pdDiag",
#     nlme_control = robust_nlme_control,
#     start_value_method = "pooled_nls" # Good for more complex models
#   ),
#   silent = TRUE
# )
# 
# if (
#   !inherits(fit_drug_slope_random, "try-error") &&
#     !is.null(fit_drug_slope_random$model)
# ) {
#   print(fit_drug_slope_random)
# 
#   cat("\nFixed Effects:\n")
#   print(nlme::fixef(fit_drug_slope_random))
# 
#   cat("\nRandom Effects Structure (Standard Deviations and Correlations):\n")
#   print(nlme::VarCorr(fit_drug_slope_random$model))
# 
#   cat(
#     "\nRandom Effect Estimates for each Monkey (deviations from fixed effects):\n"
#   )
#   print(utils::head(nlme::ranef(fit_drug_slope_random)))
# 
#   cat("\nSubject-Specific Coefficients (Fixed + Random, log10 scale):\n")
#   print(head(coef(fit_drug_slope_random))) # type="combined" is default
# 
#   all_params_emms <- get_demand_param_emms(
#     fit_obj = fit_drug_slope_random,
#     factors_in_emm = c("drug", "dose"), # Get all cells
#     at = list(monkey = "A"), # Condition on monkey A
#     include_ev = TRUE # Or FALSE if not needed
#   )
# 
#   observed_params_emms <- get_observed_demand_param_emms(
#     fit_obj = fit_drug_slope_random,
#     factors_in_emm = c("drug", "dose"), # Get all cells
#     at = list(monkey = "A"), # Condition on monkey A
#     include_ev = TRUE # Or FALSE if not needed
#   )
# } else {
#   cat("Random-slope model failed to converge with the current settings.\n")
#   fit_drug_slope_random <- NULL
# }

## ----complex_random_effects_coefs, eval = !render_fast, include = !render_fast----
# if (!is.null(fit_drug_slope_random) && !is.null(fit_drug_slope_random$model)) {
#   # Get individual coefficients in wide format (default)
#   individual_coefs_wide <- get_individual_coefficients(
#     fit_drug_slope_random,
#     params = c("Q0", "alpha"),
#     format = "wide"
#   )
# 
#   cat("Individual coefficients (wide format, log10 scale):\n")
#   print(individual_coefs_wide)
# 
#   # Get individual coefficients in long format
#   individual_coefs_long <- get_individual_coefficients(
#     fit_drug_slope_random,
#     params = c("Q0", "alpha"),
#     format = "long"
#   )
# 
#   cat("\nIndividual coefficients (long format, log10 scale):\n")
#   print(head(individual_coefs_long, 10))
# 
#   # Convert to natural scale for interpretation
#   individual_coefs_natural <- individual_coefs_wide
# 
#   # Convert Q0 coefficients to natural scale
#   q0_cols <- grep(
#     "^estimated_Q0_",
#     names(individual_coefs_natural),
#     value = TRUE
#   )
#   for (col in q0_cols) {
#     natural_col <- gsub("estimated_Q0_", "Q0_natural_", col)
#     individual_coefs_natural[[natural_col]] <- 10^individual_coefs_natural[[
#       col
#     ]]
#   }
# 
#   # Convert alpha coefficients to natural scale
#   alpha_cols <- grep(
#     "^estimated_alpha_",
#     names(individual_coefs_natural),
#     value = TRUE
#   )
#   for (col in alpha_cols) {
#     natural_col <- gsub("estimated_alpha_", "alpha_natural_", col)
#     individual_coefs_natural[[natural_col]] <- 10^individual_coefs_natural[[
#       col
#     ]]
#   }
# 
#   cat("\nExample individual coefficients on natural scale:\n")
#   print(individual_coefs_natural[
#     1:3,
#     c(
#       "id",
#       grep("Q0_natural_", names(individual_coefs_natural), value = TRUE)[1:2],
#       grep("alpha_natural_", names(individual_coefs_natural), value = TRUE)[1:2]
#     )
#   ])
# 
#   # You can also get coefficients for specific parameters only
#   q0_only <- get_individual_coefficients(
#     fit_drug_slope_random,
#     params = "Q0",
#     format = "wide"
#   )
# 
#   cat("\nQ0 coefficients only:\n")
#   print(q0_only)
# } else {
#   cat("Random-slope model not available for coefficient extraction.\n")
# }

## ----continuous_setup, eval = !render_fast, include = !render_fast------------
# set.seed(123)
# 
# # Create a per-monkey continuous covariate: age
# monkey_age <- unique(ko[, c("monkey")]) |>
#   dplyr::mutate(age = round(runif(dplyr::n(), min = 18, max = 25), 1))
# 
# ko_age <- dplyr::left_join(ko, monkey_age, by = "monkey")
# # Ensure a numeric version of dose is available for continuous modeling
# ko_age <- ko_age |>
#   dplyr::mutate(dose_num = suppressWarnings(as.numeric(as.character(dose))))
# 
# # Robust median for dose_num (fallbacks if all values are NA)
# median_dose_num <- suppressWarnings(stats::median(
#   ko_age$dose_num,
#   na.rm = TRUE
# ))
# if (is.na(median_dose_num) || !is.finite(median_dose_num)) {
#   finite_vals <- ko_age$dose_num[is.finite(ko_age$dose_num)]
#   if (length(finite_vals) > 0) {
#     median_dose_num <- finite_vals[1]
#   } else {
#     median_dose_num <- 0
#   }
# }
# 
# # Sanity check
# head(ko_age[, c("monkey", "age")])

## ----continuous_age_add, eval = !render_fast, include = !render_fast----------
# fit_age_add <- try(
#   fit_demand_mixed(
#     data = ko_age,
#     y_var = "y_ll4",
#     x_var = "x",
#     id_var = "monkey",
#     factors = "dose",
#     continuous_covariates = c("age"),
#     equation_form = "zben",
#     nlme_control = quick_nlme_control,
#     start_value_method = "pooled_nls"
#   ),
#   silent = TRUE
# )
# 
# if (!inherits(fit_age_add, "try-error") && !is.null(fit_age_add$model)) {
#   print(fit_age_add)
# 
#   # EMMs over dose at age = 21
#   emms_age_add <- get_demand_param_emms(
#     fit_obj = fit_age_add,
#     factors_in_emm = c("dose"),
#     at = list(age = 21),
#     include_ev = TRUE
#   )
#   cat("\nEMMs (age = 21):\n")
#   print(emms_age_add)
# 
#   # Pairwise comparisons for dose at age = 21 (log10 scale; ratios reported too)
#   comps_age_add <- get_demand_comparisons(
#     fit_obj = fit_age_add,
#     compare_specs = ~dose,
#     contrast_type = "pairwise",
#     at = list(age = 21),
#     adjust = "fdr",
#     report_ratios = TRUE
#   )
#   cat("\nPairwise comparisons for dose (age = 21):\n")
#   print(comps_age_add)
# 
#   # Plot population lines at age = 21 (natural scale on y)
#   plot(
#     fit_age_add,
#     at = list(age = 21),
#     inv_fun = ll4_inv,
#     color_by = "dose",
#     x_trans = "pseudo_log",
#     y_trans = "pseudo_log",
#     title = "Demand by Dose with Age as Covariate (Population Fit at age = 21)"
#   )
# } else {
#   cat(
#     "\nAdditive continuous covariate model failed to converge for the vignette settings.\n"
#   )
# }

## ----continuous_age_rhs, eval = !render_fast, include = !render_fast----------
# fit_age_rhs <- try(
#   fit_demand_mixed(
#     data = ko_age,
#     y_var = "y_ll4",
#     x_var = "x",
#     id_var = "monkey",
#     factors = c("drug"), # only 'drug' is treated as a factor
#     fixed_rhs = "~ 1 + drug + dose_num + age",
#     equation_form = "zben",
#     nlme_control = quick_nlme_control,
#     start_value_method = "pooled_nls"
#   ),
#   silent = TRUE
# )
# 
# if (!inherits(fit_age_rhs, "try-error") && !is.null(fit_age_rhs$model)) {
#   print(fit_age_rhs)
# 
#   # EMMs over drug (factor) at age = 21 and dose set to its median
#   emms_age_rhs <- get_demand_param_emms(
#     fit_obj = fit_age_rhs,
#     factors_in_emm = c("drug"),
#     at = list(age = 21, dose_num = median_dose_num),
#     include_ev = TRUE
#   )
#   cat("\nEMMs (drug; age = 21, dose_num = median):\n")
#   print(emms_age_rhs)
# 
#   # Pairwise comparisons: drug at age = 21 and dose set to its median
#   comps_drug_by_dose_age <- get_demand_comparisons(
#     fit_obj = fit_age_rhs,
#     compare_specs = ~drug,
#     contrast_type = "pairwise",
#     at = list(age = 21, dose_num = median_dose_num),
#     adjust = "fdr",
#     report_ratios = TRUE
#   )
#   cat("\nPairwise comparisons of drug (age = 21, dose_num = median):\n")
#   print(comps_drug_by_dose_age)
# 
#   # Plot population lines at age = 21 and dose_num = median, color by drug and facet by drug
#   plot(
#     fit_age_rhs,
#     at = list(age = 21, dose_num = median_dose_num),
#     inv_fun = ll4_inv,
#     color_by = "drug",
#     facet_formula = ~drug,
#     x_trans = "pseudo_log",
#     y_trans = "pseudo_log",
#     title = "Demand by Drug with Dose Continuous (Population Fit at age = 21; dose_num = median)"
#   )
# } else {
#   cat(
#     "\nfixed_rhs model (drug factor, dose continuous) failed to converge for the vignette settings.\n"
#   )
# }

## ----trends, eval = !render_fast, include = !render_fast----------------------
# # Choose a fitted model for trends (prefer fixed_rhs fit)
# fit_for_trends <- NULL
# if (
#   exists("fit_age_rhs") && !is.null(fit_age_rhs) && !is.null(fit_age_rhs$model)
# ) {
#   fit_for_trends <- fit_age_rhs
# } else if (
#   exists("fit_age_add") && !is.null(fit_age_add) && !is.null(fit_age_add$model)
# ) {
#   fit_for_trends <- fit_age_add
# }
# 
# if (!is.null(fit_for_trends)) {
#   # Determine which of the candidate covariates are actually present in the model RHS
#   # Use Q0 formula (or alpha if Q0 is NULL) - they may differ if collapse_levels was used
#   rhs_formula_str <- fit_for_trends$formula_details$fixed_effects_formula_str_Q0
#   if (
#     is.null(rhs_formula_str) ||
#       is.na(rhs_formula_str) ||
#       !nzchar(rhs_formula_str)
#   ) {
#     rhs_formula_str <- fit_for_trends$formula_details$fixed_effects_formula_str_alpha
#   }
#   rhs_vars_tr <- tryCatch(
#     all.vars(as.formula(rhs_formula_str)),
#     error = function(e) character(0)
#   )
#   covariates_candidate <- c("age", "dose_num")
#   covariates_in_model <- intersect(rhs_vars_tr, covariates_candidate)
# 
#   if (length(covariates_in_model) > 0) {
#     cat("\n--- Trends (overall) at age = 21 and dose_num = median ---\n")
#     trends_overall <- get_demand_param_trends(
#       fit_obj = fit_for_trends,
#       params = c("Q0", "alpha"),
#       covariates = covariates_in_model,
#       specs = ~1,
#       at = list(age = 21, dose_num = median_dose_num)
#     )
#     print(trends_overall)
# 
#     # If 'drug' is a model factor, show trends by drug as well
#     model_factors_tr <- fit_for_trends$param_info$factors
#     if (!is.null(model_factors_tr) && ("drug" %in% model_factors_tr)) {
#       cat("\n--- Trends by drug at age = 21 and dose_num = median ---\n")
#       trends_by_drug <- get_demand_param_trends(
#         fit_obj = fit_for_trends,
#         params = c("Q0", "alpha"),
#         covariates = covariates_in_model,
#         specs = ~drug,
#         at = list(age = 21, dose_num = median_dose_num)
#       )
#       print(trends_by_drug)
#     } else {
#       cat(
#         "\nNote: The selected model does not include 'drug' as a factor; skipping by-drug trends.\n"
#       )
#     }
#   } else {
#     cat(
#       "\nNo candidate continuous covariates ('age', 'dose_num') are present in the model RHS; skipping trends.\n"
#     )
#   }
# } else {
#   cat(
#     "\nNo suitable fitted model available for computing trends in the vignette.\n"
#   )
# }

