## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
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

## ----data-example, echo=TRUE--------------------------------------------------
library(beezdemand)

# Example data structure
knitr::kable(
    head(apt, 10),
    caption = "Example APT data structure (first 10 rows)"
)

## ----basic-fit----------------------------------------------------------------
# Fit 2-RE model (simpler, faster)
fit2 <- fit_demand_hurdle(
    data = apt,
    y_var = "y",
    x_var = "x",
    id_var = "id",
    random_effects = c("zeros", "q0"), # 2 random effects
    verbose = 0
)

## ----fit-3re, eval=FALSE------------------------------------------------------
# # Fit 3-RE model (more flexible)
# fit3 <- fit_demand_hurdle(
#     data = apt,
#     y_var = "y",
#     x_var = "x",
#     id_var = "id",
#     random_effects = c("zeros", "q0", "alpha"), # 3 random effects
#     verbose = 1
# )

## ----summary-example----------------------------------------------------------
# View summary
summary(fit2)

# Extract coefficients
coef(fit2)

# Standardized tidy summaries
tidy(fit2) |> head()
glance(fit2)

# Get subject-specific parameters
head(get_subject_pars(fit2))

## ----diagnostics--------------------------------------------------------------
check_demand_model(fit2)
plot_residuals(fit2)$fitted
plot_qq(fit2)

## ----model-comparison, eval=FALSE---------------------------------------------
# # Compare nested models
# compare_hurdle_models(fit3, fit2)
# 
# # Unified model comparison (AIC/BIC + LRT when appropriate)
# compare_models(fit3, fit2)
# 
# # Output:
# # Likelihood Ratio Test
# # =====================
# #            Model n_RE    LogLik df     AIC     BIC
# #    Full (3 RE)    3 -1234.56 12 2493.12 2543.21
# # Reduced (2 RE)    2 -1245.78  9 2509.56 2547.89
# #
# # LR statistic: 22.44
# # df: 3
# # p-value: 5.2e-05

## ----plotting-demand, fig.cap="Population demand curve from 2-RE hurdle model."----
# Population demand curve
plot(fit2, type = "demand")

## ----plotting-probability, fig.cap="Probability of zero consumption as a function of price."----
# Probability of zero consumption
plot(fit2, type = "probability")

## ----plotting-extra, eval=FALSE-----------------------------------------------
# # Distribution of individual parameters
# plot(fit2, type = "parameters")
# plot(fit2, type = "parameters", parameters = c("Q0", "alpha", "Pmax"))
# 
# # Individual demand curves
# plot(fit2, type = "individual", subjects = c("1", "2", "3", "4"))
# 
# # Single subject with observed data
# plot_subject(fit2, subject_id = "1", show_data = TRUE, show_population = TRUE)

## ----simulate-basic, eval=FALSE-----------------------------------------------
# # Simulate with default parameters
# sim_data <- simulate_hurdle_data(
#     n_subjects = 100,
#     seed = 123
# )
# 
# head(sim_data)
# #   id    x         y delta       a_i        b_i
# # 1  1 0.00 12.345678     0 -0.523456  0.1234567
# # 2  1 0.50 11.234567     0 -0.523456  0.1234567
# # ...
# 
# # Custom parameters
# sim_custom <- simulate_hurdle_data(
#     n_subjects = 100,
#     logQ0 = log(15), # Q0 = 15
#     alpha = 0.1, # Lower elasticity
#     k = 3, # Higher k (ensures Pmax exists)
#     stop_at_zero = FALSE, # All prices for all subjects
#     seed = 456
# )

## ----monte-carlo, eval=FALSE--------------------------------------------------
# # Run Monte Carlo study
# mc_results <- run_hurdle_monte_carlo(
#     n_sim = 100, # Number of simulations
#     n_subjects = 100, # Subjects per simulation
#     n_random_effects = 2, # 2-RE model
#     verbose = TRUE,
#     seed = 123
# )
# 
# # View summary
# print_mc_summary(mc_results)
# 
# # Monte Carlo Simulation Summary
# # ==============================
# #
# # Simulations: 100 attempted, 95 converged (95.0%)
# #
# #    Parameter True Mean_Est   Bias Rel_Bias% Emp_SE Mean_SE SE_Ratio Coverage_95%  N
# #        beta0 -2.0    -2.01  -0.01      0.5   0.12    0.11     0.92         94.7 95
# #        beta1  1.0     1.02   0.02      2.0   0.08    0.08     1.00         95.8 95
# #        logQ0  2.3     2.29  -0.01     -0.4   0.05    0.05     1.00         94.7 95
# #            k  2.0     2.03   0.03      1.5   0.15    0.14     0.93         93.7 95
# #        alpha  0.5     0.51   0.01      2.0   0.04    0.04     1.00         95.8 95
# # ...

## ----integration, eval=FALSE--------------------------------------------------
# # Fit hurdle model
# hurdle_fit <- fit_demand_hurdle(data,
#     y_var = "y", x_var = "x", id_var = "id",
#     random_effects = c("zeros", "q0"), verbose = 0
# )
# 
# # Extract subject parameters
# hurdle_pars <- get_subject_pars(hurdle_fit)
# 
# # These can be merged with other analyses
# # e.g., correlate with individual characteristics
# cor(hurdle_pars$Q0, subject_characteristics$age)
# cor(hurdle_pars$breakpoint, subject_characteristics$dependence_score)

## ----export, eval=FALSE-------------------------------------------------------
# # Subject parameters
# write.csv(get_subject_pars(hurdle_fit), "hurdle_subject_parameters.csv")
# 
# # Summary statistics
# summ <- get_hurdle_param_summary(hurdle_fit)
# print(summ)

## ----control-params, eval=FALSE-----------------------------------------------
# fit <- fit_demand_hurdle(
#     data,
#     y_var = "y",
#     x_var = "x",
#     id_var = "id",
#     epsilon = 0.001, # Constant for log(price + epsilon)
#     tmb_control = list(
#         max_iter = 200, # Maximum iterations
#         eval_max = 1000, # Maximum function evaluations
#         trace = 0 # Optimization trace level
#     ),
#     verbose = 1 # 0 = silent, 1 = progress, 2 = detailed
# )

## ----custom-starts, eval=FALSE------------------------------------------------
# custom_starts <- list(
#     beta0 = -3.0,
#     beta1 = 1.5,
#     logQ0 = log(8),
#     k = 2.5,
#     alpha = 0.1,
#     logsigma_a = 0.5,
#     logsigma_b = -0.5,
#     logsigma_e = -1.0,
#     rho_ab_raw = 0
# )
# 
# fit <- fit_demand_hurdle(data,
#     y_var = "y", x_var = "x", id_var = "id",
#     start_values = custom_starts, verbose = 1
# )

