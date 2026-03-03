## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    include = TRUE,
    collapse = TRUE,
    comment = "#>",
    dev = "png",
    dpi = 144,
    fig.width = 7,
    fig.height = 5
)

library(beezdemand)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Load package datasets
data("etm", package = "beezdemand")
data("lowNicClean", package = "beezdemand")
data("cannabisCigarettes", package = "beezdemand")
data("ongoingETM", package = "beezdemand")

lnic <- lowNicClean |>
    mutate(
        target = if_else(commodity == "cigarettesAdj", "adjusting", "fixed"),
        group = "cigarettes"
    ) |>
    select(
        -commodity
    ) |>
    relocate(condition, .after = group)

can_cig <- cannabisCigarettes |>
    select(
        id,
        x,
        y,
        target = commodity
    )

ongoing_etm <- ongoingETM |>
    pivot_longer(
        -c(id, x, flavor),
        names_to = "target",
        values_to = "y",
        values_drop_na = TRUE
    ) |>
    select(id, x, y, target)

## -----------------------------------------------------------------------------
glimpse(etm)

## ----load-cp------------------------------------------------------------------
# Load the cross-price example dataset
data("cp", package = "beezdemand")

# Examine structure
glimpse(cp)

# View conditions
table(cp$target)

## ----fit-alone----------------------------------------------------------------
# Filter to alone condition
alone_data <- cp |>
    dplyr::filter(target == "alone")

# Fit demand curve (modern interface)
fit_alone <- fit_demand_fixed(
    data = alone_data,
    equation = "koff",
    k = 2
)

# View results
fit_alone

## ----fit-own------------------------------------------------------------------
# Filter to own condition
own_data <- cp |>
    dplyr::filter(target == "own")

# Fit demand curve
fit_own <- fit_demand_fixed(
    data = own_data,
    equation = "koff",
    k = 2
)

# View results
fit_own

## ----fit-alt-cp---------------------------------------------------------------
# Filter to alt condition
alt_data <- cp |>
    dplyr::filter(target == "alt")

# Fit cross-price model
fit_alt <- fit_cp_nls(
    data = alt_data,
    equation = "exponentiated",
    return_all = TRUE
)

# View results
summary(fit_alt)

## ----compare-conditions-------------------------------------------------------
# Extract key parameters for each condition
coef_alone <- coef(fit_alone)
Q0_alone <- coef_alone$estimate[coef_alone$term == "q0"]
Alpha_alone <- coef_alone$estimate[coef_alone$term == "alpha"]

coef_own <- coef(fit_own)
Q0_own <- coef_own$estimate[coef_own$term == "q0"]
Alpha_own <- coef_own$estimate[coef_own$term == "alpha"]

comparison <- data.frame(
    Condition = c("Alone (Target)", "Own (Target)", "Alt (Cross-Price)"),
    Q0_or_Qalone = c(
        Q0_alone,
        Q0_own,
        coef(fit_alt)["qalone"]
    ),
    Alpha_or_I = c(
        Alpha_alone,
        Alpha_own,
        coef(fit_alt)["I"]
    )
)

comparison

## ----combined-plot, fig.width=8, fig.height=4---------------------------------
# Create prediction data
x_seq <- seq(0.01, max(cp$x), length.out = 100)

# Get demand predictions for each condition
pred_alone <- predict(fit_alone, newdata = data.frame(x = x_seq))$.fitted
pred_own <- predict(fit_own, newdata = data.frame(x = x_seq))$.fitted

# Cross-price model predictions (always on the natural y scale)
pred_alt <- predict(fit_alt, newdata = data.frame(x = x_seq))$y_pred

# Combine into plot data
plot_data <- data.frame(
    x = rep(x_seq, 3),
    y = c(pred_alone, pred_own, pred_alt),
    Condition = rep(c("Target (Alone)", "Target (Own)", "Alternative (Cross-Price)"), each = length(x_seq))
)

# Plot
ggplot() +
    geom_point(data = cp, aes(x = x, y = y, color = target), alpha = 0.6) +
    geom_line(data = plot_data, aes(x = x, y = y, color = Condition), linewidth = 1) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
        x = "Target Price",
        y = "Consumption",
        title = "Cross-Price Analysis: All Conditions",
        color = "Condition"
    ) +
    theme_bw()

## -----------------------------------------------------------------------------
etm |>
    dplyr::filter(group %in% "E-Cigarettes" & id %in% 1)


unsys_one <- etm |>
    filter(group %in% "E-Cigarettes" & id %in% 1) |>
    check_systematic_cp()

unsys_one$results

unsys_one_lnic <- lnic |>
    filter(
        target == "adjusting",
        id == "R_00Q12ahGPKuESBT"
    ) |>
    check_systematic_cp()

unsys_one_lnic$results

## ----compute-unsys-etm, results='hide'----------------------------------------
unsys_all <- etm |>
    group_by(id, group) |>
    nest() |>
    mutate(
        sys = map(data, check_systematic_cp),
        results = map(sys, ~ dplyr::select(.x$results, -id))
    ) |>
    select(-data, -sys) |>
    unnest(results)

## ----show-unsys-etm-----------------------------------------------------------
knitr::kable(
    unsys_all |>
        group_by(group) |>
        summarise(
            n_subjects = n(),
            pct_systematic = round(mean(systematic, na.rm = TRUE) * 100, 1),
            .groups = "drop"
        ),
    caption = "Systematicity check by product group (ETM dataset)"
)

## ----compute-unsys-lnic, results='hide'---------------------------------------
unsys_all_lnic <- lnic |>
    filter(target == "fixed") |>
    group_by(id, condition) |>
    nest() |>
    mutate(
        sys = map(
            data,
            check_systematic_cp
        )
    ) |>
    mutate(results = map(sys, ~ dplyr::select(.x$results, -id))) |>
    select(-data, -sys) |>
    unnest(results) |>
    arrange(id)

## ----show-unsys-lnic----------------------------------------------------------
knitr::kable(
    unsys_all_lnic |>
        group_by(condition) |>
        summarise(
            n_subjects = n(),
            pct_systematic = round(mean(systematic, na.rm = TRUE) * 100, 1),
            .groups = "drop"
        ),
    caption = "Systematicity check by condition (Low Nicotine study, Kaplan et al., 2018)"
)

## ----compute-unsys-can-cig, results='hide'------------------------------------
unsys_all_can_cig <- can_cig |>
    filter(target %in% c("cannabisFix", "cigarettesFix")) |>
    group_by(id, target) |>
    nest() |>
    mutate(
        sys = map(data, check_systematic_cp),
        results = map(sys, ~ dplyr::select(.x$results, -id))
    ) |>
    select(-data, -sys) |>
    unnest(results) |>
    arrange(id)

## ----show-unsys-can-cig-------------------------------------------------------
knitr::kable(
    unsys_all_can_cig |>
        group_by(target) |>
        summarise(
            n_subjects = n(),
            pct_systematic = round(mean(systematic, na.rm = TRUE) * 100, 1),
            .groups = "drop"
        ),
    caption = "Systematicity check by target (Cannabis/Cigarettes, unpublished data)"
)

## ----compute-unsys-ongoing-etm, results='hide'--------------------------------
unsys_all_ongoing_etm <- ongoing_etm |>
    # one person is doubled up
    distinct() |>
    filter(target %in% c("FixCig", "ECig")) |>
    group_by(id, target) |>
    nest() |>
    mutate(
        sys = map(data, check_systematic_cp),
        results = map(sys, ~ dplyr::select(.x$results, -id))
    ) |>
    select(-data, -sys) |>
    unnest(results)

## ----show-unsys-ongoing-etm---------------------------------------------------
knitr::kable(
    unsys_all_ongoing_etm |>
        group_by(target) |>
        summarise(
            n_subjects = n(),
            pct_systematic = round(mean(systematic, na.rm = TRUE) * 100, 1),
            .groups = "drop"
        ),
    caption = "Systematicity check by target (Ongoing ETM data)"
)

## -----------------------------------------------------------------------------
fit_one <- etm |>
    dplyr::filter(group %in% "E-Cigarettes" & id %in% 1) |>
    fit_cp_nls(
        equation = "exponentiated",
        return_all = TRUE
    )

summary(fit_one)

plot(fit_one, x_trans = "log10")

## ----fit-individual, results='hide'-------------------------------------------
fit_all <- etm |>
    group_by(id, group) |>
    nest() |>
    mutate(
        unsys = map(data, check_systematic_cp),
        fit = map(data, fit_cp_nls, equation = "exponentiated", return_all = TRUE),
        summary = map(fit, summary),
        plot = map(fit, plot, x_trans = "log10"),
        glance = map(fit, glance),
        tidy = map(fit, tidy)
    )

## ----show-individual-fits-----------------------------------------------------
# Show parameter estimates for first 3 subjects only
knitr::kable(
    fit_all |>
        slice(1:3) |>
        unnest(tidy) |>
        select(id, group, term, estimate, std.error),
    digits = 3,
    caption = "Example parameter estimates (first 3 subjects)"
)

# Show one example plot
fit_all$plot[[2]]

## ----fit-pooled, results='hide'-----------------------------------------------
fit_pooled <- etm |>
    group_by(group) |>
    nest() |>
    mutate(
        unsys = map(data, check_systematic_cp),
        fit = map(data, fit_cp_nls, equation = "exponentiated", return_all = TRUE),
        summary = map(fit, summary),
        plot = map(fit, plot, x_trans = "log10"),
        glance = map(fit, glance),
        tidy = map(fit, tidy)
    )

## ----show-pooled-results------------------------------------------------------
# Show tidy results instead of summary
knitr::kable(
    fit_pooled |>
        unnest(tidy) |>
        select(group, term, estimate, std.error),
    digits = 3,
    caption = "Pooled model parameter estimates by product group"
)

# Show one plot example
fit_pooled |>
    dplyr::filter(group == "E-Cigarettes") |>
    dplyr::pull(plot) |>
    pluck(1)

## ----fit-mean, results='hide'-------------------------------------------------
fit_mean <- etm |>
    group_by(group, x) |>
    summarise(
        y = mean(y),
        .groups = "drop"
    ) |>
    group_by(group) |>
    nest() |>
    mutate(
        unsys = map(data, check_systematic_cp),
        fit = map(data, fit_cp_nls, equation = "exponentiated", return_all = TRUE),
        summary = map(fit, summary),
        plot = map(fit, plot, x_trans = "log10"),
        glance = map(fit, glance),
        tidy = map(fit, tidy)
    )

## ----show-mean-results--------------------------------------------------------
# Show tidy results
knitr::kable(
    fit_mean |>
        unnest(tidy) |>
        select(group, term, estimate, std.error),
    digits = 3,
    caption = "Mean model parameter estimates by product group"
)

# Show parameter estimates plot
fit_mean |>
    unnest(cols = c(glance, tidy)) |>
    select(
        group,
        term,
        estimate
    ) |>
    ggplot(aes(x = group, y = estimate, group = term)) +
    geom_bar(stat = "identity") +
    geom_point() +
    facet_wrap(~term, ncol = 1, scales = "free_y")

# Show one example plot
fit_mean |>
    dplyr::filter(group %in% "E-Cigarettes") |>
    dplyr::pull(plot) |>
    pluck(1)

## -----------------------------------------------------------------------------
fit_one_linear <- etm |>
    dplyr::filter(group %in% "E-Cigarettes" & id %in% 1) |>
    fit_cp_linear(
        type = "fixed",
        log10x = TRUE,
        return_all = TRUE
    )

summary(fit_one_linear)
plot(fit_one_linear, x_trans = "log10")

## -----------------------------------------------------------------------------
fit_mixed <- fit_cp_linear(
    etm,
    type = "mixed",
    log10x = TRUE,
    group_effects = "interaction",
    random_slope = FALSE,
    return_all = TRUE
)

summary(fit_mixed)

# plot fixed effects only
plot(fit_mixed, x_trans = "log10", pred_type = "fixed")

# plot random effects only
plot(fit_mixed, x_trans = "log10", pred_type = "random")

# plot both fixed and random effects
plot(fit_mixed, x_trans = "log10", pred_type = "all")

## -----------------------------------------------------------------------------
glance(fit_one)
tidy(fit_one)

## -----------------------------------------------------------------------------
extract_coefficients(fit_mixed)

## -----------------------------------------------------------------------------
cp_posthoc_slopes(fit_mixed)
cp_posthoc_intercepts(fit_mixed)

