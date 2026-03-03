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

## ----packages, include = FALSE, echo = FALSE----------------------------------
library(dplyr)
library(ggplot2)
library(beezdemand)

## ----ftest--------------------------------------------------------------------
## setting the seed initializes the random number generator so results will be
## reproducible
set.seed(1234)

## manufacture random grouping
apt$group <- NA
apt[apt$id %in% sample(unique(apt$id), length(unique(apt$id))/2), "group"] <- "a"
apt$group[is.na(apt$group)] <- "b"

## take a look at what the new groupings look like in long form
knitr::kable(apt[1:20, ])

## ----ftest2-------------------------------------------------------------------
## in order for this to run, you will have had to run the code immediately
## preceeding (i.e., the code to generate the groups)
ef <- ExtraF(dat = apt, equation = "koff", k = 2, groupcol = "group", verbose = TRUE)

## ----ftest-ouput, results = 'asis', echo=FALSE--------------------------------
knitr::kable(ef$dfres[, 1:5], caption  = "Fitted Measures")
knitr::kable(ef$dfres[, c(1, 6:8)], caption = "Uncertainty and Model Information")
knitr::kable(ef$dfres[, c(1, 9:11)], caption = "Derived Measures")
knitr::kable(ef$dfres[, c(1, 12, 14)], caption = "Convergence and Summary Information")

## ----plot-ftest, warning = FALSE----------------------------------------------
## be sure that you've loaded the tidyverse package (e.g., library(tidyverse))
ggplot(apt, aes(x = x, y = y, group = group)) +
  ## the predicted lines from the sum of squares f-test can be used in subsequent
  ## plots by calling data = ef$newdat
  geom_line(aes(x = x, y = y, group = group, color = group),
            data = ef$newdat[ef$newdat$x >= .1, ]) +
  stat_summary(fun.data = mean_se, aes(width = .05, color = group),
               geom = "errorbar") +
  stat_summary(fun = mean, aes(fill = group), geom = "point", shape = 21,
               color = "black", stroke = .75, size = 4) +
  scale_x_log10(limits = c(.4, 50), breaks = c(.1, 1, 10, 100)) +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  labs(x = "Price per Drink", y = "Drinks Purchased") +
  theme(legend.position = c(.85, .75)) +
  ## theme_apa is a beezdemand function used to change the theme in accordance
  ## with American Psychological Association style
  theme_apa()

