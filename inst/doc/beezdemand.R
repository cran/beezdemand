## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, comment=NA)

## ----cran-install, eval = FALSE-----------------------------------------------
#  install.packages("beezdemand")
#  
#  library(beezdemand)

## ----git-install, eval = FALSE------------------------------------------------
#  install.packages("devtools")
#  
#  devtools::install_github("brentkaplan/beezdemand", build_vignettes = TRUE)
#  
#  library(beezdemand)

## ----gitdev-install, eval = FALSE---------------------------------------------
#  devtools::install_github("brentkaplan/beezdemand@develop")

## ----packages, include = FALSE, echo = FALSE----------------------------------
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("beezdemand", quietly = TRUE)) {
  install.packages("beezdemand")
}
library(beezdemand)

## ----example-data-set, echo=FALSE, results='asis'-----------------------------
knitr::kable(apt[c(1:10, 17:26), ])

## ----example-wide-------------------------------------------------------------
## the following code takes the apt data, which are in long format, and converts
## to a wide format that might be seen from data collection software
wide <- spread(apt, x, y)
colnames(wide) <- c("id", paste0("price_", seq(1, 16, by = 1)))
knitr::kable(wide[1:5, 1:10])

## ----example-wide1------------------------------------------------------------
## make an object to hold what will be the new column names
newcolnames <- c("id", "0", "0.5", "1", "1.50", "2", "2.50", "3", 
                 "4", "5", "6", "7", "8", "9", "10", "15", "20")
## current column names
colnames(wide)

## replace current column names with new column names
colnames(wide) <- newcolnames

## how new data look (first 5 rows only)
knitr::kable(wide[1:5, ])

## ----example-w2l--------------------------------------------------------------
## using the dataframe 'wide', we specify the key will be 'price', the values 
## will be 'consumption', and we will select all columns besides the first ('id')
long <- tidyr::gather(wide, price, consumption, -id)

## we'll sort the rows by id
long <- arrange(long, id)

## view the first 20 rows
knitr::kable(long[1:20, ])

## ----example-w2l2-------------------------------------------------------------
colnames(long) <- c("id", "x", "y")

long$x <- as.numeric(long$x)
long$y <- as.numeric(long$y)
knitr::kable(head(long))

## ----descriptive, eval = FALSE------------------------------------------------
#  GetDescriptives(dat = apt, bwplot = FALSE, outdir = "../plots/", device = "png",
#                  filename = "bwplot")

## ----descriptive-output, echo=FALSE, results='asis'---------------------------
descr <- GetDescriptives(apt)
knitr::kable(descr)

## ----change-data, eval = FALSE------------------------------------------------
#  ChangeData(apt, nrepl = 1, replnum = 0.01, rem0 = FALSE, remq0e = FALSE, replfree = NULL)

## ----unsystematic, eval=FALSE-------------------------------------------------
#  CheckUnsystematic(dat = apt, deltaq = 0.025, bounce = 0.1, reversals = 0, ncons0 = 2)

## ----unsystematic-output, echo=FALSE, results='asis'--------------------------
knitr::kable(head(CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.1, 
                                    reversals = 0, ncons0 = 2), 5))

## ----empirical, eval=FALSE----------------------------------------------------
#  GetEmpirical(apt)

## ----empirical-output, echo=FALSE, results='asis'-----------------------------
knitr::kable(head(GetEmpirical(apt), 5))

## ----zero-warning, eval=FALSE-------------------------------------------------
#  Warning message:
#  Zeros found in data not compatible with equation! Dropping zeros!

## ----hs, eval=FALSE-----------------------------------------------------------
#  FitCurves(dat = apt, equation = "hs", agg = NULL, detailed = FALSE,
#            xcol = "x", ycol = "y", idcol = "id", groupcol = NULL)

## ----hs2, eval=FALSE----------------------------------------------------------
#  FitCurves(dat = apt, equation = "hs")

## ----hs-setup, include=FALSE--------------------------------------------------
fc <- FitCurves(dat = apt, equation = "hs")
hs1 <- head(fc, 5)[ , 1:6]
hs2 <- head(fc, 5)[ , 7:11]
hs3 <- head(fc, 5)[ , 12:20]
hs4 <- head(fc, 5)[ , 21:24]

## ----hs-output, echo=FALSE, results='asis'------------------------------------
knitr::kable(hs1, caption = "Empirical Measures")
knitr::kable(hs2, caption = "Fitted Measures")
knitr::kable(hs3, caption = "Uncertainty and Model Information")
knitr::kable(hs4, caption = "Derived Measures")

## ----koff, eval=FALSE---------------------------------------------------------
#  FitCurves(dat = apt, equation = "koff")

## ----koff-setup, include = FALSE----------------------------------------------
fc <- FitCurves(dat = apt, equation = "koff")
koff1 <- head(fc, 5)[ , 1:6]
koff2 <- head(fc, 5)[ , 7:11]
koff3 <- head(fc, 5)[ , 12:20]
koff4 <- head(fc, 5)[ , 21:24]

## ----koff-output, echo=FALSE, results='asis'----------------------------------
knitr::kable(koff1, caption = "Empirical Measures")
knitr::kable(koff2, caption = "Fitted Measures")
knitr::kable(koff3, caption = "Uncertainty and Model Information")
knitr::kable(koff4, caption = "Derived Measures")

## ----agg-mean, eval = FALSE---------------------------------------------------
#  FitCurves(dat = apt, equation = "hs", agg = "Mean")

## ----agg-mean-setup, include = FALSE------------------------------------------
mn <- FitCurves(dat = apt, equation = "hs", agg = "Mean")
mn1 <- head(mn)[ , 1:6]
mn2 <- head(mn)[ , 7:11]
mn3 <- head(mn)[ , 12:20]
mn4 <- head(mn)[ , 21:24]

## ----agg-mean-output, echo = FALSE, results = 'asis'--------------------------
knitr::kable(mn1, caption = "Empirical Measures")
knitr::kable(mn2, caption = "Fitted Measures")
knitr::kable(mn3, caption = "Uncertainty and Model Information")
knitr::kable(mn4, caption = "Derived Measures")

## ----agg-pooled, eval = FALSE-------------------------------------------------
#  FitCurves(dat = apt, equation = "hs", agg = "Pooled")

## ----agg-pooled-setup, include = FALSE----------------------------------------
pl <- FitCurves(dat = apt, equation = "hs", agg = "Pooled")
pl1 <- head(pl)[ , 1:6]
pl2 <- head(pl)[ , 7:11]
pl3 <- head(pl)[ , 12:20]
pl4 <- head(pl)[ , 21:24]

## ----agg-pooled-output, echo = FALSE, results = 'asis'------------------------
knitr::kable(pl1, caption = "Empirical Measures")
knitr::kable(pl2, caption = "Fitted Measures")
knitr::kable(pl3, caption = "Uncertainty and Model Information")
knitr::kable(pl4, caption = "Derived Measures")

## ----share, eval=FALSE--------------------------------------------------------
#  FitCurves(dat = apt, equation = "hs", k = "share")

## ---- include=FALSE-----------------------------------------------------------
df <- FitCurves(dat = apt, equation = "hs", k = "share")

## ----share-output, echo=FALSE, results='asis'---------------------------------
knitr::kable(head(df, 5)[ , 1:6], caption = "Empirical Measures")
knitr::kable(head(df, 5)[ , 7:11], caption = "Fitted Measures")
knitr::kable(head(df, 5)[ , 12:20], caption = "Uncertainty and Model Information")
knitr::kable(head(df, 5)[ , 21:24], caption = "Derived Measures")

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
  stat_summary(fun.y = mean, aes(fill = group), geom = "point", shape = 21, 
               color = "black", stroke = .75, size = 4) +
  scale_x_log10(limits = c(.4, 50), breaks = c(.1, 1, 10, 100)) +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  labs(x = "Price per Drink", y = "Drinks Purchased") +
  theme(legend.position = c(.85, .75)) +
  ## theme_apa is a beezdemand function used to change the theme in accordance
  ## with American Psychological Association style
  theme_apa()

## ----plots1, eval = FALSE-----------------------------------------------------
#  out <- FitCurves(dat = apt, equation = "hs", k = "share", detailed = T)
#  
#  PlotCurves(dat = out, outdir = "../plots/", device = "png", ask = F)

## ----plots2, eval = FALSE-----------------------------------------------------
#  mn <- FitCurves(dat = apt, equation = "hs", agg = "Mean", detailed = T)
#  
#  PlotCurves(dat = mn, outdir = "../plots/", device = "png", ask = F)

## ----learn, eval=FALSE--------------------------------------------------------
#  ?CheckUnsystematic

## ----learn-output, eval=FALSE-------------------------------------------------
#  CheckUnsystematic          package:beezdemand          R Documentation
#  
#  Systematic Purchase Task Data Checker
#  
#  Description:
#  
#       Applies Stein, Koffarnus, Snider, Quisenberry, & Bickels (2015)
#       criteria for identification of nonsystematic purchase task data.
#  
#  Usage:
#  
#       CheckUnsystematic(dat, deltaq = 0.025, bounce = 0.1, reversals = 0,
#         ncons0 = 2)
#  
#  Arguments:
#  
#       dat: Dataframe in long form. Colums are id, x, y.
#  
#    deltaq: Numeric vector of length equal to one. The criterion by which
#            the relative change in quantity purchased will be compared.
#            Relative changes in quantity purchased below this criterion
#            will be flagged. Default value is 0.025.
#  
#    bounce: Numeric vector of length equal to one. The criterion by which
#            the number of price-to-price increases in consumption that
#            exceed 25% of initial consumption at the lowest price,
#            expressed relative to the total number of price increments,
#            will be compared. The relative number of price-to-price
#            increases above this criterion will be flagged. Default value
#            is 0.10.
#  
#  reversals: Numeric vector of length equal to one. The criterion by
#            which the number of reversals from number of consecutive (see
#            ncons0) 0s will be compared. Number of reversals above this
#            criterion will be flagged. Default value is 0.
#  
#    ncons0: Numer of consecutive 0s prior to a positive value is used to
#            flag for a reversal. Value can be either 1 (relatively more
#            conservative) or 2 (default; as recommended by Stein et al.,
#            (2015).
#  
#  Details:
#  
#       This function applies the 3 criteria proposed by Stein et al.,
#       (2015) for identification of nonsystematic purchase task data. The
#       three criteria include trend (deltaq), bounce, and reversals from
#       0. Also reports number of positive consumption values.
#  
#  Value:
#  
#       Dataframe
#  
#  Author(s):
#  
#       Brent Kaplan <bkaplan.ku@gmail.com>
#  
#  Examples:
#  
#       ## Using all default values
#       CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 2)
#       ## Specifying just 1 zero to flag as reversal
#       CheckUnsystematic(apt, deltaq = 0.025, bounce = 0.10, reversals = 0, ncons0 = 1)

