##
## Copyright 2016 Brent Kaplan
##
## This file is part of beezdemand.
##
## beezdemand is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## beezdemand is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with beezdemand.  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.
##
## summary
## R script for plotting demand functions
##
##

utils::globalVariables(c("X", "Y", "group", "id", "x", "x1", "x2", "y", "y1", "y2"))

# Creates minor ticks for use in plotting.
# maj Major values
##' @noRd
minTicks <- function(maj) {
  minticks <- vector(length = (length(maj)-1) * 10)
  for (i in 1:length(maj)) {
    if (i == length(maj)) {
      return(minticks)
    }
    if (i == 1) {
      minticks <- seq(maj[i], maj[i + 1], length.out = 10)
    } else {
      minticks <- c(minticks, seq(maj[i], maj[i + 1], length.out = 10))
    }
  }
}

##' Creates annotation layer
##'
##' Inherit and extend layer for use in ggplot draw
##' @title annotation_logticks2
##' @param base base for drawing in scale
##' @param sides sides to draw, by default bottom and left
##' @param scaled true by default
##' @param short short tick settings
##' @param mid mid tick settings
##' @param long long tick settings
##' @param colour default to black colour
##' @param size size for labels
##' @param linetype default linetype
##' @param alpha default alpha level
##' @param data data to include
##' @param color colors to include
##' @param ... additional arguments
##' @return ggplot2 layer
##' @author Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @import ggplot2
annotation_logticks2 <- function(base = 10, sides = "bl", scaled = TRUE, short = unit(0.1, "cm"),
                                 mid = unit(0.2, "cm"), long = unit(0.3, "cm"), colour = "black",
                                 size = 0.5, linetype = 1, alpha = 1, data =data.frame(x = NA), color = NULL, ...) {
  if (!is.null(color)) {
    colour <- color
  }

  layer(
    data = data,
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomLogticks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      scaled = scaled,
      short = short,
      mid = mid,
      long = long,
      colour = colour,
      size = size,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}

##' Creates plots
##'
##' Creates and saves plots of individual demand curves
##' @title Plot Curves
##' @param dat FitCurves object with 4 elements (dfres, newdats, adfs, fits)
##' @param outdir Directory where plots are saved
##' @param device Type of file. Default is "png". Can be "pdf"
##' @param ending Optional. Can specify to only plot through a certain number of datasets
##' @param ask Can view plots one by one. If TRUE, plots will not save
##' @param ... Pass arguments to PlotCurve (for example yscale = c("log", "linear"))
##' @return Nothing
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>, Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @examples 
##' ## Interactively view plots from output from FitCurves
##' \donttest{
##' fc <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
##' PlotCurves(fc, ask = TRUE)}
##' @export
PlotCurves <- function(dat, outdir = NULL, device = "png", ending = NULL, ask = T, ...) {
  
  if (!all(c("dfres", "newdats", "adfs") %in% names(dat))) {
    stop("Object should be from FitCurves. Try rerunning FitCurves with detailed = TRUE")
  }
  
  if (!ask) {
    outdir <- if (is.null(outdir)) paste0(tempdir(), "/") else outdir
    if (!dir.exists(outdir)){
      dir.create(outdir)
    }
  }

  if (is.null(ending)) {
    ending <- length(dat$fits)
  }

  par(ask = ask)
  
  for (i in 1:ending) {
    ggp <- PlotCurve(dat$adfs[[i]], dat$dfres[i, ], dat$newdats[[i]], ...)
    if (!class(ggp)[[1]] == "character") {
      if (ask) {
        suppressWarnings(print(ggp))
      } else {
        if (device == "png") {
          png(paste0(outdir, "Participant-", dat$dfres[i, "id"], ".png"), width = 800, height = 800, res = 120)
          suppressWarnings(print(ggp))
          graphics.off()
        } else if (device == "pdf") {
          pdf(paste0(outdir, "Participant-", dat$dfres[i, "id"], ".pdf"))
          suppressWarnings(print(ggp))
          graphics.off()
        }
      }
    } else {
      next()
    }
  }
  message(ending, " plots saved in ", outdir)
}

##' Creates a single plot object
##'
##' Creates individual demand curves
##' @title Plot Curve
##' @param adf Data frame (long form) of purchase task data.
##' @param dfrow A row of results from FitCurves
##' @param newdats A newdat dataframe from FitCurves
##' @param yscale Scaling of y axis. Default is "log". Can also take "linear"
##' @return ggplot2 graphical object
##' @author Shawn Gilroy <shawn.gilroy@@temple.edu>
##' @import ggplot2
##' @examples 
##' ## Creates a single plot from elements of an object created by FitCurves
##' \donttest{
##' fc <- FitCurves(apt, "hs", k = 2, detailed = TRUE)
##' PlotCurve(fc$adfs[[1]], fc$dfres[1, ], fc$newdats[[1]])
##' }
##' @export
PlotCurve <- function(adf, dfrow, newdats, yscale = "log") {
  if (!any(adf$y > 0)) {
   return(print("Warning: No positive consumption values!"))
  }
  
  if (!all(is.na(newdats$y))) {
    segmentFrame <- data.frame(x1 = c(0),
                               x2 = c(0),
                               y1 = c(0),
                               y2 = c(0))

    segmentFrame[1, "x1"] <- dfrow[["Pmaxd"]]
    segmentFrame[1, "x2"] <- dfrow[["Pmaxd"]]
    segmentFrame[1, "y1"] <- 0

    lowPrice <- 0.001

    # Lengthen out the curve domain
    highPrice <- max(adf$x) * 2

   if (dfrow[["Equation"]] == "hs") {
      segmentFrame[1, "y2"] <- 10^((log(dfrow[["Q0d"]])/log(10)) + dfrow[["K"]] * (exp(-dfrow[["Alpha"]] * dfrow[["Q0d"]] * dfrow[["Pmaxd"]]) - 1))
    } else if (dfrow[["Equation"]] == "koff") {
      segmentFrame[1, "y2"] <- dfrow[["Q0d"]] * 10^(dfrow[["K"]] * (exp(-dfrow[["Alpha"]] * dfrow[["Q0d"]] * dfrow[["Pmaxd"]]) - 1))
    }
    tempnew <- newdats
    pointFrame <- data.frame(X=adf$x, Y=adf$y)

    if (0 %in% pointFrame$X) {
      # Split axes are warranted here
      
      pointFrame$mask <- 1
      tempnew$mask <- 1

      pointFrame[pointFrame$X == 0,]$mask <- 0
      pointFrame[pointFrame$X == 0,]$X <- 0.0001

      segmentFrame$mask <- 1

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_line(data=tempnew, aes(x=x, y=y)) +
        ggplot2::geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), show.legend = F, data = segmentFrame, linetype=2) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::facet_grid(.~mask, scales="free_x", space="free_x") +
        ggplot2::scale_x_log10(breaks=c(0.0001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c("0.00",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(min(c(0.1, tempnew$y)), max(c(tempnew$y, pointFrame$y)) * 1.15)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["id"]], sep = "-")) +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
    
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0))
      } 
       
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
                axis.ticks.length = unit(-0.15, "cm"),
                axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
                axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
    
    } else {
      # Regular representation

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_line(data=tempnew, aes(x=x, y=y)) +
        ggplot2::geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), show.legend = F, data = segmentFrame, linetype=2) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::scale_x_log10(breaks=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.00001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(min(c(0.1, tempnew$y)), max(c(tempnew$y, pointFrame$y)) * 1.15)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["id"]], sep = "-")) +
        annotation_logticks(sides = "b") +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          ggplot2::annotation_logticks(sides="l")
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
    
    }
    plt
    

  } else {
    # fitting failed in these instances

    pointFrame <- data.frame(X=adf$x, Y=adf$y)

    if (0 %in% pointFrame$X) {
      # Split axes are warranted
      pointFrame$mask <- 1

      pointFrame[pointFrame$X == 0,]$mask <- 0
      pointFrame[pointFrame$X == 0,]$X <- 0.0001

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::geom_blank(data = data.frame(X=0.001,
                                     Y=0.001,
                                     mask=1)) +
        ggplot2::geom_blank(data = data.frame(X=max(adf$x)*2,
                                     Y=max(adf$y),
                                     mask=1)) +
        ggplot2::facet_grid(.~mask, scales="free_x", space="free_x") +
        ggplot2::scale_x_log10(breaks=c(0.0001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c("0.00",  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(0.1, max(pointFrame$Y) * 1.15)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["id"]], sep = "-")) +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        annotation_logticks2(sides="b", data = data.frame(X= NA, mask = 1)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          annotation_logticks2(sides="l", data = data.frame(X= NA, mask = 0))
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))
 
    } else {
      # Regular representation

      plt <- ggplot2::ggplot(pointFrame,aes(x=X,y=Y)) +
        ggplot2::geom_point(size=3, shape=21, show.legend=T, colour = "black", fill = "white", alpha = .9, stroke = 1) +
        ggplot2::geom_blank(data = data.frame(X=0.001,
                                     Y=0.001)) +
        ggplot2::geom_blank(data = data.frame(X=max(adf$x)*2,
                                     Y=max(adf$y))) +
        ggplot2::scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::coord_cartesian(ylim=c(0.1, max(pointFrame$Y) * 1.15)) +
        ggplot2::ggtitle(paste("Participant", dfrow[["id"]], sep = "-")) +
        ggplot2::annotation_logticks(sides = "b") +
        beezdemand::theme_apa() +
        ggplot2::theme(strip.background = element_blank(),
              strip.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size=16)) +
        ggplot2::labs(x = "Price", y = "Reported Consumption")
      
      if (yscale == "log") {
        plt <- plt +
          ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                        labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
          ggplot2::annotation_logticks(sides="l")
      } 
      
      plt <- plt +
        ggplot2::theme(axis.text.x = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.text.y = element_text(size=12, margin = unit(c(0.3,0.3,0.3,0.3), "cm")),
              axis.ticks.length = unit(-0.15, "cm"),
              axis.title.x = element_text(face = "bold", margin = unit(c(-0.1, 0, 0, 0), "cm")),
              axis.title.y = element_text(face = "bold", margin = unit(c(0, -0.1, 0, 0), "cm")))

    }

    plt
  }
}


##' APA theme for ggplot
##'
##' Theme for ggplot graphics that closely align with APA formatting
##' @title APA Theme
##' @param plot.box Boolean for a box around the plot
##' @return ggplot theme
##' @author Brent Kaplan <bkaplan.ku@@gmail.com>
##' @examples 
##' \donttest{
##' p <- ggplot2::ggplot(apt, ggplot2::aes(x = x, y = y)) +
##'   ggplot2::geom_point() 
##' p + theme_apa()
##' }
##' @export
theme_apa <- function(plot.box = FALSE){
    helv <- "Helvetica"

    out <- theme(
        plot.title = element_text(family = helv, size = 14, face = "bold", colour = "black"),
        axis.title.x = element_text(family = helv, size = 14, colour = "black"),
        axis.title.y = element_text(family = helv, size = 14, angle = 90, colour = "black"),
        axis.text.x = element_text(family = helv, size = 11, colour = "black"),
        axis.text.y = element_text(family = helv, size = 11, colour = "black"),
        axis.ticks = element_line(colour="black"))

    if (plot.box) {
        out <- out + theme(panel.background = element_rect(fill = "white",
                colour = "black"), panel.border = element_rect(fill = NA,
                colour = "white"), axis.line = element_line())
    } else {
        out <- out + theme(panel.background = element_blank(),
                           panel.border = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(colour = "black"))
    }
    out
}
