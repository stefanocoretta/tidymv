## ----setup, echo=FALSE, include=FALSE------------------------------------
knitr::opts_chunk$set(out.width = "300px", fig.align = "center", dpi = 300)
library(tidyverse)
theme_set(theme_bw())
library(itsadug)
library(tidymv)

## ----gam-----------------------------------------------------------------
set.seed(10)
data <- gamSim(4)
model <- gam(
    y ~
        fac +
        s(x2, by = fac) +
        s(x0),
    data = data
)

## ----plot-1-2------------------------------------------------------------
plot_gamsd(
    model = model,
    view = "x2",
    comparison = list(fac = c("1", "2"))
)

## ----plot-1-3------------------------------------------------------------
plot_gamsd(
    model = model,
    view = "x2",
    comparison = list(fac = c("1", "3")),
    bw = TRUE
)

