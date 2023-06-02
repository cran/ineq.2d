## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ineq.2d)

## -----------------------------------------------------------------------------
data(us16)
str(us16)

## -----------------------------------------------------------------------------
theil.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")

## -----------------------------------------------------------------------------
scv.2d(us16, "hitotal", "sex", "hitotal", "hpopwgt")

## -----------------------------------------------------------------------------
theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                   "hitransfer"), "hpopwgt")

## -----------------------------------------------------------------------------
scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                 "hitransfer"), "hpopwgt")

## -----------------------------------------------------------------------------
theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                   "hitransfer"), "hpopwgt", perc = TRUE)

scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                 "hitransfer"), "hpopwgt", perc = TRUE)

## -----------------------------------------------------------------------------
theil1 <- theil.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                             "hitransfer"), "hpopwgt")
sum(theil1[,-1])

scv1 <- scv.2d(us16, "hitotal", "sex", c("hilabour", "hicapital", 
                                         "hitransfer"), "hpopwgt")
sum(scv1[,-1])

## -----------------------------------------------------------------------------
theil.2d(us16, "hitotal", weights = "hpopwgt")

scv.2d(us16, "hitotal", weights = "hpopwgt")

## -----------------------------------------------------------------------------
us16$cohort <- 0
us16[us16$age < 25, "cohort"] <- "t24"
us16[us16$age >= 25 & us16$age < 50, "cohort"] <- "f25t49"
us16[us16$age >= 50 & us16$age < 75, "cohort"] <- "f50t74"
us16[us16$age >= 75, "cohort"] <- "f75"

## -----------------------------------------------------------------------------
theil.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital", 
                                      "hitransfer"), "hpopwgt")

scv.2d(us16, "hitotal", "cohort", c("hilabour", "hicapital", 
                                    "hitransfer"), "hpopwgt")

