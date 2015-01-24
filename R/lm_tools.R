#' A lookup for various regression methods
#'
#' This function allows you to lookup which package/method corresponds to a given regression method. Returns a data frame
#'  with the package and method that each method matches up with, as well as a short description of each method. 
#' @keywords reference
#' @export
#' @examples
#' lm_ref()

lm_ref <- function() {
  types <- c("GLS", "LAD", "Huber", "LTS", "PLS", "PCR", "Ridge")
  pkgs <- c("nlme", "quantreg", "MASS", "MASS", "pls", "pls", "MASS")  
  funcs <- c("gls", "rq", "rlm", "ltsreg", "plsr", "pcr", "lm.ridge")
  dscp <- c("errors can be correlated or have unequal variances",
            "quantile regression",
            "robust regression",
            "fit a model that avoids the outliers",
            "partial least squares", 
            "principal component analysis + regression",
            "ridge regression")
  
  ref <- data.frame(method = types, package = pkgs, func = funcs, descriptions = dscp)
  return (ref)
}

#' A wrapper for partial least squares regression. 
#'
#' Does partial least squares. 
#' @param lin_mod A linear model of class 'lm'
#' @param ncomp The number of components to consider in the PLS regression
#' @keywords pls
#' @export
#' @examples
#' my_pls(lm(resp ~ predictor), ncomp =2)

my_pls <- function(lin_mod, ncomp=5) {
  partial_ls <- pls::plsr(formula(lin_mod), data=lin_mod$model, validation="CV",ncomp=ncomp)
  pls::validationplot(partial_ls)
  return (partial_ls)
}

#' A wrapper for principal component regression
#'
#' Does PCR the easy way
#' @param lin_mod A linear model of class 'lm'
#' @keywords pcr
#' @export
#' @examples
#' my_pcr(lm(resp ~ predictor))

my_pcr <- function(lin_mod) {
  pcr_model <- pls::pcr(formula(lin_mod), data=lin_mod$model, validation="CV")
  pls::validationplot(pcr_model)
  return (pcr_model)
}

#' A wrapper for ridge regression
#'
#' Ridge regression made easy
#' @param lin_mod A linear model of class 'lm'
#' @param lam The lambda values to consider for ridge regression
#' @keywords ridge
#' @export
#' @examples
#' my_ridge(lm(resp ~ predictor))
my_ridge <- function(lin_mod, lam=seq(0, 100, .01)) {
  ridge <- MASS::lm.ridge(formula(lin_mod), data = lin_mod$model, lambda=lam)
  select(ridge)
  return (ridge)
}

#' Broken stick regression
#'
#' One class broken stick regression
#' @param response The response variable (a vector)
#' @param predictor The predictor variable (a vector)
#' @param split.point The point to split the broken stick regression
#' @keywords broken stick
#' @export
#' @examples
#' broken_stick(response, predictor, 50)
broken_stick <- function(response, predictor, split.point) {
  lhs <- function(x) ifelse(x < split.point, split.point -x, 0)
  rhs <- function(x) ifelse(x < split.point, 0, x - split.point)
  
  broken.stick <- lm(response ~ lhs(predictor) + rhs(predictor))
  return (data.frame(summary(broken.stick)))
}

#' Stepwise Regression wrapper
#'
#' This function allows you to perform stepwise regression in three different ways
#' @param mod The linear model of class 'lm'
#' @param type The type of stepwise regression. Default is AIC but other options include "Adj-R" for adjusted 
#'  r-squared, and "Mallows" for mallows critical point 
#' @keywords stepwise
#' @export
#' @examples
#' stepwise(lm(resp~pred, data), type = "Adj-R")
stepwise <- function(mod, type = "AIC") {
  adj.r <- function(form) summary(leaps::regsubsets(form, data = mod$model))$adjr2
  mallow <- function(form) summary(leaps::regsubsets(form, data = mod$model))$cp
  switch(type,
         AIC = return(step(model)),
         AdjR = return(adj.r(formula(mod))),
         Mallows = return(mallow(formula(mod)))
        )
}

