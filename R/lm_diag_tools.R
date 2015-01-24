#' Linear model normality diagnostic
#'
#' Computes model diagnostics for a linear model, including shapiro-wilkes, 
#' influential observations, outliers, and large leverage points
#' @param model A linear model of class 'lm'
#' @keywords normality, diagnostics
#' @export
#' @examples
#' normality_check(lm(resp ~ predictor))

normality_check <- function(model) {
  # make a qq-plot of the residuals
  # qqnorm(residuals(model), main = "QQ Plot")
  # qqline(residuals(model))
  
  # shapiro-wilkes test for normality 
  shap_val <- shapiro.test(residuals(model))
  
  # check for large leverage points. 
  lvrg_points <- influence(model)
  
  # check for outliers - get the jackknife residuals for the data
  outliers <- rstudent(model)
  max_outlier <- outliers[which(abs(outliers) == max(abs(outliers)))]
  
  # check for influential observations (removal of these would cause the biggest change in fit)
  cooks <- cooks.distance(model)
  
  output <- list()
  output$outliers <- outliers
  output$influential <- cooks
  output$shapiro <- shap_val
  output$large_lvrg <- lvrg_points
  
  return (output)
}

#' Diagnostics of errors
#'
#' Checks the errors/residuals with the Durbin Watts statistic (serial correlation), variation inflation factors and condition numbers
#' @param model A linear model to diagnose, of class 'lm'
#' @keywords errors, diagnostics
#' @export
#' @examples
#' error_check(lm(resp ~ pred, data))
#' 
error_check <- function(model) {
  # durbin-watts statistics to check for serial correlation
  dw_stat <- lmtest::dwtest(model)
  
  # find variance inflation factors 
  vifs <- faraway::vif(model)
  
  # condition numbers
  conds <- model.matrix(model)[,-1]
  cond_nums <- eigen(t(conds) %*% conds)
  
  output <- list()
  output$dw <- dw_stat
  output$vifs <- vifs
  output$conds <- cond_nums
  
  return (output)
  
}