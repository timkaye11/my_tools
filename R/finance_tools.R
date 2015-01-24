#' A R function that calls the Yahoo Finance API to get data into a data frame
#'
#' This function allows you to specify a stock ticker and date ranges such that the data is grabbed from the
#' yahoo finance API. The output is a data.frame of all of the information returned. 
#'
#' @param ticker The stock ticker for a given company
#' @param date.from The start date for the stock data (year month date format)
#' @param date.to The end date for the stock date (year month date format)
#' @keywords finance
#' @export
#' @examples
#' data <- yahoo_finance("goog")
#' plot.ts(data$Adj.Close)
#' 
yahoo_finance <- function(ticker, date.from="2015-01-01", data.to=Sys.Date()) {
  ticker <- toupper(ticker)
  base <- "http://ichart.yahoo.com/table.csv?s="
  
  # to handle multiple tickers, if ticker is a vector
  request_link <- paste(base, paste(ticker, collapse=","), "&", sep="")
  
  # format the dates according to the yahoo finance API
  date.from <- lubridate::ymd(date.from)
  date.to <- lubridate::ymd(date.to)
  from <- c(month(date.from)-1, day(date.from), year(date.from))
  to <- c(month(date.to)-1, day(date.to), year(date.to))
  dates <- paste(letters[1:6], c(from,to), sep="=", collapse="&")
  
  # link for the request
  link <- paste(request_link, dates, sep="")
  
  output <- httr::GET(link)
  stopifnot(output$status_code != 200)
  csv <- read.csv(textConnection(content(output, "text")))
  
  return (csv)
}

#' A R function that calculates the discrete continuous growth rate of a stock
#'
#' This function allows you to specify a stock ticker and date ranges such that the data is grabbed from the
#' yahoo finance API. The output consists of the mean dcgr and the a vector of the logged differences
#'
#' @param ticker The stock ticker for a given company
#' @param from The start date for the stock data (year month date format)
#' @param to The end date for the stock date (year month date format)
#' @keywords finance, dcgr
#' @export
#' @examples
#' dcgr <- calculate_dcgr("goog")
#' 
calculate_dcgr <- function(ticker,  from=(Sys.Date()-365), to=Sys.Date()) {
  data <- yahoo_finance(ticker, from, to)
  adj <- data$Adj.Close
  # discrete continuous growth rate (dcgr)
  dcgr <- rep(0, nrow(data))
  dcgr[1] <- 0
  for (i in 2:length(adj)) {
    dcgr[i] <- log(adj[i]) - log(adj[i-1])
  }
  output <- list()
  output$mean_dcgr <- mean(dcgr)
  output$dcgr <- dcgr
  return (output)
}

#' A R function that calls calculates the Implied Daily Volatility (IDV) of a stock
#'
#' This function calculates the IDV using the Black-Scholes method. The user must specify the strike
#' price, the stock price, the interest rate, time until execution, option price (call or put), and the type 
#' of option (call or put)
#'
#' @param strike The strike price of astock
#' @param stock_price The stock price of the stock
#' @param interest The interest rate 
#' @param time The time until execution of the option
#' @param option_price The price of the option
#' @keywords finance, idv
#' @export
#' @examples
#' call_idv <- calculate_idv(113, 111.5, 0.02, 17, 3.50, "call")
#' 
calculate_idv <- function(strike, stock_price, interest, time, option_price, type="call") {
  switch(type,
         call=.idv_call(strike, stock_price, interest, time, option_price),
         put =.idv_put(strike, stock_price, interest, time, option_price))
}

# helper function for the calculate IDV function above (only for calls)
.idv_call <- function(strike_price, stock_price, interest, time, call_price) {
  # now do some black-scholes stuff
  S <- stock_price
  K <- strike
  r <- interest
  t <- time
  
  daily_vol <- 0
  temp_call_price <- 0
  call <- call_price
  while (temp_call_price <= call) {
    daily_vol <- daily_vol + 0.000001
    denom1 <- ((log(S) - log(K)) + ((r / 365) + (daily_vol^2/2))*t)
    denom2 <- ((log(S) - log(K)) + ((r / 365) - (daily_vol^2/2))*t)
    dur_vol <- daily_vol * sqrt(t)
    dnd1 <- pnorm(denom1/dur_vol)
    dnd1 <- pnorm(denom2/dur_vol)
    ringer <- exp(-r*t/365)
    temp_call_price <- S * dnd1 - K*ringer*dnd2
  }
  output <- list()
  output$time_decay <- (S*dnd1*daily_vol) / (2*sqrt(t)) - (-r*K*exp(-r*t) * dnd2)
  output$daily_vol <- daily_vol
  return (output)
}

# helper function for the IDV calculator above (only for puts)
.idv_put <- function(strike_price, stock_price, interest, time, put_price) {
  S <- stock_price
  K <- strike_price
  r <- interest
  t <- time
  
  daily_vol <- 0
  temp_put_price <- 0
  put <- put_price
  while (temp_put_price <= put) {
    daily_vol <- daily_vol + 0.00001
    denom <- ((log(S) - log(K)) + ((-r/365) + (daily_vol^2/2))*t)
    denom2 <- ((log(S) - log(K)) + ((-r/365) - (daily_vol^2/2))*t)
    durvol <- daily_vol * sqrt(t)
    dnd1 <- pnorm(-(denom)/durvol)
    dnd2 <- pnorm(-(denom2)/ durvol)
    ringer <- exp(-r * t/365)
    temp_put_price <- -(S * dnd1 - K*ringer*dnd2)
  }
  output <- list()
  output$time_decay <- (S*dnd1*daily_vol) / (2*sqrt(t)) - (-r*K*exp(-r*t) * dnd2)
  output$daily_vol <- daily_vol
  return (output)
}

#' A R function that calculates a bunch of relevant portfolio statistics, given an array of stock tickers
#'
#' This function calculates portfolio variance & volatility, portfolio and annualized alpha, 
#' unweighted variance/volatility, and the correlation matrix. 
#' 
#'
#' @param tickers A vector of stock tickers (the portfolio). 
#' @param ratios The ratios of the stocks in the portfolio. A vcetor
#' @keywords finance, portfolio, variance
#' @export
#' @examples
#' my_portfolio <- portfolio_analysis(tickers=c("SPY", "AAPL", "GOOG", "GLD"), 
#'                                    ratios=c(0.5,0.2,0.2,0.1))
#' 
portfolio_analysis <- function(tickers, ratios=rep(1/length(tickers), length(tickers))) {
  # get all the stock prices from yahoo finance
  portfolio <- NULL
  for (ticker in tickers) {
    portfolio <- cbind(portfolio, yahoo_finance(ticker)$Adj.Close)
  }
  portfolio_df <- as.data.frame(portfolio)
  colnames(portfolio_df) <- tickers
  
  # correlation, covariance matrix
  portfolio_correlation <- as.data.frame(cor(portfolio_df))
  portfolio_covariance <- as.data.frame(cov(portfolio_df))
  
  # evaluate the dcgr for each of the stocks in the portflio  
  dcgr <- function(x) { 
    dd <- rep(0, length(x))
    for (i in 2:length(x)) {
      dd[i] <- log(x[i]) - log(x[i-1])
    }
    return ((dd))
  }
  dcgrs <- apply(portfolio, 2, dcgr)
  mean_dcgrs <- apply(dcgrs, 2, mean)
  var_dcgrs <- apply(dcgrs, 2, var)
  
  # put all relevant information into the output
  output <- list()
  output$cor_matrix <- porfolio_correlation
  output$portfolio_alpha <- mean_dcgrs * ratios
  output$annualized_alpha <- output$portfolio_alpha * 251
  output$unweighted_variance <- sum(mean_dcgrs) + sum(porfolio_covariance)
  output$unweighted_volatility <- sqrt(output$unweighted_variance)
  output$portfolio_variance <- sum((1/length(tickers)^2 * var_dcgrs))
  output$portfolio_volatility <- sqrt(output$portfolio_variance)
  return (output)   
}