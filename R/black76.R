#' black76
#'
#' \code{black76} calculates the premium of European option on future based on black
#' model, Black(1976).
#'
#' @param Ft Future price. Numeric object.
#' @param K Strike price. Numeric object.
#' @param type Type of the European option, call(C) or put(P). String object.
#' @param Time Time to maturity (in year). Numeric object.
#' @param r Continousely compounded interest rate. Numeric object.
#' @param sigma Annualsed volatility, square root of variance. Numeric object.
#'
#' @return List of 2 numeric objects, Option premium and Vega.
#' @export
#'
#' @examples
#' black76(type = "C", Ft = 48.03, Time = 0.1423, K = 50, sigma = 0.2, r = 0.03)
#' black76(type = "P", Ft = 48.03, Time = 0.1423, K = 50, sigma = 0.2, r = 0.03)
black76 <- function(Ft, K, type = c("C", "P"), Time, r, sigma){

  type <- match.arg(type)

  d1 <- (log(Ft / K) + (sigma^2/2) * Time)/(sigma * sqrt(Time))
  d2 <- d1 - sigma * sqrt(Time)

  if (type == "C") {
    optionPrice <- exp(-r * Time) * (Ft * pnorm(d1) - K * pnorm(d2))
    vega <- Ft * exp(-r * Time) * dnorm(d1) * sqrt(Time)
  } else if (type == "P") {
    optionPrice <- exp(-r * Time)*(K * pnorm(-d2) - Ft * pnorm(-d1))
    vega <- Ft * exp(-r * Time) * dnorm(d1) * sqrt(Time)
  } else {
    print("Please provide the type of option: call(c) or put(p)")
  }
  # return(list(optionPrice = optionPrice, vega = vega))
  return(c('optionPrice' = optionPrice, 'vega' = vega))
}
