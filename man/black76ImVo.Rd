% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/black76ImVo.R
\name{black76ImVo}
\alias{black76ImVo}
\title{black76ImVo}
\usage{
black76ImVo(Ft, K, type, Time, r, optionPrice, sigma_ini = NULL,
  tol = 1e-10, maxiter = 1000)
}
\arguments{
\item{Ft}{Future price. Numeric object.}

\item{K}{Strike price. Numeric object.}

\item{type}{Type of the European option, call(c) or put(p). String object.}

\item{Time}{Time to maturity (in year). Numeric object.}

\item{r}{Continousely compounded interest rate. Numeric object.}

\item{optionPrice}{Observed market option premium. Numeric object.}

\item{sigma_ini}{Initial value for iteration in the Newton-Raphson method.
Numeric object or NULL(default). If no value is provided, the sigma which
maximizes Vega is choosen.}

\item{tol}{Error tolerance of the interation step.}

\item{maxiter}{Maximum number of iteration.}
}
\value{
Calculated implied volatilited.
}
\description{
\code{black76ImVo} returns the implied volatility based on black model,
Black(1976). It employs Newton–Raphson method based on vega of the option.
}
\examples{
price <- black76(type = "C", Ft = 48.03, Time = 0.1423, K = 50,
                  sigma = 0.2, r = 0.03)
black76ImVo(type = "C", Ft = 48.03, Time = 0.1423, K = 50, sigma_ini = 1, r = 0.03,
            optionPrice = price[1], tol = 1e-10, maxiter = 20)

price <- black76(type = "C", Ft = 50, Time = 1, K = 62,
                  sigma = 0.342, r = 0.2186)
black76ImVo(type = "C", Ft = 50, Time = 1, K = 62, r = 0.2186,
            optionPrice = price[1], tol = 1e-10, maxiter = 20)

This example illustrates when the Time value (volatility) is too
small compared to intrinsic value (moneyness), the root finding is too
difficult due to the flat slope around the root (close to zero vega).
price <- black76(type = "C", Ft = 50, Time = 0.25, K = 26,
                 sigma = 0.1261, r = 0.2584)
black76ImVo(type = "C", Ft = 50, Time = 5, K = 26, r = 0.2584,
            optionPrice = price[1], tol = 1e-20, maxiter = 1000)
price <- black76(type = "C", Ft = 50, Time = 0.25, K = 26,
                 sigma = 0.1, r = 0.2584)
price <- black76(type = "C", Ft = 50, Time = 0.25, K = 26,
                 sigma = 0.3, r = 0.2584)

}
