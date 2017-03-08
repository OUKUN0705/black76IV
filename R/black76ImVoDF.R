#' black76ImVoDF
#'
#' \code{black76ImVoDF} applies \code{black76ImVo} on each row of a dataframe
#' with option price, strike price, future price, option type, interest rate,
#' and time to maturity as columns.
#'
#' @param df A dataframe.
#' @param Ftdf Name of a columns in the dataframe corresponding to future price.
#' @param Kdf Name of a columns in the dataframe corresponding to strike price.
#' @param typedf Name of a columns in the dataframe corresponding to option
#'   type.
#' @param Timedf Name of a columns in the dataframe corresponding to time to maturity.
#' @param rdf Name of a columns in the dataframe corresponding to interest rate.
#' @param optionPricedf Name of a columns in the dataframe corresponding to
#'   option price.
#' @param output Argument specifying whether the output is the implied
#'   volatility calculated only or implied volatility as a column in the
#'   dataframe (default).
#' @param sigma_ini  Initial value for iteration in the Newton-Raphson method.
#'   Numeric object or NULL (default). If no value is provided, the sigma which
#'   maximizes Vega is choosen.
#' @param tol Error tolerance of the interation step.
#' @param maxiter Maximum number of iteration
#'
#' @return Implied volatility calculated in a vector or as a column in the
#'   dataframe (default).
#'
#' @examples
#' Ft <- 50
#' Kvec <- 20:80

#' Kvec <- 20:80 + floor(runif(n = 61, min = 0, max = 5))
#' Timevec <- c(c(3, 6, 9)/12, 1:5)
#' df0 <- data.frame(expand.grid(Kvec, Timevec))
#'
#' names(df0) <- c('Strike', 'Time')
#'
#' r <- (log(df0$Time) + 2) / 100
#' Vol <- (abs(df0$Strike - 50) ^ (1.3) + 50 + 3/df0$Strike) / 200
#' type <- sample(x = c("C", "P"), size = 488, replace = TRUE, prob = c(0.5, 0.5))
#' df <- data.frame(df0, Ft, r, Vol, type)
#' df2 <- black76DF(df = df, Ftdf = 'Ft', sigmadf = 'Vol', Kdf = 'Strike',
#'                  typedf = 'type', rdf = 'r', Timedf = 'Time')
#' head(df2)
#' df3 <- black76ImVoDF(df = df2, Ftdf = 'Ft', Kdf = 'Strike', typedf = 'type',
#'                      Timedf = 'Time', rdf = 'r', optionPricedf = 'optionPrice',
#'                      tol = 1e-10, maxiter = 2000)
#' summary(abs(df3$Vol - df3$IV))
#' plot(density(df3$Vol - df3$IV))
#' df3[which.max(abs(df3$Vol - df3$IV)),]
#' df3[is.na(df3$IV),]
#' all(abs(df3$Vol - df3$IV) < 1e-10)

black76ImVoDF <- function(df,
                          Ftdf, Kdf, typedf, Timedf, rdf, optionPricedf,
                          sigma_ini = NULL, tol = 1e-10, maxiter = 1000,
                          output = c("dataframe", "IV")) {

  output <- match.arg(output)
  ImVol <- vector(mode = 'numeric', length = nrow(df))

  for (i in seq_len(nrow(df))) {
    ImVol[i] <- black76ImVo(Ft = df[i, Ftdf],
                            K = df[i, Kdf],
                            type = df[i, typedf],
                            Time = df[i, Timedf],
                            r = df[i, rdf],
                            optionPrice = df[i, optionPricedf],
                            tol = tol,
                            maxiter = maxiter)
  }
  if (output == "dataframe") {
    return(data.frame(df, IV = ImVol))
  } else if (output == "IV") {
    return(ImVol)
  }
}
