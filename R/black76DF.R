#' black76DF
#'
#' \code{black76DF} appies \code{black76} to calculate the premium of European
#' option on future based on black model, Black(1976) on each row of a dataframe
#' with strike price, future price, option type, volatility, interest rate, and
#' time to maturity as columns.
#' @param df Name of a dataframe
#' @param Ftdf Name of a columns in the dataframe corresponding to future price.
#' @param sigmadf Name of a columns in the dataframe corresponding to
#'   volatility.
#' @param Kdf Name of a columns in the dataframe corresponding to strike price.
#' @param typedf  Name of a columns in the dataframe corresponding to option
#'   type.
#' @param Timedf Name of a columns in the dataframe corresponding to interest
#'   rate.
#' @param rdf Name of a columns in the dataframe corresponding to interest rate.
#'
#' @return Option premium calculated as a column in the dataframe.
#'
#' @examples
#' Ft <- 50
#' Kvec <- 80:120
#' Timevec <- c(c(3, 6, 9)/12, 1:5)
#' r <- 0.01
#' Vol <- runif(n = 328, min = 0.1, 0.3)
#' type <- sample(x = c("C", "P"), size = 328, replace = TRUE, prob = c(0.5, 0.5))
#' df <- data.frame(expand.grid(Kvec, Timevec), Ft, r, Vol, type)
#'
#' df2 <- black76DF(df = df, Ftdf = 'Ft', sigmadf = 'Vol', Kdf = 'Var1',
#'                  typedf = 'type', rdf = 'r', Timedf = 'Var2')
#'                  head(df2)
black76DF <- function(df, Ftdf, sigmadf, Kdf, typedf, Timedf, rdf){
  temp <- vector(mode = 'numeric', length = nrow(df))

  for (i in seq_len(nrow(df))) {
    temp[i] <- black76(Ft = df[i, Ftdf],
                       sigma = df[i, sigmadf],
                       K = df[i, Kdf],
                       type = df[i, typedf],
                       Time = df[i, Timedf],
                       r = df[i, rdf])[[1]]
  }
    return(data.frame(df, optionPrice = temp))
}
