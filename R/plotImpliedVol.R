#' plotImpliedVol
#'
#' \code{plotImpliedVol} takes a data frame, applies \code{black76ImVoDF} to calculate
#' implied volatility and plot it against strike price and time to maturity.
#'
#' @param df A dataframe.
#' @param Ftdf Name of a columns in the dataframe corresponding to future price.
#' @param Kdf Name of a columns in the dataframe corresponding to strike price.
#' @param typedf Name of a columns in the dataframe corresponding to option
#'   type.
#' @param Timedf Name of a columns in the dataframe corresponding to time to maturity.
#' @param rdf Name of a columns in the dataframe corresponding to interest rate.
#' @param optionPricedf Observed market option premium. Numeric object.
#' @param sigma_ini Initial value for iteration in the Newton-Raphson method.
#'   Numeric object or NULL(default). If no value is provided, the sigma which
#'   maximizes Vega is choosen.
#' @param tol Error tolerance of the interation step.
#' @param phi Angles defining the viewing the colatitude direction.
#' @param theta Angles defining the viewing the azimuthal direction.
#' @param maxiter Maximum number of iteration
#'
#' @export
#'
#' @examples
#' example(black76ImVoDF)
#' df2 <- df2[sample(x = 1:nrow(df2), size = 10), ]
#' head(df2)
#'
#' plotImpliedVol(df = df2, Ftdf = 'Ft', Kdf = 'Strike', typedf = 'type',
#'               Timedf = 'Time', rdf = 'r', optionPricedf = 'optionPrice',
#'               tol = 1e-5, maxiter = 1000, phi = -70, theta = 5)
plotImpliedVol <- function(df, Ftdf, Kdf, typedf, Timedf, rdf, optionPricedf,
                           sigma_ini = NULL, tol = 1e-4, maxiter = 20,
                           phi = -70, theta = 5) {

  # check input names
  if (!all(c(Ftdf, Kdf, typedf, Timedf, rdf, optionPricedf) %in% names(df))) {
    stop("Please check the column names in the data frame.")
  }

  df0 <- black76ImVoDF(df = df, Ftdf = Ftdf, Kdf = Kdf, typedf = typedf,
                      Timedf = Timedf, rdf = rdf, optionPricedf = optionPricedf,
                      sigma_ini = NULL, tol = 1e-4, maxiter = 20)

  plot3d(x = df0[[Kdf]], y = df0[[Timedf]], z = df0[['IV']], type = "s",
         size = 0.75, lit = FALSE, xlab = "", ylab = "", zlab = "",
         axes = FALSE)

  rgl.bbox(color = "grey50",
           emission = "grey50",
           xlen = 0, ylen = 0, zlen = 0)

  rgl.material(color = "black")

  axes3d(edges = c("x--", "y+-", "z--"),
         ntick = 6,
         cex = .75)

  mtext3d("Strike Price", edge = "x--", line = 2)
  mtext3d("Time to Maturity", edge = "y+-", line = 3)
  mtext3d("Implied Volatility", edge = "z--", line = 3)

  view3d(theta, phi)
}
