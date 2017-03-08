#' black76ImVo
#'
#' \code{black76ImVo} returns the implied volatility based on black model, Black(1976).
#' It employs Newton–Raphson method based on vega of the option.
#'
#' @param Ft Future price. Numeric object.
#' @param K Strike price. Numeric object.
#' @param type Type of the European option, call(c) or put(p). String object.
#' @param Time Time to maturity (in year). Numeric object.
#' @param r Continousely compounded interest rate. Numeric object.
#' @param optionPrice Observed market option premium. Numeric object.
#' @param sigma_ini Initial value for iteration in the Newton-Raphson method.
#'   Numeric object or NULL(default). If no value is provided, the sigma which
#'   maximizes Vega is choosen.
#' @param tol Error tolerance of the interation step.
#' @param maxiter Maximum number of iteration.
#'
#' @return Calculated implied volatilited.
#' @export
#'
#' @examples
#' price <- black76(type = "C", Ft = 48.03, Time = 0.1423, K = 50,
#'                   sigma = 0.2, r = 0.03)
#' black76ImVo(type = "C", Ft = 48.03, Time = 0.1423, K = 50, sigma_ini = 1, r = 0.03,
#'             optionPrice = price[1], tol = 1e-10, maxiter = 20)
#'
#' price <- black76(type = "C", Ft = 50, Time = 1, K = 62,
#'                   sigma = 0.342, r = 0.2186)
#' black76ImVo(type = "C", Ft = 50, Time = 1, K = 62, r = 0.2186,
#'             optionPrice = price[1], tol = 1e-10, maxiter = 20)
#'
#' price <- black76(type = "C", Ft = 50, Time = 0.25, K = 26,
#'                  sigma = 0.1261, r = 0.2584)
#'
#' black76ImVo(type = "C", Ft = 50, Time = 5, K = 22, r = 0.3588,
#'             optionPrice = price[1], tol = 1e-20, maxiter = 1000)
#'
#'

black76ImVo <- function(Ft, K, type, Time, r, optionPrice, sigma_ini = NULL,
                        tol = 1e-10, maxiter = 1000) {

  if (is.null(sigma_ini)) {
  sigma0 <- sqrt(2 * abs(log(Ft / K) + r * Time) / Time)
  } else {
  sigma0 <- sigma_ini
  }

  ##########
  # Newton_Raphson and bisection method
  # for solving black76(sigma,...)[1] - optionPrice == 0
  sigma1 <- tryCatch(
    {
      iter <- 1
      sigma <- sigma0
      fx <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                    sigma = sigma)

      while ((abs(fx[1] - optionPrice) > tol) && (iter <= maxiter)) {
        sigma <- sigma - (fx[1] - optionPrice) / fx[2]
        # too large sigma renders vega zero in next iteration !!! work well
        sigma <- ifelse(abs(sigma) > 1e2, runif(1, 0.1, 0.2), sigma)
        fx <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                      sigma = sigma)
        iter <- iter + 1
      }
      sigma
    }, warning = function(war) {
      1000 # any big number or modify code for sigma1 singma2 selection
    }, error = function(err) {
      1000
    }
  )

  ###########
  # bisection method
  # use only if Newton_Raphson fails convergence
  if (abs(black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
             sigma = sigma1)[1] - optionPrice) > tol) {

    sigma2 <- tryCatch(
      {
        #######
        sigma_l <- 0 # volatility cannot be negative
        sigma_r <- 1 # initial guess

        fn_l <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                          sigma = sigma_l)[1] - optionPrice
        fn_r <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                          sigma = sigma_r)[1] - optionPrice

        ## make sure we start with fn_l * fn_r < 0
        while ((fn_l * fn_r > 0) && (iter <= maxiter ^ 2)) {
          sigma_r <- sigma_r + 1
          fn_r <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                          sigma = sigma_r)[1] - optionPrice
          iter <- iter + 1
        }

        iter <- 1
        while ((abs(sigma_l - sigma_r) > tol) && (iter <= maxiter ^ 2)) {
          sigma_m = (sigma_l + sigma_r) / 2
          fn_m <- black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
                          sigma = sigma_m)[1] - optionPrice
          if (fn_m == 0) {
            return(sigma_m)
          } else if (fn_l * fn_m < 0) {
            sigma_r <- sigma_m
            fn_r <- sigma_m
          } else {
            sigma_l <- sigma_m
            fn_l <- sigma_m
          }
          iter = iter + 1
        }
        return((sigma_l + sigma_r) / 2)



        ###
        sigma
      }, warning = function(war) {
        NA_real_
      }, error = function(err) {
        NA_real_
      }
    )
  }

  ###########
  # pick the more accurate one
  # sigma <- ifelse(abs(black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
  #                         sigma = sigma1)[1] - optionPrice) <
  #                 abs(black76(Ft = Ft, K = K, Time = Time, r = r, type = type,
  #                         sigma = sigma2)[1] - optionPrice),
  #                 sigma1, sigma2)
  names(sigma1) <- 'ImVol'
  return(sigma1)
}










