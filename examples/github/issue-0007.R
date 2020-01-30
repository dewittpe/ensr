library(reprex)

reprex::reprex(x = {
#'
#' I have not built a method within the package to return all the coefficients
#' directly to the user.  However, using the summary method and a Map call you
#' can extract all the coefficients, build a data.frame thereof, and explore as
#' you see fit.
#'
library(ensr)
library(ggplot2)
library(data.table)

#' build an example ensr object and summary
set.seed(42)
y_matrix <- as.matrix(landfill$evap)
x_matrix <- as.matrix(landfill[, topsoil_porosity:weather_temp])

ensr_obj <- ensr(y = y_matrix, x = x_matrix, standardize = FALSE)
ensr_obj
ensr_obj_summary <- summary(ensr_obj)

#'
#' Extract all the coefficients for every entry in the summary.  Return a matrix
#' with each column the coefficient value and two additionals columns for the
#' lambda and alpha values.
#'
coefs <-
  Map(f = function(ensr_obj, l_index, lambda, alpha) {
        out <- t(as.matrix(coef(ensr_obj[[l_index]], s = lambda))) 
        cbind(out, lambda = lambda, alpha = alpha)
      },
      l_index  = ensr_obj_summary$l_index,
      lambda   = ensr_obj_summary$lambda,
      alpha    = ensr_obj_summary$alpha,
      MoreArgs = list(ensr_obj = ensr_obj)
      )

#'
#' build a data.frame
#'
coefs <- do.call(rbind, coefs)
coefs <- as.data.frame(coefs)

#'
#' example plot
#'
ggplot(data = coefs) + aes(x = rh) + geom_histogram()
ggplot(data = coefs) +
  aes(x = lambda, y = alpha, color = cut(rh, breaks = 4)) +
  geom_point() +
  scale_x_log10()
},
venue = "gh")

