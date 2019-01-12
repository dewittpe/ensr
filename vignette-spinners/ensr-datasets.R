#'---
#'title: "Elastic Net SearcheR Datasets"
#'subtitle: "ensr package version `r packageVersion('ensr')`"
#'author: "Peter E. DeWitt"
#'date: "`r Sys.Date()`"
#'output:
#'  rmarkdown::html_vignette
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteIndexEntry{ensr-datasets}
#'  %\VignetteEncoding{UTF-8}
#'---
#+ label=setup, include = FALSE
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)
options(qwraps2_markup = "markdown")

#'
#' This vignette provides descriptions of the example data sets in the `r Rpkg(ensr)`
#' package.  We provide information about how the data sets were built and also
#' about each individual variable.
#'
#' The packages needed to constuct the data sets are:
library(data.table)
library(magrittr)
library(qwraps2)
library(digest)
set.seed(42)

#/* Only load the ensr namespace if you are not running this script form the
# data-raw directory
if (grepl("data-raw", getwd())) {
  source("../R/standardize.R") 
} else {
#*/
library(ensr)
#/*
}
#*/

#'
#/*
# ------------------------- Traumatic Brain Injuries ---------------------------
#*/
#'
#' # Traumatic Brain Injuries
#'
#' A synthetic data set based based on a Traumatic Brain Injury (TBI) study
#'  has been included with the `r Rpkg(ensr)` package.  All data points in this data
#' set have been randomly generated and have no association with any real
#' patient.
#'
#' The columns of this data set are:
#'
#' | Column(s)                 | Meaning                                                                                                 |
#' | :------------------------ | :------------------------------------------------------------------------------------------------------ |
#' | `age`                     | Age of the patient, in days, at time of hospital admission                                              |
#' | `female`                  | An integer flag for sex, 1 == female, 0 == male.                                                        |
#' | `los`                     | Length of stay: number of days between hospital admission and discharge.
#' | `pcode1`, ..., `pcode6`   | Indicator columns for the presence or absence of a procedure code from a billing database.              |
#' | `ncode1`, ..., `ncode6`   | Indicator columns for the presence or absence of the same procedure code, but from a trauma database.  |
#' | `injury1`, ..., `injury3` | Indicator columns for the presence or absence of three different types of TBI.                          |
#'
#'
#' Construction of the synthetic data set:
TBI_SUBJECTS <- 1323L

tbi <- data.table(age    = round(rweibull(TBI_SUBJECTS, shape = 1.9, scale = 2000)),
                  female = rbinom(TBI_SUBJECTS, 1, 0.10),
                  los    = round(rexp(TBI_SUBJECTS, rate = 1/13)))

#'
#' The `pcode` and `ncode` variables are difficult to simulate.  There are 64
#' permutations for each set.  We built two vectors with relative frequencies of
#' the codes.
pcodes <-
  c(`0.0.0.0.0.0` = 1230, `1.0.0.0.0.0` = 130, `0.1.0.0.0.0` = 40,
    `1.1.0.0.0.0` = 7, `0.0.1.0.0.0` = 120, `1.0.1.0.0.0` = 4, `0.1.1.0.0.0` = 20,
    `1.1.1.0.0.0` = 2, `0.0.0.1.0.0` = 4, `1.0.0.1.0.0` = 4, `0.1.0.1.0.0` = 20,
    `1.1.0.1.0.0` = 4, `0.0.1.1.0.0` = 20, `1.0.1.1.0.0` = 2, `0.1.1.1.0.0` = 2,
    `1.1.1.1.0.0` = 7, `0.0.0.0.1.0` = 5, `1.0.0.0.1.0` = 4, `0.1.0.0.1.0` = 10,
    `1.1.0.0.1.0` = 9, `0.0.1.0.1.0` = 10, `1.0.1.0.1.0` = 7, `0.1.1.0.1.0` = 1,
    `1.1.1.0.1.0` = 7, `0.0.0.1.1.0` = 10, `1.0.0.1.1.0` = 6, `0.1.0.1.1.0` = 5,
    `1.1.0.1.1.0` = 8, `0.0.1.1.1.0` = 5, `1.0.1.1.1.0` = 2, `0.1.1.1.1.0` = 6,
    `1.1.1.1.1.0` = 9, `0.0.0.0.0.1` = 40, `1.0.0.0.0.1` = 7, `0.1.0.0.0.1` = 40,
    `1.1.0.0.0.1` = 10, `0.0.1.0.0.1` = 10, `1.0.1.0.0.1` = 1, `0.1.1.0.0.1` = 20,
    `1.1.1.0.0.1` = 5, `0.0.0.1.0.1` = 10, `1.0.0.1.0.1` = 2, `0.1.0.1.0.1` = 6,
    `1.1.0.1.0.1` = 1, `0.0.1.1.0.1` = 8, `1.0.1.1.0.1` = 1, `0.1.1.1.0.1` = 7,
    `1.1.1.1.0.1` = 2, `0.0.0.0.1.1` = 9, `1.0.0.0.1.1` = 1, `0.1.0.0.1.1` = 1,
    `1.1.0.0.1.1` = 2, `0.0.1.0.1.1` = 6, `1.0.1.0.1.1` = 9, `0.1.1.0.1.1` = 6,
    `1.1.1.0.1.1` = 4, `0.0.0.1.1.1` = 2, `1.0.0.1.1.1` = 9, `0.1.0.1.1.1` = 3,
    `1.1.0.1.1.1` = 1, `0.0.1.1.1.1` = 7, `1.0.1.1.1.1` = 4, `0.1.1.1.1.1` = 4,
    `1.1.1.1.1.1` = 3)

ncodes <-
  c(`0.0.0.0.0.0` = 1240, `1.0.0.0.0.0` = 200, `0.1.0.0.0.0` = 130,
    `1.1.0.0.0.0` = 4, `0.0.1.0.0.0` = 90, `1.0.1.0.0.0` = 9, `0.1.1.0.0.0` = 20,
    `1.1.1.0.0.0` = 7, `0.0.0.1.0.0` = 20, `1.0.0.1.0.0` = 3, `0.1.0.1.0.0` = 10,
    `1.1.0.1.0.0` = 8, `0.0.1.1.0.0` = 7, `1.0.1.1.0.0` = 9, `0.1.1.1.0.0` = 8,
    `1.1.1.1.0.0` = 8, `0.0.0.0.1.0` = 6, `1.0.0.0.1.0` = 9, `0.1.0.0.1.0` = 10,
    `1.1.0.0.1.0` = 3, `0.0.1.0.1.0` = 9, `1.0.1.0.1.0` = 7, `0.1.1.0.1.0` = 6,
    `1.1.1.0.1.0` = 8, `0.0.0.1.1.0` = 6, `1.0.0.1.1.0` = 2, `0.1.0.1.1.0` = 9,
    `1.1.0.1.1.0` = 6, `0.0.1.1.1.0` = 2, `1.0.1.1.1.0` = 2, `0.1.1.1.1.0` = 5,
    `1.1.1.1.1.0` = 7, `0.0.0.0.0.1` = 20, `1.0.0.0.0.1` = 6, `0.1.0.0.0.1` = 4,
    `1.1.0.0.0.1` = 1, `0.0.1.0.0.1` = 2, `1.0.1.0.0.1` = 4, `0.1.1.0.0.1` = 7,
    `1.1.1.0.0.1` = 6, `0.0.0.1.0.1` = 4, `1.0.0.1.0.1` = 1, `0.1.0.1.0.1` = 8,
    `1.1.0.1.0.1` = 6, `0.0.1.1.0.1` = 1, `1.0.1.1.0.1` = 4, `0.1.1.1.0.1` = 2,
    `1.1.1.1.0.1` = 7, `0.0.0.0.1.1` = 3, `1.0.0.0.1.1` = 7, `0.1.0.0.1.1` = 7,
    `1.1.0.0.1.1` = 9, `0.0.1.0.1.1` = 5, `1.0.1.0.1.1` = 1, `0.1.1.0.1.1` = 6,
    `1.1.1.0.1.1` = 5, `0.0.0.1.1.1` = 2, `1.0.0.1.1.1` = 3, `0.1.0.1.1.1` = 1,
    `1.1.0.1.1.1` = 5, `0.0.1.1.1.1` = 6, `1.0.1.1.1.1` = 5, `0.1.1.1.1.1` = 4,
    `1.1.1.1.1.1` = 2)

#'
#' We generated the code variables by taking a random sample of the names of the
#' above objects, splitting the names by the `.`, and coercing the result to a
#' data.table.
pcodes <-
  sample(names(pcodes), TBI_SUBJECTS, replace = TRUE, prob = pcodes) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("pcode", 1:6))

pcodes

ncodes <-
  sample(names(ncodes), TBI_SUBJECTS, replace = TRUE, prob = ncodes + 0.1) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("ncode", 1:6))

ncodes

#'
#' The last step in building the predictor variables is to bind the three
#' data.tables together:
tbi <- cbind(tbi, pcodes, ncodes)

#'
#' The three injury categories are constructed by fitting a simplified
#' logistic regression model.  The regression coefficient vectors are defined
#' next.
injury1_coef <-
  matrix(c(-5.80494, -3.03946e-05, 0.773355, 0.00556597, -1.04436, -0.849594,
           0.770122, 0.968153, 16.6315, 0.411286, 4.07926, 5.16926, 2.84976,
           -17.1038, -14.7382, 4.30086),
         ncol = 1)

injury2_coef <-
  matrix(c(-427.083, 0.0742405, -342.072, -8.09704, 299.132, 991.75, -85.0155,
           -170.344, -57.408, 123.201, 161.41, -568.483, -767.944, 706.95,
           10.2964, 148.551),
         ncol = 1)

injury3_coef <-
  matrix(c(-3.54036, -0.00054294, 1.75962, -0.0475071, -17.515, -60.4276,
           4.58458, 0.58551, -19.7312, -1.16923, 2.16091, 63.7699, 39.3569,
           -37.8554, -14.1002, -14.8469),
         ncol = 1)

injury_expression <-
  quote(
        c("injury1", "injury2", "injury3") :=
        list(
             sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury1_coef),
                    function(p) rbinom(1, 1, p)),
             sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury2_coef),
                    function(p) rbinom(1, 1, p)),
             sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury3_coef),
                    function(p) rbinom(1, 1, p))
             )
        )

tbi[, eval(injury_expression)]

#'
#' To prove that the `tbi` object constructed in this vignette is the same as
#' the one provided in via calling `data(tbi, package = "ensr")` we show the
#' sha1 for both data sets here:
#/*
if (!grepl("data-raw", getwd())) {
#*/
ensr_env <- new.env()
data(tbi, package = "ensr", envir = ensr_env)

all.equal(digest::sha1(tbi),
          digest::sha1(ensr_env$tbi))
#/*
}
#*/

#/*
# This bit builds the documentation and the data set for the package.  It needs
# to be evaluated from the data-raw directory during the package build process.
if (!interactive()) {
cat("
# file generated by data-raw/tbi.R script.  DO NOT EDIT BY HAND
#

#' Synthetic Data Set for Traumatic Brain Injuries
#'
#' This data is synthetic, that is, it is random data generated to have similar
#' properties to a data set used for studying traumatic brain injuries.  The
#' \\code{pcode1} ... \\code{pcode6}, \\code{ncode1} ... \\code{ncode6} columns
#' are indicators for procedure or billing codes associated with a hospital
#' stay for TBI.
#'
#' @format a data.table with ", nrow(tbi), "rows and ", ncol(tbi), "columns.
#' Each row of the \\code{tbi} data.table is a unique subject.
#' \\describe{
#' \\item{age}{age, in days}
#' \\item{female}{indicator for sex, 1 == female, 0 == male}
#' \\item{los}{length of stay in the hosptial}
#' \\item{pcode1}{indicator for if the patient had a pcode1}
#' \\item{pcode2}{indicator for if the patient had a pcode2}
#' \\item{pcode3}{indicator for if the patient had a pcode3}
#' \\item{pcode4}{indicator for if the patient had a pcode4}
#' \\item{pcode5}{indicator for if the patient had a pcode5}
#' \\item{pcode6}{indicator for if the patient had a pcode6}
#' \\item{ncode1}{indicator for if the patient had a ncode1}
#' \\item{ncode2}{indicator for if the patient had a ncode2}
#' \\item{ncode3}{indicator for if the patient had a ncode3}
#' \\item{ncode4}{indicator for if the patient had a ncode4}
#' \\item{ncode5}{indicator for if the patient had a ncode5}
#' \\item{ncode6}{indicator for if the patient had a ncode6}
#' \\item{injury1}{First of three specific types of TBI}
#' \\item{injury2}{Second of three specific types of TBI}
#' \\item{injury3}{Third of three specific types of TBI}
#' }
#'
#' @seealso \\code{vignette(\"ensr-datasets\", package = \"ensr\")}
#'
\"tbi\"
",
file = "../R/tbi.R")

usethis::use_data(tbi, overwrite = TRUE)
}
#*/


#'
#/*
# ------------------- Water Percolation Through a Landfill ---------------------
#*/
#'
#' # Water Percolation Through A Landfill
#'
#' The file `landfill.csv` contains results from a computer simulation of water
#' percolation through five layers in a landfill over one year.  The raw data
#' file is provided so the end user can explore their own pre-processing of the
#' data.
#/*
if (grepl("data-raw", getwd())) {
  landfill <-  fread(file = "../inst/extdata/landfill.csv", sep = ",") 
} else {
#*/ 
landfill <-
  fread(file = system.file("extdata/landfill.csv", package = "ensr"), sep = ",")
 #/*
}
#*/

#'
#' This landfill consists of five layers:
#'
#' 1. Topsoil
#' 2. Geomembrane Liner
#' 3. Clay
#' 4. Weathered Lavery Till (WLT)
#' 5. Unweathered Lavery Till (ULT)
#'
#' The outcomes modeled by the computer program are:
#+label="regex_names", include = FALSE
regex_names <- function(pattern) {
  paste(grep(pattern, names(landfill), value = TRUE), collapse = ", ")
}

#'
#' | Outcome       | Variable Name(s)           |
#' | :--------     | :----------------          |
#' | evaporation   | `r regex_names("^evap")`   |
#' | runoff        | `r regex_names("^runoff")` |
#' | percolation   | `r regex_names("^perc_")`  |
#' | drainage      | `r regex_names("^drain_")` |
#' | water content | `r regex_names("^wc_")`    |
#'
#' The set of predictors included in the data set are:
#'
#' | Predictor                          | Variable Name(s)                    | Notes                                                   |
#' | :--------                          | :----------------                   | :----                                                   |
#' | Porosity                           | `r regex_names("_porosity$")`       |                                                         |
#' | van Genuchten $\alpha$             | `r regex_names("_alpha$")`          | related to the inverse of the air entry suction         |
#' | van Genuchten $\theta_r$           | `r regex_names("_thetar$")`         | residual water content                                  |
#' | van Genuchten N                    | `r regex_names("_n$")`              | pore size                                               |
#' | Saturated hydraulic conductivity   | `r regex_names("_ks$")`             |                                                         |
#' | Relative Humidity                  | `r regex_names("^rh$")`             |                                                         |
#' | Leaf Area Index                    | `r regex_names("^lai$")`            |                                                         |
#' | Grow Start                         | `r regex_names("growstart")`        | First day of the growing season                         |
#' | Grow End                           | `r regex_names("growend")`          | Last day of the growing season                          |
#' | Wind                               | `r regex_names("wind")`             | Average wind speed in mph                               |
#' | Solar Radiation                    | `r regex_names("^rm\\w")`           | Mean of daily solar radiation for wet days, or dry days |
#' | Amplitude of daily solar radiation | `r regex_names("^ar$")`             |                                                         |
#' | SCS AMCII Curve Number             | `r regex_names("cn")`               |                                                         |
#' | Plant root beta                    | `r regex_names("^beta$")`           | Used for calculating evaporation zone depth             |
#' | Linear Defects                     | `r regex_names("^liner_defects$")`  | Number of defects per acre of liner                     |
#' | Linear Pinholes                    | `r regex_names("^liner_pinholes$")` | Number of pinholes per acre of liner                    |
#' | 100 year average Solar Radiation   | `r regex_names("weather_solrad")`   |                                                         |
#' | 100 year average Precipitation     | `r regex_names("weather_precip")`   |                                                         |
#' | 100 year average Temperature       | `r regex_names("weather_temp")`     |                                                         |
#'
#' ## Landfill Data Preparation
#'
#' The elastic net method suggests that all predictors be centered and scaled.
#' In the case of multi-variable responses, it is recommended that the outcomes 
#' be centered and scaled as well.  For calls to `glmnet` and `cv.glmnet`, the argument
#' `standardized = TRUE` (default) will center and scale the values.  If you have
#' already centered and scaled the data you may prefer to set `standardized =
#' FALSE`.
#'
#' If you want to explicitly standardize your data, the base R function `scale` 
#' is adequate for most situations. However, with the landfill data the simple 
#' centering and scaling could be considered inappropriate.  
#' `scale` will use the mean and standard deviation of the input. 
#' However, the landfill data contains repeated measurements.  Centering
#' and scaling should be based on the mean and standard deviation of independent
#' values.  The ensr function `standardize` will standardize a numeric vector
#' based on unique values, by default, via either mean/standard deviation, or
#' median/IQR.
#'
#' Here is an example.  There are `r qwraps2::frmt(nrow(landfill))` rows in the
#' landfill data set.  There are `r length(unique(landfill$topsoil_alpha))`
#' unique values for the van Genuchten $\alpha$ value.  Here are the different
#' scale and centerings:
nrow(landfill)
length(unique(landfill$topsoil_alpha))

# Standard scaling, using the mean and standard deviation for the full vector.
scaled_topsoil_alpha_v1 <- as.vector(scale(landfill$topsoil_alpha))

scaled_topsoil_alpha_v2 <-
  (landfill$topsoil_alpha - mean(landfill$topsoil_alpha)) / sd(landfill$topsoil_alpha)

all.equal(target  = scaled_topsoil_alpha_v1,
          current = scaled_topsoil_alpha_v2,
          check.attributes = FALSE)

# Center and scale using the mean and standard deviation of only the unique
# values of the vector.
scaled_topsoil_alpha_v3 <-
  as.vector(scale(landfill$topsoil_alpha,
                  center = mean(unique(landfill$topsoil_alpha)),
                  scale  = sd(unique(landfill$topsoil_alpha))))

scaled_topsoil_alpha_v4 <- standardize(landfill$topsoil_alpha)

all.equal(target  = scaled_topsoil_alpha_v3,
          current = scaled_topsoil_alpha_v4,
          check.attributes = FALSE)

# Center and scale using the median and inner quartile range (IQR).
scaled_topsoil_alpha_v5 <-
  as.vector(scale(landfill$topsoil_alpha,
                  center = median(unique(landfill$topsoil_alpha)),
                  scale  = IQR(unique(landfill$topsoil_alpha))))

scaled_topsoil_alpha_v6 <-
  standardize(landfill$topsoil_alpha,
              stats = list(center = "median", scale = "IQR"))

all.equal(target  = scaled_topsoil_alpha_v5,
          current = scaled_topsoil_alpha_v6,
          check.attributes = FALSE)

#'
#' For the full landfill data set we can center and scale all the columns as:
landfill <- standardize(landfill)

#'
#' Note that the values used for centering and scaling are retained as
#' attributes for each variable in the data set.
str( landfill[, 1:2] )

#'
#' The `landfill` object that is generated in this vignette is the same as the
#' one which you can call via `data(landfill, package = "ensr")`.
#/*
if (!grepl("data-raw", getwd())) {
#*/
data(landfill, package = "ensr", envir = ensr_env)
all.equal(digest::sha1(landfill),
          digest::sha1(ensr_env$landfill))
#/*
}
#*/

#'
#/*
# This bit builds the documentation and the data set for the package.  It needs
# to be evaluated from the data-raw directory during the package build process.
if (!interactive()) {
cat("
# ------------------- file generated by automated process ----------------------
# --------------------------- DO NOT EDIT BY HAND ------------------------------
#

#' Water Percolation Through A Landfill
#'
#' A computer simulation of water moving through a landfill.  Detailed
#' explanation for the variables and the construction of the data set is found
#' in
#' \\code{vignette(\"ensr-datasets\", package = \"ensr\")}
#'
#'
#' @seealso \\code{vignette(\"ensr-datasets\", package = \"ensr\")}
#'
\"landfill\"
",
file = "../R/landfill.R")

usethis::use_data(landfill, overwrite = TRUE)
}
#*/




#/*
# --------------------------- Session Information ------------------------------
#*/
#'
#'
#' # Session Info
print(sessionInfo(), local = FALSE)

#/*
# ------------------------------- End of File ----------------------------------
#*/
