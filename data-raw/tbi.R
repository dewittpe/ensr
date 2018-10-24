library(data.table)
library(magrittr)
set.seed(42)
SUBJECTS <- 1323L

injury1_coef <-
  c(-5.80493739907985, -3.03946058323712e-05, 0.773355121768186,
    0.00556597435090851, -1.04435542613342, -0.849594200560248, 0.770121807815493,
    0.968152757969973, 16.6315015330743, 0.411286271226889, 4.07925548561193,
    5.16925941937841, 2.84975653728314, -17.1038350876656, -14.7381988875365,
    4.30086024415758)

injury2_coef <-
  c(-427.083132468604, 0.0742405434241317, -342.072094607234, -8.0970370643083,
    299.13198491346, 991.749590065495, -85.0154942505501, -170.344273405319,
    -57.4080279268783, 123.201027685724, 161.409886380387, -568.482572444852,
    -767.944067206803, 706.950279886359, 10.2964358402242, 148.550902568089
    )

injury3_coef <-
  c(-3.54036200105764, -0.000542940376653844, 1.75961587165548,
    -0.0475071232547805, -17.5149629757304, -60.4276475350749, 4.5845792424234,
    0.585509819731677, -19.731191710923, -1.16922617114945, 2.16090819413067,
    63.769900481884, 39.3568884099645, -37.8553590639481, -14.1001624738684,
    -14.8468859869241)

pcodes <-
  c(`0.0.0.0.0.0` = 123L, `1.0.0.0.0.0` = 13L, `0.1.0.0.0.0` = 4L,
    `1.1.0.0.0.0` = 0L, `0.0.1.0.0.0` = 12L, `1.0.1.0.0.0` = 0L,
    `0.1.1.0.0.0` = 2L, `1.1.1.0.0.0` = 0L, `0.0.0.1.0.0` = 0L, `1.0.0.1.0.0` = 0L,
    `0.1.0.1.0.0` = 2L, `1.1.0.1.0.0` = 0L, `0.0.1.1.0.0` = 2L, `1.0.1.1.0.0` = 0L,
    `0.1.1.1.0.0` = 0L, `1.1.1.1.0.0` = 0L, `0.0.0.0.1.0` = 0L, `1.0.0.0.1.0` = 0L,
    `0.1.0.0.1.0` = 1L, `1.1.0.0.1.0` = 0L, `0.0.1.0.1.0` = 1L, `1.0.1.0.1.0` = 0L,
    `0.1.1.0.1.0` = 0L, `1.1.1.0.1.0` = 0L, `0.0.0.1.1.0` = 1L, `1.0.0.1.1.0` = 0L,
    `0.1.0.1.1.0` = 0L, `1.1.0.1.1.0` = 0L, `0.0.1.1.1.0` = 0L, `1.0.1.1.1.0` = 0L,
    `0.1.1.1.1.0` = 0L, `1.1.1.1.1.0` = 0L, `0.0.0.0.0.1` = 4L, `1.0.0.0.0.1` = 0L,
    `0.1.0.0.0.1` = 4L, `1.1.0.0.0.1` = 1L, `0.0.1.0.0.1` = 1L, `1.0.1.0.0.1` = 0L,
    `0.1.1.0.0.1` = 2L, `1.1.1.0.0.1` = 0L, `0.0.0.1.0.1` = 1L, `1.0.0.1.0.1` = 0L,
    `0.1.0.1.0.1` = 0L, `1.1.0.1.0.1` = 0L, `0.0.1.1.0.1` = 0L, `1.0.1.1.0.1` = 0L,
    `0.1.1.1.0.1` = 0L, `1.1.1.1.0.1` = 0L, `0.0.0.0.1.1` = 0L, `1.0.0.0.1.1` = 0L,
    `0.1.0.0.1.1` = 0L, `1.1.0.0.1.1` = 0L, `0.0.1.0.1.1` = 0L, `1.0.1.0.1.1` = 0L,
    `0.1.1.0.1.1` = 0L, `1.1.1.0.1.1` = 0L, `0.0.0.1.1.1` = 0L, `1.0.0.1.1.1` = 0L,
    `0.1.0.1.1.1` = 0L, `1.1.0.1.1.1` = 0L, `0.0.1.1.1.1` = 0L, `1.0.1.1.1.1` = 0L,
    `0.1.1.1.1.1` = 0L, `1.1.1.1.1.1` = 0L)

ncodes <-
  c(`0.0.0.0.0.0` = 124L, `1.0.0.0.0.0` = 20L, `0.1.0.0.0.0` = 13L,
    `1.1.0.0.0.0` = 0L, `0.0.1.0.0.0` = 9L, `1.0.1.0.0.0` = 0L, `0.1.1.0.0.0` = 2L,
    `1.1.1.0.0.0` = 0L, `0.0.0.1.0.0` = 2L, `1.0.0.1.0.0` = 0L, `0.1.0.1.0.0` = 1L,
    `1.1.0.1.0.0` = 0L, `0.0.1.1.0.0` = 0L, `1.0.1.1.0.0` = 0L, `0.1.1.1.0.0` = 0L,
    `1.1.1.1.0.0` = 0L, `0.0.0.0.1.0` = 0L, `1.0.0.0.1.0` = 0L, `0.1.0.0.1.0` = 1L,
    `1.1.0.0.1.0` = 0L, `0.0.1.0.1.0` = 0L, `1.0.1.0.1.0` = 0L, `0.1.1.0.1.0` = 0L,
    `1.1.1.0.1.0` = 0L, `0.0.0.1.1.0` = 0L, `1.0.0.1.1.0` = 0L, `0.1.0.1.1.0` = 0L,
    `1.1.0.1.1.0` = 0L, `0.0.1.1.1.0` = 0L, `1.0.1.1.1.0` = 0L, `0.1.1.1.1.0` = 0L,
    `1.1.1.1.1.0` = 0L, `0.0.0.0.0.1` = 2L, `1.0.0.0.0.1` = 0L, `0.1.0.0.0.1` = 0L,
    `1.1.0.0.0.1` = 0L, `0.0.1.0.0.1` = 0L, `1.0.1.0.0.1` = 0L, `0.1.1.0.0.1` = 0L,
    `1.1.1.0.0.1` = 0L, `0.0.0.1.0.1` = 0L, `1.0.0.1.0.1` = 0L, `0.1.0.1.0.1` = 0L,
    `1.1.0.1.0.1` = 0L, `0.0.1.1.0.1` = 0L, `1.0.1.1.0.1` = 0L, `0.1.1.1.0.1` = 0L,
    `1.1.1.1.0.1` = 0L, `0.0.0.0.1.1` = 0L, `1.0.0.0.1.1` = 0L, `0.1.0.0.1.1` = 0L,
    `1.1.0.0.1.1` = 0L, `0.0.1.0.1.1` = 0L, `1.0.1.0.1.1` = 0L, `0.1.1.0.1.1` = 0L,
    `1.1.1.0.1.1` = 0L, `0.0.0.1.1.1` = 0L, `1.0.0.1.1.1` = 0L, `0.1.0.1.1.1` = 0L,
    `1.1.0.1.1.1` = 0L, `0.0.1.1.1.1` = 0L, `1.0.1.1.1.1` = 0L, `0.1.1.1.1.1` = 0L,
    `1.1.1.1.1.1` = 0L)

tbi <- data.table(age    = round(rweibull(SUBJECTS, shape = 1.9, scale = 2000)),
                  female = rbinom(SUBJECTS, 1, 0.10),
                  los    = round(rexp(SUBJECTS, rate = 1/13)))

pcodes <-
  sample(names(pcodes), SUBJECTS, replace = TRUE, prob = pcodes + 0.1) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("pcode", 1:6))

ncodes <-
  sample(names(ncodes), SUBJECTS, replace = TRUE, prob = ncodes + 0.1) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("ncode", 1:6))


tbi <- cbind(tbi, pcodes, ncodes)

tbi[, c("injury1", "injury2", "injury3") := list(
                                                 sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% matrix(injury1_coef, ncol = 1)), function(p) rbinom(1, 1, p))
                                                 ,
                                                 sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% matrix(injury2_coef, ncol = 1)), function(p) rbinom(1, 1, p))
                                                 ,
                                                 sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% matrix(injury3_coef, ncol = 1)), function(p) rbinom(1, 1, p))
                                                 )
  ]

summary(tbi)

if (interactive()) {
  glm(injury1 ~ . - injury2 - injury3, data = tbi, family = binomial()) %>% summary
  glm(injury2 ~ . - injury1 - injury3, data = tbi, family = binomial()) %>% summary
  glm(injury3 ~ . - injury1 - injury2, data = tbi, family = binomial()) %>% summary
}

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
\"tbi\"
",
file = "../R/tbi.R")





usethis::use_data(tbi, overwrite = TRUE)

