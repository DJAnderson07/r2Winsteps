#' Law School Admission Test (LSAT), Section VI, with dummy demographic
#' variables
#' 
#' A dataset consisting of three dummy demographic variables and five item
#' responses. 
#' 
#' @format A data frame with 1000 rows and 8 columns.
#'   \describe{
#'     \item{ID}{Randomly generated person identification variable}
#' 	   \item{Sex}{Randomly generated sex for each person}
#'     \item{Ethnicity}{Randomly generated ethnicity for each person}
#' 	   \item{Item 1 - Item 5}{Item responses to the LSAT}
#' }
#' 
#' @source The demographic variables were randomly generated to help illustrate
#' the handling of demographic data in the package. Item responses were
#' obtained from the \emph{ltm} package. See the original documentation at
#' \link[ltm]{LSAT}.
"LSAT"