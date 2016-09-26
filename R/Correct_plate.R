#' Baseline correction for the dose-response matrix of drug combinations
#'
#' A function to do baseline correction on the dose-response matrix for drug combinations with a weighted correction fator
#'
#' @param response.mat a dose-response matrix with concentrations as row names and column names.
#' @param Emin the minimal effect of the drug used in the 4-parameter log-logistic function to fit the dose-response 
#' curve. If it is not NA, it is fixed the value assigned by the user. Defaults to NA. 
#' @param Emax the maximal effect of the drug used in the 4-parameter log-logistic function to fit the dose-response 
#' curve. If it is not NA, it is fixed the value assigned by the user. Defaults to NA. 
#' @param nan.handle a parameter to specify if L.4 function or LL.4 function is used when fitting with LL.4 produces
#' NaNs.
#' @return A list of the original dose-response matrix without correction and the corrected dose-response matrix.
#' @author Liye He \email{liye.he@helsinki.fi}, Jing Tang \email{jing.tang@helsinki.fi}
#' @examples
#' data("mathews_screening_data")
#' data <- ReshapeData(mathews_screening_data)
#' data <- BaselineCorrectionSD(data$dose.response.mats[[1]])
BaselineCorrectionSD <- function (response.mat, Emin = NA, Emax = NA, nan.handle = c("LL4", "L4")) {
  pm <- response.mat
  # check if response.mat has row names and column names
  if (is.null(rownames(response.mat)) | is.null(colnames(response.mat))) {
    stop("Please provide drug contrations as row names and column names!")
  }
  nan.handle <- match.arg(nan.handle)
  single.fitted <- FittingSingleDrug(response.mat, c(NA, Emin, Emax, NA), nan.handle)

  baseline <- (min(as.numeric(single.fitted$drug.row.fitted)) +
                 min(as.numeric(single.fitted$drug.col.fitted)))/2

  # correct matrix by a weighted correction fator
  pm.corrected <- pm - ((100 - pm) / 100 * baseline)
  output <- list(original.mat = pm, corrected.mat = pm.corrected)
  return(output)
}
