#' Fitting single drug dose-response curve
#'
#' A function to fit single drug dose-response curve with observed response data
#'
#' @param response.mat a matrix with first column as the drug concentrations and second column as the observed responses
#' @param fixed a parameter to specify which parameters are fixed and at what value they are fixed. NAs for parameter
#' that are not fixed.
#' @param nan.handle a parameter to specify if L.4 function or LL.4 function is used when fitting with LL.4 produces
#' NaNs.
#' @details Single drug dose-response curve is fitted with a commonly used 4-paramter log-losistic (4PL) function.
#' @return Fitted responses and fitted models are returned.
#' @author Liye He \email{liye.he@helsinki.fi}
#' @references Seber, G. A. F. and Wild, C. J (1989) Nonlinear Regression, New York: Wiley \& Sons (p. 330).
#' @examples
#' data("mathews_screening_data")
#' data <- ReshapeData(mathews_screening_data)
#' single.drug.fitted <- FittingSingleDrug(data$dose.response.mats[[1]])
FittingSingleDrug <- function(response.mat, fixed = c(NA, NA, NA, NA), nan.handle = c("LL4", "L4")){
  r.num <- nrow(response.mat)
  c.num <- ncol(response.mat)
  # column drug
  drug.col <- cbind(as.numeric(colnames(response.mat)[-1]), response.mat[1, 2:c.num])
  colnames(drug.col) <- c("conc","effect")
  drug.col <- as.data.frame(apply(drug.col, 2, as.numeric))
  if (var(drug.col$effect) == 0) {
    drug.col$effect[nrow(drug.col)] <- drug.col$effect[nrow(drug.col)] + 10^-10
  }
  
  nan.handle <- match.arg(nan.handle)
  
  drug.col.model <- tryCatch({
    drm(effect ~ conc, data = drug.col, fct = LL.4(fixed = fixed), 
                          na.action=na.omit,control = drmc(errorm = FALSE))
  }, warning = function(w) {
      
    
    if(nan.handle == "L4"){
        drm(effect ~ conc, data = drug.col, fct = L.4(fixed = fixed),
        na.action=na.omit,control = drmc(errorm = FALSE))
    } else {
        
        drm(effect ~ conc, data = drug.col, fct = LL.4(fixed = fixed),
        na.action=na.omit,control = drmc(errorm = FALSE))
    }
    
  }, error = function(e) {
      
    drm(effect ~ conc, data = drug.col, fct = L.4(fixed = fixed), 
        na.action=na.omit,control = drmc(errorm = FALSE))
  })
  drug.col.fitted <- suppressWarnings(fitted(drug.col.model))

  # row drugs
  drug.row <- cbind(as.numeric(rownames(response.mat)[-1]), response.mat[2:r.num, 1])
  colnames(drug.row) <- c("conc","effect")
  drug.row <- as.data.frame(apply(drug.row, 2, as.numeric))
  if (var(drug.row$effect) == 0) {
    drug.row$effect[nrow(drug.row)] <- drug.row$effect[nrow(drug.row)] + 10^-10
  }
  drug.row.model <- tryCatch({
    drm(effect ~ conc, data = drug.row, fct = LL.4(fixed = fixed), 
                          na.action=na.omit,control = drmc(errorm = FALSE))
  }, warning = function(w) {
      if(nan.handle == "L4"){
          drm(effect ~ conc, data = drug.row, fct = L.4(fixed = fixed),
          na.action=na.omit,control = drmc(errorm = FALSE))
      } else {
          
          drm(effect ~ conc, data = drug.row, fct = LL.4(fixed = fixed),
          na.action=na.omit,control = drmc(errorm = FALSE))
      }

  }, error = function(e) {
    drm(effect ~ conc, data = drug.row, fct = L.4(fixed = fixed), 
                          na.action=na.omit,control = drmc(errorm = FALSE))
  })

  drug.row.fitted <- suppressWarnings(fitted(drug.row.model))
  return(list(drug.row.fitted = drug.row.fitted,
              drug.row.model = drug.row.model,
              drug.col.model = drug.col.model,
              drug.col.fitted = drug.col.fitted))
}
