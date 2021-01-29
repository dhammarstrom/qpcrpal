#' Calculate PCR efficiency over multiple amplification models. This function is a wrapper around qpcR:expfit()
#'
#' @param mods A list of models built with prepare_batch() or read_[EQUIPMENT]() and the processed with model_qpcr()
#' @param method Defaults to "cpD2", see qpcR::expfit for alternatives
#' @param cores Number of cores to use in parallel execution of the function. Note that cores = 1 may be faster for small data sets.
#' @param ... Other arguments passed to qpcR::expfit()
#' @return A data frame with parameters from qpcR::expfit(), if NA are returned instead of efficiencies qpcR::expfit was unable to estimate efficiencies.
#' @import "dplyr"
#' @import "parallel"
#' @import "qpcR"
#' @export

analyze_efficiency <- function(mods, method="cpD2", cores = "max", ...){

  # Use maximal n cores -1 as default
  if(cores == "max") cores <- detectCores() -1
  if(cores > detectCores()){
    cores <- detectCores() -1
    warning(paste0("You have selected more cores than you have access to. Number of cores set to ", cores),
            immediate. = TRUE)
  }

  method <- method

  # Define function that replaces errors with NA in expfit.
  expfit.tryCatch <- function(x, method = method, ...) {
    tryCatch({
      x.return  <-  qpcR::expfit(x, method = method, plot = FALSE, ...)
      return(x.return)
    },
    error = function(cond) {
      message(conditionMessage(cond))
      # Choose a return value in case of error
      return(list(points = NA,
                  cycles = NA,
                  eff = NA,
                  AIC = NA,
                  resVar = NA,
                  RMSE = NA,
                  init = NA,
                  mod = NA))}
    )
  }
  # define function to retrieve data from lists
  eff.retrieve <- function(x){
    return(as.data.frame(x[3:7]))
  }

  # If only one core is used, use lapply
  if(cores == 1){

    # apply qpcR::expfit over all models
    eff.fits <- lapply(mods, expfit.tryCatch, method = method, ...)

    eff.df <- cbind(data.frame(ID = names(eff.fits)), bind_rows(lapply(eff.fits, eff.retrieve)))

    if(any(is.na((eff.df[,2])))) {
      warning("qpcR::expfit was unable to calculate one or more efficiencies, check your input data.",
              call. = FALSE,
              immediate. = TRUE)
    }

    return(eff.df)
  }

  # Use parallel process if cores are defined to more than 1
  if(cores > 1){
    # Create cluster based on selected number of cores
    clust <- makeCluster(cores)
    # Export package functions to cluster
    clusterEvalQ(clust, {
      library(qpcR)
    })

    # Use parallel lapply
    eff.fits <- parLapply(clust, mods, expfit.tryCatch, method = method, ...)
    # Stop clusters
    stopCluster(clust)
    # Bind data and return
    eff.df <- cbind(data.frame(ID = names(eff.fits)), bind_rows(lapply(eff.fits, eff.retrieve)))

    if(any(is.na((eff.df$eff)))) {
      warning("qpcR::expfit was unable to calculate one or more efficiencies, check your input data.",
              call. = FALSE,
              immediate. = TRUE)
    }

    return(eff.df)

  }


}

