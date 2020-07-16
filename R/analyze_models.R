#' Analyze models from model_qpcr() using efficiency() from the qpcR-package
#'
#' @param models a list of models created with qpcrpal::model_qpcr()
#' @param cores Number of cores to use in parallel execution of the function. Note that cores = 1 may be faster for small data sets.
#' @param type Specifies the type argument in qpcR::efficiency. Could be one of "cpD2", "cpD1", "maxE", "expR", "CQ" or "Cy0". See ?qpcR::efficiency.
#' @param ... Other arguments passed to qpcR::efficiency().

#' @import "qpcR"
#' @import "dplyr"
#' @import "parallel"
#' @export
analyze_models <- function(models, cores = "max",  type = "cpD2", ...){

  # Use maximal n cores -1 as default
  if(cores == "max") cores <- detectCores() -1
  if(cores > detectCores()){
    cores <- detectCores() -1
    warning(paste0("You have selected more cores than you have access to. Number of cores set to ", cores),
            immediate. = TRUE)
  }

  type <- type


  efficiency.trycatch <- function(x, type = type, ...) {
    tryCatch({
      x.return  <-  qpcR::efficiency(x, plot=FALSE, type = type, ...)
      return(x.return)
    },
    error = function(cond) {
      message(conditionMessage(cond))
      # In case of error this is returned
      return(list(eff = NA,
                  resVar = NA,
                  AICc = NA,
                  AIC = NA,
                  Rsq = NA,
                  Rsq.ad = NA,
                  cpD1 = NA,
                  cpD2 = NA,
                  cpE = NA,
                  cpR = NA,
                  cpT = NA,
                  Cy0 = NA,
                  cpCQ = NA,
                  cpMR = NA,
                  fluo = NA,
                  init1 = NA,
                  init2 = NA,
                  cf = NA))}
    )
  }


  # define function to retrieve data from lists
  eff.retrieve <- function(x){
    return(as.data.frame(x))
  }

  clust <- makeCluster(cores)
  # Export package functions to cluster
  clusterEvalQ(clust, {
    library(qpcR)
  })

  # Use parallel lapply
  model.fits <- parLapply(clust, models, efficiency.trycatch, type = type)
  # Stop clusters
  stopCluster(clust)
  # Bind data and return
  models.df <- cbind(data.frame(ID = names(model.fits)), dplyr::bind_rows(lapply(model.fits, eff.retrieve)))

  if(any(is.na((models.df$eff)))) {
    warning("qpcR::efficiency was unable to evaluate one or more models, check your input data.",
            call. = FALSE,
            immediate. = TRUE)
  }

  return(models.df)

}
