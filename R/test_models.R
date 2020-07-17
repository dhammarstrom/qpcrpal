#' Test model fit on models prepared with qpcrpal::model_qpcr(), based on qpcR::mselect().
#'
#' @param models a list of models from model_qpcr()
#' @param targetID Column number corresponding to target (gene) identifier
#' @param sep A character that separates information in the identifiers (names of lists)
#' @param cores Specifies the number of cores used in model testing
#' @import "dplyr"
#' @import "ggplot2
#' @import "qpcR"
#' @import "stringr"
#' @import "parallel"
#' @return A list with summary counts of best-fit models per target (based on AICc), a figure with the same information and raw data if one wants to comute best fit models based on other criterias. Raw data comes from qpcR::mselect.
#' @export
#'
#'
test_models <- function(models, targetID = 4, sep = "_", cores = "max"){


  # Use maximal n cores -1 as default
  if(cores == "max") cores <- detectCores() -1
  if(cores > detectCores()){
    cores <- detectCores() -1
    warning(paste0("You have selected more cores than you have access to. Number of cores set to ", cores),
            immediate. = TRUE)
  }



  # Defines function for lapply
  mselect.trycatch <- function(x){

    tryCatch({

      temp <- data.frame(qpcR::mselect(x, do.all=TRUE, verbose=FALSE)$retMat)

      temp$model <- rownames(temp)
      rownames(temp) <- NULL

      return(temp)
    },
    error = function(cond) {
      message(conditionMessage(cond))
      # Choose a return value in case of error
      return(data.frame(logLik      = rep(NA, 8),
                        AIC         = rep(NA, 8),
                        AICc        = rep(NA, 8),
                        resVar      = rep(NA, 8),
                        ftest       = rep(NA, 8),
                        LR          = rep(NA, 8),
                        Chisq       = rep(NA, 8),
                        AIC.weights = rep(NA, 8),
                        AICc.weights= rep(NA, 8),
                        model       =c("l4", "l5", "l6", "l7", "b4", "b5", "b6", "b7")))}
    )
  }


  clust <- makeCluster(cores)
  # Export package functions to cluster
  clusterEvalQ(clust, {
    library(qpcR)
  })

  # Use parallel lapply
  model.tests <- parLapply(clust, models, mselect.trycatch)

  # Stop clusters
  stopCluster(clust)

  # Combine data from each data frame.
  model.tests.results <- bind_rows(mapply(cbind, model.tests, "ID" = names(models), SIMPLIFY = F))


  # Creates a summary data frame
  model.combined.results <-  model.tests.results %>%
    group_by(ID) %>%
    dplyr::slice(which.min(AICc)) %>%
    # Separate the id column to lettered ids by the separator
    tidyr::separate(ID, into = letters[1:(stringr::str_count(names(model.tests[1]), sep) + 1)],  sep = "_") %>%
    # Group by the target id (based on info given in the function arguments)
    dplyr::group_by(.data[[(letters[1:(stringr::str_count(names(model.tests[1]), sep) + 1)][targetID])]], model) %>%
    dplyr::count() %>%
    dplyr::select(target = as.name(letters[1:(stringr::str_count(names(model.tests[1]), sep) + 1)][targetID]), model, n) %>%
    data.frame()

  fig <- model.combined.results %>%
    ggplot2::ggplot(ggplot2::aes(n, target, fill = model)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::labs(x = "Number of best-fit models",
                  y = "Target",
                  fill = "Model",
                  title = "Summary of best-fit tests") +
    ggplot2::theme_classic()

  # Return a list
  return(list(results = model.combined.results,
              figure = fig,
              raw.data = model.tests.results))


}

