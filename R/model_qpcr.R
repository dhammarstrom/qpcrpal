#' Fit a model to batch of raw data using qpcR::pcrfit
#'
#' @param data A data frame compiled with read_*() or batchPrep()
#' @param sampleID Column numbers corresponding to ID, condition, timepoint and replicate
#' @param replicate Logical value indicating if replicates should be modeled together
#' @param cores Number of cores to use by the parallel package
#' @param ... Other arguments passed to qpcR::pcrfit()
#' @import "parallel"
#' @import "dplyr"
#' @import "qpcR"
#' @export

model_qpcr <- function(data, sampleID=c(2,3,4,5), replicate=TRUE, cores = 1, ...) {

  if(!"package:qpcR" %in% search()) stop("You must explicitly load the qpcR package to access models")

  if(cores == "max") cores <- detectCores() -1
  if(cores > detectCores()){
    cores <- detectCores() -1
    warning(paste0("You have selected more cores than you have access to. Number of cores set to ", cores),
            immediate. = TRUE)
  }


  rawData <- data.frame(data)

  if(replicate==TRUE){
    # creates a variable unique for each sample and target combination
    rawData$sampleID <- paste(rawData[,sampleID[1]],
                            rawData[,sampleID[2]],
                            rawData[,sampleID[3]],
                            rawData[,6],
                            sep = "_")


    # reduce dataset to only contain replicate, sampleID, cycle and Rn
    rawData2<-rawData[,c(8, 9, 10)]}

  if(replicate == FALSE){

    # creates a variable unique for each sample, replicate and target combination
    rawData$sampleID <- paste(rawData[,sampleID[1]],
                              rawData[,sampleID[2]],
                              rawData[,sampleID[3]],
                              rawData[,sampleID[4]],
                              rawData[,6],
                              sep = "_")

    # reduce dataset to only contain replicate, sampleID, cycle and Rn
    rawData2 <- rawData[,c(8, 9, 10)]}



  # Split the data set into a list
  rd <- dplyr::group_split(rawData2, sampleID)


  # Define a function for parLapply
  pcrfit.trycatch <- function(x, ...){

    tryCatch({
      model <- qpcR::pcrfit(data.frame(x), cyc=1, fluo=2, ...)

      return(model)

    }, error=function(e){cat("ERROR from pcrfit() :",conditionMessage(e), "\n")})

  }


  # Create cluster based on selected number of cores
  clust <- makeCluster(cores)
  # Export package functions to cluster
  clusterEvalQ(clust, {
    library(qpcR)
  })


  # Use parallel lapply
  models <- parLapply(clust, rd, pcrfit.trycatch)
  # Stop clusters
  stopCluster(clust)
    # Name all models
  names(models) <- as.character(unlist(lapply(rd, FUN = function(x){x[1,3]})))

  # Return list of models
  return(models)

}

