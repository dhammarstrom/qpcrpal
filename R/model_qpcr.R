#' Fit a model to batch of raw data using qpcR::pcrfit
#'
#' @param rawData A data frame compiled with read_*() or batchPrep()
#' @param replicate Logical value indicating if replicates should be modeled togheter
#' @param ... Other arguments passed to pcrfit()
#' @param sampleID Col numbers corresponding to ID, condition, timepoint and replicate
#' @param progress Should a progress bar be shown?
#' @import "tidyr"
#' @import "dplyr"
#' @import "qpcR"
#' @import "tidyr"
#' @export

model_qpcr<-function(rawData, sampleID=c(2,3,4,5), replicate=TRUE, progress=TRUE, ...){

  if(!"package:qpcR" %in% search()) stop("You must explicitly load the qpcR package to access models")

  if(replicate==TRUE){
    # creates a variable unique for each sample and target combination
    rawData$sampleID<-paste(rawData[,sampleID[1]],
                            rawData[,sampleID[2]],
                            rawData[,sampleID[3]],
                            rawData[,6],
                            sep="_")


    # reduce dataset to only contain replicate, sampleID, cycle and Rn
    rawData2<-rawData[,c(8, 9, 10)]



    # Creates an empty list for models
    models<-list()

    # number of unique samples and replicates
    samples<-unique(rawData2$sampleID)




    if(progress==TRUE){

      pb <- txtProgressBar(min = 0, max = length(samples), style = 3) #text based bar


      # loop for analyzing and storing data from qpcR model fit
      for(l in 1:length(samples)){

        tempData<-rawData2[rawData2$sampleID==samples[l],]

        tryCatch({
          models[[l]]<-qpcR::pcrfit(tempData, cyc=1, fluo=2, ...)
          names(models)[l]<-tempData[1,3]
        }, error=function(e){cat("ERROR from pcrfit() :",conditionMessage(e), "\n")})


        setTxtProgressBar(pb, l)


      }

      close(pb)


    }
    if(progress==FALSE){





      # loop for analyzing and storing data from qpcR model fit
      for(l in 1:length(samples)){

        tempData<-rawData2[rawData2$sampleID==samples[l],]

        tryCatch({
          models[[l]]<-qpcR::pcrfit(tempData, cyc=1, fluo=2, ...)
          names(models)[l]<-tempData[1,3]
        }, error=function(e){cat("ERROR from pcrfit() :",conditionMessage(e), "\n")})





      }




    }


    models # return list of models

  } else {

    # creates a variable unique for each sample, replicate and target combination
    rawData$sampleID<-paste(rawData[,sampleID[1]],
                            rawData[,sampleID[2]],
                            rawData[,sampleID[3]],
                            rawData[,6],
                            rawData[,sampleID[4]],
                            sep="_")


    # Reduce dataset
    rawData2<-rawData[,c(10, 8, 9)]


    # create empty list for models storage
    models<-list()

    # n of samples to be analyzed
    samples<-unique(rawData2$sampleID)

    if(progress==TRUE){
      pb <- txtProgressBar(min = 0, max = length(samples), style = 3) #text based bar


      # loop for analyzing and storing data from qpcR model fit
      for(l in 1:length(samples)){

        tempData<-rawData2[rawData2$sampleID==samples[l],]

        tryCatch({
          models[[l]]<-qpcR::pcrfit(tempData, cyc=2, fluo=3, ...)
          names(models)[l]<-tempData[1,1]
        }, error=function(e){cat("ERROR from pcrfit() :",conditionMessage(e), "\n")})


        setTxtProgressBar(pb, l)


      }

      close(pb)

    }
    if(progress==FALSE){

      # loop for analyzing and storing data from qpcR model fit
      for(l in 1:length(samples)){

        tempData<-rawData2[rawData2$sampleID==samples[l],]

        tryCatch({
          models[[l]]<-qpcR::pcrfit(tempData, cyc=2, fluo=3, ...)
          names(models)[l]<-tempData[1,1]
        }, error=function(e){cat("ERROR from pcrfit() :",conditionMessage(e), "\n")})





      }



    }

    models
  }
}
