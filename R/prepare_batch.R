#' Import and prepare a batch for analysis, this function replaces batchPrep()
#'
#' @param folder A path to a folder containing multiple exports from ABI7500 or Quantstudio 5
#' @param progress logical, if TRUE a progressbar is shown works only on windows
#' @param equipment Specify from what equipment the export originates, "ABI" for ABI7500, "quant" for quantstudio 5.
#' @param ... Argumnts passed to read_[EQUIPMENT]
#'
#' @import "dplyr"
#' @export
prepare_batch <- function(folder, ..., progress=TRUE, equipment="ABI"){


  if(equipment=="ABI"){

    # list all files to be prepared
    files <- list.files(folder)

    # empty list to store data
    dat <- list()

    ## Initialize a Progress Bar
    if(progress==TRUE){
      pb <- txtProgressBar(min=0, max=length(files), style=3)


      # loop trough file list and extractRawData
      for(i in 1:length(files)){
        dat[[i]] <- read_ABI(paste(folder,"/", files[i], sep=""), ...)

        setTxtProgressBar(pb, i)

      }

      # compile to one data.frame
      close(pb)
      data <- data.frame(dplyr::bind_rows(dat))

    }
    if(progress==FALSE){

      # loop trough file list and extractRawData
      for(i in 1:length(files)){
        dat[[i]] <- read_ABI(paste(folder,"/", files[i], sep=""), ...)

        print(paste(i, "of", length(files)))
        flush.console()
      }

      # compile to one data.frame
      data <- data.frame(dplyr::bind_rows(dat))



    }
    # return dataframe
    data

  }else{
    if(equipment=="quant"){

      # list all files to be prepared
      files <- list.files(folder)

      # empty list to store data
      dat <- list()

      ## Initialize a Progress Bar
      if(progress==TRUE){
        pb <- txtProgressBar(min=0, max=length(files), style=3)


        # loop trough file list and extractRawData
        for(i in 1:length(files)){
          dat[[i]] <- read_quant5(paste(folder,"/", files[i], sep=""), ...)

          setTxtProgressBar(pb, i)

        }

        # compile to one data.frame
        close(pb)
        data <- data.frame(dplyr::bind_rows(dat))

      }
      if(progress==FALSE){

        # loop trough file list and extractRawData
        for(i in 1:length(files)){
          dat[[i]] <- read_quant5(paste(folder,"/", files[i], sep=""), ...)


        }

        # compile to one data.frame
        data <- data.frame(dplyr::bind_rows(dat))



      }
      # return dataframe
      data

    }
    else{
     stop("No method for specified equipment")
  }
  }
}
