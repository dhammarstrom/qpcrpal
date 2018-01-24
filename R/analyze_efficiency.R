#' Calculate PCR efficiency over multiple amplification models. This function is a wrapper around qpcR:expfit()
#'
#' @param data A list of models built with prepare_batch() or read_[EQUIPMENT]()
#' @param ... Other arguments passed to qpcR::expfit()
#' @param progress Should a progress bar be shown?
#' @return A data frame with parameters from qpcR::expfit()
#' @import "tidyr"
#' @import "dplyr"
#' @import "qpcR"
#' @import "tidyr"
#' @export

analyze_efficiency<-function(data , method="outlier", progress=TRUE, ...){


  n.models<-length(data)


  results<-data.frame(ID=names(data),
                      point=rep(NA, length=n.models),
                      eff=rep(NA, length=n.models),
                      AIC=rep(NA, length=n.models),
                      resVar=rep(NA, length=n.models),
                      RMSE=rep(NA, length=n.models),
                      init=rep(NA, length=n.models))
  if(progress==TRUE){
    pb <- txtProgressBar(min = 0, max = n.models, style = 3) #text based bar

    for(i in 1:n.models){

      tryCatch({

        temp.fit<-qpcR::expfit(data[i][[1]], plot=FALSE, ...)

        results[i, 2]<-temp.fit[[1]]
        results[i, 3]<-temp.fit[[3]]
        results[i, 4]<-temp.fit[[4]]
        results[i, 5]<-temp.fit[[5]]
        results[i, 6]<-temp.fit[[6]]
        results[i, 7]<-temp.fit[[7]]

        setTxtProgressBar(pb, i)

      }, error=function(e){cat("ERROR from expfit() :",conditionMessage(e), "\n")})
    }
  }

  if(progress==FALSE){

    for(i in 1:n.models){
      tryCatch({
        temp.fit<-qpcR::expfit(data[i][[1]], plot=FALSE, ...)
        results[i, 2]<-temp.fit[[1]]
        results[i, 3]<-temp.fit[[3]]
        results[i, 4]<-temp.fit[[4]]
        results[i, 5]<-temp.fit[[5]]
        results[i, 6]<-temp.fit[[6]]
        results[i, 7]<-temp.fit[[7]]

      }, error=function(e){cat("ERROR from expfit() :",conditionMessage(e), "\n")})
    }
  }
  return(results)
}
