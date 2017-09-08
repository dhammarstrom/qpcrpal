#' Test model fit on models prepared with qpcrpal::model_qpcr(), based on qpcR::mselect().
#'
#' @param models a list of models from model_qpcr()
#' @param targetID Col numbers corresponding to target identifier
#' @param progress Should a progress bar be shown?
#' @import "dplyr"
#' @import "qpcR"
#' @import "stringr"
#' @return A data frame with results from mselect() tests of model fit based on AICc, lower is better.
#' @export
#'
#'
test_models<-function(models, targetID=4){

  results<-list()

  pb <- txtProgressBar(min = 0, max = length(models), style = 3) #text based bar

    for(i in 1:length(models)){



    tryCatch({
      temp<-qpcR::mselect(models[[i]], do.all=TRUE, verbose=FALSE)

      temp.summary<-data.frame(sample=names(models[i]),
                               best.model=rownames(temp$retMat)[which.min(temp$retMat[,3])],
                               l4=temp$retMat[1,3],
                               l5=temp$retMat[2,3],
                               l6=temp$retMat[3,3],
                               l7=temp$retMat[4,3],
                               b4=temp$retMat[5,3],
                               b5=temp$retMat[6,3],
                               b6=temp$retMat[7,3],
                               b7=temp$retMat[8,3])

      results[[i]]<-temp.summary

    }, error=function(e){cat("ERROR: ",conditionMessage(e), "\n")})



    setTxtProgressBar(pb, i)
  }

  close(pb)

  results<-dplyr::bind_rows(results)
  results$target<-str_split_fixed(results$sample,"_", 5)[,targetID]
  return(results)

}

