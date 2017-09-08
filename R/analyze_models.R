#' Analyze models from model_qpcr() using efficiency() from the qpcR-package
#'
#' @param models a list of models created with qpcrpal::model_qpcr()
#' @progress Should a progress bar be shown?
#' @param ... Other arguments passed to qpcr::efficiency()
#' @param sampleID Col numbers corresponding to ID, condition, timepoint and replicate

#' @import "qpcR"
#' @export

analyze_models<-function(models, progress=TRUE, ...){

  samples<-length(models)

  data<-data.frame(ID=rep(NA, length=length(samples)),
                   eff=rep(NA, length=length(samples)),
                   resVar=rep(NA, length=length(samples)),
                   AICc=rep(NA, length=length(samples)),
                   AIC=rep(NA, length=length(samples)),
                   Rsq=rep(NA, length=length(samples)),
                   Rsq.ad=rep(NA, length=length(samples)),
                   cpD1=rep(NA, length=length(samples)),
                   cpD2=rep(NA, length=length(samples)),
                   cpE=rep(NA, length=length(samples)),
                   cpR=rep(NA, length=length(samples)),
                   cpT=rep(NA, length=length(samples)),
                   Cy0=rep(NA, length=length(samples)),
                   cpCQ=rep(NA, length=length(samples)),
                   cpMR=rep(NA, length=length(samples)),
                   fluo=rep(NA, length=length(samples)),
                   init1=rep(NA, length=length(samples)),
                   init2=rep(NA, length=length(samples)),
                   cf=rep(NA, length=length(samples)),
                   eff.sliwin=rep(NA, length=length(samples)))

  ## Initialize a Progress Bar
if(progress==TRUE){
  pb <- txtProgressBar(min=0, max=samples, initial=0, style=3)

  for(l in 1:samples){

    tryCatch({
      data[l,c(2:19)]<-qpcR::efficiency(models[[l]], plot=FALSE, ...)
      data[l,1]<-names(models)[l]

    }, error=function(e){
      cat("Error :",conditionMessage(e), "\n")})


    setTxtProgressBar(pb, l)
  }
  close(pb)


}
  if(progress==FALSE){


  for(l in 1:samples){

    tryCatch({
      data[l,c(2:19)]<-qpcR::efficiency(models[[l]], plot=FALSE, ...)
      data[l,1]<-names(models)[l]

    }, error=function(e){
      cat("Error :",conditionMessage(e), "\n")})

  }



}

    data
}
