## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(qpcR)
library(qpcrpal)

## ----import and batch prep-----------------------------------------------
batch<-prepare_batch("qpcr_exports", progress=FALSE)

## ----show_import---------------------------------------------------------
head(batch)

## ----fit_models----------------------------------------------------------
models<-model_qpcr(batch, model=l4, progress=FALSE)

## ----analyze_models------------------------------------------------------
results<-analyze_models(models, progress=FALSE)

## ----show_results--------------------------------------------------------
results[c(1:6),c(1,9)]

## ----pipe_example, eval=FALSE--------------------------------------------
#  results<-prepare_batch("qpcr_exports", progress=FALSE)%>%
#    model_qpcr(model=l4, progress=FALSE)%>%
#    analyze_models(progress=FALSE)

