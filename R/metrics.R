# @file metrics.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Net reclassification improvement
#'
#' @description
#' This function calculates the net reclassification improvement (NRI) metric comparing two models at a range of predicting tresholds
#' @details
#' Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param plpModel1    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param plpModel2    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param thresholds   A sequence of predicting tresholds to calcuate the NRI at each of these tresholds
#' @return
#' A list containing the NRI value and z-value (can be used for statistical significance) plus various values used to calculate the
#' NRI for each thresold. A positive value suggests the first model is better than the second model.  A negative values suggests the opposite.
#'
#' @export
NRI <- function(plpModel1, plpModel2, thresholds=seq(0,1,1/100)){

  if(!class(plpModel1)=="runPlp"){
    stop('Incorrect class for plpModel1')
  }
  if(!class(plpModel2)=="runPlp"){
    stop('Incorrect class for plpModel1')
  }
  if(nrow(plpModel1$prediction)!=nrow(plpModel2$prediction)){
    warning('Model predictions are different sizes')
  }

  ind1 <- 1:nrow(plpModel1$prediction)
  if(!is.null(plpModel1$prediction$indexes)){
    ind1 <- plpModel1$prediction$indexes<0
  }
  ind2 <- 1:nrow(plpModel2$prediction)
  if(!is.null(plpModel2$prediction$indexes)){
    ind2 <- plpModel2$prediction$indexes<0
  }

  prediction1 <- plpModel1$prediction[ind1,c('rowId','outcomeCount','value')]
  colnames(prediction1)[3] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('rowId','value')]
  colnames(prediction2)[2] <- 'Model2'

  allres <- merge(prediction1, prediction2)

  i <- 0
  results <- list()
  length(results) <- length(thresholds)
  for(threshold in thresholds){
    i <- i+1

    outcomesTab <- table(allres$Model1[allres$outcomeCount>=0]>=threshold,
          allres$Model2[allres$outcomeCount>=0]>=threshold)
    nooutcomesTab <- table(allres$Model1[allres$outcomeCount==0]>=threshold,
                      allres$Model2[allres$outcomeCount==0]>=threshold)

    pup_event <- sum(allres$Model1[allres$outcomeCount>=0]>=threshold & !allres$Model2[allres$outcomeCount>=0]>=threshold)/sum(allres$outcomeCount>=0)
    pup_noevent <- sum(allres$Model1[allres$outcomeCount==0]>=threshold & !allres$Model2[allres$outcomeCount==0]>=threshold)/sum(allres$outcomeCount==0)
    pdown_event <- sum(allres$Model1[allres$outcomeCount>=0]<threshold & !allres$Model2[allres$outcomeCount>=0]<threshold)/sum(allres$outcomeCount>=0)
    pdown_noevent <- sum(allres$Model1[allres$outcomeCount==0]<threshold & !allres$Model2[allres$outcomeCount==0]<threshold)/sum(allres$outcomeCount==0)

    nri <- (pup_event-pdown_event)-(pup_noevent-pdown_noevent)

    z <- nri/(sqrt((pup_event+pdown_event)/sum(allres$outcomeCount>=0) + (pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0)))

    z_events <- (pup_event-pdown_event)/sqrt((pup_event+pdown_event)/sum(allres$outcomeCount>=0))
    z_noevents <- (-pup_noevent+pdown_noevent)/sqrt((pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0))

    results[[i]]  <- list(threshold=threshold, nri = nri,
                          z=z, z_events=z_events, z_noevents = z_noevents,
                          outcomesTab=outcomesTab, nooutcomesTab=nooutcomesTab)
  }

  return(results)
}


#' integrated discrimination improvement- no thresholds needed (an approximation)
#'
#' @description
#' This function calculates the ntegrated discrimination improvement (IDI) metric comparing two models
#' @details
#' Users need to input two trained models (the output of runPlp()) or the output of running an existing model
#'
#' @param plpModel1    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param plpModel2    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @return
#' A list containing the IDI value and z-value (can be used for statistical significance). A positive value suggests the first model is better than the second model.  A negative values suggests the opposite.
#'
#' @export
IDI <- function(plpModel1, plpModel2){

  if(!class(plpModel1)=="runPlp"){
    stop('Incorrect class for plpModel1')
  }
  if(!class(plpModel2)=="runPlp"){
    stop('Incorrect class for plpModel1')
  }
  if(nrow(plpModel1$prediction)!=nrow(plpModel2$prediction)){
    warning('Model predictions are different sizes')
  }

  ind1 <- 1:nrow(plpModel1$prediction)
  if(!is.null(plpModel1$prediction$indexes)){
    ind1 <- plpModel1$prediction$indexes<0
  }
  ind2 <- 1:nrow(plpModel2$prediction)
  if(!is.null(plpModel2$prediction$indexes)){
    ind2 <- plpModel2$prediction$indexes<0
  }

  p1_events <- mean(plpModel1$prediction$value[plpModel1$prediction$outcomeCount>=0 & ind1])
  p1_noevents <- mean(plpModel1$prediction$value[plpModel1$prediction$outcomeCount==0 & ind1])

  p2_events <- mean(plpModel2$prediction$value[plpModel2$prediction$outcomeCount>=0 & ind2])
  p2_noevents <- mean(plpModel2$prediction$value[plpModel2$prediction$outcomeCount==0 & ind2])


  IDI <- (p1_events-p1_noevents)-(p2_events-p2_noevents)

  prediction1 <- plpModel1$prediction[ind1,c('rowId','outcomeCount','value')]
  colnames(prediction1)[3] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('rowId','value')]
  colnames(prediction2)[2] <- 'Model2'
  allres <- merge(prediction1, prediction2)

  se_events <- sd(allres$Model1[allres$outcomeCount>=0]-allres$Model2[allres$outcomeCount>=0])/sqrt(sum(allres$outcomeCount>=0))
  se_noevents <- sd(allres$Model1[allres$outcomeCount==0]-allres$Model2[allres$outcomeCount==0])/sqrt(sum(allres$outcomeCount==0))

  z <- IDI/sqrt(se_events^2+se_noevents^2)

  return(list(IDI=IDI, z=z))
}

#' multiple integrated discrimination improvement
#'
#' @description
#' This function calculates the ntegrated discrimination improvement (IDI) metric for all model comparisons
#' @details
#' Users need to input a lsit of two or more trained models (the output of runPlp()) or the output of running an existing model
#'
#' @param plpRunList    A list of objects returned  runPlp() containing the trained model or the output when implementing an existing model
#' @return
#' A data.frame containing the columns: Model1, Model2, IDI value and z-value (can be used for statistical significance). A positive value suggests Model1 is better than Model2.  A negative values suggests the opposite.
#'
#' @export
multipleIDI <- function(plpRunList){
  if(class(plpRunList)!='list')
    stop('List of models required use IDI for two models')

  results <- c()
  for(i in 1:(length(plpRunList)-1)){
    for(j in (i+1):length(plpRunList)){
      idi <- IDI(plpModel1 = plpRunList[[i]], plpModel2 = plpRunList[[j]])
      result <- c(Model1=paste0('Model ',i), Model2=paste0('Model ',j), ICI=idi$IDI, z=idi$z)
      results <- rbind(results, result)
      }}

  return(results)
}

#' The net benefit for a model
#'
#' @description
#' This function calculates the net benefit for a model
#' @details
#' Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlp    A object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @return
#' A data.frame containing the columns: pt, netbenefit, treatall and modelId.  Pt is the probability treshold or a scaled risk score (if using an existing model)
#'
#' @export
extractNetBenefit <- function(runPlp, type=NULL, modelId=NULL){
  #[TODO] add input test
  if(missing(runPlp))
    stop('runPlp Missing')
  if(class(runPlp)!='runPlp')
    stop('Incorrect class')

  data <- runPlp$performanceEvaluation$thresholdSummary

  if(!is.null(type)){
    if(!is.null(data$Eval[1])){
      data <- data[data$Eval==type,]
    }
  }

  pt <- data$predictionThreshold
  TP <- data$truePositiveCount
  FP <- data$falsePositiveCount
  n <- data$positiveCount + data$negativeCount

  treatAll <- data$trueCount/n-data$falseCount/n*(pt/(1-pt))

  if(!is.null(modelId[1])){
    netbenefit <- data.frame(modelId=modelId, pt=pt, netBenefit=TP/n-(FP/n)*(pt/(1-pt)),
                             treatAll=treatAll)
  }else{
    netbenefit <- data.frame(pt=pt, netBenefit=TP/n-(FP/n)*(pt/(1-pt)),
                             treatAll=treatAll)
  }

  return(netbenefit)
}




