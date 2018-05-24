# @file metrics.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' @param secondThresholds A sequence of predicting tresholds to calcuate the NRI at each of these tresholds for the second model (if NULL uses thresholds)
#' @return
#' A list containing the NRI value and z-value (can be used for statistical significance) plus various values used to calculate the
#' NRI for each thresold. A positive value suggests the first model is better than the second model.  A negative values suggests the opposite.
#'
#' @export
NRI <- function(plpModel1, plpModel2, thresholds=seq(0,1,1/100), secondThresholds=NULL){

  if(is.null(plpModel1$prediction)){
    stop('No prediction object first input')
  }
  if(is.null(plpModel2$prediction)){
    stop('No prediction object in second input')
  }
  if(nrow(plpModel1$prediction)!=nrow(plpModel2$prediction)){
    warning('Model predictions are different sizes')
  }

  if(is.null(secondThresholds)){
    secondThresholds <- thresholds
  } else {
    if(length(thresholds)!=length(secondThresholds)){
      stop('thresholds and secondThresholds length mismatch')
    }
  }

  ind1 <- 1:nrow(plpModel1$prediction)
  if(!is.null(plpModel1$prediction$indexes)){
    ind1 <- plpModel1$prediction$indexes<0
  }
  ind2 <- 1:nrow(plpModel2$prediction)
  if(!is.null(plpModel2$prediction$indexes)){
    ind2 <- plpModel2$prediction$indexes<0
  }

  prediction1 <- plpModel1$prediction[ind1,c('subjectId','cohortStartDate','outcomeCount','value')]
  colnames(prediction1)[4] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('subjectId','cohortStartDate','value')]
  colnames(prediction2)[3] <- 'Model2'

  allres <- merge(prediction1, prediction2,
                  by=c('subjectId','cohortStartDate'))

  if(nrow(allres)==0){
    stop('No subjectId and cohortStartDate in common')
  }

  results <- list()
  length(results) <- length(thresholds)
  for(i in 1:length(thresholds)){
    threshold <- thresholds[i]
    threshold2 <- secondThresholds[i]

    outcomesTab <- table(allres$Model1[allres$outcomeCount>0]>=threshold,
          allres$Model2[allres$outcomeCount>0]>=threshold2)
    nooutcomesTab <- table(allres$Model1[allres$outcomeCount==0]>=threshold,
                      allres$Model2[allres$outcomeCount==0]>=threshold2)

    pup_event <- sum(allres$Model1[allres$outcomeCount>0]>=threshold & !allres$Model2[allres$outcomeCount>0]>=threshold2)/sum(allres$outcomeCount>0)
    pup_noevent <- sum(allres$Model1[allres$outcomeCount==0]>=threshold & !allres$Model2[allres$outcomeCount==0]>=threshold2)/sum(allres$outcomeCount==0)
    pdown_event <- sum(allres$Model1[allres$outcomeCount>0]<threshold & !allres$Model2[allres$outcomeCount>0]<threshold2)/sum(allres$outcomeCount>0)
    pdown_noevent <- sum(allres$Model1[allres$outcomeCount==0]<threshold & !allres$Model2[allres$outcomeCount==0]<threshold2)/sum(allres$outcomeCount==0)

    nri <- (pup_event-pdown_event)-(pup_noevent-pdown_noevent)

    z <- nri/(sqrt((pup_event+pdown_event)/sum(allres$outcomeCount>0) + (pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0)))

    z_events <- (pup_event-pdown_event)/sqrt((pup_event+pdown_event)/sum(allres$outcomeCount>0))
    z_noevents <- (-pup_noevent+pdown_noevent)/sqrt((pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0))

    results[[i]]  <- list(threshold1=threshold, threshold2=threshold2, nri = nri,
                          z=z, z_events=z_events, z_noevents = z_noevents,
                          outcomesTab=outcomesTab, nooutcomesTab=nooutcomesTab,
                          pup_event=pup_event, pdown_event=pdown_event,
                          pup_noevent=pup_noevent , pdown_noeven=pdown_noevent)
  }

  return(results)
}

#' Net reclassification improvement 2 no thresholds
#'
#' @description This function calculates a modified net reclassification improvement (NRI) metric comparing two models at a range of predicting tresholds (simialr to library(Hmisc))
#' @details Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param plpModel1    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param plpModel2    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param minOutcomes  Must be this number of outcomes minimum for comparison
#' @return
#' A list containing the NRI value and z-value (can be used for statistical significance).
#' A positive value suggests the first model is better than the second model.  A negative values suggests the opposite.
#'
#' @export
NRI2 <- function(plpModel1, plpModel2, minOutcomes=5){

  if(is.null(plpModel1$prediction)){
    stop('No prediction object first input')
  }
  if(is.null(plpModel2$prediction)){
    stop('No prediction object in second input')
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

  prediction1 <- plpModel1$prediction[ind1,c('subjectId','cohortStartDate','outcomeCount','value')]
  colnames(prediction1)[4] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('subjectId','cohortStartDate','value')]
  colnames(prediction2)[3] <- 'Model2'

  allres <- merge(prediction1, prediction2,
                  by=c('subjectId','cohortStartDate'))

  if(nrow(allres)==0){
    stop('No subjectId and cohortStartDate in common')
  }

  if(sum(allres$outcomeCount>0)< minOutcomes){
    stop('Less outcomes that minOutcomes')
  }

  pup_event <- sum(allres$Model1[allres$outcomeCount>0]>allres$Model2[allres$outcomeCount>0])/sum(allres$outcomeCount>0)
  pup_noevent <- sum(allres$Model1[allres$outcomeCount==0]>allres$Model2[allres$outcomeCount==0])/sum(allres$outcomeCount==0)
  pdown_event <- sum(allres$Model1[allres$outcomeCount>0] < allres$Model2[allres$outcomeCount>0])/sum(allres$outcomeCount>0)
  pdown_noevent <- sum(allres$Model1[allres$outcomeCount==0] < allres$Model2[allres$outcomeCount==0])/sum(allres$outcomeCount==0)

  nri <- (pup_event-pdown_event)-(pup_noevent-pdown_noevent)

  z <- nri/(sqrt(((pup_event+pdown_event)/sum(allres$outcomeCount>0)) + ((pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0))))

  z_events <- (pup_event-pdown_event)/sqrt((pup_event+pdown_event)/sum(allres$outcomeCount>0))
  z_noevents <- (-pup_noevent+pdown_noevent)/sqrt((pup_noevent+pdown_noevent)/sum(allres$outcomeCount==0))

  results  <- list(nri = nri,
                   z=z, z_events=z_events, z_noevents = z_noevents,
                   pup_event=pup_event, pdown_event=pdown_event,
                   pup_noevent=pup_noevent , pdown_noeven=pdown_noevent)


  return(results)
}

#' Get prediction risk thresholds for quantiles
#'
#' @description This function calculates the net reclassification improvement (NRI) metric comparing two models at a range of predicting tresholds
#' @details Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param plpModel1    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param plpModel2    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param percentage   A quantile percentage (e.g., c(0.99, 0.95)) will find the threshold that only 1 percent and 5 percent of people have a risk equal to or higher than for both models
#'
#' @return
#' A data.frame with each column being the risk thresholds per model and rows corresponding to the input quantiles
#' @export
getThresholds <- function(plpModel1, plpModel2, percentage=c(0.99,0.95, 0.9,0.5)){

  ind1 <- 1:nrow(plpModel1$prediction)
  if(!is.null(plpModel1$prediction$indexes)){
    ind1 <- plpModel1$prediction$indexes<0
  }
  ind2 <- 1:nrow(plpModel2$prediction)
  if(!is.null(plpModel2$prediction$indexes)){
    ind2 <- plpModel2$prediction$indexes<0
  }

  prediction1 <- plpModel1$prediction[ind1,c('subjectId','cohortStartDate','outcomeCount','value')]
  colnames(prediction1)[4] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('subjectId','cohortStartDate','value')]
  colnames(prediction2)[3] <- 'Model2'

  allres <- merge(prediction1, prediction2,
                  by=c('subjectId','cohortStartDate'))

  if(nrow(allres)==0){
    stop('No subjectId and cohortStartDate in common')
  }

  result <- data.frame(modelThreshold = quantile(allres$Model1, probs=percentage),
                       model2Threshold = quantile(allres$Model2, probs=percentage)
                       )

  return(result)
}


#' integrated discrimination improvement- no thresholds needed (an approximation)
#'
#' @description This function calculates the ntegrated discrimination improvement (IDI) metric comparing two models
#' @details Users need to input two trained models (the output of runPlp()) or the output of running an existing model where the plpData used was the same
#'
#' @param plpModel1    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @param plpModel2    The object returned by runPlp() containing the trained model or the output when implementing an existing model
#' @return
#' A list containing the IDI value and z-value (can be used for statistical significance). A positive value suggests the first model is better than the second model.  A negative values suggests the opposite.
#'
#' @export
IDI <- function(plpModel1, plpModel2){

  if(is.null(plpModel1$prediction)){
    stop('No prediction object first input')
  }
  if(is.null(plpModel2$prediction)){
    stop('No prediction object in second input')
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

  prediction1 <- plpModel1$prediction[ind1,c('subjectId','cohortStartDate','outcomeCount','value')]
  colnames(prediction1)[4] <- 'Model1'
  prediction2 <- plpModel2$prediction[ind2,c('subjectId','cohortStartDate','value')]
  colnames(prediction2)[3] <- 'Model2'

  allres <- merge(prediction1, prediction2,
                  by=c('subjectId','cohortStartDate'))

  if(nrow(allres)==0){
    stop('No subjectId and cohortStartDate in common')
  }

  p1_events <- mean(allres$Model1[allres$outcomeCount>0])
  p1_noevents <- mean(allres$Model1[allres$outcomeCount==0])

  p2_events <- mean(allres$Model2[allres$outcomeCount>0])
  p2_noevents <- mean(allres$Model2[allres$outcomeCount==0])

  increaseEvents <- p1_events-p2_events
  decreaseNoevents <- p2_noevents-p1_noevents
  model1Diff <- (p1_events-p1_noevents)
  model2Diff <- (p2_events-p2_noevents)
  IDI <- model1Diff-model2Diff

  se_events <- sd(allres$Model1[allres$outcomeCount>0]-allres$Model2[allres$outcomeCount>0])/sqrt(sum(allres$outcomeCount>0))
  se_noevents <- sd(allres$Model1[allres$outcomeCount==0]-allres$Model2[allres$outcomeCount==0])/sqrt(sum(allres$outcomeCount==0))

  z <- IDI/sqrt(se_events^2+se_noevents^2)

  return(list(IDI=IDI, z=z, model1Diff=model1Diff, model2Diff=model2Diff,
              increaseEvents=increaseEvents,
              decreaseNoevents=decreaseNoevents))
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




