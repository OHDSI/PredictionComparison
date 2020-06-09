# @file plotting.R
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

#' Plot a decsion curve for multiple models
#'
#' @description
#' This function plots a decision curve
#' @details
#' Users need to input a list of trained models (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlpList    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param names         A string or vector of model names corresponding to the runPlpList
#' @param type          A strng or vector of evaluation types (train/test/validation) to filter for each runPlp
#' @return
#' A plot is returned with the model decision cruves
#'
#' @export
plotDecisionCurve <- function(runPlpList, names=NULL, type=NULL){
  if(checkPlpObject(runPlpList)){
    if(is.null(names[1])){
      modelId = 'Model 1'
    }
    decisionCurve <- extractNetBenefit(runPlpList,type=type, modelId=modelId)
  } else if(class(runPlpList)=="list" & checkPlpObject(runPlpList[[1]])){
    if(length(type)==1){type <- rep(type, length(runPlpList))}
    if(is.null(names[1])){
      modelId = paste0('Model ', 1:length(runPlpList))
    }else{
      modelId <- names # TODO: add a check for valid length
    }
    decisionCurve  <- lapply(1:length(runPlpList), function(i) extractNetBenefit(runPlpList[[i]],type[i], modelId=modelId[i]))
    decisionCurve <- do.call(rbind,decisionCurve)
  } else {
    stop('Incorrect models class')
  }

  #plotResult <-
  plotVal <- ggplot2::ggplot(data=decisionCurve, ggplot2::aes(x=pt, y=netBenefit, group=modelId, color=modelId)) +
    ggplot2::geom_line(size=1) + ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=00, linetype="dashed",
               color = "black", size=1) +
    ggplot2::geom_line(data=decisionCurve[decisionCurve$modelId==unique(decisionCurve$modelId)[1],],
                       ggplot2::aes(x=pt, y=treatAll), color='black', linetype = "dotted",
                       size=1)+
    ggplot2::ylim(min(decisionCurve$netBenefit)*1.1,max(decisionCurve$netBenefit)*1.1) + #c(-2,4)
    ggplot2::xlab("Probability Threshold (or scaled risk score)") +
    ggplot2::labs(color = "Model Id",
                  caption = "The black dashed line is treat none and the black dotted line is treat all")

  return(plotVal)
}

#' Plot a ROC plot for multiple models
#'
#' @description
#' This function plots multiple ROC plots for comparison
#' @details
#' Users need to input a list of trained models (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlpList    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param names         A string or vector of model names corresponding to the runPlpList
#' @param type          A strng or vector of evaluation types (train/test/validation) to filter for each runPlp
#' @param grid          Whether to plot each model seperately using a grid or overlay the plots (grid=F)
#' @param ncol          The number of columns for the grid
#'
#' @return
#' A plot is returned with the ROC plots for each model
#'
#' @export
plotMultipleRoc <- function(runPlpList, names, type='test', grid=T, ncol=2){
  if(!checkPlpObject(runPlpList[[1]]))
    stop('List of runPlp objects requires - if single runPlp use plotSparseRoc')

  if(missing(names)){
    names <- paste0('Model ', 1:length(runPlpList))
  }

  if(grid==T){
    rocPlot <- list()
    length(rocPlot) <- length(runPlpList)
    for(i in 1:length(runPlpList)){

      if(is.null(runPlpList[[i]]$performanceEvaluation$thresholdSummary$Eval[1])){
        warning('Eval missing from thresholdSummary so using all values')
        runPlpList[[i]]$performanceEvaluation$thresholdSummary$Eval <- type[i]
      }

      rocPlot[[i]] <- PatientLevelPrediction::plotSparseRoc(runPlpList[[i]]$performanceEvaluation, type=type[i]) +
        ggplot2::ggtitle(names[i], subtitle = NULL)
    }
    result <- do.call(gridExtra::grid.arrange, c(rocPlot, list(ncol=ncol)))
    return(result)
  }

  # otherwise overlay plots...
  result <- do.call(plotRocs, list(evaluationList=runPlpList,modelNames=names, type=type))
  return(result)

}

#' Plot a calibration plot for multiple models
#'
#' @description
#' This function plots multiple calibration plots for comparison
#' @details
#' Users need to input a list of trained models (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlpList    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param names         A string or vector of model names corresponding to the runPlpList
#' @param type          A strng or vector of evaluation types (train/test/validation) to filter for each runPlp
#' @param grid          Whether to plot each model seperately using a grid or overlay the plots (grid=F)
#' @param ncol          The number of columns for the grid
#'
#' @return
#' A plot is returned with the calibration plots for each model
#'
#' @export
plotMultipleCal <- function(runPlpList, names, type='test', grid=F, ncol=2){
  if(!checkPlpObject(runPlpList[[1]]))
    stop('List of runPlp objects requires - if single runPlp use plotSparseCalibration2')

  if(missing(names)){
    names <- paste0('Model ', 1:length(runPlpList))
  }

  if(grid==T){
    calPlot <- list()
    length(calPlot) <- length(runPlpList)
    for(i in 1:length(runPlpList)){

      if(is.null(runPlpList[[i]]$performanceEvaluation$thresholdSummary$Eval[1])){
        warning('Eval missing from thresholdSummary so using all values')
        runPlpList[[i]]$performanceEvaluation$thresholdSummary$Eval <- type[i]
      }

      calPlot[[i]] <- PatientLevelPrediction::plotSparseCalibration2(runPlpList[[i]]$performanceEvaluation, type=type[i]) +
        ggplot2::ggtitle(names[i], subtitle = NULL)
    }
    result <- do.call(gridExtra::grid.arrange, c(calPlot, list(ncol=ncol)))
    return(result)
  }

  # otherwise overlay plots...
  result <- do.call(plotCals, list(evaluationList=runPlpList,modelNames=names, type=type))
  return(result)

}



#' Plot a the model performance visulisation stratified by risk
#'
#' @description
#' This function plots a visulisation of model performance stratified by risk
#' @details
#' Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlp    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param specificity   The required specificity value for the model
#'
#' @return
#' A plot is returned with the overal outcome risk and observed risk in different model risk groups
#'
#' @export
plotModelStratification1 <- function(runPlp, specificity=0.95){
  vals <- getValues(runPlp$performanceEvaluation,specificity)

  mytable <- data.frame(Threshold=round(vals$predictionThreshold, digits=2),
                        TP = vals$truePositiveCount,
                        FP = vals$falsePositiveCount,
                        FN = vals$falseNegativeCount,
                        TN = vals$trueNegativeCount,
                        sensitivity = round(vals$sensitivity*100),
                        specificity = round(vals$specificity*100)
  )

  mydata <- data.frame(risk=c(rep('high',2), rep('low',2)),
                       type=c('P','N','P','N'),
                       value=c(vals$truePositiveCount,
                               vals$falsePositiveCount,
                               vals$falseNegativeCount,
                               vals$trueNegativeCount))

  ratio <- 0.1 # create this ratio
  mydata <- data.frame(GroundTruth=c(rep('Have Outcome',2), rep('No Outcome',2)),
                       Prediction=c('In High Risk','In Low Risk','In High Risk','In Low Risk'),
                       value=c(vals$N*ratio*vals$truePositiveCount/vals$trueCount,
                               vals$N*ratio*vals$falseNegativeCount/vals$trueCount,
                               vals$N*(1-ratio)*vals$falsePositiveCount/vals$falseCount,
                               vals$N*(1-ratio)*vals$trueNegativeCount/vals$falseCount),
                       cover=c(vals$truePositiveCount/vals$trueCount*100,
                               vals$truePositiveCount/vals$trueCount*100,
                               vals$falsePositiveCount/vals$falseCount*100,
                               vals$falsePositiveCount/vals$falseCount*100)
  )

  # find per x number
  sizeTemp <- sapply(seq(1000,1000000,1000), function(perVal) round(perVal*(vals$fn+vals$tp)/vals$N))
  size <- seq(1000,1000000,1000)[which.min(sizeTemp>0)]
  inc <- sizeTemp[which.min(sizeTemp>0)]
  xsize <- floor(sqrt(size)/10)*10
  ysize <- xsize

  data <- data.frame(x=rep(1:xsize,ysize),
                     y= do.call(c,lapply(1:ysize, function(i) rep(i,xsize))),
                     color = c(rep('Outcome',inc),rep('No Outcome', xsize*ysize-inc))
  )
  plot1 <- ggplot(data, aes(y = y, x = x, fill = color)) +
    geom_tile(color = "white") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title=element_blank())+
    scale_fill_manual(values=c("#b3ff99", "#B22222"))
  tbl <- tableGrob(mytable, rows=NULL)

  plot2 <- ggplot(mydata, aes(GroundTruth, value))+
    geom_bar(stat = "identity", aes(fill = Prediction)) +
    geom_text(x=1,y=0, label=paste0(round(mydata$cover[1],1),'% in High Risk'))  +
    geom_text(x=2,y=0, label=paste0(round(mydata$cover[3],1),'% in High Risk')) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_fill_manual(values=c("#B22222", "#b3ff99"))

  resultPlot <- grid.arrange(tbl, arrangeGrob(plot1, top="General Population Risk"),
                             arrangeGrob(plot2, top="Stratified By Risk Model"),
                             nrow = 3, heights = c(2, 5,5))

  return(resultPlot)
}

#' Plot a the model performance visulisation stratified by risk
#'
#' @description
#' This function plots a visulisation of model performance stratified by risk
#' @details
#' Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlp    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param specificity   The required specificity value for the model
#'
#' @return
#' A plot is returned with the overal outcome risk and observed risk in different model risk groups
#'
#' @export
plotModelStratification2 <- function(runPlp, specificity=0.95){
  vals <- getValues(runPlp$performanceEvaluation,specificity)

  mytable <- data.frame(Threshold=round(vals$predictionThreshold, digits=2),
                        TP = vals$truePositiveCount,
                        FP = vals$falsePositiveCount,
                        FN = vals$falseNegativeCount,
                        TN = vals$trueNegativeCount,
                        sensitivity = round(vals$sensitivity*100),
                        specificity = round(vals$specificity*100)
  )

  # find per x number
  sizeTemp <- sapply(seq(1000,1000000,1000), function(perVal) round(perVal*(vals$fn+vals$tp)/vals$N))
  size <- seq(1000,1000000,1000)[which.min(sizeTemp>0)]
  inc <- sizeTemp[which.min(sizeTemp>0)]
  xsize <- floor(sqrt(size)/10)*10
  ysize <- xsize

  data <- data.frame(x=rep(1:xsize,ysize),
                     y= do.call(c,lapply(1:ysize, function(i) rep(i,xsize))),
                     color = c(rep('Outcome',inc),rep('No Outcome', xsize*ysize-inc))
  )
  incidence <- vals$trueCount[1]/(vals$trueCount[1]+vals$falseCount[1])

  plot1 <- ggplot(data, aes(y = y, x = x, fill = color)) +
    geom_tile(color = "white") +
    geom_text(x=xsize/2, y=ysize/2, label=paste0('Outcome Rate: ',round(incidence*100,1),'%'))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title=element_blank())+
    scale_fill_manual(values=c("#b3ff99", "#B22222"))
  tbl <- tableGrob(mytable, rows=NULL)

  inc2 <- round(vals$truePositiveCount/vals$positiveCount*ysize*xsize)
  data2 <- data.frame(x=rep(1:xsize,ysize),
                      y= do.call(c,lapply(1:ysize, function(i) rep(i,xsize))),
                      color = c(rep('Outcome',inc2),rep('No Outcome', xsize*ysize-inc2))
  )
  plot2 <- ggplot(data2, aes(y = y, x = x, fill = color)) +
    geom_tile(color = "white") +
    geom_text(x=xsize/2, y=ysize/2, label=paste0('Outcome Rate: ',round(inc2/(ysize*xsize)*100,1),'%'))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title=element_blank(),
          legend.position="none")+
    scale_fill_manual(values=c("#b3ff99", "#B22222"))

  inc3 <- round(vals$falseNegativeCount/vals$negativeCount*ysize*xsize)
  data3 <- data.frame(x=rep(1:xsize,ysize),
                      y= do.call(c,lapply(1:ysize, function(i) rep(i,xsize))),
                      color = c(rep('Outcome',inc3),rep('No Outcome', xsize*ysize-inc3))
  )
  plot3 <- ggplot(data3, aes(y = y, x = x, fill = color)) +
    geom_tile(color = "white") +
    geom_text(x=xsize/2, y=ysize/2, label=paste0('Outcome Rate: ',round(inc3/(ysize*xsize)*100,1),'%'))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title=element_blank(),
          legend.position="none")+
    scale_fill_manual(values=c("#b3ff99", "#B22222"))


  # make the size relative to sensitivity...
  widthv=round(vals$truePositiveCount/vals$trueCount*100)

  #resultPlot <- grid.arrange(tbl, arrangeGrob(plot1, top="General Population Risk"),
  #                           arrangeGrob(plot2, top="High Risk Population"),
  #                           arrangeGrob(plot3, top="Low Risk Population"),
  #                           nrow = 4, heights = c(1, 5,5,5))


  round(vals$positiveCount/vals$N*100, 1)
  resultPlot <- grid.arrange(tbl, arrangeGrob(plot1, top="General Population Risk"),
                             arrangeGrob(plot2,plot3, nrow=1,
                                         top="Stratified Risk Population",
                                         left=paste0('High Risk:',round(vals$positiveCount/vals$N*100, 1), '% of Pop' ),
                                         right=paste0('Low Risk:',round(vals$negativeCount/vals$N*100, 1), '% of Pop' ),
                                         widths=c(widthv/10,10-widthv/10)),
                             nrow = 3, heights = c(2, 5, 5))

  return(resultPlot)
}

#' Plot a the model performance visulisation stratified by risk
#'
#' @description
#' This function plots a visulisation of model performance stratified by risk
#' @details
#' Users need to input a trained model (the output of runPlp()) or the output of running an existing model
#'
#' @param runPlp    An object of class 'runPlp' or list of object of class 'runPlp' returned by implementing runPlp() or the output when implementing an existing model
#' @param type      test/train/validation
#' @param riskGroups ...
#'
#' @return
#' A plot is returned with the overal outcome risk and observed risk in different model risk groups
#'
#' @export
plotModelStratification3 <- function(runPlp, type='test',
                                     riskGroups = NULL){

  val2 <- runPlp$performanceEvaluation$thresholdSummary
  if(length(val2$Eval)>0){
    val2 <- val2[val2$Eval==type,]
  }

  incidence <- val2$trueCount[1]/(val2$trueCount[1]+val2$falseCount[1])
  sizeTemp <- sapply(seq(1000,1000000,1000), function(perVal) round(perVal*(incidence)))
  size <- seq(1000,1000000,1000)[which.min(sizeTemp>0)]
  inc <- sizeTemp[which.min(sizeTemp>0)]
  xsize <- floor(sqrt(size)/10)*10
  ysize <- xsize

  data <- data.frame(x=rep(1:xsize,ysize),
                     y= do.call(c,lapply(1:ysize, function(i) rep(i,xsize))),
                     color = c(rep('Outcome',inc),rep('No Outcome', xsize*ysize-inc))
  )
  plot1 <- ggplot2::ggplot(data, ggplot2::aes(y = y, x = x, fill = color)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(x=xsize/2, y=ysize/2, label=paste0('Outcome Rate: ',round(incidence*100,1),'%'))+
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank(),
          legend.title=ggplot2::element_blank())+
    ggplot2::scale_fill_manual(values=c("#b3ff99", "#B22222"))


  # ten values 0-x1, x1-x2, x2-x3, ...
  # size: #num people with these values
  # risk: #num of outcomes between these values/ size
  #vals <- val2[c(seq(1,100,10),100),]
  if(is.null(riskGroups)){
    vals <- val2[c(1,50,70,75,80,85,90,95,97,99,100),]
  } else{
    vals <- val2[riskGroups,]
  }
  risks <- data.frame(
    thres1=vals$predictionThreshold[-nrow(vals)],
    thres2=vals$predictionThreshold[-1],
    size = vals$positiveCount[-nrow(vals)]-vals$positiveCount[-1],
    atrisk = vals$truePositiveCount[-nrow(vals)]-vals$truePositiveCount[-1])

  riskPlot <- data.frame(riskgroup=as.factor(1:(nrow(vals)-1)),
                         name= paste0(risks$thres1,'-',risks$thres2),
                         size = risks$size,
                         risk = risks$atrisk/risks$size*100)

  #mydata <- reshape2::melt(riskPlot, id.vars=1:2)

  plot2 <- ggplot2::ggplot(riskPlot , ggplot2::aes(riskgroup, size))+
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = risk)) +
    ggplot2::geom_text(aes(label=paste0('Outcome Rate: ',round(risk,1),'%') ),vjust=-0.3) +
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank()) +
    #scale_fill_manual(values=c("#B22222", "#b3ff99"))
    ggplot2::scale_fill_gradient(low = "#b3ff99", high = "#B22222",
                        space = "Lab", na.value = "grey50", guide = "colourbar")+
    ggplot2::theme(#axis.title.x=element_blank(),
      axis.text.x=ggplot2::element_blank(),
      axis.ticks.x=ggplot2::element_blank(),
      legend.position="none")

  plotResult <- gridExtra::grid.arrange(arrangeGrob(plot1, top="General Population Risk"),
                             arrangeGrob(plot2, top="Prediction Stratified Risk"),
                             nrow = 2, heights = c(5,5))

  return(plotResult)
}

# helper for multiple roc plots
plotRocs <- function(evaluationList,modelNames, type='test', fileName=NULL){
  if(class(evaluationList)!='list')
    stop('Need to enter a list')

  if("thresholdSummary"%in%names(evaluationList[[1]])){
    evaluationList <- evaluationList
  }else if("performanceEvaluation"%in%names(evaluationList[[1]])){
    evaluationList <- lapply(evaluationList, function(x) x$performanceEvaluation)
  } else{
    stop('Wrong evaluationList')
  }

  if(missing(modelNames))
    modelNames <- paste0('Model ', 1:length(evaluationList))

  createSteps <- function(evaluation, type, name){
    if(length(unique(evaluation$thresholdSummary$Eval))>1){
      ind <- evaluation$thresholdSummary$Eval==type
      x<- evaluation$thresholdSummary[ind,c('falsePositiveRate','sensitivity')]} else{
        x<- evaluation$thresholdSummary[,c('falsePositiveRate','sensitivity')]
      }
    x <- x[order(x$falsePositiveRate, x$sensitivity),]

    # add the bit to get the step
    stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
    colnames( stepsExtra) <- colnames(x)
    x <- rbind(c(1,1), x, stepsExtra, c(0,0))
    x <- x[order(x$falsePositiveRate, x$sensitivity),]

    x$model <- name
    return(x)
  }

  stepVals <- lapply(1:length(evaluationList), function(i) createSteps(evaluationList[[i]], type=type[i], name=modelNames[i]))
  data<- do.call(rbind, stepVals)

  plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=falsePositiveRate, y=sensitivity, color=model)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = model), alpha = 0.2) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::scale_x_continuous("1 - specificity", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Sensitivity", limits=c(0,1)) +
    ggplot2::scale_color_discrete(name = 'Result')+
    ggplot2::scale_fill_discrete(guide=FALSE)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



plotCals <- function(evaluationList,modelNames, type='test', fileName=NULL){
  if(class(evaluationList)!='list')
    stop('Need to enter a list')

  if("calibrationSummary"%in%names(evaluationList[[1]])){
    evaluationList <- evaluationList
  }else if("performanceEvaluation"%in%names(evaluationList[[1]])){
    evaluationList <- lapply(evaluationList, function(x) x$performanceEvaluation)
  } else{
    stop('Wrong evaluationList')
  }

  if(missing(modelNames))
    modelNames <- paste0('Model ', 1:length(evaluationList))

  calVal <- function(evaluation, type, name){
    if(length(unique(evaluation$calibrationSummary$Eval))>1){
      ind <- evaluation$calibrationSummary$Eval==type
      x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence','PersonCountAtRisk')]} else{
        x<- evaluation$calibrationSummary[,c('averagePredictedProbability','observedIncidence','PersonCountAtRisk')]
      }

    cis <- apply(x, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
    x$lci <- cis[1,]
    x$uci <- cis[2,]
    x$model <- name
    return(x)
  }

  calVal<- lapply(1:length(evaluationList), function(i) calVal(evaluationList[[i]], type=type[i], name=modelNames[i]))
  data<- do.call(rbind, calVal)

  maxes <- max(max(data$averagePredictedProbability), max(data$observedIncidence))*1.1

  limits <- ggplot2::aes(ymax = uci, ymin= lci)

  plot <- ggplot2::ggplot(data=data,
                          ggplot2::aes(x=averagePredictedProbability, y=observedIncidence,
                                       color=model)) +
    ggplot2::geom_point(size=2) +
    ggplot2::geom_errorbar(limits) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 5, size=0.4,
                         show.legend = TRUE) +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
    ggplot2::coord_cartesian(xlim = c(0, maxes), ylim=c(0,maxes)) +
    ggplot2::scale_color_discrete(name = 'Result')

  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}
