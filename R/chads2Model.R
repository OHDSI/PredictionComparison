#' Apply the existing model CHADs2 using the standardised framework
#'
#' @details
#' This function applies CHADs2 to a target cohort and validates the performance
# using the outcome cohort
#'
#' @param connectionDetails                The connection details for extracting the data
#' @param cdmDatabaseSchema                      A string specifying the database containing the cdm
#' @param cohortDatabaseSchema                    A string specifying the database containing the target population cohort
#' @param outcomeDatabaseSchema                   A string specifying the database containing the outcome cohort
#' @param cohortTable          A string specifying the table containing the target population cohort
#' @param outcomeTable        A string specifying the table containing the outcome cohort
#' @param cohortId             An iteger specifying the cohort id for the target population cohorts
#' @param outcomeId          An iteger specifying the cohort id for the outcome cohorts
#' @param oracleTempSchema   The temp schema require is using oracle
#' @param riskWindowStart    The start of the period to predict the risk of the outcome occurring start relative to the target cohort start date
#' @param riskWindowEnd      The end of the period to predict the risk of the outcome occurring start relative to the target cohort start date
#' @param addExposureDaysToEnd  Add the riskWindowEnd to the cohort end rather than the cohort start to define the end of TAR
#' @param requireTimeAtRisk  Require a minimum number of days observed in the time at risk period?
#' @param minTimeAtRisk      If requireTimeAtRisk is true, the minimum number of days at risk
#' @param includeAllOutcomes  Whether to include people with outcome who do not satify the minTimeAtRisk
#' @param firstExposureOnly   Whether to restrict to first target cohort start date if people are in target popualtion multiple times
#' @param removePriorOutcome  Remove people with prior outcomes from the target population
#' @param recalibrate     Whether to recalibrate for the new data
#' @param calibrationPopulation A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#'
#' @return
#' A list containing the model performance and the personal predictions for each subject in the target population
#'
#' @export
chads2Model <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    outcomeDatabaseSchema,
                    cohortTable,
                    outcomeTable,
                    cohortId,
                    outcomeId,
                    oracleTempSchema=NULL,
                    riskWindowStart = 1,
                    riskWindowEnd = 365,
                    addExposureDaysToEnd = F,
                    requireTimeAtRisk = T,
                    minTimeAtRisk = 364,
                    includeAllOutcomes = T,
                    firstExposureOnly = F,
                    removePriorOutcome=T,
                    recalibrate = T,
                    calibrationPopulation=NULL){

  #input checks...
  if(missing(connectionDetails))
    stop('Need to enter connectionDetails')
  if(missing(cdmDatabaseSchema))
    stop('Need to enter cdmDatabaseSchema')
  if(missing(cohortDatabaseSchema))
    stop('Need to enter cohortDatabaseSchema')
  if(missing(outcomeDatabaseSchema))
    stop('Need to enter outcomeDatabaseSchema')
  if(missing(cohortTable))
    stop('Need to enter cohortTable')
  if(missing(outcomeTable))
    stop('Need to enter outcomeTable')
  if(missing(cohortId))
    stop('Need to enter cohortId')
  if(missing(outcomeId))
    stop('Need to enter outcomeId')
  if(!is.null(calibrationPopulation)){
    if(sum(c('subjectId','cohortStartDate','indexes')%in%colnames(calibrationPopulation))!=3){
      stop("Need 'subjectId','cohortStartDate','indexes' in data.frame")
    }
    calibrationPopulation <- calibrationPopulation[,c('subjectId','cohortStartDate','indexes')]
  }
 # modelTable <- data.frame(modelId=1,
#                           modelCovariateId=1,
#                           coefficientValue=1)
#  covariateTable <- data.frame(modelCovariateId=1,
#                               covariateId=1903)

# use history anytime prior by setting long term look back to 9999
covariateSettings <- FeatureExtraction::createCovariateSettings(useChads2 = T)

plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   oracleTempSchema =  oracleTempSchema,
                                   cohortId = cohortId, outcomeIds = outcomeId,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   cohortTable = cohortTable,
                                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                                   outcomeTable = outcomeTable, cdmVersion = 5,
                                   sampleSize = NULL, covariateSettings = covariateSettings
                                   )

population <- PatientLevelPrediction::createStudyPopulation(plpData=plpData,
                                                            outcomeId = outcomeId,
                                                            binary = T,
                                                            riskWindowStart = riskWindowStart,
                                                            riskWindowEnd = riskWindowEnd,
                                                            addExposureDaysToEnd = addExposureDaysToEnd,
                                                            requireTimeAtRisk = requireTimeAtRisk,
                                                            minTimeAtRisk = minTimeAtRisk,
                                                            includeAllOutcomes = includeAllOutcomes,
                                                            firstExposureOnly = firstExposureOnly,
                                                            removeSubjectsWithPriorOutcome =removePriorOutcome)

prediction = merge(ff::as.ram(plpData$covariates), population, by='rowId', all.y=T)

covSum <- PatientLevelPrediction:::covariateSummary(plpData, population)

recalModel <- NULL
if(!is.null(calibrationPopulation)){
  #re-calibrate model:
  prediction <- base::merge(calibrationPopulation, prediction, by=c('subjectId','cohortStartDate'))
  recalModel <- stats::glm(y ~ x,
                           family=stats::binomial(link='logit'),
                           data=data.frame(x=prediction$covariateValue,
                                           y=as.factor(prediction$outcomeCount))[prediction$indexes>0,])
  value <- stats::predict(recalModel, data.frame(x=prediction$covariateValue),
                          type = "response")
  prediction$value <- value
} else if(recalibrate & is.null(calibrationPopulation)){
  sampleCal <- sample(floor(nrow(prediction)*0.25))
  recalModel <- stats::glm(y ~ x,
                           family=stats::binomial(link='logit'),
                           data=data.frame(x=prediction$covariateValue,
                                           y=as.factor(prediction$outcomeCount))[sampleCal,])
  value <- stats::predict(recalModel, data.frame(x=prediction$covariateValue),
                          type = "response")
  prediction$value <- value
  prediction$index <- -1
  prediction$index[sampleCal] <- 1

}else {
  colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value/max(prediction$value)
}

attr(prediction, "metaData")$predictionType <- 'binary'
performance <- PatientLevelPrediction::evaluatePlp(prediction = prediction, plpData = plpData)
# reformatting the performance
analysisId <-   '000000'
nr1 <- length(unlist(performance$evaluationStatistics[-1]))
performance$evaluationStatistics <- cbind(analysisId= rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          Metric = names(unlist(performance$evaluationStatistics[-1])),
                                          Value = unlist(performance$evaluationStatistics[-1])
)
nr1 <- nrow(performance$thresholdSummary)
performance$thresholdSummary <- cbind(analysisId=rep(analysisId,nr1),
                                      Eval=rep('validation', nr1),
                                      performance$thresholdSummary)
nr1 <- nrow(performance$demographicSummary)
if(!is.null(performance$demographicSummary)){
  performance$demographicSummary <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$demographicSummary)
}
nr1 <- nrow(performance$calibrationSummary)
performance$calibrationSummary <- cbind(analysisId=rep(analysisId,nr1),
                                        Eval=rep('validation', nr1),
                                        performance$calibrationSummary)
nr1 <- nrow(performance$predictionDistribution)
performance$predictionDistribution <- cbind(analysisId=rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            performance$predictionDistribution)

inputSetting <- list(connectionDetails=connectionDetails,
                      cdmDatabaseSchema=cdmDatabaseSchema,
                      cohortDatabaseSchema=cohortDatabaseSchema,
                      outcomeDatabaseSchema=outcomeDatabaseSchema,
                      cohortTable=cohortTable,
                      outcomeTable=outcomeTable,
                      cohortId=cohortId,
                      outcomeId=outcomeId,
                      oracleTempSchema=oracleTempSchema)

executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                               packageVersion = utils::packageVersion("PatientLevelPrediction")),
                         PlatformDetails= list(platform= R.Version()$platform,
                                               cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                               RAM=utils::memory.size()), #  test for non-windows needed
                         # Sys.info()
                         TotalExecutionElapsedTime = NULL,
                         ExecutionDateTime = Sys.Date())

result <- list(performanceEvaluation=performance,
            prediction=prediction,
            inputSetting = inputSetting,
            executionSummary = executionSummary,
            model = list(model=list(name='chads2',
                         modelName=NULL,
                         modelTable=NULL,
                         covariateTable=NULL,
                         interceptTable=NULL,
                         recalModel = recalModel)),
            analysisRef=list(analysisId=NULL,
                             analysisName=NULL,
                             analysisSettings= NULL),
            covariateSummary = covSum)


class(result$model) <- 'plpModel'
attr(result$model, "type")<- 'existing model'
class(result) <- 'runPlp'
return(result)
}
