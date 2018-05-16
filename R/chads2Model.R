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
#' @param oracleTempSchema
#' @param removePriorOutcome  Remove people with prior outcomes from the target population
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
                    removePriorOutcome=T){

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
                                                            riskWindowStart = 1,
                                                            riskWindowEnd = 365,
                                                            requireTimeAtRisk = T,
                                                            minTimeAtRisk = 364,
                                                            includeAllOutcomes = T,
                                                            firstExposureOnly = F,
                                                            removeSubjectsWithPriorOutcome =removePriorOutcome)

prediction = merge(ff::as.ram(plpData$covariates), population, by='rowId', all.y=T)
colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
prediction$value[is.na(prediction$value)] <- 0
prediction$value <- prediction$value/max(prediction$value)
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
result <- list(model=list(model='chads2'),
               analysisRef ='000000',
               inputSetting =inputSetting,
               executionSummary = 'Not available',
               prediction=prediction,
               performanceEvaluation=performance)
class(result$model) <- 'plpModel'
attr(result$model, "type")<- 'existing model'
class(result) <- 'runPlp'
return(result)
}
