#' Apply the existing model ATRIA using the standardised framework
#'
#' @details
#' This function applies ATRIA to a target cohort and validates the performance
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
atriaModel <- function(connectionDetails,
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

  conceptSets <- system.file("extdata", "conceptSets.csv", package = "PredictionComparison")
  conceptSets <- read.csv(conceptSets)

  existingBleedModels <- system.file("extdata", "existingBleedModels.csv", package = "PredictionComparison")
  existingBleedModels <- read.csv(existingBleedModels)

  modelNames <- system.file("extdata", "modelNames.csv", package = "PredictionComparison")
  modelNames <- read.csv(modelNames)

  modelTable <- existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='ATRIA'],]
  modelTable <- modelTable[,c('modelId','modelCovariateId','coefficientValue')]

  # use history anytime prior by setting long term look back to 9999
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = T,
                                                                  useConditionOccurrenceLongTerm = T,
                                                                  useConditionGroupEraLongTerm = T,
                                                                  useConditionGroupEraMediumTerm  = T,
                                                                  useConditionEraLongTerm = T,
                                                                  useDrugGroupEraLongTerm =  T,
                                                                  useDrugExposureLongTerm = T,
                                                                  useProcedureOccurrenceLongTerm = T,
                                                                  useProcedureOccurrenceShortTerm = T,
                                                                  useDeviceExposureLongTerm = T,
                                                                  useMeasurementRangeGroupLongTerm = T,
                                                                  useMeasurementLongTerm = T,
                                                                  useObservationLongTerm = T,
                                                                  longTermStartDays = -9999,
                                                                  mediumTermStartDays = -365,
                                                                  shortTermStartDays = -30)

  result <- PatientLevelPrediction::evaluateExistingModel(modelTable = modelTable,
                                                          covariateTable = conceptSets[,c('modelCovariateId','covariateId')],
                                                          interceptTable = NULL,
                                                          type = 'score',
                                                          covariateSettings = covariateSettings,
                                                          riskWindowStart = 1,
                                                          riskWindowEnd = 365,
                                                          requireTimeAtRisk = T,
                                                          minTimeAtRisk = 364,
                                                          includeAllOutcomes = T,
                                                          removeSubjectsWithPriorOutcome =removePriorOutcome,
                                                          connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          cohortId = cohortId,
                                                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                          outcomeTable = outcomeTable,
                                                          outcomeId = outcomeId)

  inputSetting <- list(connectionDetails=connectionDetails,
                       cdmDatabaseSchema=cdmDatabaseSchema,
                       cohortDatabaseSchema=cohortDatabaseSchema,
                       outcomeDatabaseSchema=outcomeDatabaseSchema,
                       cohortTable=cohortTable,
                       outcomeTable=outcomeTable,
                       cohortId=cohortId,
                       outcomeId=outcomeId,
                       oracleTempSchema=oracleTempSchema)
  result <- list(model=list(model='atria'),
                 analysisRef ='000000',
                 inputSetting =inputSetting,
                 executionSummary = 'Not available',
                 prediction=result$prediction,
                 performanceEvaluation=result$performance)
  class(result$model) <- 'plpModel'
  attr(result$model, "type")<- 'existing model'
  class(result) <- 'runPlp'
  return(result)
}
