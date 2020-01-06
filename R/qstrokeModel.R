#' Apply the existing model Qstroke stroke risk using the standardised framework
#'
#' @details
#' This function applies Qstroke stroke risk to a target cohort and validates the performance
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
#' @param removePriorOutcome  Remove people with prior outcomes from the target population
#' @param recalibrate         Recalibrate on new data
#' @param calibrationPopulation A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#'
#' @return
#' A list containing the model performance and the personal predictions for each subject in the target population
#'
#' @export
qstrokeModel <- function(connectionDetails,
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
  conceptSets <- system.file("extdata", "existingStrokeModels_concepts.csv", package = "PredictionComparison")
  conceptSets <- read.csv(conceptSets)

  existingBleedModels <- system.file("extdata", "existingStrokeModels_modelTable.csv", package = "PredictionComparison")
  existingBleedModels <- read.csv(existingBleedModels)

  modelNames <- system.file("extdata", "existingStrokeModels_models.csv", package = "PredictionComparison")
  modelNames <- read.csv(modelNames)

  modelTable <- existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Qstroke'],]
  modelTable <- modelTable[,c('modelId','modelCovariateId','coefficientValue')]

  # use history anytime prior by setting long term look back to 9999
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsRace  = T,
                                                                  useConditionOccurrenceLongTerm = T,
                                                                  useConditionGroupEraLongTerm = T,
                                                                  useDrugGroupEraShortTerm = T,
                                                                  useConditionGroupEraMediumTerm = T,
                                                                  useProcedureOccurrenceLongTerm = T,
                                                                  useObservationLongTerm = T,
                                                                  longTermStartDays = -9999,
                                                                  mediumTermStartDays = -365,
                                                                  shortTermStartDays = -30)

  cust <- data.frame(covariateId=-46, sql="insert into @targetCovariateTable
select distinct a.@rowIdField as row_id, @covariateId as covariate_id,  1 as covariate_value
from
(select row_id from @targetCovariateTable where covariate_id=320128211) a
inner join
(select row_id from @targetCovariateTable where covariate_id=21600381412) b
on a.row_id=b.row_id;")

  cust$sql <- SqlRender::translateSql(sql = as.character(cust$sql),
                                      targetDialect = connectionDetails$dbms,
                                      oracleTempSchema = oracleTempSchema)$sql

  result <- PatientLevelPrediction::evaluateExistingModel(modelTable = modelTable,
                                                          covariateTable = conceptSets[,c('modelCovariateId','covariateId')],
                                                          interceptTable = NULL,
                                                          type = 'score',
                                                          covariateSettings = covariateSettings,
                                                          customCovariates =cust,
                                                          riskWindowStart = riskWindowStart,
                                                          riskWindowEnd = riskWindowEnd,
                                                          addExposureDaysToEnd = addExposureDaysToEnd,
                                                          requireTimeAtRisk = requireTimeAtRisk,
                                                          minTimeAtRisk = minTimeAtRisk,
                                                          includeAllOutcomes = includeAllOutcomes,
                                                          removeSubjectsWithPriorOutcome =removePriorOutcome,
                                                          connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          cohortId = cohortId,
                                                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                          outcomeTable = outcomeTable,
                                                          outcomeId = outcomeId,
                                                          oracleTempSchema = oracleTempSchema,
                                                          recalibrate = recalibrate,
                                                          calibrationPopulation=calibrationPopulation)

  result$model$modelName <- 'qStroke'

  result$inputSetting <- list(connectionDetails=connectionDetails,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              cohortDatabaseSchema=cohortDatabaseSchema,
                              outcomeDatabaseSchema=outcomeDatabaseSchema,
                              cohortTable=cohortTable,
                              outcomeTable=outcomeTable,
                              cohortId=cohortId,
                              outcomeId=outcomeId,
                              oracleTempSchema=oracleTempSchema)

  result$covariateSummary<- merge(result$covariateSummary,
                                  existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Qstroke'],c('Name','modelCovariateId')],
                                  by.x='covariateId', by.y='modelCovariateId', all=T)
  result$covariateSummary$covariateName = result$covariateSummary$Name

  class(result$model) <- 'plpModel'
  attr(result$model, "type")<- 'existing model'
  class(result) <- 'runPlp'
  return(result)
}
