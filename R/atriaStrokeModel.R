#' Apply the existing model Atria stroke risk using the standardised framework
#'
#' @details
#' This function applies Atria stroke risk to a target cohort and validates the performance
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
#' @param recalibrate         Recalibrate model on new data
#' @param calibrationPopulation A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#'
#' @return
#' A list containing the model performance and the personal predictions for each subject in the target population
#'
#' @export
atriaStrokeModel <- function(connectionDetails,
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
                         removePriorOutcome = T,
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

  modelTable <- existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Atria'],]
  modelTable <- modelTable[,c('modelId','modelCovariateId','coefficientValue')]

  # use history anytime prior by setting long term look back to 9999
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup  = T,
                                                                  useDemographicsGender = T,
                                                                  useConditionOccurrenceLongTerm = T,
                                                                  useConditionGroupEraLongTerm = T,
                                                                  longTermStartDays = -365*5)

  #scoreToProb <- data.frame(score = c(0,1,2,3,4,5,6,7,8,
  #                                    9,10,11,12,13,14,15),
  #                          probability = c(0.08,0.43,0.99,0.73,0.64,0.99,1.91,2.50,3.86,
  #                                          4.33,6.35,6.18,10.95,7.52,16.36,0)/100)

  result <- PatientLevelPrediction::evaluateExistingModel(modelTable = modelTable,
                                                          covariateTable = conceptSets[,c('modelCovariateId','covariateId')],
                                                          interceptTable = NULL,
                                                          type = 'score',
                                                          covariateSettings = covariateSettings,
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
                                                          #scoreToProb =  scoreToProb,
														  calibrationPopulation=calibrationPopulation,
                                                          addExposureDaysToEnd = addExposureDaysToEnd)



  result$model$modelName <- 'atriaStroke'

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
                  existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Atria'],c('Name','modelCovariateId')],
                  by.x='covariateId', by.y='modelCovariateId', all=T)
  result$covariateSummary$covariateName = result$covariateSummary$Name

  class(result$model) <- 'plpModel'
  attr(result$model, "type")<- 'existing model'
  class(result) <- 'runPlp'
  return(result)
}
