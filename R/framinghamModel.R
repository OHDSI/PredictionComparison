#' Apply the existing model Framingham stroke risk using the standardised framework
#'
#' @details
#' This function applies Framingham stroke risk to a target cohort and validates the performance
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
#' @param oracleTempSchema     The temp schema require is using oracle
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
framinghamModel <- function(connectionDetails,
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

  modelTable <- existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Framingham'],]
  modelTable <- modelTable[,c('modelId','modelCovariateId','coefficientValue')]

  # use history anytime prior by setting long term look back to 9999
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                  useConditionOccurrenceLongTerm = T,
                                                                  useConditionGroupEraLongTerm = T,
                                                                  useConditionEraLongTerm = T,
                                                                  longTermStartDays = -9999)

  # cust code to do the age scores...
  cust <- data.frame(covariateId=-1, sql="insert into @targetCovariateTable
select distinct b.@rowIdField as row_id,
case
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=60* 365.25
and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <63* 365.25
then -1
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=63* 365.25
and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <67* 365.25
then -2
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=67* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <72* 365.25
then -3
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=72* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <75* 365.25
then -4
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=75* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <78* 365.25
then -5
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=78* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <82* 365.25
then -6
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=82* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <86* 365.25
then -7
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=86* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <91* 365.25
then -8
when
DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=91* 365.25
                     and DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date) <94* 365.25
then -9 else
-10 end as covariate_id,  1 as covariate_value
from @cdmDatabaseSchema.person a inner join @cohortTable b
on a.person_id=b.subject_id
where DATEDIFF(DAY, DATEFROMPARTS(a.year_of_birth, ISNULL(a.month_of_birth,1),1), b.cohort_start_date)>=60* 365.25;
")

  cust$sql <- SqlRender::translateSql(sql = as.character(cust$sql),
                                      targetDialect = connectionDetails$dbms,
                                      oracleTempSchema = oracleTempSchema)$sql


  #scoreToProb <- data.frame(score = c(0:31),
  #                          probability = c(5,5,6,6,7,8,
  #                                          9,9,11,12,13,
  #                                          14,16,18,19,21,
  #                                          24,26,28,31,34,
  #                                          37,41,44,48,51,
  #                                          55,59,63,67,71,
  #                                          75)/100)

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
                                                         # scoreToProb = scoreToProb,
                                                          calibrationPopulation=calibrationPopulation)

  result$model$modelName <- 'Framingham'

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
                                  existingBleedModels[existingBleedModels$modelId==modelNames$modelId[modelNames$name=='Framingham'],c('Name','modelCovariateId')],
                                  by.x='covariateId', by.y='modelCovariateId', all=T)
  result$covariateSummary$covariateName = result$covariateSummary$Name

  class(result$model) <- 'plpModel'
  attr(result$model, "type")<- 'existing model'
  class(result) <- 'runPlp'
  return(result)
}
