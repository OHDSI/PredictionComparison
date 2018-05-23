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
#' @param removePriorOutcome  Remove people with prior outcomes from the target population
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
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=60
and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=62
then -1
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=63
and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=66
then -2
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=67
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=71
then -3
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=72
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=74
then -4
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=75
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=77
then -5
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=78
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=81
then -6
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=82
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=85
then -7
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=86
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=90
then -8
when
datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=91
                     and datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date) <=93
then -9 else
-10 end as covariate_id,  1 as covariate_value
from @cdmDatabaseSchema.person a inner join @cohortTable b
on a.person_id=b.subject_id
where datediff(year, datefromparts(a.year_of_birth, isnull(a.month_of_birth,1),1), b.cohort_start_date)>=60
")

  result <- PatientLevelPrediction::evaluateExistingModel(modelTable = modelTable,
                                                          covariateTable = conceptSets[,c('modelCovariateId','covariateId')],
                                                          interceptTable = NULL,
                                                          type = 'score',
                                                          covariateSettings = covariateSettings,
                                                          customCovariates =cust,
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
  result <- list(model=list(model='Framingham'),
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
