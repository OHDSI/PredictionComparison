#' Apply the existing model ORBIT using the standardised framework
#'
#' @details
#' This function applies ORBIT to a target cohort and validates the performance
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
#' @param oracleTempSchema    The temp schema require is using oracle
#' @param cdmVersion        The CDM version you are using
#' @param analysisId         An id to give to the study
#' @param sampleSize         The size to sample from T when applying the existing model
#' @param riskWindowStart    The start of the period to predict the risk of the outcome occurring start relative to the target cohort start date
#' @param startAnchor        Is the risk start relative to cohort_start or cohort_end
#' @param riskWindowEnd      The end of the period to predict the risk of the outcome occurring start relative to the target cohort start date
#' @param endAnchor          Is the risk end relative to cohort_start or cohort_end
#' @param endDay            The last day relative to index for the covariates
#' @param requireTimeAtRisk  Require a minimum number of days observed in the time at risk period?
#' @param minTimeAtRisk      If requireTimeAtRisk is true, the minimum number of days at risk
#' @param includeAllOutcomes  Whether to include people with outcome who do not satify the minTimeAtRisk
#' @param removePriorOutcome  Remove people with prior outcomes from the target population
#' @param priorOutcomeLookback  Time prior to index to remove people if they have the outcome
#' @param calibrationPopulation A data.frame of subjectId, cohortStartDate, indexes used to recalibrate the model on new data
#' @param createCovariateCohorts Create the cohorts used for the covariates in the model
#' @param overwriteExistingCohorts Overwrite cohorts that already exist in the covariateCohortTable table
#' @param covariateDatabaseSchema A read/write database to keep the covariate cohorts
#' @param covariateCohortTable A read/write table to keep the covariate cohorts
#'
#' @return
#' A list containing the model performance and the personal predictions for each subject in the target population
#'
#' @export
orbitModel <- function(connectionDetails,
                       cdmDatabaseSchema,
                       cohortDatabaseSchema,
                       outcomeDatabaseSchema,
                       cohortTable,
                       outcomeTable,
                       cohortId,
                       outcomeId,
                       oracleTempSchema=NULL,
                       cdmVersion = 5,
                       analysisId = 'Orbit',
                       sampleSize = NULL,
                       riskWindowStart = 1,
                       startAnchor = 'cohort start',
                       riskWindowEnd = 365,
                       endAnchor = 'cohort start',
                       endDay = -1,
                       requireTimeAtRisk = T,
                       minTimeAtRisk = 1,
                       includeAllOutcomes = T,
                       removePriorOutcome=T,
                       priorOutcomeLookback = 9999,
                       calibrationPopulation=NULL,
                       createCovariateCohorts = T,
                       overwriteExistingCohorts = T,
                       covariateDatabaseSchema = cohortDatabaseSchema,
                       covariateCohortTable = 'PredictionComp'){

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

  modelName <- 'orbit'


  ParallelLogger::logInfo('Extracting Orbit settings')
  # create covariate settings
  covariateInfo <- getCovariateInfo(modelName)
  cohortVarsToCreate <- covariateInfo[covariateInfo$analysisId == 456,]

  #create cohorts
  if(createCovariateCohorts){
    counts <- createCohorts(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            vocabularyDatabaseSchema = cdmDatabaseSchema,
                            covariateDatabaseSchema = covariateDatabaseSchema,
                            covariateCohortTable = covariateCohortTable,
                            oracleTempSchema = oracleTempSchema,
                            cohortVarsToCreate = cohortVarsToCreate,
                            overwriteExistingCohorts = overwriteExistingCohorts)

    if(!is.null(counts)){
      ParallelLogger::logInfo('Covariate cohort counts:')
      ParallelLogger::logInfo('------------------------')
      lapply(1:nrow(counts), function(i){ParallelLogger::logInfo(paste0(counts$covariateName[i], ': ', counts$cohortCount[i], '-',counts$personCount[i] ))})
      ParallelLogger::logInfo('------------------------')
    }
  }


  ParallelLogger::logInfo('Creating covariate settings')
  covariateSettings <- list()
  length(covariateSettings) <- 1+nrow(cohortVarsToCreate)
  covariateSettings[[1]] <- FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = T, endDays = endDay,
                                                                       useMeasurementValueAnyTimePrior = ifelse(sum(covariateInfo$analysisId == 455)>0, T, F)
                                                                       #includedCovariateIds = measurement covariates of interest
  )

  # add cohort covariates
  for(i in 1:nrow(cohortVarsToCreate)){
    covariateSettings[[1+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                              covariateId = cohortVarsToCreate$cohortId[i]*1000+456,
                                                              cohortDatabaseSchema = covariateDatabaseSchema,
                                                              cohortTable = covariateCohortTable,
                                                              cohortId = cohortVarsToCreate$cohortId[i],
                                                              startDay=cohortVarsToCreate$startDay[i],
                                                              endDay= endDay,
                                                              count= as.character(cohortVarsToCreate$count[i]),
                                                              ageInteraction = as.character(cohortVarsToCreate$ageInteraction[i]))
  }

  ParallelLogger::logInfo('Extracting Data')
  plpData <- tryCatch({PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          cohortId = cohortId,
                                                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                          outcomeTable = outcomeTable,
                                                          outcomeIds = outcomeId,
                                                          oracleTempSchema = oracleTempSchema,
                                                          covariateSettings = covariateSettings,
                                                          cdmVersion = cdmVersion,
                                                          sampleSize = sampleSize)},
                      finally= ParallelLogger::logTrace('Done extracting data.'),
                      error= function(cond){ParallelLogger::logError(paste0('Error data extraction:',cond));return(NULL)})


  ParallelLogger::logInfo('Creating study population')
  population <- tryCatch({PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                                        outcomeId = outcomeId,
                                                                        riskWindowStart = riskWindowStart,
                                                                        riskWindowEnd = riskWindowEnd,
                                                                        startAnchor = startAnchor,
                                                                        endAnchor = endAnchor,
                                                                        requireTimeAtRisk = requireTimeAtRisk,
                                                                        minTimeAtRisk = minTimeAtRisk,
                                                                        includeAllOutcomes = includeAllOutcomes,
                                                                        removeSubjectsWithPriorOutcome =removePriorOutcome,
                                                                        priorOutcomeLookback = priorOutcomeLookback)},
                         finally= ParallelLogger::logTrace('Done creating pop.'),
                         error= function(cond){ParallelLogger::logError(paste0('Error creating pop:',cond));return(NULL)})



  # the mapping from the score to probability
  map <- function(x){return(x/max(x))}

  ParallelLogger::logInfo('Creating model')
  plpModel <- tryCatch({list(model = getModel(model = modelName),
                             analysisId = paste0('Analysis_',analysisId),
                             hyperParamSearch = NULL,
                             index = NULL,
                             trainCVAuc = NULL,
                             modelSettings = list(model = modelName, modelParameters = NULL),
                             metaData = NULL,
                             populationSettings = attr(population, "metaData"),
                             trainingTime = NULL,
                             varImp = getModel(model = modelName),
                             dense = T,
                             cohortId = cohortId,
                             outcomeId = outcomeId,
                             endDay = endDay,
                             covariateMap = NULL,
                             predict = getPredict(getModel(model = modelName), mapping = map)
  )},
  finally= ParallelLogger::logTrace('Done model.'),
  error= function(cond){ParallelLogger::logError(paste0('Error creating model:',cond));return(NULL)})
  class(plpModel) <- 'plpModel'

  ParallelLogger::logInfo('Applying model')
  result <- tryCatch({PatientLevelPrediction::applyModel(population = population,
                                                         plpData = plpData,
                                                         plpModel = plpModel,
                                                         calculatePerformance = T,
                                                         databaseOutput = F,
                                                         silent = F) },
                     finally= ParallelLogger::logTrace('Done applying model.'),
                     error= function(cond){ParallelLogger::logError(paste0('Error applying model:',cond));return(NULL)})


  result$model$modelName <- modelName

  result$inputSetting <- list(connectionDetails=connectionDetails,
                              cdmDatabaseSchema=cdmDatabaseSchema,
                              cohortDatabaseSchema=cohortDatabaseSchema,
                              outcomeDatabaseSchema=outcomeDatabaseSchema,
                              cohortTable=cohortTable,
                              outcomeTable=outcomeTable,
                              cohortId=cohortId,
                              outcomeId=outcomeId,
                              oracleTempSchema=oracleTempSchema)

  class(result$model) <- 'plpModel'
  attr(result$model, "type")<- 'existing model'
  class(result) <- 'runPlp'
  return(result)
}
