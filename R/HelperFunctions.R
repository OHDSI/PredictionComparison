getCovariateInfo <- function(x='model-name'){
  cohortVarsToCreate <- utils::read.csv(system.file("settings", 'covariates.csv', package = "PredictionComparison"))
  modelCovariates  <- utils::read.csv(system.file("settings", paste0(x,'.csv'), package = "PredictionComparison"))
  merge(cohortVarsToCreate,modelCovariates, by='covariateId')
}

getModel <- function(model = 'SimpleModel.csv'){
  pathToCustom <- system.file("settings", paste0(model,'.csv') , package = "PredictionComparison")
  coefficients <- utils::read.csv(pathToCustom)
  return(coefficients)
}

getPredict <- function(model, mapping = NULL){
  predictExisting <- function(plpData, population){
    coefficients <- model

    if(is.null(mapping)){
      mapping <- function(x){return(x)}
    }

    plpData$covariateData$coefficients <- coefficients
    on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)

    prediction <- plpData$covariateData$covariates %>%
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>%
      dplyr::mutate(values = covariateValue*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>% dplyr::collect()

    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0

    # REPLACE WITH MAPPING
    # add any final mapping here (e.g., add intercept and mapping)
    prediction$value <- mapping(prediction$value)

    attr(prediction, "metaData") <- list(predictionType = 'binary', mapping = mapping, model = model)

    return(prediction)
  }
  return(predictExisting)
}


checkMakeTable <- function(connection, databaseSchema,cohortTable,oracleTempSchema){

  #check whether table exists:
  tables <- DatabaseConnector::getTableNames(connection = connection, databaseSchema = databaseSchema)
  if(toupper(cohortTable)%in%tables){return(T)}

  # else Create study cohort table structure:
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "PredictionComparison",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = databaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

}


#find cohort_definition_ids in databaseSchema.cohortTable and interact with cohortVarsToCreate
# to find missing cohort_definition_ids
getMissingCohorts <- function(connection,
                              databaseSchema,
                              cohortTable,
                              oracleTempSchema,
                              cohortVarsToCreate){

  sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                           "PredictionComparison",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           work_database_schema = databaseSchema,
                                           study_cohort_table = cohortTable)
  counts <- DatabaseConnector::querySql(connection, sql)
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))

  missing <- cohortVarsToCreate[!cohortVarsToCreate$cohortId%in%counts$cohortDefinitionId,]

  if(nrow(missing)==0){missing <- NULL}

  return(missing)
}

# get the counts for each  cohort_definition_ids in cohortVarsToCreate
getCounts <- function(connection,
                      covariateDatabaseSchema,
                      covariateCohortTable,
                      cohortVarsToCreate,
                      oracleTempSchema){

  sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                           "PredictionComparison",
                                           dbms = attr(connection, "dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           work_database_schema = covariateDatabaseSchema,
                                           study_cohort_table = covariateCohortTable)
  counts <- DatabaseConnector::querySql(connection, sql)
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))

  counts <- merge(counts, cohortVarsToCreate, by.x= 'cohortDefinitionId', by.y='cohortId')
  return(counts)
}


createCohorts <- function(connectionDetails,
                          cdmDatabaseSchema,
                          vocabularyDatabaseSchema,
                          covariateDatabaseSchema,
                          covariateCohortTable,
                          oracleTempSchema,
                          cohortVarsToCreate,
                          overwriteExistingCohorts){
  connection <- DatabaseConnector::connect(connectionDetails)
  ParallelLogger::logInfo('Creating cohorts that are needed')
  checkMakeTable(connection = connection,
                 databaseSchema = covariateDatabaseSchema,
                 cohortTable = covariateCohortTable,
                 oracleTempSchema = oracleTempSchema)

  if(!overwriteExistingCohorts){
    # get existing cohort definition ids
    missingCohortVarsToCreate <- getMissingCohorts(connection,
                                                   databaseSchema = covariateDatabaseSchema,
                                                   cohortTable = covariateCohortTable,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cohortVarsToCreate = cohortVarsToCreate)
  }else{
    missingCohortVarsToCreate  <- unique(cohortVarsToCreate[,c('cohortId', 'cohortName')])
  }

  if(!is.null(missingCohortVarsToCreate)){
    if(nrow(missingCohortVarsToCreate)>0){
      ParallelLogger::logInfo("Creating cohorts for model")
      for (i in 1:nrow(missingCohortVarsToCreate)) {
        writeLines(paste("Creating cohort:", missingCohortVarsToCreate$cohortName[i]))
        sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(missingCohortVarsToCreate$cohortName[i], ".sql"),
                                                 packageName = "PredictionComparison",
                                                 dbms = attr(connection, "dbms"),
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 vocabulary_database_schema = vocabularyDatabaseSchema,

                                                 target_database_schema = covariateDatabaseSchema,
                                                 target_cohort_table = covariateCohortTable,
                                                 target_cohort_id = missingCohortVarsToCreate$cohortId[i])
        DatabaseConnector::executeSql(connection, sql)
      }
    }
  }

  counts <-   getCounts(connection,
                        covariateDatabaseSchema,
                        covariateCohortTable,
                        cohortVarsToCreate,
                        oracleTempSchema)
  DatabaseConnector::disconnect(connection)

  return(counts)
}
