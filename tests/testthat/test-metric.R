# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of PredictionComparison
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

library("testthat")
x1 <- runif(1000)
x2 <- runif(1000)
y <- sample(0:1, 1000, TRUE)
model1 <- list(prediction=data.frame(rowId=1:1000,
                                     subjectId=1:1000,
                                     cohortStartDate=rep('2010-01-01',1000),
                                     value=x1,
                                     outcomeCount=y))
model2 <- list(prediction=data.frame(rowId=1:1000,
                                     subjectId=1:1000,
                                     cohortStartDate=rep('2010-01-01',1000),
                                     value=x2,
                                     outcomeCount=y))


context("Performance Measures")

test_that("IDI", {
  ourIDI <- PredictionComparison::IDI(model2, model1)$IDI
  existingIDI <- Hmisc::improveProb(x1, x2, y)$idi
  tolerance <- 0.001
  testthat::expect_equal(ourIDI, as.numeric(existingIDI), tolerance = tolerance)

  ourZ <- PredictionComparison::IDI(model2, model1)$z
  existingZ <-Hmisc::improveProb(x1, x2, y)$z.idi
  testthat::expect_equal(ourZ, as.numeric(existingZ), tolerance = tolerance)
})


test_that("NRI", {
  temp <- PredictionComparison::NRI(model2, model1, thresholds = 0.5)
  ourNRI <- temp[[1]]$nri

  temp2 <-  nricens::nribin(event = y, p.std = x1, p.new = x2, cut = 0.5)
  existingNRI <- temp2$nri[1,1]

  tolerance <- 0.001
  testthat::expect_equal(ourNRI, existingNRI, tolerance = tolerance)

})

test_that("NRI2", {
  temp <- PredictionComparison::NRI2(model2, model1)
  ourNRI <- temp$nri
  ourNRIZ <- temp$z
  temp2 <- Hmisc::improveProb(x1, x2, y)
  existingNRI <- temp2$nri
  existingNRIZ <- temp2$z.nri

  tolerance <- 0.001
  testthat::expect_equal(ourNRI, as.numeric(existingNRI), tolerance = tolerance)
  testthat::expect_equal(ourNRIZ, as.numeric(existingNRIZ), tolerance = tolerance)
})
