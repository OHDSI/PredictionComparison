PredictionComparison
======================

  Introduction
============
  An R package for comparing patient level predictive models developed using PatientLevelPrediction.

Features
========
  - Takes two or more PatientLevelPrediction models and calculates various performance metric comparisons
  - Plots the comparisons
  - Can interact with models using a shiny app - TO be developed
  - Contains a set of existing risk models to use as benchmarks for model development

  Technology
==========
  PredictionComparison is an R package.

System Requirements
===================
  Requires R (version 3.3.0 or higher).

Dependencies
============
  * PatientLevelPrediction

Getting Started
===============
  1. In R, use the following commands to download and install PatientLevelPrediction:

  ```r
install.packages("drat")
drat::addRepo("OHDSI")
install.packages("PredictionComparison")
```

  Getting Involved
================
  * Vignette: [Comparing PatientlevelPrediction models](https://github.com/OHDSI/PatientLevelPrediction/blob/master/inst/doc/BuildingPredictiveModels.pdf)
  * Developer questions/comments/feedback: <a href="http://forumBuildingPredictiveModels.pdfs.ohdsi.org/c/developers">OHDSI Forum</a>
  * We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
  PredictionComparison is licensed under Apache License 2.0

Development
===========
  PredictionComparison is being developed in R Studio.

Beta

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.

