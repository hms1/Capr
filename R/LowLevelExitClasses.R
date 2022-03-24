# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
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

# Cohort Exit Classes -------------------------------------------------------------

#' An S4 class for DateOffsetEndStrategy
#'
#' An end strategy class specifying a number of days from the start or end of the initial event until
#' cohort exit
#'
#' @slot DateField a character string specifying either the StartDate or EndDate of the initial event to begin counting
#' days until cohort exit
#' @slot Offset an integer value specifying padding to the cohort exit.
setClass("DateOffsetEndStrategy",
         slots=c(DateField = "character",
                 Offset = "integer"))

# TODO add validity check and show method

#' An S4 class for CustomEraEndStrategy
#'
#' An end strategy class specifying the time until the end of drug use for cohort exit
#'
#' @slot DrugCodesetId the guid of the drug concept set expression to activate in the end strategy
#' @slot GapDays an integer showing the maximum allowable days between successive exposures.
#' @slot Offset an integer value specifying padding to the cohort exit.
setClass("CustomEraEndStrategy",
         slot = c(DrugCodesetId = "character",
                  GapDays = "integer",
                  Offset = "integer"))

# TODO add validity check and show method

#' An S4 class for EndOfCtsObsEndStrategy
#'
#' When the end strategy is not defined the cohort exit is done based on the end of continuous observation.
#' This class is an end strategy type.
#'
#' @slot EndOfContinuousObservation set as true for end strategy option
setClass("EndOfCtsObsEndStrategy",
         slot = c(EndOfContinuousObservation = "logical"),
         prototype = list(EndOfContinuousObservation = TRUE))

# TODO add validity check and show method

#' An S4 class for Collapse Settings
#'
#' A class providing information that identifies the padding for cohort eras
#'
#' @slot Type boolean operator for the number of items in group to include. all, any, at most and at least
#' @slot Count the number of criteria's needed for restriction. If Type is ALL or ANY this value is NA
setClass("CollapseSettings",
         slots = c(CollapseType = "character",
                   EraPad = "integer"),
         prototype = list(CollapseType = "ERA",
                          EraPad = 0L))

# TODO add validity check and show method

#' An S4 class for CensorWindow
#'
#' A class showing dates that indicate the range of entries the are captured in the cohort
#'
#' @slot StartDate the left side of truncation for the study observation
#' @slot EndDate the right side of truncation for the study observation
setClass("CensorWindow",
         slots = c(StartDate = "character",
                   EndDate = "character"),
         prototype = list(StartDate = NA_character_,
                          EndDate = NA_character_))

# TODO add validity check and show method
