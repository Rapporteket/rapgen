#' Provide registration dates from OpenQReg quality registries
#'
#' Provides registrarion dates from an OpenQReg register. The aim is to obtain
#' some sort of "pulse", that is how frequent data are being supplied (and NOT
#' how much)
#'
#' @param registryName String Key/name of registry to get data from
#' @param startDate String Date defining the lower limit (endpoint included) of
#' data sample from the registry given as 'YYYY-MM-DD'
#' @param endDate String Date defining the upper limit (endpoint included) of
#' data sample from the registry given as 'YYYY-MM-DD'
#' @format Return a class Date vector of dates
#'
#' @details For the query these conditions apply:
#' \describe{
#' \item{Dates are collected from table \emph{SkjemaOversikt}, field
#'  \emph{OpprettetDato}}
#' \item{All registry forms (field \emph{SkjemaNavn}) are valid}
#' \item{All registrarion states (field \emph{SkjemaStatus}) are valid}
#' }
#' @return regPulseData Date Vector of dates
#' @seealso This function is used by \code{\link{RegDelay}}.
#' @export


RegPulseData <- function(registryName, startDate, endDate) {

  dbType <- "mysql"

  query <- paste0("
  SELECT
    OpprettetDato
  FROM
    SkjemaOversikt
  WHERE
    OpprettetDato >='", startDate, "' AND
    OpprettetDato <='", endDate, "';"
  )

  regPulseData <- rapbase::LoadRegData(registryName, query, dbType)
  regPulseData <- lubridate::as_date(regPulseData$OpprettetDato)

  return(regPulseData)
}
