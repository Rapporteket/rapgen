#' Provide registration dates from quality registries other than OpenQReg
#'
#' Provides registration dates from an registers other than OpenQReg. The aim
#' is to obtain some sort of "pulse", that is how frequent data are being
#' supplied (and NOT how much)
#'
#' @param registryName String Key/name of registry to get data from
#' @param startDate String Date defining the lower limit (endpoint included) of
#' data sample from the registry given as 'YYYY-MM-DD'
#' @param endDate String Date defining the upper limit (endpoint included) of
#' data sample from the registry given as 'YYYY-MM-DD'
#' @format Return a class Date vector of dates
#'
#' @details For the queries these conditions apply:
#' \describe{
#' \item{Dates of MRS registries are collected from field \emph{FormDate}}
#' \item{Table names may vary. Therefore each registry has its own query}
#' \item{Date field is aliased 'OpprettetDato' throughout}
#' }
#' @return regPulseData Date Vector of dates
#' @seealso This function is used by \code{\link{RegPulse}}.
#' @export


RegPulseDataPeculiar <- function(registryName, startDate, endDate) {

  dbType <- "mysql"

  if (registryName == "nir") {
    query <- paste0("
    SELECT
      DATE(FormDate) as OpprettetDato
    FROM
      Main
    WHERE
      DATE(FormDate) >='", startDate, "' AND
      DATE(FormDate) <='", endDate, "';"
    )
  } else if (registryName == "Hjerneslag") {
    query <- "

    "
  } else if (registryName == "NorScir") {
    query <- "

    "
  } else if (registryName == "nkr") {
    dbType <- "mssql"
    query <- "

    "
  } else {
    stop("\nThe registry name provided is not a valid one.\n")
  }

  regPulseData <- rapbase::LoadRegData(registryName, query, dbType)
  regPulseData <- lubridate::as_date(regPulseData$OpprettetDato)

  return(regPulseData)
}
