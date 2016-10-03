#' Provide registration "pulse" for OpenQReg quality registries
#'
#' The aim is to obtain some sort of "pulse", that is during a given period how
#' frequent are data being supplied (and NOT how much). This pulse is provided
#' yearly, monthly and weekly periods measured by the ratio of 'empty' periods
#' wthin the total timeframe (sample intervall). \emph{E.g.} if over a
#' timeframe of 12 months registrations occur in 6 of them the montly pulse
#' becomes 0.5
#'
#' @param years integer vector with years for results and grouping
#' @inheritParams RegPulseData
#' @param peculiarity Logical TRUE for none-OpenQReg registries. False by
#' default
#' @return data frame with registry name and "pulse" for each period
#' @seealso Data to this function is provided by \code{\link{RegPulseData}}
#' and \code{\link{RegPulseDataPeculiar}}.
#' @export

RegPulse <- function(registryName, startDate, endDate, peculiarity = FALSE) {

  # get data
  if (peculiarity) {
    pd <- RegPulseDataPeculiar(registryName, startDate, endDate)
  } else {
    pd <- RegPulseData(registryName, startDate, endDate)
  }

  # make sure data within time frame...
  pd <- pd[pd >= startDate & pd <= endDate]

  # we cannot use NAs, remove them...
  pd <- pd[!is.na(pd)]

  # convert time intervell to (lubri)dates
  startDate <- lubridate::as_date(startDate)
  endDate <- lubridate::as_date(endDate)

  # the overall time intervall
  i <- lubridate::interval(startDate, endDate)

  # make vector for each period
  y <- lubridate::year(pd)
  m <- lubridate::month(pd)
  w <- lubridate::week(pd)
  d <- lubridate::day(pd)

  # daily and weekly pulse, by use of lubridates Duration objects
  dDur <- i / lubridate::ddays(1)
  wDur <- i / lubridate::dweeks(1)
  dPulse <- dim(unique(data.frame(y, w, d)))[1] / dDur
  wPulse <- dim(unique(data.frame(y, w)))[1] / wDur

  # montly and yearly pulse
  totMonths <- length(seq(startDate, endDate, by = "month"))
  mPulse <- dim(unique(data.frame(y, m)))[1] / totMonths
  totYears <- length(seq(startDate, endDate, by = "year"))
  yPulse <- length(unique(y)) / totYears

  pulse <- data.frame(year=yPulse, month=mPulse, week=wPulse, day=dPulse)

  return(pulse)
}