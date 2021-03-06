% Generated by roxygen2 (4.1.0.9000): do not edit by hand
% Please edit documentation in R/RegPulseData.R
\name{RegPulseData}
\alias{RegPulseData}
\title{Provide registration dates from OpenQReg quality registries}
\format{Return a class Date vector of dates}
\usage{
RegPulseData(registryName, startDate, endDate)
}
\arguments{
\item{registryName}{String Key/name of registry to get data from}

\item{startDate}{String Date defining the lower limit (endpoint included) of
data sample from the registry given as 'YYYY-MM-DD'}

\item{endDate}{String Date defining the upper limit (endpoint included) of
data sample from the registry given as 'YYYY-MM-DD'}
}
\value{
regPulseData Date Vector of dates
}
\description{
Provides registration dates from an OpenQReg register. The aim is to obtain
some sort of "pulse", that is how frequent data are being supplied (and NOT
how much)
}
\details{
For the query these conditions apply:
\describe{
 \item{}{Dates are collected from table \emph{SkjemaOversikt}, field
 \emph{OpprettetDato}}
 \item{}{All registry forms (field \emph{SkjemaNavn}) are valid}
 \item{}{All registrarion states (field \emph{SkjemaStatus}) are valid}
}
}
\seealso{
This function is used by \code{\link{RegPulse}}.
}

