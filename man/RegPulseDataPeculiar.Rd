% Generated by roxygen2 (4.1.0.9000): do not edit by hand
% Please edit documentation in R/RegPulseDataPeculiar.R
\name{RegPulseDataPeculiar}
\alias{RegPulseDataPeculiar}
\title{Provide registration dates from quality registries other than OpenQReg}
\format{Return a class Date vector of dates}
\usage{
RegPulseDataPeculiar(registryName, startDate, endDate)
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
Provides registration dates from an registers other than OpenQReg. The aim
is to obtain some sort of "pulse", that is how frequent data are being
supplied (and NOT how much)
}
\details{
For the queries these conditions apply:
\describe{
 \item{}{Dates of MRS registries are collected from field \emph{FormDate}}
 \item{}{Table names may vary. Therefore each registry has its own query}
 \item{}{Date field is aliased 'OpprettetDato' throughout}
}
}
\seealso{
This function is used by \code{\link{RegPulse}}.
}

