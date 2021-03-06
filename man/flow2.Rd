% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flow2.R
\name{flow2}
\alias{flow2}
\title{\code{flow2} is an algorithm for estimating the flow and density}
\usage{
flow2(dfcrit, df1df2, tstart, tend, delt, xfunnel)
}
\arguments{
\item{dfcrit}{times when vehicles arrive at bottleneck by lane, a matrix}

\item{df1df2}{vehicle information from \code{brktrials4wrapper}, a matrix}

\item{tstart}{start time, (seconds), a number}

\item{tend}{end time, (seconds), a number}

\item{delt}{size in seconds, a number}

\item{xfunnel}{upstream location of bottleneck taper, a number}
}
\value{
\code{flow2} returns data.frame of density and flow estimates at two bottleneck locations.
}
\description{
\code{flow2} is an algorithm for estimating the flow and density
}
