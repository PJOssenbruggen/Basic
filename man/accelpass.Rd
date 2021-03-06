% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelpass.R
\name{accelpass}
\alias{accelpass}
\title{The \code{accelpass} function estimates the relative locations of two vehicles where one passes the other.}
\usage{
accelpass(tend, umn, usd, xstart, xfunnel, leff, lane, delt)
}
\arguments{
\item{tend}{end time for a simulation run, a number}

\item{umn}{start speed (mph) for vehicle in lane 1, a number}

\item{usd}{speed volatility for \code{umn}, a number}

\item{xstart}{start location for vehicle in lane 1 (feet), a number}

\item{xfunnel}{upstream location where the lane drop starts (feet), a number}

\item{leff}{effective vehicle length(feet), a number}

\item{lane}{number, a number}

\item{delt}{size in seconds, a number}
}
\value{
\code{accelpass} uses a stochastic model to show the locations of vehicle accelerating to pass
another vehicle traveling side-by-side at the speed.
}
\description{
The \code{accelpass} function estimates the relative locations of two vehicles where one passes the other.
}
