% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfmanager.R
\name{dfmanager}
\alias{dfmanager}
\title{\code{dfmanager} is a wrapper function for \code{brktrials2}}
\usage{
dfmanager(nveh, tend, umn, usd, xstart, xfunnel, leff, lane, delt)
}
\arguments{
\item{nveh}{number of vehicles in the simulation, a number}

\item{tend}{end time for a simulation run, a number}

\item{umn}{start speed (mph) for vehicle in lane 1, a number}

\item{usd}{speed volatility for \code{umn}, a number}

\item{xstart}{a vector of  \code{nveh} start locations, (feet), a vector}

\item{xfunnel}{upstream location where the lane drop starts (feet), a number}

\item{leff}{effective vehicle length(feet), a number}

\item{lane}{a vector of \code{nveh} numbers, a vector}

\item{delt}{size in seconds, a number}
}
\value{
\code{dfmanager} returns  \code{t-x} trajectories of \code{nveh} vehicles at a bottleneck.
}
\description{
\code{dfmanager} is a wrapper function for \code{brktrials2}
}
