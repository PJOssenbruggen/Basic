% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zipperwrapper.R
\name{zipperwrapper}
\alias{zipperwrapper}
\title{\code{zipperwrapper} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.}
\usage{
zipperwrapper(nveh1,nveh2,umn,usd,xstart1,xstart2,delt,tstart,tend,xfunnel,leff,type,browse)
}
\arguments{
\item{nveh1}{number of vehicles entering the bottleneck from lane 1, a number}

\item{nveh2}{number of vehicles entering the bottleneck from lane 2, a number}

\item{umn}{start speed (mph) for vehicle in lane 1, a number}

\item{usd}{speed standard deviation, a number}

\item{xstart1}{start location of the first vehicle in lane 1, (feet), a number}

\item{xstart2}{start location of the first vehicle in lane 2, (feet), a number}

\item{delt}{time-step, a number}

\item{tstart}{vehicle crossovers are are permitted below this time, a number}

\item{tend}{upper time range of simulation, a number}

\item{xfunnel}{location where the lane drop is located, a number}

\item{leff}{vehicle length in feet, a number}

\item{type}{TRUE to create plots or FALSE otherwise, a logical}

\item{browse}{to inspect \code{fixviolation} to inspect plot or FALSE otherwise}
}
\value{
The \code{zipperwrapper}, a wrapper function for \code{bmfree3}, \code{xabparam} and
\code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory
for the following vehicle, and critical information. The lead vehicle trajectory is not affected. Passing is not permitted.
The input matrix \code{lane} contains desire lines for a leading and following vehicle.
}
\description{
\code{zipperwrapper} produces \code{t-x} trajectory for vehicles traveling on parallel lanes that merge at a bottleneck.
}
