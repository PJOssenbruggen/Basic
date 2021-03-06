% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brktrials3wrapper.R
\name{brktrials3wrapper}
\alias{brktrials3wrapper}
\title{\code{brktrials3wrapper} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck}
\usage{
brktrials3wrapper(nveh1,nveh2,umn,usd,tstart,tend,xstart1,xstart2,delt,run,leff,xfunnel,kfactor)
}
\arguments{
\item{nveh1}{number of vehicles entering the bottleneck from lane 1, a number}

\item{nveh2}{number of vehicles entering the bottleneck from lane 2, a number}

\item{umn}{start speed (mph) for vehicle in lane 1, a number}

\item{usd}{speed standard deviation, a number}

\item{tstart}{start time, (seconds), a number}

\item{tend}{end time, (seconds), a number}

\item{xstart1}{start location of the first vehicle in lane 1, (feet), a number}

\item{xstart2}{start location of the first vehicle in lane 2, (feet), a number}

\item{delt}{time-step size in seconds, a number}

\item{run}{number, a number}

\item{leff}{vehicle length in feet, a number}

\item{xfunnel}{upstream location of bottleneck taper, a number}

\item{kfactor}{density at time \code{t} = 0, a number}
}
\value{
\code{brktrials3} returns a list of two matrices with 3 times \code{nveh} columns.
}
\description{
\code{brktrials3wrapper} produces \code{t-x} trajectories for lead and following vehicles at a bottleneck
}
