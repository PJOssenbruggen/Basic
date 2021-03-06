% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{run}
\alias{run}
\title{The \code{run} function is a wrapper for the \code{trial} and \code{plotfollow3} functions.}
\usage{
run(nveh, ulead, ulead.sd, ubrkdown, ubrkdown.sd, k, k.sd, d, leff, xlim,
  ylim)
}
\arguments{
\item{nveh}{number of vehicles in the investigation or \code{vehicle} = {1, 2, ..., \code{nveh}}, a number}

\item{ulead}{lead vehicle speed (mph) at time \code{t0}, i.e., before breakdown, a number}

\item{ulead.sd}{standard deviation of \code{ulead}, a number}

\item{ubrkdown}{breakdown speed (mph) of lead vehicle, a number}

\item{ubrkdown.sd}{standard deviation of \code{ubrkdown}, a number}

\item{k}{traffic density (vehicles per mile, vpm) of the pre-breakdown state, a number}

\item{k.sd}{standard deviation of following vehicle breakdown speed, a number}

\item{d}{deceleration rate of the lead  \code{l} vehicle, a number}

\item{leff}{effective vehicle length (feet), a number}

\item{xlim}{plot range along the \code{t} axis, a vector}

\item{ylim}{plot range along the \code{x} axis, a vector}
}
\value{
The function returns an \code{t-x} image and eight lists:
 \code{output[[1]]} returns a vector of \code{run} input values; \code{output[[2]]} returns
 a matrix of pre-breakdown \code{ulead} speed, breakdown \code{ubrkdown} speed,
 pre-breakdown \code{k} density values for each vehicle; \code{output[[3]]} returns
 a time matrix consisting of \code{t0,t1,t2,t3,t4,t5};
 \code{output[[4]], output[[5]], output[[6]]} and \code{output[[7]]} returns matrices
 of speed \code{u}, location {x} and safe headway \code{h} values for each vehicle
 at times \code{t0,t1,t2,t3}, respectively. Finally, \code{output[[8]]} returns a list
 the \code{cflist} list consisting of information stored as \code{L} and \code{Fw} matrices.
}
\description{
The \code{run} function is a wrapper for the \code{trial} and \code{plotfollow3} functions.
}
