\name{L}
\alias{L}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A matrix containing information about a lead vehicle.
}
\description{
A matrix of 4 columns and 4 rows where the first through fourth rows contain time \code{t}, speed \code{u}, location \code{x} and safe headway \code{h} information
}
\value{
\item{t0}{time when a lead vehicle begins to decelerate.}
   \item{t1}{\code{NA} for a lead vehicle.}
   \item{t2}{\code{NA} for a lead vehicle.}
   \item{t3}{time when the lead vehicle reaches a breakdown speed.}
   \item{u0}{lead vehicle speed at \code{t0}.}
   \item{u1}{\code{NA} for a lead vehicle.}
   \item{u2}{\code{NA} for a lead vehicle.}
   \item{u3}{lead vehicle breakdown speed.}
   \item{x0}{lead vehicle location at \code{t0}.}
   \item{x1}{\code{NA} for a lead vehicle.}
   \item{x2}{\code{NA} for a lead vehicle.}
   \item{x3}{lead vehicle location at \code{t3}.}
   \item{h0}{safe headway at \code{t0}.}
   \item{h1}{\code{NA} for a lead vehicle.}
   \item{h2}{\code{NA} for a lead vehicle.}
   \item{h3}{safe headway at \code{t3}.}
}

\author{
Paul J Ossenbruggen
}


