\name{F}
\alias{F}
\title{
A matrix containing information about a following vehicle.
}
\description{
A matrix of 4 columns and 4 rows where the first through fourth rows contain time \code{t}, speed \code{u}, location \code{x} and safe headway \code{h} information
}
\value{
\item{t0}{time when a following vehicle enters the analysis.}
   \item{t1}{time when a following vehicle begins to decelerate.}
   \item{t2}{time when a following vehicle begins to decelerate.}
   \item{t3}{time when a following vehicle reaches a breakdown speed \code{ubrkdown}.}
   \item{u0}{following vehicle speed at \code{t0}.}
   \item{u1}{following vehicle speed at \code{t1}.}
   \item{u2}{following vehicle speed at \code{t2}.}
   \item{u3}{following vehicle speed at \code{t3}.}
   \item{x0}{following vehicle location at \code{t0}.}
   \item{x1}{following vehicle location at \code{t1}.}
   \item{x2}{following vehicle location at \code{t2}.}
   \item{x3}{following vehicle location at \code{t3}.}
   \item{h0}{safe headway at \code{t0}.}
   \item{h1}{safe headway at \code{t1}.}
   \item{h2}{safe headway at \code{t2}.}
   \item{h3}{safe headway at \code{t3}.}
}
\author{
Paul J Ossenbruggen
}

