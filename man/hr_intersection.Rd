% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hr_overlap.R
\name{hr_intersection}
\alias{hr_intersection}
\alias{hr_intersection.hr_prob}
\title{Different Methods to calculate home-range intersections}
\usage{
hr_intersection(x, ...)

\method{hr_intersection}{hr_prob}(x, y, type = "ba", conditional = 0.95, ...)
}
\arguments{
\item{x, y}{A probabilistic home-range estimate.}

\item{...}{Further arguments, none implemented.}

\item{type}{\verb{[character]} \cr Which index should be calculated.}

\item{conditional}{\verb{[numeric]} \cr Condition on which number?}
}
\value{
\code{data.frame} with the isopleth level and area in units of the coordinate reference system.
}
\description{
Different Methods to calculate home-range intersections
}
