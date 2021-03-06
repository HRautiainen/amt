% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track_methods.R
\name{track_methods}
\alias{track_methods}
\alias{velocity}
\alias{velocity.track_xyt}
\alias{nsd}
\alias{nsd.track_xy}
\alias{diff_x}
\alias{diff_x.track_xy}
\alias{diff_y}
\alias{diff_y.track_xy}
\title{Track Methods}
\usage{
velocity(x, ...)

\method{velocity}{track_xyt}(x, ...)

nsd(x, ...)

\method{nsd}{track_xy}(x, ...)

diff_x(x, ...)

\method{diff_x}{track_xy}(x, ...)

diff_y(x, ...)

\method{diff_y}{track_xy}(x, ...)
}
\arguments{
\item{x}{A track_xy{t}.}

\item{...}{Further arguments, none implemented.}
}
\description{
Methods to work with a track.
Function to calculate the absolute direction of a movement track. 0 is north.
}
