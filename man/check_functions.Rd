% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_functions}
\alias{check_functions}
\title{Check functions}
\arguments{
\item{path}{file path to the package source directory}

\item{n}{\code{integer} value indicating maximum number of subprocesses that can
be simultaneously spawned when executing tasks.}

\item{output}{\code{character} value specifying path where the output should be
stored.}

\item{lib.loc}{\code{character} vector with libraries allowed to be used when
checking packages, defaults to entire \code{\link[=.libPaths]{.libPaths()}}.}

\item{repos}{\code{character} vector of repositories which will be used when
generating task graph and later pulling dependencies.}

\item{reverse_repos}{\code{character} vector of repositories which will be used
to pull sources for reverse dependencies. In some cases, for instance using
binaries on Linux, we want to use different repositories when pulling
sources to check and different when installing dependencies.}

\item{...}{Additional arguments passed to \code{\link{checked-task-df}} and \code{\link[=run]{run()}}}
}
\value{
\code{\link[=check_design]{check_design()}} R6 class storing all the details
regarding checks that run. Can be combined with
\code{\link{results}} and \code{\link[=summary]{summary()}} methods to generate results.
}
\description{
Set of functions to run orchestrated \verb{R CMD check}s and automatically manage
the dependencies installation. Each functions prepares the plan based on the
supplied package source(s) which includes installing dependencies and running
required \verb{R CMD check}s. All the functions are parallelized through sperate
processes
}
\keyword{internal}
