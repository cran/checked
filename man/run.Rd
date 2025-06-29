% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{run}
\alias{run}
\title{Run a Series of \verb{R CMD check}s}
\usage{
run(design, ..., reporter = reporter_default())
}
\arguments{
\item{design}{\code{character} or \code{check_design} If a \code{character} value is
provided, it is first coerced into a \code{check_design} using
\code{\link[=new_rev_dep_check_design]{new_rev_dep_check_design()}}.}

\item{...}{Additional arguments passed to \code{\link[=new_rev_dep_check_design]{new_rev_dep_check_design()}}}

\item{reporter}{A reporter to provide progress updates. Will default to the
most expressive command-line reporter given your terminal capabilities.}
}
\description{
\code{\link[=run]{run()}} provides a generic, and is the central interface for executing
\code{\link{check_design}}s. If a path is provided, a new reverse dependency check
plan is generated from the source code path. Otherwise a plan can be
built separately and executed using \code{\link[=run]{run()}}.
}
