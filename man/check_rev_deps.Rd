% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_rev_deps}
\alias{check_rev_deps}
\title{Check reverse dependencies}
\usage{
check_rev_deps(
  path,
  n = 2L,
  output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
  lib.loc = .libPaths(),
  repos = getOption("repos"),
  reverse_repos = repos,
  restore = options::opt("restore"),
  reporter = reporter_default(),
  ...
)
}
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

\item{restore}{\code{logical} indicating whether output directory should be unlinked before
running checks. If \code{FALSE}, an attempt will me made to restore previous
progress from the same \code{output} (Defaults to \code{NA}, overwritable using option 'checked.restore' or environment variable 'R_CHECKED_RESTORE')}

\item{reporter}{A reporter to provide progress updates. Will default to the
most expressive command-line reporter given your terminal capabilities.}

\item{...}{Additional arguments passed to \code{\link{checked-task-df}} and \code{\link[=run]{run()}}}
}
\value{
\code{\link[=check_design]{check_design()}} R6 class storing all the details
regarding checks that run. Can be combined with
\code{\link{results}} and \code{\link[=summary]{summary()}} methods to generate results.
}
\description{
Check a package's reverse dependencies in order to identify differences
in reverse dependency check results when run alongside your package's
development and release versions.
}
\details{
Runs classical reverse dependency checks for the given source package. It
first identifies reverse dependencies available in \code{repos}. Then, after
installing all required dependencies, runs \verb{R CMD check} twice for each
package, one time with the release version of the given source package
installed from \code{repos} and a second time with the development version
installed from local source. Both \verb{R CMD checks} are later compared to
identify changes in reverse dependency behaviors.
}
\seealso{
Other checks: 
\code{\link{check_design}},
\code{\link{check_dev_rev_deps}()},
\code{\link{check_dir}()},
\code{\link{check_pkgs}()},
\code{\link{new_check_design}()}
}
\concept{checks}
