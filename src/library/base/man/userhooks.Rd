% File src/library/base/man/userhooks.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{userhooks}
\alias{getHook}
\alias{setHook}
\alias{packageEvent}
\alias{.userHooksEnv}
\title{Functions to Get and Set Hooks for Load, Attach, Detach and Unload}
\description{
  These functions allow users to set actions to be taken before packages
  are attached/detached and namespaces are (un)loaded.
}
\usage{
getHook(hookName)
setHook(hookName, value,
        action = c("append", "prepend", "replace"))

packageEvent(pkgname,
             event = c("onLoad", "attach", "detach", "onUnload"))
}
\arguments{
  \item{hookName}{character string: the hook name}
  \item{pkgname}{character string: the package/namespace name}
  \item{event}{character string: an event for the package.  Can be abbreviated.}
  \item{value}{A function or a list of functions, or for \code{action = "replace"}, \code{NULL} }
  \item{action}{The action to be taken.  Can be abbreviated.}
}

\details{
  \code{setHook} provides a general mechanism for users to register
  hooks, a list of functions to be called from system (or user)
  functions.  The initial set of hooks was associated with events on
  packages/namespaces: these hooks are named via calls to
  \code{packageEvent}.

  To remove a hook completely, call \code{setHook(hookName, NULL, "replace")}.

  When an \R package is attached by \code{\link{library}} or loaded by
  other means, it can call initialization code.  See
  \code{\link{.onLoad}} for a description of the package hook functions
  called during initialization.  Users can add their own initialization
  code via the hooks provided by \code{setHook()}, functions which will
  be called as \code{funname(pkgname, pkgpath)} inside a
  \code{\link{try}} call.

  The sequence of events depends on which hooks are defined, and whether
  a package is attached or just loaded.  In the case where all hooks
  are defined and a package is attached, the order of initialization
  events is as follows:
  \enumerate{
  \item The package namespace is loaded.
  \item The package's \code{\link{.onLoad}} function is run.
  \item If S4 methods dispatch is on, any actions set by
  \code{\link{setLoadAction}} are run.
  \item The namespace is sealed.
  \item The user's \code{"onLoad"} hook is run.
  \item The package is added to the search path.
  \item The package's \code{\link{.onAttach}} function is run.
  \item The package environment is sealed.
  \item The user's \code{"attach"} hook is run.
  }

  A similar sequence (but in reverse) is run when a package is detached
  and its namespace unloaded:
  \enumerate{
  \item The user's \code{"detach"} hook is run.
  \item The package's \code{\link{.Last.lib}} function is run.
  \item The package is removed from the search path.
  \item The user's \code{"onUnload"} hook is run.
  \item The package's \code{\link{.onUnload}} function is run.
  \item The package namespace is unloaded.
  }
  Note that when an \R session is finished, packages are not detached and
  namespaces are not unloaded, so the corresponding hooks will not be
  run.

  Also note that some of the user hooks are run without the package
  being on the search path, so in those hooks objects in the package
  need to be referred to using the double (or triple) colon operator,
  as in the example.

  If multiple hooks are added, they are normally run in the order shown
  by \code{getHook}, but the \code{"detach"} and \code{"onUnload"} hooks
  are run in reverse order so the default for package events is to add
  hooks \sQuote{inside} existing ones.

  The hooks are stored in the environment \code{.userHooksEnv} in the
  base package, with \sQuote{mangled} names.
}
\value{
  For \code{getHook} function, a list of functions (possibly empty).
  For \code{setHook} function, no return value.
  For \code{packageEvent}, the derived hook name (a character string).
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{loadNamespace}}.

  See \code{\link{::}}
  for a discussion of the double and triple colon operators.

  Other hooks may be added later: functions \code{\link{plot.new}} and
  \code{\link{persp}} already have them.
}
\examples{
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::ps.options(horizontal = FALSE))
}
\keyword{utilities}
