\name{UserHooks}
\alias{getHook}
\alias{setHook}
\alias{packageEvent}
\alias{.userHooksEnv}
\title{Functions to Get and Set Hooks for Load, Attach, Detach and Unload}
\description{
  These function allow users to set actions to be taken before packages
  are attached/detached and namespaces are (un)loaded.
}
\usage{
getHook(hookName)
setHook(hookName, value, action = c("append", "prepend", "replace"))

packageEvent(pkgname, event=c("onLoad", "attach", "detach", "onUnload"))
}
\arguments{
  \item{hookName}{character string: the hook name}
  \item{pkgname}{character string: the package/namespace name.
    If versioned install has been used, \code{pkgname} should the unversioned
    name of the package and any version information will be stripped.}
  \item{event}{character string: an event for the package}
  \item{value}{A function, or for \code{action="replace"}, \code{NULL}.}
  \item{action}{The action to be taken.  The names can be appreviated.}
}

\details{
  \code{setHook} provides a general mechanism for users to register
  hooks, a list of functions to be called from system (or user)
  functions.  The initial set of hooks is associated with events on
  packages/namespaces: these hooks are named via calls to \code{packageEvent}.

  To remove a hook completely, call \code{setHook(hookName, NULL, "replace")}.
  
  When an \R package is attached by \code{\link{library}}, it can call
  initialization code via a function \code{.First.lib}, and when it is
  \code{\link{detach}}-ed it can tidy up via a function \code{.Last.lib}.
  Users can add their own initialization code via the hooks provided by
  these functions, functions which will be called as
  \code{funname(pkgname, pkgpath)} inside a \code{\link{try}}
  call.  (The attach hook is called after \code{.First.lib} and the detach
  hook before \code{.Last.lib}.)

  If a package has a namespace, there are two further actions, when the
  namespace is loaded (before being attached and after \code{.onLoad} is
  called ) and when it is unloaded (after being detached and before
  \code{.onUnload}).  Note that code in these hooks is run without the
  package being on the search path, so objects in the package need to be
  referred to using the double colon operator as in the example.
  (Unlike \code{.onLoad}, the user hook is run after the name space has
  been sealed.)

  Hooks are normally run in the order shown by \code{getHook},
  but the \code{"detach"} and \code{"onUnload"} hooks are run
  in reverse order so the default for package events is to add hooks
  \sQuote{inside} existing ones.
  
  Note that when an \R is finished, packages are not detached and
  namespaces are not unloaded, so the corresponding hooks will not be
  run.

  The hooks are stored in the environment \code{.userHooksEnv} in the
  base package, with \sQuote{mangled} names.
}
\value{
  For \code{getHook} function, a list of functions (possible empty).
  For \code{setHook} function, no return value.
  For \code{packageEvent}, the derived hook name (a character string).
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{loadNamespace}}.

  Other hooks may be added later: \code{\link{plot.new}} and
  \code{\link{persp}} already have them.
}
\examples{
setHook(packageEvent("graphics", "onLoad"),
        function(...) grDevices::ps.options(horizontal=FALSE)) 
}
\keyword{utilities}
