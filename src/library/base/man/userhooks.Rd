\name{UserHooks}
\alias{getUserOnLoadHook}
\alias{setUserOnLoadHook}
\alias{getUserAttachHook}
\alias{setUserAttachHook}
\alias{getUserDetachHook}
\alias{setUserDetachHook}
\alias{getUserOnUnloadHook}
\alias{setUserOnUnloadHook}
\alias{.userHooksEnv}
\title{Functions to Get and Set Hooks for Load, Attach, Detach and Unload}
\description{
  These function allow users to set actions to be taken before packages
  are attached/detached and namespaces are (un)loaded.
}
\usage{
getUserOnLoadHook(pkgname)
setUserOnLoadHook(pkgname, value,
                  action = c("append", "prepend", "replace"))

getUserAttachHook(pkgname)
setUserAttachHook(pkgname, value,
                  action = c("append", "prepend", "replace"))

getUserDetachHook(pkgname)
setUserDetachHook(pkgname, value,
                  action = c("prepend", "append", "replace"))

getUserOnUnloadHook(pkgname)
setUserOnUnloadHook(pkgname, value,
                    action = c("prepend", "append", "replace"))
}
\arguments{
  \item{pkgname}{character string: the package/namespace name}
  \item{value}{A function, or for \code{action="replace"}, \code{NULL}.}
  \item{action}{The action to be taken.}
}

\details{
  When an \R package is attached by \code{\link{library}}, it can call
  initialization code via a function \code{.First.lib}, and when it is
  \code{\link{detach}}-ed it can tidy up via a function \code{.Last.lib}.
  Users can add their own initialization code via the hooks provided by
  these functions, functions which will be
  called as \code{funname(pkgname, pkgpath)} inside a \code{\link{try}}
  call.  (The attach hook is called after \code{.First.lib} and the detach
  hook before \code{.Last.lib}.)

  If a package has a namespace, there are two further actions, when the
  namespace is loaded (before being attached and after \code{.onLoad} is
  called ) and when it is unloaded (after being detached and before
  \code{.onUnload}).  Note that code in these hooks is run without the
  package being on the search path, so objects in the package need to be
  referred to using the double colon operator as in the example.

  To remove a hook completely, call
  \code{setUserOnUnloadHook(pkgname, NULL, "replace")}.

  The hooks are run in the order shown by the \code{get*} functions, and
  the default actions are to add new functions \sQuote{inside} existing ones.
  
  Note that when an \R is finished, packages are not detached and
  namespaces are not unloaded, so the corresponding hooks will not be
  run.

  If versioned install has been used, \code{pkgname} is the unversioned
  name of the package.

  The hooks are stored in the environment \code{.userHooksEnv} in the
  base package.
}
\value{
  For the \code{get*} functions, a list of functions (possible empty).
  For the \code{set*} functions, no return value.
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{unloadNamespace}}
}
\examples{
setUserOnLoadHook("graphics",
   function(...) graphics::ps.options(horizontal=FALSE)) 
}
\keyword{utilities}
