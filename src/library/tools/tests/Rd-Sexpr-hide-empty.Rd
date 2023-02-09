\name{SexprExample}
\title{title}
\description{description}
\details{
  Hello
  \Sexpr[stage=build,results=hide]{
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    invisible(NULL)
    "" # workaround: remove results=hide and use the return value
  }
}
