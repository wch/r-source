% File src/library/parallel/man/sendData.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2003-2025 R Core Team
% Distributed under GPL 2 or later

\name{sendData}
\alias{closeNode}
\alias{recvData}
\alias{recvOneData}
\alias{sendData}
\title{Cluster Back-end Interface}
\description{
  The communication primitives used by the \pkg{parallel} package to
  handle the state and communicate with nodes in the clusters.
}
\usage{
  sendData(node, data)
  recvData(node)
  recvOneData(cl)
  closeNode(node)
}
\arguments{
  \item{cl}{
    The cluster object, visible to the user. Should be a list inheriting
    from class \code{cluster}, containing the node objects.
  }
  \item{node}{
    The node object corresponding to one execution unit inside the
    cluster.
  }
  \item{data}{
    The data structure containing a message to the node.
  }
}
\details{
  A \code{`[.cluster`} method is provided, which retains the classes of the
  cluster when subset. The cluster back-end should either rely on this method or
  supply its own method that also invokes this method through \code{NextMethod}
  or calls \code{.subset} directly.

  The \code{data} messages sent to the nodes are lists containing the
  following elements: \describe{
    \item{type}{
      A short string describing the type of packet: \describe{
        \item{DONE}{
          Sent by the default \code{stopCluster} implementation before
          calling \code{closeNode}.
        }
        \item{EXEC}{
          The packet contains a job to execute.
        }
      }
    }
    \item{value}{
      For messages of type \dQuote{EXEC}, a list with the following
      elements: \describe{
        \item{fun}{
          The function to execute.
        }
        \item{args}{
          The arguments for \code{fun} above as a list.
        }
        \item{return}{
          Defaults to \code{TRUE}. Not currently used by \pkg{parallel}.
        }
        \item{tag}{
          The same tag must be returned back from the worker. Used to
          identify individual elements of a larger job when using
          dynamic load balancing.
        }
      }
    }
  }

  If the \dQuote{DONE} messages are used (for example when calling
  \code{stopCluster.default}), the node can close the connection upon
  receipt.

  The response to an \dQuote{EXEC} message that should be returned by
  \code{recvData} is a list with the following elements: \describe{
    \item{type}{A string, \code{"VALUE"}.}
    \item{value}{
      The value of \code{do.call(fun, args, quote = TRUE)}. If the
      evaluation raised an error, the value of the error.
    }
    \item{success}{
      A logical scalar indicating whether the evaluation completed
      without raising an error.
    }
    \item{time}{
      The time it took to complete the job, an object of class
      \code{proc_time}. Can be obtained using
      \code{\link[base]{system.time}} or by subtracting outputs of
      \code{\link[base]{proc.time}}.
    }
    \item{tag}{
      The original \code{tag} from the \dQuote{EXEC} message.
    }
  }

  \code{recvData} can block if the job is not yet complete, and
  \code{recvOneData} should block until at least one node is able to
  return a complete job result.

  The default \code{closeNode} method does nothing. It is envisaged that
  \code{stopCluster} is used to shut down the entire cluster, although other
  back-ends may use this to implement node-specific logic.
}
\value{
  \item{sendData}{
    Ignored. Called for the side effect of sending the \code{data} to the
    node.
  }
  \item{recvData}{
    The result of the job previously submitted to the node.
  }
  \item{recvOneData}{
    A list with the following items: \describe{
      \item{node}{The index of the node returning the data.}
      \item{value}{The result of \code{recvData(cluster[[node]])}.}
    }
  }
  \item{closeNode}{
    Ignored. Called for the side effect of cleaning up the connection to
    the node.
  }
}
\seealso{
  \code{\link{stopCluster}} should also be implemented, but is a user
  interface and documented separately. The default method will post
  termination messages to individual nodes and then call
  \code{closeNode} on them.
}
\examples{\dontrun{
  # A toy cluster consisting of one connection.
  sendData.mynode <- function(node, data) serialize(data, node)
  recvData.mynode <- function(node) unserialize(node)
  recvOneData.mycluster <- function(cl) list(
    node = 1, value = recvData(cl[[1]])
  )
  closeNode.mynode <- function(node) close(node)

  # Not shown: R starting a serverSocket on the other end, ready to
  # accept connections and evaluate jobs
  cl <- structure(list(
    structure(
      socketConnection(..., blocking = TRUE, open = 'a+b'),
      class = 'mynode'
    )
  ), class = c('mycluster', 'cluster'))
  clusterEvalQ(cl, Sys.getpid())
  stopCluster(cl)
  rm(cl)
}}
\keyword{internal}
