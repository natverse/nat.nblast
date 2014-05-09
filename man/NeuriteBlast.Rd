\name{NeuriteBlast}
\alias{NeuriteBlast}
\title{Produce similarity score for neuron morphologies}
\usage{
NeuriteBlast(query, target = getOption(nat.default.neuronlist),
  targetBinds = NULL, Reverse = FALSE, ...)
}
\arguments{
  \item{query}{the query neuron.}

  \item{target}{a \code{\link[nat]{neuronlist}} to compare
  neuron against.  Defaults to
  \code{options("nat.default.neuronlist")}.}

  \item{targetBinds}{numeric indices or names with which to
  subset \code{target}.}

  \item{Reverse}{whether to use \code{query} as the target
  neuron rather than query (default=FALSE).}

  \item{...}{extra arguments to pass to the distance
  function.}
}
\value{
Named list of similarity scores.
}
\description{
A low-level version of the NBLAST algorithm that compares
the morphology of a neuron with those of a list of other
neurons. For most use cases, one probably would wish to use
\code{\link{nblast()}} instead.
}
\seealso{
\code{\link{WeightedNNBasedLinesetMatching}}
}

