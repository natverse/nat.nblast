\name{nblast}
\alias{nblast}
\title{Produce similarity score for neuron morphologies}
\usage{
nblast(query, target, smat = get(getOption("nat.nblast.defaultsmat")),
  version = c("2", "1"), ...)
}
\arguments{
  \item{query}{the query neuron.}

  \item{target}{a \code{\link[nat]{neuronlist}} to compare
  neuron against.  Defaults to
  \code{options("nat.default.neuronlist")}.}

  \item{smat}{the score matrix to use.}

  \item{version}{the version of the algorithm to use (the
  default, 2, is the latest).}

  \item{...}{extra arguments to pass to the distance
  function.}
}
\value{
Named list of similarity scores.
}
\description{
Uses the NBLAST algorithm that compares the morphology of
two neurons.
}

