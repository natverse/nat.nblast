\name{nblast2}
\alias{nblast2}
\title{Produce similarity score for neuron morphologies}
\usage{
nblast2(query, target, targetBinds = NULL, Reverse = FALSE, ...)
}
\arguments{
  \item{query}{the query neuron.}

  \item{target}{a \code{\link[nat]{neuronlist}} to compare
  neuron against.  Defaults to
  \code{options(nat.default.neuronlist)}.}

  \item{targetBinds}{numeric indices or names with which to
  subset \code{target}.}

  \item{Reverse}{whether to use \code{query} as the target
  neuron rather than query (default=FALSE).}
}
\value{
Named list of similarity scores.
}
\description{
An updated version of the NBLAST algorithm that compares
the morphology of a neuron with those of a list of other
neurons.
}
\examples{
scores<-NeuriteBlast2(dps[["ChaMARCM-F000015_seg002"]])
# use blast style sum lod (logarithm odds ratio) scores
scores2=NeuriteBlast2(dps[['DvGlutMARCM-F004047_seg001']],NNDistFun=lodsby2dhist)
}
\seealso{
\code{\link{WeightedNNBasedLinesetMatching}}
}

