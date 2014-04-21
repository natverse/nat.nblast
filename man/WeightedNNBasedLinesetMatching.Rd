\name{WeightedNNBasedLinesetMatching}
\alias{WeightedNNBasedLinesetMatching}
\alias{WeightedNNBasedLinesetMatching.dotprops}
\title{Compute point & tangent vector similarity score between two dotprops objects}
\usage{
WeightedNNBasedLinesetMatching(...)

\method{WeightedNNBasedLinesetMatching}{dotprops}(dp1, dp2, UseAlpha = FALSE,
  ...)
}
\arguments{
  \item{dp1,dp2}{dotprops objects}

  \item{UseAlpha}{Whether to scale dot product of tangent
  vectors (default=F)}

  \item{...}{extra arguments to pass to the distance
  function.}
}
\value{
Return value of NNDistFun passd to
WeightedNNBasedLinesetMatching
}
\description{
UseAlpha determines whether the alpha values
(eig1-eig2)/sum(eig1:3) are passed on to
WeightedNNBasedLinesetMatching. These will be used to scale
the dot products of the direction vectors for nearest
neighbour pairs.
}
\seealso{
\code{\link[nat]{dotprops}}
}

