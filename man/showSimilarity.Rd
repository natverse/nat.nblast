\name{showSimilarity}
\alias{showSimilarity}
\title{Display two neurons with segments coloured by similarity}
\usage{
showSimilarity(n1, n2, smat = get(getOption("nat.nblast.defaultsmat")),
  cols = colorRampPalette(c("#0000FF", "#FF0000")), AbsoluteScale = FALSE,
  PlotVectors = TRUE, ...)
}
\arguments{
  \item{n1}{a neuron to compare and colour.}

  \item{n2}{the neuron to compare against.}

  \item{smat}{a score matrix.}

  \item{cols}{the function to use to colour the segments
  (e.g. \code{\link{heat.colors}}).}

  \item{AbsoluteScale}{logical indicating whether the
  colours should be calculated based on the minimum and
  maximum similarities for the neuron (\code{AbsoluteScale
  = FALSE}) or on the minimum and maximum possible for all
  neurons.}

  \item{PlotVectors}{logical indicating whether the vectors
  of the \code{dotprops} representation should be plotted.
  If \code{FALSE}, only the points are plotted.}

  \item{...}{extra arguments to pass to
  \code{\link[rgl]{plot3d}}.}
}
\value{
\code{showSimilarity} is called for the side effect of
drawing the plot; a vector of object IDs is returned.
}
\description{
Display two neurons with segments coloured by similarity
}

