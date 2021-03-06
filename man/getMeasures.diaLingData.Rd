% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures.R
\name{getMeasures.diaLingData}
\alias{getMeasures.diaLingData}
\title{Get linguistic distance measures}
\usage{
\method{getMeasures}{diaLingData}(set, ...)
}
\arguments{
\item{set}{object of class diaGeoDistance}

\item{...}{arguments passing to other methods}

\item{measure}{linguistic distance or similarity index to be used}

\item{binaryIndex}{binary index to be used for multiple answers}
}
\description{
Get linguistic distance measures
}
\examples{

data(bourciezDiaData)
set <- bourciezDiaData$lingdata

result <- getMeasures(
  set, formula = id ~ question, value.var = 'answer', measure = 'iri', binaryIndex = 'dice')

}

