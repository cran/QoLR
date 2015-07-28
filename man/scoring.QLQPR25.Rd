\name{scoring.QLQPR25}
\alias{scoring.QLQPR25}
\title{
Scoring of the module EORTC QLQ-PR25
}
\description{
A program that computes the scores of the module QLQ-PR25 for prostate cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQPR25(X, id="", time="")
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-PR25 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{time}{name of the variable in the dataframe \code{X} corresponding to the time identification number}
}

\details{
A score is generated if the patient answered to at least half of the corresponding items.
The scores are generated according to the EORTC scoring guidelines. Simple imputation by the personal mean is retained. In this way, missing items are ignored if at least half of the items are filled per dimension.
}

\value{
  \item{Y}{a data frame with the \code{id} and \code{time} variables of dataframe \code{X} and the score obtained for each dimension. Each score is represented by one column of \code{Y}. The names of the scores are those proposed in the scoring manual. If \code{id} and \code{time} parameters are not specified, then \code{Y} only contains the scores}
}



\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}


\references{
van Andel G, et al. An international field study of the EORTC QLQ-PR25: a questionnaire for assessing the health-related quality of life of patients with prostate cancer. European journal of cancer. 2008 Nov;44(16):2418-24.
}