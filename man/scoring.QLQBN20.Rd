\name{scoring.QLQBN20}
\alias{scoring.QLQBN20}

\title{
Scoring of the module EORTC QLQ-BN20 for brain cancer
}
\description{
A program that computes the scores of the module QLQ-BN20 specific to brain cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQBN20(X, id="", items = 1:20)
}
\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-BN20 in columns. Missing values are inserted as \code{NA}.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 20 items, in the correct order. By default items are column 1 to 20 of \code{X}}
}
\details{
A score is generated if the patient answered to at least half of the corresponding items.
The scores are generated according to the EORTC scoring guidelines. Simple imputation by the personal mean is retained. In this way, missing items are ignored.
}

\value{
  \item{Y}{a data frame with the \code{id} variable and the score obtained for each dimension. Each score is represented by one column of \code{Y}. The names of the scores are those proposed in the scoring manual. If there is no \code{id} variable in the dataframe \code{X}, then \code{Y} only contains the scores}
}


\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}

\references{
Taphoorn, M.J., et al. (2010). An international validation study of the EORTC brain cancer module (EORTC QLQ-BN20) for assessing health-related quality of life and symptoms in brain cancer patients. European Journal of Cancer, 46(6), 1033-1040.
}