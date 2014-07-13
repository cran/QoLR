\name{scoring.QLQSTO22}
\alias{scoring.QLQSTO22}

\title{
Scoring of the module EORTC QLQ-STO22
}
\description{
A program that computes the scores of the module QLQ-STO22 for gastric cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQSTO22(X, id="", items = 1:22)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-STO22 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 22 items, in the correct order. By default items columns 1 to 22 of \code{X}}
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
Blazeby, J.M., et al. (2004). Clinical and psychometric validation of a questionnaire module, the EORTC QLQ-STO 22, to assess quality of life in patients with gastric cancer. European Journal of Cancer, 40(15), 2260-2268.
}