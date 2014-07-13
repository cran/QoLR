\name{scoring.QLQLC13}
\alias{scoring.QLQLC13}

\title{
Scoring of the module EORTC QLQ-LC13
}
\description{
A program that computes the scores of the module QLQ-LC13 for lung cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQLC13(X, id="", items = 1:13)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-LC13 in columns. Missing values are inserted as NA.}
 \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 13 items, in the correct order. By default items columns 1 to 13 of \code{X}}
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
Bergman B, et al. (1994). The EORTC QLQ-LC13: a modular supplement to the EORTC Core Quality of Life Questionnaire (QLQ-C30) for use in lung cancer clinical trials. EORTC Study Group on Quality of Life. Eur J Cancer 30A:635-42.
}