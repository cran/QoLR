\name{scoring.QLQEN24}
\alias{scoring.QLQEN24}

\title{
Scoring of the module EORTC QLQ-EN24 for endometrial cancer
}
\description{
A program that computes the scores of the module QLQ-EN24 according to the EORTC scoring manual.
}
\usage{
scoring.QLQEN24(X, id="", items = 1:24)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-EN24 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 24 items, in the correct order. By default items columns 1 to 24 of \code{X}}
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
Greimel E., et al. (2011). Psychometric validation of the European organisation for research and treatment of cancer quality of life questionnaire-endometrial cancer module (EORTC QLQ-EN24). European Journal of Cancer, 47(2), 183-190.
}