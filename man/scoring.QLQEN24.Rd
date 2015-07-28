\name{scoring.QLQEN24}
\alias{scoring.QLQEN24}

\title{
Scoring of the module EORTC QLQ-EN24 for endometrial cancer
}
\description{
A program that computes the scores of the module QLQ-EN24 according to the EORTC scoring manual.
}
\usage{
scoring.QLQEN24(X, id="", time="")
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-EN24 in columns. Missing values are inserted as NA.}
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
Greimel E., et al. (2011). Psychometric validation of the European organisation for research and treatment of cancer quality of life questionnaire-endometrial cancer module (EORTC QLQ-EN24). European Journal of Cancer, 47(2), 183-190.
}