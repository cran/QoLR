\name{scoring.QLQCX24}
\alias{scoring.QLQCX24}

\title{
Scoring of the module EORTC QLQ-CX24 for cervical cancer
}
\description{
A program that computes the scores of the module QLQ-CX24 according to the EORTC scoring manual.
}
\usage{
scoring.QLQCX24(X, id="", time="")
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-CX24 in columns. Missing values are inserted as NA.}
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
Greimel ER, et al. (2006). The European Organization for Research and Treatment of Cancer (EORTC) Quality-of-Life questionnaire cervical cancer module. Cancer, 107(8), 1812-1822.
}