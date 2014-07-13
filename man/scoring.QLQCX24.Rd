\name{scoring.QLQCX24}
\alias{scoring.QLQCX24}

\title{
Scoring of the module EORTC QLQ-CX24 for cervical cancer
}
\description{
A program that computes the scores of the module QLQ-CX24 according to the EORTC scoring manual.
}
\usage{
scoring.QLQCX24(X, id="", items = 1:24)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-CX24 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 24 items, in the correct order. By default items are columns 1 to 24 of \code{X}}
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
Greimel ER, et al. (2006). The European Organization for Research and Treatment of Cancer (EORTC) Quality-of-Life questionnaire cervical cancer module. Cancer, 107(8), 1812-1822.
}