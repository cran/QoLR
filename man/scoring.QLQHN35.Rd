\name{scoring.QLQHN35}
\alias{scoring.QLQHN35}

\title{
Scoring of the module EORTC QLQ-H&N35
}
\description{
A program that computes the scores of the module QLQ-H&N35 for head and neck cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQHN35(X, id="", items = 1:35)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-H&N35 in columns. Missing values are inserted as NA.}
 \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 35 items, in the correct order. By default items columns 1 to 35 of \code{X}}
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
Bjordal, K., et al.(1999). Quality of life in head and neck cancer patients: validation of the European Organization for Research and Treatment of Cancer Quality of Life Questionnaire-H&N35. Journal of Clinical Oncology, 17(3), 1008-1008
}