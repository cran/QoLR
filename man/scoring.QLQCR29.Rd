\name{scoring.QLQCR29}
\alias{scoring.QLQCR29}
\title{
Scoring of the module EORTC QLQ-CR29 for colorectal cancer
}
\description{
A program that computes the scores of the module QLQ-CR29 according to the EORTC scoring manual.
}
\usage{
scoring.QLQCR29(X, id="", items = 1:29)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-CR29 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  
  \item{items}{a vector which indicates the positions of the 29 items, in the correct order. By default items are columns 1 to 29 of \code{X}}
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

Whistance RN, et al. Clinical and psychometric validation of the EORTC QLQ-CR29 questionnaire module to assess health-related quality of life in patients with colorectal cancer. European journal of cancer. 2009 Nov;45(17):3017-26.
}
