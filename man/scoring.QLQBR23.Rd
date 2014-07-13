\name{scoring.QLQBR23}
\alias{scoring.QLQBR23}

\title{
Scoring of the module EORTC QLQ-BR23
}
\description{
A program that computes the scores of the module QLQ-BR23 specific to breast cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQBR23(X, id="", items = 1:23)
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-BR23 in columns. Missing values are inserted as NA.}
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  \item{items}{a vector which indicates the positions of the 23 items, in the correct order. By default items columns 1 to 23 of \code{X}}
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
Sprangers MA, et al. (1996). The European Organization for Research and Treatment of Cancer breast cancer-specific quality-of-life questionnaire module: first results from a three-country field study. J Clin Oncol 14:2756-68.
}