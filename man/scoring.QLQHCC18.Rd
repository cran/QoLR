\name{scoring.QLQHCC18}
\alias{scoring.QLQHCC18}
\title{
Scoring of the module EORTC QLQ-HCC18
}
\description{
A program that computes the scores of the module QLQ-HCC18 for hepatocellular cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQHCC18(X, id = "", time="")
}
\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-HCC18 in columns. Missing values are inserted as NA.}
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

Chie W.C. et al. (2012). International cross-cultural field validation of an European Organization for Research and Treatment of Cancer questionnaire module for patients with primary liver cancer, the European Organization for Research and Treatment of Cancer quality-of-life questionnaire HCC18. Hepatology, 55(4), 1122-1129.


}
