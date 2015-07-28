\name{scoring.QLQLMC21}
\alias{scoring.QLQLMC21}
\title{
Scoring of the module EORTC QLQ-LMC21
}
\description{
A program that computes the scores of the module QLQ-LMC21 for Liver Metastases in Colorectal Cancer according to the EORTC scoring manual.
}
\usage{
scoring.QLQLMC21(X, id = "", time="")
}
\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-LMC21 in columns. Missing values are inserted as NA.}
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

Blazeby, J.M. et al. (2009). Validation of the European Organization for Research and Treatment of Cancer QLQ-LMC21 questionnaire for assessment of patient-reported outcomes during treatment of colorectal liver metastases. British Journal of Surgery, 96(3), 291-298.


}
