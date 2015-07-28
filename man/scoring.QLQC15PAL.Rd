\name{scoring.QLQC15PAL}
\alias{scoring.QLQC15PAL}

\title{
Scoring of the health-related quality of life questionnaire EORTC QLQ-C15-PAL for cancer patients in palliative care
}
\description{
A program that computes the scores of the core questionnaire QLQ-C30 according to the EORTC scoring manual.
}
\usage{
scoring.QLQC15PAL(X, id = "", time="")
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-C15-PAL in columns. Missing values are inserted as NA.} 
  \item{id}{name of the variable in the dataframe \code{X} corresponding to the patient identification number}
  
  \item{time}{name of the variable in the dataframe \code{X} corresponding to the time identification number}
}

\details{
A score is generated if the patient answered to at least half of the corresponding items.
The scores are generated according to the EORTC scoring guidelines. Simple imputation by the personal mean is retained. In this way, missing items are ignored if at least half of the items are filled per dimension.
}

\value{
  \item{Y}{a data frame with the \code{id} and \code{time} variables of dataframe \code{X} and the score obtained for each dimension. Each score is represented by one column of \code{Y}. The names of the scores are those proposed in the scoring manual. If \code{id} and \code{time} parameters are nt specified, then \code{Y} only contains the scores}
}

\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}


\references{

Groenvold M. et al. (2006). The development of the EORTC QLQ-C15-PAL: a shortened questionnaire for cancer patients in palliative care. European Journal of Cancer, 42(1), 55-64.


}