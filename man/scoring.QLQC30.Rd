\name{scoring.QLQC30}
\alias{scoring.QLQC30}

\title{
Scoring of the health-related quality of life questionnaire EORTC QLQ-C30 for cancer
}
\description{
A program that computes the scores of the core questionnaire QLQ-C30 according to the EORTC scoring manual.
}
\usage{
scoring.QLQC30(X, id="", time="")
}

\arguments{
  \item{X}{input data matrix or data frame with items of the EORTC QLQ-C30 in columns. Missing values are inserted as NA.} 
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

Aaronson N.K., et al. (1993). The European Organization for Research and Treatment of Cancer QLQ-C30: A quality-of-life instrument for use in international clinical trials in oncology. Journal of the National Cancer Institute, 85(5), 365-376.

Fayers PM. et al. The EORTC QLQC30 scoring manual. 3rd ed. Brussels: EORTC, 2001.

}


\examples{
# scoring of the data frame dataqol1:
data(dataqol)
scoring.QLQC30(dataqol, id="Id", time="time")
}
