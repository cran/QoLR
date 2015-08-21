\name{write.TTD}
\alias{write.TTD}

\title{Write in a csv file the results of the time to deterioration analysis
}
\description{
A program that computes the time to deterioration in a quality of life score and print the results in a csv file according to treatment arm 
}
\usage{
write.TTD(X, score = "", order = 1, ref.init = "baseline", MCID, death = NA, 
group = NULL, names.group, sensitivity = TRUE, file = "")
}

\arguments{
  \item{X}{input data matrix or data frame with a quality of life score. Missing values are inserted as NA}
  \item{score}{vector with the name of the quality of life scores of interest}
  \item{order}{a vector equals to 1 if the deterioration corresponds to a decrease of the score, 2 otherwise}
  \item{ref.init}{the reference score to qualify the deterioration. 
    By default, \code{ref.init} is \code{"baseline"}, i.e. the reference score is the baseline score. 
    If \code{ref.init} is \code{"best"}, the best previous quality of life score is the reference score.
    If \code{ref.init} is \code{"previous"}, the last previous score is the reference score.}
  \item{MCID}{vector equals to the minimal clinically important difference (MCID). Several MCID can be specified}
  \item{death}{missing if patients who died without experienced a deterioration are censored at the time of the last quality of life assessment, equals to the name of the death date in the dataframe \code{X} otherwise}
  \item{group}{the name of the variable in \code{X} corresponding to the treatment arm. Only two groups are allowed}
  \item{names.group}{the name of each treatment group to print}
  \item{sensitivity}{Boolean equals to \code{TRUE} by default. If \code{sensitivity} is \code{TRUE}, then all sensitivity analyses are performed, integrating patients with no baseline or with no follow up as event, death as event and simultaneously no baseline, no follow and death}
  \item{file}{the name of the csv file to create with the results of the time to deterioration analysis}
}
\value{this function does not return value in R console but creates a csv file with the results of the time to deterioration analysis
}

\author{Amelie ANOTA

Maintainer: Amelie ANOTA <aanota@chu-besancon.fr>
}
\seealso{\code{\link{TTD}}}

\examples{
### The time to deterioration of scores "QoL" and "pain" of the dataqol2 data
### set as compared to the baseline score
### with two MCID (5 points and 10 points)
### and according to the treatment arm called "arm"
### all sensitivity analyses are performed simultaneously to the main definition
### the created file is named "file_TTD_baseline.csv" and is located 
### in the current directory                             
                                 
#data(dataqol2)
#write.TTD(dataqol2,score=c("QoL","pain"),order=c(1,2),MCID=c(5,10),
#group="arm",names.group=c("arm 1","arm 2"),sensitivity=FALSE,
#file="file_TTD_baseline") 
}
