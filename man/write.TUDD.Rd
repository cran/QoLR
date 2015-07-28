\name{write.TUDD}
\alias{write.TUDD}

\title{Write in a csv file the results of the time until definitive deterioration analysis
}
\description{
A program that computes the time until definitive deterioration in a quality of life score and print the results in a csv file according treatment arm 
}
\usage{
write.TUDD(X, score = "", order = 1, ref.init = "baseline", MCID, ref.def = "def1",
death = NA, group = NULL, names.group, sensitivity = TRUE, file = "")
}

\arguments{
 \item{X}{input data matrix or data frame with a quality of life score. Missing values are inserted as NA}
  \item{score}{vector with the name of the quality of life scores of interest}
  \item{order}{a vector equals to 1 if the deterioration corresponds to a decrease of the score, 2 otherwise}
  \item{ref.init}{the reference score to qualify the deterioration. 
      By default, \code{ref.init} is \code{"baseline"}, i.e. the reference score is the baseline score. 
      If \code{ref.init} is \code{"best"}, the best previous quality of life score is the reference score.
      If \code{ref.init} is \code{"previous"}, the last previous score is the reference score.}
  \item{MCID}{vector equals to the minimal clinically important difference (MCID). Several MCIDs can be specified}
  \item{ref.def}{the deterioration is definitive 1: if there is no clinically significant improvement as compared to the reference score (\code{"def1"}); 2: if the deterioration is also observed at all times following the deterioration (\code{"def1"}); 3: or there is no clinically significant improvement as compared to the score qualifying the deterioration (\code{"def3"})}
   \item{death}{missing if patients who died without experienced a deterioration are censored at the time of the last QoL score, equals to the name of the death date in the dataframe \code{X} otherwise}
  \item{group}{the name of the variable in \code{X} corresponding to the treatment arm. Only two groups are allowed.}
  \item{names.group}{the name of each treatment group to print}
  \item{sensitivity}{Boolean equals to \code{TRUE} by default. If \code{sensitivity} is \code{TRUE}, then all sensitivity analyses are performed, integrating patients with no baseline or with no follow up as event, death as event and simultaneously no baseline, no follow and death}
  \item{file}{the name of the csv file to create with the results of the time to deterioration analysis}
}
\value{this function does not return value in R console but create a csv file with the results of the time to deterioration analysis}
\author{Amelie ANOTA

Maintainer: Amelie ANOTA <aanota@chu-besancon.fr>
}

\seealso{
\code{\link{TUDD}}
}


