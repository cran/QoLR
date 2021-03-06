\name{TTD}
\alias{TTD}
\title{Time to Quality of Life score deterioration}

\description{
A program that computes the time to deterioration in a quality of life score.}
\usage{
TTD(X, score = "", MCID, ref.init = "baseline", order = 1, 
no_baseline = "censored", no_follow = "censored", death = NA, sensitivity = FALSE)
}

\arguments{
  \item{X}{input data matrix or data frame with at least one quality of life score. Missing values are inserted as NA.}
  \item{score}{vector with the name of the quality of life scores of interest}
  \item{MCID}{the minimal clinically important difference}
  \item{ref.init}{the reference score to qualify the deterioration. 
  By default, \code{ref.init} is \code{"baseline"}, i.e. the reference score is the baseline score. 
  If \code{ref.init} is \code{"best"}, the best previous quality of life score is the reference score.
  If \code{ref.init} is \code{"previous"}, the immediately preceding score is the reference score.}
  \item{order}{a vector equals to 1 if the deterioration corresponds to a decrease of the score, 2 otherwise}
  \item{no_baseline}{By default, \code{no_baseline} equals to \code{"censored"} to indicate that patients with no baseline score are censored at baseline (Day 0). If \code{no_baseline} equals \code{"event"}, these patients are deteriorated since baseline.  If \code{no_baseline} equals \code{"excluded"}, these patients are excluded from the analysis}
  \item{no_follow}{By default, \code{no_follow} equals to \code{"censored"} to indicate that patients with no follow-up score are censored just after baseline (Day 1). If \code{no_follow} equal to \code{"event"}, these patients are deteriorated just after baseline}
  \item{death}{missing if patients who died without experienced a deterioration are censored at the time of the last quality of life assessment, equals to the name of the death date in the dataframe \code{X} otherwise}
  \item{sensitivity}{Boolean equal to \code{FALSE} by default. If \code{sensitivity} is \code{TRUE}, then all sensitivity analyses are performed, integrating patients with no baseline or with no follow up as event (SA1), death as event (SA2) and simultaneously no baseline, no follow and death (SA3)}
}
\details{To apply this function, the dataset must respect a general structure. 
The dataset \code{X} must be in long format with the following variables in the following order: 
\enumerate{
\item{Patient identification number} 
\item {Variable identify the number of the quality of life assessment, i.e. the visit number} 
\item {Date of quality of life measures}
\item {quality of life scores} 
\item {Other variables such as the date of death or the treatment arm.}
}
The dataset must also be sorted by patient identification number and quality of life measurement time. 
Dates must be in Julian format (i.e. number of days since a reference time point).

All these definitions are extensively described in the referenced papers below.
}
\value{
The result is a dataframe with the \code{id} variable of the dataframe \code{X} and the results of the time to deterioration analyses performed. 

For each score and each time to deterioration analysis, two variables are created called \code{event} and \code{time} with the name of the corresponding score as a suffix. 

Moreover, if \code{sensitivity} is \code{TRUE}, a suffix is added to each result of this function reflecting the sensitivity analysis corresponding (SA1, SA2 or SA3).

The first variable \code{event} is a dummy vector equal to 1 if the patient is deteriorated and 0 if not.
The second variable \code{time} equal to the time in months to deterioration since baseline date if the patient is deteriorated or the time to censoring.

As example, for a given score "qol" and one analyse performed (i.e. \code{sensitivity} is \code{FALSE}), then two variables are created called \code{event.qol} and \code{time.qol}. 
}

\references{

Anota A., et al. Time to Health-related Quality of Life score deterioration as a modality of longitudinal analysis for health-related quality of life studies in oncology: do we need RECIST for quality of life to achieve standardization? Qual Life Res. 2013 Nov 26.

Hamidou Z., et al. Time to deterioration in quality of life score as a modality of longitudinal analysis in patients with breast cancer. The Oncologist 2011, 16(10):1458-1468.
}


\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}

\seealso{
\code{\link{TUDD}}
}

\examples{
data(dataqol2)
# deterioration of 5 points at least as compared to the baseline score for
# the score "QoL" and the score "pain"
# order = 1 for "QoL" score because a deterioration is observed when the score decreases
# order = 2 for pain score bacause a deterioration is observed when the score increases
ttd1=TTD(dataqol2,score=c("QoL","pain"),order=1:2,MCID=5)
head(ttd1)

}

