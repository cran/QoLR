\name{TUDD}
\alias{TUDD}

\title{Time until definitive deterioration in a quality of life score}
\description{
A program that computes the time until definitive deterioration in quality of score.}
\usage{
TUDD(X, score = "", MCID, ref.init = "baseline", ref.def = "def1", order = 1,
 no_baseline = "censored", no_follow = "censored", death = NA, sensitivity = FALSE)
}

\arguments{
 \item{X}{input data matrix or data frame with a quality of life score. Missing values are inserted as NA.}
  \item{score}{vector with the name of the quality of life scores of interest}
  \item{MCID}{a vector equals to the minimal clinically important difference (MCID). Several MCID can be specified}
  \item{ref.init}{the reference score to qualify the deterioration. 
    By default, \code{ref.init} is \code{"baseline"}, i.e. the reference score is the baseline score. 
    If \code{ref.init} is \code{"best"}, the best previous quality of life score is the reference score.
    If \code{ref.init} is \code{"previous"}, the immediately preceding score is the reference score.}
 \item{ref.def}{the deterioration is definitive 1: if there is no clinically significant improvement as compared to the reference score (\code{"def1"}); 2: if the deterioration is also observed at all times following the deterioration (\code{"def2"}); 3: or there is no clinically significant improvement as compared to the score qualifying the deterioration (\code{"def3"})}
 \item{order}{a vector equals to 1 if the deterioration corresponds to a decrease of the score, 2 otherwise}
   \item{no_baseline}{By default, \code{no_baseline} equals to \code{"censored"} to indicate that patients with no baseline score are censored at baseline (Day 0). If \code{no_baseline} equals \code{"event"}, these patients are deteriorated since baseline. If \code{no_baseline} equals \code{"excluded"}, these patients are excluded from the analysis}
    \item{no_follow}{By default, \code{no_follow} equals to \code{"censored"} to indicate that patients with no follow-up score are censored just after baseline (Day 1). If \code{no_follow} equal to \code{"event"}, these patients are deteriorated just after baseline}
  \item{death}{missing if patients who died without experienced a deterioration are censored at the time of the last quality of life assessment, equals to the name of the death date in the dataframe \code{X} otherwise}
  \item{sensitivity}{Boolean equal to \code{FALSE} by default. If \code{sensitivity} is \code{TRUE}, then all sensitivity analyses are performed, integrating patients with no baseline or with no follow up as event (SA1), death as event (SA2) and simultaneously no baseline, no follow and death (SA3)}
}
  


\details{ To apply this function, the dataset must respect a general structure. 
The dataset \code{X} must be in long format with the following variables in the following order: 
\enumerate{
\item{Patient identification number} 
\item {Variable identify the quality of life assessment, i.e. the visit number} 
\item {Date of quality of life measure}
\item {quality of life scores} 
\item {Other variables such as the date of death or the treatment arm.}
}
The dataset must also be sorted by patient identification number and quality of life measurement time. 

Dates must be in Julian format (i.e. number of days since a reference time point).

All these definitions are extensively described in the referenced papers below.
}
\value{
The result is a dataframe with the \code{id} variable of the dataframe \code{X} and the results of the time to deterioration analyses performed. 

For each score and each time to deterioration analysis, two variables are created called \code{event} and \code{time} with the value of the \code{MCID} and the name of the corresponding score as a suffix.

Moreover, if \code{sensitivity} is \code{TRUE}, a suffix is added to each result of this function reflecting the sensitivity analysis corresponding (SA1, SA2 or SA3).

The first variable \code{event} is a dummy vector equal to 1 if the patient is deteriorated and 0 if not. 
The second variable \code{time} equal to the time in months to deterioration since baseline if the patient is deteriorated or the time to censoring.

As example, for a given score "qol", \code{MCID = 5} and one analyse performed (i.e. \code{sensitivity} is \code{FALSE}), then two variables are created called \code{event.5.qol} and \code{time.5.qol}. 
}


\references{

Anota A., et al. Time to Health-related Quality of Life score deterioration as a modality of longitudinal analysis for health-related quality of life studies in oncology: do we need RECIST for quality of life to achieve standardization? Qual Life Res. 2013 Nov 26.

Bonnetain F., et al. Time until definitive deterioration as a means of longitudinal analysis for treatment trials in patients with metastatic pancreatic adenocarcinoma. Eur J Cancer 2010, 46(5): 2753-2762.
}

\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}

\seealso{
\code{\link{TTD}}
}


\examples{
data(dataqol2)
# Time to definitive deterioration of  at least 5 points of the "QoL" score
# as compared to the best previous score with no further improvement of more 
# than 5 points as compared to the best previous score: 
tudd=TUDD(dataqol2,score=c("QoL","pain"),ref.init="best",order=1,MCID=5)
head(tudd)
}