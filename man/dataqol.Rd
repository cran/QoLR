\name{dataqol}
\alias{dataqol}
\docType{data}
\title{
Longitudinal health-related quality of life data
}
\description{
A data frame with the answers to the 30 items of the EORTC QLQ-C30 questionnaire for 40 patients allocated in two treatment arms with some repeated measures per patient. The dataset is in long format.
}
\usage{data("dataqol")}
\format{

  \describe{
    \item{\code{Id}}{subject identification number}
    \item{\code{Arm}}{treatment arm}
    \item{\code{time}}{visit number for quality of life assessment}
    \item{\code{date}}{date of quality of life assessment}
    \item{\code{q1}}{item 1}
    \item{\code{q2}}{item 2}
    \item{\code{q3}}{item} 3
    \item{\code{q4}}{item 4}
    \item{\code{q5}}{item 5}
    \item{\code{q6}}{item 6}
    \item{\code{q7}}{item 7}
    \item{\code{q8}}{item 8}
    \item{\code{q9}}{item 9}
    \item{\code{q10}}{item 10}
    \item{\code{q11}}{item 11}
    \item{\code{q12}}{item 12}
    \item{\code{q13}}{item 13}
    \item{\code{q14}}{item 14}
    \item{\code{q15}}{item 5}
    \item{\code{q16}}{item 16}
    \item{\code{q17}}{item 17}
    \item{\code{q18}}{item 18}
    \item{\code{q19}}{item 19}
    \item{\code{q20}}{item 20}
    \item{\code{q21}}{item 21}
    \item{\code{q22}}{item 22}
    \item{\code{q23}}{item 23}
    \item{\code{q24}}{item 24}
    \item{\code{q25}}{item 25}
    \item{\code{q26}}{item 26}
    \item{\code{q27}}{item 27}
    \item{\code{q28}}{item 28}
    \item{\code{q29}}{item 29}
    \item{\code{q30}}{item 30}
    \item{\code{death}}{date of death for patients who died during the study}
  }
}

\seealso{
\code{\link{scoring.QLQC30}}
}

