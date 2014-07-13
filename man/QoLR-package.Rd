\name{QoLR-package}
\alias{QoLR-package}
\alias{QoLR}
\docType{package}
\title{
Analysis of Health-Related Quality of Life in oncology}
\description{
A set of functions to generate the scores of the EORTC QLQ-C30 questionnaire and supplementary modules.
Two other programs to determine the time to deterioration in a Quality of Life score in longitudinal analysis 
with different definitions of deterioration explored.}
\details{
\tabular{ll}{
Package: \tab QoLR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-13\cr
License: \tab GPL (>=2.0)\cr
}
A set of functions to generate the scores of the EORTC QLQ-C30 questionnaire, for example function 'scoring.QLQC30', and supplementary modules. Function 'TTD' to determine the time to deterioration in a Quality of Life score in longitudinal analysis and function 'TUDD' to determine the time until definitive deterioration
}
\author{
Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>

}
\references{
Anota A. et al. Time to Health-related Quality of Life score deterioration as a modality of longitudinal analysis for health-related quality of life studies in oncology: do we need RECIST for quality of life to achieve standardization? Qual Life Res. 2013 Nov 26.

Bonnetain F. et al. Time until definitive deterioration as a means of longitudinal analysis for treatment trials in patients with metastatic pancreatic adenocarcinoma. Eur J Cancer 2010, 46(5): 2753-2762.

Fayers PM. et al. The EORTC QLQC30 scoring manual. 3rd ed. Brussels: EORTC, 2001.

Hamidou Z. et al. Time to deterioration in quality of life score as a modality of longitudinal analysis in patients with breast cancer. The Oncologist 2011, 16(10):1458-1468.


}

\examples{
# To generate the scores of the EORTC QLQ-C30 for the data frame dataqol1:
data(dataqol1)
scoring.QLQC30(dataqol1, items = 2:31)


# To determine the time to deterioration of 5 points at least as compared to 
# the baseline score for the score "QoL" and the score "pain".
# For score "QoL", a deterioration is observed if the score decreases, 
# thus, order equals to 1 for this score.
# For score "pain", a deterioration is observed if the score increases, 
# thus, order equals to 2 for this score.
data(dataqol2)
ttd=TTD(dataqol2,score=c("QoL","pain"),order=c(1,2),MCID=5)
head(ttd)

# To determine the time until definitive deterioration of 5 points at least 
# of the score "QoL" as compared to the baseline score with no further 
# improvement of more than 5 points:
data(dataqol2)
ttd=TUDD(dataqol2,score="QoL",order=1,MCID=5)
head(ttd)
}