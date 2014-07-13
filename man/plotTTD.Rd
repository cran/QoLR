\name{plotTTD}
\alias{plotTTD}

\title{Plot the Kaplan-Meier curve of the TTD or TUDD
}
\description{A program that plot the time to deterioration curves according to the Kaplan-Meier estimation method 
for all patients or according to treatment arm. Additional information can be added such as the number of patients at risk and the number of the cumulative events
}
\usage{
plotTTD(time, event, group = NULL, nrisk = FALSE, nevent = FALSE, group.names = NULL,
 t = NULL, info = FALSE, pos.info = NULL, xlab, ylab)
}

\arguments{
 \item{time}{vector equals to the time to deterioration or the time to censure
}
  \item{event}{a dummy vector equals to 1 if the patient is deteriorated and 0 if not
}
  \item{group}{the name of the variable corresponding to the treatment arm, only if you want survival curves according to treatment arm. Only two groups are allowed
}
  \item{nrisk}{Boolean equals to \code{FALSE} by default. If \code{nrisk} is \code{TRUE}, then the number of patients at risk is printed under the curve at each \code{t} time point.

}
  \item{nevent}{Boolean equals to \code{FALSE} by default. If \code{event} is \code{TRUE}, then the number of cumulative events is printed under the curve at each \code{t} time point. In that case, you must also fix \code{nrisk} to \code{TRUE}

}
  \item{group.names}{if you want survival curves according to treatment arm, you must give the name of the treatment arms in the \code{group.names} vector
}
  \item{t}{if \code{nrisk} is \code{TRUE}, you must give the time points to print the number of patients at risk in vector {t}
}
  \item{info}{Boolean equals to \code{FALSE} by default. If two groups are given in the group vector,
then the result of the Log-rank test and the Hazard ratio are added to the graph if \code{info} is \code{TRUE} 
}
  \item{pos.info}{the position of the Log-rank test and the Hazard ratio on the graph
}
  \item{xlab}{a title for x axis
}
  \item{ylab}{a title for y axis
}
}

\author{Amelie Anota

Maintainer: Amelie Anota <aanota@chu-besancon.fr>
}


\examples{
# Kaplan-Meier survival curve of the Time until definitive deterioration
# of the score "QoL" with a minimal clinically important difference of 5 points
# as compared to the baseline score
#tudd1=TUDD(dataqol2, score="QoL", MCID=5,ref.init="baseline",ref.def="def1")
#ttd_1=merge(tudd1,unique(dataqol2[,c("id","arm")]))
## In the next graph, we added the number of patients at risk at time t
## and the result of the Log Rank Test and the Univariate Hazard Ratio
## of arm 2 vs. arm 1
#plotTTD(ttd_1$time.5.QoL,ttd_1$event.5.QoL,ttd_1$arm,nrisk=T,nevent=F,
#group.names=c("arm 1","arm 2"),t=seq(0,8,2),info=T,pos.info=c(6,0.8),
#xlab="time (months)",ylab="probability (%)")
}

