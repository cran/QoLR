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
#data(dataqol)
## Estimation of the scores using the 'scoring.QLQC30' function:
#score_dataqol=scoring.QLQC30(dataqol,id="Id",time="time")
## Merge of "score_dataqol" dataframe with the important 
## variables of the dataqol dataframe as follows:
#info=dataqol[,c("Id","time","date","death","Arm")]
#dataqol_final=merge(score_dataqol,info,by=c("Id","time"))

## Then we reorganized the obtained "dataqol_final" dataset in order that
## the "date" variable appeared in the third position:
#dataqol_final=dataqol_final[,c(1:2,18,3:17,19:20)]

## Finally, in order to apply the "TTD" function, the dataset has to be sorted by
## patient's identification number "Id" and HRQoL measures ("time"):
#dataqol_final=dataqol_final[order(dataqol_final$time),]
#dataqol_final=dataqol_final[order(dataqol_final$Id),]


## The following Figure corresponds to the TUDD of QoL score as compared to the
## baseline score with a 5-point MCID according treatment arm (arm effect).
## In this graph, we printed the number of patients still at risk at each time
## point according to treatment arm (nrisk=T). Moreover, the result of the log-rank
## test and the Hazard Ratio of arm 2 vs. arm 1 is also printed (info=T, pos.info=c(5,0.8)).
## The Hazard Ratio (arm 2 vs. arm 1) equals to 1.20 with 95% confidence interval
## (0.69-2.09) and the result of the log-rank test is p=0.523.

#tudd1=TUDD(dataqol_final, score="QL", MCID=5,ref.init="baseline",ref.def="def1")
#ttd_1=merge(tudd1,unique(dataqol_final[,c("Id","Arm")]))
#plotTTD(ttd_1$time.5.QL,ttd_1$event.5.QL,ttd_1$Arm,nrisk=T,nevent=F,
#group.names=c("Arm 1","Arm 2"), t=seq(0,10,2),info=T,pos.info=c(6,0.8),
#xlab="time (months)", ylab="probability (%)")

}

