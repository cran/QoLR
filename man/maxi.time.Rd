\name{maxi.time}
\alias{maxi.time}
\title{
Report the highest score at each measurement time point
}
\description{
A function to report the highest score at each measurement time point taking into account all previous scores
}
\usage{
maxi.time(vector)
}
\arguments{
  \item{vector}{
A vector with quality of life scores}
}

\value{
a vector which the i-th value is equals to the maximum of the first values of the given vector until to the i-th position
}

\examples{
vect=c(10,20,30,10,2,0,4,50,20)
maxi.time(vect)
}
