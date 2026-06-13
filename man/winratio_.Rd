% Please edit documentation in R/winratio_.R
\name{winratio_}
\alias{winratio_}
\title{Creating Objects of Hierarchical Log Win-Ratio Statistic}
\usage{
winratio_(formula, endpoints, data_index = 1)
}
\arguments{
\item{formula}{a two-sided R formula \code{<label> ~ arm}. The LHS is used
purely as a row label in \code{pated()} output; the RHS must contain exactly
one variable, the column in \code{data} holding treatment-arm assignment.}

\item{endpoints}{a non-empty list of endpoint specs built by \code{nb_tte()},
\code{nb_continuous()}, or \code{nb_binary()}. The order of the list encodes
the hierarchical priority: endpoint 1 is checked first; if it ties, the
pair falls through to endpoint 2; and so on.}

\item{data_index}{integer. Index of the data frame in the \code{data}
argument of \code{jointCovariance} to be used.}
}
\value{
An object of class \code{c("jc_spec_winratio", "jc_spec")}.
}
\description{
\code{winratio_} creates an object to be passed into \code{jointCovariance}
or \code{pated} through its \code{...} argument. The object defines a
hierarchical win-ratio statistic across a list of endpoints, each declared
by \code{nb_tte()}, \code{nb_continuous()}, or \code{nb_binary()}.
}
\details{
The fitted coefficient is on the log scale:
\deqn{\widehat\theta = \log(\widehat{WR})
      = \log(\widehat\pi_W / \widehat\pi_L)}
where \eqn{\widehat\pi_W} and \eqn{\widehat\pi_L} are the proportions of all
control-treatment pairs where the treatment subject wins or loses,
respectively. Tied pairs remain in the denominator of both proportions but
do not contribute to either numerator.

The raw win ratio can be recovered by exponentiating the coefficient. The
log scale is used because it gives the standard large-sample Wald
representation and maps the null win ratio of 1 to 0.

The arm reference level is inferred from the arm column the same way
\code{model.matrix(~ arm)} would: \code{levels(arm)[1]} for factor, the
smaller value for numeric or logical, and the alphabetically first value for
character. To override, convert the column to a factor with the desired level
order before calling \code{winratio_()}.
}
\seealso{
\code{\link[=nb_tte]{nb_tte()}}, \code{\link[=nb_continuous]{nb_continuous()}},
\code{\link[=nb_binary]{nb_binary()}}, \code{\link[=jointCovariance]{jointCovariance()}},
\code{\link[=pated]{pated()}}.
}
