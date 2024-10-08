% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oat_analysis.R, R/forest_plot.R,
%   R/sensitivity_analysis.R
\name{createScenarios}
\alias{createScenarios}
\alias{createScenarios,forest_plot-method}
\alias{createScenarios,sensitivity_analysis-method}
\title{Create scenarios.}
\usage{
createScenarios(object, ...)

\S4method{createScenarios}{forest_plot}(object, dataset, ...)

\S4method{createScenarios}{sensitivity_analysis}(object, model, ...)
}
\arguments{
\item{object}{OAT analysis, type forest plot or sensitivity analysis}

\item{...}{extra arguments}

\item{dataset}{Campsis dataset}

\item{model}{Campsis model}
}
\value{
a list of CAMPSIS scenarios to be run
}
\description{
Create scenarios.
}
