% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotMSE.R
\name{PlotBoxplot}
\alias{PlotBoxplot}
\title{Plots a box plot}
\usage{
PlotBoxplot(dt, start_year, end_year, wrangle_function, y_value_label,
  x_value_label, plot_title = NULL, func_group_name, fleet_group_name,
  true_year_beginning, save_plot_folder, plot_height_inches, plot_width_inches,
  units, ...)
}
\arguments{
\item{dt}{The data table contain the data}

\item{start_year}{The year from which to select data where the years run from 1 to the number of years of the simulation}

\item{end_year}{The year upto which to select data where the years run from 1 to the number of years of the simulation#' @param end_year}

\item{wrangle_function}{Set to the type of data manipulation you want depending on the plot you are creating}

\item{y_value_label}{The y-axis label for the plot}

\item{x_value_label}{The x-axis label for the plot}

\item{plot_title}{Optional The plot title}

\item{func_group_name}{If required, the functional group name to be plotted}

\item{fleet_group_name}{If required, the fleet group name to be plotted}

\item{true_year_beginning}{The actual date year that is the start of the simulation}

\item{save_plot_folder}{The path to the plots folder which will contain folders for each plot type and inside them the plots created using this function}

\item{plot_height_inches}{The number of inches high for the produced plot}

\item{plot_width_inches}{The number of inches wide for the produced plot}

\item{units}{The units of the y-axis}

\item{...}{If you are using WrangleBoxplotProbability this will be bref}
}
\description{
Plots a box plot
}
