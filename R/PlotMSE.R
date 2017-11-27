#library(ggplot2)
#library(stringr)
#library(data.table)
#source("CreateListStrings.R")
#source("ConvertFleetName2Number.R")
#source("https://raw.githubusercontent.com/MarkPlatts/MSE_Plugin_Results_Plotting/master/share_tools.R")

#' Title Plot MSE Plugin Results
#'
#' This function contains a complete set of functionality to plot the formal plot types required for an Management Strategy Evaluation Analysis
#'
#' @param func_group_name If required, the functional group name to be plotted
#' @param fleet_group_name If required, the fleet group name to be plotted
#' @param true_year_beginning The actual date year that is the start of the simulation
#' @param plot_width_inches The number of inches wide for the produced plot
#' @param plot_height_inches The number of inches high for the produced plot
#' @param y_value_label The y-axis label for the plot
#' @param x_value_label The x-axis label for the plot
#' @param plot_title Optional The plot title
#' @param units The units of the y-axis
#' @param n_label_cols The number of columns that label the data before the actual numeric values, contained in the csv being plotted
#' @param yearly_in_filename Logical value representing whether the csv file to plot contains in the title the word yearly
#' @param results_folder_path The path to the folder that contains the csv file being plotted
#' @param save_plot_folder The path to the plots folder which will contain folders for each plot type and inside them the plots created using this function
#' @param fleet_lookup_file Contains the path and filename for the fleet lookup file.
#' The first row contains headers for:
#' Column1 = Fleet name, Column2 = Fleet Number
#' This is used when the file names don't contain the fleet name but FleetNoX. It converts from the fleet name to FleetX
#' @param total_area The total area covered by the model
#' @param scale_unit Used to scale the units, so if you want to convert from tons to kilo tons set to 1000
#' @param wrangle_function Set to the type of data manipulation you want depending on the plot you are creating
#' WrangleBoxplotMean
#' WrangleBoxplotProbability
#' WrangleBoxplotAAV
#' WrangleTimeseriesAnnualChange
#' @param plot_type_fun the choice of two types of plots, PlotTimeseries or PlotBoxplot
#' @param ... If you are using WrangleBoxplotProbability set the argument bref
#' @export
#' @import data.table
#' @examples
#' PlotMSE(func_group_name = "AdCod",
#' fleet_group_name = "AllFleets",
#' start_year = 29, end_year = 29,
#' true_year_beginning = 1990,
#' plot_width_inches = 6, plot_height_inches = 4,
#' y_value_label = "AAV Catch", x_value_label = "Strategy",
#' units = "%",
#' n_label_cols = 5,
#' yearly_in_filename = TRUE,
#' results_folder_path = "Results-and-plots-2017-09-08/Results/CatchTrajectories/",
#' save_plot_folder = "Plots/",
#' wrangle_function = WrangleBoxplotAAV,
#' plot_type_fun = PlotBoxplot
#' )
PlotMSE <- function(func_group_name = NULL, fleet_group_name = NULL,
                    plot_width_inches, plot_height_inches,
                    y_value_label, x_value_label, plot_title = NULL,
                    units = "", n_label_cols, yearly_in_filename, results_folder_path,
                    save_plot_folder, fleet_lookup_file = NULL, total_area = NULL,
                    scale_unit = 1, wrangle_function = NULL, plot_type_fun, ...){

  save_plot_folder <- paste0(save_plot_folder, y_value_label, "/")

  if(!file.exists(save_plot_folder)) dir.create(save_plot_folder)

  if(!is.null(fleet_lookup_file)){
    fleet_group_number <- LookupInFile(file = fleet_lookup_file, x = fleet_group_name,
                                       header = TRUE)
  } else {
    fleet_group_number <- NULL
  }

  # Load data
  dt <- NULL
  if(!is.null(func_group_name)){
    for(ifunc_groups in func_group_name){
      if (yearly_in_filename) {
        strings_in_filename <- c("Yearly", ifunc_groups, fleet_group_number)
      } else {
        strings_in_filename <- c(ifunc_groups, fleet_group_number)
      }
      dt_temp <- LoadFile_ContainsListStrings(Dir.Path = results_folder_path,
                                              StringsInFileName = strings_in_filename,
                                              Read.Function = fread)
      dt <- rbind(dt, dt_temp)
    }
  } else { # do this only if its fleet only
    if (yearly_in_filename) {
      strings_in_filename <- c("Yearly", fleet_group_number)
    } else {
      strings_in_filename <- c(fleet_group_number)
    }

    dt <- LoadFile_ContainsListStrings(Dir.Path = results_folder_path,
                                       StringsInFileName = strings_in_filename,
                                       Read.Function = fread)
  }

  dt <- melt(dt, id.vars = 1:n_label_cols, variable.name = "TimeStep", value.name = "y_val")
  dt <- dt[, .(y_val = sum(y_val)), by = .(ModelID, StrategyName, TimeStep)]

  dt <- dt[, TimeStep := as.numeric(TimeStep)]

  if(!is.null(total_area)){
    area <- get_area(plot.path = results_folder_path, file.name = igroup, Area = total_area)
  } else {
    area = 1
  }
  dt[, y_val := y_val * area/scale_unit]

  plot_type_fun(dt = dt,
                wrangle_function = wrangle_function,
                y_value_label = y_value_label, x_value_label = x_value_label,
                func_group_name = func_group_name,
                fleet_group_name = fleet_group_name,
                save_plot_folder = save_plot_folder,
                plot_height_inches = plot_height_inches,
                plot_width_inches = plot_width_inches,
                units = units, ...)

}

#' Calculates the mean of the data.tables y value grouped by ModelID, StrategyName
#'
#' @param dt data.table
#'
#' @return data.table
WrangleBoxplotMean <- function(dt){

  dt <- dt[, .(y_val = mean(y_val)), by = .(ModelID, StrategyName)]
  return(dt)
}

#' Calculates the probability of y_val going below bref grouped by Strategy and TimeStep
#'
#' @param dt data.table
#'
#' @param bref the reference point that is being checked to see if y_val is going beneath
#'
#' @return data.table
WrangleBoxplotProbability <- function(dt, bref){

  dt <- dt[, .(y_val = sum(y_val < bref) / .N), by = .(StrategyName, TimeStep)]
  return(dt)

}

#' Calculates the Average Annual Variation in y_val
#'
#' @param dt data.table
#'
#' @return data.table
WrangleBoxplotAAV <- function(dt){

  dt[, diff := c(NA, diff(y_val)), by = .(ModelID, StrategyName)]
  minTimeStep = min(dt$TimeStep)
  dt <- dt[as.numeric(TimeStep)>minTimeStep]
  dt[, diff := abs(diff)]
  dt_out <- dt[, .(y_val = 100 * sum(diff)/sum(y_val)), by = .(ModelID, StrategyName)]

  return(dt_out)
}

#' Calculates the annual variation of the y_val grouped by TimeStep, ModelID, StrategyName and ResultType
#'
#' @param dt data.table
#'
#' @return data.table
WrangleTimeseriesAnnualChange = function(dt){

  dt[, diff := c(NA, diff(y_val)), by = .(ModelID, StrategyName)]
  dt <- dt[as.numeric(TimeStep)>1]
  dt_out <- dt[, .(y_val = 100 * diff/y_val), by = .(TimeStep, ModelID, StrategyName)]

  return(dt_out)

}

#' Plots a box plot
#'
#' @param dt The data table contain the data
#' @param start_year The year from which to select data where the years run from 1 to the number of years of the simulation
#' @param end_year The year upto which to select data where the years run from 1 to the number of years of the simulation#' @param end_year
#' @param wrangle_function Set to the type of data manipulation you want depending on the plot you are creating
#' @param y_value_label The y-axis label for the plot
#' @param x_value_label The x-axis label for the plot
#' @param plot_title Optional The plot title
#' @param func_group_name If required, the functional group name to be plotted
#' @param fleet_group_name If required, the fleet group name to be plotted
#' @param true_year_beginning The actual date year that is the start of the simulation
#' @param save_plot_folder The path to the plots folder which will contain folders for each plot type and inside them the plots created using this function
#' @param plot_width_inches The number of inches wide for the produced plot
#' @param plot_height_inches The number of inches high for the produced plot
#' @param units The units of the y-axis
#' @param ... If you are using WrangleBoxplotProbability this will be bref
PlotBoxplot <- function(dt, start_year, end_year,
                        wrangle_function, y_value_label, x_value_label, plot_title = NULL,
                        func_group_name, fleet_group_name,
                        true_year_beginning, save_plot_folder,
                        plot_height_inches, plot_width_inches, units, ...){

  dt <- dt[TimeStep >= start_year & TimeStep <= end_year]
  dt <- wrangle_function(dt, ...)

  # Plot
  y_label <- paste0(y_value_label, ifelse(units=="", "", paste0(" (", units, ")")))
  g <- ggplot2::ggplot(data = dt, ggplot2::aes(x = StrategyName, y = y_val)) + ggplot2::geom_boxplot() +
    ggplot2::labs(x = x_value_label, y = y_label) +
    ggplot2::expand_limits(y = 0)
  list_of_groups_string <- CreateListString(c(func_group_name, fleet_group_name))
  if(!is.null(plot_title)){
    g <- g + ggplot2::ggtitle(paste0(plot_title, " for ", list_of_groups_string, "\nYear ", true_year_beginning + start_year, " to ", true_year_beginning + end_year))
  } else {
    if (diff(c(start_year, end_year)) == 0) {
      g <- g + ggplot2::ggtitle(paste0("The ", y_value_label, " of ",list_of_groups_string, " in Year ", true_year_beginning + start_year - 1))
    } else {
      g <- g + ggplot2::ggtitle(paste0("The Mean ", y_value_label, " of ",list_of_groups_string, "\nYear ", true_year_beginning + start_year, " to ", true_year_beginning + end_year))
    }
  }

  # Save plot
  ggplot2::ggsave(filename =  paste0(save_plot_folder, str_replace_all(Sys.time(), ":", "-"), "-boxplot-", y_value_label, "-", list_of_groups_string, ".pdf"), plot = g, height = plot_height_inches, width = plot_width_inches, units = "in")

}

#' Plots a time series
#'
#' @param dt The data table contain the data
#' @param start_year The year from which to select data where the years run from 1 to the number of years of the simulation
#' @param end_year The year upto which to select data where the years run from 1 to the number of years of the simulation#' @param end_year
#' @param y_value_label The y-axis label for the plot
#' @param x_value_label The x-axis label for the plot
#' @param func_group_name If required, the functional group name to be plotted
#' @param fleet_group_name If required, the fleet group name to be plotted
#' @param true_year_beginning The actual date year that is the start of the simulation
#' @param save_plot_folder The path to the plots folder which will contain folders for each plot type and inside them the plots created using this function
#' @param plot_width_inches The number of inches wide for the produced plot
#' @param plot_height_inches The number of inches high for the produced plot
#' @param units The units of the y-axis
#' @param wrangle_function Set to the type of data manipulation you want depending on the plot you are creating
#' @param ... Not used in this function
PlotTimeseries <- function(dt, start_year = NULL, end_year = NULL,
                           y_value_label, x_value_label,
                           func_group_name, fleet_group_name,
                           true_year_beginning, save_plot_folder,
                           plot_height_inches, plot_width_inches, units,
                           wrangle_function = NULL, ...){

  if(!is.null(wrangle_function)){
    dt <- wrangle_function(dt, ...)
  }

  dt_out <- dt[, .(Low90 = quantile(y_val, probs = 0.05),
                   Low75 = quantile(y_val, probs = 0.125),
                   Low50 = quantile(y_val, probs = 0.25),
                   Median = quantile(y_val, probs = 0.5),
                   Up50 = quantile(y_val, probs = 0.75),
                   Up75 = quantile(y_val, probs = 0.875),
                   Up90 = quantile(y_val, probs = 0.95)),
               by = .(StrategyName, TimeStep)]

  dt_out[, TimeStep := TimeStep + true_year_beginning - 1]

  list_of_groups_string <- CreateListString(c(func_group_name, fleet_group_name))

  g <- ggplot2::ggplot(dt_out, ggplot2::aes(TimeStep))
  g <- g + ggplot2::labs(x = x_value_label, y = paste0(y_value_label, " (", units, ")"))
  g <- g + ggplot2::expand_limits(y = 0)
  g <- g + ggplot2::ggtitle(paste0("The ", y_value_label, " of ",list_of_groups_string))
  g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin = Low90, ymax = Up90), fill = "grey90")
  g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin = Low75, ymax = Up75), fill = "grey75")
  g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin = Low50, ymax = Up50), fill = "grey50")
  g <- g + ggplot2::geom_line(ggplot2::aes(y = Median), colour = "black")
  g <- g + ggplot2::facet_grid(.~StrategyName)
  g <- g + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing = unit(2, "lines"))

  ggplot2::ggsave(filename =  paste0(save_plot_folder, str_replace_all(Sys.time(), ":", "-"), "-timeseries-", y_value_label, "-", list_of_groups_string, ".pdf"), plot = g, height = plot_height_inches, width = plot_width_inches, units = "in")

}






