library(data.table)
source("https://raw.githubusercontent.com/MarkPlatts/MSE_Plugin_Results_Plotting/master/share_tools.R")

# main function ==================================================================

#' Creates a single column of values that can be merged to form a table of MSE values for a species-fleet combination
#'
#' @param results_folder_path The path to the folder holding the csv file from which to create a table
#' @param ifunc_groups The name of the functional group for which to create the table
#' @param fleet_group_number The fleet name as specified in the csv file title
#' @param yearly_in_filename Set to "Yearly" if you want to use a csv file that contains "Yearly" in the name
#' @param n_label_cols The number of label columns in the csv file before the numeric values
#' @param col_name The name to give the column being created
#' @param year The year in which to calculate the values
#' @param first_year The first year of the simulation in the csv file
#' @param area If specified the output values are multiplied by this
#' @param scale If specified the output values are divided by this
#' @param bref The biomass reference point for which we are calculating the probability of going beneath. Specify in units of t/km2.
#'
#' @export
#'
#' @examples
#' blim_table_2010 <- create_table(results_folder_path = "C:/Users/Mark/Box Sync/Baltic - Stockholm/Results-and-plots-2017-09-08/Results/Biomass/",
#' fun = calc_prob_below,
#' ifunc_groups = functional_group,
#' yearly_in_filename = "Yearly",
#' n_label_cols = 4,
#' col_name = 'p(<Blim)',
#' year = 2010,
#' first_year = 1990,
#' bref = 0.4
#' )
#'
#' yield_table_2012 <- create_table(results_folder_path = "C:/Users/Mark/Box Sync/Baltic - Stockholm/Results-and-plots-2017-09-08/Results/LandingsTrajectories/",
#' fun = calc_avg,
#' ifunc_groups = functional_group,
#' fleet_group_number = "AllFleets",
#' yearly_in_filename = "Yearly",
#' n_label_cols = 5,
#' col_name = 'avg(Y) (kt)',
#' year = 2012,
#' first_year = 2000,
#' area = 240000,
#' scale = 1000
#'
#' )
#'
create_table <- function(results_folder_path, ifunc_groups,
                         n_label_cols, col_name,
                         year, first_year, fun,
                         yearly_in_filename = NULL, fleet_group_number = NULL,
                         area = 1, scale = 1, ...){


  strings_in_filename <- c(yearly_in_filename, ifunc_groups, fleet_group_number)

  dt <- LoadFile_ContainsListStrings(Dir.Path = results_folder_path,
                                     StringsInFileName = strings_in_filename,
                                     Read.Function = fread)

  dt <- melt(dt, id.vars = 1:n_label_cols, variable.name = "TimeStep", value.name = "y_val")
  dt[, TimeStep := as.numeric(TimeStep) + first_year - 1]

  dt <- dt[TimeStep == year]

  output <- fun(dt, ...)

  col_name = paste0(col_name, " ", year)
  setnames(output, "y_val", col_name)

  output[, (col_name) := get(col_name) * area/scale]

}

# =======================================================================

# functions to pass =====================================================

#' Calculates the proportion of models for each strategy that go below bref
#'
#' @param dt the data table containing some y_values grouped by StrategyName
#' @param bref the reference point
#'
#' @return a data.table with the proportions
#'
calc_prob_below <- function(dt, bref){
  tot_below <- dt[, .(total_below = sum(y_val < bref)), by = StrategyName]
  tot <- dt[,.(total =.N), by = StrategyName]
  output <- merge(tot_below, tot, by = "StrategyName")
  output[, y_val := total_below/total]
  output[, (c("total_below", "total")) := NULL]
}

#' Calculates the mean of y_val by strategy
#'
#' @param dt a data table containing y_val that can be grouped by StrategyName
#'
#' @return a datatable of means labelled by StrategyName
#'
calc_avg <- function(dt){
  dt[, .(y_val = mean(y_val)), by = .(StrategyName)]
}

# =======================================================================
