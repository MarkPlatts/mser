# library(data.table)
# source("https://raw.githubusercontent.com/MarkPlatts/MSE_Plugin_Results_Plotting/master/share_tools.R")

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

#' Saves the table to
#'
#' @param list_of_tables a list of tables that you want to combine and save to pdf
#' @param name_of_file The name of the pdf file to be created
#' @param pdf_path The path to where to save the pdf
#' @param pdf.height The height of the pdf
#' @param pdf.width The width of the pdf
#' @param number_decimal_places The number of decimal places to display in the table
#'
#' @return
#' @export
#'
#' @examples
save_table_pdf <- function(list_of_tables, name_of_file, pdf_path, pdf.height, pdf.width, number_decimal_places){

  pdf_path_name <- paste0(pdf_path, name_of_file, ".pdf")

  major_table <- Reduce(function(x,y) merge(x,y, by = "StrategyName"), list_of_tables)

  # reduce the number of decimal places
  major_table <- df_reduce_decimal_places(major_table, number_decimal_places)

  pdf(pdf_path_name, height = pdf.height, width = pdf.width)
  gridExtra::grid.table(major_table, rows = NULL)
  dev.off()

}

df_reduce_decimal_places <- function(major_table, number_decimal_places){
  major_table <- as.data.frame(major_table)
  is.num <- sapply(major_table, is.numeric)
  major_table[is.num] <- as.data.table(lapply(major_table[is.num], round, number_decimal_places))
  return(major_table)
}

#' Title
#'
#' @param number_decimal_places The number of decimal places to display in the table
#' @param functional_group The function group that the table is being created for
#' @param results_folder_path The path to the folder holding the results csvs and folders
#' @param pdf_path The path to where to save the pdf
#' @param pdf.height The pdf height
#' @param pdf.width The pdf width
#' @param first_year_hindcast The actual first year of the hindcast e.g. 1990
#' @param first_year_forecast The actual first year of the forecast e.g. 1990
#' @param first_year_selection The actual first year of the selection e.g. 1990
#' @param second_year_selection The actual second year of the selection e.g. 1990
#' @param third_year_selection The actual third year of the selection e.g. 1990
#' @param blim The blim for the given functional group
#' @param bpa Bpa for the given functional group
#' @param area The area of the EwE model
#' @param scale A scaling parameter, e.g. setting to 1000 it will divide the output values by 1000
#'
#' @return
#' @export
#'
#' @examples
#' table_default(number_decimal_places = 4, functional_group = "AdCod",
#' results_folder_path = "C:/Users/Mark/Box Sync/Baltic - Stockholm/Results-and-plots-2017-09-08/Results/",
#' pdf_path = "C:/Users/Mark/Desktop/Plots/", pdf.height = 2, pdf.width = 17,
#' first_year_hindcast = 1990, first_year_forecast = 2000, first_year_selection = 2010,
#' second_year_selection = 2012, third_year_selection = 2015,
#' blim = 0.4, bpa = 0.5, area = 240000, scale = 1000)
table_default <- function(number_decimal_places, functional_group,
                                  results_folder_path, pdf_path, pdf.height, pdf.width,
                                  first_year_hindcast, first_year_forecast, first_year_selection,
                                  second_year_selection, third_year_selection,
                                  blim, bpa, area, scale){

  results_folder_path_biomass = paste0(results_folder_path, "Biomass/")
  results_folder_path_landings = paste0(results_folder_path, "LandingsTrajectories/")
  results_folder_path_realisedF = paste0(results_folder_path, "RealisedF/")
  results_folder_path_HCR_Quota = paste0(results_folder_path, "HCRQuota_Targ/")

  blim_table_2010 <- create_table(results_folder_path = results_folder_path_biomass,
                                  fun = calc_prob_below,
                                  ifunc_groups = functional_group,
                                  yearly_in_filename = "Yearly",
                                  n_label_cols = 4,
                                  col_name = 'p(B<Blim)',
                                  year = first_year_selection,
                                  first_year = first_year_hindcast,
                                  bref = blim
  )

  blim_table_2012 <- create_table(results_folder_path = results_folder_path_biomass,
                                  fun = calc_prob_below,
                                  ifunc_groups = functional_group,
                                  yearly_in_filename = "Yearly",
                                  n_label_cols = 4,
                                  col_name = 'p(B<Blim)',
                                  year = second_year_selection,
                                  first_year = first_year_hindcast,
                                  bref = blim
  )

  blim_table_2015 <- create_table(results_folder_path = results_folder_path_biomass,
                                  fun = calc_prob_below,
                                  ifunc_groups = functional_group,
                                  yearly_in_filename = "Yearly",
                                  n_label_cols = 4,
                                  col_name = 'p(B<Blim)',
                                  year = third_year_selection,
                                  first_year = first_year_hindcast,
                                  bref = blim
  )

  bpa_table_2010 <- create_table(results_folder_path = results_folder_path_biomass,
                                 fun = calc_prob_below,
                                 ifunc_groups = functional_group,
                                 yearly_in_filename = "Yearly",
                                 n_label_cols = 4,
                                 col_name = 'p(<Bpa)',
                                 year = first_year_selection,
                                 first_year = first_year_hindcast,
                                 bref = bpa
  )

  bpa_table_2012 <- create_table(results_folder_path = results_folder_path_biomass,
                                 fun = calc_prob_below,
                                 ifunc_groups = functional_group,
                                 yearly_in_filename = "Yearly",
                                 n_label_cols = 4,
                                 col_name = 'p(B<Bpa)',
                                 year = second_year_selection,
                                 first_year = first_year_hindcast,
                                 bref = bpa
  )

  bpa_table_2015 <- create_table(results_folder_path = results_folder_path_biomass,
                                 fun = calc_prob_below,
                                 ifunc_groups = functional_group,
                                 yearly_in_filename = "Yearly",
                                 n_label_cols = 4,
                                 col_name = 'p(B<Bpa)',
                                 year = third_year_selection,
                                 first_year = first_year_hindcast,
                                 bref = bpa
  )

  yield_table_2010 <- create_table(results_folder_path = results_folder_path_landings,
                                   fun = calc_avg,
                                   ifunc_groups = functional_group,
                                   fleet_group_number = "AllFleets",
                                   yearly_in_filename = "Yearly",
                                   n_label_cols = 5,
                                   col_name = 'avg(Y) (kt)',
                                   year = first_year_selection,
                                   first_year = first_year_forecast,
                                   area = area,
                                   scale = scale
  )

  yield_table_2012 <- create_table(results_folder_path = results_folder_path_landings,
                                   fun = calc_avg,
                                   ifunc_groups = functional_group,
                                   fleet_group_number = "AllFleets",
                                   yearly_in_filename = "Yearly",
                                   n_label_cols = 5,
                                   col_name = 'avg(Y) (kt)',
                                   year = second_year_selection,
                                   first_year = first_year_forecast,
                                   area = area,
                                   scale = scale

  )

  yield_table_2015 <- create_table(results_folder_path = results_folder_path_landings,
                                   fun = calc_avg,
                                   ifunc_groups = functional_group,
                                   fleet_group_number = "AllFleets",
                                   yearly_in_filename = "Yearly",
                                   n_label_cols = 5,
                                   col_name = 'avg(Y) (kt)',
                                   year = third_year_selection,
                                   first_year = first_year_forecast,
                                   area = area,
                                   scale = scale
  )

  f_table_2010 <- create_table(results_folder_path = results_folder_path_realisedF,
                               fun = calc_avg,
                               ifunc_groups = functional_group,
                               yearly_in_filename = "Yearly",
                               n_label_cols = 4,
                               col_name = 'avg(F)',
                               year = first_year_selection,
                               first_year = first_year_forecast
  )

  f_table_2012 <- create_table(results_folder_path = results_folder_path_realisedF,
                               fun = calc_avg,
                               ifunc_groups = functional_group,
                               yearly_in_filename = "Yearly",
                               n_label_cols = 4,
                               col_name = 'avg(F)',
                               year = second_year_selection,
                               first_year = first_year_forecast
  )

  f_table_2015 <- create_table(results_folder_path = results_folder_path_realisedF,
                               fun = calc_avg,
                               ifunc_groups = functional_group,
                               yearly_in_filename = "Yearly",
                               n_label_cols = 4,
                               col_name = 'avg(F)',
                               year = third_year_selection,
                               first_year = first_year_forecast
  )

  tac_table_2010 <- create_table(results_folder_path = results_folder_path_HCR_Quota,
                                 fun = calc_avg,
                                 ifunc_groups = functional_group,
                                 fleet_group_number = "AllFleets",
                                 n_label_cols = 5,
                                 col_name = 'avg(TAC)',
                                 year = first_year_selection,
                                 first_year = first_year_forecast
  )

  tac_table_2012 <- create_table(results_folder_path = results_folder_path_HCR_Quota,
                                 fun = calc_avg,
                                 ifunc_groups = functional_group,
                                 fleet_group_number = "AllFleets",
                                 n_label_cols = 5,
                                 col_name = 'avg(TAC)',
                                 year = second_year_selection,
                                 first_year = first_year_forecast
  )

  tac_table_2015 <- create_table(results_folder_path = results_folder_path_HCR_Quota,
                                 fun = calc_avg,
                                 ifunc_groups = functional_group,
                                 fleet_group_number = "AllFleets",
                                 n_label_cols = 5,
                                 col_name = 'avg(TAC)',
                                 year = third_year_selection,
                                 first_year = first_year_forecast
  )


  list_of_tables <- list(blim_table_2010, blim_table_2012, blim_table_2015,
                         bpa_table_2010, bpa_table_2012, bpa_table_2015)

  save_table_pdf(list_of_tables = list_of_tables, name_of_file = "table1",
                 pdf_path = pdf_path,
                 pdf.height = pdf.height, pdf.width = pdf.width,
                 number_decimal_places = number_decimal_places)


  list_of_tables <- list(f_table_2010, f_table_2012, f_table_2015,
                         tac_table_2010, tac_table_2012, tac_table_2015,
                         yield_table_2010, yield_table_2012, yield_table_2015)

  save_table_pdf(list_of_tables = list_of_tables, name_of_file = "table2",
                 pdf_path = pdf_path,
                 pdf.height = pdf.height, pdf.width = pdf.width,
                 number_decimal_places = number_decimal_places)

  graphics.off()

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
