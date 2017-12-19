# Purpose: this script defines the functions that are used to generate kables for the R markdown.
#
# Notes: the function generate.processed_models_kables

# generate.processed_models_kables ----------------------------------------------------------------------------

#' Create a kable of the models and experiments that were processed
#'
#' \code{generate.processed_models_kables} is function that creates kables of the models that were sucessfully
#' processed and are included in the subsquent project analysis.
#'
#' @param remove_list_path the path to the .csv file that defines which models are to be exluded in the 0.5 post
#' pic processing step.
#' @param removed_path the path to the .rda object that contains the observations that were intentially removed
#' and why.
#' @param processed_data_path the path to some .rda object of data actually used in the analysis.
#' @param output_path the path to the location of where to save the list of kables
#' @param output_name is the name for the data object saved at the end of the function.
#' @return a list of 2 kables that contain information pertaining to the models that were removed pre-processing
#' and another kable of the processed models / experiments / variables

generate.processed_models_kables <- function(remove_list_path, removed_path, processed_data_path, output_path, output_name = "observations_table.rda"){

  # Import the data frames using the paths provided as function input.
  # Start by importing the dir path to the .csv used to remove model observations and the data frame of removed
  # observations and combine into a single data frame.
  to_remove_df <- readr::read_csv(remove_list_path) %>%  mutate(note = " ")
  removed_df  <- get(load(removed_path)) %>% mutate(note = "not all model observations were removed just for certain netcdfs")
  removed_obs <- dplyr::bind_rows(to_remove_df, removed_df)

  # Load the actual processed raw data
  data <- get(load(processed_data_path))


  # Save information as kable
  observations_removed <- knitr::kable(removed_obs)

  # Create a dataframe of the unique model / variable / experiment combinations.
  basin1 <- unique(data$basin)[1]

  data %>%
    filter(basin == basin1, month_name == "Jan", year == min(year)) %>%
    select(experiment, variable, model, year, value) %>%
    distinct() %>%
    spread(variable, value) ->
    ex_mo_variables

  # This is not a great fix... should redo
  if("spco2" %in% names(ex_mo_variables)){
    # create historical and future ids for each variable.
    ex_mo_variables %>%
      filter(grepl("[H|h]istorical", experiment)) %>%
      mutate(ph_hist = if_else(is.na(ph), "  ", "hist")) %>%
      mutate(spco2_hist = if_else(is.na(spco2), "  ", "hist")) %>%
      mutate(tos_hist = if_else(is.na(tos), "  ", "hist")) %>%
      mutate(co3_hist = if_else(is.na(co3), "  ", "hist")) %>%
      select(model, ph_hist, spco2_hist, tos_hist, co3_hist) %>%
      distinct ->
      historical_ids

    ex_mo_variables %>%
      filter(!grepl("[H|h]istorical", experiment)) %>%
      mutate(ph_fut = if_else(is.na(ph), "  ", "fut")) %>%
      mutate(spco2_fut = if_else(is.na(spco2), "  ", "fut")) %>%
      mutate(tos_fut = if_else(is.na(tos), "  ", "fut")) %>%
      mutate(co3_fut = if_else(is.na(co3), "  ", "fut")) %>%
      select(model, ph_fut, spco2_fut, tos_fut, co3_fut) %>%
      distinct ->
      future_ids

    # Combine the future and histoircal id data frame together and
    # create the variable columns
    historical_ids %>%
      inner_join(future_ids, by = "model") %>%
      unite(ph, ph_hist, ph_fut, sep = "  ") %>%
      unite(spco2, spco2_hist, spco2_fut, sep = "  ") %>%
      unite(co3, co3_hist, co3_fut, sep = "  ") %>%
      unite(tos, tos_hist, tos_fut, sep = "  ") ->
      EandV_table_df

  } else {

    # create historical and future ids for each variable.
    ex_mo_variables %>%
      filter(grepl("[H|h]istorical", experiment)) %>%
      mutate(ph_hist = if_else(is.na(ph), "  ", "hist")) %>%
      mutate(tos_hist = if_else(is.na(tos), "  ", "hist")) %>%
      mutate(co3_hist = if_else(is.na(co3), "  ", "hist")) %>%
      select(model, ph_hist, tos_hist, co3_hist) %>%
      distinct ->
      historical_ids

    ex_mo_variables %>%
      filter(!grepl("[H|h]istorical", experiment)) %>%
      mutate(ph_fut = if_else(is.na(ph), "  ", "fut")) %>%
      mutate(tos_fut = if_else(is.na(tos), "  ", "fut")) %>%
      mutate(co3_fut = if_else(is.na(co3), "  ", "fut")) %>%
      select(model, ph_fut, tos_fut, co3_fut) %>%
      distinct ->
      future_ids

    # Combine the future and histoircal id data frame together and
    # create the variable columns
    historical_ids %>%
      inner_join(future_ids, by = "model") %>%
      unite(ph, ph_hist, ph_fut, sep = "  ") %>%
      unite(co3, co3_hist, co3_fut, sep = "  ") %>%
      unite(tos, tos_hist, tos_fut, sep = "  ") ->
      EandV_table_df

  }



  # Format the data frame into a kable
  EandV_table_df %>%
    knitr::kable(format = "markdown") ->
    EandV_table

  # Format output and save
  tables = list(observations_removed = observations_removed, experiment_variable = EandV_table)
  save(tables, file = file.path(output_path, output_name))
  message("kables saved as ", file.path(output_path, output_name))

  return(tables)

}

# End ---
