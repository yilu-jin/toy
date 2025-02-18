#' Core function to generate the number of participants randomized by site
#'
#' @param population_from Subject-level input dataset, typically ADSL.
#' @param population_where A character vector of the filtering condition applied to the subject-level dataset.
#' @param idvar A string representing the variable name for subject ID.
#' @param treatment_var A string representing the variable name for the treatment.
#' @param treatment_order A character vector of treatment groups in the desired order.
#' @param site_var A string representing the variable name for the site, default is 'SITEID'.
#' @param country_var A string representing the variable name for the country, default is NULL.
#' This is optional; if not provided, country information will be excluded.
#' @param level_col_name A logical parameter indicating whether to include the leveling columns.
#' @param display_total_list A list of named lists for aggregating treatment groups.
#'
#' @return A list containing two data frames: one named report.table.n for count results,
#' and the other named report.table.p for percentage results.
#' @import dplyr
#' @export
#'
#' @examples
#' dm_site_core(
#'   population_from = adsl,
#'   population_where = "FASFL == 'Y'",
#'   idvar = "USUBJID",
#'   treatment_var = "TRT01P",
#'   treatment_order = NULL,
#'   site_var = "SITEID",
#'   country_var = "COUNTRY",
#'   level_col_name = TRUE,
#'   display_total_list = NULL
#'  )
dm_site_core <- function(
    population_from,
    population_where = NULL,
    idvar = "USUBJID",
    treatment_var = "TRT01P",
    treatment_order = NULL,
    site_var = "SITEID",
    country_var = NULL,
    level_col_name = FALSE,
    display_total_list = NULL
){

  #-----------------------------------------------------------------------
  # I. Validate parameters and set defaults
  #-----------------------------------------------------------------------

  if(!is.data.frame(population_from)) {
    stop("The required argument, population_from cannot be missing. Please specify a valid input dataset.")
  }

  if(nchar(idvar) == 0) {
    stop("The required argument, idvar cannot be missing.")
  }

  if(nchar(treatment_var) == 0) {
    stop("The required argument, treatment_var cannot be missing.")
  }

  # Check if input variables exist
  inpt_vars <- c(idvar, treatment_var, site_var, country_var)
  diff_vars <- setdiff(inpt_vars, colnames(population_from))

  if(length(diff_vars) > 0 ) {
    stop(paste(diff_vars, collapse = ", "),
         " not found in the population_from dataset.")
  }

  # Check if the values in treatment_order exist
  if(!is.null(treatment_order) &&
     !all(treatment_order %in% unique(population_from[[treatment_var]]))
  ) {
    warning("The argument treatment_order not match with the population_from dataset. ")
  }


  if(!is.null(display_total_list)) {
    if(!is.list(display_total_list)) {
      stop("The required argument, display_total_list must be a list. ")
    }else{
      if( any(is.null(names(display_total_list)) |
              '' %in% names(display_total_list))) {
        stop("All members in display_total_list must have a name. ")
      }
      if(!all(unlist(display_total_list) %in%
              unique(population_from[[treatment_var]])) ) {
        warning("display_total_list not match with population_from")
      }
    }
  }

  # Collapse multiple filters if present
  if(!is.null(population_where)){
    population_where2  <- paste0("(",  population_where, ")", collapse = " & ")
  }



  #-----------------------------------------------------------------------
  # II. Data pre-processing
  #-----------------------------------------------------------------------

  # Copy useful columns for downstream processing
  population_from <- population_from %>%
    dplyr::mutate(
      id = !!sym(idvar) %>% as.character(),
      trt = !!sym(treatment_var) %>% as.character(),
      trt = dunlin::reformat(trt, missing_rule)
    )  %>%
    dplyr::filter(trt != '<Missing>')


  # Set a default for treatment_order if NULL,
  # using alphabetical order of treatment groups present in the subset.
  if(is.null(treatment_order)) {
    treatment_order <- sort(unique(population_from$trt))
  }
  population_from <- population_from %>%
    dplyr::filter(trt %in% treatment_order) %>%
    dplyr::mutate(trt = factor(trt, levels = treatment_order))



  # Take subset of the population
  if(is.null(population_where)){
    population_need <- population_from
  }else{
    population_filter <- paste0("population_from %>% filter(", population_where2, ")")
    population_need <- tryCatch({
      eval(parse(text=population_filter))
    },
    error = function(err){
      message("Error at population_where = ", population_where2, ". \n",
              "Original error message: ", conditionMessage(err))

    })
  }

  # Add the total group to data sets
  if(!is.null(display_total_list)) {

    df_total_pop <-
      names(display_total_list) %>%
      lapply(
        function(this_total) {
          population_need %>%
            filter(trt %in% display_total_list[[this_total]]) %>%
            mutate(trt = this_total)
        }
      )

    population_need <- bind_rows(population_need, df_total_pop)
    population_need$trt <- factor(
      population_need$trt,
      levels = c(treatment_order, names(display_total_list))
    )

  }



  #-----------------------------------------------------------------------
  # III. Data manipulation
  #-----------------------------------------------------------------------

  # III.1 Number of total participants
  nn <- population_need %>% big_n()
  n_subj <- population_need %>% desc_cate()
  n_subj <- n_subj %>% dplyr::mutate(pct = NA_real_)
  n_header <- pivot_cate(n_subj) %>% dplyr::mutate(name = paste("headerN"))


  # III.2 Number of patients by site (and country)
  if(is.null(country_var)) {

    n_all <- population_need %>%
      desc_cate(row_vars = site_var) %>%
      dplyr::mutate(name = !!sym(site_var) %>% as.character())

  } else {

    n_country <- population_need %>%
      desc_cate(row_vars = country_var) %>%
      dplyr::mutate(ord=1)

    n_site <- population_need %>%
      desc_cate(row_vars = c(country_var, site_var)) %>%
      dplyr::mutate(ord=2)

    n_all <- dplyr::bind_rows(n_country, n_site) %>%
      dplyr::arrange(!!sym(country_var), trt, ord, !!sym(site_var)) %>%
      dplyr::select(-ord) %>%
      dplyr::mutate(name = ifelse(
        is.na(!!sym(site_var)),
        paste0(!!sym(country_var)),
        paste0('  ', !!sym(site_var))
      ))

  }

  n_all_wide <-
    n_all %>%
    percent_cate(nn) %>%
    pivot_cate()


  #-----------------------------------------------------------------------
  # IV. Final tables
  #-----------------------------------------------------------------------

  # combine all rows from above into a final data frame
  final_df <-
    dplyr::bind_rows(
      n_header,
      n_all_wide
    )

  # final count table
  report.table.n <- final_df %>%
    dplyr::select(name, any_of(c(country_var, site_var)), starts_with('n_')) %>%
    dplyr::rename_at(vars(starts_with('n_')), ~gsub('n_','',.x))

  # final percentage table
  report.table.p <- final_df %>%
    dplyr::select(name, any_of(c(country_var, site_var)), starts_with('pct_')) %>%
    dplyr::rename_at(vars(starts_with('pct_')), ~gsub('pct_','',.x))

  # drop level name columns if applicable
  if(isFALSE(level_col_name)) {

    report.table.n <- report.table.n %>%
      dplyr::select(-any_of(c(country_var, site_var)))

    report.table.p <- report.table.p %>%
      dplyr::select(-any_of(c(country_var, site_var)))

  }

  return(list(
    report.table.n = report.table.n,
    report.table.p = report.table.p
  ))

}#end of dm_site_core function



