#' Set defaults for function
#'
#' @param func Function
#' @param recursive Recursively merge defaults with new arguments or not
#' @param ... Default arguments
#' @export
set_func_defaults <- function(func, recursive = FALSE, ...) {
  cl <- as.call(c(list(quote(func)), list(...)))
  cl <- match.call(func,cl)
  args <- as.list(cl)[-1]
  newfunc <- function(...) {
    new_cl <- as.call(c(list(quote(func)), list(...)))
    new_cl <- match.call(func,new_cl)
    new_args <- as.list(new_cl)[-1]
    if (recursive) {
      new_args <- modifyList(args, new_args, keep.null = TRUE)
    }  else {
      keep_args <- setdiff(names(args), names(new_args))
      new_args <- c(args[keep_args], new_args)
    }
    do.call(func, new_args)
  }
}


#' create column definition for columns
#'
#' @param df dataframe
#' @param pattern pattern of colnames
#' @param col_def column definition
#'
#' @return A list of column definition
#'
#' @export
create_col_defs <- function(df, pattern, col_def) {
  cols <- stringr::str_extract(string = colnames(df), pattern = pattern)
  cols <- cols[!is.na(cols)]
  def_of_cols <- vector("list", length(cols))
  def_of_cols <- setNames(def_of_cols, cols)
  for (i in seq_along(def_of_cols)) {
    def_of_cols[[i]] <- col_def
  }
  def_of_cols
}


#' Create an HTML table widget using the DataTables library with Chinese menu
#'
#' @inheritParams DT::datatable
#' @param pageLength the length of page
#' @param lengthMenu the menu of length
#' @param buttons buttons shows
#' @export
c_datatable <-
  set_func_defaults(
    func = DT::datatable,
    recursive = TRUE,
    extensions = "Buttons",
    options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'),
      dom = 'lBfrtip',
      pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )




#' Add image url prefix to image column
#'
#' @param df a tbl or dataframe
#' @param df_image_url_col_name the colname of image url column
#' @param image_url_prefix the prefix for the image url column
#' @return df
#' @export
fix_image_link <- function(df,
                           df_image_title_col_name,
                           df_image_url_col_name
                           ) {
  title_col_name <- rlang::ensym(df_image_title_col_name)
  url_col_name <- rlang::ensym(df_image_url_col_name)
  df %>%
    dplyr::mutate(!!url_col_name := dplyr::case_when(
      is.na(!!url_col_name) | stringi::stri_length(!!url_col_name) == 0  ~ NA_character_,
      TRUE ~ paste0(ifelse(stringr::str_starts(!!title_col_name, "C"),
                           getOption('shinycomp.image_url_prefix_2'),
                           getOption('shinycomp.image_url_prefix_1')
                           ),
                    !!url_col_name)
    ))
}
