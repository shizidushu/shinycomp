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
  def_of_cols <- stats::setNames(def_of_cols, cols)
  for (i in seq_along(def_of_cols)) {
    def_of_cols[[i]] <- col_def
  }
  def_of_cols
}


#' Set defaults for function
#' Create an HTML table widget using the DataTables library with Chinese menu
#'
#' @param ... Arguments to \code{DT::datatable}
#'
#' @export
c_datatable <-
  set_func_defaults(
    func = DT::datatable,
    recursive = TRUE,
    extensions = "Buttons",
    options = list(
      language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Chinese.json"),
      dom = 'lBfrtip',
      pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )




#' Add image url prefix to image column
#'
#' @param df a tbl or dataframe
#' @param df_image_url_col_name Column name of image url
#' @param df_image_title_col_name Column name of image title
#' @param image_pattern Only fix image with this pattern; set others to \code{NA_character_}
#' @param sku_image_url_prefix_1 image url prefix 1
#' @param title_pattern_for_prefix_2 When the title column has this pattern, fix with image url prefix 2. Otherwise fix with image url prefix 1.
#' @param sku_image_url_prefix_2 image url prefix 2
#' @return df
#' @export
fix_image_link <- function(df,
                           df_image_url_col_name = "图片",
                           df_image_title_col_name = "SKU",
                           image_pattern = ".*.[a-z]+",
                           sku_image_url_prefix_1 = getOption('sku_image_url_prefix_1'),
                           title_pattern_for_prefix_2 = "^C.*",
                           sku_image_url_prefix_2 = getOption('sku_image_url_prefix_2')) {
  df %>%
    dplyr::mutate(dplyr::across(
      {{ df_image_url_col_name }},
      ~ dplyr::case_when(
        grepl(image_pattern, .x) ~ paste0(ifelse(
          stringr::str_starts({{ df_image_title_col_name }}, title_pattern_for_prefix_2),
          sku_image_url_prefix_2,
          sku_image_url_prefix_1
        ),
        .x),
        TRUE ~ NA_character_
      )))
}



#' download image of the image column
#' @param df a tbl or dataframe
#' @param df_image_url_col_name Column name of image url
#' @param df_image_title_col_name Column name of image title
#' @param image_save_path The path images saved to.
#' @return df
#' @export
download_image_url <- function(df,
                               df_image_url_col_name = "图片",
                               df_image_title_col_name = "SKU",
                               image_save_path = getOption('sku_image_save_path', "~/images")) {
  df %>%
    dplyr::filter(dplyr::across({{ df_image_url_col_name }}, ~ !is.na(.x))) %>%
    purrr::pwalk(function(...) {
      current <- tibble::tibble(...)

      image_file_extension <- stringr::str_extract(
        current[[df_image_url_col_name]],
        ".[a-z]*$")

      image_filepath <- paste0(
        file.path(image_save_path, current[[df_image_title_col_name]]),
        image_file_extension)

      if (!file.exists(image_filepath)) {
        dir.create(image_save_path, showWarnings = FALSE)
        image_url <- current[[df_image_url_col_name]]
        curl::curl_download(url = image_url, image_filepath)
      }

    })
  df
}


#' Create a workbook for dataframe with sku image
#'
#' @inheritParams download_image_url
#' @inheritParams openxlsx::setColWidths
#' @inheritParams openxlsx::setRowHeights
#' @inheritParams openxlsx::insertImage
#' @return an openxlsx workbook
#' @export
create_workbook_sku_image <- function(df,
                                      df_image_url_col_name = "图片",
                                      df_image_title_col_name = "SKU",
                                      image_save_path = getOption('sku_image_save_path', "~/images"),
                                      widths = 27,
                                      heights = 144,
                                      width = 5,
                                      height = 5,
                                      units = "cm",
                                      dpi = 300) {
  ## 创建工作簿
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  ## 将数据写入工作簿
  tb <- df
  tb[[df_image_url_col_name]] <- ""
  openxlsx::writeDataTable(wb, 1, tb)
  ## 寻找图片列的位置
  col_pos <- which(df_image_url_col_name == colnames(df))
  ## 设置高度和宽度
  openxlsx::setColWidths(wb, 1, cols = col_pos, widths = widths)
  openxlsx::setRowHeights(wb, 1, rows = 2:(nrow(df) + 1), heights = heights)

  df %>%
    dplyr::mutate(.row_number = dplyr::row_number()) %>%
    purrr::pwalk(function(...) {
      current <- tibble::tibble(...)

      image_file_extension <- stringr::str_extract(
        current[[df_image_url_col_name]],
        ".[a-z]*$")

      image_filepath <- paste0(
        file.path(image_save_path, current[[df_image_title_col_name]]),
        image_file_extension)

      if (file.exists(image_filepath)) {
        ## 将图片写入工作簿 其中开始的行加上一是因为标题行占去的一列
        openxlsx::insertImage(
          wb,
          "Sheet 1",
          image_filepath,
          width = width,
          height = height,
          units = units,
          dpi = dpi,
          startRow = current[[".row_number"]] + 1,
          startCol = col_pos
        )
      }
    })

  wb
}


#' Write dataframe with image url to excel file with image column
#'
#' @inheritParams create_workbook_sku_image
#' @param file A character string naming an xlsx file
#' @export
write_excel_image <- function(df,
                              file = tempfile(fileext = ".xlsx"),
                              df_image_url_col_name = "图片",
                              df_image_title_col_name = "SKU",
                              image_save_path = getOption('sku_image_save_path', "~/images"),
                              widths = 27,
                              heights = 144,
                              width = 5,
                              height = 5,
                              units = "cm",
                              dpi = 300) {
  # download images to temp image dir
  shinycomp::download_image_url(df = df,
                           df_image_title_col_name = df_image_title_col_name,
                           df_image_url_col_name = df_image_url_col_name,
                           image_save_path = image_save_path)
  # create workbook
  wb <- shinycomp::create_workbook_sku_image(df = df,
                                        df_image_title_col_name = df_image_title_col_name,
                                        df_image_url_col_name = df_image_url_col_name,
                                        image_save_path = image_save_path,
                                        widths = widths,
                                        heights = heights,
                                        width = width,
                                        height = height,
                                        units = units,
                                        dpi = dpi)

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  file
}
