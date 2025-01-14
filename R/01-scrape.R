utils::globalVariables(c("partei", "spende", "eingang_anzeige", "eingang_spende"))

#' Retrieve Annual Report URLs for Party Financing
#'
#' This function scrapes and retrieves URLs of annual party financing reports
#' from the Bundestag website, starting from 2009. The reports are listed
#' on the specified webpage in a tabular format.
#'
#' @return A character vector containing the URLs of the annual reports.
#' @details This function accesses the Bundestag's official webpage and extracts
#' hyperlinks related to annual party financing reports which then can be fed
#' to the `scrape_annual_report()`function. The function uses the
#' `rvest` package to parse HTML content.
#'
#' @examples
#' \dontrun{
#' urls <- get_annual_report_urls()
#' print(urls)
#' }
#'
#' @importFrom rvest read_html html_nodes html_attr
#' @export
get_annual_report_urls <- function() {
  base_url <- "https://www.bundestag.de"
  start_url <- "https://www.bundestag.de/ajax/filterlist/de/parlament/praesidium/parteienfinanzierung/fundstellen50000/462002-462002?limit=0&noFilterSet=true&offset=0"
  start_page <- rvest::read_html(start_url)
  report_urls <- start_page |>
    rvest::html_nodes(css = "li  a") |>
    rvest::html_attr("href")
  report_urls <- paste0(base_url, report_urls)
  report_urls
}


#' Scrape Annual Report Data
#'
#' This function downloads and extracts tabular data from a specified
#' annual party financing report on the Bundestag website.
#'
#' @param url A character string specifying the URL of the annual report
#' webpage to scrape.
#'
#' @return A data frame containing the extracted tabular data from the
#' provided URL. If no table is found, it may return `NULL` or an empty data frame.
#'
#' @details This function uses the `rvest` package to parse HTML content
#' and extract the first HTML table found on the report page.
#'
#' @examples
#' \dontrun{
#' # Example URL (replace with a valid report URL)
#' report_url <- "https://www.bundestag.de/parlament/praesidium/parteienfinanzierung/fundstellen50000/2025/2025-inhalt-1032412"
#' report_data <- scrape_annual_report(report_url)
#' head(report_data)
#' }
#'
#' @importFrom rvest read_html html_node html_table
#' @export
scrape_annual_report <- function(url) {

  if (httr::http_error(url)) {
    warning("The URL cannot be reached or does not exist: ", url)
    return(NULL)
  }

  page <- rvest::read_html(url)

  table_node <- rvest::html_node(page, css = "table.table")
  if (is.null(table_node)) {
    warning("No table found on the page at the specified URL.")
    return(NULL)
  }
  table <- rvest::html_table(table_node)

  table
}


#' Clean and Standardize Annual Report Data
#'
#' This function cleans and standardizes the data frame extracted from
#' annual party financing reports from bundestag.de. It renames columns,
#' removes unwanted rows, and converts data types for analysis.
#'
#' @param x A data frame containing the raw scraped data from the annual report.
#'
#' @return A cleaned data frame
#'
#' @details This function performs several data cleaning steps:
#' \itemize{
#'   \item Renames the columns to standardized names: `"partei"`, `"spende"`,
#'   `"spender"`, `"eingang_spende"`, and `"eingang_anzeige"`.
#'   \item Filters out rows where the `"partei"` column contains year-like patterns.
#'   \item Detects approximate donation values with the `"ca. "` prefix and flags them.
#'   \item Cleans the `"spende"` column by removing currency symbols,
#'   correcting decimal formatting, and converting values to numeric.
#'   \item Extracts document references from the `"eingang_anzeige"` column.
#'   \item Converts `"eingang_spende"` and `"eingang_anzeige"` to `Date` format.
#' }
#'
#' @examples
#' \dontrun{
#' # Example of cleaning a raw scraped data frame
#' raw_data <- data.frame(
#'   V1 = c("Partei A", "Partei B"),
#'   V2 = c("ca. 1.000 €", "500 €"),
#'   V3 = c("Spender X", "Spender Y"),
#'   V4 = c("02.01.2024", "15.02.2024"),
#'   V5 = c("03.01.2024 1 Drs. 19/1000", "16.02.2024")
#' )
#' clean_data <- cleanup_dataframe(raw_data)
#' print(clean_data)
#' }
#'
#' @importFrom dplyr filter mutate across
#' @importFrom stringr str_detect str_remove str_replace str_extract
#' @export
cleanup_dataframe <- function(x) {

  colnames(x) <- c("partei", "spende", "spender", "eingang_spende", "eingang_anzeige")
  x |>
    dplyr::filter(!stringr::str_detect(partei, "\\d{4}")) |>
    dplyr::mutate(
      spende_ca = stringr::str_detect(spende, "ca. "),
      spende = stringr::str_remove(spende, "\\s(EUR|Euro|\U20AC)") |>
        stringr::str_remove("ca. ") |>
        stringr::str_remove("\\.") |>
        stringr::str_replace(",", ".") |>
        as.numeric(),
      eingang_anzeige_drucksache = stringr::str_extract(eingang_anzeige, "Drs\\.\\s.+"),
      eingang_anzeige = stringr::str_remove(eingang_anzeige, "Drs\\.\\s.+"),
      dplyr::across(c(eingang_spende, eingang_anzeige), function(x) as.Date(x, format = "%d.%m.%Y"))
    )
}



