utils::globalVariables(c("partei", "spende", "eingang_anzeige",
                         "eingang_spende"))

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
#' provided URL. If no table is found, it may return `NULL` or an empty
#' data frame.
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
#'   \item Filters out rows where the `"partei"` column contains year-like
#'   patterns.
#'   \item Detects approximate donation values with the `"ca. "` prefix and
#'   flags them.
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

  x <- x[, 1:5]
  colnames(x) <- c("partei", "spende", "spender", "eingang_spende",
                   "eingang_anzeige")
  x |>
    dplyr::filter(!stringr::str_detect(partei, "\\d{4}")) |>
    dplyr::mutate(
      spende_ca = stringr::str_detect(spende, "ca. "),
      spende = stringr::str_remove(spende, "\\s(EUR|Euro|\U20AC)") |>
        stringr::str_remove("ca. ") |>
        stringr::str_remove_all("\\.") |>
        stringr::str_replace(",", ".") |>
        as.numeric(),
      eingang_anzeige_drucksache = stringr::str_extract(
        eingang_anzeige, "Drs\\.\\s.+"),
      eingang_anzeige = stringr::str_remove(eingang_anzeige, "Drs\\.\\s.+"),
      dplyr::across(c(eingang_spende, eingang_anzeige),
                    function(x) as.Date(x, format = "%d.%m.%Y"))
    )
}

#' Download and Clean Annual Party Financing Reports
#'
#' This function downloads, cleans, and optionally combines annual party
#' financing reports from the German Bundestag website for specified years.
#'
#' @param years A numeric vector of years (from 2009 onwards) indicating which
#' reports to download and process.
#' @param combine Logical. If `TRUE` (default), the cleaned data from all years
#' will be combined into a single data frame. If `FALSE`, a list of data frames
#' for each year will be returned.
#'
#' @return A data frame if `combine = TRUE` or a list of data frames if
#' `combine = FALSE`. Each data frame contains cleaned and standardized data
#' from the selected annual reports.
#'
#' @details This function performs the following steps:
#' \itemize{
#'   \item Retrieves available report URLs using
#'   \code{\link{get_annual_report_urls}}.
#'   \item Filters reports for the specified years (2009 and later).
#'   \item Scrapes and cleans each report using
#'   \code{\link{scrape_annual_report}} and \code{\link{cleanup_dataframe}}.
#'   \item Optionally combines the cleaned data into a single data frame, adding
#'   a \code{jahr} column that indicates the year of the report.
#' }
#'
#' @examples
#' \dontrun{
#' # Download and combine reports from 2018 to 2020
#' reports <- pull_reports(years = 2018:2020)
#' head(reports)
#'
#' # Download reports for 2015 and 2017 without combining
#' reports_list <- pull_reports(years = c(2015, 2017), combine = FALSE)
#' str(reports_list)
#' }
#'
#' @importFrom stats setNames
#' @export
pull_reports <- function(years, combine = TRUE) {
  urls <- get_annual_report_urls()
  available_years <- extract_year_from_url(urls)
  names(urls) <- available_years
  # The data before 2009 is not ready to be scraped from the website
  available_years <- available_years[available_years >= 2009]
  urls <- urls[as.character(years[years %in% available_years])]

  data <- lapply(urls, scrape_annual_report)
  cleaned_data <- lapply(data, cleanup_dataframe)

  if (combine) {
    cleaned_data <- do.call(rbind, cleaned_data)
    cleaned_data$jahr <- as.numeric(format(cleaned_data$eingang_anzeige, "%Y"))
    cleaned_data <- cleaned_data[, c("jahr",
                                     setdiff(names(cleaned_data), "jahr"))]
  }
  cleaned_data
}

#' Extract Year from Bundestag Report URL
#'
#' This function extracts the 4-digit year from a Bundestag party financing
#' report URL.
#'
#' @param x A character vector of URLs containing the year
#'
#' @return A character vector of extracted 4-digit years. If the URL does not
#' contain a year in the specified format, the result will be an empty string.
extract_year_from_url <- function(x) {
  year <- sub(".*fundstellen50000/(\\d{4})/.*", "\\1", x)
  year
}
