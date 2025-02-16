% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape.R
\name{get_annual_report_urls}
\alias{get_annual_report_urls}
\title{Retrieve Annual Report URLs for Party Financing}
\usage{
get_annual_report_urls()
}
\value{
A character vector containing the URLs of the annual reports.
}
\description{
This function scrapes and retrieves URLs of annual party financing reports
from the Bundestag website, starting from 2009. The reports are listed
on the specified webpage in a tabular format.
}
\details{
This function accesses the Bundestag's official webpage and extracts
hyperlinks related to annual party financing reports which then can be fed
to the `scrape_annual_report()`function. The function uses the
`rvest` package to parse HTML content.
}
\examples{
\dontrun{
urls <- get_annual_report_urls()
print(urls)
}

}
