# parteispenden R Package

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Überblick

Das Package **parteispenden** bietet Werkzeuge zum Scrapen und Bereinigen von Berichten über Parteispenden an politische Parteien über 35.000 EUR, die gemäß § 25 Abs. 3 Satz 3 Parteiengesetz auf der Website des Deutschen Bundestages veröffentlicht wurden, unter Verwendung von R. Es automatisiert die Extraktion von Berichts-URLs, scraped tabellarische Daten und bereinigt sie für die Analyse.

---

## Overview

The **parteispenden** package provides tools to scrape and clean reports about party donations to political parties exceeding 35,000 EUR published on the German Bundestag website using R. It automates the extraction of report URLs, scrapes tabular data, and cleans it for analysis.

### About the Data

According to § 25 Abs. 3 Satz 3 Parteiengesetz, donations to political parties exceeding 35,000 EUR must be reported immediately to the President of the German Bundestag. These donations are promptly published as Bundestag printed documents, including the donor's identity. Since March 5, 2024, the reporting and publication obligation arises as soon as the donation exceeds 35,000 EUR.

## Installation

``` r
# Install the package from GitHub 
# devtools::install_github("bydata/parteispenden")

# Load the package
library(parteispenden)
```

## Workflow

``` r
# Download and combine reports from 2018 to 2020 into a single dataframe
combined_reports <- pull_reports(years = 2018:2020)
head(combined_reports)

# Download reports for 2015 and 2017 without combining
separate_reports <- pull_reports(years = c(2015, 2017), combine = FALSE)
str(separate_reports)
```

## Data Dictionary

The resulting cleaned data frame contains the following columns:

-   **jahr**: The year the donation was received (extracted from `eingang_spende`).
-   **partei**: The political party that received the donation.
-   **spende**: The amount of the donation in numeric format.
-   **spender**: The name or identity of the donor.
-   **eingang_spende**: The date when the donation was received.
-   **eingang_anzeige**: The date when the donation was reported.
-   **spende_ca**: Logical flag indicating if the donation amount is approximate (`TRUE` if marked as "ca.").
-   **eingang_anzeige_drucksache**: Document reference ("Drucksache") extracted from the report, potentially missing.

## Notes

-   Reports before 2009 are not available for scraping directly from the website.

## License

MIT License
