#' New Zealand 2013 census data
#'
#' A dataset containing aggregate statistics from the 2013 New Zealand census.
#'
#' This data has been randomly rounded to protect confidentiality.  Individual
#' figures may not add up to totals, and values for the same data may vary in
#' different tables.
#'
#' Source data symbols "..C" for "confidential" and "..." for "not applicable"
#' have both been imported as `NA`
#'
#' Refer to the source spreadsheet for other notes.
#'
#' @format A data frame with 416,085 rows and 11 variables:
#'
#' * `sheet` the name of the table and the worksheet that the data was imported
#'     from
#' * `title` the title of the table
#' * `dimensions` the subgroups that the table uses
#' * `population` the population that the table reports
#' * `row` the row number of the cell that the data was imported from
#' * `col` the column number of the cell that the data was imported from
#' * `value` the number of people observed in the given subset of the New
#' Zealand population
#' * `col_header1` a type of subset of the New Zealand population
#' * `col_header2` a subset of the New Zealand population
#' * `col_header3` a subset of the New Zealand population
#' * `row_header1` a type of subset of the New Zealand population
#' * `row_header2` a subset of the New Zealand population
#' * `row_header3` a subset of the New Zealand population
#' * `row_header4` a subset of the New Zealand population
#'
#' @source
#' http://www.stats.govt.nz/Census/2013-census/data-tables/electorate-tables.aspx
#' released under the CC BY 4.0 licence
#' https://creativecommons.org/licenses/by/4.0/
"nzcensus2013"
