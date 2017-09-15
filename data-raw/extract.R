# TODO: include table names and descriptions

# Values "..C" for "confidential" and "..." for "not applicable" are both
# imported as `NA`

library(tidyverse)
library(stringr)
library(tidyxl)
library(unpivotr)
library(here)

book_path <- here("inst", "extdata", "2013-census-electorate-tables.xlsx")
out_path <- here("inst", "extdata", "nzcensus2013.tsv")

book <- tidy_xlsx(book_path)

# Combine all sheets into one data frame (the new tidyxl api will do this)
sheets <- bind_rows(book$data, .id = "sheet")
formats <- book$formats

# Omit the first sheet
datasheets <- filter(sheets, sheet != "Contents")

# Formatting vectors for lookup
bottom_border_ids <- which(!is.na(formats$local$border$bottom$style))
top_border_ids <- which(!is.na(formats$local$border$top$style))
indents <- formats$local$alignment$indent

# Functions to help locate the top-left data cell

get_top_left_header_cell <- function(cells, top_border_ids) {
  # First cell in col 1 that has a top border
  cells %>%
    filter(col == 1, local_format_id %in% top_border_ids) %>%
    top_n(1, desc(row))
}

get_top_left_data_cell <- function(cells, top_left_header_cell) {
  # From the top-left header cell, go down until the first non-blank cell (which
  # is the first row header), then right one column.  That cell might be blank,
  # but it definitely is not a header.
  top_left_header_cell %>%
    offset_S(cells, boundary = ~ !is.na(character), include = TRUE) %>%
    offset_E(cells, 1)
}

get_bottom_cell <- function(cells, bottom_border_ids) {
  # The final cell in column 1 that has a bottom border
  cells %>%
    filter(col == 1, local_format_id %in% bottom_border_ids) %>%
    arrange(desc(row)) %>%
    slice(1)
}

# Functions to split headers into levels

get_row_headers <- function(cells, top_left_header_cell, bottom_cell) {
  # Row headers are all in the same column, so use the amount of indentation as
  # the hierarchy
  out <-
    cells %>%
    filter(col == 1,
           between(row,
                   top_left_header_cell$row + 1,
                   bottom_cell$row),
           !is.na(character)) %>%
    mutate(indent = indents[local_format_id]) %>%
    select(row, col, indent, header = character) %>%
    mutate(indent = if_else(is.na(indent), 0L, indent)) %>%
    split(.$indent) %>%
    map(~ select(.x, -indent))
  c(list(select(top_left_header_cell, row, col, header = character)), out)
}

get_col_headers <- function(cells, top_left_header_cell, top_left_data_cell) {
  # Column header hierarchy is given by the row number
  cells %>%
    filter(col >= 2,
           between(row,
                   top_left_header_cell$row,
                   top_left_data_cell$row - 1),
           !is.na(character)) %>%
    select(row, col, header = character) %>%
    split(.$row)
}

# Function to find the data cells
get_data_cells <- function(cells,
                       top_left_data_cell,
                       bottom_cell,
                       row_headers,
                       col_headers) {
  # Data cells are at the intersection of row and column headers
  cells %>%
    filter(col >= 2,
           between(row,
                   top_left_data_cell$row,
                   bottom_cell$row)) %>%
    semi_join(bind_rows(row_headers), by = "row") %>%
    semi_join(bind_rows(col_headers), by = "col") %>%
    filter(!is.na(numeric) | character %in% c("..C", "...")) %>%
    select(row, col, value = numeric)
}

# Function to match data cells to their nearest headers in each tier
match_headers <- function(datacells, row_headers, col_headers) {
  out <- datacells
  for (i in seq_along(row_headers)) {
    out <- WNW(out, row_headers[[i]])
  }
  for (i in seq_along(col_headers)) {
    out <- NNW(out, col_headers[[i]])
  }
  new_colnames <-
    c("row", "col", "value",
      paste0("col_header", rev(seq_along(col_headers))),
      paste0("row_header", rev(seq_along(row_headers))))
  datacells <- arrange(datacells, row, col)
  colnames(out) <- new_colnames
  out
}

# Function to import the table on a worksheet
tidy_data <- function(cells) {
  top_left_header_cell <- get_top_left_header_cell(cells, top_border_ids)
  top_left_data_cell   <- get_top_left_data_cell(cells, top_left_header_cell)
  bottom_cell          <- get_bottom_cell(cells, bottom_border_ids)
  row_headers          <- get_row_headers(cells, top_left_header_cell, bottom_cell)
  col_headers          <- get_col_headers(cells, top_left_header_cell, top_left_data_cell)
  data_cells           <- get_data_cells(cells, top_left_data_cell, bottom_cell, row_headers, col_headers)
  out                  <- match_headers(data_cells, row_headers, col_headers)
  out$title            <- filter(cells, row == 3, col == 1)$character
  out$dimensions       <- filter(cells, row == 4, col == 1)$character
  out$population       <- filter(cells, row == 5, col == 1)$character
  out
}

# Import all sheets
nzcensus2013 <-
  datasheets %>%
  nest(-sheet) %>%
  mutate(tidy_data = map(data, tidy_data)) %>%
  select(sheet, tidy_data) %>%
  unnest() %>%
  # Sort the columns by name, apart from sheet, row, col and value
  select(sort(colnames(.))) %>%
  select(sheet, title, dimensions, population, row, col, value, everything())

devtools::use_data(nzcensus2013, overwrite = TRUE)
