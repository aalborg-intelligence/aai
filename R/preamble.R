#' @title Short function for DT output
#' @param tab The table to format
#' @param ... Arguments passed to `DT::datatable`
#' @export
dt_table <- function(tab, ...){
  tab %>%
    DT::datatable(...,
                  extensions = 'Buttons',
                  filter = "bottom",
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE,
                    dom = 'Btpl',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    lengthMenu = list(c(10, 25, 50, 100, -1),
                                      c("10", "25", "50", "100", "All"))
                  ), class = 'white-space: nowrap')
}

#' @title Simple function for DT output
#' @param tab The table to format
#' @param ... Arguments passed to `DT::datatable`
#' @export
dt_simple <- function(tab, ...){
  tab %>%
    DT::datatable(...,
                  options = list(
                    scrollX = TRUE,
                    dom = 't'
                  ), class = 'white-space: nowrap')
}

#' @title Simple function for kable output
#' @param tab The table to format
#' @param ... Arguments passed to `knitr::kable`
#' @export
kable_ <- function(tab, ...){
  tab %>% knitr::kable() %>%
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left", ...)
}

#' @title Makes print return all rows in a tibble
#' @param ... Arguments passed to `knitr::kable`
#' @export
Print <- function(...) print(n = Inf, ...)
