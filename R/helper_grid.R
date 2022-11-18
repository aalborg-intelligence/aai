#' @title Create breaks for `seq_cut`
#' @param x Data variable
#' @param step Step size
#' @param center If not through zero, then through `center`
#' @export
#' @examples
#' seq_zero(rnorm(100), step = 2, center = 0)
seq_zero <- function(x, step, center){
  seq(from = floor(min(x)/step)*step - abs(center), to = ceiling(max(x)/step)*step + abs(center), by = step)
}

#' @title Create discretised version with some pretty labels
#' @param x Data variable
#' @param step Step size
#' @param center If not through zero, then through `center`
#' @param breaks Logical. Should break labels we returned?
#' @examples
#' seq_cut(rnorm(100), step = 2, center = 0, breaks = TRUE)
seq_cut <- function(x, step, center, breaks = FALSE){
  bx <- seq_zero(x, step = step, center = center)
  bnx <- max(nchar(paste(bx)))
  if(!breaks) return(bx)
  cut(x, breaks = bx, dig.lab = bnx*2L)
}

#' @title Method for predicting the majority vote or "?" if ties
#' @param n Counts
#' @param x Data vector
pred_max <- function(n, x){
  n_max <- max(n)
  if(sum(n_max == n) == 1L) return(x[n_max == n])
  return("?")
}

#' @title Helper function for making preditive grid
#' @param data Dataset
#' @param step Step size in each data variable
#' @param response The name of the response variable
#' @param center If not through zero, then through `center`
#' @export
pred_grid <- function(data, step = 10, response = "Type", pred_var = "Prediction", center = 0){
  grp_vars <- setdiff(names(data), response)
  data_cuts <- lapply(data[grp_vars], seq_cut, step = step, center = center, breaks = FALSE)
  pred <- data %>%
    mutate(across(all_of(grp_vars), ~ seq_cut(.x, step = step, center = center, breaks = TRUE))) %>%
    table() %>%
    as_tibble() %>%
    group_by_at(all_of(grp_vars)) %>%
    summarise({{pred_var}} := pred_max(n, !!sym(response)), .groups = "drop")
  list(cuts = data_cuts, pred = pred)
}

#' @title Helper function for making preditive grid
#' @param pred_grd Output from YY function
#' @param remove Is parsed to the `remove` argument of `tidyr::separate()`
#' @export
pred_plot_grid <- function(pred_grd, pred_var = "Prediction", remove = TRUE){
  pred_grd <- pred_grd$pred
  vars <- setdiff(names(pred_grd), pred_var)
  for(i in vars){
    ii <- paste0(i, c("_0", "_1"))
    pred_grd <- pred_grd %>% separate(i, into = ii, sep = ",", remove = remove) %>%
      mutate(across(all_of(ii), ~as.numeric(sub("\\(|\\[|\\)|\\]", "", .x))))
  }
  pred_grd
}

#' @title Create grid for new data
#' @param pred_grd Returned from ?
#' @param newdata New data to be used in prediction
#' @export
predict_grid <- function(pred_grd, newdata){
  for(x in names(pred_grd$cuts)){
    bxn <- max(nchar(paste(pred_grd$cuts[[x]])))
    newdata[[paste0(".MfWwfw8szw.",x)]] <-  newdata[[x]]
    newdata[[x]] <- cut(newdata[[x]], breaks = pred_grd$cuts[[x]], dig.lab = 2L*bxn)
  }
  left_join(newdata, pred_grd$pred, by = names(pred_grd$cuts)) %>%
    select(-all_of(names(pred_grd$cuts))) %>%
    set_names(sub("\\.MfWwfw8szw\\.","", names(.)))
}

