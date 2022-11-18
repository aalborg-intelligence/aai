#' @title Wrapper around `class::knn1`
#' @param .test Data that should be classified based on the training data
#' @param .train Annotated training data that should be classified the test data
#' @param response Name of the response variable
#' @export
kNN1 <- function(.test, .train, response = "Type"){
  grp_vars <- setdiff(names(.train), response)
  class::knn1(train = .train[,grp_vars], test = .test, cl = .train[[response]])
}

#' @title Wrapper around `class::knn`
#' @param K Number of nearest neighbors to use
#' @param .test Data that should be classified based on the training data
#' @param .train Annotated training data that should be classified the test data
#' @param response Name of the response variable
#' @export
kNN <- function(K = 3, .test, .train, response = "Type"){
  grp_vars <- setdiff(names(.train), response)
  class::knn(train = .train[,grp_vars], test = .test, cl = .train[[response]], k = K)
}

#' @title Wrapper around `class::knn.cv` which does Leave one Out (LoO)
#' @param K Number of nearest neighbors to use (can be a vector)
#' @param .train Annotated training data that should be classified the test data
#' @param response Name of the response variable
#' @export
kNN.loo <- function(K = 3, .train, response = "Type"){
  if(length(K) > 1L) return(unlist(lapply(K, kNN.loo, .train = .train, response = response)))
  grp_vars <- setdiff(names(.train), response)
  LOO <- knn.cv(train = .train[,grp_vars], cl = .train[[response]], k = K)
  mean(LOO == .train[[response]])
}
## kNN.loo <- Vectorize(kNN.loo, vectorize.args = "K")

#' @title Actual cross-validation function for kNN.
#' @param K Vector of nearest neighbor values (the k in kNN)
#' @param .train The data to use kNN on
#' @param response The variable name of the response
#' @param fold The number of folds to use in cross validation
#' @examples
#' data(classification_train_data)
#' K_LOO <- tibble(K = 1:15,
#' LOO = kNN.loo(K, .train = classification_train_data)
#' ) %>%
#' rowwise() %>%
#' mutate(CV = list(kNN.cv(K, .train = classification_train_data)))
#'
#' K_LOO %>% ggplot(aes(x = factor(K))) +
#' geom_boxplot(data = unnest(K_LOO, CV), aes(y = CV)) +
#' geom_point(aes(y = LOO), colour = "#999999") +
#' labs(x = "K", y = "Accuracy")
#' @export
kNN.cv <- function(K = 3, .train, response = "Type", fold = 10){
  if(length(K) > 1L) return(lapply(K, kNN.cv, .train = .train, response = response, fold = fold))
  grp_vars <- setdiff(names(.train), response)
  xval <- sample(fold, size = nrow(.train), replace = TRUE)
  unlist(lapply(seq_len(fold), function(idx){
    mean(knn(train = .train[xval != idx, grp_vars], test = .train[xval == idx, grp_vars],
             cl = .train[[response]][xval != idx], k = K) ==  .train[[response]][xval == idx])
    }
  ))
}
## kNN.cv <- Vectorize(kNN.cv, vectorize.args = "K")

#' @title Visualise a kNN trainer
#' @param K Number of neighbors to use
#' @param .train The training data
#' @param response The name of the response/class variable
#' @param grid The resolution of the grid. Larger numbers gives higher resolution (and slower performance).
#' @examples
#' k <- 3
#' kNN_plot(.train = classification_train_data, K = k) %>%
#' ggplot() + labs(title = paste("K =", k)) +
#' geom_rect(aes(xmin = Længde_0, xmax = Længde_1, ymin = Vægt_0, ymax = Vægt_1, fill = Type), alpha = 0.3) +
#' geom_point(data = train, aes(x = Længde, y = Vægt, colour = Type))
#' @export
kNN_plot <- function(K = 3, .train, response = "Type", grid = 100){
  grp_vars <- setdiff(names(.train), response)
  .train_grid <- .train %>%
    summarise(across(all_of(grp_vars), ~seq(from = min(.x), to = max(.x), len = grid)))
  .train_grid_step <- .train_grid %>% summarise(across(all_of(grp_vars), ~diff(.x[1:2])/2))
  .train_grid <- .train_grid %>% expand.grid() %>% as_tibble()
  .train_grid[[response]] <- kNN(.train = .train, response = response, K = K, .test = .train_grid)
  for(x in grp_vars){
    .train_grid[[x]] <- paste(.train_grid[[x]]-.train_grid_step[[x]],
                              .train_grid[[x]]+.train_grid_step[[x]], sep = ",")
    xx <- paste0(x, c("_0", "_1"))
    .train_grid <- .train_grid %>% separate(x, into = xx, sep = ",") %>%
      mutate(across(all_of(xx), ~as.numeric(.x)))
  }
  .train_grid
}


#' @title Mean distance to k nearest
#' @param K Number of nearest neighbors
#' @param .train The training data
#' @param return_all Logical. Should the distance to the nearest K be returned or just the mean distance of them?
#' @returns If `return_all = FALSE` a dataframe of the mean distance to each class of `response` is returned.
#'     If `return_all = TRUE` a list is returned - `top_K` is as above, `all` contains the closest neighbors from each class.
meandist_to_k_nearest_ <- function(K = 5, .train, response = "Type", return_all = FALSE){
  train_dist <- .train %>% select(-all_of(response)) %>% dist() %>%
    spaa::dist2list() %>% as_tibble() %>% rename(dist = value) %>%
    mutate(across(col:row, ~as.integer(.x))) %>%
    left_join(.train %>% mutate(col = row_number()) %>% select(col, col_resp = all_of(response)), by = "col") %>%
    left_join(.train %>% mutate(row = row_number()) %>% select(row, row_resp = all_of(response)), by = "row") %>%
    filter(col != row)
  train_dist_top_df <- train_dist %>%
    group_by(obs = row, obs_resp = row_resp, resp = col_resp) %>%
    slice_min(dist, n = K, with_ties = FALSE) ## if not row == 29 has six observations in group 1
  train_dist_top <- train_dist_top_df %>%
    summarise(Distance = mean(dist), .groups = "drop")
  if(return_all) return(list(all = train_dist_top_df, top_K = train_dist_top))
  train_dist_top
}
#' @examples
#' data(classification_train_data)
#' meandist_to_k_nearest_(K = 3, .train = classification_train_data) %>%
#'   mutate(same_Type = ifelse(obs_Type == Type, "Y", "N")) %>%
#'   ggplot(aes(x = obs_Type, y = Distance, fill = Type, colour = same_Type)) +
#'   labs(x = "Type of the observation", fill = "Type of the nearest points") +
#'   theme(legend.position = "top") +
#'   guides(colour = FALSE) + scale_colour_manual(values = c("Y" = "#666666", "N" = "#000000")) +
#'   geom_boxplot() + coord_flip()


#' @title Mean distance to k nearest
#' @param K Number of nearest neighbors
#' @param .train The training data
#' @param return_all Logical. Should the distance to the nearest K be returned or just the mean distance of them?
#' @returns If `return_all = FALSE` a dataframe of the mean distance to each class of `response` is returned.
#'     If `return_all = TRUE` a list is returned - `top_K` is as above, `all` contains the closest neighbors from each class.
meandist_to_k_nearest <- function(K = 3, .test, .train, response = "Type", dist = FALSE, info = TRUE){
  response_ <- sym(response)
  grp_vars <- setdiff(names(.train), response)
  test_dist <- proxy::dist(x = select(.train, -!!response_), y = .test) %>%
    unclass() %>% as_tibble(rownames = "obs") %>%
    left_join(.train %>% mutate(obs = paste(row_number())) %>% select(obs, !!response_), by = "obs") %>%
    select(-obs)
  test_dist_top_df <- test_dist %>%
    pivot_longer(cols = -!!response_, names_to = "obs", names_pattern = "V(.*)", values_to = "dist") %>%
    group_by(obs, !!response_)
  if(length(K) == 1L){
    test_dist_top_df <- test_dist_top_df %>% slice_min(dist, n = K) %>%
      summarise(dist = mean(dist), .groups = "drop")
    test_dist_pred <- test_dist_top_df %>%
      group_by(obs) %>% slice_min(dist, n = 1) %>%
      ungroup()
    if(!dist){
      test_dist_pred <- test_dist_pred %>% select(-dist)
      if(!info) test_dist_pred <- deframe(test_dist_pred)
    }
  }
  else{
    test_dist_pred <- map(as.numeric(K), ~ test_dist_top_df %>% slice_min(dist, n = .x) %>%
      summarise(dist = mean(dist), .groups = "drop") %>%
      group_by(obs) %>% slice_min(dist, n = 1) %>%
      ungroup() %>% rename("dist_{{.x}}" := dist, "{response}_{{.x}}" := !!response_)) %>%
      reduce(inner_join, by = "obs")
    if(!dist) test_dist_pred <- test_dist_pred %>% select(-starts_with("dist_"))
  }
  test_dist_pred
}

# meandist_to_k_nearest(K = 3, .test = test, .train = train)
# meandist_to_k_nearest(K = 1:3, .test = test, .train = train)

#' @title Data for plotting a grid based the mean of the K nearest neighbors
#' @param K Number of neighbors
#' @param .train Training data
#' @param response Name of the class variable
#' @param grid Resolution of the grid - higher values gives finer grid
kMD_plot <- function(K = 3, .train, response = "Type", grid = 100){
  grp_vars <- setdiff(names(.train), response)
  .train_grid <- .train %>%
    summarise(across(all_of(grp_vars), ~seq(from = min(.x), to = max(.x), len = grid)))
  .train_grid_step <- .train_grid %>% summarise(across(all_of(grp_vars), ~diff(.x[1:2])/2))
  .train_grid <- .train_grid %>% expand.grid() %>% as_tibble()
  .train_grid <- left_join(.train_grid %>% mutate(obs = paste(row_number())),
                             meandist_to_k_nearest(.train = .train, response = response, .test = .train_grid,
                                                   K = K),
                             by = "obs") %>% select(-obs)
  for(x in grp_vars){
    .train_grid[[x]] <- paste(.train_grid[[x]]-.train_grid_step[[x]],
                              .train_grid[[x]]+.train_grid_step[[x]], sep = ",")
    xx <- paste0(x, c("_0", "_1"))
    .train_grid <- .train_grid %>% separate(x, into = xx, sep = ",") %>%
      mutate(across(all_of(xx), ~as.numeric(.x)))
  }
  .train_grid
}

# kMD_plot(.train = train, K = 3, grid = 100) %>%
#   ggplot() +
#   geom_rect(aes(xmin = Længde_0, xmax = Længde_1, ymin = Vægt_0, ymax = Vægt_1, fill = Type),
#             alpha = 0.3) +
#   geom_point(data = train, aes(x = Længde, y = Vægt, colour = Type))
#
# rr <- kMD_plot(.train = train, K = 1:6, grid = 50)
# 1:6 %>% map(~ rr %>% select(1:4, Type = paste0("Type_", as.numeric(.x))) %>%
#   ggplot() + labs(title = paste("K =", .x)) +
#   geom_rect(aes(xmin = Længde_0, xmax = Længde_1, ymin = Vægt_0, ymax = Vægt_1,
#                 fill = Type), alpha = 0.3) +
#   geom_point(data = train, aes(x = Længde, y = Vægt, colour = Type))) %>%
#   wrap_plots(ncol = 3, nrow = 2) + plot_layout(guides = "collect")

