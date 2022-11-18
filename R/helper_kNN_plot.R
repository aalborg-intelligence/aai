#' @title Plot of data for exercise by Jan B Sørensen on classification
#' @param train Training data set
#' @param test Test data set
#' @param x,y,colour parameters controlling the x and y axis and point colours
#' @param selected points to highlight
#' @export
#' @examples
#' data(classification_train_data)
#' data(classification_test_data)
#' type_cols <- c("1" = "#E41A1C", "2" = "#377EB8", "3" = "#4DAF4A", "?" = "#444444")
#' xy_plot(train = classification_train_data, x = Længde, y = Vægt, colour = Type) +
#'   scale_colour_manual(values = type_cols)
xy_plot <- function(train, x, y, colour, test = NULL, selected = NULL){
  ## Declaration of colours on plot
  ## Specify training plot
  xyplot <- train %>% ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_point(aes(colour = {{colour}}))
  if(!is.null(selected)){
    train_selected <- train %>% slice(selected)
    xyplot <- xyplot +
      geom_point(data = train_selected,
                 aes(x = {{x}}, y = {{y}}), stroke = 0.5, pch = 1, colour = "#000000")
  }
  ## Add test layer
  if(!is.null(test)){
    xyplot <- xyplot +
      geom_point(data = test, colour = "black", pch = 4, size = 2)
  }
  xyplot
}

# pred_plot(pred_grid(train, step = 5, center = 2.5)) %>%
#   ggplot() +
#   geom_rect(aes(xmin = Længde_0, xmax = Længde_1, ymin = Vægt_0, ymax = Vægt_1, fill = Prediction),
#             color = "black", alpha = 0.3, size = 0.3) +
#   scale_fill_manual(values = type_cols) + scale_color_manual(values = type_cols) +
#   geom_point(data = train, aes(y = Vægt, x = Længde, colour = Type)) +
#   coord_equal() + theme(legend.position = "top")

# fitting <- tibble(step_size = c(20, 10, 5, 2.5, 1), shift = step_size/2) %>%
#   bind_rows(mutate(., shift = 0))
#
# fitting <- fitting %>%
#   rowwise() %>%
#   mutate(
#     pred = list(pred_plot(pred_grid(train, step = step_size, center = shift)))
#   )
#
# fitting %>% unnest(cols = pred) %>%
#   mutate(shift_text = ifelse(shift == 0, "Origo", "Mid point")) %>%
#   ggplot() +
#   geom_rect(aes(xmin = Længde_0, xmax = Længde_1, ymin = Vægt_0, ymax = Vægt_1, fill = Prediction),
#             alpha = 0.3) + # color = "#444444", size = 0.3 +
#   scale_fill_manual(values = type_cols) + scale_color_manual(values = type_cols) +
#   geom_point(data = train, aes(y = Vægt, x = Længde, colour = Type)) +
#   # coord_equal() +
#   theme(legend.position = "top") +
#   facet_grid(shift_text ~ step_size, scales = "free", labeller = label_both)
#
