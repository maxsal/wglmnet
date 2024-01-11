#' Select plausible values for mtry to consider
#' @param p number of exposures to consider.
#'
#' @return a vector of values of mtry to consider
#' @export
.mtry_grid_calc <- function(p) {
  out <- c(floor(p/3), floor(sqrt(p)), floor(sqrt(p)*2), floor(sqrt(p)*0.5))
  out[out == 0] <- 1
  unique(sort(out))
}

#' Select plausible values for mtry to consider
#' @param data dataset
#' @param exposures a vector containing the names of the exposure variables in the data
#' @param outcome the name of the outcome variable in the data
#' @param weights the name of the weights variable in the data
#' @param mtry_grid vector of values of mtry to consider. default is c(floor(p/3), floor(sqrt(p)), floor(sqrt(p)*2), floor(sqrt(p)*0.5)) where p is the number of exposures.
#' @param min.node.size_grid vector of values of min.node.size to consider. default is 1, 3, 5, and 10
#' @param num_trees number of trees to build. default is 500.
#' @param splitrule name of splitrule to employ. default is 'gini', which is ranger default for classification and probability programs. consider 'variance' for regression problems.
#' @param n_folds number of CV folds
#' @param full_results return full results from caret::train(). default is FALSE
#' @param include_training_data include training data in output if full_results = TRUE
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom caret trainControl
#' @importFrom caret  train
#' @importFrom tidyselect any_of
#'
#' @return a list. either bestTune parameters (default) or output from caret::train()
#' @export
tune_ranger <- function(
    data,
    exposures,
    outcome,
    weights,
    mtry_grid             = NULL,
    min.node.size_grid    = c(1, 3, 5, 10),
    num_trees             = 500,
    splitrule             = "gini",
    n_folds               = 5,
    full_results          = FALSE,
    include_training_data = FALSE
) {
  data <- data |> dplyr::filter(!is.na(weights))
  if (is.null(mtry_grid)) {
    mtry_grid <- wglmnet::.mtry_grid_calc(ncol(data |> dplyr::select(tidyselect::any_of(exposures))))
  }
  grid <- expand.grid(
    mtry          = mtry_grid,
    min.node.size = min.node.size_grid,
    splitrule     = splitrule
  )
  fitControl <- caret::trainControl(
    method = "CV",
    number = n_folds
  )
  fit <- caret::train(
    x = x,
    y = y,
    method    = 'ranger',
    num.trees = num_trees,
    tuneGrid  = grid,
    trControl = fitControl
  )

  if (!full_results) {
    return(list("bestTune" = fit$bestTune))
  } else {
    if (include_training_data) {
      return(fit)
    } else {
      fit$trainingData <- NULL
      return(fit)
    }
  }

}
