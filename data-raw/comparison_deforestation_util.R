#' Run an experiment.
#'
#' @param samples_tb    A sits tibble.
#' @param sits_method   A sits method.
#' @param n_iterations  A length-one integer. The number of iterations in the
#'                      experiment.
#' @param n_samples     A length-one integer. The number of new labelled samples
#'                      to take in each iteration.
#' @param f_new_samples A function for selecting the new labelled samples in
#'                      each iteration.
#' @return              A tibble with accuracy measuments.
experiment <- function(start_samples_tb, sits_method, n_iterations, n_samples,
                       f_new_samples, ...) {
  stopifnot(c("label", "oracle_label", "sample_id") %in%
              colnames(start_samples_tb))
  my_samples <- start_samples_tb
  accuracy_tb <- tibble::tibble()
  for (i in seq_len(n_iterations)) {
    classification_acc <- run_classification(my_samples,
                                             sits_method) %>%
      dplyr::mutate(iteration = i)
    accuracy_tb <- accuracy_tb %>%
      dplyr::bind_rows(classification_acc)
    if (i == n_iterations)
      break()
    new_sample_ids <- f_new_samples(my_samples = my_samples,
                                    n_samples = n_samples)
    my_samples <- my_samples %>%
      dplyr::mutate(label = dplyr::if_else(sample_id %in% new_sample_ids,
                                           oracle_label,
                                           label))
  }
  return(accuracy_tb)
}



#' Get the testing labels from a sits tibble.
#'
#' @param samples_tb A sits tibble.
#' @return           A sits tibble.
get_testing_samples <- function(my_samples) {
  #my_samples %>% count(label) %>% print(n = Inf)
  my_samples %>%
    dplyr::filter(is.na(label)) %>%
    ensurer::ensure_that(nrow(.) > 0,
                      err_desc = "No testing samples found! The available") %>%
    return()
}



#' Get the training labels from a sits tibble.
#'
#' @param samples_tb A sits tibble.
#' @return           A sits tibble.
get_training_samples <- function(my_samples) {

  my_samples %>%
    dplyr::filter(!is.na(label)) %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_desc = "No training samples found!") %>%
    return()
}



#' Get the sample ids of the samples for the next iteration using EGAL.
#'
#' @param my_samples A sits tibble.
#' @param n_samplse  An length-one integer. The number of new samples.
#' @return           An integer.
new_samples_egal <- function(my_samples, n_samples) {
  my_samples %>%
    al_egal() %>%
    dplyr::slice_max(egal,
                     n = n_samples,
                     with_ties = FALSE) %>%
    dplyr::pull(sample_id) %>%
    return()
}



#' Get the sample ids of the samples for the next iteration using random sampling
#' without Active Learning.
#'
#' @param my_samples A sits tibble.
#' @param n_samplse  An length-one integer. The number of new samples.
#' @return           An integer.
new_samples_no_al <- function(my_samples, n_samples) {
  my_samples %>%
    get_testing_samples() %>%
    dplyr::sample_n(size = n_samples) %>%
    dplyr::pull(sample_id) %>%
    return()
}



#' Get the sample ids of the samples for the next iteration using Active
#' Learning with random sampling.
#'
#' @param my_samples A sits tibble.
#' @param n_samplse  An length-one integer. The number of new samples.
#' @return           An integer.
new_samples_rs <- function(my_samples, n_samples) {
  my_samples %>%
    al_random_sampling(sits_method = sits_method,
                       multicore = parallel::detectCores()) %>%
    dplyr::slice_max(entropy,
                     n = n_samples,
                     with_ties = FALSE) %>%
    dplyr::pull(sample_id) %>%
    return()
}



#' Run a sits classification with the given sample points and return its
#' accuracy.
#'
#' @param samples_tb       A sits tibble. The rows with NA in the label column
#'                         will be given labels using a model trained with those
#'                         not-NA labels.
#' @param sits_method      A sits classification method.
#' @return                 A tibble with the accuracy of the classification.
run_classification <- function(samples_tb,
                               sits_method) {

  training_samples <- samples_tb %>%
    get_training_samples()

  reference_labels <- training_samples %>%
    dplyr::pull(label) %>%
    unique() %>%
    sort()

  classification_model  <- sits::sits_train(training_samples,
                                            ml_method = sits_method)

  prediction_fct <- samples_tb %>%
    sits::sits_classify(ml_model = classification_model) %>%
    dplyr::pull(predicted) %>%
    purrr::map_chr(magrittr::extract("class")) %>%
    factor(levels = reference_labels)

  reference_fct <- samples_tb %>%
    pull(oracle_label) %>%
    factor(levels = reference_labels)

  caret::confusionMatrix(data = prediction_fct,
                         reference = reference_fct) %>%
    .get_acc() %>%
    dplyr::mutate(n_training = nrow(training_samples)) %>%
    return()
}



#' Get an initial set of labelled samples.
#'
#' @param samples_tb A sits tibble.
#' @param n_samples  A length-one integer. The number of labelled samples.
#' @return           A sits tibble where n_samples are labelled and the
#'                   remaining are NA.
start_sample_set <- function(samples_tb, n_samples) {
  my_samples <- samples_tb %>%
    dplyr::mutate(sample_id = dplyr::row_number(),
                  oracle_label = label)

  initial_vec <- my_samples %>%
    dplyr::group_by(oracle_label) %>%
    dplyr::sample_n(size = n_samples) %>%
    dplyr::ungroup() %>%
    dplyr::pull(sample_id)

  my_samples %>%
    dplyr::mutate(label = dplyr::if_else(sample_id %in% initial_vec,
                                         oracle_label,
                                         NA_character_)) %>%
    return()
}



#---- Hidden ----



#' Get accuracy metrics from a caret's confusion matrix.
#'
#' @param conf_mat A confusion matrix objetc.
#' @return         A numeric.
.get_acc <- function(conf_mat) {
    overall_accuracy  <- conf_mat$overall["Accuracy"]
    by_class <- conf_mat[["byClass"]]
    f1_score <- by_class[, "F1"]
    prod_acc <- by_class[, "Pos Pred Value"]
    user_acc <- by_class[, "Sensitivity"]
    class_names <- stringr::str_sub(rownames(by_class), 8)

    tibble::tibble(class = class_names,
                   metric = "f1",
                   accuracy = f1_score) %>%
      dplyr::bind_rows(tibble::tibble(class = class_names,
                                      metric = "prod_acc",
                                      accuracy = prod_acc)) %>%
      dplyr::bind_rows(tibble::tibble(class = class_names,
                                      metric = "user_acc",
                                      accuracy = user_acc)) %>%
      dplyr::add_row(class = "overall",
                     metric = "accuracy",
                     accuracy = overall_accuracy) %>%
      return()
}
