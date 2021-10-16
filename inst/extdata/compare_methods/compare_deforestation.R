library(activelearning)
library(dplyr)
library(caret)
library(ggplot2)
library(sits)



#---- Load code & data ----

# Load functions.
source(system.file("extdata", "compare_methods", "compare_al_methods.R",
                   package = "activelearning"))

# Get samples of deforestation.
samples_tb <- system.file("extdata", "samples", "deforestation.rds",
                          package = "activelearning") %>%
    readRDS() %>%
    dplyr::group_by(label) %>%
    dplyr::slice_sample(prop = 0.2) %>%
    dplyr::ungroup() %>%
    magrittr::set_class(class(sits::cerrado_2classes))
stopifnot(nrow(samples_tb) > 0)



#---- Outputs ----

results_dir <- "~/Documents/github/activelearning/inst/extdata/compare_methods_results"


#---- Set up ----

sits_method <- sits::sits_xgboost(verbose = FALSE)

# Initial number of labels to be taken from each label.
n_labelled   <- 3

# Number of labels to be selected on each iteration.
n_samples    <- length(unique(samples_tb$label)) * 3

# Number of consecutive iterations.
n_iterations <- 3

# Number of times the experiment is repeated.
n_experiments <- 3



#---- Run experiment ----

res_no_al <- lapply(
    seq_len(n_experiments),
    FUN = function(x,
                   samples_tb,
                   sits_method,
                   n_iterations,
                   n_samples) {
        experiment(
            start_samples_tb = start_sample_set(samples_tb = samples_tb,
                                                n_samples = n_samples),
            sits_method = sits_method,
            n_iterations = n_iterations,
            n_samples = n_samples,
            f_new_samples = new_samples_no_al
        )},
    samples_tb = samples_tb,
    sits_method = sits_method,
    n_iterations = n_iterations,
    n_samples = n_samples
)

res_egal <- lapply(
    seq_len(n_experiments),
    FUN = function(x,
                   samples_tb,
                   sits_method,
                   n_iterations,
                   n_samples) {
        experiment(
            start_samples_tb = start_sample_set(samples_tb = samples_tb,
                                                n_samples = n_samples),
            sits_method = sits_method,
            n_iterations = n_iterations,
            n_samples = n_samples,
            f_new_samples = new_samples_egal
        )},
    samples_tb = samples_tb,
    sits_method = sits_method,
    n_iterations = n_iterations,
    n_samples = n_samples
)

res_al_rs <- lapply(
    seq_len(n_experiments),
    FUN = function(x,
                   samples_tb,
                   sits_method,
                   n_iterations,
                   n_samples) {
        experiment(
            start_samples_tb = start_sample_set(samples_tb = samples_tb,
                                                n_samples = n_samples),
            sits_method = sits_method,
            n_iterations = n_iterations,
            n_samples = n_samples,
            f_new_samples = new_samples_rs
        )},
    samples_tb = samples_tb,
    sits_method = sits_method,
    n_iterations = n_iterations,
    n_samples = n_samples
)

saveRDS(list(no_active_learning = res_no_al,
             egal = res_egal,
             al_rand_sampling = res_al_rs),
        file = file.path(results_dir, "results.rds"))

acc_no_al <- res_no_al %>%
    purrr::map(.f = function(x){
        return(x[["accuracy"]])
    }) %>%
    magrittr::set_names(seq_len(length(.))) %>%
    dplyr::bind_rows(.id = "experiment") %>%
    dplyr::mutate(type = "No Active Learning")

acc_egal <- res_egal %>%
    purrr::map(.f = function(x){
        return(x[["accuracy"]])
    }) %>%
    magrittr::set_names(seq_len(length(.))) %>%
    dplyr::bind_rows(.id = "experiment") %>%
    dplyr::mutate(type = "EGAL")

acc_rs <- res_al_rs %>%
    purrr::map(.f = function(x){
        return(x[["accuracy"]])
    }) %>%
    magrittr::set_names(seq_len(length(.))) %>%
    dplyr::bind_rows(.id = "experiment") %>%
    dplyr::mutate(type = "AL Random Sampling")

plot_data <-  dplyr::bind_rows(acc_no_al, acc_egal, acc_rs) %>%
    dplyr::mutate(experiment = as.integer(experiment)) %>%
    dplyr::select(type, experiment, iteration,
                  n_training, metric, class, accuracy)

saveRDS(plot_data,
        file = file.path(results_dir, "plot_data.rds"))

my_plot <- plot_data %>%
    dplyr::filter(metric == "f1") %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = n_training,
                                       y = accuracy,
                                       group = n_training)) +
    ggplot2::facet_grid(cols = dplyr::vars(type),
                        rows = dplyr::vars(class)) +
    ggplot2::labs(title = "Active learning accuracy",
                  subtitle = paste(
                      "Amazonia samples",
                      paste(n_experiments, " runs"),
                      paste(n_iterations, " iterations"),
                      sep = " - "
                  )) +
    ggplot2::xlab("Number of training samples") +
    ggplot2::ylab("F1 score")

ggplot2::ggsave(
    plot = my_plot,
    path = results_dir,
    filename = "compare_deforestation.png",
    device = "png",
    width = 297,
    height = 210,
    units = "mm")
