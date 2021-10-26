library(activelearning)
library(dplyr)
library(caret)
library(ggplot2)
library(sits)



#---- Load code & data ----

# Load functions.
source(system.file("extdata", "compare_methods", "compare_al_methods.R",
                   package = "activelearning"))

samples_file <- "/home/alber.sanchez/Documents/github/bdc-classifications/bdc_cerrado_tc/data/samples/ts_claudio_fase_2_with_ecoregion.rds"
stopifnot(file.exists(samples_file))

ecoregions_shp <- "~/Documents/github/bdc-classifications/bdc_cerrado_tc/data/ecoregions/ECORREGIOES_CERRADO_V7.shp"
stopifnot(file.exists(ecoregions_shp))

# Get samples of deforestation.
samples_tb <-  samples_file %>%
    readRDS() %>%
    dplyr::group_by(label) %>%
    dplyr::slice_sample(prop = 0.20) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sample_id = dplyr::row_number()) %>%
    magrittr::set_class(class(sits::cerrado_2classes))
stopifnot(nrow(samples_tb) > 0)



#---- Pre-process the samples ----

# Get ecoregion for the samples without one.
ecoregion_sf <- ecoregions_shp %>%
    sf::st_read(quiet = TRUE)

missing_eco <- samples_tb %>%
    dplyr::filter(is.na(nome_eco)) %>%
    dplyr::select(longitude, latitude, sample_id) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326) %>%
    sf::st_transform(crs = sf::st_crs(ecoregion_sf))

missing_eco["nome_eco_new"] <- vapply(seq_len(nrow(missing_eco)),
                                      FUN = function(x) {
                                          ecoregion_sf[which.min(
                                              sf::st_distance(ecoregion_sf,
                                                              missing_eco[x,])
                                          ),][["NOME"]]
                                      },
                                      FUN.VALUE = character(1))

missing_eco <- missing_eco %>%
    sf::st_drop_geometry()

samples_tb <- samples_tb %>%
    dplyr::left_join(missing_eco,
                     by = "sample_id") %>%
    dplyr::mutate(nome_eco = dplyr::if_else(is.na(nome_eco),
                                            nome_eco_new,
                                            nome_eco)) %>%
    dplyr::select(-nome_eco_new)

# Merge eco-regions with few samples to their neighbors.
samples_tb %>%
    dplyr::count(label)
samples_tb %>%
    dplyr::count(nome_eco)

samples_tb <- samples_tb %>%
    dplyr::mutate(my_group = dplyr::recode(
        nome_eco,
        "Bananal"            = "Araguaia Tocantins",
        "Bico do Papagaio"   = "Araguaia Tocantins",
        "Complexo Bodoquena" = "Paraná Guimarães",
        "Depressão Cárstica do São Francisco" = "Chapadão do São Francisco",
        "Depressão Cuiabana" = "Paraná Guimarães",
        "Jequitinhonha"      = "Paracatu",
        "Parnaguá"           = "Chapadão do São Francisco",
        "Vão do Paranã"      = "Chapadão do São Francisco")
    )

samples_tb %>%
    dplyr::count(label)
samples_tb %>%
    dplyr::count(my_group)



#---- Outputs ----

results_dir <- "~/Documents/github/activelearning/inst/extdata/compare_methods_results"


#---- Set up ----

sits_method <- sits::sits_xgboost(verbose = FALSE)

# Initial number of labels to be taken from each label.
n_labelled   <- 3

# Number of labels to be selected on each iteration.
n_samples    <- length(unique(samples_tb$label)) * n_labelled

# Number of consecutive iterations.
n_iterations <- 30

# Number of times the experiment is repeated.
n_experiments <- 30



#---- Run experiment ----

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
            f_new_samples = new_samples_egal,
            sim_method = "dtw_basic"
        )},
    samples_tb = samples_tb,
    sits_method = sits_method,
    n_iterations = n_iterations,
    n_samples = n_samples
)

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
        file = file.path(results_dir, "results_cerrado.rds"))

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
        file = file.path(results_dir, "plot_data_cerrado.rds"))

plot_f1 <- plot_data %>%
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
    plot = plot_f1,
    path = results_dir,
    filename = "compare_cerrado.png",
    device = "png",
    width = 297,
    height = 210,
    units = "mm")

plot_mean_overall <- plot_data %>%
    dplyr::filter(metric %in% "accuracy") %>%
    dplyr::group_by(type, n_training, metric) %>%
    dplyr::summarize(mean_acc = mean(accuracy)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = n_training,
                                    y = mean_acc,
                                    color = type)) +
    ggplot2::labs(title = "Active learning accuracy",
                  subtitle = paste(
                      "Amazonia samples",
                      paste(n_experiments, " runs"),
                      paste(n_iterations, " iterations"),
                      sep = " - "
                  )) +
    ggplot2::xlab("Number of training samples") +
    ggplot2::ylab("Mean overall accuracy")

ggplot2::ggsave(
    plot = plot_mean_overall,
    path = results_dir,
    filename = "compare_cerrado_mean_overall.png",
    device = "png",
    width = 297,
    height = 210,
    units = "mm")

plot_mean_pu <- plot_data %>%
    dplyr::filter(metric %in% c("f1", "prod_acc", "user_acc")) %>%
    dplyr::group_by(type, n_training, metric, class) %>%
    dplyr::summarize(mean_acc = mean(accuracy)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = n_training,
                                    y = mean_acc,
                                    group = class,
                                    color = class)) +
    ggplot2::facet_grid(cols = dplyr::vars(type),
                        rows = dplyr::vars(metric)) +
    ggplot2::labs(title = "Active learning accuracy",
                  subtitle = paste(
                      "Amazonia samples",
                      paste(n_experiments, " runs"),
                      paste(n_iterations, " iterations"),
                      sep = " - "
                  )) +
    ggplot2::xlab("Number of training samples") +
    ggplot2::ylab("Mean accuracy")

ggplot2::ggsave(
    plot = plot_mean_pu,
    path = results_dir,
    filename = "compare_cerrado_mean_pu.png",
    device = "png",
    width = 297,
    height = 210,
    units = "mm")
