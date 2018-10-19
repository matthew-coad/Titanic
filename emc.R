library(tidyverse)
library(rlang)

## -----------------------------------
## Exploration functions
## -----------------------------------

emc_data_statistics <- function(.x) {
    rows <- nrow(.x)
    cols <- ncol(.x)
    tibble(rows = rows, cols = cols)
}

emc_variable_distribution <- function(.x) {
    
    data <- .x
    var_names <- colnames(data)
    classes <- data %>% map(class)
    types <- classes %>% map_chr(~first(.))
    idxs <- 1:length(var_names)
    count <- rep(TRUE, length(var_names))
    distribution <- classes %>% map_lgl(~"integer" %in% . || "numeric" %in% .)
    
    mins <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) min(data[[idx]], na.rm = TRUE) else NA})
    means <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) mean(data[[idx]], na.rm = TRUE) else NA})
    medians <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) median(data[[idx]], na.rm = TRUE) else NA})
    maxes <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) max(data[[idx]], na.rm = TRUE) else NA})
    firstQ <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) quantile(data[[idx]], probs = c(0.25), na.rm = TRUE) else NA})
    thirdQ <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) quantile(data[[idx]], probs = c(0.75), na.rm = TRUE) else NA})
    sds <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) sd(data[[idx]], na.rm = TRUE) else NA})
    skews <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) e1071::skewness(data[[idx]], na.rm = TRUE) else NA})
    
    tibble(variable = var_names, type = types, mean = means, min = mins, firstQ = firstQ, median = medians, thirdQ = thirdQ, max = maxes, sd = sds, skew = skews)
}

emc_variable_degeneracy <- function(.x) {
    
    data <- .x
    var_names <- colnames(data)
    classes <- data %>% map(class)
    types <- classes %>% map_chr(~first(.))
    
    continuous <- classes %>% map_lgl(~"numeric" %in% .)
    discrete <- classes %>% map_lgl(~"integer" %in% .)
    quantative <- classes %>% map_lgl(~"numeric" %in% . || "integer" %in% .)
    categorical <- classes %>% map_lgl(~"factor" %in% .)
    qualitative <- classes %>% map_lgl(~"factor" %in% .)
    two_class <- classes %>% map_lgl(~"factor" %in% . && length(levels(.)) == 2)
    multi_class <- classes %>% map_lgl(~"factor" %in% . && length(levels(.)) > 2)
    
    idxs <- 1:length(var_names)
    count <- rep(TRUE, length(var_names))
    classes <- categorical | discrete
    distribution <- continuous | discrete
    vals <- idxs %>% map_int(function(idx) { if (count[idx]) sum(!is.na(data[[idx]])) else NA})
    nas <- idxs %>% map_int(function(idx) { if (count[idx]) sum(is.na(data[[idx]])) else NA})
    zeros <- idxs %>% map_int(function(idx) { if (count[idx]) sum(!is.na(data[[idx]]) & data[[idx]] == 0) else NA})
    uniqs <- idxs %>% map_int(function(idx) { if (classes[idx]) length(unique(data[[idx]])) else NA})
    vars <- idxs %>% map_dbl(function (idx) { if (distribution[idx]) var(data[[idx]]) else NA})
    
    nearZeroIdx <- caret::nearZeroVar(data)
    nearZeroVar <- if (!is_empty(nearZeroIdx)) idxs %in% nearZeroIdx else rep(FALSE, length(var_names))
    
    tibble(variable = var_names, type = types, vals = vals, nas = nas, zero = zeros, unique = uniqs, variance = vars, nearZeroVar = nearZeroVar)
}

emc_level_statistics <- function(.x) {
    
    data <- .x
    variable_names <- colnames(data)
    classes <- data %>% map(class)
    types <- classes %>% map_chr(~first(.))
    factor_variables <- classes %>% map_lgl(~"factor" %in% .)
    
    level_statistics <- function(i) {
        x <- data[[i]]
        x_levels <- levels(x)
        variable_name <- variable_names[[i]]
        
        counts <- x_levels %>% map_int(~sum(is.finite(x) & x == .))
        freqs <- x_levels %>% map_dbl(~sum(is.finite(x) & x == .) / length(x))
        nacount <- sum(is.na(x))
        nafreq <- sum(is.na(x)) / length(x)
        
        statistics <- tibble(variable = variable_name, level = x_levels, count = counts, freqs = freqs)
        statistics <- add_row(statistics, variable = variable_name, level = ".NA", count = nacount, freqs = nafreq)
        statistics
    }
    
    which(factor_variables) %>% purrr::map_df(level_statistics)
}

## -----------------------------------
## Algorithms
## -----------------------------------


# Get a data frame listing all caret algorithms
emc_algorithms <- function(uninstalled = FALSE) {
    
    core_algorithms <- 
        c("lm", "glm", "Penalized Linear Model" = "glmnet", "pls", "pcr", "enet", "lars", "lda", "plr", "polr", 
          "nnet", "earth", "svmLinear", "svmPoly", "svmRadial", "knn", "qda", "nb", 
          "rpart", "M5", "M5Rules",  "treebag", "rf", "cubist", "ada", "eXtreme Gradient Boosting - Linear" = "xgbLinear", "eXtreme Gradient Boosting - Tree" = "xgbTree",
          "null", "OneR")
    
    core_names <- names(core_algorithms) %>% purrr::keep(~ . != "")
    label_overrides <- core_names %>% set_names(core_names %>% purrr::map(~ core_algorithms[.]))
    overridden_label <- function(algorithm, label) { 
        if_else(!is.na(label_overrides[algorithm]),label_overrides[algorithm],label) %>% set_names(NULL)
    }
    
    installed_packages <- installed.packages() %>% as_tibble() %>% pull(Package) 
    model_installed <- function(info) {
        sum(info$library %in% installed_packages) == length(info$library)
    }
    
    infos <- caret::getModelInfo()
    algorithms <- names(infos)
    installed <- infos %>% purrr::map_lgl(model_installed)
    
    labels <- infos %>% purrr::map_chr("label")
    overridden_labels <- purrr::map2_chr(algorithms, labels, overridden_label)
    regression <- infos %>% purrr::map_lgl(~ "Regression" %in% .$type)
    classification <- infos %>% purrr::map_lgl(~ "Classification" %in% .$type)
    core <- algorithms %in% core_algorithms
    linear <- infos %>% purrr::map_lgl(~ "Linear Regression" %in% .$tags || "Linear Classifier" %in% .$tags)
    two_class_only <- infos %>% purrr::map_lgl(~ "Two Class Only" %in% .$tags)
    two_class <- classification
    multi_class <- classification & !two_class_only
    handle_missing <- infos %>% purrr::map_lgl(~ "Handle Missing Predictor Data" %in% .$tags)
    tree_based <- infos %>% purrr::map_lgl(~ "Tree-Based Model" %in% .$tags)
    rule_based <- infos %>% purrr::map_lgl(~ "Rule-Based Model" %in% .$tags)
    support_vector_machine <- infos %>% purrr::map_lgl(~ "Support Vector Machines" %in% .$tags)
    neural_network <- infos %>% purrr::map_lgl(~ "Neural Network" %in% .$tags)
    case_weights <- infos %>% purrr::map_lgl(~ "Accepts Case Weights" %in% .$tags)
    feature_selection <- infos %>% purrr::map_lgl(~ "Implicit Feature Selection" %in% .$tags)
    
    boosting <- infos %>% purrr::map_lgl(~ "Boosting" %in% .$tags)
    ensemble <- infos %>% purrr::map_lgl(~ "Ensemble Model" %in% .$tags)
    bagging <- infos %>% purrr::map_lgl(~ "Bagging" %in% .$tags)
    generalized_linear_model <- infos %>% purrr::map_lgl(~ "Generalized Linear Model" %in% .$tags)
    ordinal_outcomes <- infos %>% purrr::map_lgl(~ "Ordinal Outcomes" %in% .$tags)
    
    mars <- infos %>% purrr::map_lgl(~ "Multivariate Adaptive Regression Splines" %in% .$tags)
    discriminant_analysis  <- infos %>% purrr::map_lgl(~ "Discriminant Analysis" %in% .$tags)
    model_tree  <- infos %>% purrr::map_lgl(~ "Model Tree" %in% .$tags)
    bayesian_model <- infos %>% purrr::map_lgl(~ "Bayesian Model" %in% .$tags)
    logistic_regression <- infos %>% purrr::map_lgl(~ "Logistic Regression" %in% .$tags)
    L1_regularization <- infos %>% purrr::map_lgl(~ "L1 Regularization" %in% .$tags)
    L2_regularization <- infos %>% purrr::map_lgl(~ "L2 Regularization" %in% .$tags)
    
    robust_model <- infos %>% purrr::map_lgl(~ "Robust Model" %in% .$tags)
    partial_least_squares <- infos %>% purrr::map_lgl(~ "Partial Least Squares" %in% .$tags)
    random_forest <- infos %>% purrr::map_lgl(~ "Random Forest" %in% .$tags)
    gaussian_process <- infos %>% purrr::map_lgl(~ "Gaussian Process" %in% .$tags)
    
    quick <- !(boosting | ensemble | bagging | model_tree | neural_network | support_vector_machine | gaussian_process)
    
    known_tags <- c("Linear Regression", "Linear Classifier", "Two Class Only", 
                    "Handle Missing Predictor Data",  "Accepts Case Weights", "Implicit Feature Selection",
                    "Tree-Based Model", "Rule-Based Model", "Support Vector Machines", "Neural Network", "Boosting",
                    "Ensemble Model", "Generalized Linear Model", "Multivariate Adaptive Regression Splines", "Discriminant Analysis",
                    "Model Tree", "Bayesian Model", "Logistic Regression", 
                    "L1 Regularization", "L2 Regularization", "Ordinal Outcomes", "Bagging", "Robust Model", "Partial Least Squares", "Random Forest",
                    "Gaussian Process")
    
    tags <- infos %>% purrr::map_chr(~ paste(.$tags[!.$tags %in% known_tags], collapse = ", "))
    
    r <- tibble(
        algorithm = algorithms,
        label = overridden_labels,
        installed = installed,
        regression = regression,
        classification = classification,
        two_class = two_class,
        multi_class = multi_class,
        core = core,
        quick = quick,
        ordinal_outcomes = ordinal_outcomes,
        feature_selection = feature_selection,
        case_weights  = case_weights,
        handle_missing = handle_missing,
        linear = linear,
        tree_based  = tree_based,
        rule_based = rule_based,
        boosting = boosting,
        ensemble = ensemble,
        bagging = bagging,
        L1_regularization = L1_regularization,
        L2_regularization  = L2_regularization,
        robust_model = robust_model,
        
        model_tree = model_tree,
        logistic_regression = logistic_regression,
        partial_least_squares = partial_least_squares,
        random_forest = random_forest,
        svm = support_vector_machine,
        neural_net = neural_network,
        discriminant_analysis = discriminant_analysis,
        bayesian_model = bayesian_model,
        generalized_linear_model = generalized_linear_model,
        mars = mars,
        gaussian_process = gaussian_process,
        tags = tags
    )
    
    if (!uninstalled) {
        r <- r %>% filter(installed)
        
    }
    r
}

emc_algorithm_tags <- function(algorithms) {
    infos <- caret::getModelInfo()
    info_names <- names(infos)
    info_tags <- function(info, name) {
        info$tags %>% purrr::map(~ list(algorithm = name, tag = .)) %>% bind_rows()
    }
    tags <- purrr::map2(infos, info_names, info_tags) %>% bind_rows()
    algorithms %>% select(algorithm) %>% inner_join(tags, by = "algorithm")
}

emc_algorithm_libraries <- function(algorithms, uninstalled_only = TRUE) {
    
    installed_packages <- installed.packages() %>% as_tibble() %>% select(Package) %>% .[[1]]
    
    algorithm_libraries <- function(code) {
        libraries <- caret::getModelInfo(code, regex = FALSE)[[1]]$library
    }
    libraries <- algorithms$algorithm %>% purrr::map(algorithm_libraries) %>% unlist() %>% unique()
    if (uninstalled_only) {
        libraries <- libraries[!libraries %in% installed_packages]
    }
    libraries
}

emc_install_algorithms_script <- function(algorithms, uninstalled_only = TRUE) {
    libraries <- emc_algorithm_libraries(algorithms, uninstalled_only = uninstalled_only)
    if (length(libraries) == 0) {
        stop("Found no packages to install")
    }
    sprintf('install.packages(c("%s"))', paste(libraries, collapse = '", "'))
}

emc_bind <- function(.x, ...) {
    stopifnot(is.data.frame(.x))
    quos <- enquos(...)
    
    eval_row_quo <- function(x, quo, idx) {
        value <- eval_tidy(quo, data = x[idx,])
        value
    }
    
    x <- .x
    idx <- seq2(1, nrow(x))
    for (quo in quos) {
        rows <- purrr::map(idx, eval_row_quo, x = x, quo = quo)
        quo_df <- bind_rows(!!! rows)
        x <- bind_cols(x, quo_df)
    }
    x
}

# Call a function, with the given parameters, recording all messages, warnings and errors along the way
# Optionally using a result cache
# Returns an evaluation object that contains the result, error, execution times and the replay log
# By default environments are executed in the globalenv to accidently prevent accidentlal use of closures which
# are not correctly hashed when using cachine
# All information needed to execute the function should be passed as parameters 
emc_record <- function(..., .label = NULL, .verbose = TRUE, .sep = "_", .cache_path = NULL) {
    
    quos <- enquos(..., .named = TRUE)
    quo_names <- names2(quos)
    label <- rlang::eval_tidy(enquo(.label), data = list(.arg_name = quo_name))
    cache_path <- rlang::eval_tidy(enquo(.cache_path), data = list(.arg_name = quo_name, .label = label))

    stopifnot(length(quos) == 1)
    
    quo <- quos[[1]]
    quo_name <- quo_names[[1]]
    
    if (!is.null(cache_path) && file.exists(cache_path)) {
        return (readRDS(cache_path))
    }
    
    result <- list(NULL)
    succeded <- FALSE
    started <- lubridate::now()
    handler <- evaluate::new_output_handler(
        value = function(v) {
            result <<- v
            succeded <<- TRUE
        }
    )
    
    if (!.verbose && !is.null(label)) {
        message(">>>> ", label, " <<<<")
    }
    
    eval_quo <- function() {

        if (!is.null(label)) {
            message(">>>> ", label, " <<<<")
        }
        
        result <- list(rlang::eval_tidy(quo))
        evaluate::flush_console()
        result
    }
    
    log <- evaluate::evaluate(eval_quo, output_handler = handler, stop_on_error = 1)
    finished <- lubridate::now()
    messages <- log %>% purrr::keep(evaluate::is.message) %>% length()
    if (!is.null(label)) {
        messages <- messages - 1
    }
    warnings <- log %>% purrr::keep(evaluate::is.warning) %>% length()
    errors <- log %>% purrr::keep(evaluate::is.error) %>% length()
    first_error <- log %>% purrr::keep(evaluate::is.error) %>% first()
    if (!is.null(first_error)) {
        error <- conditionMessage(first_error)
    }
    else {
        error <- ""
    }
    recording <- list()
    value_name <- function(base_name, value) {
        if (!is_empty(.sep)) {
            paste0(quo_name, .sep, base_name)
        }
        else {
            base_name
        }
    }
    recording[[quo_name]] <- result
    recording[[value_name("log")]] <- list(log)
    recording[[value_name("error")]] = error
    recording[[value_name("errors")]] = errors
    recording[[value_name("messages")]] = messages
    recording[[value_name("warnings")]] = warnings
    recording[[value_name("duration")]] = as.numeric(difftime(finished, started, units = "secs"))
    
    if (.verbose) {
        evaluate::replay(purrr::discard(log, evaluate::is.source))
    }
    final <- tibble(!!! recording)
    if (!is.null(cache_path)) {
        dir.create(dirname(cache_path), showWarnings = FALSE)
        saveRDS(final, cache_path)
    }
    final
}

emc_replay <- function(.log) {
    for (log in .log) {
        evaluate::replay(purrr::discard(log, evaluate::is.source))
    }
    invisible()
}

emc_performance <- function(.train) {
    
    train <- .train[[1]]
    if (is.null(train)) {
        return(tibble::tibble()[1,])
    }
    stopifnot("train" %in% class(train))
    
    caret_performance <- caret::getTrainPerf(train) %>% dplyr::select(-method)

    cols <- colnames(caret_performance)
    out_cols <- gsub("^Train", "", cols)
    
    vars <- purrr::map2(cols, out_cols, ~ list2(!! .y := sym(.x) ) ) %>% flatten()
    performance <- dplyr::rename(caret_performance, !!!vars )
    performance
}

emc_resamples <- function(.train) {
    
    train <- .train[[1]]
    if (is.null(train)) {
        return(tibble::tibble()[1,])
    }
    stopifnot("train" %in% class(train))
    resamples <- train$resample
    tibble(resamples = list(resamples))
}

emc_t_test <- function(.formula, .data, alternative = c("two.sided", "less", "greater"),
                       mu = 0, paired = FALSE, var.equal = FALSE,
                       conf.level = 0.95) {
    
    formula <- rlang::eval_tidy(enquo(.formula))
    data <- rlang::eval_tidy(enquo(.data))

    t_test <- t.test(formula = formula, data = data, alternative = alternative, mu = mu, paired = paired, var.equal = var.equal, conf.level = conf.level)

    p_value <- t_test$p.value
    estimates <- list2(!!! t_test$estimate)
    conf_int_low <- t_test$conf.int[1]
    conf_int_high <- t_test$conf.int[2]

    tibble(p_value = p_value, !!! estimates, mean_diff_low = conf_int_low, mean_diff_high = conf_int_high)
    
}

# emc_variable_importance <- function(.train) {
#     if (is.null(.train)) {
#         return (NULL)
#     }
#     importance <- caret::varImp(.train)
#     importance <- importance$importance %>% tibble::rownames_to_column(var = "Variable") %>% as_tibble()
#     tibble(importance = list(importance))
# }

# best_algorithm <- age_emc %>% filter(errors == 0, warnings == 0) %>% arrange(metric) %>% pull(algorithm) %>% first()
# 
