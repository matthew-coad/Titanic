
# Installing lots of R libraries is a slightly fraught process.

# The most reliable process I've found is to:
# 1) Source this script
# 2) Run the following command to copy the install script to the clipboard

# writeClipboard(titanic_install_libraries_script(titantic_missing_libraries()))

# 3) Restart R
# 4) Paste the clipboard into the console and run

# List of libraries required by the Titantic project that are missing
titantic_missing_libraries <- function() {
    rendering_libraries <- c("caTools", "bitops")
    runtime_libraries <- c("tidyverse", "rlang", "caret","recipes","doParallel")
    algorithm_libraries <- c("ada", "plyr", "Cubist", "earth", "elasticnet", "glmnet", "Matrix", 
                             "lars", "MASS", "RWeka", "klaR", "nnet", "pls", "stepPlr", "randomForest", 
                             "rpart", "kernlab", "ipred", "e1071", "xgboost")
    required_libraries <- c(rendering_libraries, runtime_libraries, algorithm_libraries)
    installed_libraries  <- installed.packages()[,"Package"]
    missing_libraries <- required_libraries[!required_libraries %in% installed_libraries]
    missing_libraries
}

titanic_install_libraries_script <- function(libraries) {
    if (length(libraries) == 0) {
        message("Required packages are already installed")
        return (invisible())
    }
    sprintf('install.packages(c("%s"))', paste(libraries, collapse = '", "'))
}


