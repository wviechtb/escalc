#' Options for the escalc package
#'
#' The `escalc::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `escalc::opts$set` to set options,
#' `escalc::opts$get` to get options, or `escalc::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `escalc` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `escalc::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = "\n"`. For
#'   `escalc::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `escalc::opts$set`, the name of the option to set.}
#'   \item{default}{For `escalc::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#' 
#'   \item{EFFECTSIZE_POINTESTIMATE_NAME_IN_DF}{The name of the column
#'   with the effect size values.}
#'   
#'   \item{EFFECTSIZE_VARIANCE_NAME_IN_DF}{The name of the column
#'   with the effect size variance.}
#'   
#'   \item{EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF}{The name of the column
#'   with the missing values.}
#'   
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default name for the point estmate
#' escalc::opts$get(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF);
#'
#' ### Set it to a custom version
#' escalc::opts$set(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = "value_i");
#'
#' ### Check that it worked
#' escalc::opts$get(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF);
#'
#' ### Reset this option to its default value
#' escalc::opts$reset(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF);
#'
#' ### Check that the reset worked, too
#' escalc::opts$get(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("escalc.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for escalc!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for escalc!");
  } else {
    return(getOption(paste0("escalc.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("escalc.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for escalc!");
    }
  }
}

opts$defaults <-
  list(EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = 'yi',
       EFFECTSIZE_VARIANCE_NAME_IN_DF = 'vi',
       EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF = 'na_reason'
  )

