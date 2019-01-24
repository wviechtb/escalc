### Internal variables to allow flexibility, should we need it in the future,
### yet retain consistency
.PACKAGENAME <- 'escalc';
.EFFECTSIZE_POINTESTIMATE_NAME_IN_DF <- 'yi';
.EFFECTSIZE_VARIANCE_NAME_IN_DF <- 'vi';

### This function creates consistent, userfriendly error messages.
.errmsg <- function(...) {

  args <- list(...)
  
  ### If `missing` is provided, a required argument is missing.
  if ('missing' %in% names(args)) {
    errorMsg <-
      paste0("Argument '",
             args$missing,
             "' is missing, but must be provided!")
  }

  ### If `differentLengths` is provided, argument lengths differ
  if ('differentLengths' %in% names(args)) {
    errorMsg <-
      paste0("The lengths of arguments ",
             .vecTxtQ(args$differentLengths$argNames),
             " must all be the same, but are ",
             .vecTxt(args$differentLengths$argLengths),
             ".")
  }
  
  ### If `conditionalMissing` is provided, one or more required arguments
  ### were provided, but one or more additional arguments that is/are required
  ### because of those arguments that were provided, is or are missing.
  if ('conditionalMissing' %in% names(args)) {
    errorMsg <-
      paste0("If you provide ",
             ifelse(length(args$conditionalMissing$provided) == 1,
                    "argument ",
                    "arguments "),
             .vecTxtQ(args$conditionalMissing$provided),
             " you must also provide ",
             .conditionalMissingList(args$conditionalMissing$missing),
             ".")
  }

  ### If `argumentRedundancy` is provided, conflicting arguments are provided
  if ('argumentRedundancy' %in% names(args)) {
    errorMsg <-
      paste0("You provided conflicting arguments, specifically, both ",
             .vecTxtQ(args$argumentRedundancy$argNames1),
             " *and* ",
             .vecTxtQ(args$argumentRedundancy$argNames2),
             ". However, these are mutually exclusive; specify *either* ",
             .vecTxtQ(args$argumentRedundancy$argNames1),
             " *or* ",
             .vecTxtQ(args$argumentRedundancy$argNames2),
             ", but not all together!")
  }
  
  ### If `wrongType` is provided, an argument has the wrong type.
  if ('wrongType' %in% names(args)) {
    errorMsg <-
      paste0("You provided an object of class '",
             args$wrongType$providedType,
             "' as argument '",
             args$wrongType$argName,
             "', but an object of class '",
             args$wrongType$requiredType,
             "' is required.")
  }

  ### If `cantBeNullOrNA` is provided, an argument is NULL or NA but shouldn't be  
  if ('cantBeNullOrNA' %in% names(args)) {
    errorMsg <-
      paste0("For argument '",
             args$cantBeNullOrNA$argName,
             "', you provided ",
             ifelse(is.null(args$cantBeNullOrNA$argVal),
                    'NULL',
                    'NA'),
             ", but this is not allowed.")
  }

  ### If `invalidValue` is provided, an argument has an invalid value.
  if ('invalidValue' %in% names(args)) {
    errorMsg <-
      paste0("As value of argument '",
             args$invalidValue$argName,
             "' you provided  '",
             args$invalidValue$argVal,
             "', but that is an invalid value. ",
             args$invalidValue$argName,
             " has to be ",
             args$invalidValue$validValues,
             ".")
  }

  ### If `invalidValueCombo` is provided, two or more arguments combine
  ### in a way that results in an invalid value.
  if ('invalidValueCombo' %in% names(args)) {
    errorMsg <-
      paste0("As value of arguments ",
             .vecTxtQ(args$invalidValueCombo$argName),
             " you provided values ",
             .vecTxtQ(args$invalidValueCombo$argVal),
             ", respectively. However, these values combined ",
             "in an invalid way. Specifically, ",
             args$invalidValueCombo$validValues,
             ".")
  }
  
  return(paste0(errorMsg,
                " You can get more details by typing:\n\n  ?",
                .PACKAGENAME,
                "::",
                args$callingFunction))

}

.conditionalMissingList <- function(x) {
  if (is.list(x)) {
    return(paste0("either ", paste0(unlist(lapply(x, .vecTxtQ)), collapse=" or ")))
  } else {
    return(paste0(.vecTxtQ(x)))
  }
}

.functionalityNotImplementedMsg <- function(...) {

  args <- list(...)
  
  if (args$reason == "nonexistent") {
    errorMsg <-
      paste0("I am sorry, but obtaining ",
             args$conversion,
             " is not yet possible - there exists no ",
             "methodologically and statistically correct ",
             "procedure for this conversion.")
  }
  
  if (args$reason == "notyet") {
    errorMsg <-
      paste0("I am sorry, but obtaining ",
             args$conversion,
             " is not yet possible - this functionality  ",
             " has not yet been implemented.")
  }

  return(paste0(errorMsg,
                " You can get more details by typing:\n\n  ?",
                .PACKAGENAME,
                "::",
                args$callingFunction))
}


.vecTxt <- function(vector, delimiter = ", ", useQuote = "",
                   firstDelimiter = NULL, lastDelimiter = " & ",
                   firstElements = 0, lastElements = 1,
                   lastHasPrecedence = TRUE) {
  
  vector <- paste0(useQuote, vector, useQuote);
  
  if (length(vector) == 1) {
    return(vector);
  }
  
  if (firstElements + lastElements > length(vector)) {
    if (lastHasPrecedence) {
      firstElements <- length(vector) - lastElements;
    } else {
      lastElements <- length(vector) - firstElements;
    }
  }
  
  firstTxt <- lastTxt <- "";
  
  if (is.null(firstDelimiter)) {
    firstDelimiter <- delimiter;
  }
  if (is.null(lastDelimiter)) {
    lastDelimiter <- delimiter;
  }
  
  midBit <- vector;
  if (firstElements > 0) {
    firstBit <- utils::head(vector, firstElements);
    midBit <- utils::tail(vector, -firstElements);
    firstTxt <- paste0(paste0(firstBit, collapse=firstDelimiter),
                       firstDelimiter);
  }
  if (lastElements > 0) {
    lastBit <- utils::tail(vector, lastElements);
    midBit <- utils::head(midBit, -lastElements);
    lastTxt <- paste0(lastDelimiter, paste0(lastBit,
                                            collapse=lastDelimiter));
  }
  
  midTxt <- paste0(midBit, collapse=delimiter);
  
  return(paste0(firstTxt, midTxt, lastTxt));
  
}

.vecTxtQ <- function(vector, useQuote = "'", ...) {
  return(.vecTxt(vector, useQuote = useQuote, ...));
}

### Written by Nick Sabbe, http://stackoverflow.com/questions/7307987/logging-current-function-name
.curfnfinder <- function(skipframes=0,
                         skipnames="(FUN)|(.+apply)|(replicate)",
                         retIfNone="Not in function",
                         retStack=FALSE,
                         extraPrefPerLevel="\t") {
  prefix <- sapply(3 + skipframes+1:sys.nframe(), function(i) {
    currv<-sys.call(sys.parent(n=i))[[1]]
    return(currv)
  });
  prefix[grep(skipnames, prefix)] <- NULL;
  prefix <- gsub("function \\(.*", "do.call", prefix);
  if(length(prefix)==0) {
    return(retIfNone);
  } else if(retStack) {
    return(paste(rev(prefix), collapse = "|"));
  } else {
    res <- as.character(unlist(prefix[1]));
    res <- unlist(lapply(res, gsub, pattern=".*::", replacement=""));
    if (length(prefix) > 1) {
      res <- paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), res, sep="");
    }
    return(res);
  }
}
