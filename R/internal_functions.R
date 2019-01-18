### This function creates consistent, userfriendly error messages.

.errmsg <- function(...) {
  
  pkgname <- 'transformer';
  
  args <- list(...)
  
  ### If `missing` is provided, a required argument is missing.
  if ('missing' %in% names(args)) {
    errorMsg <-
      paste0("Argument '",
             args$missing,
             "' is missing, but must be provided!")
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
  
  return(paste0(errorMsg,
                " You can get more details by typing:\n\n  ?",
                pkgname,
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
