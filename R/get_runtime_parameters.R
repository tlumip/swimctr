#' Read the runtime parameters file and store them in environment
#'
#' @param propertiesFN Filename containing the runtime parameters, with full
#'   path information included in the filename
#' @param format The format the runtime parameters are stored in within the
#'   file, which includes java (the default, and currently only supported
#'   format) and xml (future)
#' @param delimiter The character used to separate the parameter name from its
#'   value (defaults to '=', not relevant for XML format)
#' @param import_flag Some parameters are tables stored in external files in
#'   command separated value (CSV) format. This parameter, when it precedes the
#'   runtime parameter value, signals the function that the value string is
#'   a file that should should be read when adding that runtime parameter to the
#'   simulation environment. (default is '&')
#' @param echo_parameters Print the runtime parameters after setting them up
#'   (defaults to TRUE)
#' @param save_to Filename for saving the runtime parameters to R binary file
#'   for later processing or archiving (optional)
#'
#' @details This function reads a file containing the runtime parameters
#'   required by the CT platform. The variables required depend somewhat upon
#'   the exact model implementation (i.e., what CT functions are called, using
#'   what methods, and in what order). These should be specified by the model
#'   developer. Note that it is possible to call this function twice in order
#'   to set default parameters the first time through, and scenario-specific
#'   parameters the second time. In that case the default parameters are
#'   overwritten if also defined in the scenario-specific parameters file. This
#'   enables the latter to only contain parameter values that differ from one
#'   model run to the next, rather than repeating standard parameters that
#'   rarely change, if ever, between different simulations.
#'
#'   It is important to note that all of the parameters are treated as strings
#'   in plaintext (java) formatted files, whereas the class can be explicitly
#'   set in XML or JSON files. However, the latter are not yet implemented (and
#'   will not be until needed). Thus, it is incumbent upon the user to cast the
#'   runtime parameters to correct class before evaluating or manipulating them
#'   in their code.
#'
#' @export
#' @examples
#' get_runtime_parameters("/Models/swim25/run81/t0/swim.properties")
#' get_runtime_parameters("/Models/swim25/run91/t25/swim.properties",
#'   save_to = "ct_runtime_parameters.RData")

get_runtime_parameters <- function(propertiesFN, format = "java",
  delimiter = '=', import_flag = '&', echo_parameters = TRUE, save_to = NULL) {
  require(stringr)  # package prefix omitted in this function for brevity sake

  # How we will read the parameters file will depend upon how it is coded. For
  # now the only supported type is java.
  supported_formats <- c("java")
  if (!format %in% supported_formats) {
    error_message <- paste("Format", format, "is not supported")
    stop(error_message)
  }

  # Start with code for handling java format
  if (format == "java") {
    # Start by reading the contents of the runtime parameters file and removing
    # any embedded tab characters
    raw <- scan(file = propertiesFN, what = "character", sep = '\n',
      strip.white = TRUE, comment.char = '#')  #, quote = "")
    notabs <- str_replace(noquote(raw), '\t', "")
    noquotes <- str_replace_all(notabs, '\"', "") # Remove R's double quote

    # Split each record into token and value by using the delimiter argument to
    # split them. Start by extracting the token names. Remember that str_locate
    # returns two values: the first and last occurrences of the character in the
    # string. We only need the first value in this case.
    params <- dplyr::data_frame(token = str_trim(str_sub(noquotes, 1,
      str_locate(notabs, delimiter)[,1]-1)))

    # Next isolate the value associated with each token
    params$value <- str_trim(str_sub(noquotes,
      str_locate(noquotes, delimiter)[,1]+1, str_length(noquotes)))
    params$value <- str_replace_all(params$value, " ", "")  # Remove spaces

    # Finally, we need to determine whether the string we read for the value is
    # indeed what the user intended, or whether it is a pointer to a file that
    # should be read to obtain the contents. Thus, we look for a leading import
    # flag, which is specified in function list.
    params$flag <- ifelse(substr(params$value, 1, 1) == import_flag, "file",
      "literal")
    params$value <- ifelse(params$flag == "literal", params$value,
      substr(params$value, 2, str_length(params$value)))
    # End java format handling
  } else {
    error_message <- paste("Format", format, "not yet supported")
    stop(error_message)
  }

  # Create the runtime parameters (RTP) environment if it does not exist, and
  # add the current token-value pairs to it. This will overwrite existing pairs
  # if redefined in the current set.
  if (!exists("RTP")) RTP <<- new.env()

  # We need to set the root.dir first, because other parameters might depend
  # upon it to find location of the files if relative directory structure is
  # used. So we'll sequentially number the params records, set root.dir to zero,
  # and then re-sort the data frame accordingly.
  params$seq <- 1:nrow(params)
  params$seq <- ifelse(params$token == "root.dir", 0, params$seq)

  # If the user did not explicitly set root.dir then assume it is the current
  # directory. Again, it needs to be first, so put it there after defining it.
  all_tokens <- unique(params$token)
  if (!"root.dir" %in% all_tokens) {
    params <- dplyr::bind_rows(
      dplyr::data_frame(token = "root.dir", value = "./", flag = "literal",
        seq = 0),
      params)
    ct_msg(paste("Required parameter root.dir assumed to be", getwd()))
  }
  params <- dplyr::arrange(params, seq)

  # Set the individual runtime enviroment variables from this stack
  for (i in 1:nrow(params)) {
    if (params$flag[i] == "literal") {
      # If the value is literal then simply place it into the environment
      RTP[[params$token[i]]] <- params$value[i]
    } else {
      # But otherwise we will attempt to read the file as CSV format from the
      # current folder. If it is not found there then look for it in root.dir.
      # And if not there then stop the program, for the user has either not
      # set the required root.dir runtime parameter or mis-specified the file.
      filename <- params$value[i]
      if (!file.exists(filename)) {
        # If the file does not exist in the current directory then look for it
        # in the root.dir folder
        if (nchar(RTP[["root.dir"]])<1) {
          error_message <- paste("Somehow root.dir is not defined at seq=",
            params$seq[i])
          stop(error_message)
        } else {
          filename <- file.path(RTP[["root.dir"]], filename)
        }

        # If the file isn't there, either, then stop
        if (!file.exists(filename)) {
          error_message <- paste(params$value[i], "not found in either the",
            "current folder or in root.dir")
          stop(error_message)
        }
      }
      RTP[[params$token[i]]] <- readr::read_csv(filename)
    }
  }

  # Finally, echo the relevant parameters so that we know they were properly
  # set. However, we don't want to print a data frame associated with any given
  # token, so just print abbreviated summary. Finally, omit any parameters that
  # we don't care about.
  keep <- c("root.dir", "scenario.name", "base.year", "t.year", "t.year.prefix",
    "scenario.outputs", "alpha2beta.file", "highway.assign.previous.skim.path",
    "pecas.makeuse", "pecas.zonal.employment")

  # In addition, we want to save anything that has "ct." or "faf." prefix
  for (this_token in all_tokens) {
    # Find the first dot character in the token name
    first_dot <- str_locate(this_token, '\\.')[,1]
    prefix <- substr(this_token, 1, first_dot)
    if (prefix %in% c("ct.", "faf.")) keep <- c(keep, this_token)
  }
  keep <- sort(keep)

  # Finally, print the parameters unless the user has told us not to
  if (echo_parameters == TRUE) {
    ct_msg("Runtime parameters:")
    for (this_token in keep) {
      if (!this_token %in% all_tokens) next  # Skip kept tokens that don't exist
      if (is.data.frame(RTP[[this_token]])) {
        S <- paste(this_token, ": ", dim(params)[1], " x ", dim(params)[2],
          " tibble", sep = '')
      } else {
        S <- paste(this_token, ": ", RTP[[this_token]], sep = '')
      }
      ct_msg(S)
    }
  }

  # Finally, if the user has asked to save them put the parameters into a binary
  # data file in the root directory
  if (!is.null(save_to)) {
    filename <- file.path(RTP[["root.dir"]], "ct_runtime_parameters.RData")
    save(RTP, file = filename)
    ct_msg(paste("Runtime parameters saved to", filename))
  }

  # There is nothing to return, for the RTP environment is already populated
}
