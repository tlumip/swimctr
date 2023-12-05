#' Read the runtime parameters file and store them in environment
#'
#' @param properties_FN Filename containing the runtime parameters, with full
#'   path information included in the filename
#' @param file_format The format the runtime parameters are stored in within the
#'   file, which includes 'simple' (the default, and currently only supported
#'   format) and hypertext formats in the future
#' @param delimiter The character used to separate the parameter name from its
#'   value (defaults to '=', not relevant for XML format)
#'
#' @details This function reads a file containing the runtime parameters
#'   required by the CT platform. The variables required depend somewhat upon
#'   the exact model implementation (i.e., what CT functions are called, using
#'   what methods, and in what order). These should be specified by the model
#'   developer. Note that it is possible to call this function multiple times in
#'   order to set default parameters the first time through, and scenario- or
#'   run-specific parameters later. In that case, earlier parameters are
#'   overwritten if also defined in files read later. This enables the latter to
#'   only contain parameter values that differ from one model run to the next,
#'   rather than repeating standard parameters that rarely change, if ever,
#'   between different simulations.
#'
#'   A token name can be included in one or more value fields by enclosing it
#'   between percent signs. For example, the `t.year` parameters could be
#'   included in the value `results_%t.year%.csv`. All of the token substitution
#'   is carried out before the token-value pairs are placed in global runtime
#'   environment.
#'
#' @returns This function creates a RTP environment that is subsequently accessed by
#'   all CT functions during a model run. The function returns a string that
#'   reports the tokens and their corresponding values. The user can ignore the
#'   results by calling the function with reading the results into a variable.
#'
#' @export
#' @examples
#' get_runtime_parameters("/Models/swim25/run81/t0/swim.properties")
#' first_params <- get_runtime_parameters(
#'   "/Models/swim25/run91/t25/swim.properties")


get_runtime_parameters <- function(properties_FN, file_format = "simple",
  delimiter = '=') {
  require(stringr)  # package prefix omitted in this function for brevity sake
  # Introduce yourself
  print(swimctr:::self_identify(match.call()), quote = FALSE)

  # Read the properties file
  if (file_format == "simple") {
    # The file has a single token-value pair per record separated by equals sign
    # Start by putting the contents into a tibble and separating the token (key)
    # from its value
    raw <- scan(file = properties_FN, what = "character", sep = '\n',
      strip.white = TRUE, comment.char = '#')  #, quote = "")
    params <- tibble(token = raw) %>%
      separate(token, c("token", "value"), sep = delimiter) %>%
      mutate(token = str_trim(token), value = str_trim(value))

    # Substitute token values that appear in the value field. A substitutable
    # token will have percent signs on either side of it when it appears in the
    # value field.
    all_tokens <- paste0('%', params$token, '%')
    for (this_token in all_tokens) {
      update <- params$value[params$token == gsub('%', '', this_token)]
      params$value <- str_replace_all(params$value, this_token, update)
    }

    # Determine whether the value is a folder, file, or literal value
    params <- mutate(params, status = case_when(dir.exists(value) ~ "dir",
      file.exists(value) ~ "file", TRUE ~ "literal"))

    # Ensure that output files are not read even though they exists.
    params <- mutate(params, status = ifelse(str_detect(token, "ct.truck.trips") |
        str_detect(token, "et.truck.trips"),
        "literal", status))

    # Zipped matrix (.zmx) or OMX (.omx) files are a special case that we don't
    # want to read into our environment. We'll just pass this filename along so
    # flag it as a literal string
    params <- mutate(params,
      status = ifelse(str_detect(value, ".zmx") | str_detect(value, ".omx"),
        "literal", status))

  } else {
    # The user asked for YAML, JSON, or something similar that I'd love to use
    # in the future but haven't gotten to yet
    stop(paste("The properties file format ", file_format, "not supported"))
  }

  # Since we might be reading a system-wide properties files with tokens for
  # modules other than CT we need to remove those that are not relevant to us.
  # We might otherwise be reading a *lot* of files we have no need for, creating
  # a huge environment.
  n_original <- nrow(params)
  params <- params %>%
    mutate(status = case_when(
      token %in% c("root.dir", "repo.dir", "alpha2beta.file", "base.year",
        "current.year", "scenario.outputs", "pecas.makeuse", "t.year",
        "pecas.zonal.employment", "highway.assign.previous.skim.path", "et.truck.trips") ~ status,
      substr(token, 1, 3) == "ct." ~ status,
      substr(token, 1, 4) == "cvs." ~ status,
      substr(token, 1, 4) == "faf." ~ status, TRUE ~ "drop")) %>%
    filter(status != "drop")
  print(paste(nrow(params), "of", n_original,
    "properties appear applicable to CT"), quote = FALSE)

  # Now that we've processed the properties file we will create the runtime
  # properties (RTP) container and populate it with the tokens and their values.
  # Add to rather than overwrite the environment if it already exists:
  if (!exists("RTP")) RTP <<- new.env()

  # Next add each token and its value to the environment. If the value points to
  # a file we will read its contents into the environment.
  params$outcome <- NA_character_  # We will fill this in on the fly
  for (i in 1:nrow(params)) {
    if (params$status[i] == "file") {
      # Read the contents of the file into the environment. At the present time
      # we can only handle CSV files.
      RTP[[params$token[i]]] <- readr::read_csv(params$value[i],
        guess_max = 1e6, progress = FALSE)
      rows <- nrow(RTP[[params$token[i]]])
      cols <- length(colnames(RTP[[params$token[i]]]))
      params$outcome[i] <- paste0(params$token[i], ' -> ', params$value[i],
        ": ", rows, " x ", cols, " tibble")
    } else {
      # We're assuming it is either a folder or literal
      RTP[[params$token[i]]] <- params$value[i]
      params$outcome[i] <- paste0(params$token[i], ': ', params$value[i])
    }
  }

  # We will return a list of the contents that the user can print in the run
  # log, as the log file might not yet exist when one or more runtime properties
  # files are first read
  results <- c(paste0("Runtime parameters read from ", properties_FN, ':'),
    params$outcome)
  print(results, quote = FALSE)
}
