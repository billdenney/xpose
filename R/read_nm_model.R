#' NONMEM model file parser
#'
#' @description Parse NONMEM model files in R format
#'
#' @inheritParams xpose_data
#' @inheritSection xpose_data File path generation
#'
#' @details A NONMEM model output file (i.e. .lst, .out, .res, .nmlog) should
#' preferably be provided to \code{read_nm_model} to allow for a more extensive
#' xpose summary. However in some cases these output files may not contain the
#' model code, thus preventing xpose from identifying the associated output
#' tables names. In such cases xpose will attempt to read the associated model
#' file (i.e. .mod, .ctl or .nmctl) instead to find the model code. Note: it is
#' important that between the naming convention between the NONMEM output and
#' the model file remains consistent e.g. run001.lst should be associated with
#' run001.mod.
#'
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @return A \code{\link[dplyr]{tibble}} of class \code{model} containing the
#'   following columns: \itemize{ \item{\strong{problem}}{: a numeric identifier
#'   for the $PROBLEM associated with the code.} \item{\strong{level}}{: a
#'   unique numeric identifier to each subroutine block associated with the
#'   code.} \item{\strong{subroutine}}{: a character identifier named after the
#'   3 first letters of the subroutine name e.g. '$THETA' and '$TABLE' will
#'   become 'the' and 'tab' respectively. In addition all output from the .lst
#'   is labeled 'lst', the general nonmem output e.g. NM-TRAN messages are
#'   labelled 'oth'. With priors thp, tpv, omp, opd, sip, spd abbreviations are
#'   given to the THETAP, THETAPV, OMEGAP, etc.} \item{\strong{code}}{: the code
#'   without comments or subroutine names e.g. '$THETA 0.5 ; TVCL' will return
#'   '0.5'.} \item{\strong{comment}}{: the last comment of a record e.g. '0.5 ;
#'   Clearance (L/h) ; TVCL' will return 'TVCL'.} }
#'
#' @examples
#' \dontrun{
#' # Using the `file` argument to import a model file:
#' nm_model <- read_nm_model(file = 'run001.lst', dir = 'models')
#'
#' # Using the `runno` argument to import a model file:
#' nm_model <- read_nm_model(runno = '001', ext = '.lst', dir = 'models')
#' }
#'
#' @export
read_nm_model <- function(runno   = NULL,
                          prefix  = 'run',
                          ext     = '.lst',
                          file    = NULL,
                          dir     = NULL) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (!is.null(runno)) {
    ext       <- make_extension(ext)
    full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext       <- get_extension(file)
    full_path <- file_path(dir, file)
  }
  
  if (!ext %in% c('.lst', '.out', '.res', '.mod', '.ctl', '.nmlog', '.nmctl')) {
    stop('NONMEM model file extension should be one lst, out, res, mod, ctl, nmlog or nmctl.', call. = FALSE) 
  }
  
  if (!file.exists(full_path)) { 
    stop('Model file ', basename(full_path), ' not found.', call. = FALSE) 
  }
  
  model_raw <- readr::read_lines(full_path)
  
  if (!any(stringr::str_detect(model_raw, '^\\s*\\$PROB.+')) && 
      ext %in% c('.lst', '.out', '.res', '.nmlog')) {
    
    # Attempts to recover the model code from model file rather than in the nonmem output file
    full_path <- update_extension(full_path, c('.mod', '.ctl', '.nmctl'))
    full_path <- full_path[file.exists(full_path)]
    
    if (any(file.exists(full_path))) {
      warning(c('No model code found in `', ext, '` NONMEM output file importing `', 
                get_extension(full_path)[1], '` instead.'), call. = FALSE)
      model_raw <- readr::read_lines(full_path[1])
    }
  }
  
  # Return error if input is bad
  if (!any(stringr::str_detect(model_raw, '^\\s*\\$PROB.+'))) {
    stop(basename(full_path), ' is not a NONMEM model.', call. = FALSE)
  }
  
  # Save the MD5 checksum for the model file
  file <-  tibble::tibble(
    problem = 0,
    name    = basename(full_path),
    file    = full_path,
    md5     = tools::md5sum(full_path),
    type    = 'model'
  )
  
  # Parse the raw model file
  model <- tibble::tibble(code = model_raw) %>% 
    
    # Drop empty rows and comment rows except special headers (;;)
    dplyr::filter(!stringr::str_detect(!!rlang::sym('code'), '^;[^;]*$|^$')) %>%
    
    # Change all tabs or consecutive spaces to single spaces
    dplyr::mutate(code = stringr::str_replace_all(!!rlang::sym('code'), '\\t+|\\s{2,}', ' ')) %>% 
    
    # Extract the problem, level and subroutine infos
    dplyr::mutate(
      problem     = findInterval(seq_along(!!rlang::sym('code')), 
                                 which(stringr::str_detect(!!rlang::sym('code'), '^\\s*\\$PROB.+'))),
      level       = findInterval(seq_along(!!rlang::sym('code')), 
                                 which(stringr::str_detect(!!rlang::sym('code'), '^\\s*\\$.+'))),
      subroutine  = stringr::str_match(.$code, '^\\s*\\$(\\w+)')[, 2]) %>% 
    tidyr::fill(dplyr::one_of('subroutine'))
  
  # Generate abbreviated subroutine names
  special <- c('THETAI', 'THETAR', 'THETAP', 'THETAPV', 'OMEGAP', 'OMEGAPD', 'SIGMAP', 'SIGMAPD')
  match_special <- match(model$subroutine[model$subroutine %in% special], special)
  model$subroutine[model$subroutine %in% special] <- c('thi', 'thr', 'thp', 'tpv', 
                                                       'omp', 'opd', 'sip', 'spd')[match_special]
  model$subroutine <- stringr::str_extract(tolower(model$subroutine), '[a-z]{1,3}')
  
  # Format lst part
  if (any(stringr::str_detect(model$code, 'NM-TRAN MESSAGES'))) {
    lst_rows <- which(stringr::str_detect(model$code, 'NM-TRAN MESSAGES')):nrow(model)
    model[lst_rows,] <- model %>% 
      dplyr::slice(lst_rows) %>% 
      dplyr::mutate(problem = findInterval(seq_along(!!rlang::sym('problem')), 
                                           which(stringr::str_detect(!!rlang::sym('code'), 
                                                                     '^\\s*PROBLEM NO\\.:\\s*\\d+$')))) %>% 
      dplyr::mutate(level = 1 + dplyr::first(!!rlang::sym('level')) + !!rlang::sym('problem'),
                    subroutine = 'lst')
  }
  
  # Handle other special cases
  if (any(stringr::str_detect(model$code, '#CPUT'))) {
    cput_row <- which(stringr::str_detect(model$code, '#CPUT'))
    model[cput_row, 'problem'] <- 0
    model[cput_row:nrow(model), 'level'] <- model[cput_row:nrow(model), ]$level + 1
  }
  
  if (any(stringr::str_detect(model$code, 'Stop Time'))) {
    end_rows <- which(stringr::str_detect(model$code, 'Stop Time')):nrow(model)
    model[end_rows, 'problem'] <- 0
    model[end_rows, 'level'] <- model[end_rows[1], ]$level + 1
  }
  
  model[is.na(model$subroutine) | (model$problem == 0 & model$subroutine == 'lst'), 'subroutine'] <- 'oth'
  
  # Remove subroutine names from the code
  model$code <- stringr::str_replace(model$code, '^\\s*\\$\\w+\\s*', '')
  
  # Remove empty rows but $PROBLEM
  model <- model[!stringr::str_detect(model$code, '^(\\s|\\t)*$') | model$subroutine == 'pro', ]
  
  # Create comment column
  code_rows <- !model$subroutine %in% c('lst', 'oth') | model$level == 0
  model[code_rows, 'comment'] <- stringr::str_match(model[code_rows, ]$code, ';\\s*(.*)\\s*$')[, 2]
  model[code_rows, 'code'] <- stringr::str_replace(model[code_rows, ]$code, '\\s*;.*$', '')
  
  # Remove na values
  model <- model %>% 
    tidyr::replace_na(replace = list(code = '', comment = '')) %>% 
    dplyr::select(dplyr::one_of('problem', 'level', 'subroutine', 'code', 'comment')) %>% 
    dplyr::mutate(problem = as.integer(!!rlang::sym('problem')),
                  level   = as.integer(!!rlang::sym('level')))
  
  # Ouptut code
  tibble::tibble(code = list(model_raw, model),
                 raw  = c(TRUE, FALSE)) %>% 
    dplyr::mutate(md5_ref = purrr::map_chr(.x = !!rlang::sym('code'), 
                                           .f = digest::digest)) %>% 
    list(data      = .,
         file_info = file) %>% 
    structure(class = 'nm_model')
}
