#' Summarizing xpose_data
#'
#' @description This function returns a summary of an \code{\link{xpose_data}} 
#' to the console.
#' @param object An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param .problem The problem to be used, by default returns the last one for each label.
#' @param ... Ignored in this function
#'
#' @method summary xpose_data
#' @examples
#' summary(xpdb_ex_pk)
#'
#' @export
#' @importFrom dplyr arrange_at filter group_by_at mutate mutate_at slice
#' @importFrom purrr map map_chr
#' @importFrom stringr fixed str_c str_detect str_pad str_replace_all
#' @importFrom tidyr nest
summary.xpose_data <- function(object, .problem = NULL, ...) {
  order <- c('software', 'version', 'dir', 'file', 'run', 'ref', 'descr', 'timestart', 
             'timestop', 'probn', 'label', 'data', 'nind', 'nobs', 'subroutine', 'method',
             'term', 'runtime', 'ofv', 'nsig', 'covtime', 'condn','etashk', 'epsshk', 
             'nsim', 'simseed', 'nesample', 'esampleseed', 'errors', 'warnings')
  
  out_summary <- get_summary(xpdb=object, .problem=.problem, only_last = FALSE)
  out_filtered <- out_summary[!(out_summary$value %in% "na"), , drop=FALSE]

  out <-
    out_filtered %>%
    dplyr::mutate_at(.vars="label", .funs=ordered, levels=order) %>%
    dplyr::arrange_at(.vars=c("problem", "subprob", "label")) %>%
    dplyr::mutate_at(.vars="label", .funs=as.character) %>%
    tidyr::nest(data=setdiff(names(.), c('problem', 'label', 'descr'))) %>% 
    dplyr::mutate(
      value =
        purrr::map_chr(
          .$data,
          function(x) {
            if (nrow(x) == 1) return(x$value)
            value <- stringr::str_c(x$value, ' (subprob no.', x$subprob, ')', sep = '')
            stringr::str_c(value, collapse = '\n')
          }
        )
    ) %>%
    select_at(.vars=setdiff(names(.), "data")) %>%
    dplyr::mutate(
      descr = stringr::str_pad(.$descr, width = max(nchar(.$descr)) + 2, 'right'),
      label = stringr::str_pad(.$label, width = max(nchar(.$label)), 'right'),
      value = stringr::str_replace_all(.$value, '\n', stringr::str_pad('\n', max(nchar(.$descr)) + max(nchar(.$label)) + 9, 'right'))
    ) %>% 
    dplyr::mutate(
      descr = stringr::str_c(.$descr, '@', .$label, '')
    ) %>%
    dplyr::mutate(
      string = stringr::str_c(' -', .$descr, ':', .$value, sep = ' '),
      grouping = as.character(.$problem)
    ) %>% 
    tidyr::nest(data=setdiff(names(.), "grouping"))
  
  purrr::map(
    .x=out$data,
    .f=function(x) {
      x <- dplyr::filter(.data = x, !stringr::str_detect(x$descr, 'Problem number'))
      if (x$problem[1] == 0) {
        lab <- '[Global information]'
      } else {
        lab_row <- which(stringr::str_detect(x$descr, stringr::fixed('Run label')))
        lab <- stringr::str_c('[', x$value[lab_row], ']', sep = '')
        x <- x[-lab_row, ]
      }
      
      cat('\nSummary for problem no.', x$problem[1], lab, '\n')
      cat(x$string, sep = '\n')
    }
  )
  invisible(out)
}
