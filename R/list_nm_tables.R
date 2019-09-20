#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{nm_model} object.
#'
#' @param nm_model An xpose nm_model object generated with \code{\link{read_nm_model}}.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' read_nm_model(file = 'run001.lst') %>% 
#'   list_nm_tables()
#' }
#' @export
#' @importFrom dplyr filter group_by_at mutate summarize tibble
#' @importFrom stringr str_match str_detect
list_nm_tables <- function(nm_model = NULL) {
  
  if (is.null(nm_model) || !is.nm.model(nm_model)) {
    stop('Object of class `nm_model` required.', call. = FALSE)
  }
  
  # Prepare null object to be returned if no $table is found
  null_object <- as.nm.table.list(dplyr::tibble(problem = -1, file = '', 
                                                firstonly = NA, simtab = NA))
    
  # Get NM code associated with the tables
  table_list_prep <-
    nm_model %>% 
    dplyr::filter(.$problem > 0, .$subroutine == 'tab') 
  
  if (nrow(table_list_prep) == 0) return(null_object)
  
  table_list_file <-
    table_list_prep %>% 
    dplyr::group_by_at(.vars = c('problem', 'level')) %>% 
    dplyr::summarize_at(.vars="code", .funs=paste, collapse=" ") %>%
    dplyr::ungroup() %>%
    dplyr::rename(string="code") %>%
    dplyr::mutate(file = stringr::str_match(.$string, '\\s+FILE\\s*=\\s*([^\\s]+)')[, 2]) %>% 
    dplyr::filter(!is.na(.$file))
  
  if (nrow(table_list_file) == 0) return(null_object)
  
  # Find table names and firstonly option
  table_list <-
    table_list_file %>% 
    dplyr::mutate(
      file = file_path(attr(nm_model, 'dir'), .$file),
      firstonly = stringr::str_detect(.$string, 'FIRSTONLY')
    ) %>% 
    dplyr::select_at(.vars=c('problem', 'file', 'firstonly'))

  # Prep simtab flag
  sim_flag <-
    nm_model %>% 
    dplyr::filter(.$problem > 0) %>% 
    tidyr::nest(data=setdiff(names(.), "problem")) %>% 
    dplyr::mutate(simtab = purrr::map_lgl(.$data, ~!any(stringr::str_detect(.$subroutine, 'est')))) %>% 
    dplyr::select_at(.vars=c(c('problem', 'simtab')))
  
  # Merge and output
  table_list %>% 
    dplyr::left_join(sim_flag, by = 'problem') %>% 
    as.nm.table.list()
}
