#' List available variables
#'
#' @description Function listing all available variables in an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model code will be extracted.
#' @param .problem The problem to be used, by lists all available problems.
#' @return Prints the list of all available variables, and returns that list
#'   invisibly.  The name of the list is the problem number, the names of the
#'   elements of the sub-lists are the variable types, and the values of the
#'   sub-lists are the column names.
#' @seealso \code{\link{set_var_types}}
#' @examples
#' list_vars(xpdb_ex_pk)
#' @export
#' @importFrom dplyr group_by_at
#' @importFrom purrr map
#' @importFrom stringr str_c
list_vars <- function(xpdb, .problem=NULL) {
  name_map <-
    c(
      "id"="Subject identifier (id)",
      "occ"="Occasion flag (occ)", 
      "na"="Not attributed (na)", 
      "amt"="Dose amount (amt)", 
      "idv"="Independent variable (idv)", 
      "ipred"="Model individual predictions (ipred)", 
      "pred"="Model typical predictions (pred)", 
      "res"="Residuals (res)", 
      "evid"="Event identifier (evid)", 
      "dv"="Dependent variable (dv)", 
      "catcov"="Categorical covariates (catcov)", 
      "contcov"="Continuous covariates (contcov)", 
      "param"="Model parameter (param)", 
      "eta"="Eta (eta)", 
      "a"="Compartment amounts (a)", 
      "dvid"="DV identifier (dvid)", 
      "mdv"="Missing dependent variable (mdv)"
    )
  ret <- list_vars_prep(xpdb, .problem=.problem)
  ret_print <-
    lapply(
      X=ret,
      FUN=function(x) {
        new_names <- name_map[names(x)]
        new_names <-
          sprintf(
            # left-justified, space-filled with the required number of
            # characters
            fmt=paste0("%-", max(nchar(new_names)) + 1, "s"),
            new_names
          )
        setNames(object=x, nm=new_names)
      }
    )
  lapply(
    X=names(ret_print),
    FUN=function(x) {
      cat("\nList of available variables for problem no. ", x, "\n", sep="")
      cat(
        sprintf(
          " - %s: %s\n",
          names(ret_print[[x]]),
          sapply(X=ret_print[[x]], FUN=paste, collapse=", ")
        ),
        sep=""
      )
    }
  )
  invisible(ret)
}

#' @importFrom tidyr nest
#' @importFrom dplyr bind_rows
list_vars_prep <- function(xpdb, .problem=NULL) {
  check_xpdb(xpdb, check = "data")
  x <- xpdb$data
  if (!is.null(.problem)) {
    if (!all(.problem %in% x$problem)) {
      stop(
        "Problem no.",
        stringr::str_c(.problem[!.problem %in% x$problem], collapse = ", "),
        " not found in the data.", 
        call. = FALSE
      )
    }
    x <- x[x$problem %in% .problem, ]
  }
  type_order <-
    c("id", "dv", "idv", "dvid",  "occ", "amt", "evid", "mdv", "pred",
      "ipred", "param", "eta", "res", "catcov", "contcov", "a", "na")
  ret <-
    tidyr::nest(
      data=dplyr::group_by_at(.tbl=x, .vars="problem")
    )
  ret$list_of_vars <-
    purrr::map(
      .x=ret$data,
      .f=function(y) {
        ret <- list()
        current_index <- dplyr::bind_rows(y$index)
        for (current_type in intersect(type_order, current_index$type)) {
          ret[[current_type]] <- unique(current_index$col[current_index$type %in% current_type])
        }
        ret
      }
    )
  setNames(object=ret$list_of_vars, nm=as.character(ret$problem))
}
