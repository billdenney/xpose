#' Extract the log-likelihood from xpose_data
#'
#' @param object The xpdb object
#' @param ... passed to `get_summary()` and `get_prm()`.
#' @return A logLik object including the likelihood, number of observations,
#'   number of individuals, and degrees of freedom (number of estimated
#'   parameters in the model).
#' @examples
#' logLik(xpdb_ex_pk)
#' AIC(xpdb_ex_pk)
#' BIC(xpdb_ex_pk)
#' nobs(logLik(xpdb_ex_pk))
#' @export
logLik.xpose_data <- function(object, ...) {
  sumry <- get_summary(xpdb=object, ...)
  prm <- get_prm(xpdb=object, ...)
  method <- sumry$value[sumry$label %in% "method"]
  mask_sim <- method %in% "sim"
  method <- method[!mask_sim]
  if (!all(method %in% c("fo", "foce", "fo-i", "foce-i"))) {
    # Note that "sim" will be ignored later
    warning("method is not in the fo/foce family.  OFV may not be the same as -2*log-likelihood.")
  }
  # The -2 is to convert from -2*logLik to logLik.
  ofv <- as.numeric(sumry$value[sumry$label %in% "ofv"][!mask_sim])/-2
  if (length(ofv) != 1) {
    warning("It appears that more than one problem is being summarized. logLik may not work with other methods expecting a scalar logLik.")
  }
  nobs <- as.integer(sumry$value[sumry$label %in% "nobs"][!mask_sim])
  nind <- as.integer(sumry$value[sumry$label %in% "nind"][!mask_sim])
  df <- sum(!prm$fixed)
  structure(
    ofv,
    nobs=nobs,
    nind=nind,
    df=df,
    class="logLik"
  )
}
