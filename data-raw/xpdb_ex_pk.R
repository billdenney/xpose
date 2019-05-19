#' Generate xpdb example for the xpose package
#' 
#' @description Generate the xpdb_ex_pk.rda object when change in internal xpose 
#' structure is made.
#' @usage Set working directory to the xpose project directory. And run the whole script.
#' 
library(xpose)

# Generate xpdb
xpdb_ex_pk <- xpose_data(runno = '001', dir = 'inst/extdata/', quiet = FALSE)

# Overwrite directory paths
xpdb_ex_pk$summary$value[xpdb_ex_pk$summary$label == 'dir'] <- 'analysis/models/pk/'
xpdb_ex_pk$options$dir <- 'analysis/models/pk/'
attr(xpdb_ex_pk$code, 'dir') <- 'analysis/models/pk/'
xpdb_ex_pk <- xpose::as.xpdb(xpdb_ex_pk)

# Export rda to data folder
save(xpdb_ex_pk, file = 'data/xpdb_ex_pk.rda', compress = 'xz')
