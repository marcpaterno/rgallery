# Read an appropriately-organized HDF5 Group into a data.frame.
# This expects that the named Group contains Datasets that are all the same length.
# The datasets should contain basic types, including fixed-size arrays of basic types.

#' read.data.frame
#'
#' @param hfile The H5File object, as created by h5file.
#' @param groupname character vector, the name of the Group to be read. Must be length 1.
#'
#' @return a data.frame
#' @export
#'

read.data.frame <- function(hfile, groupname)
{
  g <- hfile[groupname] # Obtain the H5Group object
  on.exit(h5::h5close(g))
  dframe.from.group <- function(g) {
    ds.names <- h5::list.datasets(g, full.names=FALSE)
    ds.handles <- sapply(ds.names, FUN = function(n) g[n])
    on.exit(sapply(ds.handles, h5::h5close))
    ds.datas <- sapply(ds.handles, h5::readDataSet)
    as.data.frame(ds.datas)
  }
  dframe.from.group(g)
}

#' read.data.table
#'
#' @param hfile The H5File object, as created by h5file
#' @param groupname character vector, the name of the Group to be read. Must be length 1.
#'
#' @return a data.table
#' @export
#'
read.data.table <- function(hfile, groupname) data.table::data.table(read.data.frame(hfile, groupname))
