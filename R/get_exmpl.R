#' Small pedigree dataframe for example
#'
#' This function returns an example pedigree dataframe (no arguments).
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' get_exmpl_df()
#' }
get_exmpl_df <- function(){
  return(data.frame(id = c(113, 114, 1186, 55284, 55951, 55952, 55953, 55954, 56570, 57425, 57672, 57673, 57674, 57675, 57980, 117508),
                  momid = c(117508, 117508, 0, 0, 57980, 57980, 57980, 57980, 117508, 0, 55284, 55284, 55284, 55284, 0, 0),
                  dadid = c(57672, 57672, 0, 0, 1186, 1186, 1186, 1186, 55952, 0, 57425, 57425, 57425, 57425, 0, 0),
                  sex = c(1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2),
                  bdate = as.Date(c("1795-06-16", "1798-06-17", "1733-04-08", "1740-11-11", "1762-03-14", "1763-08-23", "1765-09-22", "1768-03-19", "1801-05-20", "1741-06-13", "1768-09-16", "1771-07-18", "1775-04-21", "1777-05-22", "1730-01-09", NA)),
                  bplace = c("WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "x.unknown"),
                  bstatus = c("", "", "", "", "E", "", "", "", "", "", "", "", "", "", "", ""),
                  ddate = as.Date(c(NA, NA, NA, "1789-12-17", "1762-04-23", NA, NA, "1768-03-19", NA, "1793-06-04", "1799-04-14", "1800-10-29", "1822-08-18", "1805-02-09", "1803-05-06", NA)),
                  dplace = c("x.unknown", "x.unknown", "x.unknown", "x.unknown", "WI", "x.unknown", "x.unknown", "WI", "x.unknown", "x.unknown", "x.unknown", "x.unknown", "x.unknown", "x.unknown", "x.unknown", "x.unknown"),
                  dstatus = c("", "", "", "", "E", "", "", "", "", "", "", "", "", "", "", ""),
                  famid = 1,
                  affected = 1,
                  status = 1))
  }
