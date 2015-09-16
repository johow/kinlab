#' Source and local file paths
#'
#' Returns file path to directories containing `kinlab` sources
#' and local data on linux and windows machines.
#' @param path_type Either 'local' for the 'kinlab' folder installed on the current machine, or 'source' folders specified in `linux_source` and `win_source`, or 'cloud' for predefined Dropbox folder)
#' @param db A name of a data source, currently either 'kh' (i.e. Krummhörn, as default) or 'qb' (Quebec).
#' @param dir Name of the local kinlab directory in library (Default is 'inst')
#' @param sub NULL or name of subdirectory (if required, default is `file.path("data", db))
#' @param linux_source Linux source folder (including subdirectory for `db`)
#' @param win_source Windows source folder (including subdirectory for `db`)
#' @param inst_source Source folder for library directory
#' @param my_lib Optionally allows setting a library directory manually
#' @param pkg Internal use (not to be changed by user)
#' @keywords file
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }
get_file_path <- function(path_type = c("local", "source", "inst", "cloud")[1],
                          db = c("kh", "qb")[1],
                          dir = "data",
                          sub = NULL,
                          linux_source = paste0("/home/johow/Dropbox/db/", db),
                          win_source = paste0("C:/Users/JJ/Desktop/db/", db),
                          inst_source = ifelse(grepl("linux", R.version["platform"]),
                                               "/media/johow/USB DISK/inst/data", "H://inst/data"),
                          pkg = "kinlab",
                          my_lib=NULL){
  if(is.null(my_lib)){
    my_lib <- .libPaths()[[which(unlist(lapply(lapply(lapply(.libPaths(), list.files),
                                                      "%in%", "kinlab"), any)))[1]]]
  }
  stopifnot(pkg %in% list.files(my_lib) &
              pkg == "kinlab" &
              path_type %in% c("source", "local", "inst", "cloud") &
              db %in% c("qb", "kh"))
if (path_type=="source"){
  return(ifelse(grepl("linux", R.version["platform"]), linux_source, win_source))
}
  if (path_type=="local") {
  if (!dir.exists(file.path(file.path(my_lib, pkg), dir))){
   dir.create(file.path(file.path(my_lib, pkg), dir))
  }
  if (!is.null(sub)){
    dir <- file.path(dir, sub)
    if (!dir.exists(file.path(file.path(my_lib, pkg), dir))){
      dir.create(file.path(file.path(my_lib, pkg), dir))
    }
    dir <- file.path(dir, db)
    if (!dir.exists(file.path(file.path(my_lib, pkg), dir))){
      dir.create(file.path(file.path(my_lib, pkg), dir))
    }
  } else {dir <- file.path(dir, db)
                           if (!dir.exists(file.path(file.path(my_lib, pkg), dir))){
                             dir.create(file.path(file.path(my_lib, pkg), dir))
                           }
          }
  return(file.path(file.path(my_lib, pkg), dir))
} else if (path_type=="cloud"){
  return(ifelse(grepl("linux", R.version["platform"]), "/home/johow/Dropbox/kinlab/inst", "c://Users/JJ/Dropbox/kinlab/inst"))
} else return(inst_source)
}
