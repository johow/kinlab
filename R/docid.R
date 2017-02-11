#' Convert 'doc' (i.e. a family's ID) to individual ID and vice versa
#'
#' @param x Input (either a numerical ID for an individual spouse or a string character for a family's ID)
#' @param kh_fam The Krummhoern family dataframe
#' @param out_var Optional specification for output (if input is `doc`, defaults to `idf`)
#'
#' @return
#' @export
#'
#' @examples
#' docid(12)
docid <- function(x=NULL, 
                  kh_fam = kh.full::kh_src[[2]], 
                  out_var = ifelse(is.numeric(x), "doc", "idf")){
  stopifnot(all(is.numeric(x)) | all(!is.numeric(x)))
 if(all(is.numeric(x))){ 
 out_obj <- paste(kh_fam[kh_fam$idf %in% x | kh_fam$idm %in% x, paste(out_var)])
 }
 if(all(!is.numeric(x))){
   out_obj <- kh_fam[kh_fam$doc %in% paste(x), paste(out_var)]
 }
  return(out_obj)
}
