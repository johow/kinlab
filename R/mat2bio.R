#' Converts event array to Biograph
#'
#' This function converts event array to Biograph
#' @param evmat An event array
#' @param df_ind A dataframe describing individuals
#' @param df_fam A dataframe describing families
#' @param id Optional vector of IDs  to restrict to
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' df_ind$bdate <- sample(seq(as.Date("1774-12-31"), as.Date("1874-12-31"), 100), nrow(df_ind))
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' sample_kh(df_ind, df_fam)
#' }

mat2bio <- function(evmat, df_ind, df_fam, id = NULL){

   if(is.null(id)){id <- as.numeric(c(dimnames(evmat)$id))}
  born <- as.Date(ifelse(evmat[paste(id),1,5]==1, evmat[paste(id),1,1], NA), origin="1970-01-01")
  interview <- rep("2019-05-09", length(id))
  sex <- ifelse(get_sex(id, df_ind) %in% 2, "F", "M")
  educ <-
    ifelse(sex=="M",
           merge(data.frame(idm = as.numeric(id), tmp=NA), df_fam[df_fam$ehem %in% c("","1"),c("idm", "grasen")],  by="idm", all.x=TRUE)$grasen,
           merge(data.frame(idf = as.numeric(id), tmp=NA), df_fam[df_fam$ehef %in% c("","1"),c("idf", "grasen")],  by="idf", all.x=TRUE)$grasen)[order(id)]


namstates <- c("H","A","C","M")
A <-  as.Date(ISOdate(1970,01,01) + ifelse(sex=="M",
         merge(data.frame(idm = as.numeric(id), tmp=NA), df_fam[df_fam$ehem %in% c("","1"),c("idm", "dat4")],  by="idm", all.x=TRUE)$dat4,
         merge(data.frame(idf = as.numeric(id), tmp=NA), df_fam[df_fam$ehef %in% c("","1"),c("idf", "dat4")],  by="idf", all.x=TRUE)$dat4)[order(id)])


C <- suppressWarnings(first_bdate(id,  df_ind))
M <-  get_date(id, df_ind, df_fam, "id", "ddate")
d <- data.frame(A=A,C=C,M=M,stringsAsFactors =FALSE)
for (i in which(unlist(lapply(d, is.numeric)))){
d[,i] <- as.Date(d[,i], origin="1970-01-01")
}
dd <- d
for (i in 1:ncol(d)){
  dd[,i] <- as.numeric(d[,i])
}
nsample <- nrow(d)
# d= character object
# dd<- apply(d,1,function(x) y=as.Date(x))
# dd <- data.frame(t(dd))  #  dd is numeric
dimnames(dd) <- dimnames(d)
f <- Biograph::Sequences.ind.0(dd,namstates,absorb=NULL)
dates <- data.frame (f$d)
for (i in 1:3){
  dates[,i] <- as.Date(dates[,i],origin="1970-01-01")
}
path <- as.character(f$path)

bio  <- data.frame (ID=id,born=born,start=born,end=interview,sex=sex,educ=educ,path=as.character(path),dates[,1:(max(nchar(path))-1)],stringsAsFactors=FALSE)
namtrans <- paste("Tr",1:ncol(f$d),sep="")
colnames(bio)[8:9] <- namtrans[1:2]

attr(bio,"format.date") <- "%Y-%m-%d"
attr(bio,"format.born") <- "%Y-%m-%d"
attr(bio,"param") <- Biograph::Parameters(bio)
# Path to folder where bio should be stored

return (bio)
}
