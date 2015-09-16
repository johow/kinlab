#' Wrapper function to import and format Krummh\enc{ö}{oe}rn data for children and families from SPSS files
#'
#' This function returns a list containing three data.frames:
#' \enumerate{
#'   \item Imported 'kindermc.sav' SPSS file as read from source.
#'   \item Imported 'familienmc.sav' SPSS file as read from source.
#'   \item Dataframe holding individual IDs and those of the parents.
#' }
#' @param kindermc File path to  the 'kindermc.sav' SPSS file.
#' @param familienmc File path to  the 'familienmc.sav' SPSS file
#' @keywords kh import
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)
get_kh <- function(kindermc="", familienmc=""){
  ###############################
  #
  #  DATA IMPORT AND PREPARATION
  #
  ###############################

  #
  # IMPORT DATA
  #

  df_kinder <- suppressWarnings(foreign::read.spss(kindermc, to.data.frame=TRUE))
  df_fam <- suppressWarnings(foreign::read.spss(familienmc, to.data.frame=TRUE))

  df_kinder <- df_kinder[, which(unlist(lapply(lapply(df_kinder, levels), length))!=1)]
  df_fam <- df_fam[, which(unlist(lapply(lapply(df_fam, levels), length))!=1)]
  #
  # REMOVE 'UMLAUTE' AND 'WHITE SPACE' FROM FACTOR LEVELS
  #

  kh <- list("kind" = df_kinder, "fam" = df_fam)

  for (j in 1:2){
    for (i in 1:ncol(kh[[j]])){
      if (is.factor(kh[[j]][[i]])){
        levels(kh[[j]][[i]]) <-  gsub("\u00c4", "AE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00d6", "OE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00dc", "UE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00e4", "AE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00f6", "OE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00fc", "UE", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <-  gsub("\u00df", "SS", levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <- trim(levels(kh[[j]][[i]]))
        levels(kh[[j]][[i]]) <- toupper(levels(kh[[j]][[i]]))
      }
    }
  }

  df_kinder <- kh[["kind"]]
  df_fam <- kh[["fam"]]

  table(df_kinder$gebkk)

  df_kinder$gebkk[is.na(df_kinder$gebk4) | is.na(df_kinder$gebk8)] <- "E"
  table(df_kinder$gebkk)

  #
  # FORMAT DATES
  #

  # SET SINGLE BIRTH DATE
  # bdate: mean value of gebk4 and gebk8 (excluding missing values)

  df_kinder$bdate <- as.Date(as.POSIXct(ifelse(!is.na(df_kinder[,c("gebk4")])  & !is.na(df_kinder[,c("gebk8")]),(df_kinder[,c("gebk4")] + df_kinder[,c("gebk8")])/2,
                                               ifelse(is.na(df_kinder[,c("gebk4")]), df_kinder[,c("gebk8")], df_kinder[,c("gebk4")])), origin="1582-10-14"))

  df_fam$bdatef <- as.Date(as.POSIXct(ifelse(!is.na(df_fam[,c("gebf4")])  & !is.na(df_fam[,c("gebf8")]), (df_fam[,c("gebf4")] + df_fam[,c("gebf8")])/2,
                                             ifelse(is.na(df_fam[,c("gebf4")]), df_fam[,c("gebf8")], df_fam[,c("gebf4")])), origin="1582-10-14"))


  df_fam$bdatem <- as.Date(as.POSIXct(ifelse(!is.na(df_fam[,c("gebm4")])  & !is.na(df_fam[,c("gebm8")]), (df_fam[,c("gebm4")] + df_fam[,c("gebm8")])/2,
                                             ifelse(is.na(df_fam[,c("gebm4")]), df_fam[,c("gebm8")], df_fam[,c("gebm4")])), origin="1582-10-14"))

  # SET SINGLE DEATH DATE
  # ddate: todk8

  df_kinder$ddate <- as.Date(as.POSIXct(df_kinder[,c("todk8")], origin="1582-10-14"))

  df_fam$ddatef <- as.Date(as.POSIXct( df_fam[,c("todf8")], origin="1582-10-14"))
  df_fam$ddatem <- as.Date(as.POSIXct( df_fam[,c("todm8")], origin="1582-10-14"))

  #
  # RECODE DATE VARIABLES
  #

  for (i in c("gebk4", "gebk8", "todk4", "todk8")){
    df_kinder[,paste(i)] <- as.Date(as.POSIXct(df_kinder[,paste(i)], origin="1582-10-14"))
  }

  for (i in c("dat4", "dat8", "gebm4", "gebm8", "todm4", "todm8", "gebf4", "gebf8", "todf4", "todf8")){
    df_fam[,paste(i)] <- as.Date(as.POSIXct(df_fam[,paste(i)], origin="1582-10-14"))
  }

  # ADJUST FACTOR LEVELS

  df_kinder$gebortk <- factor(paste(trim(df_kinder$gebortk)))
  df_kinder$gebortk[df_kinder$gebortk=="'WI"] <- "WI"
  levels(df_kinder$gebortk) <- unlist(lapply(levels(df_kinder$gebortk), get_kirchspiele))
  levels(df_kinder$todortk) <- unlist(lapply(levels(df_kinder$todortk), get_kirchspiele))

  levels(df_fam$gebortf) <- unlist(lapply(levels(df_fam$gebortf), get_kirchspiele))
  df_fam$bplacef <- paste(df_fam$gebortf)

  levels(df_fam$gebortm) <- unlist(lapply(levels(df_fam$gebortm), get_kirchspiele))
  df_fam$bplacem <- paste(df_fam$gebortm)

  levels(df_fam$todortf) <- unlist(lapply(levels(df_fam$todortf), get_kirchspiele))
  df_fam$dplacef <- paste(df_fam$todortf)

  levels(df_fam$todortm) <- unlist(lapply(levels(df_fam$todortm), get_kirchspiele))
  df_fam$dplacem <- paste(df_fam$todortm)



  ###############################
  #
  #  STEP 1: ASSIGN INDIVIDUAL ID
  #
  ###############################

  #
  # GENERATE UNIQUE ID FOR CHILDREN
  #

  df_kinder$id <- 1:nrow(df_kinder)

  # UPDATE TABLES IN LIST
  kh <- list("kind" = df_kinder, "fam" = df_fam)

  #
  # MATCH IDs WITH MULTIPLE MARRIAGES, CHECK FOR CONFLICTS
  #

  df_x <- kh[["fam"]][,c("doc", names(kh[["fam"]])[which(grepl("ehe", names(kh[["fam"]])))])]

  for (i in c("M", "W")){
    for (j in 1:5){
      df_tmp    <- kh[["kind"]][kh[["kind"]]$sex == paste(i) & paste0("famnrk",j)!=levels(kh[["kind"]][[paste0("famnrk",j)]])[1],
                                c("id", paste0("famnrk",j))]
      names(df_tmp) <- c(paste0("id", ifelse(i == "M", "m", "f"), j), "doc")
      df_x <- merge(df_x, df_tmp, by = "doc", all.x = TRUE)
      df_tmp <- NULL
    }
  }
  df_x$tmp <- NULL
  # summary(df_x)

  df_x$idm <- df_x$idm1
  df_x$idf <- df_x$idf1

  table(rowSums(apply(df_x[,14:23],2, is.na)==FALSE))

  # summary(df_x$idm)
  # summary(df_x$idf)

  for (i in c("m", "f")){
    for (j in 2:5){
      df_x[,paste0("id", i)] <- ifelse(is.na(df_x[,paste0("id", i)]),
                                       df_x[,paste0("id", i, j)],
                                       ifelse(is.na(df_x[,paste0("id", i, j)]),
                                              df_x[,paste0("id", i)], -999))

    }
    if (any(  df_x[!is.na(df_x[,paste0("id", i)]),paste0("id", i)]<0)){
      message("ID conflict!")
      print(df_x[df_x[!is.na(df_x[,paste0("id", i)]),paste0("id", i)]<0,])
    }
  }

  # CHECKS FOR EXISTENCE OF FOUNDED FAMILIES
  tmp_known <- df_x$doc[!is.na(df_x[,"idm"])]
  stopifnot(all(kh[[1]]$famnrk1[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk1[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk2[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk2[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk3[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk3[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk4[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk4[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk5[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk5[kh[[1]]$sex=="M"] == ""))

  # summary(df_x$idm)

  # ADD IDs OF 1ST-(EVER-)MARRIED HUSBANDS NOT LISTED AS CHILD
  df_x$idm[is.na(df_x$idm) & df_x$ehem %in% c("", "1")] <- c(1:length(df_x$idm[is.na(df_x$idm) & df_x$ehem %in% c("", "1")]))+nrow(kh[[1]])

  # summary(df_x$idm)
  # MATCH MULTIPLE MARRIAGES
  for (i in paste0("ehenrm", 1:4)){
    df_x<- merge(df_x, data.frame(doc = df_x[df_x[,paste(i)] != levels(df_x[,paste(i)])[1] & !duplicated(df_x[,paste(i)]),paste(i)],
                                  idm_tmp = df_x[df_x[,paste(i)] != levels(df_x[,paste(i)])[1] & !duplicated(df_x[,paste(i)]),"idm"]), by = "doc", all.x=TRUE)
    df_x$idm <- ifelse(is.na(df_x$idm), df_x$idm_tmp, df_x$idm)
    df_x$idm_tmp <- NULL
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 1)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 1)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 1)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 2)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 2)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 2)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 3)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 3)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 3)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 4)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 4)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 4)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  # summary(df_x$idf)
  # ADD IDs OF 1ST-(EVER-)MARRIED WIFES NOT LISTED AS CHILD
  df_x$idf[is.na(df_x$idf) & df_x$ehef %in% c("", "1")] <- c(1:length(df_x$idf[is.na(df_x$idf) & df_x$ehef %in% c("", "1")]))+max(df_x$idm)
  # summary(df_x$idf)
  # MATCH MULTIPLE MARRIAGES
  for (i in names(df_x)[which(grepl("ehenrf", names(df_x)))]){
    df_x<- merge(df_x, data.frame(doc = df_x[df_x[,paste(i)] != levels(df_x[,paste(i)])[1] & !duplicated(df_x[,paste(i)]),paste(i)],
                                  idf_tmp = df_x[df_x[,paste(i)] != levels(df_x[,paste(i)])[1] & !duplicated(df_x[,paste(i)]),"idf"]), by = "doc", all.x=TRUE)
    df_x$idf <- ifelse(is.na(df_x$idf), df_x$idf_tmp, df_x$idf)
    df_x$idf_tmp <- NULL
  }

  # summary(df_x$idf)

  #################################
  #
  #  STEP 2:  COMPILE PEDIGREE DATA
  #
  #################################

  df_x2 <- merge(df_kinder[,c("id", "doc")], df_x[,c("doc", "idm", "idf")], by ="doc", all.x = TRUE)
  df_x2 <-df_x2[,c("id", "idm", "idf")]
  # str(df_x2)



  ord_x <- pedigree::orderPed(df_x2)
  df_x2 <- df_x2[order(ord_x),]
  # str(df_x2)


  df_y <- pedigree::add.Inds(df_x2)
  # str(df_y)
  df_y$id <- as.numeric(paste(df_y$id))
  df_kinder$bplace <- df_kinder$gebortk
  df_kinder$dplace <- df_kinder$todortk
  df_ped <- merge(df_y, df_kinder[,c("id", "bdate", "bplace", "ddate", "dplace", "sex", "gebkk", "todkk")], by ="id", all.x=TRUE)
  df_ped$sex[df_ped$id %in% df_ped$idf] <- "W"
  df_ped$sex[df_ped$id %in% df_ped$idm] <- "M"
  df_ped$sex <- ifelse(df_ped$sex == "M", 1, ifelse(df_ped$sex == "W", 2, 3))
  # summary(df_ped$sex)

  df_fam <- merge(df_fam, df_x[,c("doc", "idf", "idm")], by ="doc", all.x=TRUE)

  df_fam$id <- df_fam$idf
  df_ped <- merge(df_ped, df_fam[!duplicated(df_fam$id),c("id", "bdatef", "bplacef", "ddatef", "dplacef", "gebfk", "todfk")], by ="id", all.x=TRUE)
  df_fam$id <- NULL

  df_fam$id <- df_fam$idm
  df_ped <- merge(df_ped, df_fam[!duplicated(df_fam$id),c("id", "bdatem", "bplacem", "ddatem", "dplacem", "gebmk", "todmk")], by ="id", all.x=TRUE)
  df_fam$id <- NULL

  # summary(df_ped$bdate)
  df_ped$bdate <- as.Date(ifelse(is.na(df_ped$bdate), ifelse(df_ped$sex == 1, df_ped$bdatem, ifelse(df_ped$sex == 2, df_ped$bdatef,  df_ped$bdate)), df_ped$bdate), origin="1970-01-01")
  # summary(df_ped$bdate)

  # summary(factor(df_ped$bplace))
  df_ped$bplace <- paste(ifelse(is.na(df_ped$bplace), ifelse(df_ped$sex == 1, paste(df_ped$bplacem), ifelse(df_ped$sex == 2, paste(df_ped$bplacef),  paste(df_ped$bplace))), paste(df_ped$bplace)))
  # summary(factor(df_ped$bplace))

  #  summary(df_ped$gebk)
  df_ped$gebk <- ifelse(is.na(df_ped$gebkk), ifelse(df_ped$sex == 1, paste(df_ped$gebfk), ifelse(df_ped$sex == 2, paste(df_ped$gebmk),  paste(df_ped$gebkk))), paste(df_ped$gebkk))
  # summary(df_ped$gebk)

  #  summary(df_ped$ddate)
  df_ped$ddate <- as.Date(ifelse(is.na(df_ped$ddate), ifelse(df_ped$sex == 1, df_ped$ddatem, ifelse(df_ped$sex == 2, df_ped$ddatef,  df_ped$ddate)), df_ped$ddate), origin="1970-01-01")
  # summary(df_ped$ddate)

  #   summary(df_ped$dplace)
  df_ped$dplace <- paste(ifelse(is.na(df_ped$dplace) | df_ped$dplace == "x.unknown", ifelse(df_ped$sex == 1, paste(df_ped$dplacem),
                                                                                            ifelse(df_ped$sex == 2, paste(df_ped$dplacef),  paste(df_ped$dplace))), paste(df_ped$dplace)))
  # table(df_ped$dplace)

  #  table(df_ped$todk)
  df_ped$todk <- ifelse(is.na(df_ped$todkk), ifelse(df_ped$sex == 1, paste(df_ped$todfk), ifelse(df_ped$sex == 2, paste(df_ped$todmk),  paste(df_ped$todkk))), paste(df_ped$todkk))
  # table(df_ped$todk)
  df_ped$todk[df_ped$todk=="NA"] <- ""
  # table(df_ped$todk)

  # df_ped$ddate[df_ped$ddate==df_ped$bdate & !is.na(df_ped$bdate) & !is.na(df_ped$ddate)] <-
  #   df_ped$ddate[df_ped$ddate==df_ped$bdate & !is.na(df_ped$bdate) & !is.na(df_ped$ddate)] + 1

  df_ped <- df_ped[,c("id", "idf", "idm", "sex", "bdate", "bplace", "gebk", "ddate", "dplace", "todk")]
  df_ped$momid <- df_ped$idf
  df_ped$dadid <- df_ped$idm
  #  # str(df_ped)

  for (i in 1:ncol(df_ped)){
    if (is.character(df_ped[,i])){
      df_ped[,i] <- factor( df_ped[,i])
    }
  }
  # str(df_ped)

  #   df_ped[df_ped$idf %in% df_ped$idm | df_ped$idm %in% df_ped$idf,]
  #   df_ped$idf[df_ped$idf %in% df_ped$idm] <- (max(c(df_ped$idm, df_ped$idf), na.rm=TRUE) + 1)


  df_ped <- df_ped[order(df_ped$idf, df_ped$bdate),]
  df_ped$ibi <- ifelse(df_ped$idf == c(df_ped$idf[2:nrow(df_ped)], NA) & !is.na(df_ped$bdate),
                       as.numeric(difftime(c(df_ped$bdate[2:nrow(df_ped)], NA), df_ped$bdate, units="days"))/365.25, NA)

  df_ped$preibi <- c(NA, df_ped$ibi[1:(nrow(df_ped)-1)])

  #summary(df_ped$ibi)
  df_ped <- df_ped[order(df_ped$idf, df_ped$bdate),]


  df_ped$famid <- with(df_ped, kinship2::makefamid(id, idm, idf))
  df_ped <- subset(df_ped, famid == which(table(df_ped$famid)==max(table(df_ped$famid))))

  df_ped$affected <- ifelse(df_ped$id %in% df_ped$id[!is.na(df_ped$idf)] & df_ped$idf %in% df_ped$id[!is.na(df_ped$idf)], 1, 0)
  df_ped$status <- ifelse(is.na(df_ped$ddate), 0, 1)


  df_ped$momid <- na2zero(df_ped$momid)
  df_ped$dadid <- na2zero(df_ped$dadid)

  df_ped <- df_ped[order(df_ped$momid, df_ped$bdate),]

  df_ped$bnum <- as.numeric(factor(df_ped$bplace))
  df_ped$dnum <- as.numeric(factor(df_ped$dplace))

  df_ped$gebkk <- paste(df_ped$gebk)
  df_ped$todkk <- paste(df_ped$todk)

  df_ped$bspc <- as.numeric(factor(df_ped$gebk))
  df_ped$dspc <- as.numeric(factor(df_ped$todk)) + max(df_ped$bspc)
  df_ped$dspc[df_ped$dspc==7]<-2
  df_ped$dspc[df_ped$dspc==5]<-1
  df_ped$dspc[df_ped$dspc==6]<-5
  return(list("df_kinder" = df_kinder, "df_fam" = df_fam, "df_ped" = df_ped))
}
