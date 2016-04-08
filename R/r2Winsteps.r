#' Write data and control files for Winsteps from R.
#'
#' Function for writing control and data files from R to Winsteps for Rasch 
#'   analysis.
#'
#' @param itms Dataframe or matrix of items responses
#' @param dems Dataframe or matrix of person identifiers/demographic fields
#' @param title Title of the analysis, which will be used in the name of the 
#'   data and text files. Defaults to "r2Winsteps".
#' @param partialCredit Logical. Should Masters' partial credit model be 
#'   estimated? Defaults to FALSE. If the data include more than two categories
#'   and \code{partialCredit} == FALSE, then the control file will be written 
#'   such that Andrich's rating scale model is estimated. If the data include
#'   only two response options, then partialCredit must be set to FALSE.
#' @param idelete Items to remove from the analysis. Should be supplied as a 
#'   vector.
#' @param anchorFile Optional name of an anchor file to be included in the
#'   analysis, see \code{\link{write.anchor}}
#' @param sAnchorFile Optional nam of a structure file to be included in the 
#'   analysis when anchoring a partial credit scale.
#' @param Ifile Logical, defaults to \code{TRUE}. Should the item file be
#'  returned?
#' @param Pfile Logical, defaults to \code{TRUE}. Should the person file be
#'  returned?
#' @param Sfile Logical, defaults to \code{TRUE} when the data have more than
#'  two categories. Should the item-structure file be returned?
#' @param format Format in which item and person files should be returned. 
#'   Takes values "txt" and "XLS" to return txt and XLS files, respectively. 
#'   Defaults to "txt". Note that "txt" must be used if subsequent calls to 
#'   batch.ifile/batch.pfile are used to read thefiles back into R.
#' @param dec Number of decimals reported in output. Defaults to 2. Must range
#'   from 0 to 4.
#' @export
#' @return Control and Data file for analysis with Winsteps.

r2Winsteps<-function(itms, dems = NULL, partialCredit = FALSE, idelete = NULL, 
    anchorFile = NULL, sAnchorFile = NULL, title = "r2Winsteps", 
    Ifile = TRUE, Pfile = TRUE, Sfile = TRUE, format = "txt", dec = 2) {

#=============================== Write Data file ==============================
    if(is.null(dems)) {
        dems <- data.frame(rownames = rownames(itms))
    }

    #Convert itms to character, code missing data as "M"
    itms <- apply(itms, 2, as.character)
    for (i in 1:ncol(itms)) {
        itms[, i] <- ifelse(is.na(itms[, i]), "M", itms[, i])
    }
    
    #Find the column with the maximum width
    maxWide <- max(apply(itms, 2, function(x) nchar(x)))
    
    #Pad all items to equal the max width
    for (i in 1:ncol(itms)) {
        itms[, i] <- stringr::str_pad(itms[, i], maxWide, side = "right")
    }

    #Extract codes in data
    dtaCodes <- apply(itms, 2, table)
    if (class(dtaCodes) == "matrix") {
        codes <- rownames(dtaCodes)
    }
    else {
        codes <- c(names(dtaCodes[[1]]), names(dtaCodes[[2]]))
        for (i in 3:length(dtaCodes)) {
            codes <- c(codes, names(dtaCodes[[i]]))
        }
        codes <- unique(codes)
    }
    if(length(grep("M",codes)) != 0){
        codes <- codes[-grep("M", codes)]
    }
    
    #Paste all items in one character string
    istring <- paste(itms[, 1], itms[, 2], sep = "")
    for (i in 3:ncol(itms)) {
        istring <- paste(istring, itms[, i], sep = "")
    }

    #Convert all dems columns to string; pad to largest column
    dems <- as.data.frame(apply(dems, 2, as.character), stringsAsFactors = FALSE)
    for (i in 1:ncol(dems)) {
        dems[, i] <- stringr::str_pad(dems[, i], max(nchar(dems[, 
            i])), side = "right")
    }
    
    #Paste demos into a single string
    if(ncol(dems) == 1) {
        dstring <- dems[ ,1]
    }
    if(ncol(dems) > 1) {
        dstring <- apply(dems, 1, paste, collapse = " | ")
    }
    
    #Put items and demos together
    d <- data.frame(dems = dstring, responses = istring)
    
    #Demo string (not a factor)
    d$dems <- as.character(d$dems)
    
    #Put data together
    winData <- paste(d$dems, d$responses, sep = " ")

    #Title for Winsteps data file
    dtaTitle <- paste(as.character(title), "Dta", sep = "")
    sink(paste(dtaTitle, "txt", sep = "."))
        cat(winData, sep = "\n")
    sink()
    
#============================= Write Control File =============================
    ttl <- paste("TITLE = ", title, sep = "")
    dtaTitle <- paste(as.character(title), "Dta", sep = "")
    dt <- paste("DATA = ", paste(dtaTitle, "txt", sep = "."))
    i1 <- paste("ITEM1 = ", nchar(dstring)[1] + 2)
    ni <- paste("NI = ", ncol(itms))
    namStart <- "NAME1 = 1"
    namLen <- paste("NAMLEN = ", nchar(dstring)[1])
    wid <- paste("XWIDE = ", maxWide, sep = "")
    cod <- paste("CODES = ", paste(codes, collapse = " "), sep = "")
    totSc <- "TOTALSCORE = YES"
    idel <- paste("IDELETE = ", paste(idelete, collapse = " "))
    afile <- paste("IAFILE = ", anchorFile, sep = "")
    sAfile <- paste("SAFILE = ", sAnchorFile, sep = "")
    udec <- paste("UDECIMALS = ", dec, sep = "")

    if (format == "XLS") {
        ifile <- paste("IFILE = ", paste(title, "Ifile.xls", 
            sep = ""))
        pfile <- paste("PFILE = ", paste(title, "Pfile.xls", 
            sep = ""))
    }
    
    if (format == "txt") {
        ifile <- paste("IFILE = ", paste(title, "Ifile.txt", 
            sep = ""))
        pfile <- paste("PFILE = ", paste(title, "Pfile.txt", 
            sep = ""))
    }
    
    #Calculate peson variable lenghts
    dLen <- rep(NA, ncol(dems))
    for (i in 1:length(dLen)) {
        dLen[i] <- max(nchar(dems[, i]))
    }
    dLengths <- data.frame(names(dems), dLen, first = cumsum(dLen) - 
        dLen + 2, last = cumsum(dLen) + 1)
    dLengths[1, 3] <- dLengths[1, 3] - 1
    demoLengths <- paste(dLengths$first, dLengths$last, sep = "E")
    finalDemScript <- paste(paste("@", names(dems), sep = ""), 
        demoLengths, sep = " = ")
    
    #Control File Name
    cntrlTitle <- paste(as.character(title), "Cntrl", sep = "")
    
    #Warnings/erros
    if(partialCredit == TRUE & length(cod) == 2){
        stop("Partial credit model defined for data with only two categories")
    }
    # if(length(codes) > 2 & !is.null(anchorFile)){
    #     warning(paste("Item structure file may need to be anchored too. Please",
    #         "do so manually (i.e., extract ISFile from Winsteps first and ",
    #         "manually edit the control file prior to analysis with ISFILE = ",
    #         "<filename>",sep = "")) 
    # } 
#================================= Write Files ================================

    l<-list("&INST", ttl, dt, i1, ni, 
        namStart, namLen, wid, cod, totSc, udec, 
        "ifile" = ifelse(exists("ifile"), ifile, ";"), 
        "pfile" = ifelse(exists("pfile"), pfile, ";"),
        "sfile" = ifelse(Sfile == TRUE & length(codes) > 2, 
            paste("SFILE = ", paste(title, "Sfile.txt", sep = "")), ";"),
        "pc" = ifelse(partialCredit == TRUE & length(codes) > 2, "GROUPS = 0",
            ifelse(partialCredit == FALSE & length(codes) > 2, 
                "GROUPS = ' ' ", ";")),
        "af" = ifelse(!is.null(anchorFile),afile,";"),
        "saf" = ifelse(!is.null(sAnchorFile),sAfile,";"),
        "idel" = ifelse(!is.null(idelete), idel, ";"),
        finalDemScript, 
        "&End", colnames(itms), "END NAMES")

    sink(paste(cntrlTitle, "txt", sep = "."))
        for(i in 1:length(l)){
            cat(l[[i]],sep = "\n")
        }
    sink()
return(match.call())
}
