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
#' @param anchorFile Optional name of an anchor file to be included in the
#'   analysis, see \code{\link{write.anchor}}
#' @param ifile Logical. Should item files be returned? Defaults to TRUE. 
#' @param pfile Logical, default to TRUE. Should person files be returned?
#' @param format Format in which item and person files should be returned. 
#'   Takes values "txt" and "XLS" to return txt and XLS files, respectively. 
#'   Defaults to "txt". Note that "txt" must be used if subsequent calls to 
#'   batch.ifile/batch.pfile are used to read thefiles back into R.
#' @export
#' @return Control and Data file for analysis with Winsteps.

##### Build in capability for partial credit model

r2Winsteps<-function (itms, dems, partialCredit = FALSE, anchorFile = NULL, 
    title = "r2Winsteps", Ifile = TRUE, Pfile = TRUE, format = "txt"){

#=============================== Write Data file ===============================
	#Convert itms to character, code missing data as "M"
    itms <- apply(itms, 2, as.character)
    for (i in 1:ncol(itms)) {
        itms[, i] <- ifelse(is.na(itms[, i]), "M", itms[, i])
    }
    
    #Find the column with the maximium width
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
    codes <- codes[-grep("M", codes)]
    
    #Paste all items in one character string
    istring <- paste(itms[, 1], itms[, 2], sep = "")
    for (i in 3:ncol(itms)) {
        istring <- paste(istring, itms[, i], sep = "")
    }
    
    #Convert all dems columns to string; pad to largest column
    for (i in 1:ncol(dems)) {
        dems[, i] <- as.character(dems[, i])
        dems[, i] <- stringr::str_pad(dems[, i], max(nchar(dems[, 
            i])), side = "right")
    }
    
    #Paste demos into a single string
    dstring <- paste(dems[, 1], dems[, 2], sep = " | ")
    for (i in 3:ncol(dems)) {
        dstring <- paste(dstring, dems[, i], sep = " | ")
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
    
#============================= Write Control File ==============================
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
    afile<-paste("IAFILE = ", anchorFile, sep = "")

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
    
#================================= Write Files =================================
    if (Ifile == TRUE & Pfile == TRUE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, ifile, pfile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, ifile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, pfile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }

    if (partialCredit == TRUE & length(codes) == 2) {
        warning(paste("Control file not written. Only two item categories", 
            "were detected, but partialCredit was specified as TRUE"))
    }  

    if(length(codes) > 2 & length(anchorFile) > 0){
        warning(paste("Item structure file may need to be anchored too. Please",
            "do so manually (i.e., extract ISFile from Winsteps first and ",
            "manually edit the control file prior to analysis with ISFILE = ",
            "<filename>",sep = "")) 
    }

    if (Ifile == TRUE & Pfile == TRUE & length(codes) > 2 & 
           partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, ifile, pfile, "GROUPS = 0", sep = "\n"), 
                cat(finalDemScript, "&End", sep = "\n"), 
                cat(colnames(itms), "END NAMES", sep = "\n")
                )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod,
                        totSc, ifile, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, pfile, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == TRUE & length(codes) > 2 & 
           partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, ifile, pfile, "GROUPS = ' ' ", sep = "\n"), 
                cat(finalDemScript, "&End", sep = "\n"), 
                cat(colnames(itms), "END NAMES", sep = "\n")
                )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, ifile, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, pfile, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) == 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod,
                        totSc, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == TRUE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, ifile, pfile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, ifile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, pfile, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) == 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile,otSc, sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == TRUE & length(codes) > 2 & 
           partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, ifile, pfile, "GROUPS = 0", sep = "\n"), 
                cat(finalDemScript, "&End", sep = "\n"), 
                cat(colnames(itms), "END NAMES", sep = "\n")
                )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod,
                        totSc, afile, ifile, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, pfile, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, "GROUPS = 0", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == TRUE & Pfile == TRUE & length(codes) > 2 & 
           partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, ifile, pfile, "GROUPS = ' ' ", sep = "\n"), 
                cat(finalDemScript, "&End", sep = "\n"), 
                cat(colnames(itms), "END NAMES", sep = "\n")
                )
            sink()
    }
    if (Ifile == TRUE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, ifile, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == TRUE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod, 
                        totSc, afile, pfile, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
    if (Ifile == FALSE & Pfile == FALSE & length(codes) > 2 & 
            partialCredit == FALSE & length(anchorFile) > 0) {
            sink(paste(cntrlTitle, "txt", sep = "."))
                cat(
                    cat("&INST", ttl, dt, i1, ni, namStart, namLen, wid, cod,
                        totSc, afile, "GROUPS = ' ' ", sep = "\n"), 
                    cat(finalDemScript, "&End", sep = "\n"), 
                    cat(colnames(itms), "END NAMES", sep = "\n")
                    )
            sink()
    }
}
