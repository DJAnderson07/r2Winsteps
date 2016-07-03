#' Run a Rasch model from R using Winsteps.
#'
#' This function uses the Winsteps software to batch analyze a given list of 
#'   dataframes/matrices. The results are read back into R without leaving the 
#'   R interface or interacting with the Winsteps GUI.
#'   
#' @param itmsL List of dataframes or matrices of item responses to be analyzed.
#' @param demsL List of dataframes or matrices of person identifiers/demographic 
#'   fields.
#' @param titleL Optional list of names for control and datafiles written to the
#'   working directory. 
#' @param keep Character vector of the external Winsteps files to retain. 
#'   Possible arguments are \code{c("all", "none", "iFile", "pFile","cntrlFile", 
#'   "dtaFile", "outFile", "bat")}. If \code{"all"} or \code{"none"} are 
#'   supplied, no other arguments are neccessary. All other arguments can be 
#'   combinded to select a specific set of files to keep. Other arguments 
#'   correspond to, respectively, item, person, control, data, output, and the 
#'   .bat (batch processing) files. Defaults to "none", in which case none of 
#'   the files used to run the analyses are retained. 
#' @param ... Additional arguments passed to \code{\link{batch_r2Winsteps}}.
#' @seealso \code{\link{batch_r2Winsteps}} \code{\link{batchWinsteps}} 
#'   \code{\link{batch.pfile}} \code{\link{batch.ifile}}
#' @export
#' @return List containing all item and person parameters from the given 
#'   analyses.

batchRunWinsteps <- function(itmsL, demsL, titleL = NULL, keep = "none", ...) {

#---------- Check for previously run analyses & remove i and p files ----------
#Create name vectors of all files
    if(!is.null(titleL)){
        pFileNameV <- rep(NA, length(itmsL))
        iFileNameV <- rep(NA, length(itmsL))
       # sFileNameV <- rep(NA, length(itmsL))
        cntrlFileV <- rep(NA, length(itmsL)) # used later for keep = FALSE
        dtaFileV <- rep(NA, length(itmsL))   #
        outFileV <- rep(NA, length(itmsL))   #
    
        for(i in 1:length(itmsL)){
            pFileNameV[i] <- paste(titleL[i], "Pfile.txt", sep = "")
            iFileNameV[i] <- paste(titleL[i], "Ifile.txt", sep = "")
           # sFileNameV[i] <- paste(titleL[i], "Sfile.txt", sep = "")
            cntrlFileV[i] <- paste(titleL[i], "Cntrl.txt", sep = "") #
            dtaFileV[i] <- paste(titleL[i], "Dta.txt", sep = "")     #
            outFileV[i] <- paste(titleL[i], "Out.txt", sep = "")     #
        }
    }

    if(is.null(titleL)){
        pFileNameV <- rep(NA, length(itmsL))
        iFileNameV <- rep(NA, length(itmsL))
       # sFileNameV <- rep(NA, length(itmsL))
        cntrlFileV <- rep(NA, length(itmsL)) #
        dtaFileV <- rep(NA, length(itmsL))   #
        outFileV <- rep(NA, length(itmsL))   #
    
        for(i in 1:length(itmsL)){
            pFileNameV[i] <- paste("r2Winsteps", i, "Pfile.txt", sep = "")
            iFileNameV[i] <- paste("r2Winsteps", i, "Ifile.txt", sep = "")
         #   sFileNameV[i] <- paste("r2Winsteps", i, "Sfile.txt", sep = "")
            cntrlFileV[i] <- paste("r2Winsteps", i, "Cntrl.txt", sep = "") #
            dtaFileV[i]   <- paste("r2Winsteps", i, "Dta.txt", sep = "")   #
            outFileV[i]   <- paste("r2Winsteps", i, "Out.txt", sep = "")   #
        }
    }
    
    if(any(sapply(iFileNameV, file.exists))) {
        warning("Previously estimated item file(s) removed.")
    }
    
    for(i in 1:length(iFileNameV)){
        if (file.exists(iFileNameV[i]) == TRUE) {
            invisible(file.remove(iFileNameV[i]))
        }
    }

    if(any(sapply(pFileNameV, file.exists))) {
        warning("Previously estimated person file(s) removed.")
    }
    for(i in 1:length(pFileNameV)){
        if (file.exists(pFileNameV[i]) == TRUE) {
            invisible(file.remove(pFileNameV[i]))
        }
    }

    # if(any(sapply(sFileNameV, file.exists))) {
    #     warning("Previously estimated structure file(s) removed.")
    # }
    
    # for(i in 1:length(sFileNameV)){
    #     if (file.exists(sFileNameV[i]) == TRUE) {
    #         invisible(file.remove(sFileNameV[i]))
    #     }
    # }


#------------------------ Get files ready for analysis ------------------------
    if(is.null(titleL)){
        lastPfileName <- paste("r2Winsteps", length(itmsL), "Pfile.txt", 
            sep = "")
        lastIfileName <- paste("r2Winsteps", length(itmsL), "Ifile.txt", 
            sep = "")
     #   lastSfileName <- paste("r2Winsteps", length(itmsL), "Sfile.txt", 
     #      sep = "")
    }
    else{
        lastPfileName <- paste(titleL[[length(titleL)]], "Pfile.txt", sep = "")
        lastIfileName <- paste(titleL[[length(titleL)]], "Ifile.txt", sep = "")
     #   lastSfileName <- paste(titleL[length(titleL)], "Sfile.txt", sep = "")  
    }

    demNamesL <- lapply(demsL, names)
 
 #-------------------------------- run analysis -------------------------------
    call <- batch_r2Winsteps(itmsL, demsL, titleL = titleL, ...)
    batName <- ifelse(is.null(as.list(call)$batName), "r2WinstepsBatch", 
        as.list(call)$batName)
    batFile <- paste(batName, ".bat", sep = "")
    
    system(paste("open", batFile))
    
    repeat {
        Sys.sleep(0.1)
        
        if(file.exists(lastPfileName) & 
            file.exists(lastIfileName)) { #&
           # file.exists(lastSfileName)) {
                pTemp <- readLines(lastPfileName)
                iTemp <- readLines(lastIfileName)
            #    sTemp <- readLines(lastSfileName)
            #    split <- strsplit(sTemp[length(sTemp)], " ")[[1]]
            if(length(pTemp) == (nrow(demsL[[length(demsL)]]) + 2) & 
                 length(iTemp) == (ncol(itmsL[[length(itmsL)]]) + 2)) { #&
                #    split[length(split)] == names(itms)[ncol(itms)]) {
                break
            }
        }
    }
#------------------------------- Import results -------------------------------
    p <- batch.pfile(demNameL = demNamesL, 
            files = substr(pFileNameV, 1, nchar(pFileNameV) - 4))
    i <- batch.ifile(files = substr(iFileNameV, 1, nchar(iFileNameV) - 4))
  #  s <- batch.sfile(files = substr(sFileNameV, 1, nchar(sFileNameV) - 4))
    
    
    pars <- vector("list", length(p))

    for(loop in seq_along(pars)) {
        pars[[loop]] <- structure(
                    list("ItemParameters" = i[[loop]], 
                         "PersonParameters" = p[[loop]]), 
                class = "r2Winsteps")
    }
    names(pars) <- substr(names(i), 1, nchar(names(i)) - 5)

    
#----------------------- Remove files, if requested -----------------------    
    
    keepL <- list("iFile" = iFileNameV, "pFile" = pFileNameV, 
                #  "sFile" = sFileNameV, 
                  "cntrlFile" = cntrlFileV, 
                  "dtaFile" = dtaFileV, "outFile" = outFileV, "bat" = batFile)
     
    suppressWarnings(
        if (keep == "all") {
            keep <- c("iFile", "pFile", #"sfile", 
                "cntrlFile", "dtaFile", 
                      "outFile", "bat")
        }
    )
    m <- names(keepL) %in% keep
 
    remove <- keepL[!m]

    invisible(sapply(remove, file.remove))

return(pars)
} 