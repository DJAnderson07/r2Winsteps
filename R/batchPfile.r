
#' Batch Read Winsteps Person Files (pfile) Into R
#'
#' This function looks at a directory, identifies all the pfiles in the 
#'  directory based on a common string pattern, and returns a list with all 
#'  person files (i.e., each element of the list represents a different pfile).
#'  Alternatively, a string vector of files to read in can be supplied. Note 
#'  that this function assumes standard .txt files. For more information on 
#'  Winsteps item files, see \url{http://www.winsteps.com/winman/pfile.htm}.
#'
#' @param demNameL Optional list containing the names of the demographic 
#'   variables for pfiles. Must equal the number of demographic fields being 
#'   read into R. 
#' @param dir Directory where pfiles are saved. Defaults to current working 
#'   directory.
#' @param files Optional character vector of the names of the person files to be
#'   read into R. Note that only the names of the files (not the extension) are
#'   neccessary. The function assumes .txt files.
#' @param pat A common character string in all item files. Only used if 
#'   \code{files = NULL}, Defaults to "Pfile", which is what is automatically 
#'   generated when \code{link{r2Winsteps}} is called.
#' @param r2WinstepsFile Logical. Was the \code{link{r2Winsteps}} function 
#'   called to produce the person files? Defaults to TRUE. Note that if FALSE 
#'   all demographic variables will be read into a single variable.
#' @param varWidths Width of the person file columns. These widths correspond to 
#'   all but the final person demographic columns. If you are using the current
#'   version of Winsteps, and have not requested any additional columns be
#'   represented in the item file, these shouldn't need to be changed. Older 
#'   versions of Winsteps (e.g., 3.6.8) may need the following widths \code{c(1, 
#'   5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6)}. Note that the first width 
#'   of 1 is included to parse comments from item files (e.g., extreme scores). 
#'   This column is dropped because it is redundant and also represented in the 
#'   \code{status} column. 
#' @param pFileNames The column names for the person files being read in. These 
#'   correspond to the names of all but the person variable names. Note that
#'   the \code{length(pFileNames)} must equal \code{length(varWidths)}. The
#'   \code{pFileNames} can be changed for personal preference or to accomodate  
#'   older versions of Winsteps. For example, version 3.6.8 names might be 
#'   \code{c("Dropped", "Entry", "Theta", "Status", "Count", "RawScore", "SE", 
#'   "Infit", "Infit_Z", "Outfit", "Outfit_Z", "Displacement", 
#'   "PointMeasureCorr", "Weight", "ObservMatch", "ExpectMatch")}.
#'
#' @export
#'
#' @return List of person files, with each element of the list representing a 
#'   separate person file.

batch.pfile <- function(demNameL = NULL, dir = getwd(), files = NULL, 
    pat = "Pfile", r2WinstepsFile = TRUE,  
    varWidths = c(1, 5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 8),
    pFileNames = c("Dropped", "Entry", "Theta", "Status", "Count", "RawScore", 
        "SE", "Infit", "Infit_Z", "Outfit", "Outfit_Z", "Displacement", 
        "PointMeasureCorr", "Weight", "ObservMatch", "ExpectMatch", 
        "PointMeasureExpected", "RMSR", "WMLE")) {

    oldDir <- getwd()
    setwd(dir)
    
    if (!is.null(files)) {
        files <- paste(files, ".txt", sep = "")
    }

    if (is.null(files)) {
        files <- list.files(pattern = as.character(pat))
    }
    
    widthL <- vector("list", length(files))
    for (i in 1:length(files)) {
        widthL[[i]] <- c(varWidths, 1000)
    }
    
    pfile <- vector("list", length(files))
    for (i in 1:length(files)) {
        pfile[[i]] <- read.fwf(files[[i]], widthL[[i]], skip = 2)
    }
 
    if (r2WinstepsFile == TRUE) {
        demFileNames <- paste("demFile", 1:length(files), ".txt", sep = "")
        
        for (i in 1:length(files)) {
            cat(as.character(pfile[[i]][ ,ncol(pfile[[i]])]), sep = "\n", 
            	file = demFileNames[i])
        }
        
        demFiles <- vector("list", length(files))
        for (i in 1:length(demFiles)) {
            demFiles[[i]] <- read.table(demFileNames[i], sep = "|", quote = "")
        }
        invisible(file.remove(demFileNames))
        
        for (i in 1:length(pfile)) {
            pfile[[i]] <- cbind(pfile[[i]][, -ncol(pfile[[i]])], demFiles[[i]])
        }
    }
    
    pFileNamesL <- vector("list", length(files))
    
    if (length(demNameL) > 0) {
        for (i in 1:length(pFileNamesL)) {
            pFileNamesL[[i]] <- c(pFileNames, demNameL[[i]])
        }
    } else {
        for (i in 1:length(pFileNamesL)) {
            pFileNamesL[[i]] <- c(pFileNames, 
                paste("v",(length(pFileNames) + 1):ncol(pfile[[i]]), sep = ""))
        }

    }
    
    if (r2WinstepsFile == TRUE) {
        for (i in 1:length(files)) {
            names(pfile[[i]]) <- pFileNamesL[[i]]
        }
    }
    
    if (r2WinstepsFile == FALSE) {
        for (i in 1:length(pfile)) {
            names(pfile[[i]]) <- c(pFileNamesL[[i]][1:(ncol(pfile[[i]]) - 1)], 
            	"NAME")
        }
    }
    
    pfile <- lapply(pfile, function(x) {
        x <- x[, -1]
        return(x)
    })
    names(pfile) <- substr(files, 1, (nchar(files)) - 4)

    on.exit(setwd(oldDir), add = TRUE)
    if(length(pfile) == 1) {
        return(pfile[[1]])
    }
    return(pfile)
}