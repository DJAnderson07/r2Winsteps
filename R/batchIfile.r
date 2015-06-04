#' Batch Read Winsteps Item Files (ifile) Into R (Winsteps 3.6.8)
#'
#' This function looks at a directory, identifies all the ifiles in the 
#'   directory based on a common string pattern, and returns a list with all 
#'   item files (i.e., each element of the list represents a different pfile).
#'   Alternatively, a list of file names to be imported can be supplied. For
#'   more information on Winsteps item files, see 
#'   \url{http://www.winsteps.com/winman/ifile.htm}.
#'
#' @param dir The directory where the item files are saved.
#' @param pattern A common character string in all item files. Only used if 
#'   \code{files = NULL}, Defaults to "Ifile", which is what is automatically 
#'   generated when \code{link{r2Winsteps}} is called.
#' @param files Optional character vector of the names of the item files to be
#'   read into R. Note that only the names of the files (not the extension) are
#'   neccessary. The function assumes .txt files.
#' @param varWidths Width of the item file columns. If you are using the current
#'   version of Winsteps, and have not requested any additional columns be 
#'   represented in the item file, these shouldn't need to be changed. Older 
#'   versions of Winsteps (e.g., 3.6.8) may need the following widths \code{c(1, 
#'   5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 8, 7, 7, 6, 6, 7, 2, 2, 100)}. Note that the 
#'   first width of 1 is included to parse comments from item files (e.g., 
#'   extreme scores). This column is dropped because it is redundant and also 
#'   represented in the \code{status} column.
#' @param iFileNames The column names for the item files being read in. Note 
#'   that the \code{length(pFileNames)} must equal \code{length(varWidths)}.
#'   iFileNames can be changed for personal preference and to accomodate older 
#'   versions of Winsteps. For example, version 3.6.8 names might be 
#'   \code{c("Dropped", "Entry", "Difficulty", "Status", "Count", "RawScore", 
#'   "SE", "Infit", "Infit_Z", "Outfit", "Outfit_Z", "Displacement", 
#'   "PointMeasureCorr", "Weight", "ObservMatch", "ExpectMatch", 
#'   "PostHocDiscrim", "Group", "Model", "ItemID")}.
#' @export
#' @return List of Winsteps ifiles, where each element of the list represents a 
#'   different ifile.

batch.ifile <- function(dir = getwd(), pattern = "Ifile", files = NULL, 
	varWidths = c(1, 5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 8, 2, 2, 
        2, 100),
	iFileNames = c("Dropped", "Entry", "Difficulty", "Status", "Count", 
        "RawScore", "SE", "Infit", "Infit_Z", "Outfit", "Outfit_Z", 
        "Displacement", "PointMeasureCorr", "Weight", "ObservMatch", 
        "ExpectMatch", "PointMeasureExpected", "RMSR", "WMLE", "Group", "Model", 
        "Recoding", "ItemID")) {

    oldD <- getwd()
    setwd(dir)
    
    if (!is.null(files)) {
        files <- paste(files, ".txt", sep = "")
    }
    
    if (is.null(files)){
        files <- list.files()
        files <- files[grep(as.character(pattern), files)]
    }
    
    ifiles <- vector("list", length(files))
    for (i in 1:length(ifiles)) {
        ifiles[[i]] <- read.fwf(files[i], widths = varWidths, skip = 2, col.names = iFileNames)
    }
    names(ifiles) <- substr(files, 1, nchar(files) - 4)
    
    ifiles <- lapply(ifiles, function(x) x[, -1])
    
    on.exit(setwd(oldD), add = TRUE)
    return(ifiles)
} 