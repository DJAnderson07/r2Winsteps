#' Batch Read Winsteps Item Files (ifile) Into R (Winsteps 3.6.8)
#'
#' This function looks at a directory, identifies all the ifiles in the 
#'   directory based on a common string pattern, and returns a list with all 
#'   item files (i.e., each element of the list represents a different pfile). 
#'
#' @param dir The directory where the item files are saved.
#' @param pattern A common character string in all item files. Only used if 
#'   \code{files = NULL}, Defaults to "Ifile", which is what is automatically 
#'   generated when \code{link{r2Winsteps}} is called.
#' @param files Optional character vector of the names of the person files to be
#'   read into R. Note that only the names of the files (not the extension) are
#'   neccessary. The function assumes .txt files.
#' @param varWidths Width of the item file columns. This should only need to be 
#'   modified if you have an older version of Winsteps. 
#' @param iFileNames The column names for the item files being read in. Note 
#'   that the \code{length(pFileNames)} must equal \code{length(varWidths)}.
#' @export
#' @return List of Winsteps ifiles, where each element of the list represents a 
#'   different ifile.

batch.ifile <- function(dir = getwd(), pattern = "Ifile", files = NULL, 
	varWidths = c(1, 5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 8, 7, 7, 6, 6, 7, 2, 2, 100), 
	iFileNames = c("Dropped", "Entry", "Difficulty", "Status", "Count", 
		"RawScore", "SE", "Infit", "Infit_Z", "Outfit", "Outfit_Z", 
		"Displacement", "PointMeasureCorr", "Weight", "ObservMatch", 
		"ExpectMatch", "PostHocDiscrim", "Group", "Model", "ItemID")) {
    
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