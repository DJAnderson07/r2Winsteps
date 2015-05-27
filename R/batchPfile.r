
#' Batch Read Winsteps Person Files (pfile) Into R
#'
#' This function looks at a directory, identifies all the pfiles in the directory based on a common string pattern, and returns a list with all person files (i.e., each element of the list represents a different pfile). 
#'
#' @param demNameL Optional list containing the names of the demographic variables for pfiles.
#' @param dir Directory where pfiles are saved. Defaults to current working directory.
#' @param r2WinstepsFile Logical. Was the r2Winsteps function called to produce the pfiles? Defaults to TRUE. Note that if FALSE all demographic variables will be read into a single variable.
#'
#' @export
#'
#' @return List of person files, with each element of the lit representing a sererate person file.

batch.pfile <- function(demNameL = NULL, dir = getwd(), pattern = "Pfile", 
	r2WinstepsFile = TRUE) {
    
    oldDir <- getwd()
    setwd(dir)
    
    files <- list.files()
    files <- files[grep(as.character(pattern), files)]
    
    widthL <- vector("list", length(files))
    for (i in 1:length(files)) {
        widthL[[i]] <- c(1, 5, 8, 3, 8, 9, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 1000)
    }
    
    pfile <- vector("list", length(files))
    for (i in 1:length(files)) {
        pfile[[i]] <- read.fwf(files[[i]], widthL[[i]], skip = 2)
    }
    
    if (r2WinstepsFile == TRUE) {
        demFileNames <- paste("demFile", 1:length(files), ".txt", sep = "")
        
        for (i in 1:length(files)) {
            cat(as.character(pfile[[i]][, ncol(pfile[[i]])]), sep = "\n", 
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
            pFileNamesL[[i]] <- c("Dropped", "Entry", "Theta", "Status", 
            	"Count", "RawScore", "SE", "Infit", "Infit_Z", "Outfit", 
            	"Outfit_Z", "Displacement", "PointMeasureCorr", "Weight", 
            	"ObservMatch", "ExpectMatch", demNameL[[i]])
        }
    } else {
        for (i in 1:length(pFileNamesL)) {
            pFileNamesL[[i]] <- c("Dropped", "Entry", "Theta", "Status", 
            	"Count", "RawScore", "SE", "Infit", "Infit_Z", "Outfit", 
            	"Outfit_Z", "Displacement", "PointMeasureCorr", "Weight", 
            	"ObservMatch", "ExpectMatch", 
            	paste("v", 17:ncol(pfile[[i]]), sep = ""))
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
    return(pfile)
}