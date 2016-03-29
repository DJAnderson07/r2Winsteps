#' Batch Process Winsteps Files
#' 
#' This function searches the working directory for Winsteps control files and 
#'   creates a batch processing file based on all the control files in the 
#'   directory. Alternatively, a vector of control file names can be supplied,
#'   from which teh batch processing file is constructed.
#' @param batchName The name of the batch file to be saved.
#' @param files Optional vector of the names of all Winsteps control files. 
#'   Note that only the name is requred, not the extension (.txt is assumed). If
#'   argument is not supplied, the function will search the working directory
#'   files with a common string pattern.
#' @param dir Directory where multiple control files are saved. Defaults to 
#'   current directory.
#' @param outFileNames Character vector of output file names from Winsteps 
#'   analyses. Defaults to NULL, and the function creates names based on the 
#'   names of the control files.
#' @param pattern Common character pattern represented in control files in the 
#'   directory. Defaults to "Cntrl", which is what is automatically generated 
#'   when r2Winsteps is called. Note that if \code{files} argument is supplied, 
#'   \code{pattern} is ignored.

batchWinsteps <- function(batchName, files = NULL, dir = getwd(), 
    outFileNames = NULL, pattern = "Cntrl") {
    
    oldD <- getwd()
    setwd(dir)

#-------------------------------- Define files --------------------------------
    if (!is.null(files)) {
        files <- paste(files, ".txt", sep = "")
    }
    
    if (is.null(files)) {
        files <- list.files()
        files <- files[grep(as.character(pattern), files)]
    }
#------------------------- Define Outifle Names ---------------------------    
    if(!is.null(outFileNames)) {
        outFileNames <- paste(outFileNames, ".txt", sep = "")
    }
    
   if (is.null(outFileNames) & pattern == "Cntrl") {
        outFileNames <- paste(substr(files, 1, nchar(files) - 9), "Out", ".txt", 
            sep = "")
    }
    
    if (is.null(outFileNames) & pattern != "Cntrl") {
        outFileNames <- paste(substr(files, 1, nchar(files) - 4), "Out", ".txt", 
            sep = "")
    }
#------------------------------- Write .bat File ------------------------------
    first <- rep("START /w C:\\winsteps\\WINSTEPS BATCH=YES ", length(files))
    
    sink(paste(batchName, "bat", sep = "."))
    cat(paste(first, files, outFileNames, sep = " "), "EXIT", sep = "\n")
    sink()
#------------------------------------------------------------------------------    
    on.exit(setwd(oldD), add = TRUE)
} 