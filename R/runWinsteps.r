#' Run a Rasch model from R using Winsteps.
#'
#' This function uses the Winsteps software to analyze data, and then reads the
#'   data back into R without leaving  R interface or interacting with the
#'   Winsteps GUI.
#' @param itms Dataframe or matrix of item responses to be analyzed.
#' @param dems Dataframe or matrix of person identifiers/demographic fields.
#' @param keep Logical. Should the external files used to conduct the analysis 
#'   be stored in the working directory? If TRUE, Winsteps control and data 
#'   files will be retained, as well as item and person files in .txt format, 
#'   and the .bat file used to process the data.
#' @param ... Additional arguments passed to \code{\link{r2Winsteps}}.
#' @seealso \code{\link{r2Winsteps}} \code{\link{batchWinsteps}} 
#'   \code{\link{batch.pfile}} \code{\link{batch.ifile}}
#' @export
#' @return List containing the item and person parameters from the given 
#'   analysis.

runWinsteps <- function(itms, dems, keep = FALSE, ...) {
    
    call <- r2Winsteps(itms, dems, ...)
    
    callTitle <- as.list(call)$title
    
    # Check for existing files
    pfileName <- paste(callTitle, "Pfile.txt", sep = "")
    ifileName <- paste(callTitle, "Ifile.txt", sep = "")
    
    if (file.exists(pfileName) == TRUE) {
        invisible(file.remove(pfileName))
        Warning("Previously estimated person file removed.")
    }
    
    if (file.exists(ifileName) == TRUE) {
        invisible(file.remove(ifileName))
        Warning("Previously estimated item file removed.")
    }
    
    batchWinsteps(callTitle)
    
    demNames <- names(dems)
    
    batFile <- paste(callTitle, ".bat", sep = "")
    
    system(paste("open", batFile))
    
    repeat {
        Sys.sleep(0.1)
        
        if (file.exists(pfileName) == TRUE & file.exists(ifileName) == TRUE) 
            
        break
    }
    
    p <- batch.pfile(list(demNames))
    i <- batch.ifile()
    
    if (keep == FALSE) {
        cntrlFile <- paste(callTitle, "Cntrl.txt", sep = "")
        dtaFile <- paste(callTitle, "Dta.txt", sep = "")
        outFile <- paste(callTitle, "Out.txt", sep = "")
        
        file.remove(c(pfileName, ifileName, batFile, cntrlFile, dtaFile, 
        	outFile))
    }
    return(list("Item Parameters" = i, "Person Parameters" = p))
} 