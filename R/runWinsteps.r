#' Run a Rasch model from R using Winsteps.
#'
#' This function uses the Winsteps software to analyze data, and then reads the
#'   data back into R without leaving  R interface or interacting with the
#'   Winsteps GUI.
#' @param itms Data frame or matrix of item responses to be analyzed.
#' @param dems Optional data frame or matrix of person identifiers/demographic
#'   fields.
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

runWinsteps <- function(itms, dems = NULL, keep = FALSE, ...) {

    if(is.null(dems)) {
    	call <- r2Winsteps(itms, ...)
        dems <- data.frame(rownames = rownames(itms))
    }
    else {
    	call <- r2Winsteps(itms, dems, ...)
    }
    demNames <- names(dems) 
   

    callTitle <- ifelse(!is.null(as.list(call)$title), as.list(call)$title, 
        "r2Winsteps")
    
    # Check for existing files
    pfileName <- paste(callTitle, "Pfile.txt", sep = "")
    ifileName <- paste(callTitle, "Ifile.txt", sep = "")
    sfileName <- paste(callTitle, "Sfile.txt", sep = "")
    
    if (file.exists(pfileName) == TRUE) {
        invisible(file.remove(pfileName))
        warning("Previously estimated person file removed.")
    }
    
    if (file.exists(ifileName) == TRUE) {
        invisible(file.remove(ifileName))
        warning("Previously estimated item file removed.")
    }
    if (file.exists(sfileName) == TRUE) {
        invisible(file.remove(sfileName))
        warning("Previously estimated structure file removed.")
    }

    
    batchWinsteps(callTitle)
    

    batFile <- paste(callTitle, ".bat", sep = "")
    
    system(paste("open", batFile))
    
    pt <- proc.time()
    pc <- as.list(call)$partialCredit
    outFile <- paste0(callTitle, "Out.txt")

    if(length(pc) != 0) { 
    	if(pc == TRUE) {
    		repeat {
	        Sys.sleep(0.1)
	        
	        if (file.exists(pfileName) & 
	        	file.exists(ifileName) &
	        	file.exists(sfileName)) {
		            pTemp <- readLines(pfileName)
		            iTemp <- readLines(ifileName)
		            sTemp <- readLines(sfileName)
		            split <- strsplit(sTemp[length(sTemp)], " ")[[1]]
	        }
	        if(length(pTemp) == (nrow(dems) + 2) & 
	           	 length(iTemp) == (ncol(itms) + 2) &
					split[length(split)] == names(itms)[ncol(itms)]) {
	        break
				}
			
            if(file.exists(outFile)) {
                if(length(readLines(outFile)) > 0 & 
                        is.na(readLines(outFile)[27])) {
                    stop(paste("Winsteps could not estimate the model. Check ", 
                                outFile, "for details."))
                    }
                }	
	    	}
		}
	}
    if(length(pc) == 0) {
    	repeat {
	        Sys.sleep(0.1)
	        
	        if (file.exists(pfileName) & file.exists(ifileName)) {
	            pTemp <- readLines(pfileName)
	            iTemp <- readLines(ifileName)
	        
	        if(length(pTemp) == (nrow(dems) + 2) & 
	           	 length(iTemp) == (ncol(itms) + 2) ) {
	        break	
		        }
		    }    
		
        if(file.exists(outFile)) {
			if(length(readLines(outFile)) > 0 & 
                    is.na(readLines(outFile)[27])) {
				stop(paste("Winsteps could not estimate the model. Check ", 
							outFile, "for details."))
				}
			}
		}	
	}   

    p <- batch.pfile(list(demNames))
    i <- batch.ifile(pat = substr(ifileName, 1, (nchar(ifileName) - 3)))
    if(file.exists(sfileName)) {
        s <- batch.sfile(pat = substr(sfileName, 1, (nchar(sfileName) - 3)))
    }
    if (keep == FALSE) {
        cntrlFile <- paste0(callTitle, "Cntrl.txt")
        dtaFile <- paste0(callTitle, "Dta.txt")
        if(exists("s")) {
            file.remove(c(pfileName, ifileName, sfileName, batFile, 
            cntrlFile, dtaFile, outFile))
        }
        else{
            file.remove(c(pfileName, ifileName, batFile, 
            cntrlFile, dtaFile, outFile))
        }    
        
    }
    
    if(exists("s")) {
        return(structure(
                list("ItemParameters" = i, 
                     "PersonParameters" = p,
                     "StructureFiles" = s), 
            class = "r2Winsteps"))
    }
    else {
        return(structure(
                list("ItemParameters" = i, 
                     "PersonParameters" = p), 
            class = "r2Winsteps"))
    }

} 