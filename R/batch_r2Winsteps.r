#' Write Winsteps control and data files as a batch.
#'
#' Function takes the typical arguments supplied by \code{\link{r2Winsteps}} in 
#'   the form of a list, and writes out Winsteps control and data files for each 
#'   element of the list. See also \code{\link{r2Winsteps}}.
#'
#' @param itmsL List of dataframes or matrices containing only the item 
#'   responses. Each element of the list should contain a seperate dataframe to
#'   be processed.
#' @param demsL List of dataframes or matrices containing person 
#'   identifiers/demographic fields.

#' @param ... Additional argument supplied to \code{\link{r2Winsteps}}. Note 
#'   that any additional arguments must be supplied in the form of a list.
#' @export
#' @return Control and data files for all dataframes/matrices in the given 
#'   lists, as well as a .bat file to batch process the files in Winsteps.

batch_r2Winsteps <- function(itmsL, demsL, batName = "r2WinstepsBatch", 
	partialCreditL = NULL, anchorFileL = NULL, titleL = NULL, ...) {
    
    # Check for other files with 'Cntrl' in the directory
    if (length(grep("Cntrl", list.files()) > 0)) {
        warning(paste("Other control files detected in the directory.", 
        	"If you are overwriting these files, you can ignore this message.", 
            "Otherwise, these files will be included in the .bat file and", 
            "processed by Winsteps. Please remove these files from the", 
            "directory if this is not what you intended."))
    }
    
    if (length(itmsL) != length(demsL)) {
        stop(paste("Length of item response list does not equal the length of", 
        	"the person demographic list"))
    }
    
    # Make title list, if none is provided
    if (is.null(titleL)) {
        titleL <- vector("list", length(itmsL))
        for (i in 1:length(itmsL)) {
            titleL[[i]] <- paste("r2Winsteps", i, sep = "")
        }
    }
    
#----------------- Run specific analysis depending on inputs #------------------
    if (is.null(partialCreditL) & is.null(anchorFileL)) {
        for (i in 1:length(itmsL)) {
            r2Winsteps(itmsL[[i]], demsL[[i]], title = titleL[[i]], ...)
        }
    }
    if (is.null(partialCreditL) & !is.null(anchorFileL)) {
        for (i in 1:length(itmsL)) {
            r2Winsteps(itmsL[[i]], demsL[[i]], anchorfile = anchorFileL[[i]], 
            	title = titleL[[i]], ...)
        }
    }
    if (!is.null(partialCreditL) & is.null(anchorFileL)) {
        for (i in 1:length(itmsL)) {
            r2Winsteps(itmsL[[i]], demsL[[i]], 
            	partialCredit = partialCreditL[[i]], title = titleL[[i]], ...)
        }
    }
    if (!is.null(partialCreditL) & !is.null(anchorFileL)) {
        for (i in 1:length(itmsL)) {
            r2Winsteps(itmsL[[i]], demsL[[i]], 
            	partialCredit = partialCreditL[[i]], 
                anchorfile = anchorFileL[[i]], title = titleL[[i]], ...)
        }
    }
    
    # Write .bat file
    batchWinsteps(batName)
return(match.call())
} 
