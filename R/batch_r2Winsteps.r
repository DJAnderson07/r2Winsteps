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
    if (is.null(titleL) & length(grep("Cntrl", list.files() )) > 0) {
        stop(paste("Other control files in the directory detected.", 
        	"Please remove these files from the directory or supply argument", 
            "titleL."))
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
            r2Winsteps(itmsL[[i]], demsL[[i]], anchorFile = anchorFileL[[i]], 
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
                anchorFile = anchorFileL[[i]], title = titleL[[i]], ...)
        }
    }

#Return anchor file check if anchorFileL is supplied.
    if(!is.null(anchorFileL)){
        cat("The following anchor files were used in the corresponding analyses:",
            paste("Anchor File", "Analysis Title", sep = " == "), sep = "\n")
        analysisAnchor<-vector("list", length(itmsL))

        if(is.null(titleL)){
            for(i in 1:length(analysisAnchor)){
                analysisAnchor[[i]] <- paste(anchorFileL[[i]],
                                            paste("r2Winsteps",i, sep = ""),  
                                        sep = " == ")
            cat(analysisAnchor[[i]], sep = "\n")
            }
        }
        if(!is.null(titleL)){
            for(i in 1:length(analysisAnchor)){
                analysisAnchor[[i]] <- paste(anchorFileL[[i]],
                                            titleL[[i]], 
                                        sep = " == ")
            cat(analysisAnchor[[i]], sep = "\n")
            }
        }    
    }

#Write .bat file
        if (!is.null(titleL) & any(grep("Cntrl", list.files()) > length(itmsL))) {
            batchWinsteps(batName, 
                files = paste(unlist(titleL), "Cntrl", sep = ""),
                outFileNames = paste(titleL, "Out", sep = ""))
    }   else {
        batchWinsteps(batName)
    }

return(match.call())
} 
