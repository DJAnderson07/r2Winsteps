#' Batch Read Winsteps Structure Files (sfiles) Into R
#' 
#' This function is essentially equivalent to \code{\link{batch.ifile}}, 
#' except that structure files are read into R rather than item files. These
#' are necessary for plotting results from rating scale or partial credit 
#' models. 
#' 
#' @param dir The directory where the item files are saved.
#' @param pat A common character string in all item files. Only used if 
#'   \code{files = NULL}, Defaults to "Ifile", which is what is automatically 
#'   generated when \code{link{r2Winsteps}} is called.
#' @param files Optional character vector of the names of the item files to be
#'   read into R. Note that only the names of the files (not the extension) are
#'   neccessary. The function assumes .txt files.
#' 
#' @export
#' @return List of Winsteps sfiles, where each element of the list represents
#' a different sfile.
#' 
batch.sfile <- function(dir = getwd(), pat = "Sfile", files = NULL) {
    oldD <- getwd()
    setwd(dir)
    
    if (!is.null(files)) {
        files <- paste(files, ".txt", sep = "")
    }

    if (is.null(files)){
        files <- list.files(pattern = pat)
    }

    sfiles <- vector("list", length(files))
    for(i in 1:length(sfiles)) {
    	sfiles[[i]] <- read.table(files[i], skip = 2) 
    	if(ncol(sfiles[[1]]) == 3) {
            colnames(sfiles[[i]]) = c("Item", "Category", "delta")    
        }
        if(ncol(sfiles[[i]]) == 2) {
            colnames(sfiles[[i]]) = c("Category", "delta")    
        }  
    }
    names(sfiles) <- substr(files, 1, (nchar(files) - 4))
    on.exit(setwd(oldD), add = TRUE)
return(sfiles)
}

