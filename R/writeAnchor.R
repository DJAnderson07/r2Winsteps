#' Write anchor files for equating scales with common items.
#'
#' This function takes two primary argument: The raw dataframe of items to be 
#'   analyzed, \code{rawItmsDta}, and an item file with item difficulties 
#'   previously estimated, \code{eqIfile}. Note that the item identifiers in 
#'   \code{eqIfile} must match the \code{names} of the items in 
#'   \code{rawItmsDta}. The function will then write an anchor file with the 
#'   common items specified as having a fixed item difficulty, according to 
#'   their value in \code{eqIfile}.
#'
#' @param rawItmsDta Raw dataframe of items to be analyzed. Must not contain
#'   any non-item response columns. The \code{names} of the items must match
#'   the item identifier in the item file (second argument in the function).
#' @param eqIfile An item file (ifile) from a previously run model, which 
#'   includes common items with the raw data file. The raw data file items will
#'   be estimated relative to the estimated item difficulties of these common
#'   items.
#' @param anchorFileName Name of the anchor file to be written. Defaults to 
#'   "AnchorFile".
#' @param eqIDcol The column name or number in the item file that includes item 
#'   identifiers. Defaults to the final column.
#' @param eqDiffcol The column that item difficulties reside in the item file. 
#'   Defaults to "Difficulty", which is the name of the column if r2Winsteps()
#'   was run to produce the ifile.
#' @export
#' @return Item anchor file for equating scales with common items.

write.anchor <- function(rawItmsDta, eqIfile, anchorFileName = "AnchorFile", 
	eqIDcol = ncol(eqIfile), eqDiffcol = "Difficulty") {
    
    raw <- data.frame(row.names = names(rawItmsDta), 
    	rawEntry = 1:ncol(rawItmsDta))
    eq <- merge(eqIfile, raw, by.x = eqIDcol, by.y = 0)
    
    anchor <- eq[, c("rawEntry", as.character(eqDiffcol))]
    anchor <- anchor[order(anchor$rawEntry), ]
    
    write.table(anchor, file = paste(anchorFileName, ".txt", sep = ""), 
    	row.names = FALSE, col.names = FALSE)
}
