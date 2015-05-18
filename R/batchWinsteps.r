#' Batch Process Winsteps Files
#' 
#' This function looks at a directory and creates a Winsteps batch processing file based on all the control files in the directory.
#' @param batchName The name of the batch file to be saved.
#' @param dir Directory where multiple control files are saved. Defaults to current directory.
#' @param outFileNames Character vector of output file names from Winsteps analyses. Defaults to NULL, and the function creates names based on the names of the control files.
#' @param pattern Common character pattern represented in control files in the directory. Defaults to "Cntrl", which is what is automatically generated when r2Winsteps is called.

batchWinsteps<-function(batchName, dir = getwd(), outFileNames = NULL, pattern = "Cntrl"){
	oldD<-getwd()
	setwd(dir)

	files<-list.files()
	files<-files[grep(as.character(pattern),files)]

	if(length(outFileNames) == 0 & pattern == "Cntrl"){
		outFileNames<-paste(substr(files,1,nchar(files) - 9),"Out",".txt",sep = "")
	}

	if(length(outFileNames) == 0 & pattern != "Cntrl"){
		outFileNames<-paste(substr(files,1,nchar(files) - 4),"Out",".txt",sep = "")
	}
	
	first<-rep("START /w C:\\winsteps\\WINSTEPS BATCH=YES ",length(files))
	
	sink(paste(batchName,"bat",sep = "."))
		cat(paste(first, files, outFileNames, sep = " "),
			"PAUSE",
			sep = "\n")
	sink()

return(setwd(oldD))
}
