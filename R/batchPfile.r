
#' Batch Read Winsteps Person Files (pfile) Into R
#'
#' This function looks at a directory, identifies all the pfiles in the directory based on a common string pattern, and returns a list with all person files (i.e., each element of the list represents a different pfile). 
#'
#' @param demL List containing the demographic variables for pfiles. Note that the list must be in the same order that person files are read in automatically (otherwise the function will return an error).
#' @param dir Directory where pfiles are saved. Defaults to current working directory.
#' @param r2WinstepsFile Logical. Was the r2Winsteps function called to produce the pfiles? Defaults to TRUE. Note that if FALSE all demographic variables will be read into a single variable.
#'
#' @export
#'
#' @return List of person files, with each element of the lit representing a sererate person file.

batch.pfile<-function(demL, dir = getwd(),r2WinstepsFile = TRUE){ #THINK ABOUT DROPPING DATA FRAME REQUIREMENT
	
	oldDir<-getwd()
	setwd(dir)

	files<-list.files()
	files<-files[grep(as.character(pattern),files)]
		
	require(stringr)
	demL<-lapply(demos,function(x){
		for(i in 1:ncol(x)){
			x[,i]<-as.character(x[,i])
			x[,i]<-str_pad(x[,i], max(nchar(x[,i])), side = "right")
		}
		return(x)	
	})
	
	pVars<-lapply(demL,function(x){			
		v<-rep(NA,ncol(x))
		for(i in 1:ncol(x)){
			v[i]<-max(nchar(x[,i]))
		}
	return(v)
	})


	pFileNamesL<-vector("list",length(demL))
	for(i in 1:length(pFileNamesL)){
		pFileNamesL[[i]]<-c("Dropped", "Entry", "Theta", "Status", "Count", "RawScore", "SE", "Infit", "Infit_Z", "Outfit", 
	  		"Outfit_Z", "Displacement", "PointMeasureCorr", "Weight", "ObservMatch", "ExpectMatch", 
	  		"PointMeasureExpected", "RMSR", "WMLE", names(demL[[i]]))	
	}

	widthL<-vector("list",length(demL))
	for(i in 1:length(demL)){
		widthL[[i]]<-c(1,5,8,3,8,9,7,7,7,7,7,7,7,7,6,6,6,6,8,(sum(2*pVars[[i]] + 1)))
	}
	
	pfile<-vector("list",length(demL))
	for(i in 1:length(demL)){
		pfile[[i]]<-read.fwf(files[[i]], widthL[[i]], skip = 2)
	}

	demFileNames<-paste("demFile",1:length(demL),".txt",sep = "")

	for(i in 1:length(demL)){
		cat(as.character(pfile[[i]][,20]),sep = "\n",file = demFileNames[i])	
	}
	
	demFiles<-vector("list",length(demL))
	for(i in 1:length(demFiles)){
		demFiles[[i]]<-read.table(demFileNames[i], sep = "|", quote = "")
	}
	invisible(file.remove(demFileNames))

	if(r2WinstepsFile == TRUE){
		for(i in 1:length(pfile)){
			pfile[[i]]<-cbind(pfile[[i]][,-20],demFiles[[i]])
			names(pfile[[i]])<-pFileNamesL[[i]]
		}
	}
	if(r2WinstepsFile == FALSE){
		for(i in 1:length(pfile)){
			pfile[[i]]<-cbind(pfile[[i]][,-20],demFiles[[i]])
			names(pfile[[i]])<-c(pFileNamesL[[i]][1:19],"NAME")
		}
	}
	pfile<-lapply(pfile,function(x){
		x[,1]<-ifelse(is.na(x[,1]),0,1)
	return(x)
	})
	names(pfile)<-substr(files,1,(nchar(files)) - 4)

	on.exit(setwd(oldDir), add = TRUE)
return(pfile)
}