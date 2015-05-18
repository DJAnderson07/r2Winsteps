#' Write Data and Control Files for Winsteps From R
#'
#' Function for writing files from R to Winsteps for Rasch analysis.
#'
#' @param itms Dataframe or matrix of items responses
#' @param dems Dataframe or matrix of person identifiers/demographic fields
#' @param title Title of the analysis, which will be used in the name of the data and text files. Defaults to "r2Winsteps".
#' @param ifile Logical, default to TRUE. Should item files be returned?
#' @param pfile Logical, default to TRUE. Should person files be returned?
#' @param format Format in which item and person files should be returned. Takes values "txt" and "XLS" to return txt and XLS files, 
#' respectively. Defaults to "txt". Note that "txt" must be used if subsequent calls to batch.ifile/batch.pfile are used to read the
#' files back into R.
#' @export
#' @return Control and Data file for analysis with Winsteps.

##### Build in capability for partial credit model

r2Winsteps<-function(itms, dems, title = "r2Winsteps", Ifile = TRUE, Pfile = TRUE, format = "txt"){

	##### Write Data
	for(i in 1:ncol(itms)){
		itms[,i]<-ifelse(is.na(itms[,i]),9,itms[,i])
	}
	itms<-apply(itms,2,as.character)

	istring<-paste(itms[,1],itms[,2],sep = "")
	for(i in 3:ncol(itms)){
		istring<-paste(istring,itms[,i],sep = "")
	}

	require(stringr)
	for(i in 1:ncol(dems)){
		dems[,i]<-as.character(dems[,i])
		dems[,i]<-str_pad(dems[,i], max(nchar(dems[,i])),side = "right")
	}

	dstring<-paste(dems[,1],dems[,2], sep = " | ")
	for(i in 3:ncol(dems)){
		dstring<-paste(dstring,dems[,i], sep = " | ")
	}

	d<-data.frame(dems = dstring,responses = istring)
	d$dems<-as.character(d$dems)

	winData<-paste(d$dems,d$responses,sep = " ")
	
	dtaTitle<-paste(as.character(title),"Dta",sep = "")
	sink(paste(dtaTitle,"txt",sep = "."))
	cat(winData,sep = "\n")
	sink()

	

	###### Write Control file
	ttl<-paste("TITLE = ",title,sep = "")
		dtaTitle<-paste(as.character(title),"Dta",sep = "")
	dt<-paste("DATA = ",paste(dtaTitle,"txt",sep = "."))
	i1<-paste("ITEM1 = ",nchar(dstring)[1] + 2)
	ni<-paste("NI = ",ncol(itms))
	namStart<-"NAME1 = 1"
	namLen<-paste("NAMLEN = ",nchar(dstring)[1])
	wid<-"XWIDE = 1"
	cod<-"CODES = 01"
	totSc<-"TOTALSCORE = YES"
	
	if(format == "XLS"){
		ifile<-paste("IFILE = ",paste(title,"Ifile.xls",sep = ""))
		pfile<-paste("PFILE = ",paste(title,"Pfile.xls",sep = ""))
	}
	
	if(format == "txt"){
		ifile<-paste("IFILE = ",paste(title,"Ifile.txt",sep = ""))
		pfile<-paste("PFILE = ",paste(title,"Pfile.txt",sep = ""))
	}

	#CREATE PERSON LABELS
	dLen<-rep(NA,ncol(dems))
	for(i in 1:length(dLen)){
		dLen[i]<-max(nchar(dems[,i]))
	}

	dLengths<-data.frame(names(dems),dLen,first = cumsum(dLen) - dLen + 2,last = cumsum(dLen) + 1)
		dLengths[1,3]<-dLengths[1,3] - 1
	demoLengths<-paste(dLengths$first,dLengths$last,sep = "E")

	finalDemScript<-paste(
		paste("@",names(dems),sep = ""),
		demoLengths,sep = " = ")

	#Combine all elements into final control file
	cntrlTitle<-paste(as.character(title),"Cntrl",sep = "")
	
	if(Ifile == TRUE & Pfile == TRUE){
		sink(paste(cntrlTitle,"txt",sep = "."))
			cat(
				cat("&INST",
					ttl,
					dt,
					i1,
					ni,  
					namStart,
					namLen,
					wid,
					cod,
					totSc,
					ifile,
					pfile,
				sep = "\n"),
				cat(finalDemScript,"&End",sep = "\n"),
				cat(colnames(itms),"END NAMES",sep = "\n")
			)
		sink()	
	}

	if(Ifile == TRUE & Pfile == FALSE){
		sink(paste(cntrlTitle,"txt",sep = "."))
			cat(
				cat("&INST",
					ttl,
					dt,
					i1,
					ni,  
					namStart,
					namLen,
					wid,
					cod,
					totSc,
					ifile,
				sep = "\n"),
				cat(finalDemScript,"&End",sep = "\n"),
				cat(colnames(itms),"END NAMES",sep = "\n")
			)
		sink()	
	}

	if(Ifile == FALSE & Pfile == TRUE){
		sink(paste(cntrlTitle,"txt",sep = "."))
			cat(
				cat("&INST",
					ttl,
					dt,
					i1,
					ni,  
					namStart,
					namLen,
					wid,
					cod,
					totSc,
					pfile,
				sep = "\n"),
				cat(finalDemScript,"&End",sep = "\n"),
				cat(colnames(itms),"END NAMES",sep = "\n")
			)
		sink()	
	}

	if(Ifile == FALSE & Pfile == FALSE){
		sink(paste(cntrlTitle,"txt",sep = "."))
			cat(
				cat("&INST",
					ttl,
					dt,
					i1,
					ni,  
					namStart,
					namLen,
					wid,
					cod,
					totSc,
				sep = "\n"),
				cat(finalDemScript,"&End",sep = "\n"),
				cat(colnames(itms),"END NAMES",sep = "\n")
			)
		sink()	
	}
}	
