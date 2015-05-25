
#Make batch processing possible (r2Winsteps code needs to be updated). Also
#consider editing the other commands so this can be the single analysis case.
#Also consider adding file.remove() to remove Winsteps files and reduce clutter.
#Add "keep" argument to supress file.remove().

runWinsteps<-function(itms, dems, ...){
	call<-r2Winsteps(itms, dems, ...)
	
	callTitle<-as.list(call)$title

	batchWinsteps(callTitle)

	demNames<-names(dems)

	batFile<-paste(callTitle, ".bat", sep = "")

	system(paste("open", batFile))

	repeat{
		Sys.sleep(.1)
		
		pfileName<-paste(callTitle,"Pfile.txt", sep = "")
		ifileName<-paste(callTitle,"Ifile.txt", sep = "")

		if(file.exists(pfileName) == TRUE & 
		   file.exists(ifileName) == TRUE) 
			
			break
		}

	p<-batch.pfile(list(demNames))
	i<-batch.ifile()	
return(list("ItemParameter" = i, "Person Parameters" = p))
}

