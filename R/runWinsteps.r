
#Make batch processing possible (r2Winsteps code needs to be updated). Also
#consider editing the other commands so this can be the single analysis case.
#Also consider adding file.remove() to remove Winsteps files and reduce clutter.
#Add "keep" argument to supress file.remove().

runWinsteps<-function(itms, dems, ...){
	r2Winsteps(itms, dems, ...)
	batchWinsteps("r2WinstepsBatch")

	demNames<-names(dems)

	system(paste("open", "r2WinstepsBatch.bat"))

	repeat{
		Sys.sleep(.1)
		
		if(file.exists("r2WinstepsPfile.txt") == TRUE & 
		   file.exists("r2WinstepsIfile.txt") == TRUE) 
			
			break
		}

	p<-batch.pfile(list(demNames))
	i<-batch.ifile()	
return(list("ItemParameter" = i, "Person Parameters" = p))
}



	