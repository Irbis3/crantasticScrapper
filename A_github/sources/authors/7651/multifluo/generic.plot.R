generic.plot <-
function(type="R", width=7, height=7, name="output", CALLFUN=NULL) 
 {
   CALLFUN=c(CALLFUN)   

   if(!is.character(type)) stop("The output format (type) must be a string!")
   if(!is.character(name)) stop("The output name (name) must be a string!")

  	dev.new()
   if(!is.null(CALLFUN))
   {
     res=lapply(CALLFUN, FUN = eval)    
   }
 
   if (type!="R")
   {
	   dev.off()  
   }
   invisible(res)

 }
