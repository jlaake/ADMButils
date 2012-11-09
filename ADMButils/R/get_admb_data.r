#' Extract ADMB data from tpl file
#' 
#' get_admb_init_fields will extract fields with init_ from DATA section of tplStruct created with read_tpl.
#' get_admb_data uses fields extracted by get_admb_init_fields and reads data from .DAT file and returns a list with the data.
#' 
#' Currently it only supports int,number,vector,ivector,matrix,imatrix.
#' Also all data are currently stored as numeric ignoring integer type but that could be changed if needed.
#'  
#' @usage get_admb_data(tplStruct)
#' 
#'        get_admb_init_fields(tplSection)
#' 
#' @param tplStruct tpl list structure to represent code in TPL files created with read_tpl
#' @param tplSection a section of a tpl list structure like DATA or PARAMETERS
#' @return get_admb_data returns a list containing data; get_admb_init_fields returns a list of the identified
#' init fields with the values	type= value after init_, object=name of field, dims=dimension ranges of vector or matrix, mode= all numeric at present.
#' @export get_admb_data get_admb_init_fields
#' @aliases get_admb_data get_admb_init_fields
#' @author Jeff Laake
get_admb_data=function(tplStruct)
{
	datafields=get_admb_init_fields(tplStruct$DATA)
	con=file(paste(tplStruct$tplfile,".DAT",sep=""),open="rt")
	nobj=length(datafields$object)
	lst=vector("list",nobj)
	names(lst)=datafields$object
	for(i in 1:nobj)
	{
		type=datafields$type[i]
		obj=datafields$object[i]
		xmode=datafields$mode[i]
		dims=datafields$dims[[i]]
		if(type%in%c("int","number"))
		{
			eval(parse(text=paste('lst[["',obj,'"]]=vector("',xmode,'",length=1)',sep="")))
		}else
		if(type%in%c("ivector","vector"))
		{
			veclength=eval(parse(text=paste(dims[2],"-",dims[1],"+1",sep="")),envir=lst)
			eval(parse(text=paste('lst[["',obj,'"]]=vector("',xmode,'",length=',veclength,")",sep="")))
		} else
		{
			if(type%in%c("imatrix","matrix"))
			{
				rowlength=eval(parse(text=paste(dims[2],"-",dims[1],"+1",sep="")),envir=lst)
				collength=eval(parse(text=paste(dims[4],"-",dims[3],"+1",sep="")),envir=lst)
				eval(parse(text=paste('lst[["',obj,'"]]=vector("',xmode,'",length=',rowlength*collength,")",sep="")))
				eval(parse(text=paste('dim(lst[["',obj,'"]])=c(',rowlength,",",collength,")",sep="")))
			} else
				if(type=="bounded_number")
				{
					
				}
		}
		dat=read_admb_data(lst[[obj]],con)
		if(type%in%c("imatrix","matrix"))
		{
			attr(dat,"dim")=rev(attr(lst[[obj]],"dim"))
			lst[[obj]]=t(dat)
			rownames(lst[[obj]])=eval(parse(text=paste(dims[1],":",dims[2],sep="")),envir=lst)
            colnames(lst[[obj]])=eval(parse(text=paste(dims[3],":",dims[4],sep="")),envir=lst)
        }
		else
		{
			lst[[obj]]=dat
			if(type%in%c("ivector","vector"))
				names(lst[[obj]])=eval(parse(text=paste(dims[1],":",dims[2],sep="")),envir=lst)
		}
	}
	return(lst)
}
get_admb_init_fields=function(tplSection)
{
	x=strsplit(tplSection[grep("init_",tplSection)],"//")
	x=sapply(x,function(x) strsplit(x[1],";")[[1]][1])
	x=stringr:::str_trim(x)
	x=sapply(x,function(x) strsplit(x,"init_")[[1]][2])
	names(x)=NULL
	x=strsplit(x," ")
	type=sapply(x,function(x) x[1])
	object=sapply(x, function(x) x[length(x)])
#	dims=vector("list",length(object))
#	bounds=vector("list",length(object))
#	phases=vector("list",length(object))
    field_values=vector("list",length(object))
	for(i in 1:length(object))
		field_values[[i]]=parse_field(type,object)
	object=sapply(object,function(x) strsplit(x,split="\\(")[[1]][1])
	names(object)=NULL
	Rmode=c(int="numeric",bounded_number="numeric",number="numeric",vector="numeric",matrix="numeric",ivector="numeric",imatrix="numeric")
	valid.types=c("int","bounded_number","number","vector","ivector","matrix","imatrix")
	if(any(!type%in%valid.types))stop(paste("\nEncountered unsupported types: ",
						paste(unique(type[!type%in%valid.types]),collapse=","),
						"\nValid types are: ",paste(valid.types,collapse=","),sep=""))
	return(list(type=type,object=object,mode=Rmode[type],attributes=field_values))
}

parse_field=function(type,obj)
{
	if(length(grep("\\(",obj))!=0)
	{
		begin=regexpr("\\(",object[i])+1
		end=regexpr("\\)",object[i])-1
		vals=strsplit(substr(object[i],begin,end),",")[[1]]
	}
	if(type[i]%in%c("vector","ivector","matrix","imatrix"))
	    dims=vals	
	else
		dims=1
	return(list(dims=dims))
}


read_admb_data=function(object,con)
{
	if(is.vector(object))
		len=length(object)
	else
		len=nrow(object)*ncol(object)
	reading=TRUE
	allvalues=NULL
	while(reading)
	{
		strng=readLines(con,1)
		if(length(grep("#",strng))!=0) next
		values=scan(text=strng,quiet=TRUE)
		allvalues=c(allvalues,values)
		if(length(allvalues)>=len)reading=FALSE
	}
	return(allvalues)	
}

