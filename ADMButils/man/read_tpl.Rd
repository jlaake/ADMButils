\name{read_tpl}
\alias{read_tpl}
\alias{write_tpl}
\title{Read and write tpl files}
\usage{
  read_tpl(tplfile)

  write_tpl(tplStruct,tplfile)
}
\arguments{
  \item{tplfile}{base name of tpl file (without TPL
  extension)}

  \item{tplStruct}{tpl list structure to represent code in
  TPL files}
}
\value{
  a tplStruct is returned from read_tpl and nothing is
  returned from write_tpl
}
\description{
  read_tpl reads in a tpl file and returns a tplStruct.
  write_tpl creates a tpl file from a tplStruct which must
  have a DATA, PARAMETERS and PROCEDURE list element.
}
\examples{
\dontrun{
#Example of creating a specific model would need simple.DAT
# and link to admb setup
tplStruct=list(DATA=c("init_int NData;","init_vector x(1,NData);","init_vector y(1,NData);"),
               PARAMETER=c("init_number a;","init_number b;","vector ypred(1,NData);","objective_function_value obj_fun;"),
               PROCEDURE=c(" ypred = a + b*x;","cout << ypred << endl;","obj_fun = norm2(y-ypred);"),
               REPORT=c("report << ypred << endl;","report << y << endl;"))
write_tpl(tplStruct,"simple")
newtplStruct=read_tpl("simple")
}

\donttest{
# Example of the ability to write general models - in this case a linear model with a normal error structure
# needs admb link setup before running
admb_linearmodel=function(formula,data)
{
 # create a temp base file name
	admbfile=tempfile(pattern = "admb", tmpdir = "")
	admbfile=substr(admbfile,3,nchar(admbfile))
 # create the design matrix from the formula and data
	xmat=model.matrix(formula,data)
 # extract the dependent variable y
	yvar=data[,all.vars(formula)[1]]
 # output data file
	con=file(paste(admbfile,".dat",sep=""),open="wt")
	writeLines(as.character(length(yvar)),con)
	write(t(xmat),con,ncolumns=ncol(xmat))
	write(yvar,con,ncolumns=1)
	close(con)
 # create tplStruct
	tplStruct=list(DATA=c("init_int NData;",
					paste("init_matrix x(1,NData,1,",ncol(xmat),");",sep=""),
					"init_vector y(1,NData);"),
			PARAMETER=c(paste("init_vector beta(1,",ncol(xmat),");",sep=""),"vector ypred(1,NData);","objective_function_value f;"),
			PROCEDURE=c("ypred = x*beta;","cout << ypred << endl;","f = norm2(y-ypred);"))
 # write tpl file
	write_tpl(tplStruct,admbfile)
 # compile tpl
	compile_admb(admbfile)
 # run tpl
 run_admb(admbfile)
 # read in results
	results=read_admb(admbfile)
 # change names from beta.n to values from R formula
	names(results$coefficients)=colnames(xmat)
	rownames(results$vcov)=colnames(xmat)
	colnames(results$vcov)=colnames(xmat)
	rownames(results$cor)=colnames(xmat)
	colnames(results$cor)=colnames(xmat)
 # cleanup
	clean_admb(admbfile)
	return(results)
}
# test run using iris data
admb_linearmodel(formula=Sepal.Length~Sepal.Width*Species,data=iris)
#see same results with lm
lm(formula=Sepal.Length~Sepal.Width*Species,data=iris)
}
}
\author{
  Jeff Laake
}

