\name{get_admb_data}
\alias{get_admb_data}
\alias{get_admb_init_fields}
\title{Extract ADMB data from tpl file}
\usage{
  get_admb_data(tplStruct)

  get_admb_init_fields(tplSection)
}
\arguments{
  \item{tplStruct}{tpl list structure to represent code in
  TPL files created with read_tpl}

  \item{tplSection}{a section of a tpl list structure like
  DATA or PARAMETERS}
}
\value{
  get_admb_data returns a list containing data;
  get_admb_init_fields returns a list of the identified
  init fields with the values type= value after init_,
  object=name of field, dims=dimension ranges of vector or
  matrix, mode= all numeric at present.
}
\description{
  get_admb_init_fields will extract fields with init_ from
  DATA section of tplStruct created with read_tpl.
  get_admb_data uses fields extracted by
  get_admb_init_fields and reads data from .DAT file and
  returns a list with the data.
}
\details{
  Currently it only supports
  int,number,vector,ivector,matrix,imatrix. Also all data
  are currently stored as numeric ignoring integer type but
  that could be changed if needed.
}
\author{
  Jeff Laake
}

