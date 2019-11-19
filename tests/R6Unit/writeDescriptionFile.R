require(R6Unit,quiet=TRUE)
writeDescriptionFile<-function(Imports=NULL,Depends=NULL,pkgName="ExamplePackage",pkgDir="."){
	desc <-paste("Package: ",pkgName,"\n",
"Title: EXAMPLES to TEST the POSSIBILITIES of NAMESPACES  
Version:0.1
Date: ",Sys.Date(),"
Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
Description: This package contains functions whose automatic documentation is tested.
License: GPL-3
"
        ,ifelse(
          is.null(Depends)
          ,""
          ,paste(
              "Depends:"
              ,toString(Depends)
              ,"\n"
              ,sep=""
          )
        )
        ,ifelse(
          is.null(Imports)
          ,"\n"
          ,paste(
              "Imports: "
              ,toString(Imports)
              ,""
              ,sep=""
          )
        )
        , "\n"
        ,sep=""
    )
	descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(desc,file=descFilePath)
}
