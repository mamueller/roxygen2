#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
InheritanceGraphTests<-R6Class("InheritanceGraphTests",
	inherit=InDirScriptTest,
  public=list(
    targetPkgName=""
    ,
    #------------------------
    setUp=function(){
      source('../../writeDescriptionFile.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../../',export_all=FALSE)
    }
    
    ,
    #--------------------------------
	  test.SuperClassTag=function(){
      targetPkgName<-"S4ClassInheritanceWithMethods"
      #requireNamespace("R6Unit")
      # copy the files 
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDirOrg="pkgDirOrg"
      R6Unit::cpDir(file.path(resourceDirName,targetPkgName),pkgDirOrg)
      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDirOrg,"DESCRIPTION"))){ 
        writeDescriptionFile(Imports="methods",pkgName=targetPkgName,pkgDir=pkgDirOrg)
      }
      roxygenize(pkgDirOrg,c("rd"))
	  }



    ,
    #--------------------------------
	  test.ClassGraph=function(){
      targetPkgName<-"S4ClassInheritance"
      #requireNamespace("R6Unit")
      # copy the files 
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDirOrg="pkgDirOrg"
      R6Unit::cpDir(file.path(resourceDirName,targetPkgName),pkgDirOrg)
      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDirOrg,"DESCRIPTION"))){ 
        writeDescriptionFile(Imports="methods",pkgName=targetPkgName,pkgDir=pkgDirOrg)
      }
      roxygenize(pkgDirOrg,c("inheritance_graph_roclet"))
	  }





  )
)

############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  #s$parallel <- 1 
  tr<-s$run()
  tr$print_summary()
}
