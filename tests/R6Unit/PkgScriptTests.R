#!/usr/bin/Rscript
## vim:set ff=unix expandtab ts=2 sw=2:
require(R6Unit)
PkgScriptTests<-R6Class("PkgScriptTests",
	inherit=InDirScriptTest,
  public=list(
    targetPkgName=""
    ,
    #------------------------
    setUp=function(){
      #source('../../cpDir.R')
      source('../../writeDescriptionFile.R')
      #source('../../cp_package_files.R')
      source('../../assertCranResultOk.R')
      source('../../checkExamplePackage.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../../')
    }
    ,
    ##--------------------------------
	  test.SignatureMinimal=function(){
      checkExamplePkg("SignaturesMinimal")
	  }
    ,
    #--------------------------------
	  test.SignatureMinimalWithoutTags=function(){
      checkExamplePkg("SignaturesMinimalWithoutTags")
	  }
    ,
    #--------------------------------
	  test.SignatureMinimalAutoComment=function(){
      targetPkgName<-"SignaturesMinimalWithoutTags"
      #requireNamespace("R6Unit")
      # copy the files 
      resourceDirName<-file.path("..","..","test_resources","example_packages")
      pkgDirOrg="pkgDirOrg"
      pkgDirAutoDocs="pkgDirAutoDocs"
      R6Unit::cpDir(file.path(resourceDirName,targetPkgName),pkgDirOrg)
      
      # if necessarry add a default DESCRIPTION file
      if (!file.exists(file.path(pkgDirOrg,"DESCRIPTION"))){ 
        writeDescriptionFile(Imports="methods",pkgName=targetPkgName,pkgDir=pkgDirOrg)
      }
      if(file.exists(pkgDirAutoDocs)){unlink(pkgDirAutoDocs)} 
      # now duplicate the package directory
      R6Unit::cpDir(pkgDirOrg,pkgDirAutoDocs)
      # and unlink the man subdir
      unlink(file.path(pkgDirAutoDocs,"man"),recursive=TRUE)

      # create the documentation automatically
      roxygenize(pkgDirAutoDocs,c("auto_comment_roclet","rd"))
      
      # perform cran checks on the automatic documentation
      l<-devtools::check(pkgDirAutoDocs,document=FALSE,quiet=FALSE,cran=TRUE,check_dir='.')
      assertCranResultOk(l,msg="devtools::check failed")
          
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
