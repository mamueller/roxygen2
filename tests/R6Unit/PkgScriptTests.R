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
      pkgload::load_all('../../')
    }
    ,
    ##--------------------------------
	  #test.SignatureMinimal=function(){
    #  checkExamplePkg("SignaturesMinimal")
	  #}
    #,
    #--------------------------------
	  test.SignatureMinimalWithoutTags=function(){
      checkExamplePkg("SignaturesMinimalWithoutTags")
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
