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
      source('../../cp_package_files.R')
      source('../../assertCranResultOk.R')
      source('../../checkExamplePackage.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../')
    }
    ,
    ##--------------------------------
    #test.SoilR=function(){
    #  checkExamplePkg("SoilR")
	  #}
    #,
    #--------------------------------
	  test.abbriviatedSignatureMinimal=function(){
      checkExamplePkg("SignaturesMinimal")
	  }
    #,
    ##--------------------------------
    #test.OverloadedIndexOperator=function(){
    #  checkExamplePkg("OverloadedIndexOperator")
	  #}
    #,
    ##--------------------------------
    #test.noMethodRdFilesForHiddenMethods=function(){
    #  checkExamplePkg("HiddenMethod")
	  #}
    #,
    ##--------------------------------
    #test.GenericWithDotDotDotArgumets=function(){
    #  checkExamplePkg("GenericWithDotDotDotArgumets")
	  #}
    #,
    ##--------------------------------
    #test.OverloadedIndexedAssingment=function(){
    #  checkExamplePkg("OverloadedIndexedAssingment")
	  #}
    #,
    ##--------------------------------
    #test.ClassWithMethods=function(){
    #  checkExamplePkg("ClassWithMethods")
		#}
    #,
    ##--------------------------------
    #test.ClassWithExamples=function(){
    #  checkExamplePkg("ClassWithExamples")
		#}
    #,
    ##--------------------------------
    #test.ClassWithMethodsAndExampleFiles=function(){
    #  checkExamplePkg("ClassWithMethodsAndExampleFiles")
		#}
    #,
    ##--------------------------------
    #test.AutoConstructor=function(){
    #  checkExamplePkg("AutoConstructor")
		#}
    #,
    ##--------------------------------
    #test.VirtualClass=function(){
    #  checkExamplePkg("VirtualClass")
		#}
    #,
    ##--------------------------------
    #test.MethodSrcRef=function(SKIP){
    #  checkExamplePkg("MethodSrcRef")
		#}
    #,
    ##--------------------------------
    #test.PrivateAndPublic=function(){
    #  checkExamplePkg("PrivateAndPublic")
		#}
	)
)

############################################ 
if(is.null(sys.calls()[[sys.nframe()-1]])){
  s=get_suite_from_file(get_Rscript_filename())
  #s$parallel <- 1 
  tr<-s$run()
  tr$print_summary()
}
