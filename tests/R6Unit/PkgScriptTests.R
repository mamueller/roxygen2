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
      source('../../writeDescriptionFile.R')
      source('../../assertCranResultOk.R')
      source('../../checkExamplePackage.R')
      requireNamespace("pkgload")
      requireNamespace("debugHelpers")
      pkgload::load_all('../../../../',export_all=FALSE)
    }
    


    #,
    ##--------------------------------
	  #test.Error=function(){
    #  stopifnot(FALSE)
	  #}
    
    
    
    ,
    ##--------------------------------
	  test.S4Class_autocomment=function(){
      checkExamplePkg(
        "S4ClassWithoutTags",
        roclets=c(
          "auto_comment_roclet",
          "rd"
        )
      )
	  }
    
    
    
    ,
    ##--------------------------------
	  test.S4Class_autotag=function(){
      checkExamplePkg(
        "S4ClassWithoutTags",
        roclets=c(
          "autotag_roclet",
          "rd"
        )
      )
	  }
    
    
    
    ,
    ##--------------------------------
	  test.SignatureMinimal=function(){
      checkExamplePkg(
        "SignaturesMinimal",
        roclets=c(
          "collate", 
          "namespace",
          "rd"
        )
      )
	  }
    ,
    #--------------------------------
	  test.SignatureMinimal_autotag=function(){
      checkExamplePkg(
        "SignaturesMinimalWithoutTags",
        roclets=c(
          "collate",
          "namespace",
          "autotag_roclet",
          "rd"
        )
      )
	  }
    
    
    
    ,
    #--------------------------------
	  test.SignatureMinimal_autocomment=function(){
      checkExamplePkg(
        "SignaturesMinimalWithoutTags",
        roclets=c(
          "collate",
          "namespace",
          "auto_comment_roclet",
          "rd"
        )
      )
	  }
    
    
    
    ,
    #--------------------------------
	  test.SignatureMinimal_update_auto_comment=function(){
      checkExamplePkg(
        "SignaturesMinimalWithOutdatedAutoComments",
        roclets=c(
          "collate",
          "namespace",
          "update_auto_comment_roclet",
          "rd"
        )
      )
	  }
    
    
    
    ,
    #--------------------------------
	  test.SignatureMinimal_idempotence_update_auto_comment=function(){
      targetPkgName<-"SignaturesMinimalWithAutoComments"
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
      # now duplicate the package directory
      R6Unit::cpDir(pkgDirOrg,pkgDirAutoDocs)
      roxygenize(pkgDirAutoDocs,c("update_auto_comment_roclet"))
      file_names<-list.files(file.path(pkgDirOrg,'R'))
      stopifnot(
        all(
          as.logical(
            lapply(
              file_names,
              function(file_name){
                all(
                  readLines( file.path(pkgDirOrg,'R',file_name)) == 
                  readLines( file.path(pkgDirAutoDocs,'R',file_name))
                )
              }
            )
          )
        )
      )
      #browser()
      
	  }
    
    
    
    ,
    #--------------------------------
	  test.Signature_remove_autotag=function(){
      source('../../number_of_auto_lines.R')
      targetPkgName<-"SignaturesMinimalWithAutoTags"
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
      before<-number_of_auto_lines(pkgDirOrg)
      stopifnot(before>0)
      # now duplicate the package directory
      R6Unit::cpDir(pkgDirOrg,pkgDirAutoDocs)
      
       
      # remove the @auto lines
      roxygenize(pkgDirAutoDocs,c("remove_autotag_roclet"))
      after <-number_of_auto_lines(pkgDirAutoDocs)
      stopifnot(after==0) 
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
