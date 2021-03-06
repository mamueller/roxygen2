checkExamplePkg=function(targetPkgName,roclets=c("collate", "namespace", "rd")){
  requireNamespace("R6Unit")
  #whereAmI <- getwd()
  # copy the files 
  resourceDirName<-file.path("..","..","test_resources","example_packages")
  pkgDirOrg="pkgDirOrg"
  pkgDirAutoDocs="pkgDirAutoDocs"
  R6Unit::cpDir(file.path(resourceDirName,targetPkgName),pkgDirOrg)
  
  # if necessarry add a default DESCRIPTION file
  if (!file.exists(file.path(pkgDirOrg,"DESCRIPTION"))){ 
    writeDescriptionFile(Imports="methods",pkgName=targetPkgName,pkgDir=pkgDirOrg)
  }
  
  # perform cran checks on the original documentation
  l<-devtools::check(pkgDirOrg,document=FALSE,quiet=FALSE,cran=TRUE,check_dir='.')
  assertCranResultOk(l,msg="devtools::check failed")

  # now duplicate the package directory
  R6Unit::cpDir(pkgDirOrg,pkgDirAutoDocs)
  # and unlink the man subdir
  unlink(file.path(pkgDirAutoDocs,"man"),recursive=TRUE)

  # create the documentation automatically
  roxygenize(pkgDirAutoDocs,roclets=roclets)
  #
  ## perform cran checks on the automatic documentation
  l<-devtools::check(pkgDirAutoDocs,document=FALSE,quiet=FALSE,cran=TRUE,check_dir='.')
  assertCranResultOk(l,msg="devtools::check failed")
}
