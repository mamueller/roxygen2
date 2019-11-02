#!/usr/bin/Rscript 
# vim:set ff=unix expandtab ts=2 sw=2:

require(R6Unit)
require("pkgload")
#pkgload::load_all('~/debugHelpers/pkg',export_all=FALSE)
#pkgload::load_all('~/R6Unit/pkg',export_all=FALSE)
#source("ClassDocScriptTest.R")
source("PkgScriptTests.R")

s<-TestSuite$new(list(
  PkgScriptTests$new('test.Signature_remove_autotag')
))
print(s$test_names())
#s$parallel <- 1 
tr <- s$run()
tr$print_summary()

