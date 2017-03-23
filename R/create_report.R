create_report <- function(name = "example_report"){
  report <- system.file("latex", "example_report.rnw", package = "mypackage")
  bibliography <- system.file("latex", "bibliography.bib", package = "mypackage")
  rnw<-file(paste(name, ".rnw", sep = ""))
  writeLines(paste(readLines(report), collapse = "\n"), rnw)
  close(rnw)

  bib<-file("bibliography.bib")
  writeLines(paste(readLines(bibliography), collapse = "\n"), bib)
  close(rnw)
}