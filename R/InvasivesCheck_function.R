#' invasivesCheck
#'
#' @author James Hutchison
#' @param invasives A vector of invasive species binomial names.
#' @param file Filepath to the .xlsx spreadsheet file to be checked for invasive species.
#' @param worksheet Number or name of the worksheet in the excel file to be checked.
#' @details This function searches for all of the values of a character vector in a worksheet of an Excel file. The intended use is for checking for invasive species in spreadsheets received from contractors. The invasive species should be supplied as a vector of species names, and the data to be searched should be in an Excel file.
#' @return A data frame containing the rows from the worksheet searched where invasive species names were found.
#' @export
#'

invasivesCheck <- function(invasives, file, worksheet) {
  stringrLoaded <- require(stringr)
  XLConnectLoaded <- require(XLConnect)
  if(!isTRUE(stringrLoaded)) stop("Package 'stringr' could not be loaded. Is it installed?")
  if(!isTRUE(XLConnectLoaded)) stop("Package 'XLConnect' could not be loaded. Is it installed?")
  speciesFile <- loadWorkbook(file)
  speciesList <- readWorksheet(speciesFile, worksheet)
  speciesListVec <- str_trim(as.vector(as.matrix(speciesList)))
  if(!any(speciesListVec %in% invasives)){
    print(noquote("No non-native/invasive species found!"))
    flush.console()
  }
  else {
    invasivesInd <- which(speciesListVec %in% invasives)
    invasiveRows <- ((invasivesInd - 1) %% nrow(speciesList)) + 1
    cat(noquote("Invasive species found! - Rows:"), invasiveRows)
    flush.console()
    return(speciesList[invasiveRows, ])

  }
}
