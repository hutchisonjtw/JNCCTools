#' Multibeam to polygon conversion
#'
#' Converts a raster of multibeam data to a single dissolved polygons showing the outline of the data
#' @author James Hutchison
#' @param multibeam Filepath to multibeam raster file. See gdalDrivers() for a list of readable formats. Default is NULL and function will then prompt for the file location using choose.files().
#' @param multibeamType Raster type - should be 1 for RGB multi-band file, or 2 for floating point single band. Default is NULL and function will prompt for it using readline.
#' @param polyfile Filepath and name for output file location. Default is NULL and function will then prompt for it using choose.dir() and readline().
#' @param polyformat File type for output polygon file. Default is ESRI Shapefile. See ogrDrivers() for a list of available formats.
#' @export

multibeamToPolygon <- function(multibeam=NULL, multibeamType=NA, polyfile=NULL, polyformat='ESRI Shapefile') {
if (is.null(multibeam)) multibeam <- choose.files(caption="Select multibeam raster", multi=FALSE)
while (!(multibeamType %in% c(1,2))) {
multibeamType <- as.numeric(readline('Please enter 1 or 2 for type of multibeam file: \n 1. RGB (multi-band file) \n 2. Single-band floating point \n'))
if (!(multibeamType %in% c(1,2)))  print(noquote("Please choose either 1 or 2"))
flush.console()
}
if(is.null(polyfile)) polyfile <- paste0(choose.dir(caption="Choose output file location"),"\\",readline(prompt="Type filename without file extension: "))
rasterloaded <- require(raster)
rgdalloaded <- require(rgdal)
if (!isTRUE(rasterloaded)) stop("Error: Package 'raster' could not be loaded - is it installed?")
if (!isTRUE(rgdalloaded)) stop("Error: Package 'rgdal' could not be loaded - is it installed?")

tempras <- tempfile(fileext=".tif")

if (multibeamType==1) {
   print(noquote("Reclassifying raster..."))
   flush.console()
   inRaster <- stack(multibeam)                                                   ## Read in raster
   inRasterSummed <- inRaster[[1]]+ inRaster[[2]]+ inRaster[[3]]
   binary0 <- clamp(inRasterSummed, upper=1)
   binary <- clamp(binary0, lower=1, useValues=FALSE)
   writeRaster(binary, file=tempras, format="GTiff", datatype="INT2S")
   print(noquote("Complete!"))
   flush.console()
} else {
   print(noquote("Reclassifying raster..."))
   flush.console()
   inRaster <- raster(multibeam)
   NAvalue(inRaster) <- as.numeric(readline(prompt="Please type NA value of raster (e.g. -9999): "))
   minval <- cellStats(inRaster, min)
   maxval <- cellStats(inRaster, max)
   reclassificationMat <- matrix(c(minval, maxval, 1), ncol=3, byrow=TRUE)
   binary <- reclassify(inRaster, reclassificationMat, right=NA, filename=tempras, format="GTiff", datatype="INT2S")
   print(noquote("Complete!"))
   flush.console()
}

rgdalloaded <- require(rgdal)
if(!isTRUE(rgdalloaded)) stop("Package rgdal could not be loaded. Polygons will not be read into R")

if(file.exists("D:/Programs/QGIS/OSGeo4W.bat")){                                                                  ## Check for OSGeo4W.bat
   OSGeo4W <- "D:/Programs/QGIS/OSGeo4W.bat"                                                                      ## If present assign path to variable
   }
else {
   warning("OSGeo4W.bat file not found. Please enter OSGeo4W.bat file location...")                                ## If not present in that location, ask user for path
   OSGeo4W <- readline(prompt="OSGeo4W.bat file location: ")
   }
func <- "gdal_polygonize"
system2(OSGeo4W, args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', func, tempras, polyformat, paste0(polyfile, "_undissolved"))))       ## Produce shapefile from temporary file with OSgeo4W gdal-polygonize
file.remove(tempras)                                                                                              ## Delete temporary raster
sqlStatement <- shQuote(paste0("SELECT DN, ST_Union(geometry) AS geometry FROM '", basename(polyfile), "_undissolved' GROUP BY DN"), type="cmd2")
func <- "ogr2ogr"
system2(OSGeo4W, args=(sprintf('"%1$s" "%2$s.shp" "%3$s_undissolved.shp" -dialect sqlite -sql "%4$s"', func, polyfile, polyfile, sqlStatement)))
file.remove(paste0(dirname(polyfile), "/", list.files(path=dirname(polyfile), pattern=paste0(basename(polyfile), "_undissolved"))))
}
