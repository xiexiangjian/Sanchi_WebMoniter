pro BoundingBox, INPUT_RASTER=inputRaster, $
                        NORTH=north, $
                        SOUTH=south, $
                        WEST=west, $
                        EAST=east
  compile_opt idl2                      

  spatialRef = inputRaster.SpatialRef

  if (~Isa(spatialRef,'ENVIStandardRasterSpatialRef')) then begin
    message,'Input Raster must contain a standard spatial reference.'
  endif

  ns = inputRaster.NColumns
  nl = inputRaster.NRows   

  ; Build the file coordinate bounding box from the input raster's number of
  ; lines and samples                     
  fileBoundingBox = [[0,0],[ns-1,0],[0,nl-1],[ns-1,nl-1]]

  ; Convert to map coordinates 
  spatialRef.ConvertFileToMap, fileBoundingBox[0,*],fileBoundingBox[1,*], $
    mapBoundingBoxX, mapBoundingBoxY

  ; Convert to longitude/latitude coordinates.  This is what the
  ; web map APIs require  
  spatialRef.ConvertMapToLonLat, mapBoundingBoxX, mapBoundingBoxY, $
    boundingLon, boundingLat

  ; Seperate the information into north, south, east, and west.  Each
  ; web map API may require different format for the bounding box 
  south = Min(boundingLat, MAX=north)
  west = Min(boundingLon, MAX=east)  

end