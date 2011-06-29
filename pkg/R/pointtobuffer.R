"pointtobuffer" <-
function (shapefile, shapedir, ndvidirectory, region, Ystart, ext ="shp",
    nameshape = "buffer", dirshape = ".", rad = 1) 
{
    while (!tolower(ext) %in% c("shp", "kml")) {
        ext = readline(cat("Extension is not correct. Please choose between shp and kml. \n"))
        if (ext == "") 
            return()
    }
    if (ext == "shp") {
        inPoints = readOGR(paste(shapedir, ".", sep = ""), shapefile)
    }
    else {
        inPoints = readOGR(shapedir, shapefile)
    }
    if (dim(coordinates(inPoints))[2] > 2) {
        inPoints = SpatialPointsDataFrame(coords = coordinates(inPoints)[, 
            1:2], proj4string = CRS(proj4string(inPoints)), data = as.data.frame(inPoints[names(inPoints)]))
    }
    filein = timetoMap(ndvidirectory, region, Ystart, 8, 2, type = "VITO_VGT")
    inGrid = readpartGDAL(filein, bbox(inPoints)[1, ] + c(-0.05, 
        0.05), bbox(inPoints)[2, ] + c(-0.05, 0.05))
    coord = c()
    data = c()
    for (i in 1:length(coordinates(inPoints)[, 1])) {
        for (x in -rad:rad) {
            for (y in -rad:rad) {
                coord = rbind(coord, coordinates(inPoints)[i, 
                  ] + c(x * gridparameters(inGrid)[1, 2], y * 
                  gridparameters(inGrid)[2, 2]))
                data = rbind(data, as.data.frame(inPoints[names(inPoints)])[i, 
                  ])
            }
        }
    }
    names(data) = substr(names(data), 1, 10)
    res = SpatialPointsDataFrame(coords = coord, proj4string = CRS(proj4string(inPoints)), 
        data = as.data.frame(data))
    writeOGR(res, dirshape, layer = nameshape, driver = "ESRI Shapefile")
}
