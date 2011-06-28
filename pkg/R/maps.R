"mapdiff" <-
function (mapin, mapref, mapsd = FALSE, fileout = "diff.tif") 
{
    if (as.numeric(bbox(mapin)) != as.numeric(bbox(mapref))) 
        stop("the two maps must be the same size")
    res = mapref
    res$band1 = mapin$band1 - mapref$band1
    writeGDAL(res, fileout, drivername = "GTiff", type = "Int16", 
        mvFlag = -32768)
    return(res)
}
"maplocalstat" <-
function (ndvidirectory, region, Ystart, Yend, xlim = NULL, ylim = NULL,type="VITO_CLIP") 
{
    while (!toupper(type) %in% c("GIMMS", "VITO_CLIP", "VITO_VGT")) {
        type = readline(cat("Type is not correct. Please choose between GIMMS, VITO_CLIP and VITO_VGT. \n"))
        if (type == "") 
            stop("Error : Type is not correct.")
    }
    maxNDVI=ifelse(type == "GIMMS",10000,255)
    filein = timetoMap(ndvidirectory, region, Ystart, 1, 1, type)
    inGrid = readpartGDAL(filein, xlim, ylim)
    tp = inGrid
    tp2 = inGrid
    tp3 = inGrid
    tp4 = inGrid
    mat = c()
    for (year in seq(Ystart, Yend, 1)) {
        for (month in seq(1, 12, 1)) {
            for (period in seq(1, 3, 1)) {
                if (!(type=="GIMMS" & period==3)) {
                    filein = timetoMap(ndvidirectory, region,  year, month, period, type)
                    inGrid = readpartGDAL(filein, xlim, ylim)
                    mat = cbind(mat, inGrid$band1/maxNDVI)
                }
            }
        }
    }
    tp$band1 = apply(mat, 1, meanNA)*10000
    tp2$band1 = apply(mat, 1, maxNA)*10000
    tp3$band1 = apply(mat, 1, minNA)*10000
    tp4$band1 = apply(mat, 1, sdNA)*10000
    writeGDAL(tp, paste(region, "mean", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Int16", mvFlag = -32768)
    writeGDAL(tp2, paste(region, "max", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Int16", mvFlag = -32768)
    writeGDAL(tp3, paste(region, "min", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Int16", mvFlag = -32768)
    writeGDAL(tp4, paste(region, "sd", substr(as.character(Ystart), 
        3, 4), substr(as.character(Yend), 3, 4), ".tif", sep = ""), 
        drivername = "GTiff", type = "Int16", mvFlag = -32768)
    res = c()
    res$mean = tp
    res$max = tp2
    res$min = tp3
    res$sd = tp4
    return(res)
}
"mapmaxyear" <-
function (ndvidirectory, region, year, xlim = NULL, ylim = NULL, 
    fileout = FALSE,type="VITO_CLIP") 
{
    while (!toupper(type) %in% c("GIMMS", "VITO_CLIP", "VITO_VGT")) {
        type = readline(cat("Type is not correct. Please choose between GIMMS, VITO_CLIP and VITO_VGT. \n"))
        if (type == "") 
            stop("Error : Type is not correct.")
    }
    maxNDVI=ifelse(type == "GIMMS",10000,255)
    filein = timetoMap(ndvidirectory, region, year, 1, 1, type)
    inGrid = readpartGDAL(filein, xlim, ylim)
    tp = inGrid
    for (month in seq(1, 12, 1)) {
        for (period in seq(1, 3, 1)) {
            if (!(type=="GIMMS" & period==3)) {
                filein = timetoMap(ndvidirectory, region,  year, month, period, type)
                inGrid = readpartGDAL(filein, xlim, ylim)
                tp$band1 = apply(cbind(tp$band1, inGrid$band1), 1, 
                maxNA2)
            }
        }
    }
    tp$band1=(tp$band1/maxNDVI)*10000
    if (fileout != FALSE) {
        writeGDAL(tp, fileout, drivername = "GTiff", type = "Int16", 
        mvFlag = -32768)
    }
    return(tp)
}
