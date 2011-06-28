"AnnualAnomaly" <-
function (ndvidirectory, region, Ystart, Yend, ext = "show", namefile = "anomaly", xlim = NULL, ylim = NULL, type="VITO_CLIP", obj = NULL, objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral") 
{
    while (!toupper(type) %in% c("GIMMS", "VITO_CLIP", "VITO_VGT")) {
        type = readline(cat("Type is not correct. Please choose between GIMMS, VITO_CLIP and VITO_VGT. \n"))
        if (type == "") 
            stop("Error : Type is not correct.")
    }
    dat = c()
    tab = c()
    for (year in seq(Ystart, Yend, 1)) {
        dat = cbind(dat, mapmaxyear(ndvidirectory, region, 
            year, xlim, ylim, type)$band1)
        colnames(dat) = c(colnames(dat)[-length(dat[1, ])], as.character(year))
    }
    Mean = apply(dat, 1, meanNA)
    tp = readpartGDAL(timetoMap(ndvidirectory, region, Ystart, 1, 1, type), xlim, ylim)
    if (ext!="show") {    
        par(ask = TRUE) }
    for (year in seq(Ystart, Yend, 1)) {
        tp$band1 = dat[, as.character(year)] - Mean
        writeGDAL(tp, paste(namefile, as.character(year), ".tif", 
            sep = ""), drivername = "GTiff", type = "Int16", 
            mvFlag = -32768)
        savemap(tp, obj, objdir, ext, paste(namefile, as.character(year), 
            sep = ""), pal, objext, label)
    }
}
