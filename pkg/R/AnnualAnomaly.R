"AnnualAnomaly" <-
function (ndvidirectory, region, Ystart, Yend, ext = "show", 
    namefile = "anomaly", xlim = NULL, ylim = NULL, obj = NULL, 
    objdir = NULL, objext = "shp", label = FALSE, pal = "Spectral") 
{
    dat = c()
    tab = c()
    for (year in seq(Ystart, Yend, 1)) {
        dat = cbind(dat, mapmaxyearVito(ndvidirectory, region, 
            year, xlim, ylim)$band1)
        colnames(dat) = c(colnames(dat)[-length(dat[1, ])], as.character(year))
    }
    Mean = apply(dat, 1, meanNA)
    tp = readpartGDAL(timetoMap(ndvidirectory, region, Ystart, 1, 0), xlim, ylim)
    par(ask = TRUE)
    for (year in seq(Ystart, Yend, 1)) {
        tp$band1 = dat[, as.character(year)] - Mean
        writeGDAL(tp, paste(namefile, as.character(year), ".tif", 
            sep = ""), drivername = "GTiff", type = "Int16", 
            mvFlag = -32768)
        savemap(tp, obj, objdir, ext, paste(namefile, as.character(year), 
            sep = ""), pal, objext, label)
    }
}
