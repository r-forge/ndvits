"multimap" <-
function (listfiles, names, namefile, org, ext = "show", main = "maps", 
    obj = NULL, objdir = NULL, pal = "Spectral", objext = "shp", 
    ...) 
{
    if (length(names) != length(listfiles)) {
        names = listfiles
    }
    im = readGDAL(listfiles[1])
    dat = im$band1
    for (i in listfiles[-1]) {
        dat = cbind(dat, readGDAL(i)$band1)
    }
    colnames(dat) = listfiles
    im2 = SpatialGridDataFrame(im, data = as.data.frame(dat))
    sc=mean(apply(bbox(im),1,diff))
    arrow = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(bbox(im)[1,2]-(sc/5), bbox(im)[2,2]-(sc/5)), scale = sc/7, which = org[1]*org[2], first = FALSE)
    if (typeof(obj) == "character") {
        if (objext == "shp") {
            Bound = readOGR(paste(objdir, ".", sep = ""), obj)
        }
        else {
            Bound = readOGR(objdir, obj)
        }
        if (length(grep("oint", class(Bound))) > 0) {
            lay = list("sp.points", Bound, cex = 0.75, pch = 3, 
                col = "black", first = FALSE)
            spl = list(lay,arrow)
        }
        else {
            if (length(grep("olygon", class(Bound))) > 0) {
                lay = list("sp.polygons", Bound, cex = 0.75, 
                  pch = 3, col = "black", first = FALSE)
                spl = list(lay,arrow)
            }
            else {
                spl = list(arrow)
            }
        }
    }
    else {
        spl = list(arrow)
    }
    if (ext == "jpg" || ext == "jpeg") {
        jpeg(paste(namefile, ".jpg", sep = ""), quality = 90)
        print(spplot(im2, listfiles, names.attr = names, sp.layout = spl, 
            col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                "maxcolors"], pal)), as.table = TRUE, main = main, 
            layout = org, ...))
        dev.off()
    }
    else {
        if (ext == "png") {
            png(paste(namefile, ".png", sep = ""))
            print(spplot(im2, listfiles, names.attr = names, 
                sp.layout = spl, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                  "maxcolors"], pal)), as.table = TRUE, main = main, 
                layout = org, ...))
            dev.off()
        }
        else {
            if (ext == "pdf") {
                pdf(paste(namefile, ".pdf", sep = ""))
                print(spplot(im2, listfiles, names.attr = names, 
                  sp.layout = spl, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                    "maxcolors"], pal)), as.table = TRUE, main = main, 
                  layout = org, ...))
                dev.off()
            }
            else {
                print(spplot(im2, listfiles, names.attr = names, 
                  sp.layout = spl, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                    "maxcolors"], pal)), as.table = TRUE, main = main, 
                  layout = org, ...))
            }
        }
    }
}
"savemap" <-
function (map, obj = NULL, shapedir = NULL, ext = "show", namefile = "map", 
    pal = "Spectral", objext = "shp", label = FALSE) 
{
    sc=mean(apply(bbox(map),1,diff))
    arrow = list("SpatialPolygonsRescale", layout.north.arrow(),offset = c(bbox(map)[1,2]-(sc/10), bbox(map)[2,2]-(sc/10)), scale = sc/15)
    if (typeof(obj) == "character") {
        if (objext == "shp") {
            Bound = readOGR(paste(shapedir, ".", sep = ""), obj)
        }
        else {
            Bound = readOGR(shapedir, obj)
        }
        if (dim(coordinates(Bound))[2] > 2) {
            Bound = SpatialPointsDataFrame(coords = coordinates(Bound)[, 
                1:2], proj4string = CRS(proj4string(Bound)), 
                data = as.data.frame(Bound[label]))
        }
        if (length(grep("oint", class(Bound))) > 0) {
            pt = list("sp.points", Bound, cex = 0.75, pch = 3, 
                col = "black", first = FALSE)
            if (label != FALSE) {
                txt = list("sp.text", coordinates(Bound) - 0.01, 
                  Bound[[label]], first = FALSE)
                lay = list(arrow, pt, txt)
            }
            else {
            lay = list(arrow,pt)
            }
        }
        else {
            if (length(grep("olygon", class(Bound))) > 0) {
                pol = list("sp.polygons", Bound, cex = 0.75, 
                  pch = 3, col = "black", first = FALSE)
                lay = list(arrow,pol)
            }
        }
    }
    else {
        lay = list(arrow)
    }
    if (ext == "jpg" || ext == "jpeg") {
        jpeg(paste(namefile, ".jpg", sep = ""), quality = 90)
        print(spplot(map, sp.layout = lay, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
            "maxcolors"], pal)), xlab = "Longitude()", ylab = "Latitude()", 
            add = T, scales = list(draw = T), main = namefile, 
            font.main = 4))
        dev.off()
    }
    else {
        if (ext == "png") {
            png(paste(namefile, ".png", sep = ""))
            print(spplot(map, sp.layout = lay, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                "maxcolors"], pal)), xlab = "Longitude()", ylab = "Latitude()", 
                add = T, scales = list(draw = T), main = namefile, 
                font.main = 4))
            dev.off()
        }
        else {
            if (ext == "pdf") {
                pdf(paste(namefile, ".pdf", sep = ""))
                print(spplot(map, sp.layout = lay, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                  "maxcolors"], pal)), xlab = "Longitude()", 
                  ylab = "Latitude()", add = T, scales = list(draw = T), 
                  main = namefile, font.main = 4))
                dev.off()
            }
            else {
                print(spplot(map, sp.layout = lay, col.regions = colorRampPalette(brewer.pal(brewer.pal.info[pal, 
                  "maxcolors"], pal)), xlab = "Longitude()", 
                  ylab = "Latitude()", add = T, scales = list(draw = T), 
                  main = namefile, font.main = 4))
            }
        }
    }
}
