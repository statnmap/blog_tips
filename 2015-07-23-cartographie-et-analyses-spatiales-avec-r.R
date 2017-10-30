library(sp)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(ade4)
library(dismo)
library(OpenStreetMap)
library(png)
library(rgl)
library(geoR)
library(gstat)
library(mgcv)

# --------------------------
## Read / write spatial data
# --------------------------

# List supported formats by OGR / GDAL
ogrDrivers()

# Read a polygon shapefile
readOGR()
writeOGR()

# Read a raster
raster()

# --------------------------
## Representation
# --------------------------

# Plot the default world maps and rivers
library(maps)
library(mapdata)

jpeg(filename = "World.jpeg",
     width = 20, height = 20, units = "cm", pointsize = 5,
     quality = 100,
     bg = "white", res = 300)
  map(database="worldHires",fill=T,col="grey")
dev.off()

jpeg(filename = "France.jpeg",
     width = 20, height = 20, units = "cm", pointsize = 5,
     quality = 100,
     bg = "white", res = 300)
  colDept <- rep("white",length(map("france",plot=FALSE)$names))
  colDept[which(map("france",plot=FALSE)$names %in% c("Finistere","Loire-Atlantique") )] <- "skyblue"
  map("france",fill=T,col=colDept)
dev.off()

library(ade4)
jpeg(filename = "Bretagne.jpeg",
     width = 20, height = 20, units = "cm", pointsize = 5,
     quality = 100,
     bg = "white", res = 300)
  map(database="worldHires",xlim=c(-5,-1),ylim=c(46.8,49),fill=T,col="grey",mar=c(1,4,1,2))
    title(main="La Bretagne",cex.main=5,line=2)
    map(database="rivers", add=TRUE,col="blue")
    axis(1,cex.axis=2)
    axis(2,cex.axis=2)
    box(lwd=2)
    # A North arrow
    SpatialPolygonsRescale(layout.north.arrow(2), offset= c(-4.5,47.1), scale = 0.2, plot.grid=F)
    # A scale bar:
    OneDegree <- spDists(cbind(-4,47), y =cbind(-3,47) , longlat = TRUE)
    SpatialPolygonsRescale(layout.scale.bar(), offset= c(-4.75,46.9), scale= 100/OneDegree , fill=c("transparent", "black"), plot.grid= F)
    text(-4.75+100/OneDegree,46.9,"100Km",pos=1,cex=3)
    # grid

  foncGraph <-   function() {
    plot(NA,xlim=c(-10,19),ylim=c(37,60),xaxt="n",yaxt="n",xlab="",ylab="")
    map("worldHires",add=TRUE,col="grey75",fill=TRUE)
    rect(-5,46.8,-1,49,border="red",lwd=2)
    box(lwd=2) 
  }
  add.scatter(foncGraph(),posi="bottomright",ratio=0.3,bg.col="white",inset=0)
dev.off()


# Plot Google maps
  library(dismo)
  # a dataset coordinates
  Pts_PNMI <- read.table("./data/Pts_intertidal_wgs84_DD.txt",sep="\t",dec=".",header=TRUE)
  head(Pts_PNMI)
  # plot map around dataset
  xy <- cbind(Pts_PNMI[,2:3])
  names(xy) <- c("x","y")
  g <- gmap(xy, type='satellite',exp=1.6)

  jpeg(filename = "Gmap_data.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    plot(g)
    title(main="Stations d'échantillonnage de laminaires à Molène",line=-10,cex.main=4)
    xyMerc <- Mercator(xy, inverse = FALSE)
    points(xyMerc,col="red",pch=20,cex=5)
    SpatialPolygonsRescale(layout.north.arrow(1), offset= Mercator(c(-5.18,48.33)), scale = 5000, plot.grid=F, col="green")
  dev.off()

  jpeg(filename = "Gmap_request.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    g <- gmap('Nantes', type='hybrid',exp=2)
    plot(g)
  dev.off()

# Plot OpenStreetMap
library(OpenStreetMap)

  jpeg(filename = "Osm_bing.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    map <- openmap(c(max(xy[,2])+0.05,min(xy[,1])-0.05), c(min(xy[,2])-0.05,max(xy[,1])+0.05),type='bing')
    plot(map,raster=TRUE)
  dev.off()

  Nantes <- c(47.227962,-1.558113)
  jpeg(filename = "Osm_osm.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
  map <- openmap(c(Nantes[1]+0.1,Nantes[2]-0.2), c(Nantes[1]-0.1,Nantes[2]+0.2),type='osm')
  plot(map,raster=TRUE)
  dev.off()


# Representation of raster
  RastMean <- raster("./data/Grid_mean_10m.tif")
  g <- gmap('Treffiagat, Bretagne, France', type='roadmap',exp=5)
  plot(g)
  RastMean_proj <- projectRaster(from=RastMean,to=g)
  g_crop <- crop(x=g,y=extent(RastMean_proj)*1.25)
  jpeg(filename = "plot_raster.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 10,
      quality = 100,
      bg = "white", res = 300)  
    plot(RastMean_proj,col=rainbow(30,start=1/6)[30:1])
    xyFr <- Mercator(cbind(map(database="worldHires",regions="France",exact=TRUE,plot=FALSE,fill=TRUE,resolution=0)$x,map(database="worldHires",regions="France",exact=TRUE,fill=TRUE,plot=FALSE,resolution=0)$y),inverse=FALSE)
    polygon(xyFr[,1],xyFr[,2],col="grey",lwd=2)
    plot(RastMean_proj,col=rainbow(30,start=1/6)[30:1],add=TRUE,alpha=0.5)
  dev.off()

  jpeg(filename = "image_raster.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    plot(g_crop)
    color <- col2rgb(rainbow(30,start=1/6)[30:1])
    image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)
  dev.off()

  # Choose a new extent for graphs
  RastMean_projNewExtent <- drawExtent()
  g2 <- gmap(RastMean_projNewExtent, type='satellite',exp=1, filename="g2")
  plot(g2)

  RastMean_projZoom <- crop(x=RastMean_proj,y=RastMean_projNewExtent*1.42)

  jpeg(filename = "contour_raster.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    plot(g2)
    lev <- seq(-25,30,5)
    contour(RastMean_projZoom,col=topo.colors(length(lev))[length(lev):1],add=TRUE,levels=lev,lwd=3) #,start=1/6,end=5/6
  dev.off()

# 3D plot
  databathy <- raster("./data/bathy/w001001.adf") # read raster ArcGIS file
  plot(databathy)
  dim(databathy)
  length(which(is.na(values(databathy)) == FALSE))
  databathy_NewExtent <- drawExtent()
  databathy2 <- crop(x=databathy,y=databathy_NewExtent)
  plot(databathy2)

    # assign color according to depth
   myPal <- colorRampPalette( c("darkseagreen2", "deepskyblue","blue4") )
   color <- myPal(30)[31-c((values(databathy2)-min(values(databathy2),na.rm=TRUE))/(max(values(databathy2),na.rm=TRUE)-min(values(databathy2),na.rm=TRUE)))*30]

    #transform as matrix for plotting
   datamat <- matrix(values(databathy2),nrow=dim(databathy2)[2],ncol=dim(databathy2)[1])[,c(dim(databathy2)[1]:1)]
   colormat <- matrix(color,nrow=dim(databathy2)[2],ncol=dim(databathy2)[1])[,c(dim(databathy2)[1]:1)]

  library(rgl)
  open3d()
  rgl.bg(col="black")
  persp3d(datamat
    ,x = seq(xmin(databathy2), xmax(databathy2)-1, xres(databathy2)), y = seq(ymin(databathy2), ymax(databathy2)-1, yres(databathy2))
    ,col=colormat)
  aspect3d(1,1,0.1)
  rgl.snapshot( "3D_bathy_2.png", fmt="png", top=TRUE)

# --------------------------
## SIG tools
# --------------------------
  RastMean <- raster("./data/Grid_mean_10m.tif")
  g <- gmap('Treffiagat, Bretagne, France', type='roadmap',exp=5)
  plot(g)
  RastMean_proj <- projectRaster(from=RastMean,to=g)
  g_crop <- crop(x=g,y=extent(RastMean_proj)*1.25)

# Zoom in raster
  plot(g_crop)
  color <- col2rgb(rainbow(30,start=1/6)[30:1])
  image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)
  zoom(RastMean_proj) 

# Create points
  plot(g_crop)
  color <- col2rgb(rainbow(30,start=1/6)[30:1])
  image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)

  pt <- locator(n=10,type="p",col="black",pch=20)
  pt <- data.frame(x=pt$x,y=pt$y)
  pt <- SpatialPoints(pt,proj4string=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

  jpeg(filename = "create_plot.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
  plot(g_crop)
  color <- col2rgb(rainbow(30,start=1/6)[30:1])
  image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)
  plot(pt,pch=20,cex=5,add=TRUE)
  dev.off()

# CHULL : Convex polygon
  jpeg(filename = "create_hull.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
  plot(g_crop)
  color <- col2rgb(rainbow(30,start=1/6)[30:1])
  image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)
  hpts <- chull(coordinates(pt))
  hpts <- c(hpts, hpts[1])
  plot(pt,pch=20,cex=5,add=TRUE)
  lines(coordinates(pt)[hpts, ],lwd=2)
  dev.off()


# Create polygons
  mpol <- drawPoly(sp=FALSE, col="black", lwd=2)
  mpol2 <- drawPoly(sp=FALSE, col="red", lwd=2)
  mpol3 <- drawPoly(sp=FALSE,col="pink",lwd=2)
  jpeg(filename = "create_polygon.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
  plot(g_crop)
  color <- col2rgb(rainbow(30,start=1/6)[30:1])
  image(RastMean_proj,col=rgb(color[1,],color[2,],color[3,],150,maxColorValue=255),add=TRUE)
  polygon(mpol,lwd=2,cex=5)
  polygon(mpol2,lwd=2,cex=5,border="red",density=10,col="red")
  dev.off()

# Spatial transformation
g_croplonlat <- projectRaster(g,crs="+init=epsg:2154")
pt_lonlat <- spTransform(pt,CRS=CRS("+init=epsg:2154"))

 jpeg(filename = "sp_Transform.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    image(g_croplonlat,col=rainbow(255,start=1/6,end=5/6))
    plot(pt_lonlat,pch=20,cex=5,add=TRUE)
  dev.off()

# Spatial join
  # Points and polygons
  P1 <- SpatialPolygons(list(Polygons(list(Polygon(mpol),Polygon(mpol3)),"type1"),Polygons(list(Polygon(mpol2)),"type2")))
  pt_label <- overlay(pt,P1)
  
  jpeg(filename = "overlay_polygon.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    plot(g_crop)
    plot(P1,add=TRUE,col=c(rgb(0,255,0,100,maxColorValue=255),rgb(255,0,0,100,maxColorValue=255)))
    plot(pt,cex=5,add=TRUE,pch=1)
    plot(pt[which(pt_label==1),],col="forestgreen",cex=5,add=TRUE,pch=20)
    plot(pt[which(pt_label==2),],col="orange",cex=5,add=TRUE,pch=20)
  dev.off()

# Tools for raster
databathy2_agg <- aggregate(databathy2, fact=20, fun=mean, expand=TRUE, na.rm=TRUE)
  jpeg(filename = "aggegrate.jpeg",
      width = 20, height = 20, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300)  
    par(mfrow=c(1,2))
    image(databathy2)
    image(databathy2_agg)
  dev.off()

 # rasterize
P2 <- SpatialPolygons(list(Polygons(list(Polygon(mpol)),"type1"),Polygons(list(Polygon(mpol2)),"type2"),Polygons(list(Polygon(mpol3)),"type3")))
  r <- raster(extent(g_crop),nrows=50,ncol=50,crs=projection(g_crop))
r2 <- rasterize(P2,r)
r3 <- rasterize(pt,r2)

  jpeg(filename = "rasterize.jpeg",
      width = 20, height = 10, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    par(mfrow=c(1,2))
    plot(g2)
    plot(P2,add=TRUE,col=c(rgb(0,0,255,100,maxColorValue=255),rgb(255,0,0,100,maxColorValue=255),rgb(0,255,0,100,maxColorValue=255)))
    plot(pt,cex=4,add=TRUE,pch=20)
    plot(g2)
    image(r2,add=TRUE,breaks=c(0.5,1.5,2.5,3.5),col=c("blue","red","green"))
    image(r3,add=TRUE,breaks=c(0,0.5,11),col=c(0,1))
  dev.off()

# pente / hillShade
plot(databathy2)
dim(databathy2)
databathy_NewExtent2 <- drawExtent()
databathy3 <- crop(x=databathy2,y=databathy_NewExtent2)
plot(databathy3)
dim(databathy3)

myPal <- colorRampPalette( c("darkseagreen2", "deepskyblue","blue4") )
myPal2 <- colorRampPalette( c("red","orange", "white","green","forestgreen") )
myPal3 <- colorRampPalette(c("white","black"))

Pente_databathy3 <- terrain(databathy3, opt='slope', unit='radians', neighbors=8)
Aspect_databathy3 <- terrain(databathy3, opt='aspect', unit='radians', neighbors=8)
HillShade_databathy3 <- hillShade(Pente_databathy3,Aspect_databathy3,angle=45,direction=90)
TPI_databathy3 <- focal(databathy, w=matrix(1/25,nrow=5,ncol=5), fun=function(x) x[13] - mean(x[-13]))

  jpeg(filename = "focal.jpeg",
      width = 10, height = 10, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    par(mfrow=c(2,2),mai=c(0.2,0.2,0.2,0.2))
    image(databathy3,col=rev(myPal(30)),main="bathy")
    image(Pente_databathy3,col=rev(heat.colors(30)),main="Pente")
    image(HillShade_databathy3,col=myPal3(30),main="HillShade")
    image(TPI_databathy3,col=myPal2(5),main="TPI")
  dev.off()


#--------------------
# Geostat and models
#--------------------
 # variogram and krige
#--------------------
  library(gstat)
  data(meuse)
  coordinates(meuse)=~x+y
  proj4string(meuse) <- CRS("+init=epsg:28992")
  data(meuse.grid)
  coordinates(meuse.grid) = ~x+y
  proj4string(meuse.grid) <- CRS("+init=epsg:28992")
  gridded(meuse.grid) = TRUE

  # Project meuse into mercator
  meuse_merc <- spTransform(meuse,CRS=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

  # Find google map of the area
  meuse_lonlat <- spTransform(meuse,CRS=CRS("+init=epsg:4326"))
  g_meuse <- gmap(meuse_lonlat, type='terrain',exp=1)
  plot(g_meuse)
  plot(meuse_merc,add=TRUE)
  g_meuse_NewExtent2 <- drawExtent() # Reduce the map by clicking on the figure
  g_meuse2 <- crop(x=g_meuse,y=g_meuse_NewExtent2)

  # Variogram
  v.ok <- variogram(log(zinc)~1, data=meuse)
  ok.model <- fit.variogram(v.ok, vgm(1, "Exp", 500, 1))
  jpeg(filename = "variogram.jpeg",
      width = 10, height = 10, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    plot(v.ok, ok.model, main = "ordinary kriging",lwd=2)
  dev.off()

  # ordinary kriging
  zn.ok <- krige(log(zinc)~1,meuse, meuse.grid, model = ok.model)
  zn.ok2 <- data.frame(coordinates(zn.ok)[order(zn.ok$var1.pred),1],coordinates(zn.ok)[order(zn.ok$var1.pred),2],zn.ok$var1.pred[order(zn.ok$var1.pred)])
  names(zn.ok2) <- c("x","y","pred")
  coordinates(zn.ok2) <- ~x+y
  proj4string(zn.ok2) <- CRS("+init=epsg:28992")
  zn.ok2_proj <- spTransform(zn.ok2,CRS=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

  # inverse distance
  zn.idw = krige(log(zinc)~1, meuse, meuse.grid)
  zn.idw2 <- data.frame(coordinates(zn.idw)[order(zn.idw$var1.pred),1],coordinates(zn.idw)[order(zn.idw$var1.pred),2],zn.idw$var1.pred[order(zn.idw$var1.pred)])
  names(zn.idw2) <- c("x","y","pred")
  coordinates(zn.idw2) <- ~x+y
  proj4string(zn.idw2) <- CRS("+init=epsg:28992")
  zn.idw2_proj <- spTransform(zn.idw2,CRS=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

  # Transform the dataframe of results (ordinary kriging) into a matrix of coordinates for plotting
  zn.ok_tab <- xtabs(zn.ok2_proj$pred ~ round(coordinates(zn.ok2_proj)[,1]/70,digits=0) + round(coordinates(zn.ok2_proj)[,2]/70,digits=0))
  zn.tab <- table(round(coordinates(zn.ok2_proj)[,1]/70,digits=0) , round(coordinates(zn.ok2_proj)[,2]/70,digits=0))
  zn.ok_tab2 <- zn.ok_tab/zn.tab
  step_brk<-0.25
  brk <- seq(trunc(min(zn.ok2_proj$pred,na.rm=TRUE)),round(max(zn.ok2_proj$pred,na.mr=TRUE))+step_brk,step_brk)
  ColorRamp <-  rev(heat.colors(length(brk)-1))

  jpeg(filename = "ordinary_kriging.jpeg",
      width = 10, height = 10, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(5,1), heights=c(1,1))
    plot(g_meuse2)
    image(as.numeric(as.character(rownames(zn.ok_tab2)))*70,as.numeric(as.character(colnames(zn.ok_tab2)))*70,zn.ok_tab2
	,breaks=brk,col=ColorRamp,add=TRUE)
    plot(meuse_merc,add=TRUE)
    # A North arrow
    SpatialPolygonsRescale(layout.north.arrow(1), offset= c(636400,6620800), scale = 800, plot.grid=F)
    # A scale bar:
    SpatialPolygonsRescale(layout.scale.bar(), offset= c(637500,6621200), scale= 1000 , fill=c("transparent", "black"), plot.grid= F)
    text(638500,6621200,"1km",pos=1,cex=2)

    # Add a colorbar as a legend
    par(mar = c(5,3,5,2))
    image(1, brk+step_brk*0.5,
	  matrix(data=brk, ncol=length(brk),nrow=1),
	  col=ColorRamp,
	  xlab="",ylab="",
	  xaxt="n",cex.axis=2)
  dev.off()

  # Transform the dataframe of results (idw) into a matrix of coordinates for plotting
  zn.idw_tab <- xtabs(zn.idw2_proj$pred ~ round(coordinates(zn.idw2_proj)[,1]/70,digits=0) + round(coordinates(zn.idw2_proj)[,2]/70,digits=0))
  zn.tab <- table(round(coordinates(zn.idw2_proj)[,1]/70,digits=0) , round(coordinates(zn.idw2_proj)[,2]/70,digits=0))
  zn.idw_tab2 <- zn.idw_tab/zn.tab
  step_brk<-0.25
#   brk <- seq(trunc(min(zn.idw2_proj$pred,na.rm=TRUE)),round(max(zn.idw2_proj$pred,na.mr=TRUE))+step_brk,step_brk)
#   ColorRamp <-  rev(heat.colors(length(brk)-1))

  jpeg(filename = "idw.jpeg",
      width = 10, height = 10, units = "cm", pointsize = 5,
      quality = 100,
      bg = "white", res = 300) 
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(5,1), heights=c(1,1))
    plot(g_meuse2)
    image(as.numeric(as.character(rownames(zn.idw_tab2)))*70,as.numeric(as.character(colnames(zn.idw_tab2)))*70,zn.idw_tab2
	,breaks=brk,col=ColorRamp,add=TRUE)
    plot(meuse_merc,add=TRUE)
      # A North arrow
      SpatialPolygonsRescale(layout.north.arrow(1), offset= c(636400,6620800), scale = 800, plot.grid=F)
      # A scale bar:
      SpatialPolygonsRescale(layout.scale.bar(), offset= c(637500,6621200), scale= 1000 , fill=c("transparent", "black"), plot.grid= F)
      text(638500,6621200,"1km",pos=1,cex=2)
    # Add a colorbar as a legend
    par(mar = c(5,3,5,2))
    image(1, brk+step_brk*0.5,
	  matrix(data=brk, ncol=length(brk),nrow=1),
	  col=ColorRamp,
	  xlab="",ylab="",
	  xaxt="n",cex.axis=2)
  dev.off()


#--------------------
# Modelisation
#--------------------
# Read stations
  data_PA <- read.csv("./data/Morlaix_PA.csv",sep=",",dec=".",header=TRUE)
  head(data_PA)
  Y_data_sp_A <- SpatialPointsDataFrame(coords=data_PA[,c("x","y")],data=as.data.frame(data_PA[,c("Pres")]),proj4string=CRS("+proj=longlat +datum=WGS84"))
  names(Y_data_sp_A) <- "dataY"
  head(Y_data_sp_A)
  rm(data_PA)

  # Factor layers
  datarock_merc <- raster("./data/datarock_merc.tif")
  databathy_merc <- raster("./data/databathy_merc.tif")
  datakpar_merc <- raster("./data/datakpar_merc.tif")
  dataexp_cur_force_merc <- raster("./data/dataexp_cur_force_merc.tif")
  dataTerrain_bpi_merc <- raster("./data/dataTerrain_bpi_merc.tif")  

  # Find google map of the area
  g_morlaix <- gmap(Y_data_sp_A, type='satellite',exp=1.5)
  plot(g_morlaix)
  Y_data_sp_A_merc <- spTransform(Y_data_sp_A,CRS=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
  plot(Y_data_sp_A_merc,add=TRUE,col="red")
  NewExtent <- drawExtent() # Reduce the map by clicking on the figure
  g_morlaix2 <- gmap(NewExtent, type='satellite',exp=1)

  NewExtent_Poly <- rbind(c(xmin(NewExtent),ymin(NewExtent)),c(xmin(NewExtent),ymax(NewExtent))
	    ,c(xmax(NewExtent),ymax(NewExtent)),c(xmax(NewExtent),ymin(NewExtent))
	    ,c(xmin(NewExtent),ymin(NewExtent)))
  
  Y_data_sp_A_merc2 <- Y_data_sp_A_merc[which(point.in.polygon(point.x=coordinates(Y_data_sp_A_merc)[,1],point.y=coordinates(Y_data_sp_A_merc)[,2]
		    ,pol.x=NewExtent_Poly[,1],pol.y=NewExtent_Poly[,2]) == 1),]

  # Reduce map of factors to the extent chosen; resample to get rasters of same extent and resolution for stack.
  r <- raster(NewExtent,nrow=2024,ncol=879,crs=proj4string(g_morlaix2))
  datarock_merc2 <- resample(datarock_merc,r,method="ngb")
  databathy_merc2 <- resample(databathy_merc,r,method="ngb")
  datakpar_merc2 <- resample(datakpar_merc,r,method="ngb")
  dataexp_cur_force_merc2 <- resample(dataexp_cur_force_merc,r,method="ngb")
  dataTerrain_bpi_merc2 <- resample(dataTerrain_bpi_merc,r,method="ngb")

  RasterForPred <- stack(datarock_merc2,databathy_merc2,datakpar_merc2,dataexp_cur_force_merc2,dataTerrain_bpi_merc2)
  layerNames(RasterForPred) <- c("datarock_merc2","databathy_merc2","datakpar_merc2","dataexp_cur_force_merc2","dataTerrain_bpi_merc2")  

  # Graph of factors
  jpeg(filename = "datarock.jpeg",
    width = 8, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300)
    par(mar=c(3,3,1,13))
    plot(g_morlaix2,axes=TRUE,xaxs="i",yaxs="i")
    plot(datarock_merc2,add=TRUE,col=c("NA","grey90"),legend=FALSE)
    lines(NewExtent_Poly,lwd=3)
    plot(Y_data_sp_A_merc2,add=TRUE,col=c("red","green")[Y_data_sp_A_merc2$dataY+1])
    legend(-433500,6231000,legend=c("Presence","Absence","Roches"),col=c("green","red",NA),border=c(NA,NA,1)
      ,fill=c(NA,NA,"grey90"),pch=c(3,3,NA),bty="n",cex=2,xpd=TRUE)	
  dev.off()
  jpeg(filename = "databathy.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    plot(databathy_merc2,add=TRUE)
    lines(NewExtent_Poly,lwd=3)
    plot(Y_data_sp_A_merc2,add=TRUE,col=c("red","green")[Y_data_sp_A_merc2$dataY+1])
  dev.off()  
  jpeg(filename = "datakpar.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    plot(datakpar_merc2,add=TRUE)
    lines(NewExtent_Poly,lwd=3)
    plot(Y_data_sp_A_merc2,add=TRUE,col=c("red","green")[Y_data_sp_A_merc2$dataY+1])
  dev.off()  
  jpeg(filename = "dataexp_cur_force.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    plot(dataexp_cur_force_merc2,add=TRUE)
    lines(NewExtent_Poly,lwd=3)
    plot(Y_data_sp_A_merc2,add=TRUE,col=c("red","green")[Y_data_sp_A_merc2$dataY+1])
  dev.off()  
  jpeg(filename = "dataTerrain_bpi.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    plot(dataTerrain_bpi_merc2,add=TRUE)
    lines(NewExtent_Poly,lwd=3)
    plot(Y_data_sp_A_merc2,add=TRUE,col=c("red","green")[Y_data_sp_A_merc2$dataY+1])
  dev.off()  
  
  # Extract factors for each data point
  Y_data_rock <- extract(datarock_merc2,Y_data_sp_A_merc2)
  Y_data_sp_A_merc2_B <- Y_data_sp_A_merc2[which(Y_data_rock > 0.5),]
  
  # And combine to data observation
  physParam <- extract(x=RasterForPred,y=Y_data_sp_A_merc2_B)
  Y_data_sp  <- data.frame(Y_data_sp_A_merc2_B[,"dataY"],physParam)
  coordinates(Y_data_sp) <- ~x+y  
  proj4string(Y_data_sp) <- proj4string(Y_data_sp_A_merc2_B) 

  # Gam model on data (Presence/absence => Binomial)
  library(mgcv)
  model <- "dataY ~ s(databathy_merc2,k=4) + s(dataTerrain_bpi_merc2,k=4) + s(dataexp_cur_force_merc2,k=4)"
  gamY <- gam(as.formula(model),method="REML", data=Y_data_sp,family="binomial")
  summary(gamY)
  jpeg(filename = "gam_outputs.jpeg",
    width = 18, height = 6, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    par(mfrow=c(1,3),mar=c(5,6,1,1.5))
    plot(gamY,scale=0,cex.axis=1.5,cex.lab=3)
  dev.off()

  # Produce a raster of prediction from stack and model
  predfun <- function(model, data, sefit=FALSE) {
    v <- predict(model, data, se.fit=TRUE, type="response")
    if(sefit==FALSE){p=as.vector(v$fit)}else{p=as.vector(v$se.fit)}
    p
  }
  predmap <- predict(RasterForPred, model=gamY, fun=predfun, sefit=FALSE, index=1)
  predmap.se <- predict(RasterForPred, model=gamY, fun=predfun, sefit=TRUE, index=1)

  # plot the estimates on rocks only
  values(predmap)[which(values(RasterForPred)[,"datarock_merc2"] <= 0.5)] <- NA
  jpeg(filename = "predmap_fit.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    polygon(NewExtent_Poly,lwd=3,col=rgb(255,255,255,150,max=255),border="black")
    SavePar <-  par() # Save graph parameters
    plot(datarock_merc2,add=TRUE,col=c("NA","grey50"),legend=FALSE)
    par(SavePar) # Load graph parameters (they change when plotting a raster)
    plot(predmap,add=TRUE)
    par(SavePar) # Load graph parameters (they change when plotting a raster)
    polygon(NewExtent_Poly,lwd=3)
      # A North arrow
      SpatialPolygonsRescale(layout.north.arrow(1), offset= c(-435594,6231000), scale = 1800, plot.grid=F)
      # A scale bar:
      SpatialPolygonsRescale(layout.scale.bar(height = 0.1), offset= c(-442000,6231500), scale= 5000 , fill=c("transparent", "black"), plot.grid= F)
      text(-437000,6231500,"5km",pos=1,cex=1.5)
  dev.off()  

  # plot the standard error of fits on rocks only
  values(predmap.se)[which(values(RasterForPred)[,"datarock_merc2"] <= 0.5)] <- NA
  myPal <- colorRampPalette( c("red", "orangered","yellow","white") )
  jpeg(filename = "predmap_se.fit.jpeg",
    width = 6, height = 10, units = "cm", pointsize = 5,
    quality = 100,
    bg = "white", res = 300) 
    plot(g_morlaix2)
    polygon(NewExtent_Poly,lwd=3,col=rgb(255,255,255,150,max=255),border="black")
    SavePar <-  par() # Save graph parameters
    plot(datarock_merc2,add=TRUE,col=c("NA","grey50"),legend=FALSE)
    par(SavePar) # Load graph parameters (they change when plotting a raster)
    plot(predmap.se,add=TRUE,col=rev(myPal(30)))
    par(SavePar) # Load graph parameters (they change when plotting a raster)
    polygon(NewExtent_Poly,lwd=3)
      # A North arrow
      SpatialPolygonsRescale(layout.north.arrow(1), offset= c(-435594,6231000), scale = 1800, plot.grid=F)
      # A scale bar:
      SpatialPolygonsRescale(layout.scale.bar(height = 0.1), offset= c(-442000,6231500), scale= 5000 , fill=c("transparent", "black"), plot.grid= F)
      text(-437000,6231500,"5km",pos=1,cex=1.5)
  dev.off()  

  # 3D view
    # assign color according to depth
   myPal <- colorRampPalette( c("deepskyblue","blue4") )
   color <- myPal(30)[31-c((values(databathy_merc2)-min(values(databathy_merc2),na.rm=TRUE))/(max(values(databathy_merc2),na.rm=TRUE)-min(values(databathy_merc2),na.rm=TRUE)))*30]
   color[which(values(datarock_merc2) > 0.5)] <- "grey"
   colorPred <- terrain.colors(30)[31-c((values(predmap)-min(values(predmap),na.rm=TRUE))/(max(values(predmap),na.rm=TRUE)-min(values(predmap),na.rm=TRUE)))*30]
   color[which(is.na(values(predmap)) == FALSE)] <- colorPred[which(is.na(values(predmap)) == FALSE)]

    #transform as matrix for plotting
   datamat <- matrix(-values(databathy_merc2),nrow=dim(databathy_merc2)[2],ncol=dim(databathy_merc2)[1])[,c(dim(databathy_merc2)[1]:1)]
   colormat <- matrix(color,nrow=dim(databathy_merc2)[2],ncol=dim(databathy_merc2)[1])[,c(dim(databathy_merc2)[1]:1)]

    library(rgl)
    open3d()
    rgl.bg(col="black")
    persp3d(datamat
      ,x = seq(xmin(databathy_merc2), xmax(databathy_merc2)-1, xres(databathy_merc2)), y = seq(ymin(databathy_merc2), ymax(databathy_merc2)-1, yres(databathy_merc2))
      ,col=colormat)
    aspect3d(1,2.4,0.2)
    # 3D Scale 
    segments3d(x=c(-440554,-439554)-1000,y=c(6220878,6220878)-4000,z=c(0,0),col="white",lwd=4) #X
    segments3d(x=c(-440554,-440554)-1000,y=c(6220878,6221878)-4000,z=c(0,0),col="white",lwd=4) #Y
    segments3d(x=c(-440554,-440554)-1000,y=c(6220878,6220878)-4000,z=c(0,20),col="white",lwd=4) #Z
    # save the 3d image
    rgl.snapshot( "3D_results.png", fmt="png", top=TRUE)
