# Data preparation for visual expertise
#
# Sébastien Rochette
# https://statnmap.com
# May 2016

rm(list=ls())

library(sp)
library(raster)

rawWD <- "~/Rshiny/Visual_Expert/"
dataWD <- paste0(rawWD,"data/")
imgWD <- paste0(dataWD,"imgWD/")
imgData <- paste0(dataWD,"imgData/")
polyWD <- paste0(dataWD,"polyWD/")
outWD <- paste0(rawWD,"outWD/")

# Source the image creation function
source(paste0(rawWD,"Rsource/functions.R"))


  # Get all images names
  split.names <- strsplit(list.files(imgWD),".",fixed=TRUE)
  all.names <- unique(unlist(lapply(split.names,function(x) paste(x[-length(x)],collapse="."))))

  # List of unique meristem images names (one for all time steps)
  split.names <- strsplit(all.names,"_")
  all.single.names <- unique(unlist(lapply(split.names,function(x) paste(x[-length(x)],collapse="_"))))
  all.single.names <- all.single.names[-grep("T0",all.single.names)] # One cell with "bis" name
  
  # Number of cells in each meristem
  Cell.count <- unlist(lapply(seq(1,length(all.names),2),function(x) {
    load(file=paste0(polyWD,"SpP_",all.names[x],".RData"))
    length(SpP.save)
  }))

  # Loop on all meristems and all cells
  res <- apply(t(1:length(all.single.names)), 2, function(m) { # m <- 1
    img.names <- all.names[grep(all.single.names[m],all.names)]
      apply(t(1:Cell.count[m]), 2, function(cell) { # cell <- 1
      # x11(w = 8*length(img.names), h = 8)
      jpeg(filename=paste0(imgData,all.single.names[m],"_Cell_",cell,".jpg"),
        width = 4 * length(img.names), height = 4, units = "cm", pointsize = 5,
        quality = 100,
        bg = "white", res = 200)
        par(mai = c(0.15, 0.2, 0.1, 0.1))
        ShowCell(img.names = img.names, w.cell = cell, imgWD = imgWD, polyWD = polyWD)
      dev.off()
    })
  })
  
