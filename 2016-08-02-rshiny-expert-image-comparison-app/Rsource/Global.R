# Quantitative cell micromechanics in Arabidopsis
# Publication: http://onlinelibrary.wiley.com/doi/10.1111/tpj.13290/full
#
# Sébastien Rochette
# https://statnmap.com
#
# -- Global environment -- #
rawWD <- getwd()
dataWD <- paste0(rawWD, "/data/")
imgWD <- paste0(dataWD, "imgWD/")
imgData <- paste0(dataWD, "imgData")
polyWD <- paste0(dataWD, "polyWD/")
outWD <- paste0(rawWD, "/outWD_user/")
outWD_details <- paste0(rawWD, "/outWD_details/")

# Retrieve all image names
all.cells.names <- unlist(lapply(strsplit(list.files(imgData, full.names = TRUE), 
                                          split = "_Cell"),function(x) x[1]))
all.single.names <- unique(all.cells.names)
# Number of cells for each meristem
Cell.count <- table(all.cells.names)
max.Cells <- max(Cell.count)

# Cells that do not exist
Cell.count.N <- rbind(as.factor(names(Cell.count)), Cell.count)
Cell.out <- do.call(rbind, apply(Cell.count.N, 2, function(x) {
  if (x[2] < 80 ) {cbind(x[1], (x[2] + 1):80)} else {NULL}
})
)

# User defined variables
Which.Line <-   c(1,1,1,2,2,2,1,1,1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4)
Which.Indent <- c(1,0,0,0,1,1,1,0,0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0)

# Cbind Meristem, Line, Indent
Meristem <- data.frame(1:length(Which.Line), Which.Line, Which.Indent)
names(Meristem) <- c("Meristem", "Line", "Indent")

# List all possible cells : Meristem * nb of cells
all.cells <- cbind(rep(1:length(Which.Line), each = max.Cells),
                   rep(1:max.Cells, length(Which.Line)))

# Read previous expertises and combine results
userfiles <- list.files(outWD,full.names = TRUE)[grep("user", list.files(outWD))]
userdata <- array(dim = c(max.Cells, length(Which.Line), length(userfiles)))
if (length(userfiles) != 0) {
  for (i in 1:length(userfiles)) {
    userdata[,,i] <- as.matrix(read.csv(file = userfiles[i],
                                        header = TRUE, row.names = 1, sep = ","))
  }
}  
userdata.compare <- t(apply(userdata,1,function(x) apply(x,1, 
                                                         function(y) sum(y == 1, na.rm = TRUE)/sum(!is.na(y)))))
userdata.compare[(userdata.compare < 0.5 & !is.na(userdata.compare))] <- 
  1 - userdata.compare[(userdata.compare < 0.5 & !is.na(userdata.compare))]
