##########################################
##
## Luis Manuel Román García
## luis.roangarci@gmail.com
##
## ---------------------------------------
##
## Utileries for general geo-statistical
## manipulation.
##
##########################################

## Read in functions
source("./add_functions.R")

## ---------------------------------
## Poblational data
## for rural communities
## and urban without DENUE
## ---------------------------------
data <- read.csv("../data/population/censo_filter.csv")
data <- data[, c(7, 8, 10)]

## ---------------------------------
## MAP
## ---------------------------------
map         <- get_map(location = "Monterrey",
                      zoom     = 8,
                      maptype  = "roadmap")
map.plot  <- ggmap(map)

## ---------------------------------
## CP
## ---------------------------------
## shapes_post_nl <- readOGR("../data/nl/",
##                         "CP_19NL_v2")

## PLOT STRUCTURE
## shapes_post_nl@data$id = rownames(shapes_post_nl@data)
## shapes_post_nl.points  = fortify(shapes_post_nl, region = "id")
## shapes_post_nl.df      = join(shapes_post_nl.points,
##                              shapes_post_nl@data, by = "id")

## PLOT
## ggplot(shapes_post_nl.df) +
##    aes(long, lat, group = group, fill = d_cp) +
##    geom_polygon() +
##    geom_path(color = "white") +
##    coord_equal() +
##    theme(legend.position="none",
##          panel.background = element_blank())

## ---------------------------------
## DENUE
## ---------------------------------
## READ IN POINTS
pts_denu_nl    <- read.csv("../data/denue/DENUE_INEGI_19_.csv",
                          stringsAsFactors = FALSE)
pts_denu_nl    <- pts_denu_nl[,c(40, 39, 1:38, 41)]

## PLOT
map.plot + geom_point(data      = pts_denu_nl,
                      aes(x     = Longitud,
                          y     = Latitud,
                          color = as.factor(Código.Postal)
                          ),
                      alpha = .4,
                      size  = .5) +
    theme(legend.position  = "none",
          panel.background = element_blank())
## Shape
#shapes_denu_nl <- readOGR("../data/denue/nl",
#                         "nl_localidad_urbana_y_rural_amanzanada")

#shapes_denu_nl@data$id = rownames(shapes_denu_nl@data)
#shapes_denu_nl.points  = fortify(shapes_denu_nl, region = "id")
#shapes_denu_nl.df      = join(shapes_denu_nl.points,
#                              shapes_denu_nl@data, by = "id")
## PLOT
#map.plot +
#    geom_polygon(data = shapes_denu_nl.df,
#    aes(long, lat, group = group, fill = CVE_MUN))+
#    geom_path(color = "white") +
#    coord_equal() +
#    theme(legend.position="none",
#          panel.background = element_blank())

#############################
###########Pruebas###########
#############################

### Size of cells
### Partition
grid      <- 6400                                     # Number of cells
tes       <- tesselate(grid,  map.plot, alpha = .05)  # Partition
block     <- blocks(tes[[2]], tes[[3]])               # Cell creation
cell_feat <- in.block(block,  pts_denu_nl)            # Cell characteristics

## Data population
data_pop  <- in.block(block, data)
allpop_1  <- laply(data_pop, function(t)t <- t[[3]])

## Save Results
## testJson <- RJSONIO::toJSON(cell_feat)
## write(testJson, "../data/output/cell_data.json")
## cell_feat <- rjson::fromJSON(file = "../data/output/cell_data.json")

## Save Results
## blockJson <- RJSONIO::toJSON(block)
## write(blockJson, "../data/output/all_blocks.json")

## ------------------------------
## Análisis
## ------------------------------
## Área por celda
areatest_1 <- laply(cell_feat, function(t)t <- t[[4]])

## Observaciones por celda
obs_1      <- laply(cell_feat, function(t)t <- nrow(t[[2]]))

## ------------------------------
## Operations with blocks
## ------------------------------

## Read in data
## blocks   <- rjson::fromJSON(file = "../data/output/all_blocks.json")

## Blocks over Nuevo León
mun_nl    <- readOGR("../data/area_example/recorte",
                    "recorte_municipios")

## Extract Monterrey
## monterrey <- mun_nl[mun_nl$CVE_MUN == "039",]

## Block centers
centers <- laply(block,
                  function(t)t <- gcIntermediate(t[[1]], t[[2]], 1))
centers <- unlist(centers)

## Check in shape
inside <- c()
for(i in 1:length(block)){
    center <- data.frame(centers[i,1],
                        centers[i,2])
    names(center)        <- c("lon", "lat")
    coordinates(center)  <- c("lon", "lat")
    proj4string(center)  <- proj4string(mun_nl)
    inside[i]            <- !is.na(over(center,
                                       as(mun_nl,
                                          "SpatialPolygons")))
    print(i)
}

## Obtain cells inside poligon
k <- 1
blocks_in     <- list()
for(i in 1:length(block)){
    if(inside[i] == TRUE){
        blocks_in[[k]]     <- block[[i]]
        k <- k + 1
    }
}

## Block CP
block_cp <- laply(cell_feat,
                 function(t) t <- {
                     res <- NA
                     if(length(t) > 0){
                         if(length(t[[2]]$Código.Postal) > 0){
                             aux = plyr::count(t[[2]]$Código.Postal)
                             res = aux$x[order(aux$freq, decreasing = TRUE)][1]
                         }
                     }
                     res
                 })

## Google CP ##
google_index <- which(is.na(block_cp) & inside) ## este arreglo va a ser para llenar block_cp_inside
goog_cp      <- data.frame(centers[is.na(block_cp) & inside,])
for(i in 1:nrow(goog_cp)){
    goog_cp_estimate <- get_cp(as.numeric(goog_cp[i,]))
    print(goog_cp_estimate)
}
## Save Results
blockJson <- RJSONIO::toJSON(blocks_in)
write(blockJson, "../data/output/blocks_in_mun__nl.json")

## ------------------------------
## Blocks to shapes
## ------------------------------
all_blocks_shp <- list()

## Build shape
for(i in 1:length(blocks_in)){
    block     <- blocks_in[[i]]
    block_shp <- data.frame(
        x = c(rep(block[[1]][1],2),
              rep(block[[2]][1],2)
              ),
        y = c(rep(c(block[[1]][2],
                    block[[2]][2]),2))
    )
    block_shp[c(3,4),] <- block_shp[c(4,3),]

    ## Convertir a Polígono
    block_shp <- Polygon(block_shp)
    block_shp <- Polygons(list(block_shp), paste0(i))
    all_blocks_shp[[i]] <-  block_shp
}

## Aquí van a ir todos.
block_shp <- SpatialPolygons(all_blocks_shp)

poly      <- SpatialPolygonsDataFrame(
    block_shp,
    data.frame(
        PIDS      = paste(seq(1, length(blocks_in), 1), sep = "\n"),
        block_cp  = block_cp[inside],
        block_pop = allpop_1[inside]
    )
)

## Escribir resultados
writeOGR(poly,
         "../data/output/blocks",
         "block_nl",
         driver = "ESRI Shapefile")
