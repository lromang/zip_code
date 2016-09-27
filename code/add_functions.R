##########################################
##
## Luis Manuel Román García
## luis.roangarci@gmail.com
##
## ---------------------------------------
##
## Libraries for general geo-statistical
## manipulation.
##########################################

## JSON manipulatino
library(jsonlite)
library(rjson)
library(RJSONIO)
## Urls manipulation
library(RCurl)
## Manejo de arreglos
library(plyr)
library(dplyr)
library(tidyr)
## Manejo de cadenas de caracteres
library(stringr)
## Manejo de data frames
library(data.table)
## Predicción
library(caret)
## Geoespacial
library(geosphere)
library(maps)
library(maptools)
library(spatstat)
library(rgeos)
library(rgdal)
## Gráficas
library(ggplot2)
## Otros
library(ggmap)
library(deldir)
library(rje)
library(sp)
library(SDMTools)
library(PBSmapping)
library(sp)
library(prevR)
library(foreign)

########################################
## Functions
########################################

##-------------------------------------
## get_directions
##-------------------------------------
get_directions <- function(origen, destino, mode = "driving"){
    ##-------------------------------------
    ## This function uses Google's API directions to
    ## calculate the driving directions between two given points.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    base        <- "https://maps.googleapis.com/maps/api/directions/json?"
    origin      <- paste0("origin=",
                         paste(origin, collapse = ",")
                         )
    destiny     <- paste0("destination=",
                         paste(destiny, collapse = ",")
                         )
    mode        <- paste0("mode=", mode)
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base, origin, destiny, mode, key, sep = "&")
    route       <- fromJSON(getURL(query))$routes[[1]]$legs
    steps       <- route[[1]]$steps
    list(
        "durations"  = ldply(steps, function(t)t <- t$duration$text)[, 1],
        "distance"   = ldply(steps, function(t)t <- t$distance$text)[, 1],
        "start_loc"  = ldply(steps, function(t)t <- t$start_location),
        "end_loc"    = ldply(steps, function(t)t <- t$end_location)
        )
}

##-------------------------------------
## get_distance
##-------------------------------------
get_distance <- function(origin, destiny){
    ##-------------------------------------
    ## This function uses Google's API "directions" to
    ## calculate the driving distance between two points given.
    ## origin  = geografic point in (latitude, longitude) format
    ## destiny = geografic point in (latitude, longitude) format
    ##-------------------------------------
    base        <- "https://maps.googleapis.com/maps/api/distancematrix/json?"
    origin      <- paste0("origins=",
                         paste(origin[,2:1], collapse = ",")
                         )
    destiny     <- paste0("destinations=",
                         paste(destiny[,2:1], collapse = ",")
                         )
    key         <- "key=AIzaSyAkW2m1J6oq_UblEtwhzVB9EYmz7Ayc4k0"
    query       <- paste(base, origin, destiny, key, sep = "&")
    results     <- fromJSON(getURL(query))
    distance    <- results$rows[[1]]$elements[[1]]$distance$value
    distance
}

##-------------------------------------
## distance_matrix
##-------------------------------------
distance_matrix <- function(origin, destiny){
    ##-------------------------------------
    ## origin and destiny are arrays
    ##-------------------------------------
    all_dist_m <- data.frame(matrix(0, nrow(origin), nrow(destiny)))
    for(i in 1:nrow(origin)){
        for(j in 1:nrow(destiny)){
            all_dist_m[i, j] <- get_distance(origin[i, ],
                                            destiny[j, ]
                                            )
        }
    }
    all_dist_m
}

##-------------------------------------
## get_connect
##-------------------------------------
get_connect <- function(lat, lon, dist = 20, net = 3){
    base       <- "http://api.opensignal.com/v2/networkstats.json?"
    key        <- "apikey=ca887c76265ad8d9268df0a9cc2de523"
    network    <- paste0("network_type=",net)
    format     <- "json_format=2"
    distance   <- paste0("distance=",dist)
    data <- list()
    ## Get network parameters for each of the coordinates
    for(i in 1:length(lat)){
        latitude  <- paste0("lat=", lat[i])
        longitud  <- paste0("lng=", lon[i])
        url       <- paste(base,
                          latitude,
                          longitud,
                          distance,
                          format,
                          network,
                          key,
                          sep = "&")
        data[[i]]   <- fromJSON(getURL(url))
        Sys.sleep(6.5)
    }
    data
}

##-------------------------------------
## transform coord
##-------------------------------------
trans_coord <- function(coord, pow = 1){
    length <- str_length(coord)
    sec    <- str_sub(coord, length - 1, length)
    min    <- str_sub(coord, length - 3, length - 2)
    deg    <- str_sub(coord, 1, length - 4)
    ## Default transform lon.
    (-1)^pow * (extract_numeric(deg) + extract_numeric(min) / 60 +
      extract_numeric(sec) / 3600)
}

##-------------------------------------
## tesselate
##-------------------------------------
tesselate <- function(grids,
                     map          = NULL,
                     alpha        = .3,
                     top_left     = c(-118.383398, 32.948893),
                     bottom_left  = c(-118.383398, 14.160275),
                     top_right    = c(-86.783107,  32.948893),
                     bottom_right = c(-86.783107,  14.160275)){
  results  <- list()
  intercepts <- ceiling(sqrt(grids+1))
  h_lines <- data.frame(x    = rep(top_left[1], intercepts),
                       y    = seq(top_left[2], bottom_left[2],
                                  length = intercepts),
                        xend = rep(top_right[1], intercepts),
                       yend  = seq(top_right[2], bottom_right[2],
                                  length = intercepts))
  v_lines <- data.frame(x = seq(top_left[1], top_right[1],
                               length = intercepts),
                        y = rep(top_left[2], intercepts),
                       xend = seq(bottom_left[1], bottom_right[1],
                                  length = intercepts),
                        yend = rep(bottom_right[2],intercepts))
  if (! is.null(map)){
      map    <- map +  geom_hline(data = h_lines, aes(yintercept = y),
                                 col = "red",
                                 alpha = alpha)
      map    <- map +  geom_vline(data = v_lines, aes(xintercept = x),
                                 col = "red",
                                 alpha = alpha)
}
  x <- seq(top_left[1], top_right[1], length = intercepts)
  y <- seq(top_right[2], bottom_right[2],length = intercepts)
  k = 1
  xcoord <- c()
  ycoord <- c()
  for (i in 1:(length(y)-1)){
    for(j in 1:(length(x)-1)){
      xcoord[k] <- (x[j] + x[j + 1])/2
      ycoord[k] <- (y[i] + y[i + 1])/2
      k = k + 1
    }
  }
  points <- data.frame(x = xcoord, y = ycoord)
  results[[1]] <- map
  # ordenadas
  results[[2]] <- seq(top_left[2], bottom_left[2],length = intercepts)
  # abscisas
  results[[3]] <- seq(bottom_left[1],bottom_right[1],length = intercepts)
  # area aproximada de la celda
  results[[4]] <- (abs((bottom_right[1]-top_left[1]))*
                  abs((top_left[2]-bottom_right[2])))/grids
  # centro de la celda
  results[[5]] <- points

  return(results)
}


##-------------------------------------
## blocks
##-------------------------------------
blocks <- function(ordinates, abscises){
  block <- list()
  k        <- 1
  for (i in 1:(length(ordinates)-1)){
    for ( j in 1:(length(abscises)-1)){
      entry      <- list()
      entry[[1]] <- c(abscises[j], ordinates[i])  # upper left
      entry[[2]] <- c(abscises[j + 1], ordinates[i + 1])  # bottom right
      block[[k]] <- entry
      k = k + 1
    }
  }
  block
}



##-------------------------------------
## in.block
##-------------------------------------
in.block <- function(block, pop){
  pop.block <- list()
  for ( i in 1:length(block)){
  print(i)
  list   <- block[[i]]
  pop.xreduce   <- c()
  pop.xyreduce <- c()
  # Obtiene localidades cuyas coordenadas caigan dentro de la celda i
  pop.xreduce   <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
  pop.xreduce   <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
  pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
  pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
  ## Una vez seleccionadas las localidades obtiene datos relevantes.
  data.block <- list()
  data.block[[1]] <- list                             ## coordenadas extremas de cada celda
  pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
  data.block[[2]] <- pop.xyreduce                     ## datos de las localidades
  data.block[[3]] <- sum(pop.xyreduce[, 3])           ## población por celda
  ## data.block[[4]] <- mean(pop.xyreduce[, 4])      ## altura promedio de la celda
  data.block[[4]] <-  areaPolygon(matrix(
      c(data.block[[1]][[1]][1],data.block[[1]][[1]][2],
        data.block[[1]][[2]][1],data.block[[1]][[1]][2],
        data.block[[1]][[2]][1],data.block[[1]][[2]][2],
        data.block[[1]][[1]][1],data.block[[1]][[2]][2] ),
      nrow = 4,
      ncol = 2,
      byrow = TRUE ) )/1e6 ## area de cada celda km^²
  ## data.block[[5]] <- sd(pop.xyreduce[, 4])              # desviación estandar de las altitudes
  pop.block[[i]]  <- data.block
  }
  pop.block
}

## Datos más específicos por celda
in.block.fac <- function(block, pop){
  pop.block <- list()
  for ( i in 1:length(block)){
    print(i)
    list                  <- block[[i]]
    pop.xreduce  <- c()
    pop.xyreduce <- c()
    ## Obtiene localidades cuyas coordenadas caigan dentro de la celda i
    pop.xreduce  <- pop[pop[, 1] > as.numeric(list[[1]][1]), ]
    pop.xreduce  <- pop.xreduce[pop.xreduce[, 1] < as.numeric(list[[2]][1]), ]
    pop.xyreduce <- pop.xreduce[pop.xreduce[, 2] > as.numeric(list[[2]][2]), ]
    pop.xyreduce <- pop.xyreduce[pop.xyreduce[, 2] < as.numeric(list[[1]][2]), ]
    ## Una vez seleccionadas las localidades obtiene datos relevantes.
    data.block         <- list()
    data.block[[1]] <- list                    ## coordenadas extremas de cada celda
    pop.xyreduce$celda <- rep(i,nrow(pop.xyreduce))
    data.block[[2]] <- pop.xyreduce            ## datos por celda
    data.block[[3]] <- nrow(pop.xyreduce)      ## total apariciones por celda
    data.block[[4]] <-  areaPolygon(matrix(
        c(data.block[[1]][[1]][1],data.block[[1]][[1]][2],
          data.block[[1]][[2]][1],data.block[[1]][[1]][2],
          data.block[[1]][[2]][1],data.block[[1]][[2]][2],
          data.block[[1]][[1]][1],data.block[[1]][[2]][2] ),
        nrow = 4,
        ncol = 2,
        byrow = TRUE ) )/1e6 ## area de cada celda m^²    
    pop.block[[i]]   <- data.block
  }
  pop.block
}


##-------------------------------------
## coordExtract
##-------------------------------------
coordExtract <- function(shape){
    coords     <- list()
    polygons   <- shape@polygons
    for(i in 1:length(polygons)){
        coords[[i]]       <- polygons[[i]]@Polygons[[1]]@coords
        ## coords[[i]]       <- coords[[i]][,c(1,2)]
    }
    coords
}

resRelevant <- function(list){
    ldply(list, function(t){
    result    <- list()
    result[[1]] <-  t[[1]]                  ## Coordenadas de cada celda.
    result[[2]] <-  nrow(t[[2]])            ## Número de localidades por celda.
    result[[3]] <-  t[[3]]                  ## Población por celda.
    result[[4]] <-  t[[5]]                  ## Área por cada celda.
    result
    })
}

#######################################
## Read in data
#######################################
## Shapes Services per municipality
## Get all possible dirs
files <- list.dirs("../datos/shps") 
files <- files[!str_detect(files, "metadatos")]
files <- files[!str_detect(files, "catalogos")]
files <- files[-1]
municipalities <- list()
services <- list()
## -----------------------------
## Read in all shapes
## -----------------------------
for(i in 1:length(files)){
    state <- str_split(files[i], "/")[[1]][4]
    ## Read in services
    service_name <- paste0(state, "_servicios_puntual" )
    services[[i]] <- readOGR(files[i],
                            service_name)
    ## Read in municipalities
    mun_name <- paste0(state, "_municipio" )
    services[[i]] <- readOGR(files[i],
                            mun_name)
}

## Denue
denue <- readOGR("../datos/denue_shps/tabasco",
                "DENUE_INEGI_27_")
## Population
censo <- read.dbf("../datos/censo.dbf")
censo <- censo[!is.na(censo$LATITUD),]

## proc coords
coords <- censo[,c(7,8)]
coords[, 1] <- laply(coords[,1], function(t)t <- trans_coord(t))
coords[, 2] <- laply(coords[,2], function(t)t <- trans_coord(t,0))
censo[,c(7,8)] <- coords
write.csv(censo[,1:10], "./datos/censo_filter.csv", row.names = FALSE)

## Filter censo inside 
coords <- censo[, c(7,8)]
coordinates(coords) <- c("LONGITUD", "LATITUD")
proj4string(coords) <- proj4string(mun)
inside  <- !is.na(over(coords, as(mun, "SpatialPolygons")))
censo.int   <- censo[inside,]

#######################################
## Filter relevant data DENUE
#######################################
## Escuelas
escuelas <- denue[str_detect(denue$codigo_act, "^61.*" ),]
## Hospitales
hospitales <- denue[str_detect(denue$codigo_act, "^62.*" ),]
## Energía
energia <- denue[str_detect(denue$codigo_act, "^22.*" ),]
## Gasolina
gasolina <- denue[str_detect(denue$codigo_act, "468411" ),]




########################################
## Voronoi with Euclidean Distance
########################################
W  <- as(shp.mun, "owin")  
X  <- ppp(x = data.pop[data.pop$MUN == cve_mun, 7],
        y = data.pop[data.pop$MUN == cve_mun, 8],
         window = W)
voronoi_map1 <- dirichlet(X)
map1 <- as(voronoi_map1, "SpatialPolygons")


## To Shape Tess Voronoi
plot(map1)
SPDF <- SpatialPolygonsDataFrame(map1,
                                data=data.frame(x=data.pop[data.pop$MUN == cve_mun, 7],
                                                y=data.pop[data.pop$MUN == cve_mun, 8],
                                                row.names=row.names(map1)))
writeOGR(SPDF,
         "./datos_out",
         "villa_corzo",
        driver = "ESRI Shapefile")

########################################
## Get Connect data.pop
########################################
connect_pnts <- get_connect(
             data.pop[,8],
             data.pop[,7]
)

## Network parameters

data.connect <- llply(connect_pnts,
                     function(t) {if(length(t$networkRank[[1]]) == 3){
                                 t <- t$networkRank[[1]]$type3G
                                 }
                                 else
                                 t <- NA
                     }
                     )
rssiAsu <- c()
reliability <- c()
for(i in data.connect){
      if(!is.na(i)){
        if(class(i) == "list"){
        rssiAsu <- c(rssiAsu, i$averageRssiAsu)
        reliability <- c(reliability, i$reliability)
        }else{
        rssiAsu <- c(rssiAsu, i[4])
        reliability <- c(reliability, NA)
}       
      }
}

assu <- extract_numeric(rssiAsu)
writeLines(paste0(assu), "./datos_out/rssiAsu.txt")

relia <- extract_numeric(reliability[!is.na(reliability)])
########################################
########################################
## Villaflores
########################################
########################################

########################################
## Working OGR
########################################


poly <- readOGR("./datos_out/",
              "villa_flor_1")

coords <- mun.int[, c(7,8)]
coordinates(coords) <- c("LONGITUD", "LATITUD")
proj4string(coords) <- proj4string(poly)
inside  <- !is.na(over(coords, as(poly, "SpatialPolygons")))

##
plot(poly)
points(mun.int[inside,7:8])
##
pnts.in <- mun.int[inside,]
pnts.in <- dplyr::filter(pnts.in, POBTOT > 100)

########################################
## Get distances
########################################
all_dist <- distance_matrix(pnts.in[,7:8],
                           pnts.in[,7:8])

## Maximum of two
simm.dist <- matrix(NA, nrow = nrow(all_dist),
                   ncol = ncol(all_dist))
for(i in 1:nrow(all_dist)){
    for(j in 1:ncol(all_dist)){
        simm.dist[i,j] <- max(all_dist[i,j], all_dist[j,i])
    }    
}

########################################
## Data Prim
########################################
in.coords <- pnts.in[,7:8]
size <- nrow(pnts.in)
x <- c()
y <- c()
xend <- c()
yend <- c()
costs <- c()
## Origins
for(i in 1:size){
    x <- c(x, rep(in.coords[i,1], size))
    y <- c(y, rep(in.coords[i,2], size))
}
## Destinies
for(i in 1:size){
    xend <- c(xend, in.coords[,1])
    yend <- c(yend, in.coords[,2])
}
## Costs
for(i in 1:size){
    costs <- c(costs, simm.dist[i,])
}

G <- data.frame(x = x,
               y = y,
               xend = xend,
               yend = yend,
               p = costs
               )

tree <- prim(G)

## Plot tree
ggplot(data = tree,
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment(col = "purple")


ggplot(data = pnts.in,
              aes(x = LONGITUD,
                  y = LATITUD)) +
    geom_point() +
    geom_text(aes(label = NOM_LOC),  hjust=0, vjust=0 )



########################################
########################################
## Villa Corzo
########################################
########################################

########################################
## Working OGR
########################################


poly <- readOGR("./datos/",
               "villa_corzo_2")

coords <- mun.int[, c(7,8)]
coordinates(coords) <- c("LONGITUD", "LATITUD")
proj4string(coords) <- proj4string(poly)
inside  <- !is.na(over(coords, as(poly, "SpatialPolygons")))

##
plot(poly)
points(mun.int[inside,7:8])

##
pnts.in <- mun.int[inside,]
pnts.in <- dplyr::filter(pnts.in, POBTOT > 100)

########################################
## Get distances
########################################
all_dist <- distance_matrix(pnts.in[,7:8],
                           pnts.in[,7:8])

## Maximum of two
simm.dist <- matrix(NA, nrow = nrow(all_dist),
                   ncol = ncol(all_dist))
for(i in 1:nrow(all_dist)){
    for(j in 1:ncol(all_dist)){
        simm.dist[i,j] <- max(all_dist[i,j], all_dist[j,i])
    }    
}

########################################
## Data Prim
########################################
in.coords <- pnts.in[,7:8]
size <- nrow(pnts.in)
x <- c()
y <- c()
xend <- c()
yend <- c()
costs <- c()
## Origins
for(i in 1:size){
    x <- c(x, rep(in.coords[i,1], size))
    y <- c(y, rep(in.coords[i,2], size))
}
## Destinies
for(i in 1:size){
    xend <- c(xend, in.coords[,1])
    yend <- c(yend, in.coords[,2])
}
## Costs
for(i in 1:size){
    costs <- c(costs, simm.dist[i,])
}

G <- data.frame(x = x,
               y = y,
               xend = xend,
               yend = yend,
               p = costs
               )

tree <- prim(G)
## Plot tree
ggplot(data = tree,
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment(col = "purple")


ggplot(data = pnts.in,
              aes(x = LONGITUD,
                  y = LATITUD)) +
    geom_point() +
    geom_text(aes(label = NOM_LOC),  hjust=0, vjust=0 )





