# Read in table of US census data from 2010.
cd <- read.csv("census_data_SGV_cities.csv")
str(cd)

# Make data frames of just the numeric data.
df <- cd[, 4:23]
df <- scale(df, center = T, scale = T)

# Pull out the coordinates as vectors for easy handling.
x <- cd$Longitude
y <- cd$Latitude

# Load required libraries.
library(gstat)
library(lattice)
library(raster)
library(sp)
library(rgeos)

# Assign z value variable.
z <- cd$NAME_OF_COLUMN_YOU_WANT_TO_MAP #! Plug the column name on this line.

# Calculate some relevant polygons:

# Calculate convex hull.
chull_x <- x[ chull(x, y) ]
chull_y <- y[ chull(x, y) ]
c1 <- cbind( chull_x, chull_y ) 
r1 <- rbind(c1, c1[1, ])
convex_hull <- Polygon(r1)
p1 <- SpatialPolygons( list( Polygons( list(convex_hull), "p1" ) ) )

# Bounding box. Note: 10 degress lat/long should be plenty for nearly any map.
c2 <- cbind( c(min(x) - 10, max(x) + 10, max(x) + 10, min(x) - 10), 
	c(min(y) - 10, min(y) - 10, max(y) + 10, max(y) + 10) 
)

# Calculate the interpolation.
r2 <- rbind(c2, c2[1, ])
bound_box <- Polygon(r2)
p2 <- SpatialPolygons( list( Polygons( list(bound_box), "p2" ) ) )

# White space.
white_space <- gDifference(p2, p1)

# Plot interpolation map:

# Create spatial data.frame.
geog2 <- data.frame(x, y, z)

# Assigning coordinates results in spdataframe.
coordinates(geog2) = ~x + y

# Create grid
pixels <- 2000
geog.grd <- expand.grid(x=seq(floor(min(coordinates(geog2)[,1])),
                              ceiling(max(coordinates(geog2)[,1])),
                              length.out=pixels),
                        y=seq(floor(min(coordinates(geog2)[,2])),
                              ceiling(max(coordinates(geog2)[,2])),
                              length.out=pixels))
grd.pts <- SpatialPixels( SpatialPoints( (geog.grd) ) )
grd <- as(grd.pts, "SpatialGrid")

# IDW interpolation.
geog2.idw <- idw(z ~ 1, geog2, grd, idp = 6)

spplot(geog2.idw["var1.pred"])

# Setup info for plot.
x_lim <- c(-118.2, -117.7); y_lim <- c(34, 34.2) #! Adjust if using different coordinates.
asp <- ( max(y_lim) - min(y_lim) ) / ( max(x_lim) - min(x_lim) )
width <- 8
height <- asp * width
png(file = "your_map_file.png",
	unit = "in", width = width, height = height, res = 400,
	type = "cairo")
# Adjust graphical parameters.
par(mar = c(1, 1, 1, 1), bg = "black")

# Make a color palette.
pal <- plasma( length( unique( geog2.idw@data$var1.pred ) ) )

# Plot interpolation as an image.
image(geog2.idw, xlim = c(-118.05, -117.8), ylim = c(min(y), 34.18),
	col = pal)

# Add contour lines.
contour(geog2.idw, add = T, lwd = 0.5)

# Apply white space.
plot(white_space, add = T, col = "black")

# Label variable used.
text(-117.85, 34.16, "Percent black/African-American",
	col = "white", cex = 1.2, pos = 4, xpd = T)

# Cities points.
pal2 <- plasma( length( unique( z ) ) )
colz <- pal2[ rank( z ) ]
par(fg = "black")
points(x, y, pch = 21, bg = colz)

# Cities labels.
text(Longitude, Latitude, as.character( cd$City ), xpd = T,
	pos = 3, cex = 0.8, offset = 0.4, col = "white") 

dev.off()