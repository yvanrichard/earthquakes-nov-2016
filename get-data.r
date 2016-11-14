library(data.table)
library(rgdal)
library(raster)
library(lubridate)
nz <- readOGR('/dragonfly/gis/shapes/custom', 'bigislands')

## Download data from GeoNet
enddate <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S')
download.file(sprintf('http://quakesearch.geonet.org.nz/csv?startdate=2016-11-13T11:00:00&enddate=%s',
                               enddate), 'data/earthquakes.csv')

allquakes <- suppressWarnings(fread('data/earthquakes.csv'))
allquakes[longitude < 0, longitude := longitude + 360]
allquakes[, datetime := as.POSIXct(origintime, tz = 'UTC')]
allquakes[, datetime := with_tz(parse_date_time(origintime, 'ymd HMS', tz = 'UTC'), 'NZ')]

cat(sprintf('\nDownloaded %i quakes\n', nrow(allquakes)))

allquakes.sp <- copy(allquakes)
coordinates(allquakes.sp) <- ~ longitude + latitude
proj4string(allquakes.sp) <- proj4string(nz)


nz <- crop(nz, extent(c(166.5, 179, -47, -34.5)))
allquakes.sp <- crop(allquakes.sp, extent(c(166.5, 179, -47, -34.5)))
allquakes <- allquakes[publicid %in% allquakes.sp$publicid]

nz <- spTransform(nz, CRS('+init=epsg:3994'))
allquakes.spt <- spTransform(allquakes.sp, CRS('+init=epsg:3994'))

nzf <- data.table(fortify(nz))
si <- nzf[id == 0][group == group[1]]
ni <- nzf[id == 1][group == group[1]]

save(allquakes, allquakes.sp, allquakes.spt, si, ni, file='data/data.rdata')
