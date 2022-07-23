library(openeo)
library(raster)
library(sf)
library(geojsonsf)
library(rjson)
library(ggplot2)
library(rnaturalearth)
library(gifski)
library(magick)

con = connect(host = "https://openeo.cloud")

# Collections
collections = list_collections()
#print(collections$SENTINEL_5P_L2)

#describe_collection("SENTINEL_5P_L2") # Band 3
#describe_collection("SENTINEL_5P_L2") # Band 3

# Processes
processes = list_processes()

#print(names(processes))
#print(processes$mask)

# login
login()

# User defined process
p = processes()

# user inputs
country = 'switzerland'
## time extent
date1 = "2018-01-01"
date2 = "2018-01-31"
## cloud cover value (>=)
value = 0.5
##delay for spacetime animation
delay = 0.3

# extract bbox
country_sf = ne_countries(country = country, returnclass = "sf", scale = 'large')
w = st_bbox(country_sf)[1]
s = st_bbox(country_sf)[2]
e = st_bbox(country_sf)[3]
n = st_bbox(country_sf)[4]

## time extent
date1 = "2018-06-01"
date2 = "2018-06-31"

## cloud cover value (>=)
value = 0.5

# acquire data for the extent
datacube_no2 = p$load_collection(
               id = "SENTINEL_5P_L2",
               spatial_extent = list(west = w, south = s, east = e, north = n),
               temporal_extent=c(date1, date2),
               bands=c("NO2")
             )

datacube = p$load_collection(
               id = "SENTINEL_5P_L2",
               spatial_extent = list(west = w, south = s, east = e, north = n),
               temporal_extent=c(date1, date2),
               bands=c("CLOUD_FRACTION")
             )

# 10km x 10km grid : may be optional 
datacube = p$resample_spatial(
                 data = datacube, resolution = 10/111
               )

datacube_no2 = p$resample_spatial(
               data = datacube_no2, resolution = 10/111
               )

# mask for cloud cover
threshold_ <- function(data, context) {

  threshold <- p$gte(data[1], value)
  return(threshold)
}

# apply the threshold to the cube
cloud_threshold <- p$apply(data = datacube, process = threshold_)

# mask the cloud cover with the calculated mask
datacube <- p$mask(datacube_no2, cloud_threshold)

# interpolate where nodata
interpolate = function(data,context) {
  return(p$array_interpolate_linear(data = data))
}

datacube = p$apply_dimension(process = interpolate,
               data = datacube, dimension = "t"
               )

#graph = as(datacube,"Graph")
#compute_result(graph = graph, output_file = "test.tif")

print("queuing computation... \n")
print("it may take a while... \n")
formats = list_file_formats()
result = p$save_result(data = datacube,
                       format = formats$output$GeoTiff)
job = create_job(graph=result, title = "time-animation-tif")
start_job(job = job)
jobs = list_jobs()
while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){

  print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
  Sys.sleep(60)
  
  jobs = list_jobs()
  if (jobs[[job$id]]$status == 'finished' | jobs[[job$id]]$status == 'error'){

    break
  }
}
 
files = list.files(path = "data/")
for (i in 1:length(files)){
  if (file.exists(paste0("data/", files[i]))) {
    file.remove(paste0("data/", files[i]))
  }  
}

print("downloading results")
download_results(job = job$id, folder = "data/")

pal <- hcl.colors(11, palette = "Red-Yellow", alpha = NULL, rev = TRUE, fixup = TRUE)
files = list.files('data/')
files = paste0('data/', files)
dates = sub('data/openEO_', '', files)
dates = sub('Z.tif', '', dates)
filepath = tempdir()
min = c()
max = c()
for (i in 1:length(files)){
  min[i] = hist(raster(files[i]))$breaks[1]
  max[i] = hist(raster(files[i]))$breaks[length(hist(raster(files[i]))$breaks)]
}
min_v = min(min)
max_v = max(max)
for (i in 1:length(files)){

  if (length(files) < 10){

    png(paste0(filepath, '/', as.character(0), as.character(i),'.png'), width = 800)
    plot(mask(raster(files[i]), country_sf), col = pal,
         axes = FALSE, 
         breaks = seq(min_v, max_v, 0.1*max_v),
         main = dates[i])
    plot(st_geometry(country_sf), add = T, type = 'l', lwd = 5)
    dev.off()

 } else{
   
   png(paste0(filepath, '/', as.character(i),'.png'), width = 800)
   plot(mask(raster(files[i]), country_sf), col = pal,
         axes = FALSE,
         breaks = seq(min_v, max_v, 0.1*max_v),
         main = dates[i])
   plot(st_geometry(country_sf), add = T, type = 'l', lwd = 5)
   dev.off()

    }
  }

png_files = list.files(filepath)
png_files = paste0(filepath, '/', png_files)
gifski(png_files = png_files, gif_file = 'spacetime-animation.gif',
       delay = delay,
       progress = T)

image_read("spacetime-animation.gif")
#showGIF <- function(fn) system(paste("display",fn))
#showGIF("spacetime-animation.gif")
#unlink("spacetime-animation.gif") ## clean up
