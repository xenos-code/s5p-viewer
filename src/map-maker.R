library(openeo)
library(raster)
library(sf)
library(geojsonsf)
library(rjson)
library(ggplot2)
library(rnaturalearth)

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
date1 = "2020-07-01"
date2 = "2020-07-10"
## plot date
date = "2020-07-05"
## cloud cover value (>=)
value = 0.5

# extract bbox
country_sf = ne_countries(country = country, returnclass = "sf", scale = 'large')
w = st_bbox(country_sf)[1]
s = st_bbox(country_sf)[2]
e = st_bbox(country_sf)[3]
n = st_bbox(country_sf)[4]

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

## filter
datacube_map = p$filter_temporal(
                   data = datacube, extent = c(date, date)
                 )

print("Queuing computation... ")
print("it may take a while... ")
formats = list_file_formats()
result = p$save_result(data = datacube_map,
                         format = formats$output$GTiff)
job = create_job(graph=result, title = 'one-snapshot-raster')
start_job(job = job)
jobs = list_jobs()
while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){
    
    print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
    Sys.sleep(30)
    
    jobs = list_jobs()
    
    if (jobs[[job$id]]$status == 'finished' | jobs[[job$id]]$status == 'error' ){
      
      break
      
    }
  }
files = list.files(path = "data/", pattern="\\.tif$")
for (i in 1:length(files)){
    if (file.exists(paste0("data/", files[i]))) {
      file.remove(paste0("data/", files[i]))
  }  
}
  
print("downloading results")
download_results(job = job$id, folder = "data/")
  
file = list.files(path = "data/", pattern = "\\.tif$")
rst = mask(raster(paste0("data/", file)), country_sf)
print(rst)

#png(paste0('data/', country, date, '.png'))
par(mar = c(2,2,2,2))
plot(
  rst,
  col = hcl.colors(100, palette = "inferno", alpha = NULL, rev = FALSE, fixup = TRUE),
  main = paste0('Tropospheric NO2 Vertical Column (molec/cm²)','\n', country, ' (', date, ')'),
  axes = FALSE   
     )
plot(st_geometry(country_sf), add = T, type = 'l', lwd = 5)
#dev.off()
