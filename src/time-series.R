library(openeo)
library(raster)
library(sf)
library(geojsonsf)
library(rjson)
library(ggplot2)

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
## bounding box
w = 6.09
s = 46.15
e = 6.99
n = 46.57

# Talinn
w = 24.52
s = 59.35
e = 24.96
n = 59.50

# Sarajevo
w = 18.23
s = 43.81
e = 18.54
n = 43.98

# very small test
w = 6.09
s = 46.15
e = 6.11
n = 46.17

## time extent
date1 = "2018-07-01"
date2 = "2018-10-31"

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
###
# moving average
#process1 = function(data, context = NULL) {
 # reduce1 = p$reduce_dimension(data = data, dimension = "t", reducer = mean)
  #reduce1
  #}
#datacube = p$apply_neighborhood(data = datacube, size = list(list(
 #               "dimension" = "t", "value" = "P30D")), process = process1)
              
# compress spatial dimension
lon = c(w, e)
lat = c(s, n)

bbox_df = data.frame(lon, lat)

pol = st_polygon(
  list(
    cbind(
      bbox_df$lon[c(1,2,2,1,1)], 
      bbox_df$lat[c(1,1,2,2,1)])
  )
)
polygons = st_sfc(pol, crs=4326)
polygons = st_sf(polygons)

# add any attribute as a workaround
polygons$anAttribute <- 4

# aggregate spatially
datacube_mean <- p$aggregate_spatial(data = datacube, reducer = function(data, context) { p$mean(data) }, geometries = polygons)
datacube_max <- p$aggregate_spatial(data = datacube, reducer = function(data, context) { p$max(data) }, geometries = polygons)

# graph results
if ( (((e-w)+(n-s)) * 111) < 350 ){
  
  tryCatch({
    
  print("Trying synchronous process")
  graph = as(datacube_mean,"Graph")
  compute_result(graph = graph, output_file = "data/time-series-mean.json")
  print("mean time series stored \n")
  
  graph = as(datacube_max,"Graph")
  compute_result(graph = graph, output_file = "data/time-series-max.json")
  print("max time series stored \n")
  
# read json - add save option
  ts_mean = fromJSON(file = "time-series-mean.json")
  ts_max = fromJSON(file = "time-series-max.json")
  print("time series read")
  
  }, error = function(e) {
  
    message(e)
    print("Synchronous process failed : queuing computation... \n")
    print("it may take a while... \n")
    formats = list_file_formats()
    result = p$save_result(data = datacube_mean,
                           format = formats$output$JSON)
    job = create_job(graph=result, title = "time-series-mean")
    start_job(job = job)
    jobs = list_jobs()
    while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){
      
      print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
      Sys.sleep(60)
      
      jobs = list_jobs()
      
      if (jobs[[job$id]]$status == 'finished'){
        
        print("downloading results")
        
        download_results(job = job$id, folder = "data/")
      }
      
      if (jobs[[job$id]]$status == 'error') {
        
        print('error!')
        break
        
      }
    }
    
    ts_mean = fromJSON(file = "data/timeseries.json")
    print("mean time series read")

    result = p$save_result(data = datacube_max,
                       format = formats$output$JSON)
    job = create_job(graph=result, title = "time-series-max")
    start_job(job = job)
    jobs = list_jobs()
    while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){
  
      print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
      Sys.sleep(60)
  
      jobs = list_jobs()
      if (jobs[[job$id]]$status == 'finished'){
    
        download_results(job = job$id, folder = "data/")
    }
  
      if (jobs[[job$id]]$status == 'error') {
    
        print('error!')
        
        break
      }
    }
    
    ts_max = fromJSON(file = "data/timeseries.json")
    
}, warning = function(w) {
  
  message(e)
  print("Synchronous process failed : queuing computation... \n")
  print("it may take a while... \n")
  formats = list_file_formats()
  result = p$save_result(data = datacube_mean,
                         format = formats$output$JSON)
  job = create_job(graph=result, title = "time-series-mean")
  start_job(job = job)
  jobs = list_jobs()
  while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){
    
    print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
    Sys.sleep(60)
    
    jobs = list_jobs()
    
    if (jobs[[job$id]]$status == 'finished'){
      
      print("downloading results")
      
      download_results(job = job$id, folder = "data/")
    }
    
    if (jobs[[job$id]]$status == 'error') {
      
      print('error!')
      break
      
    }
  }
  
  ts_mean = fromJSON(file = "data/timeseries.json")
  print("mean time series read")
  
  result = p$save_result(data = datacube_max,
                         format = formats$output$JSON)
  job = create_job(graph=result, title = "time-series-max")
  start_job(job = job)
  jobs = list_jobs()
  while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){
    
    print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
    Sys.sleep(60)
    
    jobs = list_jobs()
    if (jobs[[job$id]]$status == 'finished'){
      
      download_results(job = job$id, folder = "data/")
    }
    
    if (jobs[[job$id]]$status == 'error') {
      
      print('error!')
      
      break
    }
  }
  
  ts_max = fromJSON(file = "data/timeseries.json")
  
})
  
}else{

  print("queuing computation... \n")
  print("it may take a while... \n")
  formats = list_file_formats()
  result = p$save_result(data = datacube_mean,
                         format = formats$output$JSON)
  job = create_job(graph=result, title = "time-series-mean")
  start_job(job = job)
  while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){

 	print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
 	Sys.sleep(60)
  
  	if (jobs[[job$id]]$status == 'finished'){

  	    print("downloading results")
    		download_results(job = job$id, folder = "data/")
  	    break
  	}

  	if (jobs[[job$id]]$status == 'finished') {

    		print('error!')
    		break
    
  	  }
  }
  
  ts_mean = fromJSON(file = "data/timeseries.json")
  print("mean time series read")
  print(ts_mean)
  
  result = p$save_result(data = datacube_max,
                         format = formats$output$JSON)
  job = create_job(graph=result, title = "time-series-max")
  start_job(job = job)
  jobs = list_jobs()
  while (jobs[[job$id]]$status == 'running' | jobs[[job$id]]$status == 'queued' | jobs[[job$id]]$status == 'created' ){

 	print(paste0('this may take a moment, your process is ', jobs[[job$id]]$status))
 	Sys.sleep(60)
  
 	jobs = list_jobs()
  if (jobs[[job$id]]$status == 'finished'){

    		download_results(job = job$id, folder = "data/")
  	}

  if (jobs[[job$id]]$status == 'error') {

    		print('error!')
    		break
    
  }}
  ts_max = fromJSON(file = "data/timeseries.json")
  print("max time series read")
  print(ts_max)
  }

# no2 mean
no2 = list(range(length(ts_mean)))
for (i in 1:length(ts_mean)){

  no2[i] = ts_mean[[i]]

}

# no2 max
no2_max = list(range(length(ts_max)))
for (i in 1:length(ts_max)){

  no2_max[i] = ts_max[[i]]

}

no2 = unlist(no2)
no2_max = unlist(no2_max)
time = seq(as.Date(date1), by = "days", length.out=length(no2))
no2_k = ksmooth(time(time),no2,'normal',bandwidth=3)
time = seq(as.Date(date1), as.Date(date2), length.out=length(no2_k$x))

scientific_notation <- function(l) {
                                       
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("e", "%*%10^", l)
  parse(text=l)
}

# plotting
if (e <= 12.55 & w >= 10.35 & n <= 47.13 & s >= 46.10 & 
    as.Date(date1) >= as.Date("2018-12-14") & 
    as.Date(date2) <= as.Date("2021-12-31")){
  
  library(readxl)
  library(dplyr)
  library(xts)
  library(treasuryTR)

  df = read_excel("rshiny_NO2_TM75_2017-2022.xlsx")
  df = df %>% slice(5:n()) %>% dplyr::select(2:ncol(df))
  
  df = as.data.frame(df)
  date = as.POSIXct(seq(0,3600*24*nrow(df)-1,by=24*3600), origin="2017-01-01")
  df$date = as.Date(date)
  xts_df = tibble_to_xts(df)
  storage.mode(xts_df) = "integer"
  
  # interpolate
  xts_df = na.approx(xts_df)
  xts_df = na.omit(xts_df)
  ##xts_df %>% head()
  
  # aggregate
  agg = cbind(xts_df, rowMeans(xts_df))
  
  # convert to comparable scale
  agg = agg * 10e-7
  #agg %>% head()
  
  # filter by date 
  filtered = agg[paste0(date1, "/", date2)]

  time = c(time,
           seq(as.Date(date1), as.Date(date2), length.out=length(no2)),
           seq(as.Date(date1), as.Date(date2), length.out=length(no2_max)),
           seq(as.Date(date1), as.Date(date2), length.out=length(local_dt)))
  
  # to data frame
  local_dt = as.vector(filtered$rowMeans.xts_df.)
  data = c(no2_k$y, no2, no2_max, local_dt)
  group = c(rep("smoothed", length(no2_k$y)),
            rep("raw", length(no2)),
            rep("maximum", length(no2_max)),
            rep("local", length(local_dt)))

} else{
  time = c(time,
           seq(as.Date(date1), as.Date(date2), length.out=length(no2)),
           seq(as.Date(date1), as.Date(date2), length.out=length(no2_max)))
  data = c(no2_k$y, no2, no2_max)
  group = c(rep("smoothed", length(no2_k$y)),
          rep("raw", length(no2)),
          rep("maximum", length(no2_max)))
}

df = data.frame(time, data, group)

#png('time-series-no2.png')
ggplot(df, aes(x=time, y=data, group = group, col = group, linetype = group)) +
  geom_line() +
  scale_y_continuous(labels=scientific_notation) +
  theme(legend.position="top") +
  theme(legend.title = element_blank())+
  xlab("Time") + ylab("Tropospheric NO2 Vertical Column (molec/cm²)")
#dev.off()
