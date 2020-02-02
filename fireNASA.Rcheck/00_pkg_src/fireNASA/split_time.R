
#This works
df<- read.csv("cneos_fireball_data.csv")
dates <- as.Date(df$Peak.Brightness.Date.Time..UT., "%Y-%m-%d")
dates


times <- format(as.POSIXct(strptime(df$Peak.Brightness.Date.Time..UT.,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")
times










