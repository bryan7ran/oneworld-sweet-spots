library(tidyverse)
library(readr)
library(geosphere)


data <- read_delim("routes.dat", "\t", 
                     escape_double = FALSE, trim_ws = TRUE) # import routes from Airlines Route Mapper
data <- data %>%
  select(1:5) # Take some of the info

airlines <- read_delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat", ",") # Read publicly available airlines data
airlines <- airlines %>%
  select(2,4) # Select only the IATA code and the airline name

oneworld_carriers = c('CP','MQ','YV','PT','OH','YX','OO','AA', # American Airlines and American Eagle (+Envoy)
                      'BA','CJ','MN','EZ', # British Airways, Comair, and affiliates
                      'CX','KA', # Cathay Pacific and Cathay Dragon
                      'AY','N7', # Finnair and Nordic Regional
                      'IB','I2','YW', # Iberia and regional
                      'JL','NU', # JAL and Japan Transocean
                      'MH', # Malaysia
                      'QF', # Qantas
                      'QR', # Qatar
                      'RJ', # Royal Jordanian
                      'S7', 'GH', # S7 and Globus
                      'UL', # SriLankan 
                      'AT', 'FN',  # Royal Air Maroc and Royal Air Maroc Express (future oneworld member)
                      'FJ', # Fiji Airways (oneworld connect partner)
                      'EI', # Aer Lingus
                      'AS', # Alaska Airlines
                      'BE') # Flybe
                      
######### DEBUG #####
# AAA = as.data.frame((table(data$`# airline`)))
# test = subset(AAA,AAA$Var1 %in% oneworld_carriers) # does the number of routes per airline even look right? holy
# rm(AAA)
#####################

# filter route data by Avios redeemable routes only
data_filtered = subset(data, data$`# airline` %in% oneworld_carriers) # check that sum(test$Freq) matches nrows data_filtered
# implement the reverse routes as well (such as CX DFW-HKG)
reverse = data_filtered
# rename the column to do the reversal
reverse = reverse %>%
  rename(to=from,from=to)
# then re-order the columns lmao
reverse = reverse[,c(1,3,2,4,5)]
# join them
data_filtered = union(data_filtered,reverse)
# SAVE IT AND INCREMENT IT

date = Sys.Date()
filename = paste('routes_',date,'.dat',sep="")
write_csv(data_filtered,filename)

# download publicly available airport dataset
airports <- read_delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", ",", col_names = FALSE)
# get IATA code, longitude, and latitude
airports <- airports %>%
  select('X5','X7','X8')

# join my routes dataset and the latitude and longitude
data_filtered = left_join(data_filtered, airports, by = c('from' = 'X5'))
# rename the coordinates appropriately
data_filtered = data_filtered %>%
  rename(latfrom = X7,longfrom = X8)
data_filtered = left_join(data_filtered, airports, by = c('to' = 'X5'))
data_filtered = data_filtered %>%
  rename(latto = X7,longto = X8)

# define a function to get the great circle distance between the FROM and the TO airport
f = function(x, output){
  p1 = as.numeric(c(x[7],x[6]))
  p2 = as.numeric(c(x[9],x[8]))
  distHaversine(p1,p2)
}

# run the function en masse and convert metres to miles
data_filtered$distance = apply(data_filtered,1,f) * 0.000621371

# define BA award bands (specify the lower limit of each band)
bands_BA = c(0,651,1152,2001,3001,4001,5501,6501,7001,24000)
bands_BA_generous = bands_BA + 25

bands_BA_compute = function(x,output){
  distance = as.numeric(x[10])
  findInterval(distance,bands_BA)
}
bands_BA_generous_compute = function(x,output){
  distance = as.numeric(x[10])
  findInterval(distance,bands_BA_generous)
}

# run the functions to calculate the BA bands
data_filtered$Band_BA = apply(data_filtered,1,bands_BA_compute)
data_filtered$Band_BA_gen = apply(data_filtered,1,bands_BA_generous_compute)

# define a function to determine sweetness
percent_calc = function(x,output){
  band_no = as.numeric(x[11])
  lower_miles = bands_BA[band_no]
  upper_miles = bands_BA[band_no + 1]

  threshold = upper_miles - lower_miles
  sweetness = as.numeric(x[10]) - lower_miles
  pct = round(sweetness/threshold, digits = 3)*100
  print(pct)
}

data_filtered$BA_pct = apply(data_filtered,1,percent_calc)
data_filtered$BA_pct[data_filtered$BA_pct < 1] = 100


write_csv(data_filtered,paste('tableau',date,'.csv'))
