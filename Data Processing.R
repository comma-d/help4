# DS Hackathon


wd = getwd()
newdir = paste(wd, "/Desktop/DS Hackathon", sep = "")
setwd(newdir)


dataset = read.csv("fulldata_v6.csv", header = TRUE, sep = ",")
# dataset = dataset[, c(1, 3)]
# dataset = dataset[-nrow(dataset), ]

inc = dataset$Incident.Type
inc = as.character(inc)


cleansing = function(str) {
  sep_pt = regexpr("-", str)[1]
  str = strsplit(str, split = "")
  splice_range = c(sep_pt: length(str[[1]]))
  str[[1]] = str[[1]][-splice_range]
  new_str = paste(str[[1]], collapse = "")
  
  return(new_str)
}

new_inc = c(1:length(inc))

for (i in 1:length(inc)) {
    new_inc[i] = cleansing(inc[i])
}

Incident.Type = as.factor(new_inc)

dataset$Incident.Type = Incident.Type

# write.csv(dataset, file = "fulldata_v5.csv")

# Categorized Creation Time

dataset$creationtime2 = as.character(dataset$creationtime2)

creationtime = as.POSIXlt(dataset$creationtime2, format="%m/%d/%y %H:%M")
Creation.Hour = creationtime$hour

assign_hour_range = function(hour) {
  if (hour >=0 & hour <= 5) {
    hour.range = 1
  } else if (hour >= 6 & hour <= 11) {
    hour.range = 2
  } else if (hour >= 12 & hour <= 17) {
    hour.range = 3
  } else if (hour >= 18 & hour <= 23) {
    hour.range = 4
  }
  
  return(hour.range)
}

  
Creation.Hour.Range = vector()

for (i in 1:length(Creation.Hour)) {
  Creation.Hour.Range[i] = assign_hour_range(Creation.Hour[i])
}

Creation.Hour.Range = as.factor(Creation.Hour.Range)


dataset$Creation.Hour = Creation.Hour
dataset$Creation.Hour.Range = Creation.Hour.Range

# Get Date
creation_time = strsplit(dataset$creationtime2, split = " ")
Creation.Date = vector()
for (i in 1: length(dataset$creationtime2)) {
  Creation.Date[i] = creation_time[[i]][1]
}
Creation.Date = as.Date(Creation.Date, format = "%m/%d/%y")


dataset$Creation.Date = Creation.Date

# weather data

nycweather = read.csv("nycweather.csv", header = FALSE, sep = ",")
nycweather = nycweather[-1, ]
names = c("Day", "JD", "Month", "State_id", "Year", "PRCP (in)", "TAVE (F)")

colnames(nycweather) = names
weather = nycweather[-1, ]

get_date_char = function(day, month, year) {
  day = as.character(day)
  month = as.character(month)
  year = as.character(year)
  
  combine = c(year, month, day)
  date_char = paste(combine, collapse = "-")
  #date = as.Date(date_char)
  
  #return(date)
}


Date = vector()
Day = vector()
Month = vector()
Year = vector()
Day = weather$Day
Month = weather$Month
Year = weather$Year

for (i in 1:length(Day)) {
  Date[i] = get_date_char(Day[i], Month[i], Year[i])
}

Date = as.Date(Date)
weather$Date = Date

weather_clean = weather[-c(1:5, 7)]
colnames(weather_clean) = c("Rain", "Date")

is_Rain = as.numeric(weather_clean$Rain)

for (i in 1:length(is_Rain)) {
  if (is_Rain[i] == 2) {
    is_Rain[i] = 0
  } else {
    is_Rain[i] = 1
  }
}

weather_clean$Rain = is_Rain

cleandata = read.csv("cleandata.csv", header = TRUE, sep = ",")
cleandata = cleandata[, -c(1,2)]

# Add 2015 Weather Data
weather15 = read.csv("weather15.csv", header = FALSE, sep = ",")
Date15 = as.Date(weather15$V2, format = "%m/%d/%y")
weather15$V2 = Date15
colnames(weather15) = c("Rain", "Date")

new_weather = rbind(weather_clean, weather15)


# Merge weather Data

Weather_Date_all = new_weather$Date
Creation_Date_all = Creation.Date
Is.Rain = vector()

for (i in 1:length(Creation_Date_all)) {
  for (j in 1:length(Weather_Date_all)) {
    if (Weather_Date_all[j] %in% Creation_Date_all[i]) {
      Is.Rain[i] = new_weather$Rain[j]
    } 
  }
}


cleandata$Rain = Is.Rain
write.csv(cleandata, file = "cleandata_v2.csv")

finaldata = read.csv("fulldata_v11.csv", header = TRUE, sep = ",")
finaldata$Rain = Is.Rain
write.csv(finaldata, file = "fulldata_v12.csv")
