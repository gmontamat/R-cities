# World cities data

LoadCities <- function() {
  # Downloads data from geonames and loads it into a dataframe
  if (!file.exists("cities1000.txt")) {
    download.file(
      url = "http://download.geonames.org/export/dump/cities1000.zip",
      destfile = "cities1000.zip", method = "libcurl"
    )
    unzip("cities1000.zip")
    file.remove("cities1000.zip")
  }
  cities <- read.csv("cities1000.txt", header = FALSE, sep = "\t", quote = "",
                     stringsAsFactors = FALSE)[, c(1, 2, 5, 6, 9, 15)]
  names(cities) <- c("id", "name", "latitude", "longitude", "country_code",
                     "population")
  return(cities)
}


PlotCityWorldMap <- function(cities, groups = 20) {
  # Plot "world map" of cities
  limits <- with(cities, seq(min(population), max(population)+1,
                             length.out = groups+1))
  x.range <- c(-180, 180)
  y.range <- c(-90, 90)
  plot(x = x.range, y = y.range, type = "n", xlim = x.range, ylim = y.range,
       xlab = "", ylab = "")
  colors <- rgb(colorRamp(c("green", "red"))(seq(0, 1, length.out = groups)),
                maxColorValue = 256)
  for (i in seq(groups)) {
    cities_group <- cities[cities$population>=limits[i] &
                             cities$population<limits[i+1],]
    points(cities_group$longitude, cities_group$latitude, pch = 20,
           cex = log(i+0.1), col = colors[i])
  }
}


SavePlot <- function(filename, plot_function, ...) {
  # Receives a filename and a function that generates a plot
  # and saves the plot as png wih the given name
  png(filename = filename, width = 861, height = 552)
  plot_function(...)
  dev.off()
}


cities <- LoadCities()
SavePlot("./images/world_cities.png", PlotCityWorldMap, cities)
