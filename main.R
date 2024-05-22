library(FITSio)
library(formattable)
setwd("../data")
myevent <- dir()[2]
data <- readFITS(file = myevent)

standart_deviation <- function(x){
  sqrt(sum((x-mean(x))^2)/(length(x)-1))
}

loglam <- formattable(matrix(unlist(data$col[which(data$colNames == "loglam")])), format = "f")
flux <- formattable(matrix(unlist(data$col[which(data$colNames == "model")])), format = "f")
wavelengths <- 10^loglam 

plot(wavelengths, flux, type = "l", xlab = "Wavelength (Angstrom)", ylab = "Flux",
     main = "Spectrum")

decreasing_sequence <- FALSE
increasing_sequence <- FALSE
sequence_start_index <- NULL
absorption_line_indices <- c()
dif <- NULL
wave <- NULL
#wave <- rep(0,length(flux - 1))
for (i in 1:length(flux)-1) {
 dif[i] <- flux[i+1] - flux[i]
 wave[i] <- (wavelengths[i] + wavelengths[i+1])/2
}
plot(wavelengths, flux, type = "l", xlab = "Wavelength (Angstrom)", ylab = "Flux",
     main = "Spectrum")
treshold <- mean(dif)+standart_deviation(dif)

# Absorbsyon tanimlama algoritmasi
for (i in 2:(length(flux))) {
  # Flux bir oncekindek kucuk mu?
  if (flux[i] < flux[i-1]) {
    # Halihazirda decrasing sequence degilse baslat
    if (!decreasing_sequence && !increasing_sequence) {
      decreasing_sequence <- TRUE
      sequence_start_index <- i - 1
    }
    increasing_sequence <- FALSE
    # Flux bir oncekinden buyuk mu? artisin devam edip etmedigini gormek icin bir sonraki degeri de kontrol ediyor
  } else if (flux[i] > flux[i-1] && flux[i+1] > flux[i]) {
    # decreasing_sequence true ise durdur
    if (decreasing_sequence) {
      # dip noktasi atamasi
      min_index <- i-1
      decreasing_sequence <- FALSE
      increasing_sequence <- TRUE
      
      # baslangis noktasi ile dip noktasi arasindaki flux farki belirlenen tresholddan buyuk ve sifirdan buyuk mu?
      if(flux[sequence_start_index] - flux[min_index] > treshold && flux[sequence_start_index] - flux[min_index] > 0){
        # indis ekleniyor
        absorption_line_indices <- c(absorption_line_indices, min_index )
      }
      
    }
   
  } else {
    # reset
    decreasing_sequence <- FALSE
    increasing_sequence <- FALSE
  }
}

points(wavelengths[absorption_line_indices], flux[absorption_line_indices], col = "red", pch = 16,cex=0.5)

