rm(list=ls())

############### JSON to CSV conversion  ############### 

# Package loading
library(jsonlite)
library(tidyverse)
library(dplyr)

# Read the JSON file
json_data <- fromJSON("Norsat3-N1-JSON-Message-All-2024-04-24T010155Z.json")

# Convert the JSON into a dataframe
data_df <- as.data.frame(json_data)

# Expand the nested columns
data_expanded <- data_df %>%
  unnest_wider(CandidateList) %>%
  unnest_wider(NRDEmitterPosition) %>%
  unnest_wider(CollectionInformation)

# Select the desired columns
data_subset <- data_expanded %>%
  select(longitude = Longitude,
         latitude = Latitude,
         vesselid = CandidateNo,
         time = CollectionTime,
         confidence = CandidateConfidenceCurrent)

# Day and time format
data_subset <- data_subset %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         time = format(time, format = "%d/%m/%Y %H:%M:%S"))

data_subset$time <- as.character(data_subset$time)     # To also display the seconds, save the time column as a character

# Saving data in a CSV file
write.csv(data_subset, "data_complete.csv", row.names = FALSE)


# Keep data with confidence greater than 60 and also eliminate NAs
dati_filtrati <- data_subset[!is.na(data_subset$confidence) & data_subset$confidence > 60, ]

dati_filtrati$time <- as.character(dati_filtrati$time)
write.csv(dati_filtrati, "data_filter.csv", row.names = FALSE)


################## TIME DIFFERENCE #################
rm(list=ls())

library(dplyr)

dataVessel <- read.csv("data_filter.csv", header = T)

# Convert the time column to POSIXct objects
dataVessel$time <- as.POSIXct(dataVessel$time, format="%d/%m/%Y %H:%M:%S")

# Sort the dataframe by vesselid and time
dataVessel <- dataVessel %>% arrange(vesselid, time)


###### MINUTES #######

# Initialize a time difference column
#dataVessel$time_diff_min <- NA

# Calculate the time difference between successive points for each vesselid
#for (vessel in unique(dataVessel$vesselid)) {
# indices <- which(dataVessel$vesselid == vessel)
#  if (length(indices) > 1) {
#    dataVessel$time_diff_min[indices] <- c(NA, diff(dataVessel$time[indices]))
#  }
#}

# write.csv(dataVessel, "time_difference_min.csv",  row.names = FALSE)



####### SECONDS ########

# Initialize a column for time difference in seconds
dataVessel$time_diff_sec <- NA

# Calculate the time difference between successive points for each vesselid
for (vessel in unique(dataVessel$vesselid)) {
  indices <- which(dataVessel$vesselid == vessel)
  if (length(indices) > 1) {
    dataVessel$time_diff_sec[indices] <- c(NA, as.numeric(difftime(dataVessel$time[indices[-1]], dataVessel$time[indices[-length(indices)]], units = "secs")))
  }
}


dataVessel$time <- as.character(dataVessel$time)

write.csv(dataVessel, "time_difference_sec.csv",  row.names = FALSE)



############ DISTANCE ############
rm(list=ls())

library(dplyr)

dataVessel <- read.csv("time_difference_sec.csv", header = T)

# Initialize a column for the distance from one point to the next
dataVessel$distance <- NA

# Euclidean distance function / Pythagorean Theorem
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# For loop on each vessel ID
for (vessel in unique(dataVessel$vesselid)) {
  indices <- which(dataVessel$vesselid == vessel)
  if (length(indices) > 1) {
    for (i in 2:length(indices)) {
      prev_index <- indices[i - 1]
      curr_index <- indices[i]
      dataVessel$distance[curr_index] <- euclidean_distance(dataVessel$longitude[prev_index], dataVessel$latitude[prev_index], dataVessel$longitude[curr_index], dataVessel$latitude[curr_index])
    }
  }
}

dataVessel$time <- as.character(dataVessel$time)

write.csv(dataVessel, "time_and_distance.csv",  row.names = FALSE)


########## SPEED #########
rm(list=ls())

library(dplyr)

dataVessel <- read.csv("time_and_distance.csv", header = T)

# Initialize a column for speed
dataVessel$speed <- NA

# For loop on each vessel ID to calculate velocity
for (vessel in unique(dataVessel$vesselid)) {
  indices <- which(dataVessel$vesselid == vessel)
  if (length(indices) > 1) {
    for (i in 2:length(indices)) {
      curr_index <- indices[i]
      # Calcola la velocità come distanza diviso tempo in ore (poiché abbiamo il tempo in secondi)
      dataVessel$speed[curr_index] <- dataVessel$distance[curr_index] / dataVessel$time_diff_sec[curr_index]
    }
  }
}

dataVessel$time <- as.character(dataVessel$time)

write.csv(dataVessel, "speed.csv",  row.names = FALSE)



########### Nautical miles/hours ##############
rm(list=ls())

library(dplyr)

dataVessel <- read.csv("speed.csv", header = T)

# Initialize the columns to fill
dataVessel$nautical_miles <- NA
dataVessel$hours <- NA
dataVessel$knots <- NA

# Convert the values
dataVessel$nautical_miles <- dataVessel$distance * 60    # degrees in nautical miles
dataVessel$hours <- dataVessel$time_diff_sec / 3600      # seconds into hours
dataVessel$knots <- ((dataVessel$nautical_miles)/(dataVessel$hours))    # speed in knots

dataVessel$time <- as.character(dataVessel$time)
write.csv(dataVessel, "final_table.csv",  row.names = FALSE)


########### MEAN DISTANCE ##############
rm(list=ls())

library(dplyr)

dataVessel <- read.csv("final_table.csv", header = T)

# Function to calculate the Euclidean distance between two points
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

dataVessel$mean_distance <- NA

# Use a for loop to calculate the average distance for each point
for (i in 1:nrow(dataVessel)) {
  distances <- numeric()
  for (j in 1:nrow(dataVessel)) {
    if (i != j) {
      dist <- euclidean_distance(dataVessel$longitude[i], dataVessel$latitude[i], dataVessel$longitude[j], dataVessel$latitude[j])
      distances <- c(distances, dist)
    }
  }
  dataVessel$mean_distance[i] <- mean(distances)
}


######## MINIMUM DISTANCE #########

# same csv (dataVessel) done above, not saved yet

# Initialize the column for minimum distance
dataVessel$min_distance <- NA


# Use a for loop to calculate the minimum distance to each point
# calculate the Euclidean distance between the current point (indicated by i) and all other points (indicated by j)
for (i in 1:nrow(dataVessel)) {
  distances <- numeric()          # vector where to temporarily save all distances
  for (j in 1:nrow(dataVessel)) {
    if (i != j) {           # avoids calculating the distance with itself
      dist <- euclidean_distance(dataVessel$longitude[i], dataVessel$latitude[i], dataVessel$longitude[j], dataVessel$latitude[j])
      distances <- c(distances, dist)
    }
  }
  dataVessel$min_distance[i] <- min(distances)
}


# Save csv
dataVessel$time <- as.character(dataVessel$time)
write.csv(dataVessel, "vessel_with_distance.csv",  row.names = FALSE)



###########  Calculate distances vectorically  ############

# sostituire una longitudine con una intera colonna di longitudine
# distanza euclidea: distingui i passaggi
# una differenza tra il punto e la colonna ^2   ---> x1 rimane come singolo punto, x2 diventa una colonna intera
# altra differenza con y1 e y2 vedi riga su
# la somma
# la radice


# ovvero:
# elimina il for interno e sostituiscilo con il calcolo della distanza tra un punto e un vettore
# è più veloce quando i dati sono infiniti

##### mean distance ####
library(dplyr)

dataVessel <- read.csv("vessel_with_distance.csv", header = TRUE)

# Function to calculate the Euclidean distance between a point and a matrix of points
vectorized_euclidean_distance <- function(x1, y1, x2_vec, y2_vec) {
  sqrt((x1 - x2_vec)^2 + (y1 - y2_vec)^2)
}

# Initialize the column for average distance
dataVessel$vect_mean_distance <- NA

# Use a for loop to calculate the mean distance for each point
for (i in 1:nrow(dataVessel)) {
  
  # Calculate distances vectorically
  distances <- vectorized_euclidean_distance(
    dataVessel$longitude[i], 
    dataVessel$latitude[i], 
    dataVessel$longitude, 
    dataVessel$latitude
  )
  
  # Exclude the distance of the point with itself (zero distance)
  distances <- distances[-i]
  
  # Calculate the mean distance
  dataVessel$vect_mean_distance[i] <- mean(distances)
}

##### min distance #####

dataVessel$vect_min_distance <- NA

for (i in 1:nrow(dataVessel)) {
  # Calculate distances vectorically
  distances <- vectorized_euclidean_distance(
    dataVessel$longitude[i], 
    dataVessel$latitude[i], 
    dataVessel$longitude, 
    dataVessel$latitude
  )
  
  # Exclude the distance of the point with itself (zero distance)
  distances <- distances[-i]
  
  # Calculate the min distance
  dataVessel$vect_min_distance[i] <- min(distances)
}

# Save csv
dataVessel$time <- as.character(dataVessel$time)
write.csv(dataVessel, "vessel_with_distance_vect.csv",  row.names = FALSE)


##### euclidean distance step by step  ##### 

dataVessel$min_distance_2 <- NA

# Use a for loop to calculate the minimum distance for each point
for (i in 1:nrow(dataVessel)) {
  # Calculate the distances vectorially step by step
  x1 <- dataVessel$longitude[i]
  y1 <- dataVessel$latitude[i]
  
  # Subtract the coordinates
  diff_x <- x1 - dataVessel$longitude
  diff_y <- y1 - dataVessel$latitude
  
  # Calculate the square of the differences
  squared_diff_x <- diff_x^2
  squared_diff_y <- diff_y^2
  
  # Sum of the squares of the differences
  sum_squared_diff <- squared_diff_x + squared_diff_y
  
  # Calculate the Euclidean distance
  distances <- sqrt(sum_squared_diff)
  
  # Exclude the distance of the point from itself (zero distance)
  distances <- distances[-i]
  
  # Calculate the minimum distance
  dataVessel$min_distance_2[i] <- min(distances)
}
