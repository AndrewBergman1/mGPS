library(caret)
library(geosphere)
library(dplyr)
library(xgboost)
library(e1071)
library(doParallel)
library(leaflet)
library(tibble)
library(smotefamily)

data_normalise <- function(df) {
  return(df/rowSums(df))
}

data_preprocess_f <- function(train_f, target_in, hierarchy, remove_small) {
  metasub_data <- droplevels(train_f)
  
  if(length(hierarchy) == 4){
    metasub_data[, hierarchy[1]] <- factor(metasub_data[, hierarchy[1]])
    metasub_data[, hierarchy[2]] <- factor(metasub_data[, hierarchy[2]])
  } else {
    metasub_data[, hierarchy[1]] <- factor(metasub_data[, hierarchy[1]])
  }
  
  remove_small <- as.numeric(remove_small)
  if (remove_small > 0){
    small_cities <- names(which(summary(metasub_data[, target_in]) < remove_small))
    remove_samples <- which(metasub_data[, target_in] %in% c("antarctica", small_cities))
    if (length(remove_samples) != 0){
      metasub_data <- droplevels(metasub_data[-c(remove_samples), ])
    }
  }
  
  for (i in 1:length(hierarchy)){
    empty <- which(metasub_data[, hierarchy[i]] == "" | is.na(metasub_data[, hierarchy[i]]))
    if (length(empty) != 0){
      metasub_data <- metasub_data[-c(empty),]
    }
  }
  
  metasub_data[, hierarchy[1]] <- droplevels(metasub_data[, hierarchy[1]])
  if(length(hierarchy) == 4){
    metasub_data[, hierarchy[2]] <- droplevels(metasub_data[, hierarchy[2]])
  }
  
  print("metadata has been pre-processed ...")
  return(metasub_data)
}

species_select <- function(x, y, remove_correlated = TRUE, cores = 1, maxRuns = 100, confidence = 0.01) {
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  if (remove_correlated) {
    cor_matrix <- cor(x, use = "complete.obs")
    if(any(is.na(cor_matrix))) {
      warning("NA values found in correlation matrix. Check for constant variables.")
      cor_matrix[is.na(cor_matrix)] <- 0
    }
    
    correlated <- findCorrelation(cor_matrix, cutoff = 0.98, verbose = TRUE, names = FALSE)
    
    print("Correlated features indices:")
    print(correlated)
    
    if (length(correlated) > 0) {
      x <- x[, -correlated, drop = FALSE]
      print(paste("Correlated features removed: ", length(correlated)))
    } else {
      print("No correlated features to remove.")
    }
  }
  
  set.seed(123)
  boruta_output <- Boruta(x, y, maxRuns = maxRuns, doTrace = 2, ntree = 50)
  
  print(boruta_output)
  plot(boruta_output, las = 2)
  
  final_features <- getSelectedAttributes(boruta_output, withTentative = FALSE)
  print(paste("Selected features: ", toString(final_features)))
  
  stopCluster(cl)
  registerDoParallel(1)
  
  return(list(results = boruta_output, selectedFeatures = final_features))
}

mGPS <- function(training = training_data,
                 testing = testing_data,
                 classTarget = "city",
                 hierarchy = c('continent', 'city', 'Latitude', 'Longitude'),
                 variables,
                 nthread = 1,
                 coast = NULL) {
  if (is.null(training)){
    return(message("No training set given"))
  } else {
    training <- droplevels(training)
    message("Training mGPS...")
    
    set.seed(1234)
    folds <- createFolds(training[,classTarget], k = 5, returnTrain = T)
    
    trControlClass <-  trainControl(
      method = "cv",
      number = 5,
      verboseIter = FALSE,
      returnData = FALSE,
      search = "grid",
      savePredictions = "final",
      classProbs = T,
      allowParallel = T,
      index = folds
    )
    
    trControl <-  trainControl(
      method = "cv",
      number = 5,
      verboseIter = FALSE,
      returnData = FALSE,
      search = "grid",
      savePredictions = "final",
      allowParallel = T,
      index = folds
    )
    
    tune_grid <- expand.grid(
      nrounds = c(300, 600),
      eta = c(0.05, 0.1),
      max_depth = c(3, 6, 9),
      gamma = 0,
      colsample_bytree = c(0.6, 0.8),
      min_child_weight = c(1),
      subsample = (0.7)
    )
    
    if (length(hierarchy) == 4) {
      Xgb_region <- train(x = training[, variables], y = training[, hierarchy[1]],
                          method = "xgbTree",
                          trControl = trControlClass,
                          tuneGrid = tune_grid,
                          nthread = nthread)
      
      l1_train <- data.frame(training[, variables], Xgb_region[["pred"]][order(Xgb_region$pred$rowIndex), levels(training[, hierarchy[1]])])
    } else {
      l1_train <- training[, variables]
    }
    
    Xgb_class <- train(x = l1_train, y = training[, classTarget],
                       method = "xgbTree",
                       trControl = trControlClass,
                       tuneGrid = tune_grid,
                       nthread = nthread)
    
    l2_train <- data.frame(l1_train, Xgb_class[["pred"]][order(Xgb_class$pred$rowIndex), levels(training[, classTarget])])
    
    Xgb_latitude <- train(x = l2_train, y = training[,"Latitude"],
                          method = "xgbTree",
                          trControl = trControl,
                          tuneGrid = tune_grid,
                          nthread = nthread)
    
    l3_train <- data.frame(l2_train, "latPred" = Xgb_latitude[["pred"]][order(Xgb_latitude$pred$rowIndex), "pred"])
    
    Xgb_longitude <- train(x = l3_train, y = training[,"Longitude"],
                           method = "xgbTree",
                           trControl = trControl,
                           tuneGrid = tune_grid,
                           nthread = nthread)
  }
  
  if (is.null(testing)){
    model <- function(test, variables) {
      regProbs <- predict(Xgb_region, newdata = test[, variables], type ="prob")
      l1_test <- data.frame(test[, variables], regProbs)
      classPred <- predict(Xgb_class, newdata = l1_test)
      classProbs <- predict(Xgb_class, newdata = l1_test, type ="prob")
      l2_test <- data.frame(l1_test, classProbs)
      latPred <- predict(Xgb_latitude, newdata = l2_test)
      l3_test <- data.frame(l2_test, latPred)
      longPred <- predict(Xgb_longitude, newdata = l3_test)
      return(list(classPred, latPred, longPred))
    }
    message("No test set...returning trained mGPS model function")
    return(list(Xgb_region, Xgb_class, Xgb_latitude, Xgb_longitude, "model" = model))
  } else {
    message("Generating predictions")
    if (length(hierarchy) == 4) {
      regProbs <- predict(Xgb_region, newdata = testing[, variables], type ="prob")
      l1_test <- data.frame(testing[, variables], regProbs)
    } else {
      l1_test <- testing[, variables]
    }
    classPred <- predict(Xgb_class, newdata = l1_test)
    classProbs <- predict(Xgb_class, newdata = l1_test, type ="prob")
    l2_test <- data.frame(l1_test, classProbs)
    latPred <- predict(Xgb_latitude, newdata = l2_test)
    l3_test <- data.frame(l2_test, latPred)
    longPred <- predict(Xgb_longitude, newdata = l3_test)
    
    longPred[longPred > 180] <- 180
    longPred[longPred < -180] <- -180
    latPred[latPred > 90] <- 90
    latPred[latPred < -90] <- -90
    
    if (!is.null(coast)) {
      toAdjust <- which(is.na(maps::map.where(database = "world", longPred, latPred)))
      adjusted <- mapply(find_coast, long = longPred[toAdjust], lat = latPred[toAdjust])
      longPred[toAdjust] <- adjusted[1,]
      latPred[toAdjust] <- adjusted[2,]
    }
    
    return(list(classPred, latPred, longPred))
  }
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Earth radius in kilometers
  R <- 6371
  
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  
  return(distance)  # Distance in kilometers
}


# Function to calculate distance between corresponding pairs of coordinates
calculate_distances <- function(latitudes1, longitudes1, latitudes2, longitudes2) {
  distances <- haversine_distance(latitudes1, longitudes1, latitudes2, longitudes2)
  return(distances)
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Earth radius in kilometers
  R <- 6371
  
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  
  return(distance)  # Distance in kilometers
}

# Function to calculate distance between corresponding pairs of coordinates
calculate_distances <- function(latitudes1, longitudes1, latitudes2, longitudes2) {
  distances <- haversine_distance(latitudes1, longitudes1, latitudes2, longitudes2)
  return(distances)
}


soil_abundance <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/Dataset_01_22_2018_taxa.csv")
soil_meta <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/Dataset_01_22_2018_enviro.csv")

meta_abundance <- merge(soil_meta, soil_abundance, on = "ID_Environmental")

meta_abundance[28:ncol(meta_abundance)] <- data_normalise(meta_abundance[28:ncol(meta_abundance)])
meta_abundance <- data_preprocess_f(meta_abundance, "Continent", c("Continent", "Latitude", "Longitude"), 8)

trainIndex <- createDataPartition(meta_abundance$Continent, p = 0.8, list = FALSE)
trainData <- meta_abundance[trainIndex, ]
testData <- meta_abundance[-trainIndex, ]
testData[28:ncol(testData)] <- data_normalise(testData[28:ncol(testData)])
trainData[28:ncol(trainData)] <- data_normalise(trainData[28:ncol(trainData)])



features <- trainData[, 28:ncol(trainData)]
target <- trainData$Continent

smote_result <- SMOTE(
  X = features,
  target = target,
  K = 5,
  dup_size = 200 / 100
)

smote_data <- smote_result$data
smote_data$Continent <- smote_result$class
smote_data$Continent <- smote_data$class

trainData <- bind_rows(trainData, smote_data)
trainData <- trainData[, colnames(trainData)]



trainData$Continent <- as.factor(trainData$Continent)
testData$Continent <- as.factor(testData$Continent)
trainData[is.na(trainData)] <- 0
testData[is.na(testData)] <- 0

top_features <- scan("soil_gits.txt", what = "", sep = ",")
top_features <- unlist(top_features)
top_features <- trimws(top_features)

#features <- species_select(trainData[, 29:ncol(trainData) - 1], trainData$Continent, maxRuns = 10000, cores = 4)
#top_features <- unlist(features[2])

preds <- mGPS(trainData, testData, classTarget = "Continent", hierarchy = c("Continent", "Latitude", "Longitude"), variables = top_features, nthread=8)


map <- leaflet() %>%
  addTiles() %>%  
  addCircleMarkers(lng = preds[[3]], lat = preds[[2]], radius = 2, popup = "Predicted Coordinates") %>% 
  addCircleMarkers(lng = testData$Longitude, lat = testData$Latitude, radius = 0.5, color = colors, popup = "Test Data Coordinates") 

map

num_matches <- sum(preds[[1]] == testData$Continent)
distances <- calculate_distances(testData$Latitude, testData$Longitude, preds[[2]], preds[[3]])

# Calculate statistics
mean_distance <- mean(distances)
median_distance <- median(distances)
min_distance <- min(distances)
max_distance <- max(distances)
num_distances_less_than_100 <- length(which(distances < 100))
num_distances_less_than_250 <- length(which(distances < 250))
num_distances_less_than_500 <- length(which(distances < 500))
num_distances_less_than_1000 <- length(which(distances < 1000))
num_distances_less_than_1500 <- length(which(distances < 1500))
total_distances <- length(distances)

# Print statistics
cat("Statistic\t\tValue\n")
cat("Mean distance\t\t", mean_distance, "\n")
cat("Median distance\t", median_distance, "\n")
cat("Minimum distance\t", min_distance, "\n")
cat("Maximum distance\t", max_distance, "\n")
cat("Distances < 100\t\t", num_distances_less_than_100, "\n")
cat("Distances < 250\t\t", num_distances_less_than_250, "\n")
cat("Distances < 500\t\t", num_distances_less_than_500, "\n")
cat("Distances < 1000\t", num_distances_less_than_1000, "\n")
cat("Distances < 1500\t", num_distances_less_than_1500, "\n")
cat("Total distances\t\t", total_distances, "\n")
cat("classif. acc.\t\t", num_matches/length(preds[[1]]), "\n")
