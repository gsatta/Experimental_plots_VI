# Load the necessary packages
library(sf)
library(terra)
library(progress)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)


# Set the files directory
directory_path <- "./ELABORATION/RASTER/IMAGES_CLIPPED"

# List the file inside the directory folder
image_files <- list.files(directory_path, pattern = ".tif", full.names = TRUE)

# 
# Imposta la nuova directory per gli indici vegetazionali
output_directory <- "./ELABORATION/RASTER/VI_soft"

# Crea la nuova directory se non esiste
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Crea un vettore di nomi degli indici
index_names <- c("EVI", "NDVI")

# Funzione per calcolare e salvare un indice vegetazionale specifico
calculate_and_save_index <- function(image_path, output_directory) {
  # Carica l'immagine
  raster_data <- rast(image_path)
  
  EVI <- 2.5 * (raster_data[[4]] - raster_data[[3]]) / 
    (raster_data[[4]] + 6 * raster_data[[3]] - 7.5 * raster_data[[1]] + 1 )
  
  NDVI <- (raster_data[[4]] - raster_data[[3]]) / (raster_data[[4]] + raster_data[[3]])
  
  # Ciclo attraverso gli indici e salva i risultati nella nuova directory
  for (index_name in index_names) {
    index_value <- eval(parse(text = index_name))
    
    output_path <- file.path(output_directory, paste0(index_name, "_", basename(image_path)))
    
    if (!file.exists(output_path)) {
      terra::writeRaster(index_value, output_path, overwrite = TRUE)
      progress_bar$tick()
    } else {
      cat("Il file", output_path, "esiste già. Passa al ciclo successivo.\n")
      next  # Salta al ciclo successivo
      
      progress_bar$tick()
    }
  }
}

# Inizializza la barra di avanzamento
progress_bar <- progress::progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = length(image_files) * length(index_names)
)

# Ciclo attraverso ogni immagine e calcola/salva gli indici
for (image_file in image_files) {
  calculate_and_save_index(image_file, output_directory)
}

# Close the progress bar
progress_bar$terminate()

#####################################################

library(progress)
library(terra)
library(sf)
library(readr)

# Carica il file CSV con le date
date_info <- read_csv("./ELABORATION/CSV/date.csv")

date <- date_info$date

# Carica il file delle chiome NDVI
crowns <- st_read("./ELABORATION/SHP/crowns_class.shp")

# Trova i nomi unici delle aree
unique_aree <- unique(crowns$AREA)

# Definisci le variabili index_vegetations e areas
index_vegetations <- c("EVI", "NDVI")

areas <- unique(crowns$AREA)

# Imposta la directory degli indici vegetazionali
vi_directory <- "./ELABORATION/RASTER/VI_soft"

# Nuova directory per i raster multilayer
output_directory <- "./ELABORATION/RASTER/VI_MERGED_soft"

directory_path <- getwd()

# Inizializza la barra di progresso
pb <- progress_bar$new(
  format = "[:bar] :percent Elaborazione in corso...",
  total = length(index_vegetations) * length(areas)
)

# Ciclo attraverso gli indici vegetazionali
for (index_vegetation in index_vegetations) {
  # Ciclo attraverso le aree
  for (area in areas) {
    # Trova i file corrispondenti esattamente all'area e all'indice vegetazionale
    files <- list.files(vi_directory)
    
    # Crea un pattern regex dinamico
    pattern <- sprintf("%s_%s_(\\d{8})_MERGED.tif$", index_vegetation, area)
    
    # Filtra i file che corrispondono al pattern
    selected_files <- grep(pattern, files, value = TRUE)
    
    # Ottieni i percorsi completi dei file a partire dalla directory attuale
    file_paths <- file.path(getwd(), vi_directory, selected_files)
    
    # Crea un raster multilayer solo con i file corrispondenti
    multilayer <- terra::rast(file_paths)
    
    # Rinomina i layer con l'indice vegetazionale, l'area e la data
    names(multilayer) <- paste0(index_vegetation, "_", area, "_", date)
    
    # Salva il raster multilayer nella nuova directory solo se non esiste già
    output_filename <- file.path(output_directory, paste0(index_vegetation, "_", area, "_MULTILAYER.tif"))
    if (!file.exists(output_filename)) {
      writeRaster(multilayer, filename = output_filename, overwrite = TRUE)
    }
    
    # Aggiorna la barra di progresso
    pb$tick()
  }
}



#######
# Carica il file delle chiome NDVI
crowns <- st_read("./ELABORATION/SHP/crowns_class.shp")

VI_directory <- "./ELABORATION/RASTER/VI_MERGED_soft"

library(sf)
library(terra)
library(progress)
library(dplyr)

# Funzione per estrarre il nome dell'area da un file SHP
get_area_name_shp <- function(file_path) {
  # Estrai il nome dell'area tra l'ultimo slash e il punto (ignorando l'estensione .shp)
  area_name_shp <- sub(".*/(.*)\\.shp$", "\\1", file_path)
  return(area_name_shp)
}

# Funzione per estrarre il nome dell'area da un file raster
get_area_name_raster <- function(file_path) {
  # Rimuovi tutto fino al primo underscore incluso
  temp <- gsub("^[^_]*_", "", basename(file_path))
  # Rimuovi "_MULTILAYER" e tutto ciò che segue
  area_name_rast <- gsub("_MULTILAYER.*$", "", temp)
  return(area_name_rast)
}

# Set the path to the shapefile and raster file
shapefile_folder  <- "./ELABORATION/SHP/crowns_areas"
raster_folder  <- "./ELABORATION/RASTER/VI_MERGED_soft"
output_folder <- "./ELABORATION/RASTER/VI_EXTRACTED_soft"

# List files in the folders
raster_files <- list.files(raster_folder, pattern = ".tif$", full.names = TRUE)
shapefile_files <- list.files(shapefile_folder, pattern = ".shp$", full.names = TRUE)

# Estrai i nomi delle aree dai nomi dei file shapefile e raster
shapefile_area_names <- sapply(shapefile_files, get_area_name_shp)
raster_area_names <- sapply(raster_files, get_area_name_raster)

# Inizializza la barra di progresso
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent)",
  total = length(raster_files)
)

# Iterate through raster files
for (raster_index in 1:length(raster_files)) {
  # Iterate through shapefiles for each raster
  for (shapefile_index in 1:length(shapefile_files)) {
    # Estrai il nome dell'area dai file shapefile e raster
    shapefile_area_name <- shapefile_area_names[shapefile_index]
    raster_area_name <- raster_area_names[raster_index]
    
    # Verifica se il nome dell'area è lo stesso nei file shapefile e raster
    if (shapefile_area_name == raster_area_name) {
      shp0 <- vect(shapefile_files[shapefile_index])
      rast <- terra::rast(raster_files[raster_index])
      
      # Get the name of one raster layer
      filename <- names(rast[[1]])
      
      # Get the vegetation index name from the filename
      VI <- sub("^(.*?)_.*", "\\1", filename)
      
      # Extract raster values
      ex <- terra::extract(rast, shp0, method = "simple", exact = TRUE, xy = TRUE, ID = FALSE)
      
      # Convert shp from spatvector to sf object
      shp <- st_as_sf(shp0, crs = 32632)
      
      # If you want to convert the area to another unit, you can use the st_transform function
      shp$estensione <- st_area(st_transform(shp, crs = 32632), square = TRUE) # Change new_crs to the desired coordinate system
      
      # Filter polygons with at least 2/3 area coverage
      ex_filtered <- ex[ex$fraction >= (2/3),]
      
      # Create an sf object from the filtered data
      ex_sf <- st_as_sf(ex_filtered, coords = c("x", "y"))
      
      # Assign WGS 84 CRS to your sf object
      ex_sf <- st_set_crs(ex_sf, 32632)
      
      # Remove the fraction column (no longer needed now)
      ex_sf$fraction <- NULL
      
      # Remove duplicate rows based on all columns
      ex_sf2 <- distinct(ex_sf)
      
      # Assign the CRS of ex_sf to polygons
      polygons <- st_as_sf(shp, st_crs(ex_sf2))
      
      # Perform spatial join based on the position of ex_sf and polygons
      sf_join <- st_join(ex_sf2, polygons)
      
      # Calculate square side length (3 meters)
      side_length <- 3
      
      # Create squares using st_buffer
      quadrat_sf <- st_buffer(sf_join, side_length / 2, endCapStyle = "SQUARE")
      
      # Set CRS (EPSG:32632)
      quadrat_sf <- st_set_crs(quadrat_sf, 32632)
      
      # Elimina la colonna estensione
      quadrat_sf$estensione <- NULL 
      
      # Rinomina manualmente i campi problematici
      field_names <- names(quadrat_sf)[1:68]
      new_field_names <- gsub(".*_", "", field_names)  # Estrae solo l'ultima parte del nome dopo l'underscore
      
      # # Rimuovi i trattini dalle date
      new_field_names <- gsub("-", "", new_field_names)
      
      names(quadrat_sf)[1:68] <- new_field_names
      
      # Generate output filename based on the shapefile name
      area_name <- tools::file_path_sans_ext(basename(shapefile_files[shapefile_index]))
      output_filename <- file.path(output_folder, paste0(area_name, "_", VI, ".gpkg"))
      
      # Scrivi il file shapefile
      st_write(quadrat_sf, output_filename, driver = "GPKG", append = FALSE)
      
      # Aggiorna la barra di progresso
      pb$tick()
    } else {
      # Issue a warning with the problematic file path
      warning("File does not exist or path is NA at raster index:", raster_index, ", shapefile index:", shapefile_index)
      if (is.na(shapefile_files[shapefile_index])) {
        warning("Problematic shapefile path: NA")
      } else if (!file.exists(shapefile_files[shapefile_index])) {
        warning("Problematic shapefile path:", shapefile_files[shapefile_index])
      }
      if (!file.exists(raster_files[raster_index])) {
        warning("Problematic raster path:", raster_files[raster_index])
      }
    }
  }
}

# Chiudi la barra di progresso alla fine del loop
pb$close()

##############


# Set the input folder of the extracted ndvi files
INPUT_folder <- "./ELABORATION/RASTER/VI_EXTRACTED_soft"

# Get the list of SHP files in the folder
INPUT_shp <- list.files(path = INPUT_folder, pattern = "\\.gpkg$", full.names = TRUE)

# Create a progress bar with the total number of shp files
pb <- progress_bar$new(total = length(INPUT_shp), 
                       format = "[:bar] :current/:total (:percent)")

# Load all the shp files in a list of sf objects
shp_list <- lapply(INPUT_shp, function(file) {
  pb$tick()  # Update the progress bar for each iteration
  
  # Extract the raster file name from the full path
  raster_file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the shapefile and assign the raster file name as an attribute
  shp <- st_read(file)
  shp$raster_file_name <- raster_file_name
  
  return(shp)
})

# Save the list
saveRDS(shp_list, "./ELABORATION/LIST/shp_soft.rds")

# Load the list
shp_list2 <- readRDS("./ELABORATION/LIST/shp_soft.rds")

# Create a progress bar with a total number of elements in shp_list
pb <- progress_bar$new(total = length(shp_list2), format = "[:bar] :percent :eta")

# Loop for each element of the list
for (i in seq_along(shp_list2)) {
  # Get the numeric columns names that start whith "X"
  numeric_colnames <- grep("^X\\d+", names(shp_list2[[i]]), value = TRUE)
  
  # Remove the prefix "X" from each numeric column
  new_colnames <- gsub("^X", "", numeric_colnames)
  
  # Upload the columns names of the sf data frame
  names(shp_list2[[i]])[names(shp_list2[[i]]) %in% numeric_colnames] <- new_colnames
  
  # Upload the progress bar
  pb$tick()
}

# Save the list
saveRDS(shp_list2, "./ELABORATION/LIST/shp2_soft.rds" )

#########

library(dplyr)
library(tidyr)

shp_list3 <- readRDS("./ELABORATION/LIST/shp2_soft.rds")

# # Rimuovi le  colonne non necessarie
cols_to_exclude <- c("ID", "raster_file_name", "TREAT")
# 
# Inizializza la barra di avanzamento
pb <- progress_bar$new(total = length(shp_list3), 
                       format = "[:bar] :current/:total (:percent)")

# Define a function to extract the label from raster_file_name
extract_label <- function(raster_file_name) {
  parts <- strsplit(raster_file_name, "_")[[1]]
  if(length(parts) > 1) {
    return(parts[length(parts)])
  } else {
    return(NA)
  }
}

# Define the function with pb as an argument
process_shapefile <- function(shp, pb) {
  label <- as.character(extract_label(shp$raster_file_name[1]))
  
  result <- shp %>%
    dplyr::select(-one_of(cols_to_exclude)) %>%
    pivot_longer(
      cols = -c(geom, COD, AREA),
      names_to = "date",
      values_to = label
    ) %>%
    mutate(date = as.Date(date, format = "%Y%m%d"))
  
  pb$tick()
  
  return(result)
}

# Use lapply with your function
long_format_list <- lapply(shp_list3, function(shp) {
  process_shapefile(shp, pb)
})

# Save the long format list
saveRDS(long_format_list, "./ELABORATION/LIST/long_format_SOFT.rds" )


#############

long_format_list <- readRDS("./ELABORATION/LIST/long_format_SOFT.rds")


# Definisci una funzione per elaborare ciascun elemento della lista
process_element <- function(element) {
  # Aggiungi una nuova colonna "VI" con i valori dalla quinta colonna
  element$VI <- colnames(element)[5]
  
  # Rinomina la quinta colonna con "VALUE"
  element <- element %>% rename(VALUE = colnames(element)[5])
  
  return(element)
}

# Applica la funzione a ciascun elemento della lista
long_format_list_2 <- lapply(long_format_list, process_element)


# Save the long format list
saveRDS(long_format_list_2, "./ELABORATION/LIST/long_format2_soft.rds")

#############

long_format_list_2 <- readRDS("./ELABORATION/LIST/long_format2_soft.rds")

# Conversione della lista sf in un unico data frame
long_format_df <- bind_rows(long_format_list_2)

# Calcolo della media del VI per ogni COD, data e VI
vi_aggregated <- long_format_df %>%
  group_by(COD, date, VI) %>%
  summarise(VALUE = mean(VALUE))

write.csv(vi_aggregated, "./ELABORATION/CSV/vi_aggregated_soft.csv" )


#####

# Set the scipen option to hight value to eliminate the esponential notation of the values
options(scipen = 999)

vi_aggregated <- read_csv("./ELABORATION/CSV/vi_aggregated_soft.csv")

# elimina i valori NA
vi_aggregated <- na.omit(vi_aggregated)

# Esempio con tapply
min_max_values <- tapply(vi_aggregated$VALUE, vi_aggregated$VI, function(x) c(min(x), max(x)))

# Converti i risultati in un dataframe
min_max_df <- data.frame(VI = names(min_max_values), Min = sapply(min_max_values, "[[", 1), Max = sapply(min_max_values, "[[", 2))

# Visualizza il dataframe con i valori minimi e massimi per ogni VI
print(min_max_df)

library(ggplot2)

# Crea il box plot
ggplot(vi_aggregated, aes(x = VI, y = VALUE, fill = VI)) +
  geom_boxplot() +
  labs(title = "Box Plot dei Valori per Categoria VI",
       x = "Categoria VI",
       y = "Valore")

# Filtra i dati di agosto 2023
vi_aggregated_august <- vi_aggregated %>%
  filter(format(date, "%Y-%m") == "2023-08")

df_class <- read_csv("ELABORATION/CSV/symptom_assessment.csv")

# Unisci i due dataframe in base alla colonna COD
vi_aggregated_with_classes <- merge(vi_aggregated_august, df_class, by = "COD")

# Aggiungi la colonna CLASS a vi_aggregated_august
vi_aggregated_august <- mutate(vi_aggregated_august, CLASS = vi_aggregated_with_classes$CLASS)


write.csv(vi_aggregated_august, "ELABORATION/CSV/vi_aggregated_august.csv" )

###################################################################
##################################################################
# Creazione del modello RF

vi_aggregated_august <- read_csv("ELABORATION/CSV/vi_aggregated_august.csv")

# elimina i valori NA
vi_aggregated_august <- na.omit(vi_aggregated_august)


library(tidyr)

# Assume che il tuo dataframe sia chiamato 'vi_aggregated_august'
vi_aggregated_august <- vi_aggregated_august %>%
  spread(key = VI, value = VALUE)

library(dplyr)

# Assume che il tuo dataframe sia chiamato 'vi_aggregated_august'
vi_aggregated_august <- vi_aggregated_august %>%
  group_by(COD) %>%
  summarise_all(list(~ifelse(all(is.na(.)), NA, first(na.omit(.)))))

vi_aggregated_august$...1 <- NULL

vi_aggregated_august$...2 <- NULL

vi_aggregated_august$geom <- NULL

vi_aggregated_august$date <- NULL

library(caret)
library(randomForest)

vi_aggregated_august$CLASS <- factor(vi_aggregated_august$CLASS)

set.seed(123)  # Imposta il seed per rendere i risultati riproducibili

# Crea un vettore di indici casuali
train <- sample(nrow(vi_aggregated_august), (nrow(vi_aggregated_august) * 0.8))

# Seleziona le righe corrispondenti agli indici casuali
train_data <- vi_aggregated_august[train, ]

# Conta il numero di elementi per ogni classe nella colonna "CLASS"
class_counts <- table(train_data$CLASS)

# Stampa i risultati
print(class_counts)

# Calcola gli indici per il test (complemento di train)
test <- setdiff(1:nrow(vi_aggregated_august), train)

# Seleziona le righe corrispondenti agli indici del test
test_data <- vi_aggregated_august[test, ]


rf_fit <- randomForest(CLASS ~ EVI + NDVI, 
                       data = train_data,
                       importance=TRUE,
                       proximity=TRUE)

## Look at variable importance:
round(importance(rf_fit), 2)


# "http://www.r-project.it/_book/random-forest-rf-1.html"

print(rf_fit)

plot(rf_fit)


varImpPlot(rf_fit)

#############
## Do MDS on 1 - proximity:
vi.mds <- cmdscale(1 - rf_fit$proximity, eig=TRUE)
op <- par(pty="s")

pairs(cbind(vi_aggregated_august[,3:4], rf_fit$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(vi_aggregated_august$CLASS)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")


#######
MDSplot(rf_fit, vi_aggregated_august$CLASS)



# Effettua le previsioni sul set di test
predictions <- predict(rf_fit, test_data)

# Valuta le prestazioni del modello (ad esempio, usando la matrice di confusione)
conf_matrix <- table(predictions, test_data$CLASS)
print(conf_matrix)

# Calcola l'accuratezza del modello
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))


######

# Sostituisci con l'algoritmo appropriato per il tuo problema
model <- train(CLASS ~ EVI + NDVI, data = train_data, method = "rf")

nuovi_dati <- select(tuo_dataframe_con_i_nuovi_dati, VALUE, altre_colonne_di_interesse)
predictions <- predict(model, newdata = data_for_model)

# Valutazione del modello di classificazione
conf_matrix <- confusionMatrix(predictions, data_for_model$CLASS)

print(conf_matrix)














