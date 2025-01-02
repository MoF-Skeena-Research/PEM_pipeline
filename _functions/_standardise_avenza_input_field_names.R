# function to comvert geopackage to shapefile and rename column 
# gen perkins 
# Oct 10 2021


gpkg_to_shp <- function(infile, newnames){
  
  
  infile <- "D:/PEM_DATA/BEC_DevExchange_Work/WilliamsLake_AOI/2_sample_design/stage1_StudyDesign/transect_data/archive/site_to_add.gpkg"
  
  #infile <- "D:/PEM_DATA/BEC_DevExchange_Work/BoundaryTSA_AOI/2_sample_design/stage1_StudyDesign/transect_data/2021_ecora/site_to_add.gpkg"
  
  
  outfile <- gsub(".gpkg", "_edit.gpkg", infile)
  
  layers <- st_layers(infile)
  
  points <- layers$name[1]
  
  points_shp <- st_read(infile, layer = points) %>%
    st_transform(3005) %>%
    st_zm() %>%
    rename("name" = Name, 
           "desc" = descriptio,
           "Photos"= pdfmaps_ph,
           "01_trans" = F01_transec, 
           "02_observe" = F02_observe,
           "03_pt_type" = F03_pt_type,
           "04_mapunit" = F04_mapunit,
           "05_transit" = F05_transit,
           "06_mapunit" = F06_mapunit,
           "07_struc_" = F07_struct_,
           "08_struct" = F08_struct_,
           "09_comment" = F09_comment,
           "10_edatope" = F10_edatope#, 
           #"geometry" = Shape)
    ) 
           
  st_write(points_shp, file.path(outfile), delete_layer = TRUE)
  

}