

pem_create_maps <- function(sample_points, out_name, centroid_distance = 400){

  ##testing line
  sample_points = ptCoords
  
  sample_points_clhs <- st_as_sf(sample_points) %>% 
    st_transform(3005)
  rotation_angles <- seq(0, 315, 45) # Rotation degrees 
  
  sample_points_rotations <- st_sf(st_sfc()) %>% st_set_crs(3005)
  
  for(i in 1:nrow(sample_points_clhs)){
    #i = 1
    pnt <- sample_points_clhs[i,]
    pGeom <- st_geometry(pnt)
    pGeom <- pGeom + c(0, centroid_distance)
    pnt_feat <- st_set_geometry(pnt, pGeom)
    
    rotated_points <-  st_sf(st_sfc()) %>% st_set_crs(3005)
    
    rotated_points <- foreach(Bear = rotation_angles, .combine = rbind) %do%{
      #Bear = rotation_angles[5]
      Feature_geo <- st_geometry(pnt_feat)
      PivotPoint  <- st_geometry(pnt)
      ## Convert bearing from degrees to radians
      d <- ifelse(Bear > 180, pi * ((Bear -360)/ 180) ,  pi * (Bear / 180))
      rFeature <- (Feature_geo - PivotPoint) * rot(d)   + PivotPoint
      rFeature <- st_set_crs(rFeature, st_crs(pnt_feat))
      pnt_feat$geometry <- st_geometry(rFeature) ## replace the original geometry
      pnt_feat$Rotation <- Bear
      pnt_feat <- pnt_feat %>% st_set_crs(3005)
    }
    
    sample_points_rotations <- rbind(rotated_points, sample_points_rotations)  
  }
  
  sample_points_rotations <- st_as_sf(sample_points_rotations, crs = 3005) %>%
    #  mutate(rotation = mapvalues(Rotation, rotation_angles, c("N", "NE", "E", "SE", "S", "SW", #"W", "NW"))) %>%
    mutate(rotation = mapvalues(Rotation, rotation_angles, c("N", "NE", "SE", "W", "E", "NW", "SW", "S"))) %>%
    filter(!is.na(Rotation)) %>%
    dplyr::select(-Rotation)
  
  
  sample_points_rotations
  
  sample_points_clhs$rotation <- "cLHS"
  
  all_points <- rbind(sample_points_clhs, sample_points_rotations)  %>%
    mutate(ID = paste(ID, rotation, sep = "_"))# This is all possible paired samples
  
  all_triangles <- st_sf(st_sfc()) %>% st_set_crs(3005)
  
  for(i in 1:nrow(all_points)){
    poc <- all_points[i, ]
    
    triangle <- Tri_build(id = poc$ID, x =  st_coordinates(poc)[1], y =  st_coordinates(poc)[2])
    random_rotation <- runif(1, min = 0, max = 360)
    triangle <- rotFeature(triangle, poc, random_rotation)
    
    all_triangles <- rbind(all_triangles, triangle)
  }
  
  #out_name <- "DateCreek_AOI/2_sample_design/stage1_StudyDesign/transect_layout/replacement_site/replacement_sites.gpkg"
  st_write(all_points, paste0(out_name,".gpkg"), layer = "Points", delete_layer = TRUE)
  st_write(all_triangles, paste0(out_name,".gpkg"), layer = "Triangles", delete_layer = TRUE)
  triangle_buff <- st_buffer(all_triangles, dist = 10)
  st_write(triangle_buff, paste0(out_name,".gpkg"), layer = "Buffer", delete_layer = TRUE)
  return(TRUE)
}
