if(demo_server){
  ######################## DEMO car ###########################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "car")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  

  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  
  ################# DEMO BIKE #####################
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "bike")

  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############## DEMO FOOT #################"""""
  options(osrm.server = "https://routing.openstreetmap.de/", 
          osrm.profile = "foot")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
    wait()
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))

  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  wait()
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  
  ############# server param ##################
  C <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving")
  wait()
  expect_equal(dim(A$durations), c(10,10))
  
  ############# server error #####################"
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                 measure = c("wrong_measure"), 
                 osrm.server = "http://router.project-osrm.org/", 
                 osrm.profile = "driving"))
  wait()
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         osrm.server = "https://rosuter.psroject-ossrm.org/", 
                         osrm.profile = "driving"))
  wait()

}


if(local_server){
  ############## ONLY LOCAL ############################################
  options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "car")
  A <- osrmTable(src = x_df[1:10,c("lon","lat")],
                 dst = x_df[1:10,c("lon","lat")], 
                 measure = c("distance", "duration"))
  B <- osrmTable(loc = x_df[1:10, c("lon","lat")], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances, B$distances)
  expect_equal(dim(A$distances), c(10,10))
  expect_equal(A$durations, B$durations)
  expect_equal(dim(A$distances), c(10,10))
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"))
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  ######### exclude
  A <- osrmTable(src = x_sf[1:10, ],
                 dst = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 exclude = "motorway")
  B <- osrmTable(loc = x_sf[1:10, ], 
                 measure = c("distance", "duration"), 
                 exclude = "motorway")
  expect_equal(A$distances,B$distances)
  expect_equal(A$durations,B$durations)
  expect_equal(dim(A$durations), c(10,10))
  expect_equal(dim(A$distances), c(10,10))
  ##### server error
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         measure = c("wrong_measure")))
  expect_error(osrmTable(loc = x_sf[1:10, ], 
                         osrm.server = "http://0.0.0.0:5100/", 
                         osrm.profile = "car"))
  
  # snapped_distance
  A <- osrmTable(loc = x_sf[1:5, ], measure = "snapped_distance")
  expect_true(!is.null(A$snapped_distances))
  expect_true(is.null(A$distances))
  expect_equal(dim(A$snapped_distances), c(5,5))
  expect_equal(diag(A$snapped_distances), rep(0, 5))
  
  B <- osrmTable(loc = x_sf[1:5, ], measure = c("distance", "snapped_distance"))
  expect_true(!is.null(B$distances))
  expect_true(!is.null(B$snapped_distances))
  expect_equal(B$snapped_distances[1,2], 
               B$distances[1,2] + B$sources$snapping_distance[1] + B$destinations$snapping_distance[2])
}