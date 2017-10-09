\dontrun{ 
  # Package without the data directories
  path <- file.path(tempdir(), "myDefaultPackage")
  create_pkg(path)
  unlink(path, recursive = TRUE)
   
  # Package with data directories 
  create_pkg(path, use_data_raw = TRUE) 
}

# View all the template files
system.file("templates", package = "qwraps2")
