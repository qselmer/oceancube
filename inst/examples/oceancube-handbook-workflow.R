if (file.exists('DESCRIPTION')) {
  handbook_lib <- tempfile('oceancube-handbook-example-lib-')
  dir.create(handbook_lib)
  on.exit(unlink(handbook_lib,recursive=TRUE,force=TRUE),add=TRUE)
  install.packages('.',repos=NULL,type='source',lib=handbook_lib,quiet=TRUE)
  .libPaths(c(handbook_lib,.libPaths()))
}
library(oceancube)
examples <- c('baseline_ocean_cube.R','backend_memory.R','cube_slice.R','cube_crop.R','cube_extract.R','cube_transect.R','cube_mask.R','refactor_temporal_backend.R','cube_geometry_weights.R')
for (nm in examples) {
  p <- system.file('examples',nm,package='oceancube')
  stopifnot(nzchar(p),file.exists(p))
  source(p,local=new.env(parent=globalenv()))
}
boundary <- system.file('architecture','oceancube-spatind-boundary.md',package='oceancube')
stopifnot(file.exists(boundary),!any(c('gini','patchiness','center_of_gravity') %in% getNamespaceExports('oceancube')))
cat('OCEANCUBE 0.1.0 HANDBOOK WORKFLOW COMPLETED
')
