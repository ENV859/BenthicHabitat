### ENV 765 - Benthing Modeling Tool

tool_exec <- function(in_params, out_params)
{

 ### Handle input geoprocessing tool parameters
  presence_dbf <- in_params[[1]]
  random_dbf <- in_params[[2]]
  
  #Outputs
  mdl3var_file <- out_params[[1]]
  mdlaic_file <- out_params[[2]]
  
 # --- Begin common R script ---

  #Import 'foreign' library (to read DBF files)
  if (!requireNamespace("foreign", quietly = TRUE))
    install.packages("foreign")
  require(foreign)
  
  #Import 'arm' library (for bayesian modeling)
  if (!requireNamespace("arm", quietly = TRUE))
    install.packages("arm")
  require(arm)
  
  #Import 'MASS' library (for bayesian modeling)
  if (!requireNamespace("MASS", quietly = TRUE))
    install.packages("MASS")
  require(MASS)
  
  # Read in the two arcGIS SAMPLE results
  present = read.dbf(presence_dbf)
  random = read.dbf(random_dbf)
  
  # Drop the first three columns from each (ID, X, and Y fields...)
  present <- present[,c(-1:-3)]
  random <- random[,c(-1:-3)]
  
  # Add a "species" column to each, listing presence (1) or absence (0)
  present['species'] = rep(1, nrow(present))
  random['species'] = rep(0, nrow(random))
  
  # Bind them into a single table
  sp.pa = rbind(present, random)
  
  # Change missing data (-9999) to NA
  sp.pa[sp.pa == -9999] <- NA

  # change habras10 to 'factor' (categorical)
  sp.pa$habras10 = factor(sp.pa$habras10)

  # run a Bayes GLM
  mdlbayes <- bayesglm(formula = species ~ bath10_8w + botc10_8ws + brkd10_8w + klpd10_8w + habras10, family = binomial(link = "logit"), data = sp.pa)
  
  #Now let's use AIC to choose a best model.
  mdlaic <- stepAIC(mdlbayes, trace=T)

  # for comparison, lets try a model without both kelp distance and habitat
  # since habitat was also not very significant
  mdl3var <- bayesglm(formula = species ~ bath10_8w + botc10_8ws + brkd10_8w, family = binomial(link = "logit"), data = sp.pa)
  
  #write coefficients out to *.csv file for both models
  write.csv(summary(mdl3var)$coefficients, file=mdl3var_file)
  write.csv(summary(mdlaic)$coefficients, file=mdlaic_file)
}