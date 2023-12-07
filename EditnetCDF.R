### edit an input file ###

library(ncdf4) # package for netcdf manipulation

setwd('') # directory containing netCDF file

origName <- '' # insert the filename of the input file to be edited here
newName <- '' # insert new filename here

file.copy(origName, newName, overwrite = FALSE)
template <- nc_open(newName, write = TRUE)


###################################
############ VARIABLES ############
###################################
numVar <- length(template[['var']]) # number of variables (may need to edit 'var', check the file)
varNames <- names(template[['var']]) # names of variables

editVar <- '' # insert the name of the variable to be edited here

varID <- template[['var']][[editVar]] # variable metadata e.g., dimensions, units, etc.

varTab <- ncvar_get(template, varid= editVar) # variable matrix (check dimensions; can be a 2, 3, or 4 dimension matrix)

### EDIT THE VARIABLE ###

ncvar_put(template, varID, varTab)

## EXAMPLE ##
# varTab[1:72, 1:46] <- 0

### REPEAT AS NEEDED FOR EACH VARIABLE ###


##################################
############ METADATA ############
##################################
### OPTIONAL: CHANGE ANY METADATA (ATTRIBUTES) ###

attNames <- names(ncatt_get(template,0)) # names of metadata

editAtt <- '' # insert the name (title) of the metadata to be edited here

meta <- '' # metadata here

### EDIT THE METADATA ###

ncatt_put(template, 0, editAtt, meta)

## EXAMPLE ##
# ncatt_put(template, 0, 'Date created', format(Sys.time(), "%b %d %Y")) # timestamp

### REPEAT AS NEEDED FOR ALL METADATA ###

nc_close(template) # run this once all variables and metadata are edited