### edit an input file ###

library(ncdf4) # package for netcdf manipulation

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

varid <- template[['var']][[editVar]] # 

vartab <- ncvar_get(template, varid= editVar)

### EDIT THE VARIABLE ###

ncvar_put(template, varid, vartab)

### REPEAT AS NEEDED FOR EACH VARIABLE ###


##################################
############ METADATA ############
##################################
### OPTIONAL: CHANGE ANY METADATA (ATTRIBUTES) ###

attNames <- names(ncatt_get(template,0)) # names of metadata

editAtt <- '' # insert the name of the metadata to be edited here

meta <- '' # metadata here

ncatt_put(template, 0, editAtt, meta)

## EXAMPLE ##
ncatt_put(template, 0, 'Date created', format(Sys.time(), "%b %d %Y")) # timestamp

### REPEAT AS NEEDED FOR ALL METADATA ###

nc_close(template) # run this once all variables and metadata are edited