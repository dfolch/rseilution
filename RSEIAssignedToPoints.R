######################################################
#
# This script provides a function that takes point coordinates in the US and
# appends airborne toxicity data from US Environmental Protection Agency's
# RSEI model, itself based on the Toxic Release Inventory (TRI).
#
# Authors: Christopher Fowler, David C. Folch, Matthew Laird
#
######################################################




#Setup
require(R.utils)
require(data.table)
require(sp)
require(raster)
require(stringr)
require(plyr)


appendRSEI <- function(data, state, year, x_name=NULL, y_name=NULL, unique_id=NULL,
                       projection=NULL, out_file=FALSE, raster='.rds') {
    # data (str or obj): path to point data file (csv, shp, rds) or R object (data
    #                    frame, spatial points data frame, simple features)
    # state (str)      : two letter state code (if all points in same state)
    #                    or name of column containing two letter state code for 
    #                    each point
    # year (int or str): single year passed as numeric (if all points in same 
    #                    year) or name of column containing year for each point
    # x_name (str)     : name of column containing x coordinate (often
    #                    longitude) for each point (optional for spatial
    #                    objects)
    # y_name (str)     : name of column containing y coordinate (often
    #                    latitude) for each point (optional for spatial
    #                    objects)
    # unique_id (str)  : name of column containing unique identifier for each
    #                    point (optional); note, if unique_id='ID' the name
    #                    will be changed to 'ID_Original'
    # projection (str) : full proj4 string for the spatial projection of points;
    #                    default assumes points are latitude/longitude (WGS84)
    # out_file (str)   : file name to store results; if FALSE (default) then no 
    #                    file written (optional)
    # raster (str or bool) : file type for storing the rasters; by default stored
    #                        in rds file type as R objects; other options include 
    #                        ".grd", ".nc", ".tif", ".envi", ".img", ".bil";
    #                        one file stored for each state-year combo; if FALSE
    #                        then all raster and intermediate files will be deleted
    # Returns a spatial points object

    usr_data <- .get_usr_data(to_import=data, state=state, year=year, x_name=x_name, 
                              y_name=y_name, unique_id=unique_id, projection=projection)
    .download_rsei(usr_data$state_years)
    chems <- .get_chemical_names()
    small <- .get_min_extent(usr_data$state_years)
    .collapse_years(usr_data$state_years, chems, small)
    match_list <- .match_rsei(spdf=usr_data$spdf, state_years=usr_data$state_years, 
                              state_col=usr_data$state_col, year_col=usr_data$year_col)
    spdf <- .return_rsei(match_list=match_list, unique_id=usr_data$unique_id, 
                         state_col=usr_data$state_col, year_col=usr_data$year_col,
                         projection=usr_data$projection)
    .generate_output(spdf=spdf, state_years=usr_data$state_years, out_file=out_file, 
                     raster=raster)
    return(spdf)
}


##############################
##### Support functions ######
##############################

.get_usr_data <- function(to_import, state, year, x_name, y_name, unique_id, projection) {

  #Function to take in user supplied data and convert to usuable format for
  #analysis with RSEIAssignedToPoints.R. User may provide data in either
  #a file or as a preloaded R object. Accepted forms of files are .csv,
  #.rds, and .shp. Accepted forms of R objects are data.frame/data.table,
  #SpatialPointsDataFrame(sp), and simple features(sf). .rds files will be
  #checked to ensure that saved R object complies with these requirements.
  #User may additionally specify a prefered proj4string and naming converntions
  #for coordinate columns, state code columns, and year columns or use the defaults supplied.

  state_codes = c('ak','al','ar','az','ca',
                  'co','ct','de','dc','fl','ga',
                  'hi','ia','id','il','in',
                  'ks','ky','la','ma','md',
                  'me','mi','mn','mo','ms',
                  'mt','nc','nd','ne','nh',
                  'nj','nm','nv','ny','oh',
                  'ok','or','pa','ri','sc',
                  'sd','tn','tx','ut','va',
                  'vt','wa','wi','wv','wy')  # list of two char state codes (lowercase)

  # setup some objects
  data <- c()
  sp_df <- c()
  if (is.null(projection)) { #standard lat/long
    projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  }
  # various file types that can be read in
  if (is.character(to_import)) {
    #print("data is in a file")
    if (tolower(tail(unlist(strsplit(to_import,".",fixed=TRUE)),n=1)) == "rds") {
      #print("loading data from .rds file")
      to_import <- readRDS(to_import)
    } else if (tolower(tail(unlist(strsplit(to_import,".",fixed=TRUE)),n=1)) == "shp") {
      #print("loading data from .shp file")
      to_import <- raster::shapefile(to_import) # returns SpatialPointsDataFrame
    } else if (tolower(tail(unlist(strsplit(to_import,".",fixed=TRUE)),n=1)) == "csv") {
      #print("loading data from .csv file")
      to_import <- read.csv(to_import) # returns dataframe
    } else {
      stop("invalid file format")
    }
  }

  # convert various R object types to SpatialPointsDataFrame
  if (("data.frame" %in% class(to_import)) & !("sf" %in% class(to_import))) {
    # convert dataframe
    data <- as.data.frame(to_import)
    #spdf$New_ID <- 1:length(to_import)
    sp_df <- SpatialPointsDataFrame(
      data = data,
      coords = data[,c(x_name,y_name)],
      proj4string = sp:::CRS(projection)
    )
  } else if ("SpatialPointsDataFrame" %in% class(to_import)) {
    # already SpatialPointsDataFrame
    sp_df <- to_import
  } else if ("sf" %in% class(to_import)) {
    # convert simple features object
    require(sf)
    sp_df <- sf:::as_Spatial(to_import)
  } else {
    stop("invalid R object type")
  }

  # capture projection of input data
  projection <- sp_df@proj4string  # already a CRS object

  # user passed single state identifier  
  if (tolower(state) %in% state_codes) {
    # ensure that new column does not conflict with existing column
    cols = colnames(sp_df@data)
    new_col <- 'state'
    counter <- 1
    while (new_col %in% cols) {
      new_col = paste0('state', '_', counter)
      counter = counter + 1
    }
    sp_df@data[,new_col] <- rep(state,length(sp_df@data[,1]))
    state = new_col
  # user passed column name
  } else if (state %in% colnames(sp_df@data)) {
  # catch invalid values
  } else {
    stop("invalid value passed to state")
  }

  # user passed single year identifier  
  if (is.numeric(year)) {  
    # ensure that new column does not conflict with existing column
    cols = colnames(sp_df@data)
    new_col <- 'year'
    counter <- 1
    while (new_col %in% cols) {
      new_col = paste0('year', '_', counter)
      counter = counter + 1
    }
    sp_df@data[,new_col] <- rep(year,length(sp_df@data[,1]))
    year = new_col
  # user passed column name
  } else if (year %in% colnames(sp_df@data)) {
  # catch invalid values
  } else {
    stop("invalid value passed to year")
  }

  # get list of all the unique state,year pairs
  state_years <- list()
  for (i in 1:length(sp_df@data[,1])) {
    state_years[[i]] <- list(as.character(sp_df@data[i,state]),sp_df@data[i,year])
  }
  state_years <- unique(state_years)

  # create ID column to keep track of the points
  if (is.null(unique_id)) {
    # no column name passed, create new column
    sp_df$New_ID<-1:length(sp_df[,1])
    unique_id = 'New_ID'
  } else if (unique_id=='ID') { # needed because when unique_id=NULL, unique_id=='ID' crashes R
    # raster::extract creates a column called "ID", so deal with it here
    sp_df$New_ID<-1:length(sp_df[,1])
    names(sp_df)[names(sp_df) == 'ID'] <- 'ID_Original'
    unique_id = 'New_ID'
    print('Warning: column name ID changed to ID_Original')
  } else if (length(unique(sp_df@data[,unique_id])) != length(sp_df[,1])) {
    # check that column user passed is actually unique
    stop("unique_id column contains duplicate values")
  }

  # spdf is a spatial points object containing all the columns originally
  # passed to to_import; if not provided by the user, it can contain up to
  # three new columns defining the year, state and unique ID of each point
  return(list("spdf"=sp_df, "state_years"=state_years, "state_col"=state, 
              "year_col"=year, "unique_id"=unique_id, 'projection'=projection))
}


.generate_output <- function(spdf, state_years, out_file, raster){
  #Function to process the output of appendRSEI and return output according to
  #user specifications set in out_file and raster. out_file expects either FALSE,
  #which returns only the resulting SpatialPointsDataFrame object, or a string
  #specifying the file name and file type, e.g. "out_file.csv". raster expects
  #either FALSE, which removes the raster .rds files, or a string specifying
  #the file name and file type, e.g. "out_file.shp". The default is set to
  #FALSE. A non-accepted response retains the local .rds files.

  # writing the raster data
  if (raster==FALSE){  # delete RDS files
    for (i in 1:length(state_years)){
      state <- unlist(state_years[[i]][1])
      year <- unlist(state_years[[i]][2])
      file_name <- paste0("grid_",state,"_",year,".rds")
      if(file.exists(file_name)){
        file.remove(file_name)
        file.remove(paste0(state,"_",year,".rds"))
        file.remove(paste0(state,"_",year,"_pop.rds"))
      }
    }
  } else if (raster=='.rds') { # leave the RDS files
  } else if (is.character(raster)) { # write to a non-RDS format
    raster_ext <- c("grd","nc","tif","envi","bil","img")
    rgdal_ext <- !(raster_ext %in% c("grd","bil"))
    fail_ext <- c("asc","sdat","rst")
    if (tolower(tail(unlist(strsplit(raster,".",fixed=TRUE)),n=1)) %in% raster_ext) {
      if (tolower(tail(unlist(strsplit(raster,".",fixed=TRUE)),n=1)) %in% rgdal_ext) {
        require(rgdal)
      }
      if (tolower(tail(unlist(strsplit(raster,".",fixed=TRUE)),n=1))=="nc") {
        require(ncdf4)
      }
      for (i in 1:length(state_years)) {
        state <- unlist(state_years[[i]][1])
        year <- unlist(state_years[[i]][2])
        r.stack <- readRDS(paste0("grid_",state,"_",year,".rds"))
        raster::writeRaster(r.stack, filename=paste0(state,"_",year,raster))
        #file.remove(paste0("grid_",state,"_",year,".rds"))  # remove .rds file
        if (file.exists(paste0(state,"_",year,".gri"))) {
          file.remove(paste0(state,"_",year,".gri"))
        } else if (file.exists(paste0(state,"_",year,".tif.aux.xml"))) {
          file.remove(paste0(state,"_",year,".tif.aux.xml"))
        } else if (file.exists(paste0(state,"_",year,".envi.aux.xml"))) {
          file.remove(paste0(state,"_",year,".envi.aux.xml"))
        } else if (file.exists(paste0(state,"_",year,".hdr"))) {
          file.remove(paste0(state,"_",year,".hdr"))
        } else if (file.exists(paste0(state,"_",year,".img.aux.xml"))) {
          file.remove(paste0(state,"_",year,".img.aux.xml"))
        }
    }
    } else if (tolower(tail(unlist(strsplit(raster,".",fixed=TRUE)),n=1)) %in% fail_ext) {
      print("file type passed to raster does not have multi-band support, as required for raster output")
    }
  } else {
    print("the raster parameter requires either FALSE or a supported raster out file name")
  }

  # writing the point data
  if (is.character(out_file)) {
    if (tolower(tail(unlist(strsplit(out_file,".",fixed=TRUE)),n=1))=="rds") {
      saveRDS(object=spdf,file=paste0(out_file))
    } else if (tolower(tail(unlist(strsplit(out_file,".",fixed=TRUE)),n=1))=="csv") {
      df <- data.frame(x=sp::coordinates(spdf)[,1], y=sp::coordinates(spdf)[,2],spdf@data)
      write.csv(df,file=paste0(out_file))
    } else if (tolower(tail(unlist(strsplit(out_file,".",fixed=TRUE)),n=1))=="shp") {
      raster::shapefile(spdf,paste0(out_file))
    } else {
      print('out_file requires csv, shp or rds file type')
    }
  }
}

.download_rsei <- function(state_years){
    #Download and unzip the microdata. This section accesses the files from
    #the EPA web site, decompresses those files and saves just the necessary
    #columns into an Rdata file for ease of use.

    #Columns we will extract
    col_names= c("X","Y","ChemicalNumber","Media","ToxConc","Pop")

    #Loop through all years and states
    for(i in 1:length(state_years)){
        state <- unlist(state_years[[i]][1])
        year <- unlist(state_years[[i]][2])
        out_file<-paste0("./",state,"_",year,".rds")
        out_file_pop<-paste0("./",state,"_",year,"_pop.rds")
        filename<-paste0("./v234_micro_state_",state,"_",year,".csv")
        gz_filename<-paste0(getwd(),"/v234_micro_state_",state,"_",year,".csv.gz")
        if(!file.exists(out_file)){ #No final file
          if(!file.exists(filename)){ #No unzipped file
            if(!file.exists(gz_filename)){  #No .gz file
              print(paste("Downloading raster for", state, year))
              URL<-paste0("http://epa-rsei-pds.s3.amazonaws.com/v234/microdata/state/",state,"/v234_micro_state_",state,"_",year,".csv.gz")
              download.file(url = URL,destfile = gz_filename)
            }#End download
            gunzip(gz_filename)
          }#End unzip
          #Begin data processing
          data<-fread(filename,header = FALSE,select=c(2,3,5,7,9,13))
          colnames(data)<-col_names
          pop.dat<-unique(data[,c("X","Y","Pop")])
          data<-data[Media%in%c(1,2)] #select down to just airborne. Others are not modeled well
          data<-data[,Media:=NULL]  #remove Media
          data<-data[,Pop:=NULL]  #remove Pop
          keycols<-c("X","Y","ChemicalNumber")
          setkeyv(data,keycols)
          data<-data[,sum(ToxConc),by=list(X,Y,ChemicalNumber)]
          saveRDS(data,file = out_file)
          saveRDS(pop.dat,file = out_file_pop)
          file.remove(filename)
        }
      }
    }

#Second Stage accesses spatial information. This chunk is just a couple of
#custom functions to process the raw data into a spatial form that is usable.

#Spatial information provided by EPA is not good. Generate our own grid from
#specifications provided in RSEI methods paper at
#https://www.epa.gov/sites/production/files/2017-01/documents/rsei_methodology_v2.3.5_0.pdf

#This function allows the user to build an appropriate SpatialGrid based on
#the range of grid cells in the data being analyzed
.make.custom.grid<-function(dat){
  llx= -2365605 #lower left x centroid
  lly= 251505  #lower left y centroid
  ncol.grid=5724 #number of columns
  nrow.grid=3618 #number of rows
  cell.size=810 #810 meters per cell
  aea.proj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  #Test to make sure projection yields an origin value of 0,0 when using
  #values provided by EPA
  #pt<-data.frame(x=-96,y=37.5)
  #unproj<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
  #test<-SpatialPoints(coords = pt ,proj4string = CRS(unproj))
  #test2<-spTransform(x = test,CRSobj = CRS(aea.proj))
  #coordinates(test2) #nearly equals zero

  dat$Longitude<-(dat$X+ifelse(dat$X>0,.5,-.5))*810
  dat$Latitude<-(dat$Y+ifelse(dat$Y>0,.5,-.5))*810
  dat<-as.data.frame(dat)
  coordinates(dat)<-~Longitude+Latitude
  projection(dat)<-aea.proj
  #Blank raster
  blank<-raster(nrows=(max(dat$Y)+1)-min(dat$Y),
                ncols=(max(dat$X)+1)-min(dat$X),
                xmn=bbox(dat)[1,1]-405, xmx=bbox(dat)[1,2]+405,
                ymn=bbox(dat)[2,1]-405, ymx=bbox(dat)[2,2]+405,
                crs=CRS(projection(dat)))
  return(blank)
}

#This takes a blank raster, such as one created in make.custom.grid above and
#takes data in the form provided by the EPA and fills the blank raster based
#on the numeric codes assigned by EPA.
.fill.raster<-function(blank,dat,x.range,y.range){
  col.one<-x.range[1]
  row.one<-y.range[2]
  dims<-dim(blank)
  #create return vector
  retVals<-data.frame(X=NA,Y=NA,val=getValues(blank))
  retVals$X<-c(col.one:(col.one+(dims[2]-1)))
  retVals$Y<-rep(row.one:(row.one-(dims[1]-1)),each=dims[2])
  retVals$X_Y<-paste0(retVals$X,"_",retVals$Y)
  retVals$order<-c(1:length(retVals[,1]))
  dat$X_Y<-paste0(dat$X,"_",dat$Y)

  retVals.single<-merge(retVals,dat[,3:4],by="X_Y",all.x=TRUE,all.y=FALSE)
  retVals.single<-retVals.single[duplicated(retVals.single$X_Y)==FALSE,]#not dropping some of the rows
  retVals.single<-retVals.single[order(retVals.single$order),]
  values(blank)<-retVals.single[,6]
  blank@title<-colnames(retVals.single[6])
  return(blank)
}

#This next chunk uses the functions just defined. Reads in the raw data and
#combines it with information on chemicals. It then uses this information to
#build a raster stack that has a layer for every chemical in the data and an
#initial layer with total concentration at each location

#Get chemical names
.get_chemical_names <- function(){
    if(!file.exists("chemicals.csv")){
      URL="http://epa-rsei-pds.s3.amazonaws.com/v234/data_tables/chemical.csv"
      download.file(url = URL,destfile = "chemicals.csv")
    }
    chems<-read.csv("chemicals.csv")
    chems<-chems[,c("ChemicalNumber","Chemical","ToxicityCategory","MaxTW")]
    return(chems)
}

#Need to get consistent minimum extent for each state
#Loop through all years and states
.get_min_extent <- function(state_years) {
  states <- c()
  for (i in 1:length(state_years)) {
    states[i] <- unlist(state_years[[i]][1])
  }
  states <- unique(states)
  small<-data.frame(State=states,minx=as.numeric(NA),maxx=as.numeric(NA),miny=as.numeric(NA),maxy=as.numeric(NA))

  for(i in 1:length(state_years)){
      state <- unlist(state_years[[i]][1])
      year <- unlist(state_years[[i]][2])
      filename<-paste0(state,"_",year,".rds")
      data <- readRDS(file=paste0(filename))
      current<-data.frame(State=state,minx=min(data$X),maxx=max(data$X),miny=min(data$Y),maxy=max(data$Y),stringsAsFactors=FALSE)
      if(is.na(small[small$State==state,"minx"])) {
        small[small$State==state,] <- current
      } else {
        small[small$State==state,"minx"]<-max(small[small$State==state,"minx"],as.numeric(current[2]))
        small[small$State==state,"maxx"]<-min(small[small$State==state,"maxx"],as.numeric(current[3]))
        small[small$State==state,"miny"]<-max(small[small$State==state,"miny"],as.numeric(current[4]))
        small[small$State==state,"maxy"]<-min(small[small$State==state,"maxy"],as.numeric(current[5]))
      }
  }
  return(small)
}

.collapse_years <- function(state_years, chems, small) {
#Loop through all years and states
for(i in 1:length(state_years)){
    state <- unlist(state_years[[i]][1])
    year <- unlist(state_years[[i]][2])
    out_file<-paste0("grid_",state,"_",year,".rds")
    filename<-paste0(state,"_",year,".rds")
    #print(paste("Building raster for", state, year))
    if(!file.exists(out_file)){ #No final file
      data <- readRDS(filename)
      data<-merge(data,chems,by="ChemicalNumber",all.x=TRUE,all.y=FALSE)
      data<-data[(data$X > small[small$State==state,"minx"]) & (data$X < small[small$State==state,"maxx"]),]
      data<-data[(data$Y > small[small$State==state,"miny"]) & (data$Y < small[small$State==state,"maxy"]),]
      chem.list<-unique(data$ChemicalNumber)
      for(k in 1:length(unique(data$ChemicalNumber))){
        if(k==1){ #if this is the first one then create the blank raster model and the total layer
          tmp<-aggregate(data[,c("V1")],by=list(data$X,data$Y),sum)
          colnames(tmp)<-c("X","Y","Total")
          #Generate a blank raster
          grid.blank<-.make.custom.grid(tmp)
          x.range<-c(small[small$State==state,2],small[small$State==state,3])
          y.range<-c(small[small$State==state,4],small[small$State==state,5])
          #Fill raster
          r.stack<-.fill.raster(blank=grid.blank,dat=tmp,x.range,y.range)
          names(r.stack)<-"Total"
          r.stack<-stack(r.stack)

          #load pop data and put that in as the second layer in the raster brick
          pop.dat <- readRDS(paste0(state,"_",year,"_pop.rds"))
          pop.stack<-.fill.raster(blank=grid.blank,dat=pop.dat,x.range,y.range)
          names(pop.stack)<-"Pop"
          r.stack<-stack(r.stack,pop.stack)
        }
        #select down to a single chemical
        tmp<-as.data.frame(data[data$ChemicalNumber==chem.list[k],])
        chem.name<-as.character(unique(tmp$Chemical))
        tmp<-tmp[,c("X","Y","V1")]
        colnames(tmp)[3]<-chem.name
        tmp.rast<-.fill.raster(blank=grid.blank,dat=tmp,x.range,y.range)
        names(tmp.rast)<-chem.name
        r.stack<-stack(r.stack,tmp.rast)
      }
      saveRDS(r.stack,file=out_file)
    }
  }
}


#Projection to match RSEI --since we have a fixed raster grid in this
#projection, whatever the input projection the output will be in this Albers
#Equal Area

.match_rsei <- function(spdf, state_years, state_col, year_col){

    aea.proj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    spdf<-spTransform(spdf,CRSobj = CRS(aea.proj)) # converts points to RSEI projection

    match_list <- list()
    #This section attaches the appropriate RSEI data (location and year) to each record in the spdf data

    for(i in 1:length(state_years)){
        state <- unlist(state_years[[i]][1])
        year <- unlist(state_years[[i]][2])
        filename<-paste0("grid_",state,"_",year,".rds")
        r.stack <- readRDS(filename)

        if(!(exists("toxic_output"))){
            toxic_output<-raster::extract(r.stack,spdf[spdf@data[,year_col]==year & spdf@data[,state_col]==state,], df=TRUE)
            data.spdf<-cbind(spdf@data[spdf@data[,year_col]==year & spdf@data[,state_col]==state,],toxic_output)
        }else{
            toxic_output<-raster:::extract(r.stack,spdf[spdf@data[,year_col]==year & spdf@data[,state_col]==state,],df=TRUE)
            tmp.spdf<-cbind(spdf@data[spdf@data[,year_col]==year & spdf@data[,state_col]==state,],toxic_output)
            data.spdf<-rbind.fill(data.spdf,tmp.spdf) #binds data.frames together adding columns as necessary or inserting NA's when columns are missing.
        }
      }
    match_list[["spdf"]] <- spdf
    match_list[["data.spdf"]] <- data.spdf
    return(match_list)
    }

.return_rsei <- function(match_list, unique_id, state_col, year_col, projection) {
    spdf <- match_list[["spdf"]]
    data.spdf <- match_list[["data.spdf"]]
    spdf@data <- merge.data.frame(spdf@data[unique_id],
                                  data.spdf[,colnames(data.spdf)%in%c('ID')==FALSE], # ID inserted by raster::extract
                                  by=unique_id,all.x = TRUE,sort=FALSE)
    colnames(spdf@data)[colnames(spdf@data)=="Total"]<-"Toxicity"
    spdf<-spTransform(spdf,CRSobj = projection) # converts back to user's projection

    return(spdf)
    }
