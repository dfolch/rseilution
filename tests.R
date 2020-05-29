library(testthat)

source('RSEIAssignedToPoints.R')

prep_results <- function(file_name, x='lon', y='lat', s='state', yr='year', id='New_ID',
                         cap_s=FALSE, str_y=FALSE, ll=TRUE){
    
    sp <- readRDS(paste0('test_data/',file_name))
    
    # change year/state identifiers
    if (cap_s==TRUE){sp$state <- toupper(sp$state)}  # capitalize state codes
    if (str_y==TRUE){sp$year <- as.character(sp$year)}  # convert years to strings

    # change projection
    lat_lon_proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    if (ll != TRUE) {sp<-spTransform(sp,CRSobj = CRS(ll)) # arbitrary projection
        sp@data[,'Longitude'] <- sp@coords[,'Longitude']
        sp@data[,'Latitude'] <- sp@coords[,'Latitude']
    } else {sp<-spTransform(sp,CRSobj = CRS(lat_lon_proj))} # convert to lat/lon
    
    # change column names
    names(sp)[names(sp) == 'Longitude'] <- x
    names(sp)[names(sp) == 'Latitude'] <- y
    names(sp)[names(sp) == 'New_ID'] <- id
    names(sp)[names(sp) == 'state'] <- s
    names(sp)[names(sp) == 'year'] <- yr

    colnames(sp@coords) <- c(x,y)
    rownames(sp@bbox) <- c(x,y)

    return(sp)
}

prep_multi <- function(sp){    
    rownames(sp@data) <- NULL # reset row numbers to 1:n
    sp@data$New_ID <- c(1:nrow(sp@data)) # reset New_ID to 1:n
    return(sp)
}

symdiff <- function( x, y) {
    setdiff( union(x, y), intersect(x, y))
}

check_missing <- function(spdf, spdf_test){
    col_diff = symdiff(colnames(spdf@data), colnames(spdf_test@data))
    df_diff = spdf_test@data[,col_diff]
    expect_equal(sum(is.na(df_diff)), nrow(df_diff)*ncol(df_diff)) 
    spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reset columns to match spdf
    return(spdf_test)
}

print('one year, one state')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633)))
colnames(sp_df)<-c("lat","lon")
print('lower letter state, integer year')
spdf <- appendRSEI(data=sp_df,state='ri',year=2002,x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds')
expect_equal(spdf,spdf_test)
print('capital letter state')
spdf <- appendRSEI(data=sp_df,state='RI',year=2002,x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds', cap_s=TRUE)
expect_equal(spdf,spdf_test)


print('one year, one state (explicit year column as ints)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633)))
colnames(sp_df)<-c("lat","lon")
sp_df$YEAR<-c(2002,2002,2002)
spdf <- appendRSEI(data=sp_df,state='ri',year='YEAR',x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds', yr='YEAR')
spdf_test@data <- spdf_test@data[,c(1,2,3,5,4,6:ncol(spdf_test@data))]
expect_equal(spdf,spdf_test)


print('one year, one state (explicit year column as strings)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633)))
colnames(sp_df)<-c("lat","lon")
sp_df$YEAR<-c('2002','2002','2002')
spdf <- appendRSEI(data=sp_df,state='ri',year='YEAR',x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds', yr='YEAR', str_y=TRUE)
spdf_test@data <- spdf_test@data[,c(1,2,3,5,4,6:ncol(spdf_test@data))]
expect_equal(spdf,spdf_test)


print('one year, one state (explicit state column as lower case)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633)))
colnames(sp_df)<-c("lat","lon")
sp_df$STATE<-c('ri','ri','ri')
spdf <- appendRSEI(data=sp_df,state='STATE',year=2002,x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds', s='STATE')
expect_equal(spdf,spdf_test)


print('one year, one state (explicit state column as upper case)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633)))
colnames(sp_df)<-c("lat","lon")
sp_df$STATE<-c('RI','RI','RI')
spdf <- appendRSEI(data=sp_df,state='STATE',year=2002,x_name='lon',y_name='lat')
spdf_test <- prep_results('ri_2002_test.rds', s='STATE', cap_s=TRUE)
expect_equal(spdf,spdf_test)


print('one year, one state (explicit unique_id)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569,11),
c(41.812033,-71.414641,22),
c(41.8514,-71.4047633,33)))
colnames(sp_df)<-c("lat","lon","id")
spdf <- appendRSEI(data=sp_df,state='ri',year=2002,x_name='lon',y_name='lat', unique_id='id')
spdf_test <- prep_results('ri_2002_test.rds', id='id')
spdf_test$id <- c(11,22,33)
expect_equal(spdf,spdf_test)


print('one year, one state (explicit unique_id, using ID as name)')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569,11),
c(41.812033,-71.414641,22),
c(41.8514,-71.4047633,33)))
colnames(sp_df)<-c("lat","lon","ID")
spdf <- appendRSEI(data=sp_df,state='ri',year=2002,x_name='lon',y_name='lat', unique_id='ID')
spdf_test <- prep_results('ri_2002_test.rds')
spdf_test@data <- data.frame(spdf_test@data[,c(1,2,3)], ID_Original=c(11,22,33), 
                             spdf_test@data[,c(4:ncol(spdf_test@data))])
expect_equal(spdf,spdf_test)


print('two years, one state')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(39.7298862,-75.5645039),
c(39.7298862,-75.5645039),
c(39.7379306,-75.5898776),
c(39.7379306,-75.5898776),
c(39.7417784,-75.6361824),
c(39.7417784,-75.6361824)))
colnames(sp_df)<-c("lat","lon")
sp_df$state<-c("de","de","de","de","de","de")
sp_df$year<-c(2002,2003,2002,2003,2002,2003)
spdf <- appendRSEI(sp_df,state="state",year="year",x_name="lon",y_name="lat")
spdf_test <- prep_results('ri_de_2002_2003_test.rds')
spdf_test <- spdf_test[spdf_test$state!='ri',]
spdf_test <- prep_multi(spdf_test)
spdf_test <- check_missing(spdf,spdf_test)
expect_equal(spdf,spdf_test)


print('one year, two states')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.8514,-71.4047633),
c(39.7298862,-75.5645039),
c(39.7379306,-75.5898776),
c(39.7417784,-75.6361824)))
colnames(sp_df)<-c("lat","lon")
sp_df$state<-c("ri","ri","ri","de","de","de")
sp_df$year<-c(2002,2002,2002,2002,2002,2002)
spdf <- appendRSEI(sp_df,state="state",year="year",x_name="lon",y_name="lat")
spdf_test <- prep_results('ri_de_2002_2003_test.rds')
spdf_test <- spdf_test[spdf_test$year!=2003,]
spdf_test <- prep_multi(spdf_test)
spdf_test <- check_missing(spdf,spdf_test)
expect_equal(spdf,spdf_test)


print('two years, two states')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(41.486541,-71.308569),
c(41.486541,-71.308569),
c(41.812033,-71.414641),
c(41.812033,-71.414641),
c(41.8514,-71.4047633),
c(41.8514,-71.4047633),
c(39.7298862,-75.5645039),
c(39.7298862,-75.5645039),
c(39.7379306,-75.5898776),
c(39.7379306,-75.5898776),
c(39.7417784,-75.6361824),
c(39.7417784,-75.6361824)))
colnames(sp_df)<-c("lat","lon")
sp_df$state<-c("ri","ri","ri","ri","ri","ri","de","de","de","de","de","de")
sp_df$year<-c(2002,2003,2002,2003,2002,2003,2002,2003,2002,2003,2002,2003)
spdf <- appendRSEI(sp_df,state="state",year="year",x_name="lon",y_name="lat")
spdf_test <- prep_results('ri_de_2002_2003_test.rds')
spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reorder columns
expect_equal(spdf,spdf_test)


print('not lat/lon projection')
sp_df<-data.frame()
sp_df<-data.frame(rbind(
c(307274.07, 4595343),
c(299431.04, 4631725),
c(300374.01, 4636073)))
colnames(sp_df)<-c("x","y")
proj <- '+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
spdf <- appendRSEI(sp_df,state="ri",year=2002,x_name="x",y_name="y",projection=proj)
spdf_test <- prep_results('ri_2002_test.rds', x='x', y='y', ll=proj)
spdf_test@data <- spdf_test@data[,c(1,3,2,4:ncol(spdf_test@data))] # swap x,y columns
expect_equal(spdf,spdf_test, tolerance=1e-7)


print('read CSV')
spdf <- appendRSEI('test_data/sp_df.csv',state="state",year="year",x_name="longitude",y_name="latitude")
spdf_test <- prep_results('ri_de_2002_2003_test.rds', x="longitude", y="latitude")
spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reorder columns
spdf_test@data$state <- as.factor(spdf_test@data$state) # R reads in state column as factor
expect_equal(spdf,spdf_test)


print('read SHP')
spdf <- appendRSEI(data="test_data/sp_df.shp",state="state",year="year")
spdf_test <- prep_results('ri_de_2002_2003_test.rds', x="coords.x1", y="coords.x2")
spdf_test@data <- spdf_test@data[,!(names(spdf_test@data) %in% c('longitude','latitude'))]
spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reorder columns
if (substr(spdf@proj4string@projargs, 1,14) == substr(spdf_test@proj4string@projargs, 1,14)){ 
    spdf_test@proj4string <- spdf@proj4string # proj4 string is in different order
}
expect_equal(spdf,spdf_test)


print('read RDS')
spdf <- appendRSEI('test_data/sp_df.rds',state="state",year="year",x_name="longitude",y_name="latitude")
spdf_test <- prep_results('ri_de_2002_2003_test.rds', x="longitude", y="latitude")
spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reorder columns
expect_equal(spdf,spdf_test)


#### 
# the following test deletes all the intermedite files which slows down subsequent runs of the tests 
#### 
#print('two years, two states; remove intermediate files')
#sp_df<-data.frame()
#sp_df<-data.frame(rbind(
#c(41.486541,-71.308569),
#c(41.486541,-71.308569),
#c(41.812033,-71.414641),
#c(41.812033,-71.414641),
#c(41.8514,-71.4047633),
#c(41.8514,-71.4047633),
#c(39.7298862,-75.5645039),
#c(39.7298862,-75.5645039),
#c(39.7379306,-75.5898776),
#c(39.7379306,-75.5898776),
#c(39.7417784,-75.6361824),
#c(39.7417784,-75.6361824)))
#colnames(sp_df)<-c("lat","lon")
#sp_df$state<-c("ri","ri","ri","ri","ri","ri","de","de","de","de","de","de")
#sp_df$year<-c(2002,2003,2002,2003,2002,2003,2002,2003,2002,2003,2002,2003)
#spdf <- appendRSEI(sp_df,state="state",year="year",x_name="lon",y_name="lat",raster=FALSE)
#spdf_test <- prep_results('ri_de_2002_2003_test.rds')
#spdf_test@data <- spdf_test@data[,colnames(spdf@data)] # reorder columns
#expect_equal(spdf,spdf_test)



