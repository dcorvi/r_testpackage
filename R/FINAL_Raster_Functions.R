PKG <- c('rgdal','rgeos')

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE) }
}

LOAD.AND.PREP.GIS <- function(SHPNAME, PROJECT.PATH = PROJECT.PATH, PROJ.USE = PROJ.USE) {
      SHP = readOGR(dsn=file.path(PROJECT.PATH), layer=SHPNAME, verbose=F)
      SHP = spTransform(SHP, CRS=PROJ.USE)
  if(NROW(SHP)>1) {
      SHP = gBuffer(SHP, width=1, byid=T) 
      SHP = gUnaryUnion(SHP) }
      stopifnot(gIsValid(SHP))
      return(SHP)
      }
      
#--------------------------------------------------------------------


TRIM.FINL <- function(FULLSET, COMP.STATES, THE.FOLDER) {
                      FULLSET = FULLSET[which(FULLSET$STATE1%in%COMP.STATES),]
                      
                      SD = file.path(THE.FOLDER,"individualrasters",COMP.STATES)
                        if(!file.exists(SD)) return(as.character("error - no directory to search"))
                      
                      SUM.DIRS = as.list(list.dirs(path=SD, recursive=F))
                        if(NROW(SUM.DIRS)==0) return(as.character("error - no existing grid files to start with"))
                      
                      ALL.THE.FILES = lapply(SUM.DIRS, list.files, recursive=T, full.names=F, pattern="*.gri")
                      ALL.THE.IDS   = lapply(ALL.THE.FILES, function(yy) {
                                              sapply(yy, USE.NAMES=F, function(zz) {
                                                            strsplit(x = as.character(zz), split = ".gri")[[1]]
                                                            }) #close zz
                                                            })       
                      ALL.THE.IDS   = unlist(ALL.THE.IDS)
                      FINF           = FULLSET[!FULLSET$IDNUM%in%ALL.THE.IDS,]
                      cat(paste("\nUsing short FIN dataset;",NROW(FINF),
                                    "missing rasters will be created for",COMP.STATES,
                                    " \n Expected proc. time is",round((NROW(FINF)/COMPUTERRATE),2),"minutes\n", sep=" "))
                      return(FINF)  
                      print("FINF is complete")
                                    }




############### 

jgDifferenceBuffCut<- function(x, y, byid=c(T, F), RASTER, FIELD = "PCT", FILE = "") {
                        stopifnot ((c("distance25","distance50","distance75","distance90","PCT","YEAR") %in% names(x)) & NROW(x)>0 & class(x)=="SpatialPointsDataFrame")

                        gear.l = x$GEARCATX[1]
                        rand.l = x$RAND[1]
                        year.l = x$YEAR[1]
                        #x$REVENUE = x$REVENUE/4      #Revenue gets fixed later when we know the # of rows left
                        distances = c("distance25","distance50","distance75","distance90")


                      
                        t.resl = list(t.res25 = x[,names(x)%notin%distances[2:4]], t.res50 = x[,names(x)%notin%distances[c(1,3:4)]], t.res75 = x[,names(x)%notin%distances[c(1:2,4)]], t.res90 = x[,names(x)%notin%distances[c(1:3)]])

                        names(t.resl$t.res25@data)[names(t.resl$t.res25)%in%distances] = "distance"
                        names(t.resl$t.res50@data)[names(t.resl$t.res50)%in%distances] = "distance"
                        names(t.resl$t.res75@data)[names(t.resl$t.res75)%in%distances] = "distance"
                        names(t.resl$t.res90@data)[names(t.resl$t.res90)%in%distances] = "distance"

                        row.names(t.resl$t.res25) = as.numeric(row.names(t.resl$t.res25)) + 100000000
                        row.names(t.resl$t.res50) = as.numeric(row.names(t.resl$t.res50)) + 200000000
                        row.names(t.resl$t.res75) = as.numeric(row.names(t.resl$t.res75)) + 300000000
                        row.names(t.resl$t.res90) = as.numeric(row.names(t.resl$t.res90)) + 400000000

                        

  t.resl = lapply(t.resl, function(tr)  {   
                                tr = gBuffer(tr, width = tr@data$distance, byid=TRUE, id = (row.names(tr)), capStyle="ROUND")
                                            })


  t.resl = do.call(rbind, t.resl)
  
  EMPTY = gIsEmpty(crop(y,extent(t.resl)), byid=F)
  
 	if(!EMPTY) yy = crop(y, extent(t.resl))
   
  t.resl = split(t.resl, f=t.resl@data$IDNUM, drop=T)

  if(!EMPTY) {
              t.res = lapply(t.resl, function(temp) {
                                                            if(gIsEmpty(crop(yy, extent(temp), byid=F))) return(temp)
              
                                                            t.x = temp@data
                                                            tr.d = gDifference(temp, crop(yy, extent(temp)), byid=c(T,F), id=row.names(temp) )
                                                              if(NROW(tr.d)==0) return(NULL) #if NROW(tr.d) is 0, returning a NULL removes it from the list
              
                                                            return(SpatialPolygonsDataFrame(tr.d, data=t.x[row.names(tr.d),], match.ID=T))
                                                          })                                     
               }  else {  #end if(!EMPTY)
               t.res = t.resl
                        }
rm(t.resl)
                        
t.res = t.res[!sapply(t.res,is.null)]
if(NROW(t.res)==0) (stop(paste("Error - no polygon area left to rasterize (all on land) in",gear.l,"- RAND",rand.l, year.l, sep=" "), call. = T))                    
                        
if(NROW(t.res)>0) {
  t.res = (lapply(t.res, function(temp) {    #sfLapply only runs in 2+ lists
                      if(NROW(temp)<=1) return(temp) #revenue is not reapportioned when NROW(temp) = 1; no need to.
                          
                      temp = temp[order(as.numeric(row.names(temp))),]

                      for(i in 2:NROW(temp)){
                                            tt = gDifference(temp[(i),], temp[(i-1),], byid=F, id=row.names(temp[i,]))
                                                  if(i==2) HOL = temp[1,]
                                            HOL = rbind(HOL, SpatialPolygonsDataFrame(tt, data=temp@data[row.names(tt),], match.ID=T))
                                             }
                      HOL@data$PCT = (HOL@data$PCT/NROW(HOL))     #New 3-2014; rev is allocated among remaining donuts
                      return(HOL) })) 
                    }

# 3-2014; removed t.res2, just work direclty on t.res
# --> unnecessary:   t.res2 = t.res2[!sapply(t.res2,is.null)]    #just to be sure
if(NROW(t.res)==0) (stop("Error - no polygon area left to rasterize (2) (should be rare!)", call. = T))                                                                               
          
           
   # if(sfIsRunning()) sfExport("RASTER","FIELD","FILE")
    
   
return(jRasterizeMosaicListWrapper(DATA = t.res, RASTER = RASTER, FIELD = FIELD, FILE = FILE))
                        
} 


## Wrapper function for splitting a gear-based list element into a list of IDNUMs with 1-4 donuts in each
## Then running jRasterizeMosaic to yield a list of summed rasters
jRasterizeMosaicListWrapper<- function(DATA, RASTER, FIELD, FILE){
      if(NROW(DATA)<=1) {
                         t.data.mosaic=lapply(DATA, jRasterizeMosaicWrite, RASTER = RASTER, FIELD = FIELD, FILE=FILE)
                         } 
      else {
            t.data.mosaic = lapply(DATA, jRasterizeMosaicWrite, RASTER = RASTER, FIELD = FIELD, FILE=FILE)
            }
    return(t.data.mosaic) 
    }
    

    

######### This one writes the small ones to disk. Hopefully it won't have the memory problems.
jRasterizeMosaicWrite <- function(DATA, RASTER, FIELD="PCT", FILE) {
      # fix filename to adapt to tmpdir or empty ""
      finm = ""
      if (FILE!="") finm = file.path(FILE, DATA@data$IDNUM[1])
    
    if(NROW(DATA)==0) {
                        print("There's no data here"); return(RASTER)
                      }
                        
    if(NROW(DATA)==1) {
    
    rt = crop(extend(RASTER, extent(DATA)), extent(DATA))
    temp = rasterize(DATA, rt, FIELD, filename="", silent=T, overwrite=T)
    temp = temp/(ncell(temp)-cellStats(temp, 'countNA'))
    writeRaster(temp, filename=finm, overwrite=T)
    return(temp)
                      }
    
    if(NROW(DATA)>1) {
      rt = crop(extend(RASTER, extent(DATA)), extent(DATA))
      HOL = list()
      for (i in 1:NROW(DATA)) {
    
        temp = rasterize(DATA[i,], rt, FIELD, filename="", silent=T)
        temp = temp/(ncell(temp)-cellStats(temp,'countNA'))
        HOL = c(HOL, temp)     }  #end for loop
    
        temp2 = do.call(mosaic, args=c(HOL, overwrite=T, fun='sum', na.rm=T, filename=finm))      ##file.path(FILENAME, mode.apr16id)
        rm(HOL);rm(temp)
        return(temp2) } #end if
        }
    









## Additional functions:
"%notin%" = function(x,y) (x[!x%in%y])
#--------------------------
Mode = function(x) {
			ux=unique(x)
			ux[which.max(tabulate(match(x, ux)))]
			}
#-----------------
.ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing = TRUE, 
    head = TRUE, n = 10) 
{
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, 
        pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)/10^6
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    out <- out[order(out[[order.by]], decreasing = decreasing), 
        ]
    if (head) 
        out <- head(out, n)
    out
}


F.TESTING <- function (IN) {

      print(paste("You put in",IN,"to show this works!", sep=" "))
      }
      