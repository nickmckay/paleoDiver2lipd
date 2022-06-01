library(lipdR)
library(tidyverse)


sitemeta <- read_csv("data/Site_table_20200114.csv")
levels <- read_csv("data/Levels_ages_20200114.csv")

#fix character reading problem

tofix <- which(levels$DB==22 & levels$Cal_yr_BP == 4230 & is.na(levels$Age_Yng))
if(length(tofix) != 1){
  stop("something broke")
}
levels$Age_Yng[tofix] <- 4170


#get conversion sheet:
conv <- googlesheets4::read_sheet(ss = "1bldIOiarofoRhzyJzBE44s8sWkj4plY8VgbHgdPUW3Q")


#find the good sites, only the preferred:
good <- filter(sitemeta, Preferred == "Y")


allDataSet <- unique(good$DB)

if(length(allDataSet) != nrow(good)){
  stop("non unique sites")
}


db <- allDataSet[4]


for(db in allDataSet){
  #function to make LiPD here
  metaRow <- filter(good,DB == db)
  thisData <- filter(levels, DB == db)
  useRelative <- unique(thisData$Use_Relative)
  toConv <- thisData %>%
    select(Cal_yr_BP:Relative)

  if(length(unique(thisData$Use_Relative)) > 1 ){
    stop("test")
  }
  # Create root -------------------------------------------------------------

  L <- list()
  L$lipdVersion <- 1.3
  L$archiveType <- "LakeSediment"
  firstAuthor <- str_split(metaRow$Citation,pattern = " ")[[1]][1] %>%
    str_remove_all("[^A-Za-z]")
  if(is.na(firstAuthor)){
    firstAuthor <- "author"
  }
  pubYear <-  str_extract(metaRow$Citation,pattern = "([0-9][0-9][0-9][0-9])")
  if(is.na(pubYear)){
    pubYear <- "1111"
  }

  sitename <- metaRow$Name %>% str_remove_all(" ") %>%
    str_remove_all("[^A-Za-z0-9]")

  L$dataSetName <- paste(sitename,firstAuthor,pubYear,sep = ".")

  #Be aware there will be duplicate dataset names
  L$datasetId <- paste0("PD",digest::digest(metaRow))

  L$createdBy <- "github.com/nickmckay/paleoDiver2lipd"
  L$originalDataSource <- "https://www.ncei.noaa.gov/access/paleo-search/study/28771"
  L$paleoDIVERSiteId <- metaRow$Site[1]
  L$paleoDIVERDatasetId <- metaRow$DB[1]

  L <- lipdverseR::initializeChangelog(L)

  # Geo metadata ------------------------------------------------------------
  geo <- list()
  geo$latitude <- siteMeta$`Latitude (DD)`
  geo$longitude <- siteMeta$`Longitude (DD)`
  geo$description <- siteMeta$Site_Description
  geo$siteName <- siteMeta$Site_Name
  geo$continent <- siteMeta$Continent

  L$geo <- geo

  # Pub metadata ------------------------------------------------------------

  pub <- vector(mode = "list",length = 1)
  pub[[1]]$citation <- siteMeta$First_Publication

  L$pub <- pub

  # Paleodata ---------------------------------------------------------------

  #check paleo metadata
  paleoMeta <- filter(allReg,`ID (Site)` == siteId & `ID (Dataset)` == datasetId) %>%
    select(Event:Continent) %>%
    distinct()

  if(nrow(paleoMeta) == 0){
    print("no matches")
    return(NA)
  }

  if(nrow(paleoMeta) > 1){
    stop("this is bad")
  }

  thisPmt <- filter(allReg,`ID (Site)` == siteId & `ID (Dataset)` == datasetId) %>%
    select(contains("["))

  an <- names(thisPmt)

  pmt <-vector(mode = "list",length = 1)

  for(coll in 1:length(an)){
    #get conversion row
    cr <- which(conv$ColName == an[coll])
    if(length(cr) != 1){
      stop("bad conv match")
    }
    thisCol <- list()
    thisCol$number <- coll
    thisCol$TSid <- lipdR::createTSid("lc")
    thisCol$values <- as.matrix(thisPmt[,coll])
    thisCol$variableName <- conv$variableName[cr]
    thisCol$units <- conv$units[cr]

    if(startsWith(thisCol$variableName,"age")){
      thisCol$values <- as.matrix(thisPmt[,coll])*1000
      thisCol$units <- "yr BP"
    }

    thisCol$primaryTimeseries <- conv$primaryTimeseries[cr]
    thisCol$primaryAgeColumn <- conv$primaryAgeColumn[cr]
    thisCol$summaryStatistic <- conv$`summary statistic`[cr]
    if(!is.na(conv$calibration_method[cr])){
      thisCol$calibration <- list()
      thisCol$calibration$method <- conv$calibration_method[cr]
      thisCol$calibration$target <- conv$calibration_target[cr]
      thisCol$interpretation <- vector(mode = "list",length = 1)
      thisCol$interpretation[[1]]$scope <- "climate"
      thisCol$interpretation[[1]]$variable <- conv$interpVariable[cr]
      thisCol$interpretation[[1]]$variableDetail <- conv$interpVariableDetail[cr]
      thisCol$interpretation[[1]]$seasonality <- conv$seasonality[cr]
    }

    cname <- conv$variableName[cr]
    ni <- 0
    while(any(names(pmt[[1]]) == cname)){
      ni <- ni+1
      cname <- paste0(conv$variableName[cr],"-",ni)
    }
    pmt[[1]][[cname]] <- thisCol
  }

  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt




}
