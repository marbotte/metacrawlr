#require(RSQLite)

#' Create statement with foreign key REFERENCES clauses
#'
#' @param conn connection to a SQL database
#' @param tabName name of the table
#' @param fields name of the fields
#' @param types types of the fields
#' @param pk logical are the fields primary key (may only have one TRUE)
#' @param foreignTable names of the foreign table for the foreign keys
#' @param foreignRef names of the foreign variable for the foreign keys
#' @param listConstraint list, constraints to add
#' @param schema schema in which to create the table
#'
#' @returns statement to be used to create the table
#' @export
#'
createTableFK_statement<-function(conn,tabName,fields,types,pk,foreignTable,foreignRef,listConstraint=list(),schema=NULL)
{
  foreignExpression<-rep(NA,length(fields))
  okForeign<-(!is.na(foreignTable)&!is.na(foreignRef))
  if(any(okForeign)){
  foreignExpression[okForeign]<-paste0("REFERENCES ", DBI::dbQuoteIdentifier(conn,DBI::Id(schema=schema,table=foreignTable[okForeign]))
                                              ,"(",DBI::dbQuoteIdentifier(conn,foreignRef[okForeign]),")")
  }
  additionalConstraints<-""
  if(length(listConstraint)){
    additionalConstraints=paste(",",listConstraint,collapse=", ")
  }
  paste0("CREATE TABLE ",DBI::dbQuoteIdentifier(conn,DBI::Id(schema=schema,table=tabName))," (",
         paste0(
         DBI::dbQuoteIdentifier(conn,fields), " " , types, " ",
         ifelse(okForeign,foreignExpression,""),
         ifelse(pk," PRIMARY KEY",""),
         collapse=","),additionalConstraints,
         ")")
}


###################
# meta_i2d<-dbConnect(Postgres(),dbname="meta_i2d")
# conn<-meta_i2d
# schema<-"geonetwork"
# db<-conn
# extractedTables<-tabs_gn
# sqlite_file<-sqlite_gn
# overwrite=T
# saveBAK=NULL
# createFKindices=T
########################
#' Transform a name to obtain a more classical name for a SQL format
#'
#' @param x name(s)
#' @param maxBytes maximum bytes for a SQL name (by default 55, for it to work with postgres restrictions)
#'
#' @returns transformed names
#' @export
#'
sqlizeNames<-function(x,maxBytes=55)
{
  s1 <- gsub("^[-_.0-9]*","",gsub("\\_?([A-Z]{1,3})","_\\L\\1",gsub("^([A-Z]+)","\\L\\1",x,perl=T),perl=T))
  s2 <- gsub("\\.","",s1,perl=T)
  s3 <- strsplit(s2,'_')
  ctBytes<-lapply(lapply(s3,nchar,type='bytes'),function(x)cumsum(x[length(x):1]+1)[length(x):1])
  return(mapply(function(x,y)paste(x[y],collapse="_"),s3,lapply(ctBytes,function(x,m)x<m,m=maxBytes)))
}

#' From extracted R data.frame to SQL tables
#'
#' @param extractedTables extracted tables as resulting from the extractTable function
#'
#' @returns transformed tables to work with SQL databases
#' @export
#'
sqlize_extractedTables <- function(extractedTables)
{
  newTabNames<-sqlizeNames(extractedTables$info$tab$tabname)
  nr<-numRep(newTabNames)
  newTabNames[newTabNames %in% newTabNames[duplicated(newTabNames)]]<-paste(newTabNames,nr,sep="_")[newTabNames %in% newTabNames[duplicated(newTabNames)]]
  tabnames<-data.frame(old=extractedTables$info$tab$tabname,new=newTabNames)
  names(extractedTables$data)<-extractedTables$info$tab$tabname<-newTabNames

  m<-match(extractedTables$info$var$tabname,tabnames$old)
  extractedTables$info$var$tabname<-tabnames$new[m]
  m<-match(extractedTables$info$tab$foreigntable,tabnames$old)
  extractedTables$info$tab$foreigntable<-tabnames$new[m]
  extractedTables$info$tab$foreignref[!is.na(extractedTables$info$tab$foreigntable)]<-paste("cd",extractedTables$info$tab$foreigntable[!is.na(extractedTables$info$tab$foreigntable)],sep="_")
  extractedTables$info$tab$primarykey<-paste("cd",extractedTables$info$tab$tabname,sep="_")

  TABS<-extractedTables$info$tab$tabname
  for(i in 1:length(TABS))
  {
    tab<-TABS[i]
    newVarNames<-sqlizeNames(extractedTables$info$var$varname[extractedTables$info$var$tabname==tab])
    nr<-numRep(newVarNames)
    newVarNames[newVarNames %in% newVarNames[duplicated(newVarNames)]]<-paste(newVarNames,nr,sep="_")[newVarNames %in% newVarNames[duplicated(newVarNames)]]
    varnames<-data.frame(old=extractedTables$info$var$varname[extractedTables$info$var$tabname==tab],
                         new=newVarNames)
    m<-match(extractedTables$info$var$varname[extractedTables$info$var$tabname==tab],varnames$old)
    extractedTables$info$var$varname[extractedTables$info$var$tabname==tab]<-varnames$new[m]
    m<-match(colnames(extractedTables$data[[tab]]),varnames$old)
    if(sum(is.na(m))==1 & is.na(extractedTables$info$tab$foreigntable[extractedTables$info$tab$tabname==tab]))
    {
      colnames(extractedTables$data[[tab]])<-c(extractedTables$info$tab$primarykey[extractedTables$info$tab$tabname==tab],varnames$new[m[!is.na(m)]])
    }else {
      if(sum(is.na(m))==2 &!is.na(extractedTables$info$tab$foreigntable[extractedTables$info$tab$tabname==tab]))
      {
        colnames(extractedTables$data[[tab]])<-c(extractedTables$info$tab$primarykey[extractedTables$info$tab$tabname==tab],extractedTables$info$tab$foreignref[extractedTables$info$tab$tabname==tab], varnames$new[m[!is.na(m)]])
      } else {
      stop("Unexpected case in terms of variables and primary/foreign keys")
      }
    }
  }
  return(extractedTables)

}


#' Export tables in postgres
#'
#' @param extractedTables list of tables to export
#' @param conn connection to the the postgres database
#' @param schema schema in which to create the tables
#' @param overwrite boolean, whether to overwrite schemas and tables
#' @param createFKindices Whether to create the foreign keys indexes
#'
#' @returns result of the dbCommit function at the end of the transaction
#' @export
#'
extractedTables<-tabs_ceiba
conn=mI2D
schema<-schemaCeiba
overwrite=T
createFKindices=T
exportPostgres<-function(extractedTables,conn,schema=NULL,overwrite=T,createFKindices=T)
{
  RPostgres::dbBegin(conn)
  #Does the schema exist
  schemaExists <- schema %in% RPostgres::dbGetQuery(conn,"SELECT schema_name FROM information_schema.schemata")$schema_name
  if(schemaExists)
  {
    if(overwrite)
    {
      RPostgres::dbExecute(conn,paste0("DROP SCHEMA ", schema, " CASCADE"))
    }else{
      stop("The schema ",schema, "already exists, and the parameter overwrite is set to FALSE")
    }
  }
  RPostgres::dbExecute(conn,paste0("CREATE SCHEMA ",schema))
  # Create info Tables
  ## tabInfo
  stat<-createTableFK_statement(conn,tabName = "tabinfo",
                                fields=colnames(extractedTables$info$tab),
                                types=RPostgres::dbDataType(conn,extractedTables$info$tab),
                                pk=(colnames(extractedTables$info$tab)=="tabname"),
                                foreignTable=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabinfo",NA),
                                foreignRef=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabname",NA),
                                schema=schema
  )
  RPostgres::dbExecute(conn,stat)
  ## varinfo
  stat<-createTableFK_statement(conn,tabName="varinfo",
                                fields=colnames(extractedTables$info$var),
                                types=RPostgres::dbDataType(conn,extractedTables$info$var),
                                pk=rep(FALSE,ncol(extractedTables$info$var)),
                                foreignTable=ifelse(colnames(extractedTables$info$var)=="tabname","tabinfo",NA),
                                foreignRef=ifelse(colnames(extractedTables$info$var)=="tabname","tabname",NA),
                                listConstraint=list("PRIMARY KEY (tabname, varname)"),
                                schema=schema
  )
  RPostgres::dbExecute(conn,stat)
  # Add info
  RPostgres::dbAppendTable(conn,DBI::Id(schema=schema,table="tabinfo"),extractedTables$info$tab)
  RPostgres::dbAppendTable(conn,DBI::Id(schema=schema,table="varinfo"),extractedTables$info$var)
  # Create metadata tables
  for(i in 1:nrow(extractedTables$info$tab))
  {
    tab<-extractedTables$info$tab$tabname[i]
    vars<-extractedTables$info$var$varname[extractedTables$info$var$tabname==tab]
    pkey<-extractedTables$info$tab$primarykey[i]
    ft<-extractedTables$info$tab$foreigntable[i]
    fkey<-extractedTables$info$tab$foreignref[i]
    fields<-c(pkey,stats::na.omit(fkey),vars)
    stopifnot(fields==colnames(extractedTables$data[[tab]]))
    stat<-createTableFK_statement(conn,
                                  tabName=tab,
                                  fields=fields,
                                  types=RPostgres::dbDataType(conn,extractedTables$data[[tab]]),
                                  pk=(fields==pkey),
                                  foreignTable=ifelse(fields==fkey,ft,NA),
                                  foreignRef=ifelse(fields==fkey,fkey,NA),
                                  schema=schema
    )
    RPostgres::dbExecute(conn,stat)
    RPostgres::dbAppendTable(conn,DBI::Id(schema=schema,table=tab),value=extractedTables$data[[tab]])
    if(createFKindices&sum(fields==fkey,na.rm = T)>0){
      RPostgres::dbExecute(conn,paste0("CREATE INDEX fk_",tab,"_",ft,"_idx ON ",DBI::dbQuoteIdentifier(conn,DBI::Id(schema=schema,table=tab)),"(",stats::na.omit(fields[fields==fkey]),")",collapse=" ; "))}
  }
  return(RPostgres::dbCommit(conn))
}

#' Export the extracted tables in a sqlite database
#'
#' @param extractedTables extracted tables
#' @param sqlite_file path and name of the sqlite file
#' @param overwrite whether to overwrite the sqlite database
#' @param saveBAK whether to create a backup of the sqlite file
#' @param createFKindices whether to create indexes for the foreign keys
#'
#' @returns SQLite database connection
#' @export
#'
exportSQLite<-function(extractedTables,sqlite_file,overwrite=T,saveBAK=NULL,createFKindices=T){
  fExist<-file.exists(sqlite_file)
  if(fExist){
    if(overwrite){
      if(!is.null(saveBAK)){
        file.rename(sqlite_file,saveBAK)
      }else{file.remove(sqlite_file)}
    }else{stop("SQLite file exists, use overwrite if you want to remove it")}
  }
  db<-RSQLite::dbConnect(RSQLite::SQLite(),sqlite_file)
  # Create info Tables
  ## tabInfo
  stat<-createTableFK_statement(db,tabName = "tabinfo",
                          fields=colnames(extractedTables$info$tab),
                          types=RSQLite::dbDataType(db,extractedTables$info$tab),
                          pk=(colnames(extractedTables$info$tab)=="tabname"),
                          foreignTable=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabinfo",NA),
                          foreignRef=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabname",NA)
  )
  RSQLite::dbExecute(db,stat)
  ## varinfo
  stat<-createTableFK_statement(db,tabName="varInfo",
                          fields=colnames(extractedTables$info$var),
                          types=RSQLite::dbDataType(db,extractedTables$info$var),
                          pk=rep(FALSE,ncol(extractedTables$info$var)),
                          foreignTable=ifelse(colnames(extractedTables$info$var)=="tabname","tabinfo",NA),
                          foreignRef=ifelse(colnames(extractedTables$info$var)=="tabname","tabname",NA),
                          listConstraint=list("PRIMARY KEY (tabname, varname)")
  )
  RSQLite::dbExecute(db,stat)
  # Add info
  RSQLite::dbAppendTable(db,"tabinfo",extractedTables$info$tab)
  RSQLite::dbAppendTable(db,"varinfo",extractedTables$info$var)
  # Create metadata tables
  for(i in 1:nrow(extractedTables$info$tab))
  {
    tab<-extractedTables$info$tab$tabname[i]
    vars<-extractedTables$info$var$varname[extractedTables$info$var$tabname==tab]
    pkey<-extractedTables$info$tab$primarykey[i]
    ft<-extractedTables$info$tab$foreigntable[i]
    fkey<-extractedTables$info$tab$foreignref[i]
    fields<-c(pkey,stats::na.omit(fkey),vars)
    stopifnot(fields==colnames(extractedTables$data[[tab]]))
    stat<-createTableFK_statement(db,
                                  tabName=tab,
                                  fields=fields,
                                  types=RSQLite::dbDataType(db,extractedTables$data[[tab]]),
                                  pk=(fields==pkey),
                                  foreignTable=ifelse(fields==fkey,ft,NA),
                                  foreignRef=ifelse(fields==fkey,fkey,NA)
                                  )
    RSQLite::dbExecute(db,stat)
    RSQLite::dbAppendTable(db,tab,extractedTables$data[[tab]])
    if(createFKindices&sum(fields==fkey,na.rm = T)>0){
      RSQLite::dbExecute(db,paste0("CREATE INDEX fk_",tab,"_",ft,"_idx ON ",tab,"(",stats::na.omit(fields[fields==fkey]),")",collapse=";"))}
  }
  return(db)
}

#require(openxlsx)
#' Export a list of tables in an Excel spreadsheet (xlsx file)
#'
#' @param file path and name of a file
#' @param lVar list of variables (or vector variable names)
#'
#' @returns result of the openxlsx::saveWorkbook function
#' @export
#'
save_in_excel<-function(file,lVar)
{
  if(!is.list(lVar)){
    listVar<-mget(lVar,envir = .GlobalEnv)
  }else{listVar<-lVar}
  pbSizeName<-nchar(names(listVar))>31
  if(any(pbSizeName)){
    names(listVar)[pbSizeName]<-paste(substr(names(listVar)[pbSizeName],1,12),substr(names(listVar)[pbSizeName],nchar(names(listVar)[pbSizeName])-13,nchar(names(listVar)[pbSizeName])),sep="_..._")
  }
  dupesNames<-duplicated(names(listVar))
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(listVar))
  {
    sn<- names(listVar)[i]
    openxlsx::addWorksheet(wb, sheetName = sn)
    hasRownames <- !all(grepl("^[0-9]*$",rownames(listVar[[i]])))
    openxlsx::writeDataTable(wb, sheet =sn, listVar[[i]],rowNames = hasRownames)
    nCols<-ifelse(hasRownames,ncol(listVar[[i]]), ncol(listVar[[i]]))
    openxlsx::setColWidths(wb, sheet =sn,cols = 1:nCols, widths = 'auto')
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}


#' Export extracted tables from a list of xml documents
#'
#' @param extractedTables extracted tables from the function extractTable
#' @param file path and file name from the excel file
#' @param exportInfoTables whether to export the tables which describe the tables
#'
#' @returns result of the openxlsx::saveWorkbook
#' @export
#'
exportXL<-function(extractedTables,file,exportInfoTables=T)
{
  if(exportInfoTables){
  listExport<-c(extractedTables$info,extractedTables$data)
  }else{
    listExport<-extractedTables$info
  }
  save_in_excel(file,listExport)
}



