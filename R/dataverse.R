#' Analyse variables from a dataverse postgres database
#'
#' The variables considered here are the variables that are defined as part of
#' a metadatablock ("user-defined" variables, even though it might be the ç
#' default variables from the default installation of a dataverse instance)
#'
#'
#' @param dvcon connection to the dataverse postgres database (as created from RPostgres::dbConnect)
#'
#' @returns A list with various elements that will be used
#' @export
#'
dvAnalyseVar<-function(dvcon)
{

  # Analyses the variables from a dataverse database,
  analysis<-RPostgres::dbGetQuery(dvcon,"
  SELECT dft.id, mdb.name metadatablock, dft.name, fieldtype, allowmultiples,
    COUNT(dfv.value) FILTER (WHERE value IS NOT NULL) nb_values,
    COUNT(dfcvv.controlledvocabularyvalues_id) FILTER (WHERE dfcvv.controlledvocabularyvalues_id  IS NOT NULL) nb_controlled_values,
    dft.parentdatasetfieldtype_id,
    dft.id IN (SELECT DISTINCT parentdatasetfieldtype_id FROM datasetfieldtype WHERE parentdatasetfieldtype_id IS NOT NULL) is_parent
  FROM datasetfieldtype dft
  LEFT JOIN metadatablock mdb ON dft.metadatablock_id=mdb.id
  LEFT JOIN datasetfield df ON df.datasetfieldtype_id=dft.id
  LEFT JOIN datasetfieldvalue dfv ON dfv.datasetfield_id=df.id
  LEFT JOIN datasetfield_controlledvocabularyvalue dfcvv ON dfcvv.datasetfield_id=df.id
  GROUP BY dft.id, mdb.name, dft.name, fieldtype, allowmultiples, dft.id IN (SELECT DISTINCT parentdatasetfieldtype_id FROM datasetfieldtype WHERE parentdatasetfieldtype_id IS NOT NULL), dft.parentdatasetfieldtype_id
  ORDER BY dft.id
  ")
  analysis$is_gp<-analysis$allowmultiples | analysis$is_parent
  # Here we checked whether the defined variables are actually used in the
  # datasets, but if the variable is a parent, we need to check whether the
  # children variables in the group have values
  toKeep<-logical(nrow(analysis))
  for(i in analysis$id)
  {toKeep[analysis$id==i]<- as.logical(
    sum(colSums(analysis[analysis$id == i | analysis$parentdatasetfieldtype_id == i,
                         c("nb_values","nb_controlled_values")],na.rm = T)))}
  # It is much more simple not to consider that variables can be children
  # AND parents, here we check whether this is the case in the instance of dataverse
  analysis<-analysis[toKeep,]
  if(any((!is.na(analysis$parentdatasetfieldtype_id)) & analysis$allowmultiples))
  {stop("The code has been thought for cases where variables either have parent variable or accept multiple values, not both")}
  stopifnot(!nrow(analysis[(analysis$is_parent)  & (!is.na(analysis$parentdatasetfieldtype_id)),]))
  gpTab<-analysis[analysis$is_gp,]
  listGp<-list()
  for(i in 1:nrow(gpTab))
  {
    listGp[[gpTab$name[i]]]<-analysis[analysis$id==gpTab[i,"id"]|(!is.na(analysis$parentdatasetfieldtype_id) & analysis$parentdatasetfieldtype_id==gpTab[i,"id"]),]
  }
  analysis$var_gp<-factor(NA,levels=c("var","gp","gpvar"))
  analysis$var_gp[analysis$is_parent]<-"gp"
  analysis$var_gp[!analysis$is_parent & analysis$is_gp & rowSums(analysis[,c("nb_values","nb_controlled_values")])] <- "gpvar"
  analysis$var_gp[!analysis$is_parent & !analysis$is_gp & rowSums(analysis[,c("nb_values","nb_controlled_values")])] <- "var"

  analysis$gpHier <- analysis$inGp <- NA
  for (i in 1:nrow(analysis))
  {
    prepGpHier<-c("dataverse","dataset","datasetversion",analysis$metadatablock[i])
    if(analysis$var_gp[i]=="gp")
    {
      prepGpHier<-c(prepGpHier,analysis$name[i])
      analysis$inGp[i]<-analysis$id[i]
    }
    if(analysis$var_gp[i]=="gpvar")
    {
      prepGpHier<-c(prepGpHier,paste0(analysis$name[i],"_"))
      analysis$inGp[i]<-analysis$id[i]
    }
    if(!is.na(analysis$parentdatasetfieldtype_id[i]))
    {
      prepGpHier<-c(prepGpHier,analysis$name[analysis$id==analysis$parentdatasetfieldtype_id[i]])
      analysis$inGp[i]<-analysis$parentdatasetfieldtype_id[i]
    }
    if(analysis$var_gp[i]=="var"||analysis$var_gp[i]=="gpvar"){
      prepGpHier<-c(prepGpHier,analysis$name[i])
    }
    analysis$gpHier[i]<-paste(prepGpHier,collapse="/")
  }

  gpTab<-rbind(
    data.frame(
      id=c(1000,1001,1002,1003,1004,1005),
      name=c("dataverse","dataset","datasetversion","citation","geospatial","socialscience"),
      gpHier=c("dataverse","dataverse/dataset","dataverse/dataset/datasetversion",paste("dataverse/dataset/datasetversion",c("citation","geospatial","socialscience"),sep="/")),
      var_gp="gp",
      inGp=c(1000,1001,1002,1003,1004,1005)
    ),
    analysis[analysis$var_gp %in% c("gp", "gpvar"),c("id", "name", "gpHier", "var_gp", "inGp")]
  )
  gpTab$name[gpTab$var_gp=="gpvar"]<-paste0(gpTab$name[gpTab$var_gp=="gpvar"],"_")
  gpTab$gpHier<-sub("(^.*_).*$","\\1",gpTab$gpHier)
  gpTab$var_gp="gp"

  varTab<-analysis[analysis$var_gp %in% c("gpvar", "var"), c("id", "name", "gpHier", "var_gp", "inGp") ]
  addGp<-is.na(varTab$inGp)
  varTab$inGp[addGp]<-gpTab$id[match(analysis[analysis$var_gp %in% c("gpvar", "var"),"metadatablock"][addGp],gpTab$name)]
  varTab$var_gp<-"var"

  varTab$inGp%in%gpTab$id

  hierTab<-rbind(gpTab,varTab)
  if(any(hierTab$name%in%NODE_RESERVED_NAMES_CONST))
  {
    hierTab$name[hierTab$name%in%NODE_RESERVED_NAMES_CONST]<-paste0(
      hierTab$name[hierTab$name%in%NODE_RESERVED_NAMES_CONST],"_")
  }
  return(list(analysis=analysis,gpTab=gpTab,varTab=varTab,hierTab=hierTab))
}

dv_plot_variables<-function(res_analysis)
{
  net<-as.igraph(FromDataFrameTable(res_analysis$hierTab,pathName="gpHier"))
  m<-match(V(net)$name,res_analysis$hierTab$name)
  V(net)$var_gp<-res_analysis$hierTab$var_gp[m]
  V(net)$inGp<-as.numeric(factor(res_analysis$hierTab$inGp))[m]

  plot(net, vertex.size=5, vertex.label=V(net)$name, vertex.shape=c(var="circle",gp="square")[V(net)$var_gp],
       vertex.color=rainbow(length(unique(V(net)$inGp)))[V(net)$inGp])
}

# sqlizeNames <- function(x)
# {
#   gsub("^[-_.0-9]*","",gsub("\\_?([A-Z]{1,3})","_\\L\\1",gsub("^([A-Z]+)","\\L\\1",x,perl=T),perl=T))
# }


dvPrepareTableDescription<-function(listAnalysis, dvDb)
{
  mGp<-match(listAnalysis$gpTab$id,listAnalysis$analysis$id)
  pkeys<-character(nrow(listAnalysis$gpTab))
  pkeys<-paste(listAnalysis$gpTab$name,"id",sep="_")
  pkeys[is.na(mGp)&listAnalysis$gpTab$name%in%listAnalysis$analysis$metadatablock]<-"datasetversion_id"
  pkeys[grep("_$",listAnalysis$gpTab$name)]<-paste(listAnalysis$gpTab$name[grep("_$",listAnalysis$gpTab$name)],"id",sep="")
  fkeys<-rep("datasetversion_id",nrow(listAnalysis$gpTab))
  fkeys[listAnalysis$gpTab$name=="datasetversion"]<-"dataset_id"
  fkeys[listAnalysis$gpTab$name=="dataset"]<-"dataverse_id"
  fkeys[listAnalysis$gpTab$name=="dataverse"]<-NA
  pkeys_sql<-sqlizeNames(pkeys)
  fkeys_sql<-sqlizeNames(fkeys)
  existingDBfields<-RPostgres::dbGetQuery(dvDb,"SELECT DISTINCT column_name,data_type FROM information_schema.columns WHERE table_schema='public'")
  #pkeys_sql %in% existingDBfields$column_name
  #fkeys_sql %in% existingDBfields$column_name
  existingDBfields[existingDBfields$column_name %in% pkeys | existingDBfields$column_name %in% fkeys,]

  (mVarGp<-match(listAnalysis$varTab$inGp,listAnalysis$gpTab$id))
  mVar<-match(listAnalysis$varTab$id,listAnalysis$analysis$id)
  table(factor(listAnalysis$varTab$inGp,levels=listAnalysis$gpTab$id,labels = listAnalysis$gpTab$name))

  tableAttrib<-vector(mode="list",length=nrow(listAnalysis$gpTab))
  names(tableAttrib)<-listAnalysis$gpTab$name

  types_pkeys <- rep("bigserial",length(pkeys))
  mpkeys<-match(pkeys,existingDBfields$column_name)
  types_pkeys[!is.na(mpkeys)]<-existingDBfields$data_type[mpkeys[!is.na(mpkeys)]]
  for(i in 1:length(tableAttrib))
  {
    tableAttrib[[i]]<-data.frame(id=NA,metadatablock=NA,name=pkeys[i],sql_name=pkeys_sql[i],type=types_pkeys[i],primarykey=T,reference=ifelse(pkeys[i]=="datasetversion_id"&listAnalysis$gpTab$name[i]!="datasetversion","datasetversion(datasetversion_id)",NA))
    if(!is.na(fkeys[i])&is.na(tableAttrib[[i]][1,"reference"]))
    {
      tableAttrib[[i]]<-rbind(tableAttrib[[i]],
                              data.frame(id=NA,metadatablock=NA,name=fkeys[i],sql_name=fkeys_sql[i],type="bigint",primarykey=F,reference=gsub("^(.*)(_id)$","\\1(\\1\\2)",fkeys[i])))
    }
  }


  res<-by(listAnalysis$analysis[mVar,],mVarGp,function(x)
  {
    data.frame(
      id=x$id,metadatablock=x$metadatablock,name=x$name,
      sql_name=sqlizeNames(x$name),
      type=c("TEXT"="text","DATE"="text","EMAIL"="text","INT"="integer","NONE"="text","TEXTBOX"="text","URL"="text")[x$fieldtype],# Note I changed dates to text because it makes trouble later when the dates entered are not real dates, but...
      primarykey=F,
      reference=NA
    )
  })

  tableAttrib[as.numeric(names(res))]<-mapply(rbind.data.frame,tableAttrib[as.numeric(names(res))],res,SIMPLIFY = F)



  tableAttrib$dataverse<-rbind(tableAttrib$dataverse,
                               data.frame(id=NA,metadatablock=NA,name="createdate",sql_name="createdate",type="timestamp",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="affiliation",sql_name="affiliation",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="alias",sql_name="alias",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="dataversetype",sql_name="dataversetype",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="description",sql_name="description",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="name",sql_name="name",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="dataversecontacts",sql_name="dataversecontacts",type="text",primarykey=F,reference=NA),
                               data.frame(id=NA,metadatablock=NA,name="dataversesubjects",sql_name="dataversesubjects",type="text",primarykey=F,reference=NA)
  )
  tableAttrib$dataset<-rbind(tableAttrib$dataset,
                             data.frame(id=NA,metadatablock=NA,name="createdate",sql_name="createdate",type="timestamp",primarykey=F,reference=NA),
                             data.frame(id=NA,metadatablock=NA,name="globalidcreatetime",sql_name="globalidcreatetime",type="timestamp",primarykey=F,reference=NA),
                             data.frame(id=NA,metadatablock=NA,name="modificationtime",sql_name="modificationtime",type="timestamp",primarykey=F,reference=NA),
                             data.frame(id=NA,metadatablock=NA,name="publicationdate",sql_name="publicationdate",type="timestamp",primarykey=F,reference=NA),
                             data.frame(id=NA,metadatablock=NA,name="doi",sql_name="doi",type="text",primarykey=F,reference=NA),
                             data.frame(id=NA,metadatablock=NA,name="metadatalanguage",sql_name="metadatalanguage",type="text",primarykey=F,reference=NA)
  )
  tableAttrib$datasetversion<-rbind(tableAttrib$datasetversion,
                                    data.frame(id=NA,metadatablock=NA,name="createtime",sql_name="createtime",type="timestamp",primarykey=F,reference=NA),
                                    data.frame(id=NA,metadatablock=NA,name="versionnumber",sql_name="versionnumber",type="integer",primarykey=F,reference=NA),
                                    data.frame(id=NA,metadatablock=NA,name="minorversionnumber",sql_name="minorversionnumber",type="integer",primarykey=F,reference=NA),
                                    data.frame(id=NA,metadatablock=NA,name="versionnote",sql_name="versionnote",type="text",primarykey=F,reference=NA),
                                    data.frame(id=NA,metadatablock=NA,name="versionstate",sql_name="versionstate",type="text",primarykey=F,reference=NA)
  )
  tableAttrib$datafile <- rbind(
    data.frame(id=NA,metadatablock=NA,name="datafile_id",sql_name="datafile_id",type="bigint",primarykey=T,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="dataset_id",sql_name="dataset_id",type="bigint",primarykey=F,reference="dataset(dataset_id)"),
    data.frame(id=NA,metadatablock=NA,name="createdate",sql_name="createdate",type="timestamp",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="modificationtime",sql_name="modificationtime",type="timestamp",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="publicationdate",sql_name="publicationdate",type="timestamp",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="storageidentifier",sql_name="storageidentifier",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="checksumtype",sql_name="checksumtype",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="checksumvalue",sql_name="checksumvalue",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="contenttype",sql_name="contenttype",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="filesize",sql_name="filesize",type="bigint",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="restricted",sql_name="restricted",type="boolean",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="previousdatafileid",sql_name="previousdatafileid",type="bigint",primarykey=F,reference="datafile(datafile_id)"),
    data.frame(id=NA,metadatablock=NA,name="rootdatafileid",sql_name="rootdatafileid",type="bigint",primarykey=F,reference="datafile(datafile_id)")
  )
  tableAttrib$filedescription <- rbind(
    data.frame(id=NA,metadatablock=NA,name="filedescription_id",sql_name="filedescription_id",type="bigserial",primarykey=T,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="datafile_id",sql_name="datafile_id",type="bigint",primarykey=F,reference="datafile(datafile_id)"),
    data.frame(id=NA,metadatablock=NA,name="datasetversion_id",sql_name="datasetversion_id",type="bigint",primarykey=F,reference="datasetversion(datasetversion_id)"),
    data.frame(id=NA,metadatablock=NA,name="version",sql_name="version",type="bigint",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="label",sql_name="label",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="description",sql_name="description",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="restricted",sql_name="restricted",type="boolean",primarykey=F,reference=NA)
  )
  tableAttrib$ingest <- rbind(
    data.frame(id=NA,metadatablock=NA,name="ingest_id",sql_name="ingest_id",type="bigserial",primarykey=T,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="datafile_id",sql_name="datafile_id",type="bigint",primarykey=F,reference="datafile(datafile_id)"),
    data.frame(id=NA,metadatablock=NA,name="ingeststatus",sql_name="ingeststatus",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="report",sql_name="report",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="status",sql_name="status",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="casequantity",sql_name="casequantity",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="varquantity",sql_name="varquantity",type="text",primarykey=F,reference=NA)
  )
  tableAttrib$variable <- rbind(
    data.frame(id=NA,metadatablock=NA,name="variable_id",sql_name="variable_id",type="bigserial",primarykey=T,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="datafile_id",sql_name="datafile_id",type="bigint",primarykey=F,reference="datafile(datafile_id)"),
    data.frame(id=NA,metadatablock=NA,name="name",sql_name="name",type="text",primarykey=F,reference=NA),
    data.frame(id=NA,metadatablock=NA,name="fileorder",sql_name="fileorder",type="int",primarykey=F,reference=NA)
  )
  names(tableAttrib)<-gsub("_$","",names(tableAttrib))
  return(tableAttrib)


}

createTableStatement<-function(nameTable, tabAttr, dbConnection, schema=NULL)
{
  isSerial<-grepl("serial",tabAttr$type,ignore.case=T)
  isDate<-tabAttr$type
  nameTable<-sqlizeNames(sub("_$","",nameTable))
  if(class(dbConnection)=="SQLiteConnection")
  {
    if(any(grepl("serial",tabAttr$type)&!tabAttr$primarykey))
    {stop("In SQLite, there is no simple possibility to use equivalent to serial types when not primary keys")}
    tabAttr$type <- c(int="integer", bigint="integer", smallint="integer", integer="integer", serial="integer", bigserial="integer", smallserial="integer", boolean="integer", date="text", timestamp="text", text="text", real="real", `double precision`="real")[tabAttr$type]
    tabAttr$type[is.na(tabAttr$type)]<-"blob"
  }
  if(class(dbConnection) %in% c("PqConnection", "PostgreSQLConnection"))
  {
    nameTable <- dbQuoteIdentifier(dbConnection, Id(schema = schema, table = nameTable))
    if(any(!is.na(tabAttr$reference)) & !is.null(schema))
    {
      refTables <- gsub("^ *(.+)\\((.+)\\)", "\\1", tabAttr$reference[!is.na(tabAttr$reference)])
      refFields <- gsub("^ *(.+)\\((.+)\\)", "\\2", tabAttr$reference[!is.na(tabAttr$reference)])
      tabAttr$reference[!is.na(tabAttr$reference)] <- paste0(sapply(refTables, function(x,s,db) dbQuoteIdentifier(dbConnection,Id(schema=s,table=x)), s=schema, db=dbConnection),"(",refFields,")")
    }
  }
  statement<-paste0("CREATE TABLE IF NOT EXISTS ", nameTable," (\n",
                    paste(tabAttr$sql_name, tabAttr$type, ifelse(tabAttr$primarykey,"PRIMARY KEY",""),
                          ifelse(rep(class(dbConnection)=="SQLiteConnection",nrow(tabAttr))&isSerial,"AUTOINCREMENT",""),
                          ifelse(!is.na(tabAttr$reference),paste("REFERENCES",tabAttr$reference),""),
                          collapse=",\n"),
                    "\n);")
  return(statement)
}








extractValues<-function(tableAttrib, dvDb)
{
  extracted<-vector(mode="list",length=length(tableAttrib))
  names(extracted) <- names(tableAttrib)

  extracted$dataverse=dbGetQuery(conn=dvDb,
                                 "SELECT d.id dataverse_id, createdate,affiliation,alias,dataversetype,description,name,  dataversecontacts, dataversesubjects
FROM dataverse d
LEFT JOIN dvobject dvo ON d.id=dvo.id
LEFT JOIN (SELECT dataverse_id,STRING_AGG(contactemail,'|' ORDER BY displayorder) dataversecontacts FROM dataversecontact GROUP BY dataverse_id) dc ON d.id=dc.dataverse_id
LEFT JOIN (SELECT dataverse_id,  STRING_AGG(DISTINCT strvalue,'|') dataversesubjects FROM dataversesubjects ds LEFT JOIN controlledvocabularyvalue ccv ON ds.controlledvocabularyvalue_id=ccv.id GROUP BY dataverse_id)  ds ON d.id=ds.dataverse_id"
  )

  extracted$dataset<-dbGetQuery(conn=dvDb,
                                "SELECT id dataset_id, owner_id dataverse_id, createdate, globalidcreatetime, modificationtime, publicationdate, authority || '/' || identifier doi, metadatalanguage
FROM dataset
LEFT JOIN dvobject USING (id)
ORDER BY dataset_id")


  extracted$datasetversion<-dbGetQuery(conn=dvDb,
                                       "SELECT id datasetversion_id, dataset_id, createtime, versionnumber,minorversionnumber,versionnote,versionstate
FROM datasetversion
ORDER BY dataset_id, createtime")

  extracted$ingest<-dbGetQuery(conn=dvDb,"
SELECT df.id datafile_id, ingeststatus, report,status,casequantity,varquantity
FROM datafile df
LEFT JOIN ingestreport ir ON df.id=ir.datafile_id
LEFT JOIN datatable dt ON df.id=dt.datafile_id")
  extracted$datafile<-dbGetQuery(conn=dvDb,
                                 "SELECT id datafile_id, owner_id dataset_id, createdate, modificationtime, publicationdate, storageidentifier, checksumtype, checksumvalue, contenttype, filesize,  restricted,  CASE WHEN previousdatafileid IS NULL THEN id ELSE previousdatafileid END previousdatafileid, CASE WHEN rootdatafileid=-1 THEN id ELSE id END rootdatafileid
FROM datafile df
LEFT JOIN dvobject dvo USING (id)")

  extracted$filedescription<-dbGetQuery(conn=dvDb,
                                        "SELECT datafile_id, datasetversion_id, version, label, description, restricted
FROM filemetadata")

  extracted$variable<-dbGetQuery(conn=dvDb,"
SELECT datafile_id, name, fileorder
FROM datavariable dv
LEFT JOIN datatable dt ON dv.datatable_id=dt.id
LEFT JOIN datafile df ON dt.datafile_id=df.id")

  allValuesBiocultural<-dbGetQuery(dvDb,
                                   "
WITH a AS(
SELECT dft.id,dfv.value,df.parentdatasetfieldcompoundvalue_id,COALESCE(df.datasetversion_id,pdf.datasetversion_id) datasetversion_id
FROM datasetfieldvalue dfv
LEFT JOIN datasetfield df ON dfv.datasetfield_id=df.id
LEFT JOIN datasetfieldtype dft ON df.datasetfieldtype_id=dft.id
LEFT JOIN datasetfieldcompoundvalue dfcv ON df.parentdatasetfieldcompoundvalue_id=dfcv.id
LEFT JOIN datasetfield pdf ON dfcv.parentdatasetfield_id=pdf.id
UNION
SELECT dft.id,cvc.strvalue,df.parentdatasetfieldcompoundvalue_id,COALESCE(df.datasetversion_id,pdf.datasetversion_id)
FROM datasetfield_controlledvocabularyvalue dfcvv
LEFT JOIN datasetfield df ON dfcvv.datasetfield_id=df.id
LEFT JOIN datasetfieldtype dft ON df.datasetfieldtype_id=dft.id
LEFT JOIN controlledvocabularyvalue cvc ON dfcvv.controlledvocabularyvalues_id=cvc.id
LEFT JOIN datasetfieldcompoundvalue dfcv ON df.parentdatasetfieldcompoundvalue_id=dfcv.id
LEFT JOIN datasetfield pdf ON dfcv.parentdatasetfield_id=pdf.id
)
SELECT *
FROM a
WHERE value IS NOT NULL AND value != '' AND datasetversion_id IS NOT NULL"
  )

un_gp <- unique(allValuesBiocultural[,c("parentdatasetfieldcompoundvalue_id","datasetversion_id")])
gps <- allValuesBiocultural[,c("parentdatasetfieldcompoundvalue_id","datasetversion_id")]
allValuesBiocultural$ref_un_gp<-match(split(as.matrix(gps),row(gps)),split(as.matrix(un_gp),row(un_gp)))

extractMetaData <- tableAttrib[ sapply(tableAttrib,function(tab) sum(!is.na(tab$id)) > 0) ]
names(extractMetaData) <- names(tableAttrib) [ sapply(tableAttrib,function(tab) sum(!is.na(tab$id)) > 0) ]

for(i in names(extractMetaData))
{
  tabExtract<-extractMetaData[[i]]
  val<-allValuesBiocultural[ allValuesBiocultural$id %in% tabExtract$id ,]
  resMat<-matrix(NA,nrow=length(unique(val$ref_un_gp)),ncol=length(unique(val$id)),dimnames=list(unique(val$ref_un_gp),unique(val$id)))
  resMat[cbind(row = match(val$ref_un_gp,rownames(resMat)), col = match(val$id,colnames(resMat)))] <- val$value
  extracted[[i]]<-data.frame(datasetversion_id=un_gp$datasetversion_id[as.numeric(rownames(resMat))],resMat)
  colnames(extracted[[i]])<-c("datasetversion_id",tabExtract$sql_name[match(as.numeric(colnames(resMat)),tabExtract$id)])
  extracted[[i]]<-extracted[[i]][order(extracted[[i]]$datasetversion_id),,drop=F]
}

return(extracted)
}

# extracted<-extractedBiocultural
# dbConnection<-meta_i2d
# tableAttrib <- descriTables
# schema<-"biocultural"

insertTables <- function(extracted, dbConnection, tableAttrib,schema=NULL)
{
  if(is(dbConnection,"SQLiteConnection"))
  {
    for(i in 1:length(extracted)){
      ts <- tableAttrib[[i]]$sql_name[tableAttrib[[i]]$type == "timestamp"]
      if(length(ts)>0){
        for(j in 1:length(ts)){
          extracted[[i]][ts[j]]<-as.character(extracted[[i]][,ts[j]])
        }
      }
      ts <- tableAttrib[[i]]$sql_name[tableAttrib[[i]]$type == "date"]
      if(length(ts)>0){
        for(j in 1:length(ts)){
          extracted[[i]][,ts[j]]<-as.character(extracted[[i]][,ts[j]])
        }
      }
    }
  }
  tableNames <- sqlizeNames(sub("_$","",names(tableAttrib)))
  temp_tables <- paste0("temp_",tableNames)
  for(i in (1:length(extracted))[!sapply(extracted,is.null)]){
    dbBegin(dbConnection)
    tablename<-tableNames[i]
    temp_table<-temp_tables[i]
    fieldsInExtracted <- match( colnames(extracted[[i]]), tableAttrib[[i]]$sql_name)
    if(is(dbConnection,"PqConnection")|is(dbConnection, "PostgreSQLConnection"))
    { selectFields <- paste(tableAttrib[[i]]$sql_name[fieldsInExtracted],tableAttrib[[i]]$type[fieldsInExtracted],sep="::",collapse=", ") } else { selectFields= paste(tableAttrib[[i]]$sql_name[fieldsInExtracted],collapse=", ")}
    dbWriteTable(dbConnection,name = dbQuoteIdentifier(dbConnection,Id(schema=schema,table=temp_table)),extracted[[i]],overwrite=T)
    insertStatement <- paste0("INSERT INTO ",dbQuoteIdentifier(dbConnection,Id(schema=schema,table=tablename))," (",paste(colnames(extracted[[i]]),collapse=", "),") SELECT ", selectFields," FROM ",dbQuoteIdentifier(dbConnection,Id(schema=schema, table=temp_table)),";")
    insertStatementSend<-dbSendStatement(dbConnection,insertStatement)
    dbClearResult(insertStatementSend)
    dbExecute(dbConnection,paste0("DROP TABLE ",dbQuoteIdentifier(dbConnection,Id(schema=schema,table=temp_table))," ;"))
    dbCommit(dbConnection)
  }
}
