

#' Extract indexes of an xml extraction from bash functions
#'
#' @param text_doc_xml character vector including the xml content, and separator (see grep_name)
#' @param grep_name regex expected to match complete elements to be considered as separators between files, a part of the regex should be considered as the name of the elements
#' @param name_part part of the separator regex that should be extracted as name (see second argument of sub)
#' @param verbose should the function give information during execution
#'
#' @returns data.frame with names, begin and end indexed in the character vector containing the xml content
#' @export
#'
extractAdressesMultiXml <- function(text_doc_xml,grep_name="^---file:(.*)---$",name_part="\\1",verbose=T)
{
  sepFiles <- grep("---file:.*---",text_doc_xml)
  if(verbose)
  {
    cat("Number of elements:",length(sepFiles))
  }
  res<-data.frame(
    name=sub(grep_name,name_part,text_doc_xml[sepFiles]),
    beg=sepFiles+1,
    end=c(sepFiles[2:length(sepFiles)]-1,length(text_doc_xml))
  )
  pbs<-which(res$beg>res$end)
  if(length(pbs))
  {
    warning("Some xml documents appear to be empty (we will not consider them):", paste(res$name[pbs],collapse="\n"))
  }
  if(length(pbs)){return(res[-pbs,])}
  return(res)
}

#' Navigate a list of list
#'
#' @param metaList List of list (as those exported by the XML::xmlToList function)
#' @param numpath numeric vector containing, for each level the index of the element to navigate
#'
#' @returns gives the list or element of list corresponding to the successive indexes given in numpath
#' @export
#'
navMetaList <- function(metaList,numpath)
{
  x=metaList
  for(i in numpath)
    x <- x[[i]]
  return(x)
}


#' @rdname navMetaList
#' @export
#'
nextLevelLength <- function(metaList,numpath)
{
  length(navMetaList(metaList,numpath))
}

#' @rdname navMetaList
#' @export
nextLevelNames <- function(metaList,numpath)
{
  names(navMetaList(metaList,numpath))
}

appendRepListVectorElements<-function(l,toAdd)
{
  stopifnot(length(l)==length(toAdd))
  n<-sapply(toAdd,length)
  return(mapply(append,rep(l,n),unlist(toAdd),SIMPLIFY=F))
}

childrenPath <- function(path,l)
{
  appendRepListVectorElements(path,lapply(l,function(x)1:x))
}

numRep<-function(x)
{
  x<-factor(x)
  un<-levels(x)
  m<-match(x,un)
  t<-table(x)
  res<-integer(length(x))
  res[order(m)]<-unlist(lapply(t,function(x)1:x))
  return(res)
}

extractStructureListDocuments<-function(listDocument,goFurther=c("list","XMLAttributes"))
{
  #initialization
  currentList<-listDocument
  LEV<-0
  classes<-currentClasses<-sapply(currentList,class,USE.NAMES = F)
  listPath<-lapply(1:length(currentList),function(x)x)
  listNames<-as.list(names(currentList))
  currentW<-1:length(currentList)
  #listNames<-c(listNames,appendRepListVectorElements(listNames,namesChildren))
  nChildren<-sapply(currentList,length, USE.NAMES = F)
  levStruct<-rep(LEV,length(currentList))
  parents<-rep(0,length(currentList))
  keepGoing<-currentClasses%in%goFurther
  directChildren<-mapply(function(x,y,s)s+seq(y,x),A<-cumsum(nChildren[keepGoing]),c(1,A[-length(A)]+1),s=length(listPath),SIMPLIFY = F)
  num_rep<-rep(NA,length(currentList))
  null_val<-NULL
  val<-NULL
  while(sum(keepGoing)>0)
  {
    LEV<-LEV+1
    parentW<-currentW
    currentW<-(max(parentW)+1):(max(parentW)+sum(nChildren[keepGoing]))
    parents<-c(parents,rep(parentW[keepGoing],nChildren[keepGoing]))
    #add Paths
    listPath<-c(listPath,childrenPath(listPath[parentW][keepGoing],nChildren[keepGoing]))
    #Add names
    finalCurrentNames<-lapply(currentList[keepGoing],names)
    ## DupliNames
    num_rep<-c(num_rep,unlist(lapply(finalCurrentNames,function(x)
    {
      res<-rep(NA,length(x))
      if(!anyDuplicated(x)){return(res)}
      dupliNames<-unique(x[duplicated(x)])
      m<-match(x,dupliNames)
      res[!is.na(m)]<-numRep(x[!is.na(m)])
      return(res)
    })))
    listNames<-c(listNames,appendRepListVectorElements(listNames[parentW][keepGoing],finalCurrentNames))
    # Change currentList
    currentList<-unlist(currentList[keepGoing],recursive=F,use.names = F)
    nChildren<-sapply(currentList,length)
    # Following classes
    currentClasses<-sapply(currentList,class)
    keepGoing<-currentClasses%in%goFurther
    ##Correcting it: when there is a name in the element, we keep going
    keepGoing[!keepGoing][!sapply(lapply(currentList[!keepGoing],names),is.null)]<-T
    nChildren[!keepGoing]<-0

    if(length(currentClasses))
    {classes<-c(classes,currentClasses)}

    levStruct<-c(levStruct,rep(LEV,length(currentList)))
    directChildren<-c(directChildren,
                      mapply(function(nC,x,y,s){
                        if(nC==0){return(NULL)}
                        if(nC==1){return(s+y)}
                        return(s+seq(y,x))
                      },nChildren,A<-cumsum(nChildren),c(1,A[-length(A)]+1),s=length(listPath),SIMPLIFY = F))
    if(any(!keepGoing))
    {
      cNull<-sapply(currentList[!keepGoing],is.null)
      null_val<-c(null_val,currentW[!keepGoing][cNull])
      val<-c(val,currentW[!keepGoing][!cNull])
    }
    stopifnot(length(unique(
      c(length(parents),length(listPath),length(listNames),length(directChildren),length(levStruct),length(classes),length(num_rep))
    ))==1)
  }
  return(list(levs=unique(levStruct),levStruct=levStruct,paths=listPath,listNames=listNames,parents=parents,directChildren=directChildren,classes=classes,val=val,null_val=null_val,num_rep=num_rep))
}

listWord2finalNames<-function(lW,gp,max_nbW=max(sapply(lW,length)),reservedNames=c("string","name","int","text","class","para"))
{
  gp<-factor(gp)
  initNames<-finalNames<-sapply(lW,function(x)x[length(x)])
  rpt<-tapply(initNames,gp,function(x)
    unique(x[duplicated(x)|x%in%reservedNames]))
  repeatedInGp<-data.frame(gp=rep(levels(gp),sapply(rpt,length)),
                           repNames=unlist(rpt))
  nbW<-1
  while(nrow(repeatedInGp)&nbW<=max_nbW)
  {
    nbW<-nbW+1
    concerned<-unlist(apply(repeatedInGp,1,function(x,g,w)which(g==x[1]&w==x[2]),g=gp,w=finalNames,simplify = F))
    possibleEvol<-concerned[sapply(lW[concerned],length)>=nbW]
    finalNames[possibleEvol]<-sapply(lW[possibleEvol],function(x,n)paste(x[(length(x)-(nbW-1)):length(x)],collapse="_"))
    rpt<-tapply(finalNames,gp,function(x)unique(x[duplicated(x)]))
    repeatedInGp<-data.frame(gp=rep(levels(gp),sapply(rpt,length)),
                             repNames=unlist(rpt))
  }
  if(nrow(repeatedInGp))
  {
    warning("We did not find a way to extract unique names in groups")
    print(repeatedInGp)
  }
  return(finalNames)
}

allChildren<-function(id,dChildren,includeSelf=F)
{
  res<-newChildren<-if (includeSelf) id else dChildren[[id]]
  while(length(newChildren))
  {
    newChildren<-unlist(dChildren[newChildren])
    res<-c(res,newChildren)
  }
  return(res)
}

groupsAndVariables<-function(extract_struct,uniqueGpNames=T,gp0="xml_doc",simplify_var_id=T)
{
  #Definition vars and gps
  allVarNames<-lapply(extract_struct$listNames[extract_struct$val],function(x)x[2:length(x)])
  varNames<-unique(allVarNames)
  varLeaves<-integer(length(extract_struct$paths))
  varLeaves[extract_struct$val]<-match(allVarNames,varNames)
  allGpNames<-lapply(extract_struct$listNames[!is.na(extract_struct$num_rep)],function(x)x[2:length(x)])
  gpNames<-unique(allGpNames)
  #update gps and num_rep
  gps<-match(lapply(extract_struct$listNames,function(x)x[2:length(x)]),gpNames)
  num_rep_final<-extract_struct$num_rep
  num_rep_final[is.na(num_rep_final)&!is.na(gps)]<-1
  # gives the groups to all children of not NA num_rep (but maybe wait to give the hierarchy (to every case which is a leaf only) as name of group + enumeration)

  #Relationships between gps and var
  levGp<-unique(sapply(gpNames,length))
  levGp<-levGp[order(levGp)]
  ging<-rep(0,length(gpNames))
  ving<-rep(0,length(varNames))
  for(i in levGp)
  {
    mg<-match(lapply(gpNames,function(x,ii)
    {
      if(length(x)<=ii){return(NA)}
      return(x[1:ii])
    },ii=i),gpNames)
    ging[!is.na(mg)]<-mg[!is.na(mg)]
    mv<-match(lapply(varNames,function(x,ii)
    {
      if(length(x)<ii){return(NA)}
      return(x[1:ii])
    },ii=i),gpNames)
    ving[!is.na(mv)]<-mv[!is.na(mv)]
  }
  #names of variables and groups
  finalGpNames<-listWord2finalNames(gpNames,if(uniqueGpNames)rep(1,length(gpNames)) else ging)
  finalVarNames<-listWord2finalNames(varNames,ving)

  # Creating gp Ids
  idGpParentsGp<-vector(mode="list",sum(!is.na(gps)))
  idGpParentsVar<-vector(mode="list",length(extract_struct$val))
  ##Gp0
  allChildrenDoc<-lapply(which(extract_struct$levStruct==0),allChildren,dChildren=extract_struct$directChildren)
  mG<-lapply(lapply(allChildrenDoc,match,table=which(!is.na(gps))),na.omit)
  for(i in 1:length(mG)){
    elementsToAdd<-i
    names(elementsToAdd)<-gp0
    idGpParentsGp[mG[[i]]]<-lapply(idGpParentsGp[mG[[i]]],append,elementsToAdd)
  }
  mV<-lapply(lapply(allChildrenDoc,match,table=extract_struct$val),na.omit)
  for(i in 1:length(mV)){
    elementsToAdd<-i
    names(elementsToAdd)<-gp0
    idGpParentsVar[mV[[i]]]<-lapply(idGpParentsVar[mV[[i]]],append,elementsToAdd)
  }
  ##other groups
  all_children<-lapply(1:length(extract_struct$paths),allChildren,extract_struct$directChildren,includeSelf=T)
  cbind(which(!is.na(gps))[sapply(all_children[which(!is.na(gps))],length)==1],all_children[!is.na(gps)][sapply(all_children[!is.na(gps)],length)==1])
  gpInGpId<-na.omit(data.frame(parentId=rep(which(!is.na(gps)),sapply(all_children[!is.na(gps)],length)),
                               gp=rep(gps[!is.na(gps)],sapply(all_children[!is.na(gps)],length)),
                               gpName=rep(finalGpNames[gps[!is.na(gps)]],sapply(all_children[!is.na(gps)],length)),
                               num=rep(num_rep_final[!is.na(gps)],sapply(all_children[!is.na(gps)],length)),
                               childGp=match(unlist(all_children[!is.na(gps)]),which(!is.na(gps)))
  ))
  valInGpId<-na.omit(data.frame(parentId=rep(which(!is.na(gps)),sapply(all_children[!is.na(gps)],length)),
                                gp=rep(gps[!is.na(gps)],sapply(all_children[!is.na(gps)],length)),
                                gpName=rep(finalGpNames[gps[!is.na(gps)]],sapply(all_children[!is.na(gps)],length)),
                                num=rep(num_rep_final[!is.na(gps)],sapply(all_children[!is.na(gps)],length)),
                                childVal=match(unlist(all_children[!is.na(gps)]),extract_struct$val)
  ))
  invisible(by(gpInGpId,gpInGpId$childGp,function(x){
    elt<-unique(x$childGp)
    toAdd<-x$num
    names(toAdd)<-x$gpName
    idGpParentsGp[[elt]]<<-append(idGpParentsGp[[elt]],toAdd)
  }))
  invisible(by(valInGpId,valInGpId$childVal,function(x){
    elt<-unique(x$childVal)
    toAdd<-x$num
    names(toAdd)<-x$gpName
    idGpParentsVar[[elt]]<<-append(idGpParentsVar[[elt]],toAdd)
  }))
  ## Create the list of tables to manage variables and groups
  gpsNotNa<-gps[!is.na(gps)]
  listVarId<-listGpId<-list()
  for(i in 1:length(finalGpNames))
  {
    #lastCol<-matrix(num_rep_final[!is.na(gps)&gps==i],ncol=1,dimnames=list(NULL,finalGpNames[i]))
    listGpId[[i]]<-Reduce(rbind,idGpParentsGp[gpsNotNa==i],simplify = F)
    rownames(listGpId[[i]])<-which(gps==i)
  }
  names(listGpId)<-finalGpNames
  vl<-varLeaves[extract_struct$val]
  for(i in 1:length(finalVarNames))
  {

    listVarId[[i]]<-Reduce(rbind,idGpParentsVar[vl==i],simplify = F)
    if(sum(vl==i)==1&!is.matrix(listVarId[[i]])){listVarId[[i]]<-matrix(listVarId[[i]],nrow=1,dimnames=list(NULL,names(listVarId[[i]])))}
    rownames(listVarId[[i]])<-which(varLeaves==i)
  }
  names(listVarId)<-finalVarNames
  #Group and var hierarchy
  gpHier<-sapply(listGpId,function(x)paste(colnames(x),collapse="/"))
  varHier<-paste(sapply(listVarId,function(x)paste(colnames(x),collapse = "/")),names(listVarId),sep="/")
  gpHier_un<-sapply(listGpId,function(x,tab){
    numGp<-match(colnames(x),tab)
    numGp[is.na(numGp)]<-0
    return(paste(paste0("gp_",numGp),collapse="/"))
  }
  ,tab=finalGpNames)
  varHier_un<-paste(sapply(ving,function(x,y)if(x==0)"gp_0"else y[x],y=gpHier_un),paste("var",1:length(ving),sep="_"),sep="/")
  # simplifying listVarId
  if(simplify_var_id){
    s_listVarId<-list()
    for(i in 1:length(listVarId))
    {
      inGp<-ving[i]
      if(inGp==0)
      {
        s_listVarId[[i]]<-listVarId[[i]][,1]
      }else{
        s_listVarId[[i]]<-match(split(listVarId[[i]],row(listVarId[[i]])),split(listGpId[[inGp]],row(listGpId[[inGp]])))
        names(s_listVarId[[i]])<-rownames(listVarId[[i]])
      }
    }
    names(s_listVarId)<-names(listVarId)
  }
  # Formatting and returning results:
  res<-list(finalGpNames=finalGpNames,gp0=gp0,finalVarNames=finalVarNames,gpInGp=ging,
            varInGp=ving,gpNames=gpNames,varNames=varNames,gpHier=gpHier,
            varHier=varHier,gpId=listGpId,varHier_un=varHier_un,gpHier_un=gpHier_un)
  if(simplify_var_id) res$varId<-s_listVarId else
    res$varId<-listVarId
  return(res)
}

require(data.tree)
require(igraph)
plotGroupsAndVariables<-function(gpsAndVar,...)
{
  dfPath<-rbind(
    data.frame(pathString=as.character(gpsAndVar$gpHier_un), label=gpsAndVar$finalGpNames, gpVar="gp",
               un_names=paste0("gp_",1:length(gpsAndVar$gpHier_un)), inGp=gpsAndVar$gpInGp,
               gpCol=1:length(gpsAndVar$gpHier_un)),
    data.frame(pathString=as.character(gpsAndVar$varHier_un),label=gpsAndVar$finalVarNames,gpVar="var",
               un_names=paste0("var_",1:length(gpsAndVar$varHier_un)), inGp=gpsAndVar$varInGp,
               gpCol=gpsAndVar$varInGp)
  )
  if(any(dfPath$label%in%NODE_RESERVED_NAMES_CONST))
  {
    dfPath$label[dfPath$label%in%NODE_RESERVED_NAMES_CONST]<-paste0(
      dfPath$label[dfPath$label%in%NODE_RESERVED_NAMES_CONST],"_")
  }
  net<-as.igraph(as.Node(dfPath,"pathString","label","gpVar"))
  m<-match(names(V(net)),dfPath$un_names)
  V(net)$label<-NA
  V(net)$label[1]<-gpsAndVar$gp0
  V(net)$label[!is.na(m)]<-dfPath$label[na.omit(m)]
  V(net)$gpCol<-NA
  V(net)$gpCol[1]<-0
  V(net)$gpCol[!is.na(m)]<-dfPath$gpCol[na.omit(m)]
  V(net)$gpVar<-NA
  V(net)$gpVar[1]<-"gp"
  V(net)$gpVar[!is.na(m)]<-dfPath$gpVar[na.omit(m)]

  plot(net,vertex.label=V(net)$label,vertex.size=5,
       vertex.shape=c(var="circle",gp="square")[V(net)$gpVar],
       vertex.color=rainbow(max(V(net)$gpCol+1))[V(net)$gpCol+1],
       ...)
}
#dat<-xml_list_gn
#struct<-structGn
#gpsAndVar<-gnv_gn
extractTables<-function(dat,struct,gpsAndVar,convertMode=T,noConvert=c("emlVersion","replacedEmlVersion","version"))
{
valVar<-lapply(gpsAndVar$varId,
               function(x,d,s)
                 sapply(s[as.numeric(names(x))],
                        function(ss)navMetaList(d,ss))
               ,d=dat,s=struct$paths)
if(convertMode){
  nC<-names(valVar)%in%noConvert
  valVar[!nC]<-type.convert(valVar[!nC],as.is=T)

  }
res<-list(data=NULL,info=NULL)
res$data<-varTab<-listTab<-vector(mode="list",length=length(gpsAndVar$finalGpNames)+1)
names(res$data)<-names(varTab)<-names(listTab)<-c(gpsAndVar$gp0,gpsAndVar$finalGpNames)
varInTab<-tapply(gpsAndVar$finalVarNames,
                 factor(gpsAndVar$varInGp,levels=0:length(gpsAndVar$finalGpNames)),
                 function(x)x)
varTab[as.numeric(names(varInTab))+1]<-varInTab
pk<-paste0("cd_",c(gpsAndVar$gp0,gpsAndVar$finalGpNames))
fk<-c(NA,pk[gpsAndVar$gpInGp+1])
nrow<-sapply(gpsAndVar$gpId,nrow)
idGp0<-matrix(1:length(dat),ncol=1,dimnames=list(NULL,gpsAndVar$gp0))
forPK_FK<-append(gpsAndVar$gpId,list(idGp0),0)
for(i in 1:length(res$data))
{
  PK<-1:nrow(forPK_FK[[i]])
  matVar<-matrix(NA,nrow=length(PK),ncol=length(varTab[[i]]),dimnames=list(NULL,varTab[[i]]))
  for(j in which(gpsAndVar$varInGp==(i-1)))
  {
    if(length(valVar[[j]])>0){
    var<-gpsAndVar$finalVarNames[j]
    matVar[gpsAndVar$varId[[j]],var]<-valVar[[j]]}
  }
  if(!is.na(fk[i]))
  {
    curMat<-forPK_FK[[i]][,-ncol(forPK_FK[[i]]),drop=F]
    refMat<-forPK_FK[[gpsAndVar$gpInGp[i-1]+1]]
    FK=match(split(curMat,row(curMat)),split(refMat,row(refMat)))
    keys<-as.data.frame(matrix(c(PK,FK),ncol=2,dimnames=list(NULL,c(pk[i],fk[i]))))
  }else{keys=as.data.frame(matrix(PK,ncol=1,dimnames=list(NULL,pk[i])))}
  res$data[[i]]<-as.data.frame(cbind(keys,matVar))
}
res$info$tab<-data.frame(
  tabname=c(gpsAndVar$gp0,gpsAndVar$finalGpNames),
  primarykey=pk,
  foreigntable=c(gpsAndVar$gp0,gpsAndVar$finalGpNames)[c(NA,gpsAndVar$gpInGp+1)],
  foreignref=fk,
  nbRow=sapply(res$data,nrow),
  nbRowSinVal=sapply(res$data,function(x,y)sum(apply(x[!colnames(x)%in%y],1,function(x)sum(is.na(x))==length(x))),y=pk),
  pathSimp=c(gpsAndVar$gp0,gpsAndVar$gpHier),
  pathTot=c(gpsAndVar$gp0,sapply(gpsAndVar$gpNames,paste,collapse="|"))
)
res$info$var<-data.frame(
  varname=gpsAndVar$finalVarNames,
  tabname=c(gpsAndVar$gp0,gpsAndVar$finalGpNames)[gpsAndVar$varInGp+1],
  occurrences=sapply(valVar,length),
  mode=sapply(valVar,mode),
  pathSimp=gpsAndVar$varHier,
  pathTot=sapply(gpsAndVar$varNames,paste,collapse="|")
)

return(res)
}

require(RSQLite)

createTableFK_statement<-function(conn,tabName,fields,types,pk,foreignTable,foreignRef,listConstraint=list(),schema=NULL)
{
  foreignExpression<-rep(NA,length(fields))
  okForeign<-(!is.na(foreignTable)&!is.na(foreignRef))
  if(any(okForeign)){
  foreignExpression[okForeign]<-paste0("REFERENCES ", dbQuoteIdentifier(conn,Id(schema=schema,table=foreignTable[okForeign]))
                                              ,"(",dbQuoteIdentifier(conn,foreignRef[okForeign]),")")
  }
  additionalConstraints<-""
  if(length(listConstraint)){
    additionalConstraints=paste(",",listConstraint,collapse=", ")
  }
  paste0("CREATE TABLE ",dbQuoteIdentifier(conn,Id(schema=schema,table=tabName))," (",
         paste0(
         dbQuoteIdentifier(conn,fields), " " , types, " ",
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
sqlizeNames<-function(x,maxBytes=55)
{
  s1 <- gsub("^[-_.0-9]*","",gsub("\\_?([A-Z]{1,3})","_\\L\\1",gsub("^([A-Z]+)","\\L\\1",x,perl=T),perl=T))
  s2 <- gsub("\\.","",s1,perl=T)
  s3 <- strsplit(s2,'_')
  ctBytes<-lapply(lapply(s3,nchar,type='bytes'),function(x)cumsum(x[length(x):1]+1)[length(x):1])
  return(mapply(function(x,y)paste(x[y],collapse="_"),s3,lapply(ctBytes,function(x,m)x<m,m=maxBytes)))
}

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


exportPostgres<-function(extractedTables,conn,schema=NULL,overwrite=T,createFKindices=T)
{
  dbBegin(conn)
  #Does the schema exist
  schemaExists <- schema %in% dbGetQuery(conn,"SELECT schema_name FROM information_schema.schemata")$schema_name
  if(schemaExists)
  {
    if(overwrite)
    {
      dbExecute(conn,paste0("DROP SCHEMA ", schema, " CASCADE"))
    }else{
      stop("The schema ",schema, "already exists, and the parameter overwrite is set to FALSE")
    }
  }
  dbExecute(conn,paste0("CREATE SCHEMA ",schema))
  # Create info Tables
  ## tabInfo
  stat<-createTableFK_statement(conn,tabName = "tabinfo",
                                fields=colnames(extractedTables$info$tab),
                                types=dbDataType(conn,extractedTables$info$tab),
                                pk=(colnames(extractedTables$info$tab)=="tabname"),
                                foreignTable=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabinfo",NA),
                                foreignRef=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabname",NA),
                                schema=schema
  )
  dbExecute(conn,stat)
  ## varinfo
  stat<-createTableFK_statement(conn,tabName="varinfo",
                                fields=colnames(extractedTables$info$var),
                                types=dbDataType(conn,extractedTables$info$var),
                                pk=rep(FALSE,ncol(extractedTables$info$var)),
                                foreignTable=ifelse(colnames(extractedTables$info$var)=="tabname","tabinfo",NA),
                                foreignRef=ifelse(colnames(extractedTables$info$var)=="tabname","tabname",NA),
                                listConstraint=list("PRIMARY KEY (tabname, varname)"),
                                schema=schema
  )
  dbExecute(conn,stat)
  # Add info
  dbAppendTable(conn,Id(schema=schema,table="tabinfo"),extractedTables$info$tab)
  dbAppendTable(conn,Id(schema=schema,table="varinfo"),extractedTables$info$var)
  # Create metadata tables
  for(i in 1:nrow(extractedTables$info$tab))
  {
    tab<-extractedTables$info$tab$tabname[i]
    vars<-extractedTables$info$var$varname[extractedTables$info$var$tabname==tab]
    pkey<-extractedTables$info$tab$primarykey[i]
    ft<-extractedTables$info$tab$foreigntable[i]
    fkey<-extractedTables$info$tab$foreignref[i]
    fields<-c(pkey,na.omit(fkey),vars)
    stopifnot(fields==colnames(extractedTables$data[[tab]]))
    stat<-createTableFK_statement(conn,
                                  tabName=tab,
                                  fields=fields,
                                  types=dbDataType(conn,extractedTables$data[[tab]]),
                                  pk=(fields==pkey),
                                  foreignTable=ifelse(fields==fkey,ft,NA),
                                  foreignRef=ifelse(fields==fkey,fkey,NA),
                                  schema=schema
    )
    dbExecute(conn,stat)
    dbAppendTable(conn,Id(schema=schema,table=tab),value=extractedTables$data[[tab]])
    if(createFKindices&sum(fields==fkey,na.rm = T)>0){
      dbExecute(conn,paste0("CREATE INDEX fk_",tab,"_",ft,"_idx ON ",dbQuoteIdentifier(conn,Id(schema=schema,table=tab)),"(",na.omit(fields[fields==fkey]),")",collapse=" ; "))}
  }
  return(dbCommit(conn))
}

exportSQLite<-function(extractedTables,sqlite_file,overwrite=T,saveBAK=NULL,createFKindices=T){
  fExist<-file.exists(sqlite_file)
  if(fExist){
    if(overwrite){
      if(!is.null(saveBAK)){
        file.rename(sqlite_file,saveBAK)
      }else{file.remove(sqlite_file)}
    }else{stop("SQLite file exists, use overwrite if you want to remove it")}
  }
  db<-dbConnect(SQLite(),sqlite_file)
  # Create info Tables
  ## tabInfo
  stat<-createTableFK_statement(db,tabName = "tabinfo",
                          fields=colnames(extractedTables$info$tab),
                          types=dbDataType(db,extractedTables$info$tab),
                          pk=(colnames(extractedTables$info$tab)=="tabname"),
                          foreignTable=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabinfo",NA),
                          foreignRef=ifelse(colnames(extractedTables$info$tab)=="foreigntable","tabname",NA)
  )
  dbExecute(db,stat)
  ## varinfo
  stat<-createTableFK_statement(db,tabName="varInfo",
                          fields=colnames(extractedTables$info$var),
                          types=dbDataType(db,extractedTables$info$var),
                          pk=rep(FALSE,ncol(extractedTables$info$var)),
                          foreignTable=ifelse(colnames(extractedTables$info$var)=="tabname","tabinfo",NA),
                          foreignRef=ifelse(colnames(extractedTables$info$var)=="tabname","tabname",NA),
                          listConstraint=list("PRIMARY KEY (tabname, varname)")
  )
  dbExecute(db,stat)
  # Add info
  dbAppendTable(db,"tabinfo",extractedTables$info$tab)
  dbAppendTable(db,"varinfo",extractedTables$info$var)
  # Create metadata tables
  for(i in 1:nrow(extractedTables$info$tab))
  {
    tab<-extractedTables$info$tab$tabname[i]
    vars<-extractedTables$info$var$varname[extractedTables$info$var$tabname==tab]
    pkey<-extractedTables$info$tab$primarykey[i]
    ft<-extractedTables$info$tab$foreigntable[i]
    fkey<-extractedTables$info$tab$foreignref[i]
    fields<-c(pkey,na.omit(fkey),vars)
    stopifnot(fields==colnames(extractedTables$data[[tab]]))
    stat<-createTableFK_statement(db,
                                  tabName=tab,
                                  fields=fields,
                                  types=dbDataType(db,extractedTables$data[[tab]]),
                                  pk=(fields==pkey),
                                  foreignTable=ifelse(fields==fkey,ft,NA),
                                  foreignRef=ifelse(fields==fkey,fkey,NA)
                                  )
    dbExecute(db,stat)
    dbAppendTable(db,tab,extractedTables$data[[tab]])
    if(createFKindices&sum(fields==fkey,na.rm = T)>0){
      dbExecute(db,paste0("CREATE INDEX fk_",tab,"_",ft,"_idx ON ",tab,"(",na.omit(fields[fields==fkey]),")",collapse=";"))}
  }
  return(db)
}

require(openxlsx)
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
  wb <- createWorkbook()
  for(i in 1:length(listVar))
  {
    sn<- names(listVar)[i]
    addWorksheet(wb, sheetName = sn)
    hasRownames <- !all(grepl("^[0-9]*$",rownames(listVar[[i]])))
    writeDataTable(wb, sheet =sn, listVar[[i]],rowNames = hasRownames)
    nCols<-ifelse(hasRownames,ncol(listVar[[i]]), ncol(listVar[[i]]))
    setColWidths(wb, sheet =sn,cols = 1:nCols, widths = 'auto')
  }
  saveWorkbook(wb, file, overwrite = TRUE)
}


exportXL<-function(extractedTables,file,exportInfoTables=T)
{
  if(exportInfoTables){
  listExport<-c(extractedTables$info,extractedTables$data)
  }else{
    listExport<-extractedTables$info
  }
  save_in_excel(file,listExport)
}



