# Nom fichier :        RequeteODBCwhere (classe)
#' @title RequeteODBCwhere class 
#' @note Inherits from RequeteODBC
#' the syntax is where="WHERE ..."
#' and =vector("AND...","AND...")
#' order_by="ORDER BY.."
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @slot select="character"
#' @slot where="character"
#' @slot and="vector"
#' @slot order_by="character"
#' @slot baseODBC="vector" 	(inherited from ConnectionODBC)
#' @slot silent="logical" 	(inherited from ConnectionODBC)
#' @slot etat="character" 	(inherited from ConnectionODBC)
#' @slot connection="ANY" 	(inherited from ConnectionODBC)
#' @slot sql="character" 	(inherited from RequeteODBC)
#' @slot query="data.frame"	(inherited from RequeteODBC)
#' @slot open="logical" 	(inherited from RequeteODBC)
#' @examples object=new("RequeteODBCwhere")
setClass(Class="RequeteODBCwhere",
		representation= representation(select="character",where="character",and="vector",order_by="character"),
		prototype = list(silent=TRUE,open=FALSE),contains="RequeteODBC")



setAs("RequeteODBCwhere","RequeteODBC",function(from,to){
			requeteODBC=new("RequeteODBC")
			requeteODBC@sql=paste(from@select,from@where,paste(from@and,collapse=" "),from@order_by,";",sep=" ")
			requeteODBC@baseODBC=from@baseODBC
			requeteODBC@silent=from@silent
			# other slots will be filled in by connect	
			return(requeteODBC)
		})
#' connect method loads a request to the database and returns either an error or a data.frame
#' @note method modified from v0.2.1240 to use the connect method of the mother class
#' @return An object of class RequeteODBCwhere
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' object<-new("RequeteODBCwhere")
#' object@baseODBC<-baseODBC
#' object@sql<- "select * from t_lot_lot"
#' object@where<-"WHERE lot_tax_code='2038'"
#' object@and<-c("AND lot_std_code='CIV'","AND lot_ope_identifiant<1000")
#' object@order_by<-"ORDER BY lot_identifiant"
#' object<-connect(object)
setMethod("connect",signature=signature("RequeteODBCwhere"),definition=function(object) {
			requeteODBC=as(object,"RequeteODBC")
			requeteODBC=connect(requeteODBC) # uses mother class method
			object@sql=requeteODBC@sql
			object@connection=requeteODBC@connection
			object@query=requeteODBC@query
			object@etat=requeteODBC@etat
			return(object)
		})
