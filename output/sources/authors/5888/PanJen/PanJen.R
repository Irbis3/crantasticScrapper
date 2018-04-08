# require(mgcv) 


choose.fform <-function(data,base_form,variable="dep",functionList, distribution=gaussian){
  ########################################
  ###   VALIDATION
  ########################################
  if (class(base_form)!="formula") {print("You need to provide a 'formula'-object, e.g. base_form<-formula(y ~x1+x2), Please see ?fform or ?formula for an example")}
  stopifnot(class(base_form)=="formula")
  
  ## remove problematic transformations
  a<-0
  drops<-c()
  suppressWarnings(
    for (i in functionList){
      a<-a+1
      var<-i(get(variable,data))
      var[!is.finite(var)] <- NA
      if (mean(as.numeric(is.na(var)))>0){
        print(paste(names(functionList)[a],"is dropped because it produces NaNs or infite",sep=" "))
        drops<-c(drops,a)
      }
    }
  )
  functionList[drops]<-NULL
  ## If there are no transformations lefts then stop
  nforms<-length(functionList) ## number of functional forms+base
  if (nforms<1) {" no transformations left"}
  
  
  ########################################
  ###   creating table and lists for output 
  ########################################
  frameNames=c("smoothing","base")
  
  for (j in 1:nforms){
    frameNames=c(frameNames, names(functionList)[j])
  }
  tableOut=matrix(nrow=nforms+2,ncol=3)
  rownames(tableOut)=frameNames
  colnames(tableOut)= c("AIC", "BIC","ranking (BIC)")
  
  
  ########################################
  ###   Estimate models and save AIC + BIC
  ########################################  
  ## Estimate base
  base<-gam(base_form, 
            family=distribution,
            method="GCV.Cp",
            data=data)
  
  ## save for output
  tableOut[2,1]=AIC(base)
  tableOut[2,2]=BIC(base)
  models <- list(base)
  
  equation_update<-
    update.formula(base_form, ~ . +s(var))
  data$var<-get(variable,data)
  smoothing<-gam(equation_update, 
                 family=distribution,
                 method="GCV.Cp",
                 data=data)
  tableOut[1,1]=AIC(smoothing)
  tableOut[1,2]=BIC(smoothing)
  models <- c(models, list(smoothing))
  
  ## estimate one model for each transformation
  namesLL=c("model_base","model_smoothing") ## model names
  
  
  for (j in 1:(nforms)){  
    equation_update<-update.formula(base_form, ~ . + var)
    data$var<-get(variable,data)
    data$var<-functionList[[j]](data$var)
    model_c<-gam(equation_update, 
                 family=distribution,
                 method="GCV.Cp",
                 data=data)
    tableOut[j+2,1]=AIC(model_c)
    tableOut[j+2,2]=BIC(model_c)
    models <- c(models, list(model_c))
    namesLL<-c(namesLL,paste("model",names(functionList)[j], sep="_"))
  }
  names(models)<-namesLL
  
  
  ## sort table after BIC
  tableOut[,1]<-round(tableOut[,1],2)
  tableOut[,2]<-round(tableOut[,2],2)
  tableOut[,3]=as.numeric(rank(tableOut[,2]))
  tableOut=tableOut[order(tableOut[,3]),]             
  
  ## sort models after BIC  
  order=paste("model",rownames(tableOut), sep="_")
  models<- models[order]
  #####################################################################
  ## return list  
  #####################################################################
  print(tableOut)   ## print table
  print("Smoothing is a semi-parametric and data-driven transformation, please see Wood (2006) for an elaboration")
  ## if a variable is repeated in base_form
  if(grepl(variable,toString(base_form))){print(paste("please note that you included",variable,"in the base-formula and it is also the variable you test"))}
  
  class(data)<-"data.frame"
  
  output=list("models"=models,
              "dataset"=data,
              "rank.table"=tableOut,
              "functionList"=functionList,
              "variable"=variable)
  class(output)<-"PJ"
  return(output) 
}




fform <-function(data,variable="dep",base_form, distribution=gaussian){
  if (class(base_form)!="formula") {print("You need to provide a 'formula'-object, e.g. base_form<-formula(y~x1+x2), Please see ?fform or ?formula for an example")}
  stopifnot(class(base_form)=="formula")

  
  ## transform a explanatory variable
  if(variable!="dep"){
  ## Predefined forms
  functionList <- list(
    "x" = function(x) x,
    "x^2" = function(x) x^.5,
    "sqr(x)" = function(x) x^2,
    "x+x^2" = function(x) x+x^2,
    "1/x" = function(x) 1/(x^2),
    "log(x)" = function(x) log(x)
  )
  
  ## remove problematic transformations
  a<-0
  drops<-c()
  suppressWarnings(
    for (i in functionList){
      a<-a+1
      var<-i(get(variable,data))
      var[!is.finite(var)] <- NA
      if (mean(as.numeric(is.na(var)))>0){
        print(paste(names(functionList)[a],"is dropped because it produces NaNs or infinite",sep=" "))
        drops<-c(drops,a)
      }
    }
  )
  functionList[drops]<-NULL
  ## If there are no transformations lefts then stop
  nforms<-length(functionList) ## number of functional forms+base
  if (nforms<1) {" no transformations left"
  }
  
  ########################################
  ###   creating table and lists for output 
  ########################################
  frameNames=c("smoothing","base")
  
  
  for (j in 1:nforms){
    frameNames=c(frameNames, names(functionList)[j])
  }
  

  tableOut=matrix(nrow=nforms+2,ncol=3)
  rownames(tableOut)=frameNames
  colnames(tableOut)= c("AIC", "BIC","ranking (BIC)")


  
  
  ########################################
  ###   Estimate models and save AIC + BIC
  ########################################  
  ## Estimate base
  base<-gam(base_form, 
            family=distribution,
            method="GCV.Cp",
            data=data)
  
  ## save for output
  tableOut[2,1]=AIC(base)
  tableOut[2,2]=BIC(base)
  models <- list(base)
  
  ## estimate smoothing
  
  equation_update<-
    update.formula(base_form, ~ . +s(var))
  data$var<-get(variable,data)
  smoothing<-gam(equation_update, 
                 family=distribution,
                 method="GCV.Cp",
                 data=data)
  tableOut[1,1]=AIC(smoothing)
  tableOut[1,2]=BIC(smoothing)
  models <- c(models, list(smoothing))
  
  ## estimate one model for each transformation
  namesLL=c("model_base","model_smoothing") ## model names
  
  
  for (j in 1:(nforms)){  
    equation_update<-update.formula(base_form, ~ . + var)
    data$var<-get(variable,data)
    data$var<-functionList[[j]](data$var)
    model_c<-gam(equation_update, 
                 family=distribution,
                 method="GCV.Cp",
                 data=data)
    tableOut[j+2,1]=AIC(model_c)
    tableOut[j+2,2]=BIC(model_c)
    models <- c(models, list(model_c))
    namesLL<-c(namesLL,paste("model",names(functionList)[j], sep="_"))
  }
  names(models)<-namesLL
  
  }
  
  ## transform the dependent variable
  if(variable=="dep"){
    trans<-c("identity","inverse", "log")
    frameNames=trans
    
    tableOut=matrix(nrow=3,ncol=3)
    rownames(tableOut)=frameNames
    colnames(tableOut)= c("AIC", "BIC","ranking (BIC)")
    
    
    ########################################
    ###   Estimate models and save AIC + BIC
    ########################################  

    models<-list()
    for (k in 1:3){
      base<-gam(base_form, 
                family=Gamma(make.link(trans[k]))
                  # distribution
                                      # "identity"
                  # print(as.character(trans[k]))
                ,
                method="GCV.Cp",
                data=data)  
      ## save for output  
      tableOut[k,1]=AIC(base)
      tableOut[k,2]=BIC(base)
      models <- list(models,base)
    }
  }
  
  
  ## sort table after BIC
  tableOut[,1]<-round(tableOut[,1],2)
  tableOut[,2]<-round(tableOut[,2],2)
  tableOut[,3]=as.numeric(rank(tableOut[,2]))
  tableOut=tableOut[order(tableOut[,3]),]             
  
  ## sort models after BIC  
  order=paste("model",rownames(tableOut), sep="_")
  models<- models[order]
  #####################################################################
  ## return list  
  #####################################################################
  print(tableOut)   ## print table
  
  if(variable!="dep"){
  print("Smoothing is a semi-parametric and data-driven transformation, please see Wood (2006) for an elaboration")   ## print GAM description
  }
  
  if(variable=="dep"){
  functionList=list()
  }
  
  
      ## if the "variable" is repeated in "base_form"
  if(grepl(variable,toString(base_form))){print(paste("please note that you included",variable,"in the base-formula and it is also the variable you test"))}

  class(data)<-"data.frame"
  
  output=list("models"=models,
              "dataset"=data,
              "rank.table"=tableOut,
              "functionList"=functionList,
              "variable"=variable)
  class(output)<-"PJ"
  return(output) 
}

plotff<-function(input){
  nModels= length(input$models) ## number of model
  namesLL<-names(input$functionList)
  nameslS<-names(input$models)
  ms<-grep("model_smoothing",nameslS)
  mb<-grep("model_base",nameslS)
  
  ######################################################################
  ### creating prediction frame 
  ######################################################################
  
  ## 100 points from min to max of variable
  pred_frame<-data.frame(matrix(NA, nrow=100, ncol=1))
  var<-get(input$variable,input$dataset)
  pred_frame$var<-seq(as.numeric(quantile(var,0.1)),as.numeric(quantile(var,0.9)),(as.numeric(quantile(var,0.9))-as.numeric(quantile(var,0.1)))/100)[1:100]
  
  ## set all control variable to median value
  control=apply(data.frame(input$models$model_base$model),2, median)
  depNam<-names(control)[1]
  control<-control[-1]
  for (i in 1:length(control)){
    pred_frame[names(control)[i]]=as.numeric(rep(control[i],100))
  }

    ## predict dependent variable for each model
  pred_frame$model_smoothing<-predict(input$models[["model_smoothing"]],newdata=pred_frame, type="response")
  pred_frame$model_base<-predict(input$models[["model_base"]],newdata=pred_frame, type="response")
  
  pred_frame$var_orig<-pred_frame$var
  a=0
  ylimits<-c()
  for (i in namesLL){ ## predict y for each model
    a=a+1
    pred_frame$var<-input$functionList[[a]](pred_frame$var_orig)
    
    ac=predict(input$models[[paste("model",i,sep="_")]],newdata=pred_frame, type="response")
    pred_frame[paste("model",i,sep="_")]<-ac
    ylimits<-c(ylimits,ac)
  }   
  
  ###########################################################################
  ###############################################################################
  firstM=length(control) + 4 ## first model in pred_frame
  lastM=dim(pred_frame)[2] ## last model in pred_frame
  
  
  ### Plotting
  limy=c(min(ylimits),max(ylimits)) ## limits for y-axis   
  limx=c(min(pred_frame$var_orig),max(pred_frame$var_orig)) ## limits for x-axis   
  
  #### plotting the the fit 
  par(mar=c(4, 4, 4,6), xpd=TRUE)
  opt <- options("scipen" = 20)
  
  ## Start plot
  
  plot(model_smoothing~var_orig, data=pred_frame, type="l",main="",sub="",xlab=input$variable,ylab=depNam, lwd=3, col="black", xlim=limx, ylim=limy) ## name variable of interest
  
  ## adding base and  functional form lines 
  color<-c("darkred","aquamarine4","darkgrey","blue3","orange3","brown3","chartreuse4","springgreen","gold","steelblue2","hotpink","blueviolet")
  color[ms]<-"black"
  color[mb]<-"darkgreen"
  
  n<-0
  colL<-vector()
  for (i in nameslS){
    n<-n+1
    lines(get(i,pred_frame)~pred_frame$var_orig,
          col= color[n], lwd=2)
    colL=cbind(colL,as.character(color[n]))
  }
  
  
  lines(get("model_smoothing",pred_frame)~pred_frame$var_orig,col="black", lwd=3)
  nameslS<-gsub("model_","",nameslS)
  
  legend("right",
         inset=-0.375,
    # limx[2],limy[2],
         cex=1,bty="n",
         nameslS, 
         fill=colL, 
         horiz=FALSE)
}