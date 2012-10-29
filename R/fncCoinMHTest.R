# based on rcmdr code enterTable

enterTableMHTest <- function(){
  Library("abind")
  env <- environment()
  initializeDialog(title=gettextRcmdr("Enter Two-Way Table for Marginal Homogeneity test"))
  outerTableFrame <- tkframe(top)
  assign(".tableFrame", tkframe(outerTableFrame), envir=env)
  setUpTable <- function(...){
    tkdestroy(get(".tableFrame", envir=env))
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols <- nrows#as.numeric(tclvalue(colsValue))
    make.col.names <- "labelRcmdr(.tableFrame, text='')"
    for (j in 1:ncols) {
      col.varname <- paste(".colname.", j, sep="")
      assign(col.varname, tclVar(j), envir=env)
      make.col.names <- paste(make.col.names, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                              col.varname, ")", sep="")
    }
    eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
    for (i in 1:nrows){
      varname <- paste(".tab.", i, ".1", sep="")
      assign(varname, tclVar("") , envir=env)
      row.varname <- paste(".rowname.", i, sep="")
      assign(row.varname, tclVar(i), envir=env)
      make.row <- paste("ttkentry(.tableFrame, width='5', textvariable=",
                        row.varname, ")", sep="")
      make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                        varname, ")", sep="")
      for (j in 2:ncols){
        varname <- paste(".tab.", i, ".", j, sep="")
        assign(varname, tclVar(""), envir=env)
        make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                          varname, ")", sep="")
      }
      eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
    }
    tkgrid(get(".tableFrame", envir=env), sticky="w")
  }
  rowColFrame <- tkframe(top)
  rowsValue <- tclVar("2")
  rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,
                        resolution=1, orient="horizontal", command=setUpTable)
  rowsShow <- labelRcmdr(rowColFrame, textvariable=rowsValue, width=2, justify="right")
  #colsValue <- tclVar("2")
  #colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
  #    resolution=1, orient="horizontal", command=setUpTable)
  #colsShow <- labelRcmdr(rowColFrame, textvariable=colsValue, width=2, justify="right")
  onOK <- function(){
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols <- nrows#as.numeric(tclvalue(colsValue))
    cell <- 0
    counts <- rep(NA, nrows*ncols)
    row.names <- rep("", nrows)
    col.names <- rep("", ncols)
    for (i in 1:nrows) row.names[i] <-
      eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
    for (j in 1:ncols) col.names[j] <-
      eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
    for (i in 1:nrows){
      for (j in 1:ncols){
        cell <- cell+1
        varname <- paste(".tab.", i, ".", j, sep="")
        counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
      }
    }
    counts <- na.omit(counts)
    if (length(counts) != nrows*ncols){
      errorCondition(recall=enterTableMHTest, message=sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number of rows (%d) * number of columns (%d)."), length(counts), nrows, ncols))
      return()
    }
    if (length(unique(row.names)) != nrows){
      errorCondition(recall=enterTableMHTest, message=gettextRcmdr("Row names are not unique."))
      return()
    }
    if (length(unique(col.names)) != ncols){
      errorCondition(recall=enterTableMHTest, message=gettextRcmdr("Column names are not unique."))
      return()
    }
    percents <- as.character(tclvalue(percentsVariable))
    test <- as.character(tclvalue(testVariable)) #
    closeDialog()
    command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
                     ", byrow=TRUE)", sep="")
    assign(".Table", justDoIt(command), envir=.GlobalEnv)
    logger(paste(".Table <- ", command, sep=""))
    command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
    justDoIt(paste("rownames(.Table) <- ", command, sep=""))
    logger(paste("rownames(.Table) <- ", command, sep=""))
    command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
    justDoIt(paste("colnames(.Table) <- ", command, sep=""))
    logger(paste("colnames(.Table) <- ", command, sep=""))
    doItAndPrint(".Table  # Counts")
    if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
    if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
    if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
    if (test == "default") {
      strDistribution = ""
    } else {
      strDistribution = paste(", distribution='", test,"' ", sep="")
    } 
    doItAndPrint(paste("mh_test(as.table(.Table)", strDistribution,")", sep=""))
    logger("remove(.Test)")
    logger("remove(.Table)")
    remove(.Table, envir=.GlobalEnv)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="mh_test")
  radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"), values=c("row", "column", "total", "none"),
               initialValue="none", labels=gettextRcmdr(c("Row percentages", "Column percentages",  "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
  
  optionsFrame <- tkframe(top) #
  radioButtons(optionsFrame, name="test", buttons=c("default", "approximate", "asymptotic"), 
               labels=gettextRcmdr(c("Default", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
               title=gettextRcmdr("Type of Test"))
  
  tkgrid(labelRcmdr(rowColFrame, text=gettextRcmdr("Number of Rows:")), rowsSlider, rowsShow, sticky="w")
  
  tkgrid(rowColFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter counts:"), fg="blue"), sticky="w")
  tkgrid(outerTableFrame, sticky="w")
  tkgrid(percentsFrame, sticky="w")
  
  tkgrid(optionsFrame, sticky="w")
  tkgrid(testFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  dialogSuffix(rows=8, columns=2)
}

# based on code from Rcmdr by J. Fox twoWayTable

fncCoinMHTest <- function(){
  Library("abind")
  initializeDialog(title=gettextRcmdr("Marginal Homogeneity Test for Two-Way Table"))
  variablesFrame <- tkframe(top)
  .factors <- Factors()
  rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable\n(select one)"))
  columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable\n(select one)"))
  subsetBox()
  subset <- tclvalue(subsetVariable)
  subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
  else paste(", subset=", subset, sep="")
  
  onOK <- function(){
    row <- getSelection(rowBox)
    column <- getSelection(columnBox)
    if (length(row) == 0 || length(column) == 0){
      errorCondition(recall=fncCoinMHTest, message=gettextRcmdr("You must select two variables."))
      return()
    }
    if (row == column) {
      errorCondition(recall=fncCoinMHTest, message=gettextRcmdr("The variables are the same."))
      return()
    }
    
    command <- paste("length(levels(as.factor(", ActiveDataSet(), "$",row, "))) == length(levels(as.factor(", ActiveDataSet(), "$", column, ")))", sep="")
    assign(".bolEqualNbLevels", justDoIt(command), envir=.GlobalEnv)
    if (.bolEqualNbLevels == FALSE) {
      errorCondition(recall=fncCoinMHTest, message=gettextRcmdr("The factors have different number of levels."))
      return()
    }
    remove(.bolEqualNbLevels, envir=.GlobalEnv)
    
    command <- paste("levels(as.factor(", ActiveDataSet(), "$",row, ")) == levels(as.factor(", ActiveDataSet(), "$", column, "))", sep="")
    assign(".bolEqualLevels", justDoIt(command), envir=.GlobalEnv)
    if (.bolEqualLevels == FALSE) {
      errorCondition(recall=fncCoinMHTest, message=gettextRcmdr("The factors have different levels."))
      return()
    }
    remove(.bolEqualLevels, envir=.GlobalEnv)
    
    
    percents <- as.character(tclvalue(percentsVariable))
    
    test <- as.character(tclvalue(testVariable)) #
    closeDialog()
    command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
                     subset, ")", sep="")
    logger(paste(".Table <- ", command, sep=""))
    assign(".Table", justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(".Table")
    if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
    if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
    if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
    if (test == "default") {
      strDistribution = ""
    } else {
      strDistribution = paste(", distribution='", test,"' ", sep="")
    } 
    doItAndPrint(paste("mh_test(as.table(.Table)", strDistribution,")", sep=""))
    
    logger("remove(.Test)")
    remove(.Test, envir=.GlobalEnv)
    
    logger("remove(.Table)")
    remove(.Table, envir=.GlobalEnv)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="chisq_test")
  radioButtons(name="percents",
               buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"),
               values=c("row", "column", "total", "none"), initialValue="none",
               labels=gettextRcmdr(c("Row percentages", "Column percentages", "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
  
  optionsFrame <- tkframe(top) #
  radioButtons(optionsFrame, name="test", buttons=c("default", "approximate", "asymptotic"), 
               labels=gettextRcmdr(c("Default", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
               title=gettextRcmdr("Type of Test"))   
  
  tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(percentsFrame, sticky="w")
  
  tkgrid(optionsFrame, sticky="w")
  tkgrid(testFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=7, columns=1)
}
