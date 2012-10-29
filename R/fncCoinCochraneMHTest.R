# based on code from Rcmdr by J. Fox multiWayTable

fncCoinCochraneMHTest <- function(){
  Library("abind")
  initializeDialog(title="Cochran-Mantel-Haenzsel test")
  variablesFrame <- tkframe(top)
  .factors <- Factors()
  rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable\n(select one)"))
  columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable\n(select one)"))
  controlBox <- variableListBox(variablesFrame, .factors, selectmode="multiple",
                                title=gettextRcmdr("Control variable(s)\n(select one or more)"))
  subsetBox()
  onOK <- function(){
    row <- getSelection(rowBox)
    column <- getSelection(columnBox)
    controls <- getSelection(controlBox)
    if (length(row) == 0 || length(column) == 0 || length(controls) == 0) {
      errorCondition(recall=fncCoinCochraneMHTest , message=gettextRcmdr("You must select row, column, and control variables"))
      return()
    }
    if ((row == column) || is.element(row, controls) || is.element(column, controls)) {
      errorCondition(recall=fncCoinCochraneMHTest , message=gettextRcmdr("Row, column, and control variables must be different."))
      return()
    }
    percents <- as.character(tclvalue(percentsVariable))
    subset <- tclvalue(subsetVariable)
    subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
    else paste(", subset=", subset, sep="")
    closeDialog()
    command <- paste("xtabs(~", row, "+", column, "+", paste(controls, collapse="+"),
                     ", data=", ActiveDataSet(), subset, ")", sep="")
    logger(paste(".Table <- ", command, sep=""))
    assign(".Table", justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(".Table")
    if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
    if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
    doItAndPrint(".Table")
    doItAndPrint("cmh_test(.Table)")
    remove(.Table, envir=.GlobalEnv)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="cmh_test")
  radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "nonePercents"), values=c("row", "column", "none"),
               initialValue="none", labels=gettextRcmdr(c("Row percentages", "Column percentages", "No percentages")), title=gettextRcmdr("Compute Percentages"))
  tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), labelRcmdr(variablesFrame, text="    "),
         getFrame(controlBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(percentsFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=4, columns=1)
}