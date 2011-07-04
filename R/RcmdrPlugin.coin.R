# Rcmdr dialogs for the coin package

###### TO DO: 
# to add blocks for all tests cause all have blocks, (2way conting table, max sel stat surv, paired wilcox) - not so usefull, maybe
# maybe optiuni complexe pt teste asympt/approx/exact ...  
# Check dataset whitout factors see error for block? - seems ok

###### Last modified: 
# log 2011.07.04_v1.0-20: all recalls verified, all help buttons checked. Added Marginal Homogeneity test for variables (not by enter table),replaced pick with select use \n for titles, fixed ansary,fligner to work with ties. added surv test distributions - Works - by Daniel Leucuta 
# log 2011.07.04_v1.0-19: Kruskal Walis added distribution options, Added zero.method for 2 sample Wilcox test - Works - by Daniel Leucuta 
# log 2011.06.18_v1.0-18: modified multiple comparison for friedman test, and Kruskal Walis so that pairwise tests works - Works - by Daniel Leucuta 
# log 2011.06.12_v1.0-17: modified multiple comparison for friedman test, and Kruskal Walis - Works - by Daniel Leucuta 
# log 2011.04.27_v1.0-16: erased old code from RcmdrPlugin.survivalT - Works - by Daniel Leucuta 
# log 2011.04.26_v1.0-15: added surv_test for survival. It workes, including ties! integrated also with survival data definition in RcmdrPlugin.survival - Works - by Daniel Leucuta 
# log 2011.04.26_v1.0-14: added maxstat test for survival. It workes integrated also with survival data definition in RcmdrPlugin.survival - Works - by Daniel Leucuta 
# log 2011.04.03_v1.0-13: added maxstat test. - Works - by Daniel Leucuta 
# log 2011.04.12_v1.0-12: fixed spearman test. - Works - by Daniel Leucuta 
# log 2011.04.03_v1.0-12: added spearman test. - NOT Works - by Daniel Leucuta 
# log 2011.04.03_v1.0-11: added Fligner Killeen test - Works but without ties/CI. Schimbat la toate normal approx cu Monte carlo. Added blocks for median test and normal test. - Works - by Daniel Leucuta 
# log 2011.03.27_v1.0-10: added Ansary Bradley test - Works but without ties/CI. added enter table for Marginal homogeneity test - Works - by Daniel Leucuta 
# log 2011.03.13_v1.0-8: added two-way table ERROR. modified in two/k sample permutation test, added function to allow for it/ Error: exact for k sample doesn t work - by Daniel Leucuta 
# log 2011.02.27_v1.0-7: added Friedman test - merge generic. Verified: OK, except multiple pairwise comparations doesn t work - by Daniel Leucuta
# log 2011.02.17_v1.0-6: added al Walis test - merge generic. minor fixes. Verified: OK, except multiple pairwise comparations doesn t work - by Daniel Leucuta
# log 2011.02.15_v1.0-5: added median and normal test. enabled blocks for Wilcox test. added condition block=group var, and disalbed confint for blocks. added ties for normal test - problem with ties method. added oneway 2 sample permutation test. Verified: OK - by Daniel Leucuta
# log 2011.02.14_v1.0-4: added paired Wilcox test function (exact, approximate, asymptotic, zero.method), added and disabled method for ties for 2 sample Wilcox test. Verified: OK - by Daniel Leucuta
# log 2011.02.14_v1.0-3: added subset box for 2 sample Wilcox test. Verified: OK - by Daniel Leucuta
# log 2011.02.14_v1.0-2: added confidence level for 2 sample Wilcox test. Verified: OK - by Daniel Leucuta

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

fncCoinTwoSampleWilcoxonTest <- function(){
    initializeDialog(title=gettextRcmdr("Two-Sample Wilcoxon Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }

	level <- tclvalue(confidenceLevel) #
	strConfintText <- paste (", conf.int = TRUE, conf.level=", level , sep="") #

        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinTwoSampleWilcoxonTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute wilcox test for blocks !!!
        } else {
	    block = "" #
	}
        alternative <- as.character(tclvalue(alternativeVariable))

        zeromethod <- as.character(tclvalue(zeromethodVariable))
	strZeroMethod = ""
        if (zeromethod == "Default") {
	    strZeroMethod = ""
        } else {
            strZeroMethod = paste(", zero.method='", zeromethod, "' ", sep="")
        }
	
        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
#        if (ties == "default") {
	    strTies = ""
#        } else {
#            strTies = paste(", ties.method='", ties, "' ", sep="")
#        }
        if (test == "default"){
            doItAndPrint(paste("wilcox_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, strTies, strZeroMethod, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("wilcox_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, ", distribution='", test, strTies, strZeroMethod, "', data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    confidenceFrame <- tkframe(optionsFrame) #
    confidenceLevel <- tclVar(".95") #
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

    #tiesFrame <- tkframe(top) #
    #radioButtons(tiesFrame, name="ties", buttons=c("default", "mid-ranks", "average-scores"), 
    #    labels=c("Default", "Mid ranks", "Average scores"), 
    #    title="Method for ties") # To add translation !!!

    zeroMethodFrame <- tkframe(top) #
    radioButtons(zeroMethodFrame, name="zeromethod", buttons=c("Default", "Pratt", "Wilcoxon"), 
        labels=c("Default", "Pratt", "Wilcoxon"), title="Zero method")    
    
    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # getFrame(blockBox), 
    tkgrid(variablesFrame, sticky="nw") #
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w") #
    tkgrid(confidenceField, sticky="w") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), confidenceFrame, labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    #tkgrid(tiesFrame, sticky="w") #
    tkgrid(zeromethodFrame, labelRcmdr(zeroMethodFrame, text="    "), sticky="nw") #
    tkgrid(zeroMethodFrame, sticky="nw") #
    
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=5, columns=1) #
    #tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    #groupsLabel(groupsBox=groupBox, columnspan=2)
    #tkgrid(alternativeFrame, testFrame, sticky="nw")
    #tkgrid(buttonsFrame, columnspan=2, sticky="w")
    #dialogSuffix(rows=4, columns=2)
    }    

fncCoinPairedWilcoxonTest <- function(){
    initializeDialog(title=gettextRcmdr("Paired Wilcoxon Test"))
    variablesFrame <- tkframe(top) #
    .numeric <- Numeric()
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("First variable\n(select one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Second variable\n(select one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
	#exactoption <- as.character(tclvalue(exactbuttonsVariable))
        maxpts <- as.integer(tclvalue(asymptoticMaxpts))
        abseps <- as.double(tclvalue(asymptoticAbseps))
        releps <- as.double(tclvalue(asymptoticReleps))
	replications <- as.integer(tclvalue(approximateReplications))
        zeromethod <- as.character(tclvalue(zeromethodVariable))
        closeDialog()
	strExactOption = ""
        #if (exactoption == "default") {
	#    strExactOption = ""
        #} else {
        #    strExactOption = paste("algorithm='", exactoption, "'", sep="")
        #}
	strZeroMethod = ""
        if (zeromethod == "Default") {
	    strZeroMethod = ""
        } else {
            strZeroMethod = paste(", zero.method='", zeromethod, "' ", sep="")
        }
        alternative <- as.character(tclvalue(alternativeVariable))
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        test <- as.character(tclvalue(testVariable))
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall=fncCoinPairedWilcoxonTest, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (x == y) {
            errorCondition(recall=fncCoinPairedWilcoxonTest, message=gettextRcmdr("The two variables must be different."))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("median(", .activeDataSet, "$", x, " - ", .activeDataSet, "$", y, 
            ", na.rm=TRUE) # median difference", sep=""))
        if (test == "default"){
             doItAndPrint(paste("wilcoxsign_test(", .activeDataSet, "$", x, " ~ ", .activeDataSet, "$", y,
                ", alternative='",  alternative, "' ", strZeroMethod, subset, ")", sep=""))           
        } else if (test == "exact"){
            doItAndPrint(paste("wilcoxsign_test(", .activeDataSet, "$", x, " ~ ", .activeDataSet, "$", y,
                ", alternative='", alternative, "' ", strZeroMethod, subset, ", distribution=exact(", strExactOption, "))", sep=""))
        } else if (test == "asympt"){
            doItAndPrint(paste("wilcoxsign_test(", .activeDataSet, "$", x, " ~ ", .activeDataSet, "$", y,
                ", alternative='", alternative, "' ", strZeroMethod, subset, ", distribution=asymptotic(maxpts = ", maxpts, ", abseps = ", abseps, ", releps = ", releps, " ) )", sep=""))
        } else {
            doItAndPrint(paste("wilcoxsign_test(", .activeDataSet, "$", x, " ~ ", .activeDataSet, "$", y,
                ", alternative='", alternative, "' ", strZeroMethod, subset, ", distribution=approximate(B = ", replications, " ) )", sep=""))
        }
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("two.sided", "less", "greater"), 
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "asympt", "approx"), 
        labels=c("Default", "Exact", "Asymptotic", "Approximate"), 
        title=gettextRcmdr("Type of Test"))

    distributionsFrame <- tkframe(top)
	#radioButtons(distributionsFrame, name="exactbuttons", buttons=c("Default", "Shift", "SplitUp"), values=c("default", "shift", "split-up"), labels=c("Default", "Shift", "Split-up"), title="Exact options")

    asymptoticFrame <- tkframe(distributionsFrame) #
    asymptoticMaxptsFrame <- tkframe(asymptoticFrame) #
    asymptoticMaxpts <- tclVar("25000") #
    asymptoticMaxptsField <- ttkentry(asymptoticMaxptsFrame, width="6", textvariable=asymptoticMaxpts) #
    asymptoticAbsepsFrame <- tkframe(asymptoticFrame) #
    asymptoticAbseps <- tclVar("0.001") #
    asymptoticAbsepsField <- ttkentry(asymptoticAbsepsFrame, width="6", textvariable=asymptoticAbseps) #
    asymptoticRelepsFrame <- tkframe(asymptoticFrame) #
    asymptoticReleps <- tclVar("0") #
    asymptoticRelepsField <- ttkentry(asymptoticRelepsFrame, width="6", textvariable=asymptoticReleps) #

    approximateFrame <- tkframe(distributionsFrame) #
    approximateReplications <- tclVar("1000") #
    approximateField <- ttkentry(approximateFrame, width="6", textvariable=approximateReplications) #

    zeroMethodFrame <- tkframe(top) #
    radioButtons(zeroMethodFrame, name="zeromethod", buttons=c("Default", "Pratt", "Wilcoxon"), 
        labels=c("Default", "Pratt", "Wilcoxon"), title="Zero method")

    subsetBox() #

    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw") #
    tkgrid(variablesFrame, sticky="nw") #

    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #

    tkgrid(labelRcmdr(distributionsFrame, text="Distribution options", fg="blue"),sticky="w") #


    #tkgrid(exactbuttonsFrame, sticky="w") #


    tkgrid(labelRcmdr(asymptoticFrame, text="Asymptotic options", fg="blue"),sticky="w") #
    tkgrid(labelRcmdr(asymptoticMaxptsFrame, text="Max pts", fg="blue"),sticky="w") #
    tkgrid(asymptoticMaxptsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticAbsepsFrame, text="Abs. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticAbsepsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticRelepsFrame, text="Rel. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticRelepsField, sticky="w") #

    tkgrid(asymptoticMaxptsFrame, asymptoticAbsepsFrame, asymptoticRelepsFrame, sticky="s") #
    tkgrid(asymptoticFrame, sticky="nw")

    tkgrid(labelRcmdr(approximateFrame, text="Approximate replications", fg="blue"),sticky="w") #
    tkgrid(approximateField, sticky="w") #

    tkgrid(approximateFrame, labelRcmdr(distributionsFrame, text="    "), sticky="nw") #
    tkgrid(distributionsFrame, sticky="nw") #

    tkgrid(zeromethodFrame, labelRcmdr(zeroMethodFrame, text="    "), sticky="nw") #
    tkgrid(zeroMethodFrame, sticky="nw") #

    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
#    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
#    tkgrid(alternativeFrame, testFrame, sticky="nw")
#    tkgrid(buttonsFrame, columnspan=2, sticky="w")
#    dialogSuffix(rows=3, columns=1)
    }

fncCoinMedianTest <- function(){
    initializeDialog(title=gettextRcmdr("Median Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinMedianTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinMedianTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }

	level <- tclvalue(confidenceLevel) #
	strConfintText <- paste (", conf.int = TRUE, conf.level=", level , sep="") #

        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinMedianTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute wilcox test for blocks !!!
        } else {
	    block = "" #
	}

        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
#        if (ties == "default") {
	    strTies = ""
#        } else {
#            strTies = paste(", ties.method='", ties, "' ", sep="")
#        }
        if (test == "default"){
            doItAndPrint(paste("median_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, strTies, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("median_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, strTies, ", distribution='", test, "', data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="median_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    confidenceFrame <- tkframe(optionsFrame) #
    confidenceLevel <- tclVar(".95") #
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

    #tiesFrame <- tkframe(top) #
    #radioButtons(tiesFrame, name="ties", buttons=c("default", "mid-ranks", "average-scores"), 
    #    labels=c("Default", "Mid ranks", "Average scores"), 
    #    title="Method for ties") # To add translation !!!

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw")  
    tkgrid(variablesFrame, sticky="nw") #
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w") #
    tkgrid(confidenceField, sticky="w") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), confidenceFrame, labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    #tkgrid(tiesFrame, sticky="w") #
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=5, columns=1) #
    #tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    #groupsLabel(groupsBox=groupBox, columnspan=2)
    #tkgrid(alternativeFrame, testFrame, sticky="nw")
    #tkgrid(buttonsFrame, columnspan=2, sticky="w")
    #dialogSuffix(rows=4, columns=2)
    }    

fncCoinNormalTest <- function(){
    initializeDialog(title=gettextRcmdr("Normal Quantile (van der Waerden) Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinNormalTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinNormalTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }

	level <- tclvalue(confidenceLevel) #
	strConfintText <- paste (", conf.int = TRUE, conf.level=", level , sep="") #

        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinNormalTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute wilcox test for blocks !!!
        } else {
	    block = "" #
	}
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable)) #
        if (test == "exact") {
	    if (length(block) > 0) {
            errorCondition(recall=fncCoinNormalTest , message=gettextRcmdr("Cannot compute exact p-values with blocks."))
            return()
            }
        }
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
#        if (ties == "default") {
	    strTies = ""
 #       } else {
 #           strTies = paste(", ties.method='", ties, "' ", sep="")
 #       }
        if (test == "default"){
            doItAndPrint(paste("normal_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, strTies, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("normal_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strConfintText, strTies, ", distribution='", test, "', data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="normal_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    confidenceFrame <- tkframe(optionsFrame) #
    confidenceLevel <- tclVar(".95") #
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

#    tiesFrame <- tkframe(top) #
#    radioButtons(tiesFrame, name="ties", buttons=c("default", "mid-ranks", "average-scores"), 
#        labels=c("Default", "Mid-ranks", "Average-scores"), 
#        title="Method for ties") # To add translation !!!

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w") #
    tkgrid(confidenceField, sticky="w") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), confidenceFrame, labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
#    tkgrid(tiesFrame, sticky="w") #
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
    #tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    #groupsLabel(groupsBox=groupBox, columnspan=2)
    #tkgrid(alternativeFrame, testFrame, sticky="nw")
    #tkgrid(buttonsFrame, columnspan=2, sticky="w")
    #dialogSuffix(rows=4, columns=2)
    }    

fncCoinTwoSamplePermutationTest <- function(){
    initializeDialog(title=gettextRcmdr("Two/K Sample Permutation Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinTwoSamplePermutationTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinTwoSamplePermutationTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinTwoSamplePermutationTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
        } else {
	    block = "" #
	}
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
#        if (ties == "default") {
	    strTies = ""
#        } else {
#            strTies = paste(", ties.method='", ties, "' ", sep="")
#        }
        if (test == "default"){
            doItAndPrint(paste("oneway_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strTies, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("oneway_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strTies, ", distribution='", test, "', data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="oneway_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

    #tiesFrame <- tkframe(top) #
    #radioButtons(tiesFrame, name="ties", buttons=c("default", "mid-ranks", "average-scores"), 
    #    labels=c("Default", "Mid ranks", "Average scores"), 
    #    title="Method for ties") # To add translation !!!

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    #tkgrid(tiesFrame, sticky="w") #
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=5, columns=1) #
    #tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    #groupsLabel(groupsBox=groupBox, columnspan=2)
    #tkgrid(alternativeFrame, testFrame, sticky="nw")
    #tkgrid(buttonsFrame, columnspan=2, sticky="w")
    #dialogSuffix(rows=4, columns=2)
    }  

fncCoinKruskalWallisTest <- function(){
    Library("multcomp")
    initializeDialog(title=gettextRcmdr("Kruskal-Wallis Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #

    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinKruskalWallisTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinKruskalWallisTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinKruskalWallisTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            strLineBlock = paste(" | ", block, " ", sep="") #
        } else {
	    strLineBlock = "" #
	}
        #alternative <- as.character(tclvalue(alternativeVariable))
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
	
	maxpts <- as.integer(tclvalue(asymptoticMaxpts))
        abseps <- as.double(tclvalue(asymptoticAbseps))
        releps <- as.double(tclvalue(asymptoticReleps))
	replications <- as.integer(tclvalue(approximateReplications))
        closeDialog()
	strExactOption = ""
        test <- as.character(tclvalue(testVariable))

	
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        
	if (test == "default"){
             doItAndPrint(paste("kruskal_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, ")", sep=""))           
        } else if (test == "asympt"){
            doItAndPrint(paste("kruskal_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, 
		", distribution=asymptotic(maxpts = ", maxpts, ", abseps = ", abseps, ", releps = ", releps, " ) )", sep=""))
        } else {
            doItAndPrint(paste("kruskal_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, 
		 ", distribution=approximate(B = ", replications, " ) )", sep=""))
        }
	
	# code from coin package examples:
	### Nemenyi-Damico-Wolfe-Dunn test (joint ranking)
	### Hollander & Wolfe (1999), page 244
	### (where Steel-Dwass results are given)

	justDoIt(paste("NDWD <- oneway_test(", response," ~ ", group, block, ", data = ", .activeDataSet, ",
		ytrafo = function(data) trafo(data, numeric_trafo = rank),
		xtrafo = function(data) trafo(data, factor_trafo = function(x)
		model.matrix(~x - 1) %*% t(contrMat(table(x), 'Tukey'))),
		teststat = 'max', distribution = approximate(B = 90000))", sep=""))
	### global p-value
	pNDWD = pvalue(NDWD)
	logger(paste("Global p-value (distribution = approximate(B = 90000)):", pNDWD, sep=""))

	### sites (I = II) != (III = IV) at alpha = 0.01 (page 244)
	logger("Pairwise comparisons of groups:")
	doItAndPrint("pvalue(NDWD, method = 'single-step')")

        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="kruskal_test")

  
    
    optionsFrame <- tkframe(top) #

	radioButtons(optionsFrame, name="test", buttons=c("default", "asympt", "approx"), 
        labels=c("Default", "Asymptotic", "Approximate"), 
        title=gettextRcmdr("Type of Test"))

    distributionsFrame <- tkframe(top)
	#radioButtons(distributionsFrame, name="exactbuttons", buttons=c("Default", "Shift", "SplitUp"), values=c("default", "shift", "split-up"), labels=c("Default", "Shift", "Split-up"), title="Exact options")

    asymptoticFrame <- tkframe(distributionsFrame) #
    asymptoticMaxptsFrame <- tkframe(asymptoticFrame) #
    asymptoticMaxpts <- tclVar("25000") #
    asymptoticMaxptsField <- ttkentry(asymptoticMaxptsFrame, width="6", textvariable=asymptoticMaxpts) #
    asymptoticAbsepsFrame <- tkframe(asymptoticFrame) #
    asymptoticAbseps <- tclVar("0.001") #
    asymptoticAbsepsField <- ttkentry(asymptoticAbsepsFrame, width="6", textvariable=asymptoticAbseps) #
    asymptoticRelepsFrame <- tkframe(asymptoticFrame) #
    asymptoticReleps <- tclVar("0") #
    asymptoticRelepsField <- ttkentry(asymptoticRelepsFrame, width="6", textvariable=asymptoticReleps) #

    approximateFrame <- tkframe(distributionsFrame) #
    approximateReplications <- tclVar("1000") #
    approximateField <- ttkentry(approximateFrame, width="6", textvariable=approximateReplications) #


    pairwiseVariable <- tclVar("0")
    pairwiseCheckBox <- tkcheckbutton(optionsFrame, variable=pairwiseVariable)

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #

    tkgrid(testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="w")

    tkgrid(labelRcmdr(distributionsFrame, text="Distribution options", fg="blue"),sticky="w") #

    tkgrid(labelRcmdr(asymptoticFrame, text="Asymptotic options", fg="blue"),sticky="w") #
    tkgrid(labelRcmdr(asymptoticMaxptsFrame, text="Max pts", fg="blue"),sticky="w") #
    tkgrid(asymptoticMaxptsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticAbsepsFrame, text="Abs. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticAbsepsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticRelepsFrame, text="Rel. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticRelepsField, sticky="w") #

    tkgrid(asymptoticMaxptsFrame, asymptoticAbsepsFrame, asymptoticRelepsFrame, sticky="s") #
    tkgrid(asymptoticFrame, sticky="nw")

    tkgrid(labelRcmdr(approximateFrame, text="Approximate replications", fg="blue"),sticky="w") #
    tkgrid(approximateField, sticky="w") #

    tkgrid(approximateFrame, labelRcmdr(distributionsFrame, text="    "), sticky="nw") #
    tkgrid(distributionsFrame, sticky="nw") #


    tkgrid(labelRcmdr(optionsFrame, text="Pairwise comparisons of groups"), pairwiseCheckBox, sticky="w")


    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=2) #
    
    }  

fncCoinFriedmanTest <- function(){
    Library("multcomp")
    initializeDialog(title=gettextRcmdr("Friedman rank-sum Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #

    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinFriedmanTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinFriedmanTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        strBlock <- getSelection(blockBox) #
        if (length(strBlock) > 0) {
	    if (strBlock == group) {
            errorCondition(recall=fncCoinFriedmanTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            strLineBlock = paste(" | ", strBlock, " ", sep="") #
	    strBlockBlock = paste(", block = ", .activeDataSet, "$",strBlock, sep="")
        } else {
	    strLineBlock = "" #
	    strBlockBlock = ""
	}

        maxpts <- as.integer(tclvalue(asymptoticMaxpts))
        abseps <- as.double(tclvalue(asymptoticAbseps))
        releps <- as.double(tclvalue(asymptoticReleps))
	replications <- as.integer(tclvalue(approximateReplications))
        closeDialog()
	strExactOption = ""
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        test <- as.character(tclvalue(testVariable))

        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))

        if (test == "default"){
             doItAndPrint(paste("friedman_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, ")", sep=""))           
        } else if (test == "asympt"){
            doItAndPrint(paste("friedman_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, 
		", distribution=asymptotic(maxpts = ", maxpts, ", abseps = ", abseps, ", releps = ", releps, " ) )", sep=""))
        } else {
            doItAndPrint(paste("friedman_test(", response, " ~ ", group, strLineBlock, ", data=", .activeDataSet, subset, 
		 ", distribution=approximate(B = ", replications, " ) )", sep=""))
        }

        #doItAndPrint(paste("friedman_test(", response, " ~ ", group, block, ", data=", .activeDataSet, subset, ")", sep=""))

	# code from coin package examples:
		### Wilcoxon-Nemenyi-McDonald-Thompson test
		### Hollander & Wolfe (1999), page 295
		if (require("multcomp")) {
			### all pairwise comparisons
			justDoIt(paste("objMultTest <- symmetry_test(",response ," ~ ",group, strLineBlock, ", data = ",.activeDataSet, ",
			teststat = 'max',
			xtrafo = function(data)
				trafo(data, factor_trafo = function(x)
					model.matrix(~ x - 1) %*% t(contrMat(table(x), 'Tukey'))
				),
			ytrafo = function(data)
				trafo(data, numeric_trafo = rank", strBlockBlock, ")
			)", sep=""))
			### a global test, again
			justDoIt("objGlobalPValue = pvalue(objMultTest)")
			logger(paste("Global p-value:", objGlobalPValue, sep=""))
			### simultaneous P-values for all pair comparisons
			logger("Pairwise comparisons of groups:")
			doItAndPrint("pvalue(objMultTest, method = 'single-step')")
		}



        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="friedman_test")

    optionsFrame <- tkframe(top) #

	radioButtons(optionsFrame, name="test", buttons=c("default", "asympt", "approx"), 
        labels=c("Default", "Asymptotic", "Approximate"), 
        title=gettextRcmdr("Type of Test"))

    distributionsFrame <- tkframe(top)
	#radioButtons(distributionsFrame, name="exactbuttons", buttons=c("Default", "Shift", "SplitUp"), values=c("default", "shift", "split-up"), labels=c("Default", "Shift", "Split-up"), title="Exact options")

    asymptoticFrame <- tkframe(distributionsFrame) #
    asymptoticMaxptsFrame <- tkframe(asymptoticFrame) #
    asymptoticMaxpts <- tclVar("25000") #
    asymptoticMaxptsField <- ttkentry(asymptoticMaxptsFrame, width="6", textvariable=asymptoticMaxpts) #
    asymptoticAbsepsFrame <- tkframe(asymptoticFrame) #
    asymptoticAbseps <- tclVar("0.001") #
    asymptoticAbsepsField <- ttkentry(asymptoticAbsepsFrame, width="6", textvariable=asymptoticAbseps) #
    asymptoticRelepsFrame <- tkframe(asymptoticFrame) #
    asymptoticReleps <- tclVar("0") #
    asymptoticRelepsField <- ttkentry(asymptoticRelepsFrame, width="6", textvariable=asymptoticReleps) #

    approximateFrame <- tkframe(distributionsFrame) #
    approximateReplications <- tclVar("1000") #
    approximateField <- ttkentry(approximateFrame, width="6", textvariable=approximateReplications) #


    pairwiseVariable <- tclVar("0")
    pairwiseCheckBox <- tkcheckbutton(optionsFrame, variable=pairwiseVariable)

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #

    tkgrid(testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="w")

    tkgrid(labelRcmdr(distributionsFrame, text="Distribution options", fg="blue"),sticky="w") #

    tkgrid(labelRcmdr(asymptoticFrame, text="Asymptotic options", fg="blue"),sticky="w") #
    tkgrid(labelRcmdr(asymptoticMaxptsFrame, text="Max pts", fg="blue"),sticky="w") #
    tkgrid(asymptoticMaxptsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticAbsepsFrame, text="Abs. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticAbsepsField, sticky="w") #
    tkgrid(labelRcmdr(asymptoticRelepsFrame, text="Rel. err. tolerance", fg="blue"),sticky="w") #
    tkgrid(asymptoticRelepsField, sticky="w") #

    tkgrid(asymptoticMaxptsFrame, asymptoticAbsepsFrame, asymptoticRelepsFrame, sticky="s") #
    tkgrid(asymptoticFrame, sticky="nw")

    tkgrid(labelRcmdr(approximateFrame, text="Approximate replications", fg="blue"),sticky="w") #
    tkgrid(approximateField, sticky="w") #

    tkgrid(approximateFrame, labelRcmdr(distributionsFrame, text="    "), sticky="nw") #
    tkgrid(distributionsFrame, sticky="nw") #


    tkgrid(labelRcmdr(optionsFrame, text="Pairwise comparisons of groups"), pairwiseCheckBox, sticky="w")


    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=2) #
    }  

# based on code from Rcmdr by J. Fox twoWayTable

fncCoinTwoWayTable <- function(){
    Library("abind")
    initializeDialog(title=gettextRcmdr("Two-Way Table"))
    variablesFrame <- tkframe(top)
    .factors <- Factors()
    rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable\n(select one)"))
    columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable\n(select one)"))
    subsetBox()
    onOK <- function(){
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        if (length(row) == 0 || length(column) == 0){
            errorCondition(recall=fncCoinTwoWayTable, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (row == column) {
            errorCondition(recall=fncCoinTwoWayTable, message=gettextRcmdr("Row and column variables are the same."))
            return()
            }
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTestVariable)
        chisqComp <- tclvalue(chisqComponentsVariable)
        expected <- tclvalue(expFreqVariable)
	linearbylinear <- tclvalue(lblTestVariable)
        fisher <- tclvalue(fisherTestVariable)
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subset, sep="")
        closeDialog()
        command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
            subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)" # using old test to get info for expected counts
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            #doItAndPrint(".Test") not showing clasical chi square test
	    doItAndPrint("chisq_test(.Table)") #showing goin chi square test
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                gettextRcmdr("expected frequencies are less than 1"))
            if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
                gettextRcmdr(" expected frequencies are less than 5"), sep="")
            if (!is.null(warnText)) Message(message=warnText,
                type="warning")
            if (chisqComp == 1) {
                command <- "round(.Test$residuals^2, 2) # Chi-square Components"
                doItAndPrint(command)
                }
            logger("remove(.Test)")
            remove(.Test, envir=.GlobalEnv)
            }
	if (linearbylinear == 1) doItAndPrint("lbl_test(.Table)")
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="chisq_test")
    radioButtons(name="percents",
        buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"),
        values=c("row", "column", "total", "none"), initialValue="none",
        labels=gettextRcmdr(c("Row percentages", "Column percentages", "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
    checkBoxes(frame="testsFrame", boxes=c("chisqTest", "chisqComponents", "expFreq", "lblTest", "fisherTest"), initialValues=c("1", "0", "0", "0", "0"),
        labels=gettextRcmdr(c("Chi-square test of independence", "Components of chi-square statistic",
            "Print expected frequencies", "Linear-by-linear association test", "Fisher's exact test")))
    tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Hypothesis Tests"), fg="blue"), sticky="w")
    tkgrid(testsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=7, columns=1)
    }

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

fncCoinAnsariBradleyTest <- function(){
    initializeDialog(title=gettextRcmdr("Ansari Bradley Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinAnsariBradleyTest , message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinAnsariBradleyTest , message=gettextRcmdr("You must select a response variable."))
            return()
            }

	#level <- tclvalue(confidenceLevel) #
	#strConfintText <- paste (", conf.int = TRUE, conf.level=", level , sep="") #
	#strConfintText <- "conf.int = FALSE"
	#vties <- as.character(tclvalue(tiesVariable))

        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinAnsariBradleyTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute test for blocks !!!
        } else {
	    block = "" #
	}
        alternative <- as.character(tclvalue(alternativeVariable))

        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
	Library("abind")
        doItAndPrint(paste("numSummary(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste("groups=",.activeDataSet,"$", group, sep=""), ", statistics=c('quantiles', 'sd'), quantiles=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))", sep=""))
	Ties <- as.character(tclvalue(tiesmethodVariable)) #
        if (Ties == "default") {
	    strTies = ""
        } else {
            strTies = paste(", ties.method='", Ties, "' ", sep="")
        }
        if (test == "default"){
            doItAndPrint(paste("ansari_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strTies, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("ansari_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", ", distribution='", test, "'", strTies, ", data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="ansari_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    #confidenceFrame <- tkframe(optionsFrame) #
    #confidenceLevel <- tclVar(".95") #
    #confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

	radioButtons(optionsFrame, name="tiesmethod",
		 buttons=c("default", "midranks", "averagescores"), #
		 values=c("default", "mid-ranks", "average-scores"), initialValue="default",
		labels=c("Default", "Mid-ranks", "Average scores"), 
		title=gettext("Method for ties"))	

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # getFrame(blockBox), 
    tkgrid(variablesFrame, sticky="nw") #
    #tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w") #
    #tkgrid(confidenceField, sticky="w") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "),  labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    tkgrid(tiesmethodFrame, sticky="new")
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
    }    

fncCoinFlignerKilleenTest <- function(){
    initializeDialog(title=gettextRcmdr("Fligner Killeen Test"))
    variablesFrame <- tkframe(top) #
    groupBox <- variableListBox(variablesFrame, Factors(), title=gettextRcmdr("Groups\n(select one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=fncCoinFlignerKilleenTest , message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=fncCoinFlignerKilleenTest , message=gettextRcmdr("You must select a response variable."))
            return()
            }

	#level <- tclvalue(confidenceLevel) #
	#strConfintText <- paste (", conf.int = TRUE, conf.level=", level , sep="") #
	#strConfintText <- "conf.int = FALSE"
	#vties <- as.character(tclvalue(tiesVariable))

        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (block == group) {
            errorCondition(recall=fncCoinFlignerKilleenTest, message=gettextRcmdr("The group and block variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute test for blocks !!!
        } else {
	    block = "" #
	}
        alternative <- as.character(tclvalue(alternativeVariable))

        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
	Library("abind")
        doItAndPrint(paste("numSummary(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste("groups=",.activeDataSet,"$", group, sep=""), ", statistics=c('quantiles', 'sd'), quantiles=c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))", sep=""))
	Ties <- as.character(tclvalue(tiesmethodVariable)) #
        if (Ties == "default") {
	    strTies = ""
        } else {
            strTies = paste(", ties.method='", Ties, "' ", sep="")
        }
        if (test == "default"){
            doItAndPrint(paste("fligner_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", strTies, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("fligner_test(", response, " ~ ", group, block, ", alternative='", 
            alternative, "'", ", distribution='", test, "'", strTies, ", data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="fligner_test")
    optionsFrame <- tkframe(top) #
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))

    #confidenceFrame <- tkframe(optionsFrame) #
    #confidenceLevel <- tclVar(".95") #
    #confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

	radioButtons(optionsFrame, name="tiesmethod",
		 buttons=c("default", "midranks", "averagescores"), #
		 values=c("default", "mid-ranks", "average-scores"), initialValue="default",
		labels=c("Default", "Mid-ranks", "Average scores"), 
		title=gettext("Method for ties"))	    

    subsetBox() #

    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), getFrame(blockBox), sticky="nw") # getFrame(blockBox), 
    tkgrid(variablesFrame, sticky="nw") #
    #tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w") #
    #tkgrid(confidenceField, sticky="w") #
    groupsLabel(groupsBox=groupBox) #
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "),  labelRcmdr(optionsFrame, text="    "), testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    tkgrid(tiesmethodFrame, sticky="new")
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
    }    

fncCoinSpearmanTest <- function(){
    initializeDialog(title=gettextRcmdr("Spearman Test"))
    variablesFrame <- tkframe(top) #
    .numeric <- Numeric()
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("First variable\n(select one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Second variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        test <- as.character(tclvalue(testVariable))
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall=fncCoinSpearmanTest, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (x == y) {
            errorCondition(recall=fncCoinSpearmanTest, message=gettextRcmdr("The two variables must be different."))
            return()
            }
        block <- getSelection(blockBox) #
        if (length(block) > 0) {
            block = paste(" | ", block, " ", sep="") #
        } else {
	    block = "" #
	}
        closeDialog()
        .activeDataSet <- ActiveDataSet()

        if (test == "default"){
            doItAndPrint(paste("spearman_test(", x, " ~ ", y, block, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("spearman_test(", x, " ~ ", y, block, ", distribution='", test, "'", ", data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="spearman_test")
    optionsFrame <- tkframe(top) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

    subsetBox() #

    tkgrid(getFrame(xBox), labelRcmdr(variablesFrame, text="    "), getFrame(yBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #
    tkgrid(testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    #tkgrid(tiesFrame, sticky="w") #
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
    }    

fncCoinMaxstatTest <- function(){
    initializeDialog(title=gettextRcmdr("Maximally Selected Statistics Test"))
    variablesFrame <- tkframe(top) #
    .numeric <- Numeric()
    xBox <- variableListBox(variablesFrame, .numeric, selectmode="multiple",
        title=gettextRcmdr("Explanatory variables\n(select one or more)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Response variable\n(select one)"))
    blockBox <- variableListBox(variablesFrame, Factors(), title="Block\n(select none or one)") #
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)

        if (0 == length(y)) {
            errorCondition(recall=fncCoinMaxstatTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        if (0 == length(x)) {
            errorCondition(recall=fncCoinMaxstatTest, message=gettextRcmdr("No explanatory variables selected."))
            return()
            }
        if (is.element(y, x)) {
            errorCondition(recall=fncCoinMaxstatTest, message=gettextRcmdr("Response and explanatory variables must be different."))
            return()
            }
        block <- getSelection(blockBox) #
        if (length(block) > 0) {
            block = paste(" | ", block, " ", sep="") #
        } else {
	    block = "" #
	}

        test <- as.character(tclvalue(testVariable)) #
	subset <- tclvalue(subsetVariable) #
	subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="") #
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        if (test == "default"){
            doItAndPrint(paste("maxstat_test(", y, " ~ ", paste(x, collapse="+"), block, ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else doItAndPrint(paste("maxstat_test(", y, " ~ ", paste(x, collapse="+"), block, 
            ", distribution='", test, "'", ", data=", .activeDataSet, subset, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="maxstat_test")
    optionsFrame <- tkframe(top) #

    radioButtons(optionsFrame, name="test", buttons=c("default", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))

    subsetBox() #

    tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), getFrame(blockBox), sticky="nw") # 
    tkgrid(variablesFrame, sticky="nw") #
    tkgrid(testFrame, sticky="nw") #
    tkgrid(optionsFrame, sticky="nw") #
    tkgrid(subsetFrame, sticky="w") #
    tkgrid(buttonsFrame, sticky="w") #
    dialogSuffix(rows=6, columns=1) #
    }    

NumericOrDate <- function(dataSet=ActiveDataSet()) {#copied from John Fox's R-RcmdrPlugin.survival, needed below 
	setdiff(Variables(), Factors())
}
startStop <- function(time){#copied from John Fox's R-RcmdrPlugin.survival, needed below
	times <- na.omit(eval(parse(text=paste(ActiveDataSet(), '[,c("', time[1], '", "', time[2],'")]', sep=""))))
	if (all(times[[time[1]]] <= times[[time[2]]])){
		return(list(start=time[1], stop=time[2], error=FALSE))
	} else if (all(times[[time[2]]] <= times[[time[1]]])){
		return(list(start=time[2], stop=time[1], error=FALSE))
	}
	else return(list(start="", stop="", error=TRUE))
}

fncCoinMaxstatSurvTest <- function(){#based on R-RcmdrPlugin.survival, survdif, and it is interlinked in this way with survival data definition
	require(survival)
	if (!activeDataSetP()) return()
	currentModel <- FALSE
	initializeDialog(title=gettext("Maximally Selected Statistics Test for Survival data"))#, domain="R-RcmdrPlugin.survival"
	onOK <- function(){
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=fncCoinMaxstatSurvTest, 
					message=gettext("Start and stop times must be ordered."), model=TRUE)#, domain="R-RcmdrPlugin.survival"
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=fncCoinMaxstatSurvTest, message=gettext("You must select one or two time variables."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=fncCoinMaxstatSurvTest, message=gettext("You must select an event indicator."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
		x <- getSelection(xBox) 
		if (length(x) == 0) {
			errorCondition(recall=fncCoinMaxstatSurvTest, message=gettext("You must select at least one explanatory variable."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
		closeDialog()
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettext("<all valid cases>") #, domain="R-RcmdrPlugin.survival"
			|| trim.blanks(subset) == ""){
			subset <- ""
		}
		else{
			subset <- paste(", subset=", subset, sep="")
		}
		formula <- paste("Surv(", time1, ",",
			if(length(time2) != 0) paste(time2, ",", sep=""),
			event, ")", sep="")
		formula <- paste(formula, " ~ ", paste(x, collapse=" + "), sep="")
		command <- paste("maxstat_test(", formula, ", ", 
			"ytrafo = function(data) trafo(data, surv_trafo = function(x) logrank_trafo(x, ties = 'HL')) ",
			', data=', ActiveDataSet(), subset, ")", sep="")
		doItAndPrint(command)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="maxstat_test")
	survFrame <- tkframe(top)
	.activeDataSet <- ActiveDataSet()
	.numeric <- NumericOrDate()
	.factors <- Factors()
	time1 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time1")', sep="")))
	time1 <- if (!is.null(time1)) which(time1 == .numeric) - 1 
	time2 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time2")', sep="")))
	time2 <- if (!is.null(time2)) which(time2 == .numeric) - 1 
	event <- eval(parse(text=paste('attr(', .activeDataSet, ', "event")', sep="")))
	event <- if (!is.null(event)) which(event == Numeric()) - 1 
	#strata <- eval(parse(text=paste('attr(', .activeDataSet, ', "strata")', sep="")))
	#strata <- if (!is.null(strata)) which(is.element(.factors, strata)) - 1 else -1
	timeBox <- variableListBox(survFrame, NumericOrDate(), 
		title=gettext("Time or start/end times\n(select one or two)"),
		selectmode="multiple", initialSelection=if(is.null(time1)) NULL else c(time1, time2))
	eventBox <- variableListBox(survFrame, Numeric(), 
		title=gettext("Event indicator\n(select one)"),
		initialSelection=event)
	xBox <- variableListBox(survFrame, Numeric(), 
		title=gettext("Explanatory variables\n(select one or more)"), 
		selectmode="multiple")
	#rhoFrame <- tkframe(top)
	#rhoValue <- tclVar("0")
	#rhoSlider <- tkscale(rhoFrame, from=0, to=1, showvalue=TRUE, variable=rhoValue,
	#	resolution=0.1, orient="horizontal")
#	modelFormula(hasLhs=FALSE)
	subsetBox()
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(xBox), sticky="nw")
	tkgrid(survFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=9, columns=1)
}

fncCoinSurvTest <- function(){#based on R-RcmdrPlugin.survival, survdif, and it is interlinked in this way with survival data definition
	require(survival)
	if (!activeDataSetP()) return()
	currentModel <- FALSE
	initializeDialog(title=gettext("Independent Two/K Sample Test for Censored Data..."))#, domain="R-RcmdrPlugin.survival"
	onOK <- function(){
		time <- getSelection(timeBox)
		if (length(time) == 1){
			time1 <- time
			time2 <- numeric(0)
		}
		else if (length(time) == 2){
			ss <- startStop(time)
			if (ss$error) errorCondition(recall=fncCoinSurvTest, 
					message=gettext("Start and stop times must be ordered."), model=TRUE)#, domain="R-RcmdrPlugin.survival"
			time1 <- ss$start
			time2 <- ss$stop
		}
		else {
			errorCondition(recall=fncCoinSurvTest, message=gettext("You must select one or two time variables."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
		event <- getSelection(eventBox)
		if (length(event) == 0) {
			errorCondition(recall=fncCoinSurvTest, message=gettext("You must select an event indicator."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
		strata <- getSelection(strataBox) 
		if (length(strata) == 0) {
			errorCondition(recall=fncCoinSurvTest, message=gettext("You must select strata."))#, domain="R-RcmdrPlugin.survival"
			return()
		}
        block <- getSelection(blockBox) #
        if (length(block) > 0) {
	    if (is.element(block, strata)) {
            errorCondition(recall=fncCoinSurvTest, message=gettextRcmdr("The group and strata variables must be different."))
            return()
            }
            block = paste(" | ", block, " ", sep="") #
	    strConfintText = "" # cannot compute test for blocks !!!
        } else {
	    block = "" #
	}
	Ties <- as.character(tclvalue(tiesmethodVariable)) #
        if (Ties == "default") {
	    strTies = ""
        } else {
            strTies = paste(", ties.method='", Ties, "' ", sep="")
        }
	
	test <- as.character(tclvalue(testVariable))
	closeDialog()
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettext("<all valid cases>") #, domain="R-RcmdrPlugin.survival"
			|| trim.blanks(subset) == ""){
			subset <- ""
		}
		else{
			subset <- paste(", subset=", subset, sep="")
		}
   
		formula <- paste("Surv(", time1, ",",
			if(length(time2) != 0) paste(time2, ",", sep=""),
			event, ")", sep="")
		formula <- paste(formula, " ~ ", paste(strata, collapse=" + "), sep="")
		
	if (test == "default"){
            command <- paste("surv_test(", formula, block, strTies,
			', data=', ActiveDataSet(), subset, ")", sep="")
	}  else command <- paste("surv_test(", formula, block, strTies,
			', distribution="', test, '", data=', ActiveDataSet(), subset, ")", sep="")
		
		doItAndPrint(command)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="surv_test")
	survFrame <- tkframe(top)
	.activeDataSet <- ActiveDataSet()
	.numeric <- NumericOrDate()
	.factors <- Factors()
	time1 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time1")', sep="")))
	time1 <- if (!is.null(time1)) which(time1 == .numeric) - 1 
	time2 <- eval(parse(text=paste('attr(', .activeDataSet, ', "time2")', sep="")))
	time2 <- if (!is.null(time2)) which(time2 == .numeric) - 1 
	event <- eval(parse(text=paste('attr(', .activeDataSet, ', "event")', sep="")))
	event <- if (!is.null(event)) which(event == Numeric()) - 1 
	strata <- eval(parse(text=paste('attr(', .activeDataSet, ', "strata")', sep="")))
	strata <- if (!is.null(strata)) which(is.element(.factors, strata)) - 1 else -1
	timeBox <- variableListBox(survFrame, NumericOrDate(), 
		title=gettext("Time or start/end times\n(select one or two)"),
		selectmode="multiple", initialSelection=if(is.null(time1)) NULL else c(time1, time2))
	eventBox <- variableListBox(survFrame, Numeric(), 
		title=gettext("Event indicator\n(select one)"),
		initialSelection=event)
	strataBox <- variableListBox(survFrame, Factors(), 
		title=gettext("Strata\n(select one)"), 
		initialSelection=strata)
        blockBox <- variableListBox(survFrame, Factors(), title="Block\n(pick none or one)") #
	optionsFrame <- tkframe(top)
	radioButtons(optionsFrame, name="test", buttons=c("default", "exact", "approximate", "asymptotic"), 
        labels=gettextRcmdr(c("Default", "Exact", "Monte Carlo resampling approximation", "Asymptotic null distribution")), 
        title=gettextRcmdr("Type of Test"))       
	
	radioButtons(optionsFrame, name="tiesmethod",
		buttons=c("default","logrank", "HL", "averagescores"), #
		values=c("default","logrank", "HL", "average-scores"), initialValue="default",
		labels=gettext(c("Default","Logrank", "HL", "Average-scores")),
		title=gettext("Ties method"))
#	modelFormula(hasLhs=FALSE)
	subsetBox()
	tkgrid(getFrame(timeBox), labelRcmdr(survFrame, text="  "), getFrame(eventBox), labelRcmdr(survFrame, text="  "), getFrame(blockBox), sticky="sw")
	tkgrid(labelRcmdr(survFrame, text=""))
	tkgrid(getFrame(strataBox), sticky="nw")
	tkgrid(survFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(tiesmethodFrame, sticky="new")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(optionsFrame, sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(testFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(subsetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=9, columns=1)
}

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
