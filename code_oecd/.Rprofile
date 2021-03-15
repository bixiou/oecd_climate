library(utils)

# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Documents/www/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

package("plyr")
package("tm")
package("memisc")
package('tidyverse')
package("xtable")
package("rms")
package('pwr')
package("foreign")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/home/adrien/anaconda3/bin", sep = .Platform$path.sep))
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/Users/afabre/Anaconda3/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep)) # to correct bug orca, add folder of orca.exe
package("plotly")
package('gdata')
package("Hmisc")
package("quantreg")
package("rcompanion")
package("DescTools")
package("VCA")
package("glmnet")
# package("installr") # not for linux
package("plotly")
package("processx")
package("readstata13")
package("permute")
package("AER")
package("ivmodel")
package("rattle")
package("data.table")
package("reshape2")
# package("rddtools") # not available
# package("rddapp") # not available
package("mets")
package("descr")
package("stargazer")
package("clipr")
package("ergm") # wtd.median
package("mfx")
package("margins")
package("plotrix")
package("grDevices")
package("colorspace")
package("RColorBrewer")
package("colorRamps")
package("ordinal")
package("oglmx")
package("logistf")
package("emmeans")
package("ggeffects")
package("snakecase")
package("rdd")
package("corrplot")
package("psy")
package("lavaan")
package("StatMatch")
package("np")
package("AMR")
package("KSgeneral")
package("dgof")
package("SnowballC")
package("wordcloud")
package("RCurl")
package("XML")
package("equivalence")
package("RMallow")
package("Peacock.test")
package("devtools")
# install_github("rstudio/webshot2")
# package("webshot2")
package("htmlwidgets")
# package("magick") # Bug sur Ubuntu, ne surtout pas décommenter sur Ubuntu
library(magick) # TODO
# install_github(repo = "MatthieuStigler/RCompAngrist", subdir = "RCompAngrist")
# package("RCompAngrist")

# package("psych") # library(psych, exclude = "describe")
# package("semTools")
# package("interplot")
# package("jtools")
# package("effects")
# package("sjplot")
# package("doMC") # for parallel computing, does not work on Windows

# Fs <- function(QID) { s[QID][[1]] }
# Vs <- function(QID) { as.vector(Fs(QID))  } 
n <- function(var) { as.numeric(as.vector(var)) }
NSPs <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
nsps <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
decrit <- function(variable, miss = TRUE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) { # TODO!: allow for boolean weights
  # if (!missing(data)) variable <- data[[variable]]
  if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
  if (!missing(which)) variable <- variable[which] 
  if (weight) { 
    # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
    if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
    if (!missing(which)) weights <- weights[which]
    if (length(weights)!=length(variable)) {
      warning("Lengths of weight and variable differ, non-weighted results are provided")
      weights <- NULL
    } }
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {  
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}

# decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) { # TODO!: allow for boolean weights
#   # if (!missing(data)) variable <- data[[variable]]
#   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#   if (!missing(which)) variable <- variable[which] 
#   if (weight) { 
#     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
#     if (!missing(which)) weights <- weights[which]
#     if (length(weights)!=length(variable)) {
#       warning("Lengths of weight and variable differ, non-weighted results are provided")
#       weights <- NULL
#     } }
#   if (length(annotation(variable))>0 & !numbers) {
#     if (!miss) {
#       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#     }
#     else {
#       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#   }
#   else {  
#     if (length(annotation(variable))>0) {
#       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
# }
# decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) { # TODO!: allow for boolean weights
#   # if (!missing(data)) variable <- data[[variable]]
#   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#   if (!missing(which)) variable <- variable[which] 
#   if (weight) { 
#     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
#     if (!missing(which)) weights <- weights[which]
#     if (length(weights)!=length(variable)) {
#       warning("Lengths of weight and variable differ, non-weighted results are provided")
#       weights <- NULL
#     } }
#   if (length(annotation(variable))>0 & !numbers) {
#     if (!miss) {
#       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#     }
#     else {
#       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#   }
#   else {  
#     if (length(annotation(variable))>0) {
#       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
# }
export_stats_desc <- function(data, file, miss = TRUE, sorted_by_n = FALSE, return = FALSE, fill_extern = FALSE) {
  original_width <- getOption("width")
  options(width = 10000)
  des <- des_miss <- nb_miss <- labels <- n <- c()
  for (i in 1:length(data)) {
    decrit_i <- capture.output(print(decrit(data[[i]], miss=TRUE)))
    n <- c(n, as.numeric(sub('[[:blank:]]*([[:digit:]]*).*', '\\1', decrit_i[3])))
    des_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(capture.output(print(decrit(data[[i]]))), collapse='<br>')))))
    if (str_count(des_i, fixed("),"))>1) { des_i <- gsub("),",")<br>",des_i) }
    des_miss_i <-  gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i, collapse='<br>')))))
    if (str_count(des_miss_i, fixed("),"))>1) { des_miss_i <- gsub("),",")<br>",des_miss_i) }
    nb_miss_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i[1:3], collapse='<br>')))))
    des <- c(des, des_i)
    des_miss <- c(des_miss, des_miss_i)
    nb_miss <- c(nb_miss, nb_miss_i)
    if (Label(data[[i]])=='') label_i <- colnames(data)[i]
    else label_i <- paste(colnames(data)[i], sub('[^:]*:', '', Label(data[[i]])), sep=':')
    labels <- c(labels, label_i)
  }
  
  if (miss=='both') output <- matrix(c(names(data), labels, des, des_miss), ncol=4)
  if (miss=='nb') output <- matrix(c(names(data), labels, nb_miss), ncol=3)
  else if (sorted_by_n) output <- matrix(c(names(data), labels, des_miss), ncol=3)[order(-n),]
  else if (miss) output <- matrix(c(names(data), labels, des_miss), ncol=3)
  else output <- matrix(c(names(data), labels, des), ncol=3)
  write.table(output, file=file, sep=";;;", row.names=FALSE, col.names=FALSE, quote=FALSE)
  options(width = original_width)
  if (fill_extern) {
    nb_reponses <<- n
    nb_manquants <<- des_miss     }
  if (return) return(output)
}
desc_table <- function(dep_vars, filename = NULL, data = e, indep_vars = control_variables, indep_labels = control_variables, weights = data$weight,
                       save_folder = "../tables/", dep.var.labels = dep_vars, dep.var.caption = c(""), digits= 3, mean_control = FALSE,
                       mean_above = T, only_mean = F, keep = indep_vars, nolabel = F) {
  # Wrapper for stargazer
  # dep_vars accepts expressions of type : var_name expression (e.g. "equal_quota %in% 0:1", but not "equal_quota == 0 | equal_quota==1)
  models <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    models[[i]] <- lm(as.formula(paste(dep_vars[i], "~", paste("(", indep_vars, ")", collapse = ' + '))), data = data, weights = weights)
    if (mean_control==FALSE){
      means[i] <- round(wtd.mean(eval(parse(text = paste( "data$", parse(text = dep_vars[i]), sep=""))), weights = weights, na.rm = T), d = digits)
      mean_text <- "Mean"
    } else {
      means[i] <- round(wtd.mean(eval(parse(text = paste( "(data$", parse(text = dep_vars[i]), ")[data$treatment=='None']", sep=""))), weights = weights[data$treatment=='None'], na.rm = T), d = digits)
      mean_text <- "Control group mean"      
    }
  }
  if (missing(filename)) file_path <- NULL
  else file_path <- paste(save_folder, filename, ".tex", sep="")
  if (only_mean) mean_above <- T
  if (mean_above) { 
    if (nolabel) table <- do.call(stargazer, c(models,
                                    list(out=NULL, header=F, model.numbers = F, add.lines = list(c(mean_text, means)),
                                         dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption,
                                         multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep #, omit.stat = c("n")
                                    )))
    else  table <- do.call(stargazer, c(models,
                                  list(out=NULL, header=F, model.numbers = F,
                                       covariate.labels = indep_labels, add.lines = list(c(mean_text, means)),
                                       dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption,
                                       multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep #, omit.stat = c("n")
                                  )))
    mean_line <- regmatches(table, regexpr('(Mean|Control group mean) &[^\\]*', table))
    if (only_mean) {
      table <- write_clip(gsub(paste(indep_labels[1], ".*"), paste(mean_line, '\\\\\\\\'), table), collapse=' ')
      table <- table[c(1:grep('(Mean|Control group mean) &[^\\]*', table)[1], (length(table)-3):length(table))]
    } else table <- write_clip(gsub(indep_labels[1], paste(mean_line, '\\\\\\\\ \\\\hline \\\\\\\\[-1.8ex]', indep_labels[1]), 
                                    gsub('(Mean|Control group mean) &.*', '', table)), collapse=' ')
    cat(paste(table, collapse="\n"), file = file_path)
  } else {
      if (nolabel) table <- do.call(stargazer, c(models,
                                         list(out=file_path, header=F, add.lines =list(c(mean_text, means)),
                                              dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption,
                                              multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep
                                         )))
      else table <- do.call(stargazer, c(models,
                                                 list(out=file_path, header=F,
                                                      covariate.labels = indep_labels, add.lines =list(c(mean_text, means)),
                                                      dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption,
                                                      multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep
                                                 ))) }
  return(table)
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) 
}
clean_number <- function(vec, high_numbers='') { 
  numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
  if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
  else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
  else if (high_numbers=='divide&remove') { 
    numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
    is.na(numeric_vec) <- numeric_vec>6000 }
  return(numeric_vec)
}
uc <- function(nb_pers, nb_14_et_plus) {
  # https://www.insee.fr/fr/metadonnees/definition/c1802
  return(1 + 0.5 * pmax(0, nb_14_et_plus - 1) + 0.3 * pmax(0, nb_pers - nb_14_et_plus))
}
quotient <- function(nb_pers, nb_adultes) {
  # https://droit-finances.commentcamarche.com/contents/907-quotient-familial-calcul-du-nombre-de-parts-fiscales
  # nb de parts fiscales en fonction de la situation et de nb_pers = 1 / 2 / 3 / 4 / 5
  # marie: x / 2 / 2.5 / 3 / 4 --- en concubinage: x / 1 / 1.5 /2 / 3 (= marie - 1) --- seul: 1 / 2 / 2.5 / 3.5 / 4.5
  return((nb_pers == 1) + (nb_pers == 2)*2 + (nb_pers == 3)*2.5 + (nb_pers == 4)*3 + (nb_pers > 4)*pmin(6, nb_pers - 1) + (nb_adultes==1)*(nb_pers > 3)*0.5 )
}
irpp <- function(rev, nb_adultes, nb_pers) {
  # quotient <- (nb_pers < 2) + (nb_pers == 2) * 2 + (nb_pers == 3) * 2.5 + (nb_pers == 4) * 3 + (nb_pers > 4) * pmin(6, nb_pers - 1)
  income <- 0.9334 * rev / quotient(nb_pers, nb_adultes) # (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
  ir <- 0
  ir <- ir + (income - 12815.25*12) * 0.45 * (income > 12815.25*12)
  ir <- ir + (pmin(income, 12676*12) - 6051.42*12) * 0.41  * (income > 6051.42*12)
  ir <- ir + (pmin(income, 6051.42*12) - 2257.17*12) * 0.3  * (income > 2257.17*12)
  ir <- ir + (pmin(income, 2257.17*12) - 817.25*12) * 0.14  * (income > 817.25*12)
  
  ir <- quotient(nb_pers, nb_adultes) * ir
  seuil_decote <- (nb_adultes>1)*2585/12 + (nb_adultes<=1)*1569/12
  # decote <- (1920 - 0.75 * ir) * (marie & ir<2560) + (1165 - 0.75 * ir) * (!(marie) & ir<1553)
  decote <- (ir < seuil_decote) * 0.75 * (seuil_decote - ir)
  return(pmax((ir-decote),0)) # vrai calcul
}



##### Graphiques #####
stack_bars <- function(vars, data=s, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
  matrice <- c()
  colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (is.na(labels)) { labels <- vars }
  if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
  if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
  else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
  else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
  if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
  if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
  if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (miss) { 
    if (en) values <- c(values, "PNR")
    else values <- c(values, "NSP") }
  # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
  # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
  # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
oui_non <- function(vars, file, labels = vars, data = s, display_value = T, sort=T, colors=color(2), weights=T, margin_r=0, margin_l=NA, title="", en=FALSE, NSP=FALSE) { # 250 l
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  oui <- non <- nsp <- c()
  for (var in vars) {
    if (weights) {
      oui <- c(oui, sum(data[['weight']][which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      non <- c(non, sum(data[['weight']][which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
    }
    else {
      oui <- c(oui, length(which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      non <- c(non, length(which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
    }  
  }
  true_nsp <- round(100 * nsp*(oui+non))
  oui <- round(100 * oui)
  non <- round(100 * non)
  nsp <- round(100 * nsp)
  if (sort) order_as <- order(oui/(oui+non))
  else order_as <- 1:length(oui)
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  if (sort) oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en==T) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else if (en==FALSE) {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- en
    if (length(Text) ==2) Text <- c(Text, 'PNR')}
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }
  if (!(NSP)) Text[3] <- ''
  print(oui)
  print(non)
  print(nsp)
  print(o)
  print(n)
  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
                  hoverinfo = 'text', marker = list(color = colors[1], line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = colors[2])) %>%
    add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = c(10, 90, 110),
                    y = 1.1,
                    text = Text,
                    font = list(family = 'Arial', size = 15, color = 'black'),
                    showarrow = FALSE) # %>%
  # labeling the percentages of each bar (x_axis)
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o / 2, y = y,
  #                 text = paste(data[,"oui"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n / 2, y = y,
  #                 text = paste(data[,"non"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n + nsp / 2, y = y,
  #                 text = paste(data[,"nsp"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # api_create(bars, filename=file, sharing="public")
  return(bars) # bugs most often than not
}
data5 <- function(vars, data=e, miss=T, weights=T, rev=FALSE) {
  matrice <- c()
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      if (is.null(annotation(data[[var]]))) {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
      else {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))    
      if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (rev & !(miss)) return(matrice[5:1,])
  else if (rev & miss) return(matrice[c(5:1,6),])
  else return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=e, weights=T) {
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T))/length(which(data[[var]]==T | data[[var]]==FALSE))) }
  }
  return( matrix(res, ncol=length(vars)) )
}
dataN <- function(var, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE, rev_legend = FALSE) {
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  mat <- c()
  if (is.character(data[[var]]) | (is.numeric(data[[var]]) & !grepl("item", class(data[[var]]))) | is.logical(data[[var]])) v <- as.factor(data[[var]]) # before: no is.logical
  else v <- data[[var]]
  if (setequal(levels(v), c(T, F))) levels <- c(T) # before: not this line
  else if (is.null(annotation(v))) levels <- levels(v)
  else levels <- labels(v)@.Data
  levels <- levels[!(levels %in% c("NSP", "PNR", "Non concerné·e", "Included"))]
  if (rev_legend) levels <- rev(levels) # new (05/20)
  if (weights) N <- sum(data[['weight']][!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))])
  else N <- length(which(!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))))
  for (val in levels) { # before: no %in% nowhere below
    if (weights) mat <- c(mat, sum(data[['weight']][which(v==val)])/N)
    else mat <- c(mat, length(which(v==val))/N) }
  if (rev) mat <- rev(mat)
  if (miss) {
    if (is.null(annotation(v))) {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.na(v) | v %in% c("NSP", "Non concerné·e"))])/N)
      else mat <- c(mat, length(which(is.na(v) | v %in% c("NSP", "Non concerné·e")))/N)
    } else  {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.missing(v) & !is.na(v))])/N) # was defined without " & (!(v %in% c("NSP", "Non concerné·e")))" here and line below
      else mat <- c(mat, length(which(is.missing(v) & !is.na(v)))/N) } } # mais ça semble équivalent pck les NSP sont missing dans ces cas-là
  if (max(nchar(levels))==3 & 'Oui' %in% levels & 'Non' %in% levels) { if (which(levels=='Non') < which(levels=='Oui')) mat[2:1] <- mat[1:2]; levels[c(which(levels=='Oui'),which(levels=='Non'))] <- c('Non', 'Oui') }
  if ((return %in% c("levels", "legend")) & miss & fr==TRUE) return(c(levels, 'NSP'))
  else if ((return %in% c("levels", "legend")) & miss & (fr==FALSE)) return(c(levels, 'PNR'))
  else if ((return %in% c("levels", "legend")) & miss & is.character(fr)) return(c(levels, fr))
  else if ((return %in% c("levels", "legend")) & (!(miss))) return(levels)
  else if (return == "N") return(N)
  else return(matrix(mat, ncol=1))
}
dataKN <- function(vars, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE) {
  if (is.logical(data[[vars[1]]])) return(data1(vars, data, weights))
  else {
    res <- c()
    for (var in vars) res <- c(res, dataN(var, data, miss, weights, return, fr, rev))
    return(matrix(res, ncol=length(vars))) }
}
dataN2 <- function(var, df = list(c, e), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev))) }
dataN3 <- function(var, df = list(e2, e, c), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[3]], miss = miss, weights = weights, fr = fr, rev = rev))) }
data12 <- function(vars, df = list(e, e2), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (length(vars)==1) return(dataN2(var=vars, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
  else {
    init <- T 
    for (var in vars) {
      if (init) {
        data <- dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return)
        init <- F
      } else {
        data <- cbind(data, dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
      }
    }
    return(data)
  } }
barres12 <- function(vars, df=list(e, e2), labels, legend=hover, comp = "V2", orig = NULL, miss=T, weights = T, fr=F, rev=T, color=c(), rev_color = FALSE, hover=legend, sort=TRUE, thin=T, return="", showLegend=T) {
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  data1 <- dataKN(vars, data=df[[1]], miss=miss, weights = weights, return = "", fr=fr, rev=rev)
  if (missing(legend) & missing(hover)) { 
    if (is.logical(df[[1]][[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights) # before: uncommented and "else" next line
    else hover <- legend <- dataN(var = vars[1], data=df[[1]], miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } 
  agree <- order_agree(data = data1, miss = miss)
  if (is.logical(df[[1]][[vars[1]]])) agree <- rev(agree)
  if (return=="data") return(data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""))
  else if (return=="labels") return(labels12(labels[agree], en = !fr, comp = comp, orig = orig))
  else if (return=="legend") return(legend)
  else return(barres(data = data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""), 
                     labels=labels12(labels[agree], en = !fr, comp = comp, orig = orig), legend=legend, 
                     miss=miss, weights = weights, fr=fr, rev=rev, color=color, rev_color = rev_color, hover=hover, sort=F, thin=thin, showLegend=showLegend))
}

labels12 <- function(labels, en=F, comp = "V2", orig = NULL) {
  new_labels <- c()
  lab2 <- ifelse(comp=="V2", ifelse(en, "Wave 2 (W2)", "Vague 2 (V2)"), comp)
  lab1 <- ifelse(missing(orig), ifelse(en, "(W1)", "(V1)"), orig)
  for (l in labels) {
    new_labels <- c(new_labels, lab2, paste(l, lab1))
    lab2 <- paste("", lab2) }
  return(new_labels)
}
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') {
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
# accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
oui_non5 <- c("Non, pas du tout", "Non, pas vraiment", "Indifférent-e/NSP", "Oui, plutôt", "Oui, tout à fait")
yes_no5 <- c("Not at all", "Not really", "Indifferent/PNR", "Rather yes", "Yes, completely")
# agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
# evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
# evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
order_agree <- function(data, miss, rev = T, n = ncol(data)) {
  agree <- c()
  if (!missing(miss)) {
    if (miss) for (i in 1:n) agree <- c(agree, sum(data[floor(nrow(data)/2+1):max(1,(nrow(data)-1)),i]))
    else for (i in 1:n) agree <- c(agree, sum(data[ifelse(nrow(data)==1,1,ceiling(nrow(data)/2+1)):nrow(data),i]))
  } else {
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:n) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:n) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:n) { agree <- c(agree, data[1, i]) } } }
  return(order(agree, decreasing = rev)) }
barres <- function(data, vars, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE, 
                   display_values=T, thin=T, legend_x=NA, show_ticks=T, xrange=NA, save = FALSE, df=e, miss=T, weights = T, fr=F, rev=T, grouped = F, error_margin = F, color_margin = '#00000033', N = NA) {
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  if (missing(data) & !missing(vars)) {
    data <- dataKN(vars, data=df, miss=miss, weights = weights, return = "", fr=fr, rev=rev)
    N <- dataN(vars[1], data=df, miss=miss, weights = weights, return = "N")
    if (missing(legend) & missing(hover)) { 
      if (is.logical(df[[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights)
      else hover <- legend <- dataN(var = vars[1], data=df, miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } }
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15 # 10, 13
  legendY <- 1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data)==1) legendY = 1.5 + 0.3*thin
  if (sort) {
    order <- order_agree(data = data, miss = miss, rev = rev, n = length(labels))
    labels <- labels[order]
    data <- matrix(data[, order], nrow=nrow(data))
  }
  if (nrow(data)==1 & (sort | !showLegend)) {  # new: add !showLegend to manage responsable_CC i.e. comparisons of a multiple answer question
    if (!sort) order <- 1:length(labels)
    hover <- hover[order]
    value <- c()
    for (i in 1:length(hover)) { 
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(round(100*data[1, i]), '%', sep='') } # '%  '
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j])), '%', sep='')) # '%  '
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
        values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j])), '%', sep='')) # '%  '
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "NSP", "Included"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "NSP", "Included"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j]), '%', sep='')) # '%  '
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto', 
                  error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[1,]*(1-data[1,])/(N-1)), color = color_margin), # sort=FALSE, 
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0
    
    layout(xaxis = list(title = "",
                        showgrid = show_ticks,
                        showline = FALSE,
                        showticklabels = show_ticks,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5*show_ticks,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T, 
                        range = xrange,
                        domain = c(0.01 + 0.14*(!(" " %in% labels)), 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = ifelse(grouped, 'group', 'stack'),
    title = list(text = title, font = list(color = 'black')),
    # title = title,
    # titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
    # showlegend = (showLegend & !((("Yes" %in% legend) | ("Oui" %in% legend)) & (length(legend)<4)))) %>% 
    showlegend = (showLegend & !(setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP'))))) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%
  # Legend in the Yes/No case
  if ((setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')))) { 
    bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                     x = c(0.1, 0.9, 1.1),
                                     y = 1.5,
                                     text = legend,
                                     font = list(family = 'Arial', size = 16, color = 'black'),
                                     showarrow = FALSE) } # %>%
  # print(nrow(data))
  # print(hover)
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) { # evaluate=TRUE, 
    bars <- add_trace(bars, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]), 
                      error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[i,]*(1-data[i,])/(N-1)), color = color_margin)) # width thickness (in px)
  } }
  if (online) { api_create(bars, filename=file, sharing="public") }
  if (!missing(file) & save) save_plotly(bars, filename = file) # new
  return(bars)
}
# plot(1:3,1:3) # example
# dev.copy(png, filename="test.png") # save plot from R (not plotly)
# dev.off()
# orca(example, file = "image.png") # BEST METHOD, cf. below
save_plotly <- function(plot, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
  file <- paste(folder, filename, ".png", sep='')
  print(file)
  if (grepl('webshot', method)) { # four times faster: 2.5s (vs. 10s) but saves useless widgets and doesn't exactly respect the display
    saveWidget(politiques_1, 'temp.html')
    webshot('temp.html', file, delay = 0.1, vwidth = width, vheight = height)  
    file.remove('temp.html')}
  else orca(plot, file = file, width = width, height = height) # bug with encoding in Windows
  # else {
  #   server <- orca_serve() # doesn't work within a function because requires admin rights
  #   server$export(plot, file = file, width = width, height = height)
  #   server$close()
  # }
  if (trim) image_write(image_trim(image_read(file)), file)
}


##### Other #####
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }

# from http://pcwww.liv.ac.uk/~william/R/crosstab.r http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
Crosstab <- function (..., dec.places = NULL, type = NULL, style = "wide", row.vars = NULL, col.vars = NULL, percentages = TRUE,  addmargins = TRUE, subtotals=TRUE) {
  #Declare function used to convert frequency counts into relevant type of proportion or percentage
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }
  
  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  
  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))    
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)
  
  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types
  
  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables
  
  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct  
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count  
    type <- "frequency"
  }
  
  
  
  #Check for integrity of requested analysis and adjust values of function arguments as required
  
  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }
  
  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }
  
  if ((length(type)>1) & (subtotals==FALSE)) { 
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }
  
  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }
  
  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
  
  
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, Crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }
  
  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }
  
  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'
  
  args <- list(...)    
  
  if (length(args) > 1) {
    if (!all(sapply(args, is.factor))) 
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate Crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages", 
                    "addmargins", "subtotals")) if (is.null(get(opt))) 
                      assign(opt, eval(parse(text = paste("tbl$", opt, 
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }
  
  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))
  
  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }
  
  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (Crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency") 
    Crosstab <- tbl
  else 
    Crosstab <- mk.pcnt.tbl(tbl, type[1])
  
  
  #If multiple table types requested, create and add these to 
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(Crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency") 
        Crosstab <- tbl
      else Crosstab <- mk.pcnt.tbl(tbl, type[i])
      Crosstab <- as.data.frame.table(Crosstab)
      Crosstab[z] <- i
      tbldat <- rbind(tbldat, Crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    Crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(Crosstab))[z - 1] <- ""
  }
  
  
  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {
    
    vars <- c(row.vars,col.vars)
    
    if (length(type)==1) {
      if (type=="row.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else 
      { if (type=="column.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else 
        { if (type=="joint.pct") 
        { Crosstab <- addmargins(Crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        }
          else #must be total.pct OR frequency
          { Crosstab <- addmargins(Crosstab)
          tbl <- addmargins(tbl)
          }
        }
      } 
    }
    
    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }
    
  }  
  
  
  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    
    #Create version of Crosstab in ftable format
    t1 <- Crosstab 
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
    
    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
    
    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
    
    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]
    
    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
      }
    }
    
    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals
    
    t1 <- t1[((lab==0) | (lab==n.row.vars)),]
    
    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""
    
    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL
    
  }
  
  
  
  #Create output object 'result' [class: Crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals
  
  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$Crosstab <- Crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]  
  result$Crosstab.nosub <- t1  #Crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
  class(result) <- "Crosstab"    
  
  #Return 'result' as output of function
  result
  
}

print.Crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
  
  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  if (length(x$type)>1) {
    z<-length(names(dimnames(x$Crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z) 
    } else {
      col.vars<-c(z,col.vars)
    }
  }
  
  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$Crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$Crosstab,x$dec.places))
    }
  }
  
  
  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$Crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
    
    tbl <- ftable(x$Crosstab,row.vars=row.vars,col.vars=col.vars)
    
    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)
    
  }
  
  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$Crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {
    
    t1 <- x$Crosstab.nosub
    
    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
    
    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
    
    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }
    
    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
    
  }
  
}

inflate_for_miss <- function(v) return(c(v[1:(length(v)-1)]/(1-v[length(v)]), v[length(v)]))

close <- function(x, y, prec = 0.0001) return(all(abs(x - y) < prec))

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
rquery.wordcloud <- function(x, type=c("text", "url", "file"), lang="english", excludeWords=NULL, 
                            textStemming=FALSE,  colorPalette="Dark2", min.freq=3, max.words=200) { 
  # http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}
