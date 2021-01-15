# Examples
# Be careful with the arguments (df, miss...)

labels_responsible <- c()
for (v in variables_CC_responsible) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# In case you want to redefine the labels, it need not be automatized, e.g.:
# labels_responsible <- c("Each of us", "The rich", "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "CC doesn't exist")
(responsible_US <- barres(vars = variables_CC_responsible, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_responsible, hover=labels_responsible))
save_plotly(responsible_US) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=T, labels=labels_burden_sharing))
save_plotly(burden_sharing_US) 

(CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
save_plotly(CC_exists_US)
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 

