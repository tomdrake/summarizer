hr.plot = function(df, dependent, explanatory, dependent_label = NULL, factorlist=NULL, coxfit=NULL, frailty_cluster_off = F, column_space=c(-0.5, 0, 0.5), ...){
  require(ggplot2)
  require(scales)
  # Generate or format factorlist object
  if(is.null(factorlist)){
    factorlist = summary.factorlist(df, dependent, explanatory, glm.id=TRUE)
    if (frailty_cluster_off == FALSE){
    factorlist = summary.factorlist(df, dependent, explanatory, glm.id=TRUE)
    factorlist = factorlist[!grepl("cluster", factorlist$glm.id),]#remove if using this word
    factorlist = factorlist[!grepl("frailty", factorlist$glm.id),]#remove if using this word
    }
  }
  
  # Extract totals (this is CPH specific due to how summary.factorlist works)
  factorlist$Total = as.numeric(str_extract(as.character(factorlist$all), "^[:digit:]*"))
  factorlist$all = NULL
  
  # Generate or format glm
  if(is.null(coxfit)){
    coxfit = coxphmulti(df, dependent, explanatory)
  }
  df_fit_c = fit2df(coxfit, condense = TRUE, estimate.suffix = " (multivariable)", ...)
  df_fit = fit2df(coxfit, condense = FALSE, ...)
  
  # Merge
  df.out = summarizer.merge(factorlist, df_fit_c)
  df.out = summarizer.merge(df.out, df_fit, ref.symbol = "1.0")
  
  # Fill in total for continuous variables (NA by default)
  df.out$Total[df.out$levels == "Mean (SD)" | df.out$levels == "Median (IQR)"] = dim(df)[1]
  
  # Remove unwanted lines, where there are more variables in model than wish to display.
  # Note merge function in summarizer merge is now `all` rather than `all.x` as wish to preserve interactions
  # These not named in factorlist, creating this problem. Interactions don't show on plot.
  if (any(
    is.na(df.out$label)
  )
  ){
    remove_rows = which(is.na(df.out$label)) # This row doesn't work when is.na == FALSE, hence if()
    df.out = df.out[-remove_rows,]
  } else {
    df.out
  }
  
  # Fix order
  df.out$levels = as.character(df.out$levels)
  df.out$glm.id = factor(df.out$glm.id, levels = df.out$glm.id[order(-df.out$index)])
  
  # Plot
  g1 = ggplot(df.out, aes(x = as.numeric(HR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
                          y = glm.id))+
    geom_point(aes(size = Total), shape=22, fill="darkblue")+
    geom_errorbarh(height=0.2) +
    geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
    scale_x_continuous(name="Hazard ratio (95% CI, log scale)", trans="log10", breaks= pretty_breaks())+
    theme_classic(14)+
    theme(axis.title.x = element_text(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position="none")
  
  t1 = ggplot(df.out, aes(x = as.numeric(HR), y = glm.id))+
    annotate("text", x = column_space[1], y =  df.out$glm.id, label=df.out[,2], hjust=0, size=5)+
    annotate("text", x = column_space[2], y =  df.out$glm.id, label=df.out[,3], hjust=1, size=5)+
    annotate("text", x = column_space[3], y =  df.out$glm.id, label=df.out[,6], hjust=1, size=5)+
    theme_classic(14)+
    theme(axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())
  
  if (is.null(dependent_label)){
    title = paste0(dependent, ": ", "(HR, 95% CI, p-value)")
  } else {
    title = paste0(dependent_label, ": ", "(HR, 95% CI, p-value)")
  }
  
  gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
                          top=grid::textGrob(title, x=0.02, y=0.2, gp=grid::gpar(fontsize=18), just="left"))
}


