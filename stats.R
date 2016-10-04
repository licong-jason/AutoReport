##################################################################
# stats.R
# Description: This script defines the functions for the statistical analyses used
# Author: Cong Li
# Date: 09/14/2016
#
# Updates:
# ...
#
#
#
##################################################################

setwd("C:\\Users\\coli1\\Documents\\Dropbox\\Projects\\AutoCorr")
require(ggplot2)
require(glmnet)
require(survival)
source("misc.R")

###
# tab: a table of the results for each variable
#
# misc: some summary results 
#   N: total number of non-missing subjects
#   adj_r2: adjusted R-squared
#   F_pval: A p-value for the whole model using F-test
#   p: total number of variables
#
# fig: a ggplot2 object for the plot
#   
###
linear_regression = function(dat, x, y)
{
  expr = paste(y, "~", paste(x, collapse = "+"))
  
  model = lm(expr, data = dat)  
  
  tab = matrix(nrow = length(x), ncol = 4)
  rownames(tab) = x
  colnames(tab) = c("Coefficient", "Standard error", "Correlation", "P-value")
  tab = as.data.frame(tab)
  
  tab[,1] = summary(model)$coef[-1,1,drop = F]
  tab[,2] = summary(model)$coef[-1,2,drop = F]
  tab[,4] = summary(model)$coef[-1,4,drop = F]
  
  for(ii in 1:length(x))
  {
    tab[ii, 3] = cor(dat[, x[ii]], dat[, y])
  }
  
  if(length(x) > 1)
  {
    tab = cbind(tab, p.adjust(tab[,4], method = "BH"))
    colnames(tab)[5] = "FDR"
  }
  
  misc = list()
  
  misc$N = sum((apply(is.na(dat[, x]), MARGIN = 1, FUN = sum) + is.na(dat[, y])) == 0)
  misc$p = length(x)
  misc$adj_r2 = summary(model)$adj.r.squared
  
  fstat = summary(model)$fstatistic
  misc$F_pval = pf(q = fstat[1], df1 = fstat[2], df2 = fstat[3], lower.tail = F)
  
  ret = list()
  
  ret$tab = tab
  ret$misc = misc
  
  if(length(x) == 1)
  {
    fig_expr = paste0("ggplot(data = dat, aes(x = ", x, ", y = ", y,"))")
    fig_expr = paste0(fig_expr, " + geom_point(shape = 1)")
    fig_expr = paste0(fig_expr, " + geom_smooth(method = lm)")
    
    fig = eval(parse(text = fig_expr))
    ret$fig = fig
  }
  
  return(ret)
}


###
# tab: a table of the results for each variable
#
#
# figs: a list ggplot2 objects for each variable
#   
###
linear_regression_sep_each = function(dat_each)
{
  model = lm(dat_each[, 2] ~ dat_each[, 1])
  CI = confint(model)
  N = nrow(dat_each)
  coef = summary(model)$coef[2, 1]
  coef_L95 = CI[2, 1]
  coef_U95 = CI[2, 2]
  se = summary(model)$coef[2, 2]
  corr = cor(dat[, 1], dat[, 2])
  adj.r2 = summary(model)$adj.r.squared
  p.value = summary(model)$coef[2, 4]
  
  
  

  fig_expr = paste0("ggplot(data = dat, aes(x = ", x[ii], ", y = ", y,"))")
  fig_expr = paste0(fig_expr, " + geom_point(shape = 1)")
  fig_expr = paste0(fig_expr, " + geom_smooth(method = lm)")
    
  fig = eval(parse(text = fig_expr))
  
  
  ret = list()
  ret$N = N
  ret$coef = coef
  ret$coef_L95 = L95
  ret$coef_U95 = U95
  ret$coef_se = se
  ret$corr = corr
  ret$adj.r2 = adj.r2
  ret$p.value = p.value
  ret$fig = fig
  
  return(ret)
}


linear_regression_sep = function(dat, x, y)
{
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]], ", y, " = dat[, y])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii], y)
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = linear_regression_sep_each, simplify = F)
  return(ret)
}



###
# tab: a table of the results for each variable
#
# misc: some summary results 
#   N: total number of non-missing subjects
#   F_pval: A p-value for the whole model using F-test
#   p: total number of variables
#
# fig: a ggplot2 object for the plot
#   
###
logistic_regression = function(dat, x, y)
{
  expr = paste(y, "~", paste(x, collapse = "+"))
  
  model = glm(expr, data = dat, family = "binomial")  
  
  tab = matrix(nrow = length(x), ncol = 3)
  rownames(tab) = x
  colnames(tab) = c("Coefficient", "Standard error", "P-value")
  tab = as.data.frame(tab)
  
  tab[,1] = summary(model)$coef[-1,1,drop = F]
  tab[,2] = summary(model)$coef[-1,2,drop = F]
  tab[,3] = summary(model)$coef[-1,4,drop = F]
  
  if(length(x) > 1)
  {
    tab = cbind(tab, p.adjust(tab[,3], method = "BH"))
    colnames(tab)[4] = "FDR"
  }
  
  misc = list()
  
  misc$N = sum((apply(is.na(dat[, x, drop = F]), MARGIN = 1, FUN = sum) + is.na(dat[, y])) == 0)
  misc$p = length(x)
  misc$F_pval = pf(q = m$null.deviance - m$deviance, df1 = m$df.null - m$df.residual, df2 = m$df.residual, lower.tail = F)
  
  ret = list()
  
  ret$tab = tab
  ret$misc = misc
  
  if(length(x) == 1)
  {
    if("ID" %in% colnames(dat) && misc$N <= 20)
    {
      dat_temp = dat[order(dat[, y]), ]
      fig_expr = paste0("ggplot(data = dat_temp, aes(x = ", "ID", ", y = ", x,"))")
      fig_expr = paste0(fig_expr, " + geom_point(aes(color = ", y,"))")
    }else
    {
      fig_expr = paste0("ggplot(data = dat, aes(x = ", y, ", y = ", x,"))")
      fig_expr = paste0(fig_expr, " + geom_jitter(aes(color = ", y,"), height = 0)")
    }

    
    fig = eval(parse(text = fig_expr))
    ret$fig = fig
  }
  
  return(ret)
}


dat_each = data.frame(x = rnorm(100), y = sample(c(1, 0), replace = T, size = 100))
logistic_regression_sep_each = function(dat_each)
{
  model = glm(dat_each[, 2] ~ dat_each[, 1], family = "binomial")
  CI = confint(model)
  N = nrow(dat_each)
  coef = summary(model)$coef[2, 1]
  coef_L95 = CI[2, 1]
  coef_U95 = CI[2, 2]
  se = summary(model)$coef[2, 2]
  p.value = summary(model)$coef[2, 4]
  t.p.value = t.test(dat_each[dat_each[,2]==unique(dat_each[,2])[1], 1], dat_each[dat_each[,2]==unique(dat_each[,2])[2], 1])$p.value
  

  x = colnames(dat_each)[1]
  y = colnames(dat_each)[2]

  fig_ID = NULL
  if(N <= 20)
  {
      dat_temp = dat_each[order(dat_each[, 2]), ]
      dat_temp$ID = rownames(dat_each)
      fig_expr = paste0("ggplot(data = dat_temp, aes(x = ", "ID", ", y = ", x,"))")
      fig_expr = paste0(fig_expr, " + geom_point(aes(color = ", y,"))")
      fig_ID = eval(parse(text = fig_expr))
  }
  
  fig_expr = paste0("ggplot(data = dat, aes(x = ", y, ", y = ", x,"))")
  fig_expr = paste0(fig_expr, " + geom_jitter(aes(color = ", y,"), height = 0)")
  fig = eval(parse(text = fig_expr))
  
  
  ret = list()
  ret$N = N
  ret$coef = coef
  ret$coef_L95 = L95
  ret$coef_U95 = U95
  ret$se = se
  ret$p.value = p.value
  ret$t.p.value = t.p.value
  ret$fig = fig
  ret$fig_ID = fig_ID
  
  return(ret)
}

logistic_regression_sep = function(dat, x, y)
{
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]], ", y, " = dat[, y])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii], y)
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = logistic_regression_sep_each, simplify = F)
  return(ret)
}


dat_each = data.frame(x = sample(c("0", "1"), replace = T, size = 100), y = sample(c("0", "1"), replace = T, size = 100))
fisher_test_each = function(dat_each)
{
  c_tab = table(dat_each)
  
  temp = fisher.test(c_tab)
  N = nrow(dat_each)
  OR = temp$estimate
  OR_L95 = temp$conf.int[1]
  OR_U95 = temp$conf.int[2]
  p.value = temp$p.value
  
  ret = list()
  ret$c_tab = c_tab
  ret$N = N
  ret$OR = OR
  ret$OR_L95 = OR_L95
  ret$OR_U95 = OR_U95
  ret$p.value = p.value
  
  return(ret)
}

fisher_test = function(dat, x, y)
{
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]], ", y, " = dat[, y])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii], y)
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = fisher_test_each, simplify = F)
  return(ret)
}






x = factor(rbinom(100, 1, 0.5))
y = round(runif(100) * 100)
y = paste0(y, sample(c("+", ""), 100, replace = T))
dat_each = data.frame(x = x, y = y)
x = "x"
y = "y"
temp = dat_each[, y]
censor_idx = grep('\\+$', temp)
censor_flag = numeric(nrow(dat_each))
censor_flag[censor_idx] = 1

surv_time = as.numeric(gsub('\\+$', "", temp))
dat_each[, y] = Surv(surv_time, censor_flag == 0)

surv_analysis_each = function(dat_each)
{
  km = coxph(dat_each[, 2] ~ dat_each[, 1])
  N_all = km$n
  Event_all = km$nevent
  HR = coef(summary(km))[1, 2]
  p.value = coef(summary(km))[1, 5]
  HR_U95 = summary(km)$conf.int[4]
  HR_L95 = summary(km)$conf.int[3]
  
  Ns = NULL
  Events = NULL
  median_surv = NULL
  median_surv_U95 = NULL
  median_surv_L95 = NULL
  
  x = colnames(dat_each)[1]
  y = colnames(dat_each)[2]
  
  if(is.factor(dat_each[, 1]))
  {
    expr = paste0("survfit(", y, " ~ ", x, ", data = dat_each)")
    km = eval(parse(text = expr))
    temp = read.table(textConnection(capture.output(print(km))), skip = 3, header = F)
    Ns = temp[,2]
    Events = temp[,3]
    median_surv = temp[,4]
    median_surv_U95 = temp[,5]
    median_surv_L95 = temp[,6]
    
    fig_expr = paste0("ggsurv(km, xlab = '', main = ", y, ") + xlim(0, max(surv_time))")
    km_fig = eval(parse(text = fig_expr))
  }

  ret = list()
  
  ret$km_fig = km_fig
  ret$fig_expr = fig_expr
  ret$Ns = Ns
  ret$Events = Events
  ret$median_surv = median_surv
  ret$median_surv_U95 = median_surv_U95
  ret$median_surv_L95 = median_surv_L95
  
  ret$N_all = N_all
  ret$Event_all = Event_all
  ret$HR = HR
  ret$p.value = p.value
  ret$HR_U95 = HR_U95
  ret$HR_L95 = HR_L95
  
  return(ret)
}

surv_analysis = function(dat, x, y)
{
  temp = dat[, y]
  censor_idx = grep('\\+$', temp)
  censor_flag = numeric(nrow(dat))
  censor_flag[censor_idx] = 1
  
  surv_time = as.numeric(gsub('\\+$', "", temp))
  dat[, y] = Surv(surv_time, censor_flag == 0)
  
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]], ", y, " = dat[, y])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii], y)
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = surv_analysis_each, simplify = F)
  return(ret)
}


x = factor(rbinom(100, 1, 0.5))
z = factor(rbinom(100, 1, 0.5))
y = round(runif(100)*100)
y = paste0(y, sample(c("+", ""), 100, prob = c(0.1, 0.9), replace = T))
dat = data.frame(x = x, y = y, z = z)
x = "x"
y = "y"
z = "z"
dat_each = dat
surv_analysis_interaction_each = function(dat_each)
{
  km = coxph(dat_each[, 2] ~ dat_each[,1]*dat_each[,3])
  p.value = summary(km)$coef[3, 5]
  
  Ns = NULL
  Events = NULL
  median_surv = NULL
  median_surv_U95 = NULL
  median_surv_L95 = NULL
  HR = NULL
  HR_U95 = NULL
  HR_L95 = NULL
  fig = NULL
  
  x = colnames(dat_each)[1]
  y = colnames(dat_each)[2]
  z = colnames(dat_each)[3]
  
  if(is.factor(dat_each[, 1]))
  {
    fig = list()
    
    x_levels = levels(dat_each[, 1])
    z_levels = levels(dat_each[, 1])
    
    Ns = matrix(nrow = 2, ncol = 2)
    rownames(Ns) = z_levels
    colnames(Ns) = x_levels
    
    Events = matrix(nrow = 2, ncol = 2)
    rownames(Events) = z_levels
    colnames(Events) = x_levels
    
    median_surv = matrix(nrow = 2, ncol = 2)
    rownames(median_surv) = z_levels
    colnames(median_surv) = x_levels
    
    median_surv_U95 = matrix(nrow = 2, ncol = 2)
    rownames(median_surv_U95) = z_levels
    colnames(median_surv_U95) = x_levels
    
    median_surv_L95 = matrix(nrow = 2, ncol = 2)
    rownames(median_surv_L95) = z_levels
    colnames(median_surv_L95) = x_levels
    
    HR = matrix(nrow = 1, ncol = 2)
    colnames(HR) = x_levels
    
    HR_U95 = matrix(nrow = 1, ncol = 2)
    colnames(HR_U95) = x_levels
    
    HR_L95 = matrix(nrow = 1, ncol = 2)
    colnames(HR_L95) = x_levels
    
    for(jj in 1:length(x_levels))
    {
      expr = paste0("survfit(", y, " ~ ", z, ", data = dat_each[dat_each[,1] == x_levels[jj], ,drop = F])")
      km = eval(parse(text = expr))
      
      fig_title = paste0(y, ", ", x, " = ", x_levels[jj])
      fig_expr = paste0("ggsurv(km, xlab = '', main = fig_title) + xlim(0, max(surv_time))")
      fig[[jj]] = eval(parse(text = fig_expr)) 
      
      temp = read.table(textConnection(capture.output(print(km))), skip = 2)
      
      Ns[, jj] = temp[, 1]
      Events[, jj] = temp[, 2]
      median_surv[, jj] = temp[,3]
      median_surv_CI[, jj] = paste0("(", sprintf('%.3g', temp[,4]), ", ", sprintf('%.3g', temp[,5]),  ")")
      
      expr = paste0("coxph(", y, " ~ ",  z, ", data = dat_each[dat_each[,1] == x_levels[jj], ,drop = F])")
      km = eval(parse(text = expr))
      HR[jj] = summary(km)$coef[2]
      HR_U95[jj] = summary(km)$conf.int[3]
      HR_L95[jj] = summary(km)$conf.int[4]
    }
  }
  
  ret = list()
  
  ret$Ns = Ns
  ret$Events = Events 
  ret$median_surv = median_surv 
  ret$median_surv_U95 = median_surv_U95 
  ret$median_surv_L95 = median_surv_L95 
  ret$HR = HR
  ret$HR_U95 = HR_U95 
  ret$HR_L95 = HR_L95 
  ret$fig = fig 
  
  return(ret)
}


surv_analysis_interaction = function(dat, x, y, z)
{
  temp = dat[, y]
  censor_idx = grep('\\+$', temp)
  censor_flag = numeric(nrow(dat))
  censor_flag[censor_idx] = 1
  
  surv_time = as.numeric(gsub('\\+$', "", temp))
  dat[, y] = Surv(surv_time, censor_flag == 0)
  
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]], ", y, " = dat[, y], ", z, " = dat[, z])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii], y, z)
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = surv_analysis_interaction_each, simplify = F)
  return(ret)
}

continous_summary_each = function(dat_each)
{
  dat = data.frame(dat_each)
  m = mean(dat[,1], na.rm = T)
  med = median(dat[,1], na.rm = T)
  std = sd(dat[,1], na.rm = T)
  L95 = quantile(dat[,1], 0.025, na.rm = T)
  U95 = quantile(dat[,1], 0.025, na.rm = T)
  N = sum(!is.na(dat[,1]))
  
  fig_expr = paste0("ggplot(dat, aes(x = ",  colnames(dat)[1], ")) + geom_boxplot()")
  fig = eval(parse(text = fig_expr))
  
  ret = list()
  
  ret$m = m
  ret$med = med
  ret$std = std
  ret$L95 = L95
  ret$U95 = U95
  ret$N = N
  ret$fig = fig
  
  return(ret)
}

continuous_summary = function(dat, x)
{
  packed_dat = vector("list", length = length(x))
  
  for(ii in 1:length(x))
  {
    temp_expr = paste0("data.frame(", x[ii], " = dat[, x[ii]])")
    temp = eval(parse(text = temp_expr))
    colnames(temp) = c(x[ii])
    if("ID"%in%colnames(dat))
    {
      rownames(temp) = dat[, "ID"]
    }
    packed_dat[[ii]] = temp
  }
  
  ret = sapply(packed_dat, FUN = surv_analysis_each, simplify = F)
  return(ret)
}
