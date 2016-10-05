##################################################################
# dispatch.R
# Description: This script defines the functions for the statistical analyses used
# Author: Cong Li
# Date: 10/04/2016
#
# Updates:
# ...
#
#
#
##################################################################

setwd("C:\\Users\\coli1\\Documents\\Dropbox\\Projects\\AutoCorr")

check_parathesis = function(x)
{
  flag = F
  if(substr(x, 1, 1) == "(" && substr(x, length(x), length(x)) == ")")
  {
    flag = T
    x = substr(2, length(x)-1)
  }
  
  ret = list()
  
  ret$flag = flag
  ret$x = x
}

check_bracket = function(x)
{
  flag = F
  if(substr(x, 1, 1) == "[" && substr(x, length(x), length(x)) == "]")
  {
    flag = T
    x = substr(2, length(x)-1)
  }
  
  ret = list()
  
  ret$flag = flag
  ret$x = x
}

dispatch_CORR_cmd = function(params, vars_loc, meta_data)
{
  
  #### still need to deal with variables in []
  
  ret = list()
  ret$err_msg = NA
  ret$expr = NA
  
  if(length(params) == 1)
  {
    ret$expr = paste0("Too few arguments.")
    return(ret)
  }else if(length(params) >= 2)
  {
    x = params[1]
    y = params[2]
    
    temp = check_parathesis(y)
    if(temp$flag)
    {
      ret$err_msg = "Response variable cannot be in parathesis."
      return(ret)
    }
    
    y = strsplit(y, split = " ")[[1]]
    if(length(y) != 1)
    {
      ret$err_msg = "Only 1 response variable is allowed."
      return(ret)
    }
    
    temp = check_parathesis(x)
    x = strsplit(temp$x, split = " ")[[1]]
    x = x[x != ""]
    each_flag = !temp$flag
    
    if( sum(!(vars_loc[x, 3] %in% c("numeric", "categorical"))) != 0 )
    {
      ret$err_msg = "Independent variables can only be numerical or categorical."
      return(ret)
    }
  }
  
  
  
  if(length(params) == 2)
  {
    if(vars_loc[y, 3] == "numeric")
    {
      if(each_flag)
      {
        expr = paste0("result = linear_regression_sep(dat, ", x, ", ", y, ")")
      }else
      {
        expr = paste0("result = linear_regression(dat, ", x, ", ", y, ")")
      }
    }else if(vars_loc[y, 3] == "binomial")
    {
      if(each_flag)
      {
        #### logistic regression
      }else
      {
        #### logistic regression for each
      }
    }else if(vars_loc[y, 3] == "time_to_event")
    {
      if(each_flag)
      {
        #### Cox regression
      }else
      {
        #### Cox regression for each
      }
    }
    
  }else if(length(params) == 3)
  {
    z = params[3]
    temp = check_parathesis(z)
    if(temp$flag)
    {
      ret$err_msg = "Response variable cannot be in parathesis."
      return(ret)
    }
    
    if( sum(!(vars_loc[z, 3] %in% c("numeric", "categorical"))) != 0 )
    {
      ret$err_msg = "Covariates variables can only be numerical or categorical."
      return(ret)
    }
    
    if(vars_loc[y, 3] == "numeric")
    {
      if(each_flag)
      {
        expr = paste0("result = linear_regression_sep_with_covar(dat, ", x, ", ", y, ")")
      }else
      {
        expr = paste0("result = linear_regression_with_covar(dat, ", x, ", ", y, ")")
      }
    }else if(vars_loc[y, 3] == "binomial")
    {
      if(each_flag)
      {
        #### logistic regression with covariates
      }else
      {
        #### logistic regression for each with covariates
      }
    }else if(vars_loc[y, 3] == "time_to_event")
    {
      if(each_flag)
      {
        #### Cox regression with covariates
      }else
      {
        #### Cox regression for each with covariates
      }
    }
    
  }else
  {
    ret$expr = paste0("Too many arguments.")
  }
  
  return(ret)
}