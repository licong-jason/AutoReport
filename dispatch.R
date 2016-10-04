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

dispatch_CORR_cmd = function(params, vars_loc)
{
  ret = list()
  ret$err_msg = NA
  ret$expr = NA
  
  if(length(params) == 2)
  {
    
  }elseif(length(params) == 3)
  {
    
  }else
  {
    ret$expr = paste0("Too many arguments.")
  }
  
  return(ret)
}