##################################################################
# utilities.R
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

source("dispatch.R")

parse_file_names = function(file_name)
{
  err_msg = NA
  root = NA
  ext = NA
  
  file_name_new = gsub("\\.(?=\\.*$)", "", file_name, perl = T)
  if(file_name_new != file_name)
  {
    err_msg = "File name cannot end with .(dot)."
    ret = list(err_msg = err_msg, root = root, ext = ext)
    return(ret)
  }
  
  temp = strsplit(file_name, "\\.(?=[^\\.]+$)", perl=TRUE)[[1]]
  root = temp[1]
  if(length(temp) == 1)
  {
    root = ""
  }else
  {
    ext = temp[2]
  }
  
  if(!(ext %in% c("txt", "csv", "", "xlsx")))
  {
    err_msg = paste0("File extension ", ext, " is not supported.")
    ret = list(err_msg = err_msg, root = root, ext = ext)
    return(ret)
  }
  
  ret = list(err_msg = err_msg, root = root, ext = ext)
  
  return(ret)
}

readData = function(data_file, root, ext)
{
  require(xlsx)
  ret = NA
  
  if(ext %in% c("txt", ""))
  {
    ret = read.table(data_file, header = T, stringsAsFactors = F)
  }else if(ext == "csv")
  {
    ret = read.csv(data_file, header = T, stringsAsFactors = F)
  }else if(ext == "xlsx")
  {
    ret = read.xlsx(data_file, sheetIndex = 1, header = T, stringsAsFactors = F)
  }
  
  return(ret)
}

#################
# ret$meta_data: vector("list", length(roots)), meta_data[[ii]]: a character vector of variable types in each data file
#
#
#################
parse_meta_data = function(dat, root)
{
  err_msg = NA
  dat = dat
  meta_data = sapply(dat, FUN = class, simplify = T)
  names(meta_data) = colnames(dat)
  
  
  ret = list(err_msg = err_msg, dat = dat, meta_data = meta_data)
  
  file_names = dir()
  
  mdata_file_name = paste0(roots[ii], ".mdata")
  if (!(mdata_file_name %in% file_names))
  {
    err_msg = paste0("Meta data for ", roots[ii], " is not found.")
    ret$err_msg = err_msg
    return(ret)
  }
  
  temp = read.table(mdata_file_name, header = F)
  if (ncol(temp) != 2)
  {
    err_msg = paste0("Meta data for ", roots[ii], " has wrong format.")
    ret$err_msg = err_msg
    return(ret)
  }
  
  s = strsplit(temp[,2], split = ":")
  facs = rep(NA, nrow(temp))
  for(ii in 1:nrow(temp))
  {
    temp[ii,2] == s[[ii]][1]
    if(length(s[[ii]]) == 2)
    {
      facs[ii] = s[[ii]][2]
    }
  }
  if (sum(!( temp[,2] %in% c("numeric", "categorical", "ordinal", "time_to_event", "ID"))) != 0)
  {
    err_msg = paste0("Unrecognized variable type in meta-data for ", roots[ii], ".")
    ret$err_msg = err_msg
    return(ret)
  }
  
  meta_data[temp[,1]] = temp[,2]
  
  ret$meta_data = meta_data
  
  for(ii in 1:nrow(temp))
  {
    if(temp[ii, 2] %in% c("numeric", "ordinal"))
    {
      dat[,temp[ii, 1]] = as.numeric(dat[,temp[ii, 1]])
    }else if(temp[ii, 2] %in% c("time_to_event", "ID"))
    {
      dat[,temp[ii, 1]] = as.character(dat[,temp[ii, 1]])
    }else if(temp[ii, 2] %in% c("categorical"))
    {
      dat[,temp[ii, 1]] = factor(dat[,temp[ii, 1]], levels = facs)
    }
  }
  
  return(ret)
}

trim_space = function (x) gsub("^\\s+|\\s+$", "", x)

#################
# vars: a vector of variable names, can also be data file (root) names
# ret: matrix(nrow = length(vars), ncol = 3), ret[,1]: root name, ret[,2]: index of variable in the root, ret[,3]: variable type
# ret[,1]: NA is variable not found, ret[,2]: NA if this is a root name, ret[,3]: NA if this is a root name
#
#################
locate_var = function(vars, meta_data)
{
  ret = matrix(NA, nrow = length(vars), ncol = 3)
  rownames(ret) = vars
  for(ii in 1:length(vars))
  {
    if(substr(vars[ii], 1,1) == "[" && substr(vars[ii], length(vars[ii]),length(vars[ii])) == "]")
    {
      vars[ii] = substr(vars[ii], 2,length(vars[ii])-1)
      if(vars[ii] %in% names(meta_data))
      {
        idx = match(vars[ii], names(meta_data))
        ret[ii, 1] = names(meta_data)[idx]
      }
    }else
    {
      for(jj in 1:length(meta_data))
      {
        if(vars[ii] %in% names(meta_data[[jj]]))
        {
          idx = match(vars[ii], names(meta_data[[jj]]))
          ret[ii, 1] = names(meta_data)[jj]
          ret[ii, 2] = names(meta_data[[jj]])[idx]
          ret[ii, 3] = meta_data[[jj]][idx]
          
          break
        }
      }
    }
  }
  ######
  ######
  return(ret)
}

merge_data = function(params, data_list, meta_data)
{
  merged_data = NA
  err_msg = NA
  ret$err_msg = err_msg
  ret$merged_data = merged_data
  ret$vars_loc = NA
  
  params = gsub("(", "", params)
  params = gsub(")", "", params)
  params = unlist(strsplit(params, split = " "))
  
  vars_loc = locate_var(params, meta_data)
  na.idx = which(is.na(vars_loc[,1]))
  if(length(na.idx) !=  0)
  {
    err_msg = paste0(params[na.idx[1]], " not found.")
    ret$err_msg = err_msg
    return(ret)
  }
  
  roots = unique(vars_loc[,1])
  data_temp = list()
  for(ii in 1:length(roots))
  {
    temp = vars_loc[vars_loc[,1]==roots[1], , drop = F]
    if(!is.na(temp[,2]))
    {
      data_temp[[ii]] = data_list[[roots[ii]]]
    }else
    {
      data_temp[[ii]] = data_list[[roots[ii]]][, temp[,2]]
    }
  }
  
  if(length(data_temp) == 1)
  {
    ret$merged_data = data_temp[[1]]
    return(ret)
  }
  
  IDs = data_temp[[1]][,"ID"]
  for(ii in 1:length(data_temp))
  {
    IDs = intersect(IDs, data_temp[[ii]][, "ID"])
    if(max(table(data_temp[[ii]][, "ID"])) > 1)
    {
      err_msg = paste0("Repeated IDs in ", roots[ii], ".")
      ret$err_msg = err_msg
      return(ret)
    }
  }
  
  if(length(IDs) == 0)
  {
    err_msg = "No sample overlap between data sets."
    ret$err_msg = err_msg
    return(ret)
  }
  
  merged_data = data_temp[[1]][IDs,]
  for(ii in 2:length(data_temp))
  {
    merged_data = cbind(merged_data, data_temp[[2]][IDs, setdiff(colnames(data_temp[[2]], "ID"))])
  }
  
  ret$merged_data = merged_data
  ret$vars_loc = vars_loc
  
  return(ret)
}

dispatch_cmd = function(cmd, params, vars_loc)
{
  ret = list()
  ret$err_msg = NA
  ret$expr = NA
  
  if(cmd == "CORR")
  {
    temp = dispatch_CORR_cmd(params, vars_loc)
    ret$err_msg = temp$err_msg
    ret$expr = temp$expr
  }elseif(cmd == "SUMMARY")
  {
    temp = dispatch_SUMMARY_cmd
    temp = dispatch_CORR_cmd(params, vars_loc)
    ret$err_msg = temp$err_msg
    ret$expr = temp$expr
  }elseif(cmd == "INTERACTION")
  {
    temp = dispatch_INTERACTION_cmd(params, vars_loc)
    ret$err_msg = temp$err_msg
    ret$expr = temp$expr
  }else
  {
    err_msg = "Wrong command."
    ret$err_msg = err_msg
  }
  
  return(ret)
}

execute_stat = function(command, data_list, meta_data)
{
  err_msg = NA
  result = NA
  
  ret = list(err_msg = err_msg, result = result)
  
  temp = strsplit(command, split = ":", fixed = T)[[1]]
  temp = trim_space(temp)
  
  cmd = temp[1]
  
  if(!is.check_cmd(cmd))
  {
    err_msg = "Wrong command."
    ret$err_msg = err_msg
    return(ret)
  }
  
  if(length(temp) != 2)
  {
    err_msg = "Wrong format in the command."
    ret$err_msg = err_msg
    return(ret)
  }
  
  params = temp[2]
  params = strsplit(params, split = ",", fixed = F)[[1]]
  params = trim_space(params)
  
  temp = merge_data(params, data_list, meta_data)
  dat = temp$dat
  err_msg = temp$err_msg
  vars_loc = temp$vars_loc
  
  if(!is.na(err_msg))
  {
    ret$err_msg = err_msg
    return(ret)
  }
  
  cmd_expr = dispatch_cmd(cmd, params, vars_loc)
  
  #############
  #############
}

run_stats = function()
{
  setwd("data/")
  data_files = dir()
  temp = sapply(data_files, FUN = parse_file_names)
  
  err_msg = NA
  ret = list(err_msg = err_msg)
  if(sum(!sapply(temp["err_msg", ], FUN = is.na)) != 0)
  {
    err_msg = temp["err_msg", which(!sapply(temp["err_msg", ], FUN = is.na))[1]]
    ret$err_msg = err_msg
    return(ret)
  }
  
  roots = unlist(temp['root', ])
  exts = unlist(temp['ext', ])
  
  data_list = vector("list", length = length(roots))
  meta_data = vector("list", length = length(roots))
  
  names(data_list) = roots
  names(meta_data) = roots
  
  for(ii in 1:length(roots))
  {
    data_list[[ii]] = readData(data_files[ii], roots[ii], exts[ii])
  }
  
  setwd("../meta_data/")
  for(ii in 1:length(roots))
  {
    temp = parse_meta_data(data_list[[ii]], roots[ii])
    err_msg = temp$err_msg
    if(!is.na(err_msg))
    {
      ret$err_msg = err_msg
      return(ret)
    }
    
    data_list[[ii]] = temp$dat
    meta_data[[ii]] = temp$meta_data
  }

  setwd("../")
  commands = readLines("commands.txt")
  
  results = sapply(commands, FUN = execute_stat, data_list = data_list, meta_data = meta_data)
}




