library(tidyverse)


############ returns yes, if there is a single "yes" value in a vector ######
if_yes <- function(x){
  as.character( ifelse(any(x == "yes"), "yes", "no" ))
  
}

############ returns no, if there is a single "yes" value in a vector ######
if_no <- function(x){
  as.character(ifelse(any(x == "no"), "no", "yes" ))
  
}

################ Mode - if tie NA ##################
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (length(ta) == 2 & all(ta == tam))
    mod = NA
  else
    mod = names(ta)[ta == tam]
  return(as.character(mod))
}


################ mode if tie "No" ############3
Mode_if_tie_no = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (length(ta) == 2 & all(ta == tam))
    mod = "No"
  else
    mod = names(ta)[ta == tam]
  return(as.character(mod))
}


################ mode if tie both responses ############ 
Mode_if_tie_both = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (length(ta) == 2 & all(ta == tam))
    mod = as.character(paste(names(ta)[1], names(ta)[2], sep = ", "))
  else
    mod = names(ta)[ta == tam]
  return(as.character(mod))
}




################ mode if tie "yes" ######################
Mode_if_tie_yes = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (length(ta) == 2 & all(ta == tam))
    mod = "Yes"
  else
    mod = names(ta)[ta == tam]
  return(as.character(mod))
}


#################### Returns uniqu values in a vector ###################
present <- function(x){
  a <- x[!is.na(x)]
  paste(unique(a), collapse = " ")
  
}

########### returns Unique words in a charector string ##############
un_strip <- function(x){
paste(unique(unlist(strsplit(x, " "))), collapse = " " )
}


################# To-Do - Rank top 3 questions ######################