extract_json = function(df, col){
  df_n = data.frame(stringsAsFactors = F)
  
  for(i in 1:nrow(df)){
    df_n = cbind(
      df[i,],
      new_col = sapply(rjson::fromJSON(as.character(df[i, col])), function(x){x$name}), 
      value = 1
    ) %>% 
      plyr::rbind.fill(df_n)
  }
  
  df_n = df_n %>% 
    spread(new_col, value, fill = 0)
}

extract_json2 = function(df, col){
  df_n = data.frame(stringsAsFactors = F)
  for(i in 1:nrow(df)){
    st = as.character(df[i, col])
  #  string preprocessing for conflicts with " and '
    if (!str_starts(st, fixed('['))) st = paste0("[", st, "]")
    st = str_replace_all(st, fixed(": None"), ': "None"')
    st = str_replace_all(st, fixed('"s '), '-s ')
    st = str_replace_all(st, fixed('"s"'), '-s"')
    st = str_replace_all(st, fixed('s" '), 's ')
    st = str_replace_all(st, fixed(' d"'), ' d')
    st = str_replace_all(st, fixed(' D"'), ' d')
    st = str_replace_all(st, fixed(' O"'), ' o')
    st = str_replace_all(st, fixed(' o"'), ' o')
    st = str_replace_all(st, fixed(' l"'), ' l')
    st = str_replace_all(st, fixed(' L"'), ' l')
    st = str_replace_all(st, fixed(' "X"'), ' X')
    st = str_replace_all(st, fixed('&'), 'and')
    st = str_replace_all(st, fixed('\\'), ' ')
    if (str_detect(st, '\"\"[A-Z]')) st = str_replace_all(st, '\"\"[A-Z]', str_sub(str_extract_all(st, '\"\"[A-Z]'),3,3))
    if (str_detect(st, '\"\"[a-z]')) st = str_replace_all(st, '\"\"[a-z]', str_sub(str_extract_all(st, '\"\"[a-z]'),3,3))
    st = str_replace_all(st, fixed('"""'), '"')
    # end preprocessing
    temp = sapply(rjson::fromJSON(st), function(x){x$name})
    if (length(temp)!=0){
      df_n = cbind(
        df[i,],
        new_col = temp, 
        value = 1
      ) %>% 
        plyr::rbind.fill(df_n)
    }
 #   print(i)
  }
  names(df_n)[match("new_col", names(df_n))] =  paste0(col, "_sep") 
  names(df_n)[match("value", names(df_n))] =  paste0(col, "_v") 
  df_n
}