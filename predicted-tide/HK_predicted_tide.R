library(tidyverse)
#Method 1 ####
method1 = function(URL, URL2){
  Line1 = read_lines(URL)
  num1 = match("<TABLE cellSpacing=5 cellPadding=5 width=\"100%\" align=center border=0>", Line1)
  num2 = match('<DIV id=wcag_logo_area>', Line1)
  Line2 = Line1[(num1+5):(num2-8)]
  Line3 = as.data.frame(Line2) %>%
    mutate(filter = (grepl("_", Line2) | grepl("Date", Line2) | Line2 == "")) %>%
    filter(filter == F)
  
  Data1 = read_table(I(Line3$Line2)) %>%
    filter(MM != "MM") %>%
    select(-X27) %>%
    mutate(YYYY = year) %>%
    relocate(YYYY, .before = everything()) %>%
    lapply(as.numeric) %>%
    as_tibble() %>%
    pivot_longer(cols = -c("YYYY", "MM", "DD")) %>%
    mutate(time = ISOdatetime(YYYY, MM, DD, name, 00, 00, tz = "HongKong")) %>%
    rename(height = value) %>%
    select(time, height)
  
  Line1 = read_lines(URL2)
  num1 = match("<TABLE cellSpacing=5 cellPadding=5 width=\"100%\" align=center border=0>", Line1)
  num2 = match('<DIV id=wcag_logo_area>', Line1)
  Line2 = Line1[(num1+5):(num2-7)]
  Line3 = as.data.frame(Line2) %>%
    mutate(filter = (grepl("_", Line2) | grepl("Date", Line2) | Line2 == "")) %>%
    filter(filter == F)
  
  Data2 = tibble(YYYY = year,
                 MM = substr(Line3$Line2,  1, 2) %>% as.numeric(),
                 DD = substr(Line3$Line2,  5, 6) %>% as.numeric(),
                 hh1 = substr(Line3$Line2, 12,13) %>% as.numeric(),
                 mm1 = substr(Line3$Line2, 14,15) %>% as.numeric(),
                 height1 = substr(Line3$Line2, 16,25) %>% as.numeric(),
                 hh2 = substr(Line3$Line2, 28, 29) %>% as.numeric(),
                 mm2 = substr(Line3$Line2, 30, 31) %>% as.numeric(),
                 height2 = substr(Line3$Line2, 35,38) %>% as.numeric(),
                 
                 
                 hh3 = substr(Line3$Line2, 44, 45) %>% as.numeric(),
                 mm3 = substr(Line3$Line2, 46, 47) %>% as.numeric(),
                 height3 = substr(Line3$Line2, 51,54) %>% as.numeric(),
                 hh4 = substr(Line3$Line2, 60, 61) %>% as.numeric(),
                 mm4 = substr(Line3$Line2, 62, 63) %>% as.numeric(),
                 height4 = substr(Line3$Line2, 64,999) %>% as.numeric()) %>%
    mutate(time1 = ISOdatetime(YYYY, MM, DD, hh1, mm1, 00, tz = "HongKong"),
           time2 = ISOdatetime(YYYY, MM, DD, hh2, mm2, 00, tz = "HongKong"),
           time3 = ISOdatetime(YYYY, MM, DD, hh3, mm3, 00, tz = "HongKong"),
           time4 = ISOdatetime(YYYY, MM, DD, hh4, mm4, 00, tz = "HongKong")) %>%
    select(-c(YYYY, MM, DD, hh1, mm1, hh2, mm2, hh3, mm3, hh4, mm4))
  Data3 = tibble(time   = Data2$time1,
                 height = Data2$height1) %>%
    bind_rows(tibble(time   = Data2$time2,
                     height = Data2$height2)) %>%
    bind_rows(tibble(time   = Data2$time3,
                     height = Data2$height3)) %>%
    bind_rows(tibble(time   = Data2$time4,
                     height = Data2$height4)) %>%
    arrange(time) %>%
    distinct()
  
  Data4 = bind_rows(Data1, Data3) %>%
    distinct() %>%
    arrange(time) %>%
    mutate(station = station,
           type = "tide_predict") %>%
    relocate(station, type, .before = everything()) %>%
    drop_na()
  write_csv(x = Data4, file = paste0(station, "_", year, ".csv"))
  
  
  #          #         #         #        #         #          #
  # MM  DD     hhmm   HHHH     hhmm   HHHH     hhmm   HHHH     hhmm   HHHH
  #01234567890123456789012345678901234567890123456789012345678901234567890
  #"12  30     0425   0.53     1123   1.52     1516   1.24     2125   2.44"
}

#Method 2 ####
method2 = function(URL, URL2){
  Line1 = read_lines(URL)
  Line2 = Line1 %>%
    as_tibble() %>%
    mutate(filter = grepl("<TD>", value) | grepl("<TH>", value) ) %>%
    filter(filter == T) %>%
    mutate(value = str_remove(string = .$value, pattern =  "<TR>")) %>%
    mutate(value = str_replace_all(string = .$value, pattern = "<TD>", replacement =" ")) %>%
    mutate(value = str_replace_all(string = .$value, pattern = '</TD>', replacement =" ")) %>%
    mutate(value = str_remove(string = .$value, pattern =  "</TR>")) %>%
    mutate(filter = grepl("<TH>", value)) %>%
    mutate(value = str_replace_all(string = .$value, pattern = '<TH>', replacement =" ")) %>%
    mutate(value = str_replace_all(string = .$value, pattern = '</TH>', replacement =" ")) %>%
    mutate(value= str_remove(string = .$value, pattern =  "&nbsp;")) %>%
    filter(filter == F) %>%
    bind_rows(tibble(value = " MM  DD    01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24 "), .)
  
  Data1 = read_table(file = I(Line2$value)) %>%
    select(-X27) %>%
    pivot_longer(cols = -c("MM", "DD"), names_to = "hh", values_to = "height") %>%
    mutate(time = ISOdatetime(year, MM, DD, hh, 00, 00, tz = "HongKong")) %>%
    select(time, height) %>%
    arrange(time) %>%
    distinct()
  
  Line1 = read_lines(URL2)
  Line2 = Line1 %>%
    as_tibble() %>%
    mutate(filter = grepl("<TD>", value) | grepl("<TH>", value) ) %>%
    filter(filter == T) %>%
    mutate(value = str_remove(string = .$value, pattern =  "<TR>")) %>%
    mutate(value = str_replace_all(string = .$value, pattern = "<TD>", replacement =" ")) %>%
    mutate(value = str_replace_all(string = .$value, pattern = '</TD>', replacement =" ")) %>%
    mutate(value = str_remove_all(string = .$value, pattern =  "</TR>")) %>%
    mutate(value = str_remove_all(string = .$value, pattern = "nbsp")) %>%
    mutate(value = str_remove_all(string = .$value, pattern = ";")) %>%
    mutate(value = str_remove_all(string = .$value, pattern = "&")) %>%
    mutate(filter = !(str_length(value) > 80)) %>%
    filter(filter == T) %>%
    bind_rows(tibble(value = "MM DD t1 h1 t2 h2 t3 h3 t4 h4"), .)
  
  Data2 = read_table(I(Line2$value)) %>%
    mutate(hh1 = substr(t1, 1, 2),
           mm1 = substr(t1, 3, 4),
           hh2 = substr(t2, 1, 2),
           mm2 = substr(t2, 3, 4),
           hh3 = substr(t3, 1, 2),
           mm3 = substr(t3, 3, 4),
           hh4 = substr(t4, 1, 2),
           mm4 = substr(t4, 3, 4),
           time1 = ISOdatetime(year, MM, DD, hh1, mm1, 00, tz = "HongKong"),
           time2 = ISOdatetime(year, MM, DD, hh2, mm2, 00, tz = "HongKong"),
           time3 = ISOdatetime(year, MM, DD, hh3, mm3, 00, tz = "HongKong"),
           time4 = ISOdatetime(year, MM, DD, hh4, mm4, 00, tz = "HongKong")) %>%
    select(time1, time2, time3, time4,
           h1, h2, h3, h4)
  Data3 = tibble(time   = Data2$time1,
                 height = Data2$h1) %>%
    bind_rows(tibble(time   = Data2$time2,
                     height = Data2$h2)) %>%
    bind_rows(tibble(time   = Data2$time3,
                     height = Data2$h3)) %>%
    bind_rows(tibble(time   = Data2$time4,
                     height = Data2$h4)) %>%
    arrange(time) %>%
    distinct()
  
  Data4 = bind_rows(Data1, Data3) %>%
    distinct() %>%
    arrange(time) %>%
    mutate(station = station,
           type = "tide_predict") %>%
    relocate(station, type, .before = everything()) %>%
    drop_na()
  write_csv(x = Data4, file = paste0(station, "_", year, ".csv"))
}

#Control Panel ####
station_list = c("CLK", "CCH", "CMW", "KLW", "KCT",
                 "LOP", "MWC", "QUB", "SPW", "TMW",
                 "TAO", "TPK", "TBT", "WAG")

for(year in 2015:2025){
  for(station in station_list){
    URL = paste0("https://www.hko.gov.hk/tide/", station, "textPH", year, ".htm")
    URL2= paste0("https://www.hko.gov.hk/tide/e",station,"text",year,".html")
    
    if(year >= 2020){method2(URL = URL, URL2 = URL2)} 
    else {method1(URL = URL, URL2 = URL2)}
  }
}

#2020 - 2025: Use method 2
#2015 - 2019: Use method 1