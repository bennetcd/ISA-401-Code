# ISA-401-Code
R code for scraping football

library(rvest)
library(magrittr)
library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(data.table)

#url <- "http://www.pro-football-reference.com/players/"

get_and_clean_table <- function(player, id) {
  #player <<- readline(prompt = "Player:")
  url <- "http://www.pro-football-reference.com/players/"
  player <- tolower(id)
  fst <- substr(id, 0,1)
    
  full_url <- paste(url,fst,'/',id,".htm", sep = "")
  
  print(full_url)
  
  url_try <- try(get_name <- read_html(full_url) %>%
                   html_node("h1") %>%
                   html_text() %>%
                   substr(0,nchar(.)), silent = TRUE)
  
  tbl_len <- length(web_data <- read_html(full_url) %>%
    html_table(header = TRUE) %>%
    as.data.frame())
  
  
  if(tbl_len < 2){
    web_data <- "skip"
  }
  else{


  if(str_sub(get_name,-1) == " "){
    get_name <- substr(get_name, 0, nchar(get_name) - 1)
  }
    web_data <- read_html(full_url) %>%
    html_table(header = TRUE) %>%
    as.data.frame()
    
    #colnames(web_data) <- web_data[1, ] 
    #web_data <- web_data[-1, ]
    
    plyr_name <- c(rep(id, nrow(web_data)))

    web_data <- cbind.data.frame(plyr_name, web_data)
  }
  
  
  return(web_data)
  }
  
df <- read.csv(file = "nfl_draft.csv")

players <- as.character(df$Player)

ids <- as.character(df$Player_Id)

plyr_list <- data.frame()

QB_list <- data.frame()

RB_list <- data.frame()

WR_TE_list <- data.frame()

Def_list <- data.frame()

Off_list <- data.frame()

P_K_list <- data.frame()

nofit_plyrs <- list()
#2781
for (i in 2781:8435){

  player <- players[i]
  
  id <- ids[i]
  
  print(player)
  print(id)
  
  if(id == ""){print(paste(player, "Skipped", sep = " "))}
  else{
  
  another_flag <- try(
  n_tbl <- get_and_clean_table(player, id)
  )
  
  
  vari <- try(if("Var.1" %in% colnames(n_tbl)){
    colnames(n_tbl) <- n_tbl[1, ] 
    n_tbl <- n_tbl[-1, ]
  })

  otr_flag <- try( n_tbl$Pos <- toupper(n_tbl$Pos), silent = TRUE)
  
  
  
  if(class(otr_flag) != "try-error"){
  if(n_tbl == "skip"){
    print(paste(player, "Skipped", sep = " "))
  }
  else if(length(n_tbl) < 11){
    print(paste(player, "Skipped", sep = " "))
  }
  else{
  for (i in 1:nrow(n_tbl)){
  
    #row_flag <- try( 
    
    if(n_tbl$Pos[i] %in% c("RB","FB")){
      if(length(n_tbl) == 28){
      RB_list <- rbind(RB_list, n_tbl[i,])
      print("RB")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    else if(n_tbl$Pos[i] %in% c("QB")){
      if(length(n_tbl) == 32){
      QB_list <- rbind(QB_list, n_tbl[i,])
      print("QB")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    else if(n_tbl$Pos[i] %in% c("WR","TE")){
      if(length(n_tbl) == 28){
      WR_TE_list <- rbind(WR_TE_list, n_tbl[i,])
      print("WRTE")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    else if("Tkl" %in% colnames(n_tbl)){
      if(length(n_tbl) == 23){
     Def_list <- rbind(Def_list, n_tbl[i,])
     print("Def")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    else if(n_tbl$Pos[i] %in% c("C","T","G","LS")){
      if(length(n_tbl) == 9){
      Off_list <- rbind(Off_list, n_tbl[i,])
      print("Off")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    else if(n_tbl$Pos[i] %in% c("K","P","PK","P/K")){
      if(length(n_tbl) == 31){
      P_K_list <- rbind(P_K_list, n_tbl[i,])
      print("PK")}
      else{nofit_plyrs <- append(nofit_plyrs, player)}
    }
    }

  }}
  else{
    nofit_plyrs <- append(nofit_plyrs, player)
  }

  }
}

write.csv(x = QB_list, file = "QB_NFL")
write.csv(x = RB_list, file = "RB_NFL")
write.csv(x = WR_TE_list, file = "WRTE_NFL")
write.csv(x = Def_list, file = "Def_NFL")
write.csv(x = P_K_list, file = "PK_NFL")

#n_tbl$Year <- substr(n_tbl$Year,0,4)

#g_tbl <- n_tbl[grep(pattern = "[0-9]|[C-c]", x = n_tbl$Year),]

#full_url <- "http://www.pro-football-reference.com/players/P/PaytWa00.htm"

#player_ID <- regexpr(pattern = "[A-Z][a-z]{2,}[A-Z].{1,}[0-9][0-9]", text = full_url)
#player_ID <- regmatches(x = full_url, m = player_ID)



