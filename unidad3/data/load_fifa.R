feetToCm <- function(x){
  feet <- as.numeric(str_split(x, "'", simplify = T)[1])
  inch <- as.numeric(str_split(x, "'", simplify = T)[2])
  cm <- conv_unit(feet, from = "ft", to = "cm") +
    conv_unit(inch, from = "inch", to = "cm")
  cm
}

refactorMoney <- function(q){
  if(is.na(q)) return(0)
  if(q %>% str_detect("K")){
    return(1000 * q %>% str_replace("K","") %>% as.numeric()) 
  }
  if(q %>% str_detect("M")){
    return(1000000 * q %>% str_replace("M","") %>% as.numeric()) 
  }
  return(q %>% as.numeric())
}

simplificaPosicion <- function(data){
  x <- as.factor(data$Position)
  levels(x) <- list(GK  = c("GK"), 
                    DEF = c("LWB","RWB", "LB", "CB", "RB", "LCB","RCB"),
                    MID = c("LM","CDM","CM","CAM","RM","RCM","LCM","LAM","RAM","LDM","RDM"), 
                    FWD = c("RW","LW","CF", "RF", "LF", "ST","LS","RS"))
  data %>% mutate(Position = x)
}

#fifa %>% is.na %>% colSums()

load_fifa <- function(){
  read_csv("./data/fifa.csv") %>%
    mutate(
      Height = Height %>% map_dbl(feetToCm),
      Weight = Weight %>% str_remove("lbs") %>% as.numeric(),
      across(c(Wage,Value,`Release Clause`),~map_dbl(.,refactorMoney)),
      across(LS:RB,~replace_na(.,0)),
    ) %>% 
    select(-`Loaned From`) %>%
    drop_na() %>% simplificaPosicion() %>% 
    filter(Overall > 75)
} 