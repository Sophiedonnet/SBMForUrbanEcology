assignSeason <- function(dat, SeasonStarts=seasons) {
  dat %<>% mutate(
    Season = lapply(Date,
                    function(x) {
                      findInterval(
                        x, 
                        SeasonStarts[which(year(x)==year(SeasonStarts$WS)), ]
                      )
                    }
    ) %>% unlist    
  )
  dat[which(dat$Season==0 | dat$Season==4), ]$Season   <- "Winter"
  dat[which(dat$Season==1), ]$Season                  <- "Spring"
  dat[which(dat$Season==2), ]$Season                  <- "Summer"
  dat[which(dat$Season==3), ]$Season                  <- "Fall"
  return(dat)
}