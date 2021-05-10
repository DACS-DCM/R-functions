FirstMed <- function(population,ATCcodes){
  print(ATCcodes)
  firstMed <- NULL
  for (year in 1995:2018) {
    meds <- readRDS(paste(population,year,".rds",sep = ""))%>%
      select(PNR, eksd, ATC)%>%
      filter(str_detect(ATC,paste(ATCcodes,collapse = "|")))
    print(paste(population, year))
    print(meds%>%select(ATC)%>%table())
    firstMed <- meds%>%
      select(PNR,eksd)%>%
      bind_rows(firstMed)%>%
      group_by(PNR)%>%
      mutate(first=first(eksd))%>%
      filter(eksd==first)%>%
      distinct()%>%
      ungroup()%>%
      select(PNR,eksd)
    
  }
  return(firstMed)
}

ADHDmedsB <- FirstMed("E:/workdata/707463/00Data/pop_lmdb",c("N06BA0[2479]","N06BA12","C02AC02"))
