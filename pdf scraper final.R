######################## PDF SCRAPER ####################################
##########POWERED BY ERTAN AGAOGLU SOFTWARE INC. CORP. CO. :))) #########


#!!! please install required packages if necessary !!! #

library(textreadr)
library(tidyverse)
library (dplyr)
library(openxlsx)
library(stringr)

# 3262-1960 # 3261

# CASE INFO
# buraya coder id'niz ve dosya numarasi gelecek

setwd("D:\\data")
coderid = "3"
casenumber = "3256"
filename <- sprintf("%s.pdf", casenumber)
adress <- sprintf("http://jurisprudence.tas-cas.org/Shared Documents/%s.pdf", casenumber)

# INITIALIZE
# çiktilar kontrole ihtiyaç duydugu için numarasi girilen dosyayi web tarayicida açiyor 
browseURL(url = paste(adress))


# FUNCTIONS 
# burasi her bir degisken için özel olarak olusturulmus fonksiyonlar. 
# text çekmek için kullaniliyor.

loc <-function(var, regex, n = 1, ignore.case = TRUE){
  locs <- grep(regex, var, ignore.case = ignore.case)
  out <- sort(unique(c(locs)))
  out <- out[out > 0]
  out[out <= length(var)]
}

locs2 <-function(var, regex, n = 1, ignore.case = TRUE){
  locs2 <- grep(regex, var, ignore.case = ignore.case)
  out <- sort(unique(c(locs2, locs2 + 1)))
  out <- out[out > 0]
  out[out <= length(var)]
}

loc4par <- function(var, regex, n = 1, ignore.case = FALSE){
  locs4par <- grep(regex, var, ignore.case = ignore.case)
  out <- sort(unique(c(locs4par +1, locs4par+2, locs4par + 3, locs4par + 4, locs4par + 5, locs4par + 6, locs4par + 7,locs4par + 8, locs4par + 9, locs4par + 10, locs4par + 11, locs4par + 12, locs4par + 13, locs4par + 14, locs4par + 15, locs4par + 16, locs4par + 17, locs4par + 18, locs4par + 19, locs4par + 20, locs4par + 21, locs4par + 22, locs4par + 23, locs4par + 24)))
  out <- out[out > 0]
  out[out <= length(var)]
}

locsotg <-function(var, regex, n = 1, ignore.case = TRUE){
  locsotg <- grep(regex, var, ignore.case = ignore.case)
  out <- sort(unique(c(locsotg, locsotg + 1, locsotg + 2, locsotg + 3, locsotg + 4, locsotg + 5, locsotg +6, locsotg +7, locsotg +8, locsotg +9, locsotg+10, locsotg+11, locsotg+12, locsotg+13)))
  out <- out[out > 0]
  out[out <= length(var)]
}

# pdf'lerde tarihler yazili olarak belirtildigi için bu kisim numerik formata çeviriyor

dateformat <- function(date) {
  datenew <- str_replace(date,"January", "01") %>%
    str_replace(fixed("February", ignore_case = TRUE), "02") %>%
    str_replace("March", "03") %>%
    str_replace("April", "04") %>%
    str_replace("May", "05") %>%
    str_replace("June", "06") %>%
    str_replace("July", "07") %>%
    str_replace("August", "08") %>%
    str_replace("September", "09") %>%
    str_replace("October", "10") %>%
    str_replace("November", "11") %>%
    str_replace("July", "12") %>%
    str_replace(" ", ".") %>% str_replace(" ", ".")
  
  return(datenew)
}


# GET AWARD DATE #

awardraw <- adress %>%
  read_pdf() %>%
  slice(loc(text, 'award of')) 

awarddate <- awardraw[3,] 
awarddate <- awarddate[-c(1,2)] %>% 
  toString(awarddate, width = 0) %>%
  str_remove("award of ") %>%
  dateformat()

# GET OPERATIVE DATE


operatedraw <- adress %>%
  read_pdf() %>%
  slice(loc(text, 'operative part of \\d'))

operativedate <- operatedraw[3,] 
operativedate <- operativedate[-c(1,2)] %>% 
  toString(operativedate, width = 0) %>%
  str_remove("\\)") %>% str_remove("\\(") %>% 
  str_remove("operative part of ") %>% 
  dateformat()

# GET PANEL INFORMATION #


panelraw <- adress %>%
  read_pdf() %>%
  slice(locs2(text, 'Panel:')) 
panelraw <- panelraw[-c(1,2)]
panelraw <- paste(unlist(panelraw), collapse =" ") %>%
  str_remove("Panel: ") 

## president
presidentraw <- word(panelraw, 1, sep = fixed(','))
president <- word(presidentraw, sep = fixed("("))
presidentcountry <- word(presidentraw, 2, sep = fixed('(')) %>%
  str_remove("\\)")

# panel members
therest <- word(panelraw, 2, sep = fixed('President')) %>%
  str_remove("; ") 
r1panel <- word(therest, 1, sep = fixed(';'))
r2panel <- word(therest, 2, sep = fixed('; '))

# panel member 1
m1panel <- word(r1panel, 1, sep = fixed('('))
m1panelc <- word(r1panel, 2, sep = fixed('(')) %>%
  str_remove("\\)")

# panel member 3

m2panel <- word(r2panel, 1, sep = fixed('('))
m2panelc <- word(r2panel, 2, sep = fixed('(')) %>%
  str_remove("\\)")


# GET PARTIES 
## en büyük sikintimiz burada. bazi seyleri hala çözemedim. 
#Örnegin is'den sonrasini al dedigimde, "Turkish" olursa burada sorun çikariyordu, bunu hallettim
# ikinci is'den sonrasini alamiyor. Parties farkli formatlarda girildigi için
#tek bir script ile almak oldukça zor. ugrasilmasi gerek.


pr <- adress %>%
  read_pdf()  %>%
  slice(loc4par(text, 'PARTIES'))
pr <- pr[-c(1,2)]
pr <- paste(unlist(pr), collapse =" ") %>% toString()
pr <- unlist(strsplit(pr, "\\d\\.\\s"))
#
appellantint <- pr[2] %>% toString()
appellantint <- word(appellantint, 1, sep=fixed("."))

appellant <- sub(".*is", "", appellantint) 

#
respondant <- pr[3] %>% toString() %>% word(1, sep=fixed("."))
respondant <- sub(".*is", "", respondant)  
respondant <- paste(respondant, ".", sep="")


## GET CONCLUSIONS

conclusionraw <- adress %>%
  read_pdf()  %>%
  slice(loc4par(text, 'CONCLUSION'))
conclusionraw <- conclusionraw[-c(1,2)]
conclusionraw <- paste(unlist(conclusionraw), collapse =" ")
toString(conclusionraw, width = NULL)
conclusionint <- word(conclusionraw, 1, sep = fixed('ON THESE GROUNDS')) 
conclusionint <- unlist(strsplit(conclusionint, "\\d\\d\\d. "))
c1 <- word(conclusionint[2], 1, sep = fixed("\\d+")) 
c2 <- word(conclusionint[3], 1, sep = fixed("\\d. "))
c3 <- word(conclusionint[4], 1, sep = fixed("\\d. "))
c4 <- word(conclusionint[5], 1, sep = fixed("\\d. "))
c5 <- word(conclusionint[6], 1, sep = fixed("\\d. "))
cf <- paste(c1, c2, c3, c4, c5, sep = "")
cf <- sub(".[^.]+$", "", cf) %>% 
  paste(".", sep='') %>% word(1, sep = fixed(' CAS'))

# On These Grounds #

otgraw <- adress %>%
  read_pdf() %>%
  slice(locsotg(text, 'on these grounds')) 
otgraw <- paste(unlist(otgraw), collapse =" ") %>%
  word(2, sep = fixed(':')) 
otg1 <- word(otgraw, 2, sep = fixed('1. ')) %>%
  word(1, sep = fixed('2.')) %>%
str_trim()
otg2 <- word(otgraw, 2, sep = fixed('2.')) %>%
  word(1, sep = fixed ('3. ')) %>%
str_trim()
otg3 <- word(otgraw, 2, sep = fixed('3.')) %>%
  word(1, sep = fixed ('4. ')) %>% 
  str_trim()
otg4 <- word(otgraw, 2, sep = fixed('4. ')) %>%
  word(1, sep = fixed ('5.'))  %>% 
  str_trim()
otg5 <- word(otgraw, 2, sep = fixed('5. ')) %>%
  word(1, sep = fixed ('6.'))  %>% 
str_trim()
otg6 <- word(otgraw, 2, sep = fixed('6. ')) %>% 
  str_trim()

# CREATING THE DATA FRAME

maindataframe <- data.frame(Coder = coderid,
                            "File Name" = filename,
                            "Award Date" = awarddate,
                            "Operative Date" = operativedate,
                            "President" = president,
                            "Country" = presidentcountry,
                            "Panel Member-1" = m1panel,
                            "Country" = m1panelc,
                            "Panel Member-2" = m2panel,
                            "Country" = m2panelc,
                            "Appellant" = appellant,
                            "Respondant" = respondant,
                            "Conclusions" = cf,
                            "otg1" = otg1,
                            "otg2" = otg2,
                            "otg3" = otg3,
                            "otg4" = otg4,
                            "otg5" = otg5,
                            "otg6" = otg6)

# burasi da olusturulan dataframe'i, dosya numarasina göre isimlendirip excel dosyasina çeviriyor.

# OUTPUT
write.xlsx(maindataframe, file = paste(casenumber,".xlsx", sep=""))
head(maindataframe)

