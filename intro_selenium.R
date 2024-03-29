##previous steps
#install java dk
#set paths for java and JAVA_HOME

#download chromedriver
#set path for chromedriver

#install.packages("xx")

#select libraries

library(tidyverse)
library(RSelenium)
library(netstat)

#start the server
rs_driver_object <- rsDriver(browser ='chrome',
                             chromever = '102.0.5005.61',
                             verbose = FALSE,
                             #port=free_port())
                             port = 48518L) 

#what class is "rs_driver_object"?
class(rs_driver_object)               
#it has 2 classes, we need the client object in order to work
remDr <- rs_driver_object$client

#open browser
remDr$open()

#maximize windows size of the browser
remDr$maxWindowSize()

#navigate to website (it is important to start by "https...")
remDr$navigate('https://www.mercadolibre.com.pe')

#finding element "Ofertas". A simple element really visible from the home page
##################################################
ofertas_object <- remDr$findElement(using = 'link text', 'Ofertas')
#we can search all the attributes from the requested element. In thisn case, we want the href 
ofertas_object$getElementAttribute('href')
#and, also it is possible to "click" the referred element by coding
ofertas_object$clickElement()

#go back
remDr$goBack()
#go forward
#remDr$goForward()

#search for an item "playstation 5" (first try)
#----------------------------------------------
#locate the id for the searchbox
search_box <- remDr$findElement(using = 'class', 'nav-search-input')
#enter in a list the desired item to search
search_box$sendKeysToElement(list('Playstation 5', key = 'enter'))

#scroll to the end of the webpage, using commands consulted from stackoverflow
remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

#click a wanted box "condicion" by using 'xpath' and an attribute
##############################################
condition_click <- remDr$findElement(using = 'xpath', '//a[@aria-label="Nuevo"]')
condition_click$clickElement()
#sometimes we need to use 
#condition_click$refresh()



##click a display box "Relevancia" by using 'xpath' and a text line
remDr$findElement(using = 'class', 'andes-dropdown__display-values')$clickElement()

#click "menor precio"
remDr$findElement(using='xpath', '//*[text()="Mayor precio"]')$clickElement()


#click an item by using 'xpath' and a text line
##############################################
remDr$findElement(using='xpath', '//*[text()="Consola Playstation 5 825gb Standard Color Blanco Y Negro "]')$clickElement()

remDr$goBack()
remDr$goBack()


###################################################
#Web Scrapping#####
###################################################

#Identify the  price
price <- remDr$findElement(using = 'class name', 'price-tag-fraction')
price$getElementText()

#Identify ALL PRICES
prices <- remDr$findElements(using = 'class name', 'price-tag-fraction')
length(prices) #how to extract text??

#use new function
#for each element of "prices", get the elementtext from each one.
#better than use loops, and unlist, and remove $ sign (where it is)
price_values <- lapply(prices, function(x) x$getElementText()) %>% 
  unlist #%>% 
  #str_remove_all()
  
price_values

#delete element 69 (not realistic price)

price_values= price_values[-69]
price_values  #better!

#convert from string to number, now that all seem to be numeric
price_values = price_values %>%
  as.numeric()

#get the mean
mean(price_values)
  #it is awful to see because of the format used by the website
median(price_values)
  #median seems to fit better!!!


#terminate Selenium session
system("taskkill /im java.exe /f")
