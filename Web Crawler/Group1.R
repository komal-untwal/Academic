# To get & set working directory : Note: As the final output file will be saved in the given current" 
#getwd()
setwd("/Users/komaluntwal/Documents/NJIT 2021/spring 2021/CS636 data analytics with R")

# Function Definition: To crawl web data of a journal for given input year.

journal_data <- function(year=2020){
  
  if(year < 2006 | year > 2021){
    return("Kindly Enter the year between 2006 and 2021, as this journal includes articles published in those years only")
  }
  
  else{
    ArryOfYears=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
    volNo = which(ArryOfYears==year)
    
    #main journal url
    url_journal = "https://almob.biomedcentral.com/articles?query=&searchType=&tab=keyword&volume="
    url_journal = paste(url_journal,volNo)
    url_journal
    
    #making the main URL
    url_journal = sub(" ","",url_journal)
    
    #to check if we can scrape the data from the website using
    install.packages("robotstxt")
    library(robotstxt)
    paths_allowed(paths = url_journal)
    
    #to download the web page using read_html() from xml2 package
    install.packages("xml2")
    library(xml2)
    
    journal = read_html(url_journal)
    journal
    
    
    # to extract content from html page we use rvest pacakage
    install.packages("rvest")
    library("rvest")
    
    #################To scrape Article Title from the website
    print("## Crawling Title ##")
    journal %>%
      html_nodes(" .c-listing__title a") %>%
      html_text() -> Title
    
    # print title
    Title
    
    # to store data into a data frame 
    JournalDf = data.frame(Title)
    
    
    ###################for authors
    print("## Crawling Authors ##")
    Authors= journal %>% html_nodes(".c-listing__authors") %>% html_text()
    
    # to clean data, remove "Authors: " from each author record
    Authors = gsub("Authors: ","",Authors)
    Authors[1]
    
    # add authors to the data frame
    JournalDf$Authors = Authors
    
    
    ################### Published date
    print("## Crawling Published Date ##")
    published_Date= journal %>% html_nodes(".c-meta__item") %>% html_text() 
    
    published_Date = published_Date[seq(2,length(published_Date), by = 2)]
    published_Date= gsub("Published on: ","",published_Date)
    
    # add Published date to the data frame
    JournalDf$Published_Date = published_Date
    
    
    #####Get URL
    print("##Crawling Full Text##")
    href = journal %>% html_nodes(".c-listing__title a")
    
    #Creating vector to store each URL 
    journalUrl = c()
    for(i in 1:length(href))
    {
      journalUrl[i] = xml_attrs(href[[i]])[["href"]]
    }
    
    #Print URL
    journalUrl[1]
    
    #Add main url in starting of each journal’s URL
    adUrl = paste("https://almob.biomedcentral.com",journalUrl)
    
    #Remove starting space while combining URL 
    adUrl = sub(" ","",adUrl)
    
    #Print combined Url
    adUrl[1]
    
    #Adding Fulltext URL to data frame
    fulltext = adUrl
    JournalDf$Fulltext.Url = fulltext
    
 
    ############################ for Abstract
    #declare the empty coloumn of Abstract
    JournalDf$Abstract = c(" ")
    
    print("## Crawling Abstract ##")
    j = 1
    #k = 1
    for(i in 1:length(adUrl)){
      Mainjournal = read_html(adUrl[j])
      Abstract = Mainjournal %>% html_nodes(".c-article-section__content") %>% html_text()
      
      #firstst value is the abstract
      JournalDf$Abstract[j] = Abstract[1]
      j = j+1
      #i= i+1
    }
    
    
    
    ######----------------------for keywords
    print("## Crawling Keywords ##")
    
    #code for keywords below
    JournalDf$Keywords = c(" ")
    j=1
    for(i in 1:length(adUrl)){
      Mainjournal = read_html(adUrl[j])
      Keywords = Mainjournal %>% html_nodes(".c-article-subject-list__subject span") %>% html_text()
      k = Keywords
      
      
      t =""
      for (i in 1:length(k)){
        t = paste(t,k[i],sep = ", ")
      }
      JournalDf$Keywords[j] = t
      JournalDf$Keywords[j] = sub(", ","",JournalDf$Keywords[j])
      j = j+1
      i= i+1
    }
    
    
    #########Affiliations
    print("##Crawling Affiliations##")
    JournalDf$Affiliations = c(" ")
    
    j = 1
    for(i in 1:length(adUrl)){
      Mainjournal = read_html(adUrl[j])
      affiliations = Mainjournal %>% html_nodes(".c-article-author-affiliation__list") %>% html_text()
      
      #firstst value is the abstract
      JournalDf$Affiliations[j] = affiliations
      j = j+1
      i= i+1
    }
    
    
    ###########################for Corresponding Authors
    JournalDf$CorrospondingAuthors = c(" ")
    print("## Crawling corrsponding Authors name ##")
    
    j= 1
    k = 1
    for(i in 1:length(adUrl)){
      Mainjournal = read_html(adUrl[j])
      z = Mainjournal %>% html_nodes("#corresp-c1") %>% html_text()
      JournalDf$CorrospondingAuthors[j] = z
      j= j+1
    }
    
    #### for coresponding Authors Emails
    #As There is no data for given field in journal so we added NA in that column
    print("Crawling Correspondence Author's Email")
    JournalDf$CorrespondenceAuthors_Emails = NA

    ################## Write fetched data into a csv file
    print("Data is crawled successfully! Please go to current directory to see the exported file!")
    library("xlsx")
    #write.csv(JournalDf,"Journal.csv", row.names = FALSE)
    write.xlsx(JournalDf, file="Journal.xlsx", sheetName = "Article_data", append = FALSE)
    
  }
}

# Run the journal_data() with desired year as argument
journal_data()








