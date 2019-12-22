#Supplementary project by Anirban Biswas and Bassam Arnout
library(crayon)
cat(blue("\n\nSUPPLEMENTARY PROJECT \nANIRBAN BISWAS \nBASSAM ARNOUT"))
uData <- read.csv("housing_price.csv", header = TRUE)       # Reading the data from the CSV file
modData <- uData[,c(1,8,13,18,20,22,23,24,25,27,32)]        # Reading specific columns from the CSV file
p <- readline(prompt="do you want to view the datas of all 104 houses? input y for yes and n for no : ")
if (p == 'y')
{
  print(uData)
  View(uData)
}
if (p == 'n')
{
  library(crayon)
  cat(blue("\nProceeding to next step...............\n\n"))
}

m <- readline(prompt="Enter the number of rows to be taken (should not be greater than 104) : ")
m <- as.integer(m) # convert character into integer

modData2 <- modData[c(1:m), ]
print(modData2)
View(modData2)

library(crayon)
cat(blue("\nDo you want to view the features of the housing data "))
o <- readline(prompt="Enter y for yes n for no : ")

if (o == 'y')
{
  modData3 <- modData2[, c(6,7,8,9)]
  M<-cor(modData3)
  feat <- summary(modData2)
  vprint <- function(x, ...) {
    require(htmltools) #htmltool package is required
    html_print(pre(paste0(capture.output(print(x, ...)), collapse="\n")))
  }
  vprint(feat)
  View(M)
  library("PerformanceAnalytics")
  x11()
  chart.Correlation(modData3, histogram = TRUE, pch = 19, title = "visual representation of correlation of various Prices")
  
  library(crayon)
  cat(blue("\nInput any value to view the price comparison among the years "))
  op <- readline(prompt="Enter : ")
  
  library(ggplot2)
  p11 <- ggplot() + geom_line(aes(y = price1998, x = housenum),
                              data = modData2)
  
  f11 <- p11 + labs(title = "Price of housing in 1998", x = "house number", y = "price", caption = "Data")
  
  p12 <- ggplot() + geom_line(aes(y = price2007, x = housenum),
                              data = modData2)
  
  f12 <- p12 + labs(title = "Price of housing in 2007", x = "house number", y = "price", caption = "Data")
  
  p13 <- ggplot() + geom_line(aes(y = price2011, x = housenum),
                              data = modData2)
  
  f13 <- p13 + labs(title = "Price of housing in 2011", x = "house number", y = "price", caption = "Data")
  
  p14 <- ggplot() + geom_line(aes(y = price2014, x = housenum),
                              data = modData2)
  
  f14 <- p14 + labs(title = "Price of housing in 2014", x = "house number", y = "price", caption = "Data")
  
  #par(mfrow=c(2,2))
  x11()
  library(gridExtra)
  grid.arrange(f11, f12, f13, f14, nrow = 2)
  
  library(ggplot2)
  dx1 <- modData2
  # compute normalized occurence for letter
  dx1$normalize_occurence <- round((dx1$price1998 - mean(dx1$price1998))/sd(dx1$price1998), 2)  
  # categorise the occurence
  dx1$category<- ifelse(dx1$normalize_occurence >0, "high","low")
  # check summary statistic
  
  xp1 <- ggplot(dx1,aes(x = housenum,y = normalize_occurence)) + 
    geom_bar(aes(fill = category),stat = "identity") +
    labs(title= "Diverging Bars-Price 1998")+
    coord_flip()
  
  library(ggplot2)
  dx2 <- modData2
  # compute normalized occurence for letter
  dx2$normalize_occurence <- round((dx2$price2007 - mean(dx2$price2007))/sd(dx2$price2007), 2)  
  # categorise the occurence
  dx2$category<- ifelse(dx2$normalize_occurence >0, "high","low")
  # check summary statistic
  
  xp2 <- ggplot(dx2,aes(x = housenum,y = normalize_occurence)) + 
    geom_bar(aes(fill = category),stat = "identity") +
    labs(title= "Diverging Bars- Price 2007")+
    coord_flip()
  
  library(ggplot2)
  df3 <- modData2
  # compute normalized occurence for letter
  df3$normalize_occurence <- round((df3$price2011 - mean(df3$price2011))/sd(df3$price2011), 2)  
  # categorise the occurence
  df3$category<- ifelse(df3$normalize_occurence >0, "high","low")
  # check summary statistic
  
  xp3 <- ggplot(df3,aes(x = housenum,y = normalize_occurence)) + 
    geom_bar(aes(fill = category),stat = "identity") +
    labs(title= "Diverging Bars- Price 2011")+
    coord_flip()
  
  library(ggplot2)
  df4 <- modData2
  # compute normalized occurence for letter
  df4$normalize_occurence <- round((df4$price2014 - mean(df4$price2014))/sd(df4$price2014), 2)  
  # categorise the occurence
  df4$category<- ifelse(df4$normalize_occurence >0, "high","low")
  # check summary statistic
  
  xp4 <- ggplot(df4,aes(x = housenum,y = normalize_occurence)) + 
    geom_bar(aes(fill = category),stat = "identity") +
    labs(title= "Diverging Bars- Price 2014")+
    coord_flip()
  
  x11()
  library(gridExtra)
  grid.arrange(xp1, xp2, xp3, xp4, nrow = 2)
  

}
if (o == 'n')
{
  library(crayon)
  cat(green("\nProceeding to the visualization phase...............\n\n"))
}

repeat{
  library(crayon)
  cat(blue("\nselect the year for which you want to perform visualization \n1. 1998 \n2. 2007 \n3. 2011 \n4. 2014\n"))
  n <- readline(prompt="Enter your choice : ")
  n <- as.integer(n)
  
  
  if (n == 1)
  {
    library(ggplot2)
    p1 <- ggplot() + geom_line(aes(y = price1998, x = housenum),
                               data = modData2)
    
    f1 <- p1 + labs(title = "Price of housing in 1998", x = "house number", y = "price", caption = "Data: Cavenez.com")
    x11()
    plot(f1)
    
    df <- modData2
    # compute normalized occurence for letter
    df$normalize_occurence <- round((df$price1998 - mean(df$price1998))/sd(df$price1998), 2)  
    # categorise the occurence
    df$category<- ifelse(df$normalize_occurence >0, "high","low")
    # check summary statistic
    
    xp <- ggplot(df,aes(x = housenum,y = normalize_occurence)) + 
      geom_bar(aes(fill = category),stat = "identity") +
      labs(title= "Diverging Bars")+
      coord_flip()
    x11()
    plot(xp)
    
    repeat{
      cat(blue("\nselect the parameter for x-axis for which you want to perform visualization \n1. bedrooms \n2. squarefeet \n3. number of rooms \n4. number of full baths \n5. garage space \n6. Walkscore\n"))
      q <- readline(prompt="Enter your choice : ")
      q <- as.integer(q)
      
      if(q == 1)
      {
        library(ggplot2) # One plot using ggplot2
        #options(device = "quartz")  #use fr windows only
        f2 <- qplot(modData2$price1998, modData2$bedrooms, data=modData2, geom=c("point", "smooth"),
                    fill=modData2$bedrooms, main="Price in 1998 by number of bedrooms",
                    xlab="Price", ylab="No. of bedrooms ")
        x11()
        plot(f2)
      }
      
      if(q == 2)
      {
        
        x11()
        plot(modData2$squarefeet, modData2$price1998, xlab = "Square Feet", ylab = "Price in 1998",
             main = "Price in 1998 per square feet")
        
        
        gg <- ggplot(modData2, aes(x=squarefeet, y=price1998)) + 
          geom_point(aes(col=squarefeet, size=price1998)) + 
          geom_smooth(method="loess", se=F) + 
          labs(subtitle="Price Vs Squarefeet", 
               y="Price",
               x="Squarefeet",
               title="Scatterplot", 
               caption = "Source")
        x11()
        plot(gg)
      }
      
      if(q == 3)
      {
        
        library(ggplot2)
        p2 <- ggplot() + geom_boxplot(aes(y = price1998, x = no_rooms, group = 5),
                                      data = modData2) + geom_violin()
        
        p3 <- p2 + labs(title = "Price of housing per room in 1998", x = "number of rooms", y = "price", caption = "Data")
        x11()
        plot(p3)
      }
      
      if(q == 4)
      {
        library(ggplot2) # One plot using ggplot2
        
        f5<-ggplot(modData2, aes(x=price1998, y=no_full_baths)) + 
          geom_dotplot(binaxis='y', stackdir='center')
        ff5 <- f5 + coord_flip()
        x11()
        plot(ff5)
      }
      
      if(q == 5)
      {
        library(ggplot2)
        p5 <- ggplot() + geom_line(aes(y = price1998, x = garage_spaces),
                                   data = modData2)
        f8 <- p5 + labs(title = "Price of housing in 1998", x = "garagespace", y = "price", caption = "Data")
        
        f7 <- barplot(modData2$garage_spaces, xlab = "", ylab = "number of garagespace", main = "garagespace in 1998")
        x11()
        #par(mfrow=c(1,3))
        plot(f7)
        plot(p5)
      }
      
      if(q == 6)
      {
        library(lattice)
        f6 <- histogram(modData2$walkscore, main = "Walkscore")
        x11()
        plot(f6)
      }
      
      cat(blue("\nDo you want to continue visualising using another parameter "))
      s <- readline(prompt="Enter any input value for yes and n for no : ")
      
      if(s == 'n')
      {
        cat("............Thank You.................")
        break
      }
    }
    
  }
  
  
  if (n == 2)
  {
    library(ggplot2)
    p1 <- ggplot() + geom_line(aes(y = price2007, x = housenum),
                               data = modData2)
    
    f1 <- p1 + labs(title = "Price of housing in 2007", x = "house number", y = "price", caption = "Data")
    x11()
    plot(f1)
    
    df <- modData2
    # compute normalized occurence for letter
    df$normalize_occurence <- round((df$price2007 - mean(df$price2007))/sd(df$price2007), 2)  
    # categorise the occurence
    df$category<- ifelse(df$normalize_occurence >0, "high","low")
    # check summary statistic
    
    xp <- ggplot(df,aes(x = housenum,y = normalize_occurence)) + 
      geom_bar(aes(fill = category),stat = "identity") +
      labs(title= "Diverging Bars")+
      coord_flip()
    x11()
    plot(xp)
    
    repeat{
      cat(blue("\nselect the parameter for x-axis for which you want to perform visualization \n1. bedrooms \n2. squarefeet \n3. number of rooms \n4. number of full baths \n5. garage space \n6. Walkscore\n"))
      q <- readline(prompt="Enter your choice : ")
      q <- as.integer(q)
      
      if(q == 1)
      {
        library(ggplot2) # One plot using ggplot2
        #options(device = "quartz")  #use fr windows only
        f2 <- qplot(modData2$price2007, modData2$bedrooms, data=modData2, geom=c("point", "smooth"),
                    fill=modData2$bedrooms, main="Price in 2007 by number of bedrooms",
                    xlab="Price", ylab="No. of bedrooms ")
        x11()
        plot(f2)
      }
      
      if(q == 2)
      {
        
        x11()
        plot(modData2$squarefeet, modData2$price2007, xlab = "Square Feet", ylab = "Price in 2007",
             main = "Price in 2007 per square feet")
        
        
        gg <- ggplot(modData2, aes(x=squarefeet, y=price2007)) + 
          geom_point(aes(col=squarefeet, size=price2007)) + 
          geom_smooth(method="loess", se=F) + 
          labs(subtitle="Price Vs Squarefeet", 
               y="Price",
               x="Squarefeet",
               title="Scatterplot", 
               caption = "Source")
        x11()
        plot(gg)
      }
      
      if(q == 3)
      {
        
        library(ggplot2)
        p2 <- ggplot() + geom_boxplot(aes(y = price2007, x = no_rooms, group = 5),
                                      data = modData2) + geom_violin()
        
        p3 <- p2 + labs(title = "Price of housing per room in 2007", x = "number of rooms", y = "price", caption = "Data")
        x11()
        plot(p3)
      }
      
      if(q == 4)
      {
        library(ggplot2) # One plot using ggplot2
        
        f5<-ggplot(modData2, aes(x=price2007, y=no_full_baths)) + 
          geom_dotplot(binaxis='y', stackdir='center')
        ff5 <- f5 + coord_flip()
        x11()
        plot(ff5)
      }
      
      if(q == 5)
      {
        library(ggplot2)
        p5 <- ggplot() + geom_line(aes(y = price2007, x = garage_spaces),
                                   data = modData2)
        f8 <- p5 + labs(title = "Price of housing in 2007", x = "garagespace", y = "price", caption = "Data")
        
        f7 <- barplot(modData2$garage_spaces, xlab = "", ylab = "number of garagespace", main = "garagespace in 2007")
        x11()
        #par(mfrow=c(1,3))
        plot(f7)
        plot(p5)
      }
      
      if(q == 6)
      {
        library(lattice)
        f6 <- histogram(modData2$walkscore, main = "Walkscore")
        x11()
        plot(f6)
      }
      
      cat(blue("\nDo you want to continue visualising using another parameter "))
      s <- readline(prompt="Enter any input value for yes and n for no : ")
      
      if(s == 'n')
      {
        cat("............Thank You.................")
        break
      }
    }
    
  }
  
  
  if (n == 3)
  {
    library(ggplot2)
    p1 <- ggplot() + geom_line(aes(y = price2011, x = housenum),
                               data = modData2)
    
    f1 <- p1 + labs(title = "Price of housing in 2011", x = "house number", y = "price", caption = "Data")
    x11()
    plot(f1)
    
    df <- modData2
    # compute normalized occurence for letter
    df$normalize_occurence <- round((df$price2011 - mean(df$price2011))/sd(df$price2011), 2)  
    # categorise the occurence
    df$category<- ifelse(df$normalize_occurence >0, "high","low")
    # check summary statistic
    
    xp <- ggplot(df,aes(x = housenum,y = normalize_occurence)) + 
      geom_bar(aes(fill = category),stat = "identity") +
      labs(title= "Diverging Bars")+
      coord_flip()
    x11()
    plot(xp)
    
    repeat{
      cat(blue("\nselect the parameter for x-axis for which you want to perform visualization \n1. bedrooms \n2. squarefeet \n3. number of rooms \n4. number of full baths \n5. garage space \n6. Walkscore\n"))
      q <- readline(prompt="Enter your choice : ")
      q <- as.integer(q)
      
      if(q == 1)
      {
        library(ggplot2) # One plot using ggplot2
        #options(device = "quartz")  #use fr windows only
        f2 <- qplot(modData2$price2011, modData2$bedrooms, data=modData2, geom=c("point", "smooth"),
                    fill=modData2$bedrooms, main="Price in 2011 by number of bedrooms",
                    xlab="Price", ylab="No. of bedrooms ")
        x11()
        plot(f2)
      }
      
      if(q == 2)
      {
        
        x11()
        plot(modData2$squarefeet, modData2$price2011, xlab = "Square Feet", ylab = "Price in 2011",
             main = "Price in 2011 per square feet")
        
        
        gg <- ggplot(modData2, aes(x=squarefeet, y=price2011)) + 
          geom_point(aes(col=squarefeet, size=price2011)) + 
          geom_smooth(method="loess", se=F) + 
          labs(subtitle="Price Vs Squarefeet", 
               y="Price",
               x="Squarefeet",
               title="Scatterplot", 
               caption = "Source")
        x11()
        plot(gg)
      }
      
      if(q == 3)
      {
        
        library(ggplot2)
        p2 <- ggplot() + geom_boxplot(aes(y = price2011, x = no_rooms, group = 5),
                                      data = modData2) + geom_violin()
        
        p3 <- p2 + labs(title = "Price of housing per room in 2011", x = "number of rooms", y = "price", caption = "Data")
        x11()
        plot(p3)
      }
      
      if(q == 4)
      {
        library(ggplot2) # One plot using ggplot2
        
        
        f5<-ggplot(modData2, aes(x=price2011, y=no_full_baths)) + 
          geom_dotplot(binaxis='y', stackdir='center')
        ff5 <- f5 + coord_flip()
        x11()
        plot(ff5)
      }
      
      if(q == 5)
      {
        library(ggplot2)
        p5 <- ggplot() + geom_line(aes(y = price2011, x = garage_spaces),
                                   data = modData2)
        f8 <- p5 + labs(title = "Price of housing in 2011", x = "garagespace", y = "price", caption = "Data")
        
        f7 <- barplot(modData2$garage_spaces, xlab = "", ylab = "number of garagespace", main = "garagespace in 2011")
        x11()
        #par(mfrow=c(1,3))
        plot(f7)
        plot(p5)
      }
      
      if(q == 6)
      {
        library(lattice)
        f6 <- histogram(modData2$walkscore, main = "Walkscore")
        x11()
        plot(f6)
      }
      
      cat(blue("\nDo you want to continue visualising using another parameter "))
      s <- readline(prompt="Enter any input value for yes and n for no : ")
      
      if(s == 'n')
      {
        cat("............Thank You.................")
        break
      }
    }
    
  }
  
  
  if (n == 4)
  {
    library(ggplot2)
    p1 <- ggplot() + geom_line(aes(y = price2014, x = housenum),
                               data = modData2)
    
    f1 <- p1 + labs(title = "Price of housing in 2014", x = "house number", y = "price", caption = "Data")
    x11()
    plot(f1)
    
    df <- modData2
    # compute normalized occurence for letter
    df$normalize_occurence <- round((df$price2014 - mean(df$price2014))/sd(df$price2014), 2)  
    # categorise the occurence
    df$category<- ifelse(df$normalize_occurence >0, "high","low")
    # check summary statistic
    
    xp <- ggplot(df,aes(x = housenum,y = normalize_occurence)) + 
      geom_bar(aes(fill = category),stat = "identity") +
      labs(title= "Diverging Bars")+
      coord_flip()
    x11()
    plot(xp)
    
    repeat{
      cat(blue("\nselect the parameter for x-axis for which you want to perform visualization \n1. bedrooms \n2. squarefeet \n3. number of rooms \n4. number of full baths \n5. garage space \n6. Walkscore\n"))
      q <- readline(prompt="Enter your choice : ")
      q <- as.integer(q)
      
      if(q == 1)
      {
        library(ggplot2) # One plot using ggplot2
        #options(device = "quartz")  #use fr windows only
        f2 <- qplot(modData2$price2014, modData2$bedrooms, data=modData2, geom=c("point", "smooth"),
                    fill=modData2$bedrooms, main="Price in 2014 by number of bedrooms",
                    xlab="Price", ylab="No. of bedrooms ")
        x11()
        plot(f2)
      }
      
      if(q == 2)
      {
        
        x11()
        plot(modData2$squarefeet, modData2$price2014, xlab = "Square Feet", ylab = "Price in 2014",
             main = "Price in 2014 per square feet")
        
        
        gg <- ggplot(modData2, aes(x=squarefeet, y=price2014)) + 
          geom_point(aes(col=squarefeet, size=price2014)) + 
          geom_smooth(method="loess", se=F) + 
          labs(subtitle="Price Vs Squarefeet", 
               y="Price",
               x="Squarefeet",
               title="Scatterplot", 
               caption = "Source")
        x11()
        plot(gg)
      }
      
      if(q == 3)
      {
        
        library(ggplot2)
        p2 <- ggplot() + geom_boxplot(aes(y = price2014, x = no_rooms, group = 5),
                                      data = modData2) + geom_violin()
        
        p3 <- p2 + labs(title = "Price of housing per room in 2014", x = "number of rooms", y = "price", caption = "Data")
        x11()
        plot(p3)
      }
      
      if(q == 4)
      {
        library(ggplot2) # One plot using ggplot2
        
        f5<-ggplot(modData2, aes(x=price2014, y=no_full_baths)) + 
          geom_dotplot(binaxis='y', stackdir='center')
        ff5 <- f5 + coord_flip()
        x11()
        plot(ff5)
      }
      
      if(q == 5)
      {
        library(ggplot2)
        p5 <- ggplot() + geom_line(aes(y = price2014, x = garage_spaces),
                                   data = modData2)
        f8 <- p5 + labs(title = "Price of housing in 2014", x = "garagespace", y = "price", caption = "Data")
        
        f7 <- barplot(modData2$garage_spaces, xlab = "", ylab = "number of garagespace", main = "garagespace in 2014")
        x11()
        #par(mfrow=c(1,3))
        plot(f7)
        plot(p5)
      }
      
      if(q == 6)
      {
        library(lattice)
        f6 <- histogram(modData2$walkscore, main = "Walkscore")
        x11()
        plot(f6)
      }
      
      cat(blue("\nDo you want to continue visualising using another parameter "))
      s <- readline(prompt="Enter any input value for yes and n for no : ")
      
      if(s == 'n')
      {
        cat("............Thank You.................")
        break
      }
    }
    
  }
  
  cat(blue("\nDo you want perform visualising of price for another year  "))
  ll <- readline(prompt="Enter any input value for yes and n for no : ")
  
  if(ll == 'n')
  {
    cat("\n\n............Thank You.................")
    break
  }
}

cat("\n\n Representing the conclusion using regression on a 3D scatterplot ")

modData4 <- modData2[,c(8,9,10)]
x11()
library("scatterplot3d") # load
# 3D scatter plot
s3d <- scatterplot3d(modData4, type = "h", color = "blue",angle = 55, scale.y = 0.7, pch = 16, main = "3D scatter plot with the regression plane")

# Add regression plane
my.lm <- lm(modData4$squarefeet ~ modData4$price2011 + modData4$price2014)
s3d$plane3d(my.lm)
# Add supplementary points
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)
cat("\n----------------------------------Thank You------------------------------------------")