#question 1 c , question 2 and question 3 
cheese_table <- read.csv("cheese.csv",header =TRUE,sep = ";")
dough_table <- read.csv("dough.csv",header =TRUE,sep = ";")
sizes_table <- read.csv("size.csv",header =TRUE,sep = ";")
state_table <- read.csv("store.csv",header =TRUE,sep = ";")
topping_table <- read.csv("tooping.csv",header =TRUE,sep = ";")
date_table <- 
  data.frame(id=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                    "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3",
                       "Q3","Q3","Q4","Q4","Q4"))

gen_sales <- function(no_of_recs) {
  
  # Generate transaction data randomly
  loc <- sample(state_table$store_name, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(date_table$id, no_of_recs, replace=T)
  time_year <- sample(c(2018, 2019), no_of_recs, replace=T)
  cheese <- sample(cheese_table$cheese_name, no_of_recs, replace=T)
  size <- sample(sizes_table$size_name, no_of_recs, replace=T,prob=c(1,1,2,2,2))
  dough <- sample(dough_table$Dough_name, no_of_recs, replace=T)
  topping <- sample(topping_table$topping_name, no_of_recs, replace=T)
  Quantity <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  profit <- Quantity*(cheese_table$price+sizes_table$price+topping_table$price
                      +dough_table$price)
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      cheese=cheese,
                      size=size,
                      dough=dough,
                      topping=topping,
                      Quantity=Quantity,
                      profit=profit
                      )
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
sales_fact <- gen_sales(700)
write.csv(sales_fact,"sales_pizza.csv")
sales=read.csv('sales_pizza.csv',header =TRUE,sep = ";")
# Look at a few records
head(sales)

# Build up a cube
revenue_cube <- 
  tapply(sales_fact$profit, 
         sales_fact[,c("size","cheese","dough","topping", "month", "year","loc")], 
         FUN=function(x){return(sum(x))})

# Showing the cells of the cude
revenue_cube
dimnames(revenue_cube)
#SLICE
revenue_cube[,,,, "2", "2019",]
revenue_cube[,,,, "2", "2018",]
revenue_cube["xlarge","Mozzarella","stuffed crust","onions", 
             "1","2018" ,"California"]
#DICE
revenue_cube[c("large","small"),,,, 
             c("1","2","3"), 
             ,
             c("California","new York")]

# drilldown and roll-up
apply(revenue_cube, c("year", "size"),
      FUN=function(x) {return(max(x, na.rm=TRUE))})
apply(revenue_cube, c("year", "size","month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
