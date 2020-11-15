rm(list = ls())

library(data.table)


### Investigate Data
######################################################

sales = fread("bakery_sales.csv")

# Fix Date
sales$Date = as.Date(sales$Date, format = "%Y-%m-%d")
sales$Time=NULL

# Sum sales quantities
sales$quantity = 1
sales[, sum(quantity), by = "Item"][order(-V1)]

# Create Time Series Data
######################################################

# get aggregated data for product
product = sales[Item == "Bread", .(quantity = sum(quantity)), by = "Date"]
product[, mean(quantity), by = weekdays(Date)]

# create completed times series
dateseq = data.table(Date = seq(min(sales$Date), max(sales$Date), by = "days"))
setdiff(dateseq$Date, product$Date)

product = merge(dateseq, product, by="Date", all.x=T)
product$is_closed = ifelse(is.na(product$quantity), 1, 0)
product[is.na(quantity),"quantity"] = 0

# create some additional features
product$day_of_week = as.numeric(strftime(as.Date(product$Date, "%Y-%m-%d"), "%u"))
product$week_of_year = week(product$Date - 2)
product = product[-1,]

### Create Test and Training Data
######################################################

index=tail(1:nrow(product),28)
train_product=product[-index,]
test_product=product[index,]

# Final data prep in test and train data

#Create lags
train_product$quantity_lag7=shift(train_product$quantity,n=7, fill=NA, type="lag")
train_product$quantity_lag14=shift(train_product$quantity,n=14, fill=NA, type="lag")

#Create seasonal decomposition
train_product_decomposed = decompose(ts(train_product$quantity,frequency = 7))
train_product$seasonal = train_product_decomposed$seasonal

# remove NAs
train_product=train_product[complete.cases(train_product),]

# save train and test data to csv
# write.csv(train_product, "train_data.csv",row.names=FALSE)
# write.csv(test_product, "test_data.csv",row.names=FALSE)

