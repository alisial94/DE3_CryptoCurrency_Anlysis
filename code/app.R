#!/usr/bin/Rscript

# Reset log file; only reason for deletion is unnecessary size
fn <- 'app.log'
if (file.exists(fn)) {
  #Delete file if it exists
  file.remove(fn)
}

library(logger)
log_appender(appender_file('app.log'))
library(AWR.Kinesis)
library(methods)
library(jsonlite)
library(rredis)
redisConnect(nodelay = FALSE)

# Reset Redis if there are already existent keys
if (!is.null(redisKeys())){ 
  redisDelete(redisKeys())
}

# save the start time into Redis
redisSet("time_of_start", .POSIXct(Sys.time(), "EET"))

kinesis_consumer(
  
  initialize = function() {
    log_info('Hello, connected to Redis')
  },
  
  processRecords = function(records) {
    log_info(paste('Received', nrow(records), 'records from Kinesis'))
    for (record in records$data) {
      symbol <- fromJSON(record)$s
      log_info(paste('Found 1 transaction on', symbol))
      redisIncr(paste('alisial_symbol', symbol, sep = ':'))
    }
  },
  
  updater = list(
    list(1/6, function() {
      log_info('Checking overall counters')
      symbols <- redisMGet(redisKeys('alisial_symbol:*'))
      log_info(paste(sum(as.numeric(symbols)), 'records processed so far'))
    })),
  
  shutdown = function()
    log_info('Bye'),
  
  checkpointing = 1,
  logfile = 'app.log')