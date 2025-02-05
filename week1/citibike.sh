#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
cut -d, -f5,9 201402-citibike-tripdata.csv | tr , '\n' | sort | uniq | wc -l
# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq | wc -l
# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | cut -d'' -f1 | sort | uniq -c
# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d'' -f1 | sort | uniq -c | sort -nr | head -n1
# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -d'' -f1 | sort | uniq -c | sort | head -n2
# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq -c | sort -nr | head -n1
# count the number of rides by gender and birth year
cut -d, -f14,15 201402-citibike-tripdata.csv | sort | uniq -c
# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
<<<<<<< HEAD
cut -d, -f5 201402-citibike-tripdata.csv | grep '[0-9].* & .*[0-9]' |  wc -l
=======


>>>>>>> f58f272c6b4f2f03da3604f4732c9551f85ff357
# compute the average trip duration
awk -F, '{ gsub(/"/, "", $1); sum += $1; k++} END {print sum/k}' 201402-citibike-tripdate.csv
