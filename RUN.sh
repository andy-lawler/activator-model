#!/bin/bash
set -e
 
echo "GENERATING ACTIVATION DATASET"
dos2unix *.sql
 
impala="/usr/bin/impala-shell -i "
echo "Downloading the training data to csv"
 
OUTPUT="activation_data_mm.csv"
$impala -f "query_activation_data.sql" -B -o "$OUTPUT" --output_delimiter=',' --print_header
 
echo "Process Complete"
