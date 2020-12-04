#!/bin/bash
set -e
 
dos2unix *.py
chmod a+x *.py
 
chmod a+x *.sql
chmod a+x *.sh
dos2unix *
 
echo "Cleaning Data"
./Clean_Data.py
 
echo "Process Complete"
