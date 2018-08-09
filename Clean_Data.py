#!/opt/cloudera/extras/python34/bin/python

 

#importing modules we need

import pandas as pd

import numpy as np

import matplotlib.pyplot as plt

 

# reading the activation dataset. One thing to note here is we have used '#' as the seperator instead of commas.

# This is done because the discovery dataset had one field('license_designations') which had commas in them.

data=pd.read_csv("activation_data_mm.csv", parse_dates=True, encoding='UTF-8',sep=',')

 

#Removing duplicates created when broker was matched with more than one channel

#within the same firm in discovery data

data=data.drop_duplicates(subset=['npn'], keep='last')

 

#Excluding AXA Distributors and AXA Advisors

data = data[data.bdfirmcrd != 25900] #AXA Distributors, LLC

data = data[data.bdfirmcrd != 6627] #AXA Advisors, LLC

 

#filtering brokers at firms with AXA selling agreements

data['firmcode'].replace('', np.nan, inplace=True)

data.dropna(subset=['firmcode'], inplace=True)

 

# counter is a custom variable which was created to define the scope of our case

# counter = 0, if the rep has never given a ticket and the rep has licenses to sell IA products

# counter = 1, if the first ticket is dropped between Sep 30, 2015 and Sep 30, 2017

# counter = 2, if the rep has first ticket before Sep 30, 2015 and has sold a ticket since Sep 30, 2015

 

#include this line if you're not going to filter by counter

data_use = data

 

# In the line below we treat the missing values

for col in data_use.columns:

        data_use[col].fillna('Unknown', inplace = True)

 

# for the column dateofbirth_full we replace the unknown value to 1800. As we just need the year for our calculation of age           

data_use.dateofbirth_full.replace(['Unknown'], ["1800"], inplace=True)

 

# we extract year of birth from dateofbirth_full, which is the first 4 characters in the string

data_use['year_of_birth'] = data_use.dateofbirth_full.str[0:4].astype(int)

 

# we calculate age since we have the year of birth. Basically, year_now - year_of_birth

data_use['age'] = pd.tslib.Timestamp.now().year - data_use['year_of_birth']       

                                                                                                                                                                               

# the features we would be using in our clustering

#data_model_final = data_model_to_use[[

#u'npn',

#u'repcrd',

#u'target',

#u'number_yearsanagent',

#u'number_branchreps',

#u'number_registeredstates',

#u'num_active_reps_last_12_months',

#u'total',

#u'carrierappointments_Multiple Carriers',             

#u'age',

#u'sc_channeltype_bak',

#u'branch_zipcode',

#]]

 

 

# In the step below, we filter out counter 0 and 1

data_use_counter_1_2 = data_use[data_use.counter != 0]

data_use_counter_0 = data_use[data_use.counter == 0]

 

#In the steps below, we create a variable called firm friendliness

data_use_counter_1_2['activerepsinfirm'] = data_use_counter_1_2.groupby('firmcode')['firmcode'].transform('count')

data_use_counter_1_2['firmfriendliness'] = data_use_counter_1_2['activerepsinfirm']/data_use_counter_1_2["number_firmreps"]

data_use_counter_1_2_index = data_use_counter_1_2.groupby(['firmcode'])['firmcode','firmfriendliness'].first()

data_use_counter_1_2_dict = dict(zip(data_use_counter_1_2_index.firmcode, data_use_counter_1_2_index.firmfriendliness))

data_use_counter_0['firmfriendliness'] = data_use_counter_0['firmcode'].map(data_use_counter_1_2_dict)

data_use_counter_1_2 = data_use_counter_1_2.drop('activerepsinfirm', 1)

data_use = pd.concat([data_use_counter_0, data_use_counter_1_2], ignore_index=True)

 

#Filtering only firms who have dropeed a ticket in past two years

data_use = data_use[np.isfinite(data_use['firmfriendliness'])]

 

data_use.to_csv("activation_data_mm_cleaned.csv")