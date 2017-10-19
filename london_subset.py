#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 18 14:10:35 2017

@author: evanodell
"""

import pandas

df = pandas.read_csv('pp-complete.csv', header=None, names = ["transaction",
                                                       "price",
                                                       "date",
                                                       "postcode",
                                                       "property_type",
                                                       "new_build",
                                                       "ownership_type",
                                                       "paon",
                                                       "saon",
                                                       "street",
                                                       "locality",
                                                       "city",
                                                       "district",
                                                       "county",
                                                       "category_type",
                                                       "record_status"])


london_df = df.query('county == "GREATER LONDON" and date >= "2012-09-01"')


london_df.to_csv('pp-london-complete.csv', index=False, header=True)
