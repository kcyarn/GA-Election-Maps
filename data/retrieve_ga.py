#!/usr/bin/python
# -*- coding: utf-8 -*-

# Georgia uses a newer version of clarity. Install clarify from git with below command
# pip install git+https://github.com/openelections/clarify.git#egg=clarify
import clarify, subprocess, os, wget, shutil
from zipfile import ZipFile

j = clarify.Jurisdiction(url='https://results.enr.clarityelections.com/GA/105369/web.264614/#/summary', level='state')

subs = j.get_subjurisdictions()

working_directory = os.getcwd()
xml_directory = os.path.join(working_directory, "xml_data")
default_zip = os.path.join(working_directory, "detailxml.zip")
default_xml = os.path.join(working_directory, "detail.xml")

# Never edit anything in the xml_data directory. This directory is automatically deleted at the start of every new run.

if os.path.exists(xml_directory):
  shutil.rmtree(xml_directory)

## Now create the xml_directory
if not os.path.exists(xml_directory):
  os.makedirs(xml_directory)

#subs[0].name is Appling. subs[158].name is Worth.
# Final range = 159 (the total number of counties)
for i in range(159):
  # Remember that all downloaded data is zipped!
  url = subs[i].report_url('xml')
  file_name = str(i) + "_data.xml"
  file_path = os.path.join(xml_directory, file_name)
  # Download the zipped xml data for this county
  wget.download(url)
  # Unzip the Zipped xml data for this county.
  with ZipFile(default_zip, 'r') as zip:
    zip.extractall()
  # Rename detail.xml and delete 
  if os.path.exists(default_xml):
    os.rename(default_xml, file_path)
  # Delete the downloaded zip file
  if os.path.exists(default_zip):
    os.remove(default_zip)

print("Finished retrieving election xml data.")
  
