#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  5 20:43:06 2018

@author: loey
"""

import os
import csv
from os import listdir
from os.path import isfile, join
import urllib.request
from bs4 import BeautifulSoup
import re


# Pulls link to abstract from each CogSci Proceeding page per year (works for 2009-2018)

allLinks = []

mypath = 'htm/' #set path to folder containing files
files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

with open('cogsci_abstracts.csv', 'w') as csv_file:
    fieldnames = ['year', 'authors', 'title', 'abstract', 'html_link', 'pdf_link']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()

    for f in files[1:len(files)]:
        print(f)
        cogsci = open('htm/'+f, 'rb')
        soup = BeautifulSoup(cogsci, 'html.parser')
        
        for link in soup.find_all('a'):
            newlink = link.get('href')
            if 'papers' in newlink and newlink not in allLinks: #checks for redundant papers
                allLinks.append(newlink)
                
                # extracts publication year from file name
                year = re.search(r'CogSci(.*?)\.htm', f).group(1)
                #print(year)
                
                page = urllib.request.urlopen(newlink)
                lsoup = BeautifulSoup(page, 'html.parser')
                     
                # extracts title
                title = lsoup.find('h1').get_text()
                #print(title)

                # extracts authors, separated by a comma
                authors = 'NA'
                if lsoup.find('div', attrs={'id':'pagebody'}) != None:
                    li = lsoup.find('div', attrs={'id':'pagebody'}).find('li')
                    authors = re.sub(",.*?\n", ", ", li.get_text())
                elif lsoup.find('ul', attrs={'class':'subAuthorList'}) != None:
                    authorList = []
                    for a in lsoup.find('ul', attrs={'class':'subAuthorList'}).find_all('span', attrs={'class':'subAuthorName'}):
                        authorList.append(a.get_text())
                    authors = ', '.join(authorList)
                elif year == '2014':
                    authorsHtml = lsoup('blockquote')[0]
                    authorList = []
                    for td in authorsHtml.find_all('td'):
                        if td.find('em') == None:
                            authorList.append(td.get_text())
                    authors = ', '.join(authorList)
                    
                # extracts abstract
                abstract = 'NA'
                if lsoup.find('p', attrs={'id':'abstract'}) != None:
                    abstract = lsoup.find('p', attrs={'id':'abstract'}).get_text()
                elif lsoup.find('span', attrs={'class':'subAbstract'}) != None:
                    abstractHtml = lsoup.find('span', attrs={'class':'subAbstract'})
                    b = abstractHtml.find('b')
                    b.extract()
                    abstract = abstractHtml.get_text()
                elif year == '2014':
                    abstract = lsoup('blockquote')[1].get_text()
                
                # extracts full pdf link
                pdf_link = 'NA'
                if lsoup.find('li', attrs={'id':'files'}) != None:
                    pdf_link = re.sub("index.html", "", newlink)
                    pdf_link = pdf_link + lsoup.find('li', attrs={'id':'files'}).find('a').get('href')
                elif lsoup.find('span', attrs={'class':'subFile'}) != None:
                    pdf_link = re.sub("index.html", "", newlink)
                    pdf_link = pdf_link + lsoup.find('span', attrs={'class':'subFile'}).find('a').get('href')
                elif year == '2014':
                    for pdfl in lsoup.find_all('a'):
                        if 'paper' in pdfl.get('href'):
                            pdf_link = re.sub("index.html", "", newlink)
                            pdf_link = pdf_link + pdfl.get('href')
                writer.writerow({'year': year, 'authors':authors, 'title': title, 'abstract': abstract, 'html_link': newlink, 'pdf_link': pdf_link})
    csv_file.close()





        
        
    