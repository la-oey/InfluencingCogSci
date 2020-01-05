import sys
reload(sys)  
sys.setdefaultencoding('utf8')
sys.path.append("/usr/local/lib/python2.7/site-packages/")
sys.setrecursionlimit(1500)
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfparser import PDFParser
from pdfminer.pdfdocument import PDFDocument
from StringIO import StringIO

import os
import csv
from os import listdir
from os.path import isfile, join
import urllib2
from urllib2 import Request
from bs4 import BeautifulSoup
import re

abstractLinks = []

def pdf_from_url_to_txt(url):
    rsrcmgr = PDFResourceManager()
    retstr = StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(rsrcmgr, retstr, codec=codec, laparams=laparams)
    # Open the url provided as an argument to the function and read the content
    f = urllib2.urlopen(urllib2.Request(url)).read()
    # Cast to StringIO object
    fp = StringIO(f)
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    password = ""
    maxpages = 0
    caching = True
    pagenos = set()
    for page in PDFPage.get_pages(fp,
                                  pagenos,
                                  maxpages=maxpages,
                                  password=password,
                                  caching=caching,
                                  check_extractable=True):
        interpreter.process_page(page)
    fp.close()
    device.close()
    str = retstr.getvalue()
    retstr.close()
    return str

def checkLink(url):
	try:
		urllib2.urlopen(url)
		return True
	except Exception:
		return False


abstractpage = urllib2.urlopen("https://papers.nips.cc/paper/8296-multimodal-model-agnostic-meta-learning-via-task-aware-modulation")
asoup = BeautifulSoup(abstractpage, 'html.parser')

# extracts title
title = asoup.find('h2').get_text()
print(title)

# extracts authors
authorList = []
for auth in asoup.find_all("li"):
    authorList.append(auth.get_text())
authors = ', '.join(authorList)
print(authors)

# extracts abstract
abstract = asoup.find("p", attrs={'class':'abstract'}).get_text()
print(abstract)

# extracts pdf link
pdf_link = asoup.find("meta", attrs={'name':'citation_pdf_url'})['content']
print(pdf_link)

full_text = 'NA'
try:
	if pdf_link != 'NA' and checkLink(pdf_link):
		full_text = pdf_from_url_to_txt(pdf_link)
		print(full_text)
except Exception, e:
	pass
except urllib2.HTTPError:
	pass


# with open('nips_abstracts.csv', 'w') as csv_file:
#     fieldnames = ['year', 'authors', 'title', 'abstract', 'html_link', 'pdf_link', 'full_text']
#     writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
#     writer.writeheader()

#     homepagestr = "https://papers.nips.cc/"
#     homepage = urllib.request.urlopen(homepagestr)
#     soup = BeautifulSoup(homepage, 'html.parser')
#     for link in soup.find_all('a'):
#     	newlink = link.get('href')
#     	if 'book' in newlink:
#     		# extracts publication year from file name
#     		year = newlink[-4:]
#     		#print(year)

# 	    	yearpage = urllib.request.urlopen(homepagestr+newlink)
# 	    	ysoup = BeautifulSoup(yearpage, 'html.parser')
# 	    	for ylink in ysoup.find_all('a'):
# 	    		abstractlink = ylink.get('href')
# 	    		if 'paper' in abstractlink and abstractlink not in abstractLinks:
# 	    			abstractpage = urllib.request.urlopen(homepagestr+abstractlink)
# 	    			asoup = BeautifulSoup(abstractpage, 'html.parser')

# 	    			# extracts title
# 	    			title = asoup.find('h2').get_text()
# 	    			print(title)

# 	    			# extracts authors
# 	    			authorList = []
# 		            for auth in asoup.find_all("li"):
# 		                authorList.append(auth.get_text())
# 		            authors = ', '.join(authorList)

# 		            # extracts abstract
# 		            abstract = asoup.find("p", attrs={'class':'abstract'}).get_text()

# 		            # extracts pdf link
# 		            pdf_link = asoup.find("meta", attrs={'name':'citation_pdf_url'})['content']




	    	