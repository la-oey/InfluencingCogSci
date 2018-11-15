#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 12 20:31:03 2018

@author: loey
"""

import urllib.request
import re
import PyPDF2
#import textract

pdfFileObj = open('pdf/p875.pdf','rb')
pdfReader = PyPDF2.PdfFileReader(pdfFileObj)
print(pdfReader.numPages)

pageObj = pdfReader.getPage(0)
print(pageObj.extractText())


#text = textract.process("pdf/p875.pdf")

import pdftotree

pdftotree.parse('pdf/p875.pdf', html_path=None, model_type=None, model_path=None, favor_figures=True, visualize=False)

