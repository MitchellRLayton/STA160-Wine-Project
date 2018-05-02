# Script for testing docx files that Duncan provided from PDF Pen Pro Results

%matplotlib inline
import pandas as pd
import numpy as np

import os
from PIL import Image, ImageEnhance, ImageFilter, features
import cv2

import docx
import pytesseract
pytesseract.pytesseract.tesseract_cmd = 'C:\\Users\\layto\\AppData\\Local\\Tesseract-OCR\\tesseract'

docx_path = 'C:\\Users\\layto\\SampleWineCatalogs\\Docx_Files'
docx_files = os.listdir(docx_path)
one = docx_path + '\\' + docx_files[0]


doc1 = docx.Document(one)

doc_text = []
for i in range(len(doc1.paragraphs)):
    index = str(docx_files[0])+ ': ' + str(i+1)
    text = doc1.paragraphs[i].text
    make_lines = text.split('\n')
    doc_text.append(make_lines)

list_of_text = [y for y in [x[0] if len(x) == 1 else ''.join(x) for x in doc_text] if y != '']

print(list_of_text)


fixer = []
for line in list_of_text:
    if line.find('(') != -1 or line.find(')') != -1:
        fix1 = ' '.join(line.split('\t'))
        fix2 = ' '.join([x for x in fix1.split(' ') if x != ''])
        fix3 = [y for y in [x for x in fix2.split(' ')] if y != '.']
        fixer.append(str(' '.join(fix3)))

test = [x for x in fixer if len(x)<40]
for i in test:
    print(i)











