# Script for testing docx files that Duncan provided from PDF Pen Pro Results

import pandas as pd
import numpy as np

import os
from PIL import Image, ImageEnhance, ImageFilter, features
import cv2

import docx
import pytesseract
pytesseract.pytesseract.tesseract_cmd = 'C:\\Users\\layto\\AppData\\Local\\Tesseract-OCR\\tesseract'

docxPath = 'C:\\Users\\layto\\SampleWineCatalogs\\Docx_Files'

class wordDocx():
    
    def __init__(self, path):
        self.path = path
    
    def fileList(path):
        '''
        Generates full file paths to Docx files given relative directory input
        '''
        import os
        fullPathFiles = []
        file = os.listdir(path)
        for doc in range(len(file)):
            fullPathFiles.append(path + '\\' + file[doc])
        return(fullPathFiles)
    
    def getAllParagraphs(files):
        import docx
        if isinstance(files,list) is True:
            docPara = []
            for file in files:
                iterDoc = docx.Document(file)
                docText = []
                for par in range(len(iterDoc.paragraphs)):
                    text = iterDoc.paragraphs[par].text
                    makeLines = text.split(' ')
                    if list(textParse.whichIsMore(makeLines).items())[0][0] == '\n':
                        newLines = text.split('\n')
                        docText.append(newLines)
                    elif list(textParse.whichIsMore(makeLines).items())[0][0] == '\t':
                        newLines = text.split('\t')
                        docText.append(newLines)
                listOfText = [y for y in [x[0] if len(x) == 1 else ' '.join(x) for x in docText] if y != '']
                docPara.append(listOfText)
            return(docPara)

        else:
            raise AttributeError('Needs list of full file paths to the .docx files')
        
    
#     def processParagraphs(paragraphs):
#         docText = []
#         for f in range(len(files)):

class textParse():

    def __init__(self, string):
        self.string = string
    
    def findParenth(userInput):
        '''
        Takes a string or a list of strings and finds any parenthesis
        Returns ???
        '''
        if isinstance(userInput,str) is True:
            if string.find('(') != -1 or string.find(')') != -1:
                return(string)
            else:
                return(0)
        elif isinstance(userInput,list) is True:
            found = []
            for line in userInput:
                if line.find('(') != -1 or line.find(')') != -1:
                    found.append(line)
                else:
                    continue
            return(found)
        else:
            raise AttributeError('Only input strings and lists of strings')
        
    def tabInString(string):
        '''
        String input to find tabs: '\t'
        Returns True/False
        '''
        if string.find('\t') != -1:
            return(True)
        else:
            return(False)

    def newLineInString(string):
        '''
        String input to find tabs: '\n'
        Returns True/False
        '''
        if string.find('\n') != -1:
            return(True)
        else:
            return(False)
        
    def whichIsMore(self):
        '''
        Input a list of string (say if you split the text on spaces)
        Returns tuple of counts of '\n' and '\t' respectively
        '''
        if self and isinstance(self,list) is True:
            counterN = 0
            counterT = 0
            for line in range(len(self)):
                if self[line].find('\n') != -1:
                    counterN += 1
                elif self[line].find('\t') != -1:
                    counterT += 1
                elif self[line].find('\n') != -1 and self[line].find('\t') != -1:
                    counterN += 1
                    counterT += 1
            result = (counterN,counterT)
            if result[0] > result[1]:
                return({'\n':result[0],'by':result[0]-result[1]})
            else:
                return({'\t':result[1],'by':result[1]-result[0]})
        else:
            raise AttributeError('Input a list of strings')
        
    def disjointNames(self):
        '''
        Finds disjoint text where name of wine, parenthesis, and prices are split up 
        '''
        
        return(joint)



if __name__ == "__main__":
    allText = wordDocx.getAllParagraphs(wordDocx.fileList(docxPath))
    print(allText)


