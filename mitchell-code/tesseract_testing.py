# code for any tesseract image processing

import pandas as pd
import numpy as np

import skimage
from scipy import ndimage
from matplotlib import pyplot as plt

import os
from PIL import Image, ImageEnhance, ImageFilter, features
import cv2
import numpy as np

# Need to install pytesseract & tesseract-ocr, then set tesseract_cmd to path if there is an error
import pytesseract
pytesseract.pytesseract.tesseract_cmd = 'C:\\Users\\layto\\AppData\\Local\\Tesseract-OCR\\tesseract'


jpgPath = 'C:\\Users\\layto\\SampleWineCatalogs\\JPG_Files'

def turn_gray(image, save):
    img = Image.open(image)
    imgFiltered = img.filter(ImageFilter.MedianFilter())
    enhancer = ImageEnhance.Contrast(imgFiltered)
    imgEnchanced = enhancer.enhance(2)
    img = imgEnchanced.convert('L')
    return(img)
    img.save(save)

gray1 = turn_gray(jpgPath + '\\' + 'UCD_Lehmann_0407.jpg', 'edited_0407.jpg')
gray1.show()

l = [x for x in pytesseract.image_to_string(Image.open('edited_0407.jpg'), lang='eng+fr', config='--psm 4 --oem 2 -c tessedit_char_whitelist=0123456789abcdefghijklmnopqrstuvwxyz').split('\n') if len(x.split())>1]

print(l)

gray2 = turn_gray(jpgPath + '\\' + 'UCD_Lehmann_0893.jpg', 'edited_0893.jpg')
arr = np.asarray(gray2)
edges = cv2.Canny(arr,10,150)

canny = Image.fromarray(edges)
canny.save('canny_0893.jpg')
canny.show()
















