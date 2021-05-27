#a program to convert multiple text files into one long text file

import csv
import glob
import pandas


read_files = glob.glob("D:/Corpora/KAIST POS Tagged/**/*.txt", recursive=True)

with open("KAIST POS.txt", "wb") as outfile:
    for f in read_files:
        with open(f, "rb") as infile:
            outfile.write(infile.read())

read_files2 = glob.glob("D:/Corpora/KAIST Automatically Analyzed Corpus/**/*.txt", recursive=True)
with open("KAIST Automatically Analyzed Corpus.txt", "wb") as outfile:
    for f in read_files2:
        with open(f, "rb") as infile:
            outfile.write(infile.read())


# -*- coding: 949 -*-

import csv
import pandas as pd
import codecs
from encodings import euc_kr
from encodings import cp949


content = []
with open('D:/Corpora/Corpora Code/KAIST Automatically Analyzed Corpus.txt', 'r', encoding = 'euc_kr',errors='ignore') as file_in:
    content = file_in.readlines()

new_content = []
for line in content:
    clean_line = line.replace('\00', '')
    new_content.append(clean_line)

with open('D:/Corpora/Corpora Code/outfile.csv', 'w', encoding = 'euc_kr', errors='ignore') as file_out:
    file_out.writelines(new_content)

        
# -*- coding: 949 -*-

import csv
import re
import sys
import sys
import csv
maxInt = sys.maxsize

while True:
    # decrease the maxInt value by factor 10 
    # as long as the OverflowError occurs.

    try:
        csv.field_size_limit(maxInt)
        break
    except OverflowError:
        maxInt = int(maxInt/10)
with open('D:/Corpora/Corpora Code/outfile.csv', 'r', encoding = 'euc_kr', errors='ignore') as file_in:
    with open('D:/Corpora/Corpora Code/outfile_revised.csv', 'w', encoding = 'euc_kr', errors='ignore') as file_out:
        corpus_writer = csv.writer(file_out, delimiter=',', lineterminator='\n')
        content=csv.reader(file_in, delimiter = '\t')
        corpus_writer.writerow(["Full word", "First Morpheme", "First Morpheme Tag", "Second Morpheme", "Second Morpheme Tag", "Third Morpheme", "Third Morpheme Tag", "Fourth Morpheme", "Fourth Morpheme Tag", "Fifth Morpheme", "Fifth Morpheme Tag", "Sixth Morpheme", "Sixth Morpheme Tag"])
        for line in content:
            if len(line) == 2:
                out = [line[0]] + re.split('/|\\+', line[1])
                corpus_writer.writerow(out)
            else:
                corpus_writer.writerow(line)