import random
import operator
import pandas as pd
from collections import Counter

import nltk, string
from sklearn.feature_extraction.text import TfidfVectorizer

from collections import Counter

import argparse

# parse arguments
def parse_args():
    '''
    Parses the arguments.
    '''
    parser = argparse.ArgumentParser(description="convert R result to submission.")

    parser.add_argument('--in_file', type = str, nargs='?', default='final_predicts.txt',
                        help='R result file')

    parser.add_argument('--out_file', type = str, nargs='?', default='tags.tsv',
                        help='R result file')

    return parser.parse_args()

args = parse_args()

tag_list = ['part-time-job', 'full-time-job', 'hourly-wage', 'salary', 'associate-needed',  'bs-degree-needed', 'ms-or-phd-needed', 'licence-needed', \
'1-year-experience-needed', '2-4-years-experience-needed', '5-plus-years-experience-needed', 'supervising-job']

def main():
    print ('Out to submission file.')
    with open (args.out_file, 'w') as out_f:
        out_f.write ('tags\n')
    with open (args.in_file, 'r') as f:
        for line in f:
            tags = map (int, line.split())
            res = ''
            for i in range (1, len(tags)):
                if tags[i] == 1:
                    if res == '':
                        res = tag_list[i-1]
                    else:
                        res = res + ' ' + tag_list[i-1]
            with open (args.out_file, 'a') as out_f:
                out_f.write (res + '\n')

if __name__ == '__main__':
    main()