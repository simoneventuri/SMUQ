import os
import sys

def file_len(filename):
    with open(filename) as file_object:
        for i, l in enumerate(file_object):
            pass
    return i + 1

def get_top_directory():
    top_directory = os.getcwd()
    return top_directory.strip()
