#!/usr/bin/python3
import re
def prsr1 (str):
    if len(str) == 1:
       if ord('0') <= ord(str) <= ord('9'):
           return(int(str))
       else:
           print("i can only handle a simple integer right now")
           raise ValueError
    else:
        print("i can only handle a single character right now")
        raise ValueError
print("2 should be 2:", prsr1("2") == 2)

def prsr2 (str):
    tokens = re.split(r'\b',str)
    if len(tokens) == 3:
        return(tokens[1])
    if len(tokens) == 5:
        if (tokens[2] == "*"): return int(tokens[1]) * int(tokens[3])
        if (tokens[2] == "+"): return int(tokens[1]) + int(tokens[3])
print(prsr2("2+2"), "should be 4")
