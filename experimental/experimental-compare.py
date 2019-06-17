#!/usr/bin/env python3

"""
 Script for automated experimental evaluation.
 @title experimental.py
 @author Vojtech Havlena, June 2019
"""

import sys
import getopt
import subprocess
import string
import re
import os
import os.path
import resource
import xml.etree.ElementTree as ET

VALIDLINE = -2
TIMELINE = -1
STATESLINE = -3
DELAYSIM = -4
TIMEOUT = 300 #in seconds
INPUTG = "in.gff"

def main():
    #Input parsing
    if len(sys.argv) < 4:
        help_err()
        sys.exit()
    try:
        opts, args = getopt.getopt(sys.argv[4:], "ta:", ["tex", "auts="])
    except getopt.GetoptError as err:
        help_err()
        sys.exit()

    complbin = sys.argv[1]
    gbin = sys.argv[2]
    autfolder = sys.argv[3]
    texout = False
    AUTOMATA = 20

    for o, a in opts:
        if o in ("-t", "--tex"):
            texout = True
        if o in ("-a", "--auts"):
            AUTOMATA = int(a)

    #Experiments

    files = [f for f in os.listdir(autfolder) \
        if os.path.isfile(os.path.join(autfolder, f)) and \
            f.endswith(".ba")]
    files.sort()
    files = files[:AUTOMATA]

    print_config(AUTOMATA)
    print("Automaton: safra, ramsey, slice")

    res = [None] * 3
    for autfile in files:
        filename = os.path.join(autfolder, autfile)

        subprocess.run([complbin, "--goal", filename, "-o", INPUTG])
        res[0] = get_output([gbin, "complement", "-m", "safra", INPUTG])
        res[1] = get_output([gbin, "complement", "-m", "ramsey", "-r", INPUTG])
        res[2] = get_output([gbin, "complement", "-m", "slice", "-r", INPUTG])

        print_output(filename, res)


def get_output(args):
    try:
        output = subprocess.check_output(args, \
            timeout=TIMEOUT).decode("utf-8")
        parse = parse_output(output)
    except subprocess.TimeoutExpired:
        parse = None
    return parse


def parse_output(output):
    root = ET.fromstring(output)
    states = len(root.findall("StateSet/State"))
    return states



def print_config(formulas):
    print("Timeout: {0}".format(TIMEOUT))
    print("Number of automata: {0}".format(formulas))


def format_output_con(parse):
    return "{0}".format("TO" if parse is None else parse)


def print_output(filename, out):
    print("{0}: {1}\t {2}\t {3}".format(filename, format_output_con(out[0]), \
        format_output_con(out[1]), format_output_con(out[2])))


def help_err():
    sys.stderr.write("Bad input arguments. \nFormat: ./experimental-compare [compl-bin]"\
        " [g-bin] [automata folder] [--tex] [--auts=X]\n")


if __name__ == "__main__":
    main()
