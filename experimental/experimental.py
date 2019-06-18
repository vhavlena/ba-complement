#!/usr/bin/env python3

"""
 Script for automated experimental evaluation.
 @title experimental.py
 @author Vojtech Havlena, April 2019
"""

import sys
import getopt
import subprocess
import string
import re
import os
import os.path
import resource

VALIDLINE = -2
TIMELINE = -1
STATESLINE = -2
DELAYSIM = -4
TIMEOUT = 300 #in seconds
QUOTIENT = ""

def main():
    #Input parsing
    if len(sys.argv) < 3:
        help_err()
        sys.exit()
    try:
        opts, args = getopt.getopt(sys.argv[3:], "ta:", ["tex", "auts="])
    except getopt.GetoptError as err:
        help_err()
        sys.exit()

    complbin = sys.argv[1]
    autfolder = sys.argv[2]
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
    print("Automaton: valid, Schewe, Schewe+comb, Schewe+purge")

    res = [None] * 4
    for autfile in files:
        filename = os.path.join(autfolder, autfile)

        res[0] = get_output([complbin, "--schewe", filename, QUOTIENT])
        res[1] = get_output([complbin, "--schewesim", filename, QUOTIENT])
        #res[2] = get_output([complbin, "--schewesimsat", filename])
        res[2] = get_output([complbin, "--schewesimrem", filename, QUOTIENT])

        print_output(filename, res)


def get_output(args):
    try:
        output = subprocess.check_output(args, \
            timeout=TIMEOUT).decode("utf-8")
        parse = parse_output(output)
    except subprocess.TimeoutExpired:
        parse = None, None, None, None
    return parse


def parse_output(output):
    lines = output.split('\n')
    lines = list(filter(None, lines)) #Remove empty lines
    valid = None #lines[VALIDLINE] == "Check: True"
    match = re.search("Time: ([0-9]+.[0-9]+)s", lines[TIMELINE])
    time = round(float(match.group(1)), 2)
    states = int(re.search("States: ([0-9]+)", lines[STATESLINE]).group(1))
    delsim = None #int(re.search("Delayed simulation: ([0-9]+)", lines[DELAYSIM]).group(1))
    return valid, time, states, delsim



def print_config(formulas):
    print("Timeout: {0}".format(TIMEOUT))
    print("Number of automata: {0}".format(formulas))


def format_output(parse):
    return "{0} {1}".format("N/A" if parse[0] is None else parse[0], "TO" if parse[2] is None else parse[2])


def format_output_con(parse):
    return "{0}".format("TO" if parse[2] is None else parse[2])


def print_output(filename, out):
    print("{0}: {1}\t {2}\t {3}".format(filename, format_output_con(out[0]), \
        format_output_con(out[1]), format_output_con(out[2])))


def help_err():
    sys.stderr.write("Bad input arguments. \nFormat: ./experimental [compl-bin]"\
        " [automata folder] [--tex] [--auts=X]\n")


if __name__ == "__main__":
    main()
