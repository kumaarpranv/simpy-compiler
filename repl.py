from Lexer import *
from Token import *
from Parser import *
from Evaluator import *
from Object import *

PROMPT = ">> "


def printParserErrors(errors):
    for msg in errors:
        print(msg, end="\n")


def Start():
    env = Environment()
    while True:
        print(PROMPT, end="")
        scanned = input()
        if not scanned:
            return
        line = scanned
        l = Lexer.New(line)
        p = Parser.New(l)

        prog = p.ParseProgram()
        if len(p.Errors()) != 0:
            printParserErrors(p.Errors())
            continue

        evaluated = Eval(prog, env)
        if evaluated != None:
            print(evaluated.Inspect(), end="\n")
