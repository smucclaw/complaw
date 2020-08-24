#!python3.8

from scholarly import scholarly
import sys, re, json

def orgprint (stars, title, text):
    print(stars, title)
    print("#+begin_example")
    print(text)
    print("#+end_example")

def enpipe (x):
    return ("| " + " | ".join(x) + " |")

for line in sys.stdin:
    names = re.split('\s*=\s*', line)
    for name in names:
        author = next(scholarly.search_author(name))
        orgprint("*", name, author)

        print("filling author...")
        filled_author = author.fill(sections=['publications'])

        orgprint("**", "filled", filled_author)

        print("** publications");

        for pub in filled_author.publications:
            orgprint("***",pub.bib['year']+ " "+pub.bib['title'], pub)
            pub.fill()
            orgprint("***",pub.bib['year']+ " "+pub.bib['title']+" :filled:", pub)
