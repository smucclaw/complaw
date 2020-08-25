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

        print("**", len(filled_author.publications),"publications");

        for pub in filled_author.publications:
            try:
                pub.fill()
                orgprint("***",pub.bib['year']+ " "+pub.bib['title']+" :filled:", pub)

                print(enpipe([pub.bib['year'],
                              pub.bib['title'],
                              pub.bib['journal']]))

            except:
                print("***",pub.bib['year']+ " "+pub.bib['title']+" :error:")
                print("WARNING: caught exception:", sys.exc_info()[0])
