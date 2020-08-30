#!python3.8

from scholarly import scholarly
import sys, re, json, time

yearCutoff = 2010

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
      for author in scholarly.search_author(name):
        orgprint("*", name, author)

        print("filling author...")
        filled_author = author.fill(sections=['publications'])

        try:
            orgprint("**", "filled", filled_author)
        except:
            print("WARNING: caught exception:", sys.exc_info()[0])

        print("**", len(filled_author.publications),"publications");

        for pub in filled_author.publications:
          bibYear    = pub.bib.get('year','NilYear')
          bibTitle   = pub.bib.get('title','NilTitle')
          if pub.bib.get('year',0) < yearCutoff:
            print("***",bibYear +" " + bibTitle + " :skipped:")
          else:
            try:
                # need to fill to get the journal details
                pub.fill()
                bibJournal = pub.bib.get('journal','NilJournal')

                orgprint("***",bibYear+ " "+bibTitle+" :filled:", pub)

                print(enpipe([bibYear, bibTitle, bibJournal]))
                time.sleep(1) # self-throttling

            except:
                # print("***",pub.bib.get('year','NilYear') + " "+pub.bib.  get('title','NilTitle')+" :error:")
                print("WARNING: caught exception:", sys.exc_info()[0])
