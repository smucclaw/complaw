# `dmnmd`: A Command-Line Interface to DMN Decision Tables In Plain Text

> Show me your flowchart and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won't usually need your flowchart; it'll be obvious." -- Fred Brooks, The Mythical Man Month (1975)

The semantics are DMN.

The syntax is Markdown.

(Hence the name; apologies to any aibohphobes.)

The input is plain-text.

The output is JS. (And, in future, XML, Python, English, LegalRuleML...)

The interface is CLI. No mouse needed!

## Download and Install

OS X:

    brew install haskell-stack
    
    stack install dmnmd

Linux:

    { yum, apt-get, ... } install haskell-stack

    stack install dmnmd
    
## Examples

This README contains decision tables formatted in plain text, in Markdown table syntax.

The DMN CLI (`dmnmd`) interpreter parses, evaluates, and translates them into alternative formats.

### Example 1: What's for dinner?

This example is taken from [Camunda's DMN Tutorial](https://camunda.com/dmn/).

| U | Season | Dish                         | # Annotation  |
|---|--------|------------------------------|---------------|
| 1 | Fall   | Spareribs                    |               |
| 2 | Winter | Roastbeef                    |               |
| 3 | Spring | Steak                        |               |
| 4 | Summer | Light Salad and a nice Steak | Hey, why not? |

A plain text version formatted for Markdown looks literally like this:

    | U | Season | Dish                         | # Annotation  |
    |---|--------|------------------------------|---------------|
    | 1 | Fall   | Spareribs                    |               |
    | 2 | Winter | Roastbeef                    |               |
    | 3 | Spring | Steak                        |               |
    | 4 | Summer | Light Salad and a nice Steak | Hey, why not? |

## Evaluation

A decision table is basically a function. Let's run it.

Interactively, on the command line:

    $ dmnmd README.md --dt="Example 1"
    Season? Winter
    Dish: Roastbeef
    
    Season? Spring
    Dish: Steak

Batch-mode:

    $ echo "Winter" | dmnmd README.md --dt="Example 1"
    Roastbeef

JSON in, JSON out.

    $ echo '{ "Season": "Winter" }' | dmnmd README.md --dt="Example 1"
    { "Season": "Winter", "Dish": "Roastbeef" }

### XML source

The canonical DMN XML representation of this example is available at https://github.com/camunda/camunda-bpm-examples/blob/master/dmn-engine/dmn-engine-java-main-method/src/main/resources/org/camunda/bpm/example/dish-decision.dmn11.xml

## Imports

### from Markdown

Supported. This is the native format for `dmnmd`.

ASCII has its limitations. In graphical decision tables, output columns are separated from input columns by a double bar; most GUI implementations use colour and other formatting to distinguish input, output, and annotation columns. In `dmnmd` syntax, output columns are optionally labeled with an `(out)`; annotation columns are prefixed with a `#`. By default, if the columns are unlabeled, the rightmost column will be taken to be the output, and columns to the left will be taken to be inputs. (Leaving out annotation columns.)

Columns are optionally typed using a colon. You will see `Column Name : String`, `Column Name : Number`, and `Column Name : Boolean`. If you omit the type definition, `dmnmd` will attempt to infer the type.

In some decision tables, the input and outputs are enumerated in a sort of sub-header row. The order matters.

This implementation only supports vertical layout. Horizontal and crosstab layouts may appear in a future version if there is demand.

The above is perhaps best explained by an example.



### from XML

On the roadmap.

    $ dmnmd --from=example1.dmn --to=example1.md

## Exports

Interactive evaluation is intended for quick testing in development.
For real-world use, you probably want *code generation* to an
operational language like Python or Javascript. Or *extraction* to SQL
or JSON (suitable for NoSQL) or to XML (as OMG intended). Or to
natural language!

### to Javascript

    $ dmnmd README.md --to=js

### to XML

    $ dmnmd README.md --to=xml

Exports to XML conforming to the DMN 1.3 specification.

### to XLSX

On the roadmap.

    $ dmnmd README.md --to=xlsx

Exports to an Excel spreadsheet.

### to SQL

On the roadmap.

    $ dmnmd README.md --to=sql

`dmnmd` outputs DDL, DML, and DQL statements suitable for SQLite, Postgres, and others:

    CREATE TABLE example_1 (row_id primary key, season text, dish text, annotation text);
    INSERT INTO  example_1 (row_id, season, dish, annotation) VALUES
        (1, "Fall",   "Spareribs", NULL),
        (2, "Winter", "Roastbeef", NULL),
        (3, "Spring", "Steak",     NULL),
        (4, "Summer", "Light Salad and a nice Steak", "Hey, why not?");

    -- Query
    SELECT row_id, dish, annotation FROM example_1 WHERE Season = ?;

What to do about rows that contain FEEL expressions? For lack of a better place, that logic goes into the query.

### to Python

On the roadmap.

    $ dmnmd README.md --to=python

### to Natural Language

On the roadmap.

    $ dmnmd README.md --to=english --dialect=Horn

    The dish is Spareribs when the Season is Fall.

    The dish is Roastbeef when the Season is Winter.

    The dish is Steak when the Season is Spring.

    The dish is Light Salad and a nice Steak when the Season is Summer. (Hey, why not?)

Brevity is a parameter which makes the output more concise.

    $ dmnmd README.md --to=english --dialect=Horn --brevity=2

    The dish is Spareribs when the Season is Fall, Roastbeef in Winter, Steak in Spring, and (Hey, why not?) Light Salad and a nice Steak in Summer.

More brevity requires more tacit knowledge. This is safer when we have an accompanying ontology to refer to.

    $ dmnmd README.md --to=english --dialect=Horn --brevity=3

    The dish is Spareribs when the Season is Fall, Roastbeef in Winter, Steak in Spring, and (Hey, why not?) Light Salad and a nice Steak in Summer.

It is characteristic of natural language that utterances omit "common sense" world knowledge, and employ other linguistic shorthand which is obvious to native speakers and often challenging to others.

    $ dmnmd README.md --to=english --dialect=Horn --brevity=4

    Spareribs in the Fall; Roastbeef in Winter; Steak in Spring; and (Hey, why not?) Light Salad and a nice Steak otherwise.

The above dialect is `Horn`, which uses an "output if input" ordering . Omitting that option, we get an "input then output" ordering:

    $ dmnmd README.md --to=english --brevity=4

    In the Fall, Spareribs; in Winter, Roastbeef; in Spring, Steak; and in Summer, Light Salad and a nice Steak (Hey, why not?) .

By default, brevity is 1.

    $ dmnmd README.md --to=english

    When the Season is Fall, the Dish is Spareribs.
    
    When the Season is Winter, the Dish is Roastbeef.
    
    When the Season is Spring, the Dish is Steak.
    
    When the Season is Summer, the Dish is Light Salad and a nice Steak (Hey, why not?).

Some linguistic magic happens behind the scenes. Different parameters take different determiners.

### to LegalRuleML

On the roadmap.

    $ dmnmd README.md --to=legalruleml --brevity=4

    ... <lrml:...> ...

### to Prolog

On the roadmap.

    $ dmnmd README.md --to=prolog --brevity=4

    dish("Spareribs") :- season("Fall").
    dish("Roastbeef") :- season("Winter").
    dish("Steak")     :- season("Spring").
    dish("Light salad and a nice Steak") :- season("Summer").

## Co-requisites

Your IDE may need a plugin to work with Markdown tables.
- VS Code: ["markdown table" extensions](https://marketplace.visualstudio.com/search?term=%22markdown%20table%22&target=VSCode&category=All%20categories&sortBy=Relevance)
- Atom: ["markdown table" packages](https://atom.io/packages/search?q=markdown+table)
- Vim: [vim extension markdown tables](https://www.google.com/search?q=vim+extension+markdown+tables)
- Emacs: You're all set. `M-x markdown-mode` and hit TAB after starting your table.

## Extensions

This implementation aims to extend DMN with higher-order functional programming capabilities. Input cells already can be what a functional programmer would call a "function section" -- a partially applied binary function curried to expect a single argument. Strictly speaking, DMN 1.3 output columns need to be "plain" values: strings, Booleans, and numbers. This implementation proposes to allow the same expressive range for output columns as input columns, so you could return a range, such as `[20..40]`, if you wanted.

## Translations

### Conversion to XLSX

## Background

Decision Model & Notation is [an XML-based standard from OMG](https://www.omg.org/spec/DMN/). One accessible tutorial is available [here](https://camunda.com/dmn/).

To help author DMN, a number of vendors provide graphical user interfaces as part of their [conforming implementations](https://dmn-tck.github.io/tck/). It is also possible to [import decision tables authored in a spreadsheet](https://github.com/camunda/camunda-dmn-xlsx).

What are decision tables? An ancient magic from an earlier age of computing, powerful but little known among developers today. See [Hillel Wayne's introduction](https://www.hillelwayne.com/post/decision-tables/). It may be making a comeback, though: a handful of [packages have appeared on npm](https://www.npmjs.com/search?q=dmn) in the last few years.

The [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) emphasizes the value of flat text files. While XML technically qualifies as text, many consider it "unwieldy": hence the popularity of [JSON](https://en.wikipedia.org/wiki/JSON) and [YAML](https://en.wikipedia.org/wiki/YAML).

Command-line utilities such as [json (on NPM)](https://www.npmjs.com/package/json) help manipulate JSON.


