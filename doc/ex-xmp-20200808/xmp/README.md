# XMP JSON

This is a command line tool to read/write JSON encoded in XMP to/from PDF.

## Installation

First, install dependencies:

#### if we build based on python-xmp-toolkit

    apt-get install libexempi-dev
    brew install exempi
    pip install xmpjson-cli

#### if we build based on exiftool

    brew install exiftool
    brew install Archive::Zip
    pip install xmpjson-cli

## Quick Start

In the `demo/` directory you will find:
- plain.pdf
- greeting.pdf

Run

    xmpjson greeting.pdf
    
It returns:

    {
      "greeting": "Hello, World!"
    }

If you run `xmpjson plain.pdf` you get nothing back:

    { }
    
Now, augment `plain.pdf` with the JSON greeting:

    xmpjson greeting.pdf | xmpjson plain.pdf --write new.pdf

As you can see, when `xmpjson` is called with `--write`, it reads
STDIN and produces a new PDF that contains the JSON, encoded in XMP.

You can confirm that the metadata is there:

    xmpjson new.pdf
    
returns:

    {
      "greeting": "Hello, World!"
    }

## File Safety

`xmpjson` never changes the input PDF; it always produces a new PDF.
If you want to edit-in-place, it is up to you to write a wrapper
script that verifies that the output PDF is correct before renaming or
overwriting the original PDF.

WARNING: Avoid ever running `xmpjson myfile.pdf < new.json > myfile.pdf`

This is a classic command-line antipattern. It will clobber myfile.pdf. Do not do that. Always output to a new file.

## Command Line Options

| short | long            | description                                       |
|:------|:----------------|:--------------------------------------------------|
| -y    | --yaml          | read and write output in YAML instead of JSON     |
| -b    | --batch         | read *.pdf and save JSON output to *.json         |
| -w    | --write OUTFILE | write to an output PDF named OUTFILE              |
| -j    | --json  INFILE  | when doing a --write, read input JSON from INFILE |

### Enumerating Some Possibilities

    metareader greeting.pdf
    metareader -j greeting.pdf # -j is default
    metareader -y greeting.pdf
    metareader greeting.pdf > meta.json

    metareader greeting.pdf | metareader plain.pdf --write new.pdf
    metareader plain.pdf --write new.pdf < meta.json
    metareader plain.pdf --write new.pdf < meta.yaml
    # metareader plain.pdf -j meta.json --write new.pdf
    # metareader plain.pdf -y meta.yaml --write new.pdf

- Reading is always to stdout
- Able to handle pipes and redirection
- May need to have some form of meta file type detection
- `exiftool` does not support `yaml` format, will need to incorporate a Python dictionary or JSON to/from YAML converter

1. Handle read from pdf, stdout to JSON
1. Handle read from pdf, stdout to YAML
1. In write mode, allow pipes from stdin
1. In write mode, allow redirection from stdin

## Cookbook

Commonly performed tasks:

### I want to extract JSON from a PDF file to STDOUT

    xmpjson myfile.pdf
    
### I want to insert JSON into a PDF file from STDIN

Assuming `metadata.json` contains the JSON you want to add, pipe it in:

    xmpjson myfile.pdf --write new.pdf < metadata.json

### I want to extract JSON from a PDF file to a JSON file

Just redirect STDOUT to the desired path.

    xmpjson myfile.pdf > metadata.json

### I want to insert JSON into a PDF file from a JSON file

    xmpjson myfile.pdf --json metadata.json --write new.pdf
    
### I want to output PDF to STDOUT

    xmpjson myfile.pdf --json metadata.json --write - > new.pdf

The convention for this is to give `-` as the output filename.

### I want to extract JSON from multiple PDF files

    xmpjson --batch *.pdf
    
If you have `alice.pdf`, `bob.pdf`, and `carol.pdf`, this will produce `alice.json`, `bob.json`, and `carol.json`, but only if none of those `*.json` already exist. If they do, `xmpjson` will abort with an error.

### I want to insert different JSON into multiple PDF files

    xmpjson --batch --write *.json
    
If you have a directory containing

- alice.json
- alice.pdf
- bob.json
- bob.pdf
- carol.json
- donna.pdf

This command will create

- alice.xmpjson.pdf
- bob.xmpjson.pdf

## Integrations and Workflows

### I have multiple physical revisions of the same logical PDF.

I started with a PDF, let's call it "base". This was a contract that
multiple parties signed a few years ago.

Subsequently, I created a new PDF, let's call it "v2", whose _raison
d'etre_ was to revise some of the information in "base": maybe it
changed the expiry date of the original or changed some of the
parties. This was an amendment letter signed by all the relevant
parties.

Subsequently, there were "v3" and "v4" containing yet more updates to
the original document: maybe the dollar amounts changed, or an
interest rate.

Now I want to compose "base" with all the subsequent versions to show
a snapshot of the latest version of reality.

Commentary: from a CS point of view, this is a problem in version
control: Git is the natural framework for thinking about these things.
The initial PDF, "base", corresponds to the first commit of a file.
The second PDF, "v2", patches the "base" file in some way. The result:
a snapshot.

We can also think about this from the point of view of [temporal databases](https://en.wikipedia.org/wiki/Temporal_database).

In future, `xmpjson` will ship with a related utility called `l4vc`
which does this job. It assumes that your PDFs contain XMP that
contain JSON that conforms to the schema defined by the L4 project,
which establishes open conventions for computational contracts. `l4vc`
stands for "L4 version control" and will flatten multiple
JSON-augmented PDFs to a current latest-state JSON.

l4vc is under construction.

## Requirements

Future directions:

- add support for metadata in Docx files, just as with PDF files.
