# L4 Metadata Tool

The `l4meta` tool is a command line tool to read/write metadata to/from a document.

## Features

- Read metadata directly from PDFs
- Write metadata into a PDF
- Specify metadata in `json` and `yaml` formats

## Requirements

- Python 3.8 and above
- `exiftool`

## Installation

**Please note that `l4meta` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

### From Source Code

```sh
git clone git@github.com:smucclaw/complaw.git
cd complaw/
git checkout xmp
cd doc/ex-xmp-20200808/xmp/
python setup.py install
```

### For Debian / Ubuntu

You will need to install the following dependencies:

```sh
apt-get install exiftool
pip install l4meta
```

### For macOS

```sh
brew install exiftool
pip install l4meta
``` 

### For Windows

- [Python](https://www.python.org/) or [Anaconda](https://www.anaconda.com/products/individual#Downloads)
- [exiftool](https://exiftool.org/)

In the Command Prompt or Powershell, run:

```powershell
pip install l4meta
```

## Quick Start

In the `demo/` directory you will find:
- plain.pdf
- greeting.pdf

### Reading

```console
$ l4meta [file ...]
```

For example, to read **greeting.pdf**, execute the following command:

```console
$ l4meta greeting.pdf
```

The output will be the metadata of **greeting.pdf** in **json** format, as below:

```console
{
    "greeting": "Hello World!"
}
```

Adding a `--type yaml`, `--yaml` or `-y` flag like so:

```console
$ l4meta -y greeting.pdf
```

...will cause the output the metadata of the same **greeting.pdf** to be in **yaml**:

```console
greeting: Hello World!

```

However, if you run the same command for **plain.pdf**, it will return:

```console
{ }
```

### Writing

```console
$ l4meta file [--write [file ...]] [--meta [file]]
```

For writing, metadata is always written to a duplicate copy of your existing document and never to your original copy. You must specify the location of your original file which you intend to write your metadata, as well as the location of your duplicate file in the `-w` or `--write` flag, which will be the same copy as your original file with metadata written. The original file remains completely untouched; however, any existing metadata that you have in your original copy will be overwritten in your duplicate copy.

When writing metadata to a single file, you must also specify the `-m` or `--meta` flag, which is the location of your metadata.

For example, to write the same metadata in **greeting.pdf** into **plain.pdf**, execute any of the following commands, all of which perform the same function. The metadata will be written into a new **plainv1.pdf**, which is a duplicate of **plain.pdf**, but with metadata.

```console
$ l4meta greeting.pdf > greeting.json
$ l4meta plain.pdf --write plainv1.pdf --meta greeting.json
```

```console
$ l4meta greeting.pdf > greeting.json
$ l4meta plain.pdf --write plainv1.pdf --meta < greeting.json
```

```console
$ l4meta greeting.pdf | l4meta plain.pdf --write plainv1.pdf --meta
```

You can verify that your new **plainv1.pdf** file has the metadata; just follow the instructions from the [Reading](#reading) section above but replace the filename to be read with the location of your new file, e.g. **plainv1.pdf**.

## Command Line Options

```console
usage: l4meta [-h] [--type {json,yaml} | -j | -y] [-w [file ...]] [-m [file]]
              [file ...]

Read/Write L4 metadata

positional arguments:
  file                  location of document

optional arguments:
  -h, --help            show this help message and exit
  --type {json,yaml}    specify metadata output format
  -j, --json            output metadata in JSON, same as --type json
  -y, --yaml            output metadata in YAML, same as --type yaml
  -w [file ...], --write [file ...]
                        location of document to be written
  -m [file], --meta [file]
                        location of metadata
```

## Integrations and Workflows

### I have multiple physical revisions of the same logical PDF.

I started with a PDF, let's call it "base". This was a contract that multiple parties signed a few years ago.

Subsequently, I created a new PDF, let's call it "v2", whose _raison d'etre_ was to revise some of the information in "base": maybe it changed the expiry date of the original or changed some of the
parties. This was an amendment letter signed by all the relevant parties.

Subsequently, there were "v3" and "v4" containing yet more updates to the original document: maybe the dollar amounts changed, or an interest rate.

Now I want to compose "base" with all the subsequent versions to show a snapshot of the latest version of reality.

Commentary: from a CS point of view, this is a problem in version control: Git is the natural framework for thinking about these things. The initial PDF, "base", corresponds to the first commit of a file. The second PDF, "v2", patches the "base" file in some way. The result: a snapshot.

We can also think about this from the point of view of [temporal databases](https://en.wikipedia.org/wiki/Temporal_database).

In future, `l4meta` will ship with a related utility called `l4vc` which does this job. It assumes that your PDFs contain XMP that contain JSON that conforms to the schema defined by the L4 project, which establishes open conventions for computational contracts. `l4vc` stands for "L4 version control" and will flatten multiple JSON-augmented PDFs to a current latest-state JSON.

`l4vc` is under construction.

## Future Work

- [ ] Add batch mode
- [ ] Add support for converting `docx` to `pdf` and writing metadata in a single write operation
