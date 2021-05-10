# L4 Metadata Tool

The `l4meta` tool is a command line tool to read/write metadata to/from a document.

## Features

- Read/Write metadata directly from/to PDFs
- Specify metadata in `json` and `yaml` formats
- Platform agnostic, can be run on Windows, macOS and Linux

## Requirements

- Python 3.8 and above
    - Note: If you have both Python 2 and 3 installed on your machine, use `python3` or `pip3` in place of `python` and `pip` respectively
- `exiftool`

## Installation

**Please note that `l4meta` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

### From Source Code

```sh
git clone git@github.com:smucclaw/l4meta.git
pip install .
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

## Usage

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

## Future Work

- [ ] Add batch mode
- [ ] Add support for converting `docx` to `pdf` and writing metadata in a single write operation
