# L4 Metadata Tool

The `l4meta` tool is a command line tool to read/write metadata to/from a document.

## Requirements

- Python 3.x
    - `pyyaml`
- `exiftool`

## Installation

### From Source Code

```sh
git clone git@github.com:smucclaw/complaw.git
cd complaw/
git checkout xmp
cd doc/ex-xmp-20200808/xmp/
pip install -r requirements.txt
```

#### NOTE
- Make sure to checkout to the `xmp` branch, and run the command inside `complaw/doc/ex-xmp-20200808/xmp/l4meta`, as this is the main entrypoint for running the `l4meta` tool.
- The documentation will use `l4meta` as the starting point, which is an alias of `python l4meta.py`. When running the commands below, you will need to reference the files relative to the `l4meta` directory. For example, if you are going to read **greeting.pdf**, you will need to reference accordingly: `python l4meta.py read ../demo/greeting.pdf`. This workaround is needed as this package has yet to be deployed to pypi; once deployed you can reference to the file directly.

### For Debian / Ubuntu

**Please note that `l4meta` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

You will need to install the following dependencies:

```sh
apt-get install exiftool
pip install pyyaml
pip install l4meta
```

### For macOS

**Please note that `l4meta` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

```sh
brew install exiftool
pip install pyyaml
pip install l4meta
``` 

### For Windows

Work in progress.

## Quick Start

In the `demo/` directory you will find:
- plain.pdf
- greeting.pdf

### Reading

```console
$ l4meta read [FILE]
```

For example, to read **greeting.pdf**, execute the following command:

```console
$ l4meta read greeting.pdf
```

The output will be the metadata of **greeting.pdf** in **json** format, as below:

```console
{
    "greeting": "Hello World!"
}
```

Adding a `--type yaml`, `--yaml` or `-y` flag like so:

```console
$ l4meta read -y greeting.pdf
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
$ l4meta write [INPUT DOCUMENT] [OUTPUT DOCUMENT] [METADATA]
```

For writing, metadata is always written to a duplicate copy of your existing document and never to your original copy. Therefore you must specify the location of your existing document as the **INPUT** and the location where you want to duplicate your document to as the **OUTPUT**. You must also specify a location for the metadata.

For example, to write the same metadata in **greeting.pdf** into **plain.pdf**, execute any of the following commands, all of which perform the same function. Note that you should write metadata into a new document, in this case, to **plain2.pdf**.

```console
$ l4meta read greeting.pdf > greeting.json
$ l4meta write plain.pdf plain2.pdf greeting.json
```

```console
$ l4meta read greeting.pdf > greeting.json
$ l4meta write plain.pdf plain2.pdf < greeting.json
```

```console
$ l4meta read greeting.pdf | l4meta write plain.pdf plain2.pdf
```

You can confirm that **plain2.pdf** has the metadata by running:

```console
$ l4meta read plain2.pdf
```

It will return:

```console
{
    "greeting": "Hello World!"
}
```

#### NOTE

We're working on making the entry for the **OUTPUT** optional, in which case the program will duplicate the document for you. For example, the program will automatically duplicate **plain.pdf** to **plain_v1.pdf** in the same directory if no `--output` flag with the new filename is defined. It will probably look something like this:

```console
$ l4meta read greeting.pdf | l4meta write plain.pdf
$ l4meta read greeting.pdf | l4meta write plain.pdf --output plain2.pdf
```

## Command Line Options

### Read

To be updated

### Write

To be updated

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

l4vc is under construction.

## Future Work

- [x] Add support for `yaml`
- [x] Add support for piping between reading and writing operations
- [ ] Make OUTPUT file for write optional, and let program automatically duplicate file if no value is supplied
- [ ] Add support for reading/writing metadata from `docx` files
- [ ] Combine both read/write modes into a single mode, with read as default 
