# L4 Metadata Tool

The `l4metadata` tool is a command line tool to read/write metadata to/from a document.

## Installation

### From Source Code

```sh
git clone git@github.com:smucclaw/complaw.git
cd complaw/
git checkout xmp
cd doc/ex-xmp-20200808/xmp/xmp_pdf/
```

#### NOTE
- Make sure to checkout to the `xmp` branch, and run the command inside `complaw/doc/ex-xmp-20200808/xmp/xmp_pdf`, as this is the main entrypoint for running the `l4metadata` tool.
- The documentation will use `l4metadata` as the starting point, which is an alias of `python terminal.py`. You can alias the command or add the following to your \*rc file, so long as the command is run inside `complaw/doc/ex-xmp-20200808/xmp/xmp_pdf`.

```sh
alias l4metadata='python l4meta.py'
```

### For Debian / Ubuntu

**Please note that `l4metadata` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

You will need to install the following dependencies:

```sh
apt-get install exiftool
pip install l4metadata
```

### For macOS

**Please note that `l4metadata` package has yet to be deployed to pypi, so you might want to follow the instructions from the section [From Source Code](#from-source-code) after installing `exiftool`.**

```sh
brew install exiftool
pip install l4metadata
``` 

### For Windows

Work in progress.

## Quick Start

In the `demo/` directory you will find:
- plain.pdf
- greeting.pdf

### Reading

```console
$ l4metadata read [FILE]
```

For example, to read `greeting.pdf`, execute the following command:

```console
$ l4metadata read greeting.pdf
```

The output will be the metadata of `greeting.pdf` in `json` format, as below:

```console
{
    "greeting": "Hello World!"
}
```

However, if you run the same command for `plain.pdf`, it will return:

```console
{ }
```

### Writing

```console
$ l4metadata write [INPUT] [OUTPUT]
```

For example, to write the same metadata in `greeting.pdf` into `plain.pdf`, execute the following command. Use the `--meta` flag to specify the location of the metadata file. Note that you should write metadata into a new document, in this case, to `plain2.pdf`.

```console
$ l4metadata read greeting.pdf > greeting.json
$ l4metadata write --meta greeting.json plain.pdf plain2.pdf
```

You can confirm that `plain2.pdf` has the metadata by running:

```console
$ l4metadata read plain2.pdf
```

It will return:

```console
{
    "greeting": "Hello World!"
}
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

In future, `xmpjson` will ship with a related utility called `l4vc` which does this job. It assumes that your PDFs contain XMP that contain JSON that conforms to the schema defined by the L4 project, which establishes open conventions for computational contracts. `l4vc` stands for "L4 version control" and will flatten multiple JSON-augmented PDFs to a current latest-state JSON.

l4vc is under construction.

## Future Work

- [ ] Add support for `yaml`
- [ ] Add support for piping between reading and writing operations
- [ ] Add support for reading/writing metadata from `docx` files
