# Command Line Options

## (WRONG) Nothing

```console
$ l4meta
```

- Error (Parser): No file read/write

## (CORRECT) Read: Single File

```console
$ l4meta file.pdf
```

- Possible Error (Backend): File not found

## (CORRECT) Read: Multiple File

```console
$ l4meta *.pdf
```

- Possible Error (Backend): File not found 

## (CORRECT) Write: Single File

```console
$ l4meta file.pdf --write filev1.pdf --meta meta.json
```

- Possible Error (Backend): File not found (`read`)
- Possible Error (Backend): File not found (`--write`)
- Possible Error (Parser Auto): File not found (`--meta`)

## (WRONG) Write: `read` w/o Input, `--write` Flag w/o Input, w/o `--meta` Flag

```console
$ l4meta --write
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (WRONG) Write: `read` w/o Input, `--write` Flag w/ Single Input, w/o `--meta` Flag

```console
$ l4meta --write filev1.pdf
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (CORRECT) Write: `read` w/o Input, `--write` Flag w/ Multiple Input, w/o `--meta` Flag

```console
$ l4meta --write *.json
```

- Possible Error (Backend): File not found
- Possible Error (Backend): Not an approved type

## (WRONG) Write: `read` w/ Single or Multiple Input, `--write` Flag w/o Input, w/o `--meta` Flag

```console
$ l4meta file.pdf --write
```

```console
$ l4meta *.pdf --write
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (WRONG) Write: `read` w/ Single or Multiple Input, `--write` Flag w/ Single Input, w/o `--meta` Flag

```console
$ l4meta file.pdf --write filev1.pdf
```

```console
$ l4meta *.pdf --write filev1.pdf
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (WRONG) Write: `read` w/ Single or Multiple Input, `--write` Flag w/ Multiple Input, w/o `--meta` Flag

```console
$ l4meta file.pdf --write *.json
```

```console
$ l4meta *.pdf --write *.json
```

## (WRONG) Write: w/o `--write` Flag, `--meta` Flag w/o Input

```console
$ l4meta file.pdf --meta
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (WRONG) Write: w/o `--write` Flag, `--meta` Flag w/ Single Input

```console
$ l4meta file.pdf --meta meta.json
```

- Error (Parser): Both `--write` and `--meta` flag must be present

## (WRONG) Write: w/o `--write` Flag, `--meta` Flag w/ Multiple Input

```console
$ l4meta file.pdf --meta *.json
```

## (WRONG) Write: `read` w/ Single Input, `--write` w/ Multiple Input, `--meta`

- Error (Parser): Both `--write` and `--meta` flag must be present

```console
$ l4meta file.pdf --write *.pdf --meta
```

If `--write` flag expand to a list of size gt 1,
Error (Parser): Cannot define `--meta` flag for multiple files

If `--write` flag expand to a list of size == 1,
Error (Backend): File not found
