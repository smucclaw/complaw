import os
import exif
import json
import shutil
import yaml

def process(args):
    # Silent or verbose
    if args.verbose:
        print(vars(args))
    elif args.silent:
        pass
    
    with exif.ExifTool() as e:
        is_read = args.mode == 0
        if is_read:
            return read(e, args)
        else:
            return write(e, args)

def read(e, args):
    meta = e.get_metadata(args.file[0].name)
    meta = meta[0]
    prefix = 'XMP:' + args.prefix

    # Print out only those with the specified prefix
    for k in list(meta.keys()):
        if not k.startswith(prefix):
            del meta[k]
    
    if args.json or args.type == 'json':
        print()
        result = json.dumps(meta, indent = 4)
    elif args.yaml or args.type == 'yaml':
        print()
        result = yaml.dump(meta)
    
    return result

def write(e, args):
    # Copy over file
    shutil.copy2(args.input.name, args.output.name)

    # Detect metadata file type
    metafile = args.meta[0].name
    metafile_name, metafile_ext = os.path.splitext(metafile)
    
    # Depending on the type of file, parse
    with open(metafile) as file:
        if metafile_ext == '.json':
            metadata = json.load(file)
        elif metafile_ext == '.yml' or metafile_ext == '.yaml':
            metadata = yaml.load(file, Loader = yaml.FullLoader)

    result = ''

    return result
