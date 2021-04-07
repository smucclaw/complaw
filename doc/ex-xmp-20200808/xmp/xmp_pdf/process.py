import os
import exif
import json
import shutil
import yaml
import terminal

def process():
    parser, args = terminal.arguments()
    
    # Silent or verbose
    if args.verbose:
        print(vars(args))
    elif args.silent:
        pass
    
    with exif.MetaTool() as e:
        is_read = args.mode == 0
        if is_read:
            return read(e, args)
        else:
            return write(e, args)

def read(e, args):
    meta = e.read(args.file[0].name)

    if args.type == 'json':
        result = json.dumps(meta, indent = 4)
    elif args.type == 'yaml':
        result = yaml.dump(meta)

    return result

def write(e, args):
    result = ''

    # Copy over file
    shutil.copy2(args.input[0].name, args.output[0].name)

    # Process metafile
    metafile = args.meta[0].name

    with open(metafile) as file:
        # Detect metadata file type
        metafile_ext = detect_file_type(metafile)

        if metafile_ext == '.json':
            new_file = 'new2.json'

            metadata = json.load(file)
            meta_flat = e.stringify(metadata)
            convert_to_flat(meta_flat, new_file)
            
            result = e.write(args.output[0].name, metafile = new_file)
        elif metafile_ext == '.yml' or metafile_ext == '.yaml':
            metadata = yaml.load(file, Loader = yaml.FullLoader)

    return result

def detect_file_type(filename):
    name, ext = os.path.splitext(filename)
    return ext

def convert_to_flat(meta, new_file):
    with open(new_file, 'w+') as file:
        file.write(meta + "\n")
