import argparse
import sys
import os
import subprocess
import shlex
import exif
import json
import yaml

def arguments():
    '''
    Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    '''
    parser = argparse.ArgumentParser(
            prog = 'l4metadata',
            description = 'Read/Write XMP',
            allow_abbrev = False 
    )

    subparser = parser.add_subparsers()
    read(subparser)
    write(subparser)

    return parser

def read(subparser):
    '''
    Arguments for reading. By default the command outputs a JSON.

    Returns:
        parser (ArgumentParser): parser
    '''
    
    parser = subparser.add_parser(
        'read',
        help = 'Read XMP from PDF into stdout'
    )

    parser.add_argument(
        'mode',
        action = 'store_const',
        const = 0,
        help = argparse.SUPPRESS
    )

    parser.add_argument(
        'file',
        help = 'location of file',
        type = argparse.FileType('r', encoding = 'UTF-8'),
        nargs = 1
    )

    parser.add_argument(
        '-t', '--type',
        help = 'specify metadata output format',
        choices = ['json', 'yaml'],
        default = 'json'
    )
    
    outputs = parser.add_mutually_exclusive_group()
    
    outputs.add_argument(
        '-j', '--json',
        help = 'output metadata in JSON',
        action = 'store_true'
    )
    outputs.add_argument(
        '-y', '--yaml',
        help = 'output metadata in YAML',
        action = 'store_true'
    )

    parser.add_argument(
        '-p', '--prefix',
        help = 'Specify prefix for metadata',
        type = str,
        nargs = 1,
        default = 'L4'
    )

    display = parser.add_mutually_exclusive_group()
    
    display.add_argument(
        '-s', '--silent',
        help = 'do not display terminal output',
        action = 'store_true'
    )
    display.add_argument(
        '-v', '--verbose',
        help = 'display terminal output',
        action = 'store_true'
    )

def write(subparser):
    parser = subparser.add_parser(
            'write',
            help = 'Write XMP to PDF from JSON'
    )
    
    parser.add_argument(
        'mode',
        action = 'store_const',
        const = 1,
        help = argparse.SUPPRESS
    )
    
    parser.add_argument(
            'input',
            help = 'location of PDF to be read',
            type = argparse.FileType('r', encoding = 'UTF-8'),
            nargs = 1
    )
    parser.add_argument(
            'output',
            help = 'location of PDF to be written',
            type = argparse.FileType('w', encoding = 'UTF-8'),
            nargs = 1
    )
    parser.add_argument(
            '-m','--meta',
            help = 'location of metadata',
            type = argparse.FileType('r', encoding = 'UTF-8'),
            nargs = 1
    )

    # Adjust log level
    # https://stackoverflow.com/a/53293042
    display = parser.add_mutually_exclusive_group()
    
    display.add_argument(
        '-s', '--silent',
        help = 'do not display terminal output',
        action = 'store_true'
    )
    display.add_argument(
        '-v', '--verbose',
        help = 'display terminal output',
        action = 'count',
        nargs = '?'
    )

def read_from_exiftool(args):
    inputs = vars(args)
    
    base_command = 'exiftool -G -j -n -xmp:all ' + args.file[0].name
    base_command_split = shlex.split(base_command)

    proc = subprocess.call(base_command_split)

    return proc

def main():
    args = arguments().parse_args()
    argvars = vars(args)
    is_read = argvars['mode'] == 0
    
    # Silent or verbose
    if argvars['verbose'] > 0:
        print(argvars)
    elif argvars['silent']:
        pass
    
    with exif.ExifTool() as e:
        if is_read:
            # Read
            meta = e.get_metadata(argvars['file'][0].name)
            meta = meta[0]
            prefix = 'XMP:' + argvars['prefix']

            # Print out only those with the specified prefix
            for k in list(meta.keys()):
                if not k.startswith(prefix):
                    del meta[k]
            
            if argvars['type'] == 'json' or argvars['json']:
                print(json.dumps(meta, indent = 4))
            elif argvars['type'] == 'yaml' or argvars['yaml']:
                print(yaml.dump(meta))
        else:
            # Write
            
            # For new file, first copy over to the specified directory then
            # write the metadata

            # Detect metadata file type
            metafile = argvars['meta'][0].name
            metafile_name, metafile_ext = os.path.splitext(metafile_path)
            
            if metafile_ext == '.json':
                print('JSON!')
            elif metafile_ext == '.yml' or metafile_ext == '.yaml':
                print('YAML!')

if __name__ == '__main__':
    main()
