import argparse
import sys
import os
import shlex
import exif

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

    # command = ['exiftool', '-G', '-j', '-n', '-xmp:all', filename]
    # command = 'exiftool -G -j -n -xmp:all'
    # command_sp = shlex.split(command)
    # proc = subprocess.Popen(command_sp)

    # parser.set_defaults(func = xmp.read_xmp)

    return parser

def write(subparser):
    parser = subparser.add_parser(
            'write',
            help = 'Write XMP to PDF from JSON'
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
            type = argparse.FileType('r', encoding = 'UTF-8'),
            nargs = 1
    )
    parser.add_argument(
            '-m','--meta',
            help = 'location of metadata',
            type = argparse.FileType('r', encoding = 'UTF-8'),
            nargs = 1
    )

    # parser.set_defaults(func = xmp.write_xmp)

    return parser

def main():
    args = arguments().parse_args()
    print(vars(args))
    # print(args.file[0].name)
    # args.func(args)

if __name__ == '__main__':
    main()
