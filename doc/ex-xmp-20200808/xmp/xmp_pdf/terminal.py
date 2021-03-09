import argparse
import sys
import os
import subprocess
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

def read_from_exiftool(args):
    inputs = vars(args)
    
    base_command = 'exiftool -G -j -n -xmp:all ' + args.file[0].name
    base_command_split = shlex.split(base_command)

    proc = subprocess.call(base_command_split)

    return proc

def main():
    args = arguments().parse_args()
    print(read_from_exiftool(args))

if __name__ == '__main__':
    main()
