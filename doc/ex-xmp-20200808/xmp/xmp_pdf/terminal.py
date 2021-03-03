import argparse
import sys
# import xmp
import exif

def arguments():
    '''
    Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    '''
    parser = argparse.ArgumentParser(
            prog = 'metareader',
            description = 'Read/Write XMP',
            allow_abbrev = False 
    )

    # subparser = parser.add_subparsers()
    # read(subparser)
    # write(subparser)

    single_mode(parser)

    return parser

def single_mode(parser):
    '''
    Arguments for reading.

    Returns:
        parser (ArgumentParser): parser
    '''

    parser.add_argument(
        'file',
        help = 'location of document',
        type = argparse.FileType('r', encoding = 'UTF-8'),
        nargs = 1,
        # default = sys.stdin
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
    
    return parser

def read(subparser):
    '''
    Arguments for reading.

    By default the command outputs a JSON.

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
            type = str,
            nargs = 1
    )
    # parser.add_argument(
    #         '-j', '--json',
    #         help = 'export into a JSON file',
    #         type = str,
    #         nargs = 1
    # )
    parser.set_defaults(func = xmp.read_xmp)

    return parser

def write(subparser):
    parser = subparser.add_parser(
            'write',
            help = 'Write XMP to PDF from JSON'
    )
    
    parser.add_argument(
            'pdf',
            help = 'location of PDF',
            type = str,
            nargs = 1
    )
    parser.add_argument(
            'json',
            help = 'location of JSON',
            type = str,
            nargs = 1
    )

    parser.set_defaults(func = xmp.write_xmp)

    return parser

def main():
    args = arguments().parse_args()
    print(vars(args))
    # args.func(args)

if __name__ == '__main__':
    main()
