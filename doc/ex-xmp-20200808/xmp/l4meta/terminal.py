import argparse
import sys

def arguments():
    '''
    Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    '''
    props = {
        "prog": "l4metadata",
        "description": "Read/Write metadata",
        "allow_abbrev": False
    }
    
    parser = argparse.ArgumentParser(**props)

    subparser = parser.add_subparsers()
    read(subparser)
    write(subparser)

    args = parser.parse_args()

    return parser, args

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
        type = str,
        nargs = 1
    )

    outputs = parser.add_mutually_exclusive_group()
    
    outputs.add_argument(
        '-t', '--type',
        help = 'specify metadata output format',
        choices = ['json', 'yaml'],
        default = 'json'
    )
    outputs.add_argument(
        '-j', '--json',
        help = 'output metadata in JSON, same as --type json',
        action = 'store_const',
        dest = 'type',
        const = 'json'
    )
    outputs.add_argument(
        '-y', '--yaml',
        help = 'output metadata in YAML, same as --type yaml',
        action = 'store_const',
        dest = 'type',
        const = 'yaml'
    )

    # parser.add_argument(
    #     '-p', '--prefix',
    #     help = 'Specify prefix for metadata',
    #     type = str,
    #     nargs = 1,
    #     default = 'L4'
    # )

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
            type = str,
            nargs = 1
    )
    parser.add_argument(
            'output',
            help = 'location of PDF to be written',
            type = str,
            nargs = 1
    )
    parser.add_argument(
            'meta',
            help = 'location of metadata',
            type = argparse.FileType('r', encoding = 'UTF-8'),
            nargs = '?',
            default = sys.stdin
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
        action = 'store_true'
        # action = 'count',
        # nargs = '?'
    )
