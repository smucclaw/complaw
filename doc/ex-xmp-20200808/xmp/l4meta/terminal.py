import sys

from argparse import ArgumentParser, FileType, SUPPRESS


def arguments():
    '''
    Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    '''
    props = {
        'prog': 'l4meta',
        'description': 'Read/Write L4 metadata',
        'allow_abbrev': False
    }
    parser = ArgumentParser(**props)

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
        help='Read XMP from PDF into stdout')

    parser.add_argument(
        'is_read',
        action='store_true',
        help=SUPPRESS)

    parser.add_argument(
        'file',
        help='location of file',
        type=str,
        nargs=1)

    outputs = parser.add_mutually_exclusive_group()
    outputs.add_argument(
        '-t', '--type',
        help='specify metadata output format',
        choices=['json', 'yaml'],
        default='json')
    outputs.add_argument(
        '-j', '--json',
        help='output metadata in JSON, same as --type json',
        action='store_const',
        dest='type',
        const='json')
    outputs.add_argument(
        '-y', '--yaml',
        help='output metadata in YAML, same as --type yaml',
        action='store_const',
        dest='type',
        const='yaml')


def write(subparser):
    '''
    '''
    parser = subparser.add_parser(
        'write',
        help='Write XMP to PDF from JSON')

    parser.add_argument(
        'is_read',
        action='store_false',
        help=SUPPRESS)

    parser.add_argument(
        'input',
        help='location of PDF to be read',
        type=str,
        nargs=1)
    parser.add_argument(
        'output',
        help='location of PDF to be written',
        type=str,
        nargs=1)
    parser.add_argument(
        'meta',
        help='location of metadata',
        type=FileType('r', encoding='UTF-8'),
        nargs='?',
        default=sys.stdin)


def single(parser):
    parser.add_argument(
        'file',
        help='location of document',
        type=str,
        nargs=1
    )

    outputs = parser.add_mutually_exclusive_group()
    outputs.add_argument(
        '-t', '--type',
        help='specify metadata output format',
        choices=['json', 'yaml'],
        default='json'
    )
    outputs.add_argument(
        '-j', '--json',
        help='output metadata in JSON, same as --type json',
        action='store_const',
        dest='type',
        const='json'
    )
    outputs.add_argument(
        '-y', '--yaml',
        help='output metadata in YAML, same as --type yaml',
        action='store_const',
        dest='type',
        const='yaml'
    )
    parser.add_argument(
        '--write',
        help='location of document to be written',
        type=str,
        nargs=1
    )
    parser.add_argument(
        'meta',
        help='location of metadata',
        type=FileType('r', encoding='UTF-8'),
        nargs='?',
        default=sys.stdin
    )
