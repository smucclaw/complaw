import sys

from argparse import ArgumentParser, FileType, SUPPRESS


def arguments():
    """Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    """
    props = {
        'prog': 'l4meta',
        'description': 'Read/Write L4 metadata',
        'allow_abbrev': False
    }
    parser = ArgumentParser(**props)
    single(parser)
    return parser


def single(parser):
    parser.add_argument(
        'read',
        help='location of document',
        type=str,
        metavar='file',
        nargs='*'
    )

    outputs = parser.add_mutually_exclusive_group()
    outputs.add_argument(
        '--type',
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
        '-w', '--write',
        help='location of document to be written',
        type=str,
        nargs='*',
        metavar='file'
    )
    parser.add_argument(
        '-m', '--meta',
        help='location of metadata',
        type=FileType('r', encoding='UTF-8'),
        metavar='file',
        nargs='?',
        const=sys.stdin
    )
