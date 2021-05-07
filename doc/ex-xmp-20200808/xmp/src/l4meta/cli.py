import sys

from exif import MetaTool, ExifToolError
from argparse import ArgumentParser, FileType, SUPPRESS


def arguments() -> ArgumentParser:
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
    return parser


def validate(parser, args):
    """Validate the arguments being passed into the command line interface.
    """
    if len(sys.argv) == 1:
        parser.error('You must specify a file to read/write.')
    multiple_files_read = args.read and len(args.read) > 1
    if multiple_files_read:
        parser.error('Reading multiple files not supported currently.')
    multiple_files_written = args.write and len(args.write) > 1
    if multiple_files_written:
        parser.error('Writing multiple files not supported currently.')
    only_meta_enabled = args.meta and not args.write
    meta_not_enabled = args.write \
        and len(args.read) == 1 \
        and len(args.write) == 1 \
        and not args.meta
    if only_meta_enabled or meta_not_enabled:
        parser.error(
                'Both --meta and --write flag '
                'must be specified at the same time.')


def execute(args):
    """
    """
    metatool = MetaTool()
    if not args.write:
        return metatool.read_multiple_files(args.read, args.type)
    if not args.read and args.write and args.meta:
        raise ExifToolError('Error: Batch mode not supported yet.')
    if not metatool.write_file(
            input_file=args.read[0],
            output_file=args.write[0],
            metadata=args.meta):
        raise ExifToolError()
    return 'Write into ' + args.write[0] + ' successful!'


def run():
    """
    """
    parser = arguments()
    args = parser.parse_args()
    validate(parser, args)
    try:
        print(execute(args))
    except ExifToolError as e:
        print(e)
        sys.exit(1)


def main():
    run()


if __name__ == '__main__':
    main()
