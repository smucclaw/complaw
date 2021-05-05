import sys

from exif import MetaTool, ExifToolError
from terminal import arguments


def process():
    try:
        parser = arguments()
        args = parser.parse_args()
        check_if_valid(parser, args)
        print(operate(args))
    except ExifToolError as e:
        print(e)
        sys.exit(1)


def check_if_valid(parser, args):
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


def operate(args):
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


def main():
    process()


if __name__ == '__main__':
    main()
