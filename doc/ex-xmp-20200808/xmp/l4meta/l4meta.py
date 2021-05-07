import sys

from exif import MetaTool, ExifToolError
from terminal import arguments


def process():
    try:
        parser = arguments()
        args = parser.parse_args()
        # print(check_mode(args))
        check_if_valid(parser, args)
        print(operate(args))
    except ExifToolError as e:
        print(e)
        sys.exit(1)


def check_mode(args):
    if not args.read and args.write:
        return 'batch write'
    if args.read and args.write and args.meta:
        return 'write'
    if len(args.read) > 1:
        return 'batch read'
    return 'read'


def check_if_valid(parser, args):
    """
    """
    if len(sys.argv) == 1:
        parser.error('You must specify a file to read/write.')
    if ((args.meta and not args.write) or
            (args.write and len(args.write) == 1 and not args.meta)):
        parser.error(
            'Both --meta and --write flag '
            'must be specified at the same time.')
    # if args.write and args.type:
    #     parser.error(
    #         '--type/-j/--json/-y/--yaml '
    #         'flag can only be used for reading.')
    if not args.read and args.write:
        return None
    if not args.read:
        parser.error('No files to read.')


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
