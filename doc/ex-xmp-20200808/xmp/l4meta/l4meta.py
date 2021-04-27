import sys

from exif import MetaTool, ExifToolError
from terminal import arguments


def process():
    try:
        parser = arguments()
        args = parser.parse_args()
        return operate(args)
    except ExifToolError as e:
        print(e)
        sys.exit(1)


def operate(args):
    metatool = MetaTool()
    if args.is_read:
        return metatool.read_multiple_files(args.file, args.type)
    if not metatool.write_file(
            input_file=args.input[0],
            output_file=args.output[0],
            metadata=args.meta):
        raise ExifToolError()
    return 'Write into ' + args.output[0] + ' successful!'


def main():
    print(process())


if __name__ == '__main__':
    main()
