import sys

from exif import MetaTool
from terminal import arguments


def process():
    try:
        parser = arguments()
        args = parser.parse_args()
        return operate(args)
    except Exception as e:
        return e


def operate(args):
    metatool = MetaTool()
    if args.is_read:
        return metatool.read_file(args.file[0], args.type)
    if not metatool.write_file(
            input_file=args.input[0],
            output_file=args.output[0],
            metadata=args.meta):
        raise Exception()
    return 'Write into ' + args.output[0] + ' successful!'


def main():
    print(process())


if __name__ == '__main__':
    main()
