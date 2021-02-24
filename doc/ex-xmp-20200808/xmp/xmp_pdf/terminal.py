import argparse
import xmp

def arguments():
    '''
    Prepare the argument list.

    Returns:
        parser (ArgumentParser): ArgumentParser object
    '''
    parser = argparse.ArgumentParser(
            prog = 'xmp',
            description = 'Read/Write XMP',
            allow_abbrev = False 
    )
    subparser = parser.add_subparsers()

    read(subparser)
    write(subparser)

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
    args.func(args)
    print(vars(args))

if __name__ == '__main__':
    main()
