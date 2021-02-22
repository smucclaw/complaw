import argparse

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

    parser.add_argument(
            '-r', '--read',
            help = 'Read XMP into a JSON'
    )

    parser.add_argument(
            '-w', '--write',
            help = 'Write JSON into XMP'
    )

    return parser

def main():
    arguments().parse_args()

if __name__ == '__main__':
    main()
