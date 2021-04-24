import exif
import terminal

def process():
    parser = terminal.arguments()
    args = parser.parse_args()
    
    with exif.MetaTool() as e:
        if args.is_read:
            return e.read(args.file, args.type)
        else:
            return e.write_single(
                    in_file = args.input,
                    out_file = args.output,
                    meta_file = args.meta
            )

def main():
    print(process())

if __name__ == '__main__':
    main()
