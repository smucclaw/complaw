import exif
import terminal

def process():
    parser, args = terminal.arguments()
    
    # Silent or verbose
    if args.verbose:
        print(vars(args))
    elif args.silent:
        pass
    
    with exif.MetaTool() as e:
        is_read = args.mode == 0
        if is_read:
            return e.read(args.file[0].name, output_format = args.type)
        else:
            return e.write_single(
                    in_file = args.input[0].name,
                    out_file = args.output[0].name,
                    metafile = args.meta[0].name
            )
