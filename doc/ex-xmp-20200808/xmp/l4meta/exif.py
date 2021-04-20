import subprocess
import os
import sys
import shlex
import shutil
import tempfile
import json
import yaml

class ExifTool:
    '''
    Reference: https://stackoverflow.com/questions/10075115/call-exiftool-from-a-python-script
    '''
    sentinel = "{ready}\r\n" if os.name == 'nt' else "{ready}\n"
    BIN_EXIF = 'exiftool'

    def __init__(self, executable = BIN_EXIF, commands = ["-stay_open", "True",  "-@", "-"]):
        self.executable = self.check_bin_exif_present(executable)
        self.commands = commands 

    def __enter__(self):
        self.process = subprocess.Popen(
            [self.executable] + self.commands,
            universal_newlines = True,
            stdin = subprocess.PIPE, 
            stdout = subprocess.PIPE
        )
        return self

    def  __exit__(self, exc_type, exc_value, traceback):
        self.process.stdin.write("-stay_open\nFalse\n")
        self.process.stdin.flush()

    def check_bin_exif_present(self, executable):
        bin_path = shutil.which(executable)

        if not bin_path:
            message = \
                "Error: exiftool not installed! \n \
                To install exiftool, run \n \
                sudo apt-get install exiftool"
            raise Exception(message)

        return bin_path

    def execute(self, *args):
        args = args + ("-execute\n",)
        self.process.stdin.write(str.join("\n", args))
        self.process.stdin.flush()
        output = ''
        fd = self.process.stdout.fileno()
        while not output.endswith(self.sentinel):
            output += os.read(fd, 4096).decode('utf-8')
        return output[:-len(self.sentinel)]

class MetaTool(ExifTool):
    def __init__(self, config = 'config/xmp.config', prefix = 'L4'):
        self.prefix = prefix
        commands = ['-config', config, "-stay_open", "True",  "-@", "-"]
        super().__init__(commands = commands)

    def extract_single_metadata(self, output, output_format):
        '''
        Extract metadata from one file and return the output
        '''

        # Check that output format is either json or yaml
        if output_format not in ['json', 'yaml']:
            raise TypeError('Output format should be in \"json\" or \"yaml\"!')

        # Extract component
        meta = self.serialize(output)

        # Produce the output
        if output_format == 'json':
            result = json.dumps(meta, indent = 4)
        elif output_format == 'yaml':
            result = yaml.dump(meta)
        
        return result

    def read_metadata(self, filename):
        metadata = {}

        # Get extension
        name, ext = os.path.splitext(filename)

        # Get metadata
        with open(filename) as file:
            if ext == '.json':
                metadata = json.load(file)
            elif ext == '.yml' or ext == '.yaml':
                metadata = yaml.load_all(file, Loader = yaml.FullLoader)

        return metadata

    def read_metadata_file(self, content):
        with content as f:
            return content.read()

    def parse_metadata(self, unclean_metadata):
        '''
        Parse the input string

        Args:
            unclean_metadata
        Returns:
            A dict of the metadata which has been parsed
        '''
        metadata = {}

        unclean_metadata = unclean_metadata.strip()

        if unclean_metadata[0] in ['{', '[']:
            # JSON
            metadata = json.loads(unclean_metadata)
        else:
            # YAML
            metadata = yaml.safe_load(unclean_metadata)

        return metadata

    def read(self, filenames, output_format):
        '''
        '''
        filenames = [os.path.abspath(f) for f in filenames]
        output = self.execute('-j', *filenames)

        result = self.extract_single_metadata(output, output_format)

        return result

    def write(self, *filenames, metafile):
        '''
        Write the metadata into the new file

        The original command is:
            exiftool -config xmp.config -j+=meta.json file.pdf

        Note: Does not work, see https://exiftool.org/forum/index.php?topic=9433.0
        '''

        output = self.execute('-j+=' + metafile, *filenames)
        return output

    def write_single(self, in_file, out_file, meta_file):
        '''
        Process to write metadata into a single file

        Args:
            in_file: The input file
            out_file: The output file
            meta_file: Metadata to be written to the output file
        Returns:
            A string
        '''

        result = ''

        # Copy over file
        output_file = self.duplicate_files(in_file, out_file)

        # Process metafile
        metadata = self.read_metadata_file(meta_file)
        parsed_metadata = self.parse_metadata(metadata)
        meta_flat = self.stringify(parsed_metadata)

        with tempfile.NamedTemporaryFile(mode = 'w+', suffix = '.json') as t:
            t.write(meta_flat + "\n")
            t.seek(0)
            result = self.write(output_file, metafile = t.name)

        return result

    def duplicate_files(self, in_file, out_file) -> str:
        '''
        Duplicate a single file based on the location of
        the input and output file. After duplication, it
        returns the location of the output file.

        Args:
            in_file: location of the input file
            out_file: location of the output file
        Returns:
            The absolute path of the output file
        '''

        # Temporary fix to get the first file
        # since the inputs to the arguments are
        # a list of str
        op_file = lambda f: os.path.abspath(f[0]) if f else None
        in_file = op_file(in_file)
        out_file = op_file(out_file)

        if out_file is None:
            name, ext = os.path.splitext(in_file)
            # TODO: Suggest a better naming convention
            # for a duplicate file
            out_file = name + '_v1' + ext

        shutil.copy2(in_file, out_file)

        return out_file

    def serialize(self, meta : str):
        '''
        Convert the stringified metadata into metadata in JSON

        Args:
            meta: The stringified metadata
        Returns:
            A dict of metadata
        '''

        try:
            meta = json.loads(meta)
            meta = meta[0][self.prefix]
            meta = json.loads(meta)
        except Exception as e:
            meta = {}

        return meta

    def stringify(self, meta):
        '''
        Convert the metadata in JSON into stringified metadata

        Args:
            meta: The metadata in dict
        '''
        
        meta = json.dumps(meta)
        meta = { self.prefix : meta }
        meta = json.dumps(meta)

        return meta
