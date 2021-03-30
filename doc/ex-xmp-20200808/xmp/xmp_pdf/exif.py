import subprocess
import os
import sys
import shlex
import json

class ExifTool:
    '''
    Reference: https://stackoverflow.com/questions/10075115/call-exiftool-from-a-python-script
    '''
    sentinel = "{ready}\r\n" if os.name == 'nt' else "{ready}\n"

    def __init__(self, executable="/usr/bin/exiftool"):
        self.executable = executable

    def __enter__(self):
        self.process = subprocess.Popen(
            [self.executable, "-stay_open", "True",  "-@", "-"],
            universal_newlines = True,
            stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE
        )
        return self

    def  __exit__(self, exc_type, exc_value, traceback):
        self.process.stdin.write("-stay_open\nFalse\n")
        self.process.stdin.flush()

    def execute(self, *args):
        args = args + ("-execute\n",)
        self.process.stdin.write(str.join("\n", args))
        self.process.stdin.flush()
        output = ""
        fd = self.process.stdout.fileno()
        while not output.endswith(self.sentinel):
            output += os.read(fd, 4096).decode('utf-8')
        return output[:-len(self.sentinel)]

    def get_metadata(self, *filenames):
        return json.loads(self.execute("-G", "-j", "-n", '-xmp:all', *filenames))

    def write_metadata(self, filename):
        return self.execute('-G', '-j', '-n', '-xmp:all', filename)

class MetaTool(ExifTool):
    def __init__(self, executable = "/usr/bin/exiftool", prefix = 'L4'):
        super().__init__(executable)
        self.prefix = prefix

    def read(self, *filenames):
        # Execute and return the output
        output = self.execute('-j', *filenames)

        # Process the output until the component is extracted
        proc = self.serialize(output)
        
        return proc

    def write(self, *filenames, metafile, configfile = 'xmp.config'):
        '''
        Write the metadata into the new file

        The original command is:
            exiftool -config xmp.config -j+=meta.json file.pdf
        '''
        output = self.execute('-config ' + configfile, 'j+=' + metafile, *filenames)
        return output

    def serialize(meta):
        '''
        Convert the stringified metadata into metadata in JSON
        '''

        meta = json.loads(meta)
        meta = meta[0][self.prefix]
        meta = json.loads(meta)

        return meta

    def stringify(meta):
        '''
        Convert the metadata in JSON into stringified metadata
        '''
        
        meta = json.dumps(meta)
        meta = { self.prefix : meta }
        meta = json.dumps(meta)

        return meta
