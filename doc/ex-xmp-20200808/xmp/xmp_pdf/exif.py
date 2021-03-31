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

    def __init__(self, executable = '/usr/bin/exiftool', commands = ["-stay_open", "True",  "-@", "-"]):
        self.executable = executable
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
    def __init__(self, commands = ['-config', 'xmp.config', "-stay_open", "True",  "-@", "-"], prefix = 'L4'):
        self.prefix = prefix
        super().__init__(commands = commands)

    def read(self, *filenames):
        # Execute and return the output
        output = self.execute('-j', *filenames)

        # Process the output until the component is extracted
        proc = self.serialize(output)
        
        return proc

    def write(self, *filenames, metafile):
        '''
        Write the metadata into the new file

        The original command is:
            exiftool -config xmp.config -j+=meta.json file.pdf

        Note: Does not work, see https://exiftool.org/forum/index.php?topic=9433.0
        '''

        output = self.execute('-j+=' + metafile, *filenames)
        return output

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
