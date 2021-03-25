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
        '''
            exiftool -args fruit-contract-2.docx | grep '^-L4' | perl -ple 's/^-/-XMP-pdfx:/' > fruit-contract-2.args
            exiftool -config xmp.config -@ fruit-contract-2.args fruit-contract-2.pdf
        '''
        return self.execute('-G', '-j', '-n', '-xmp:all', filename)

def main():
    filename = '../../fruit-contract-2.pdf'
    with ExifTool() as e:
       out = e.get_metadata(filename)
       print(json.dumps(out, indent = 4))

if __name__ == '__main__':
    main()
