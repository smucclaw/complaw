import subprocess
import os
import sys
import shlex
import shutil
import json
import yaml

from contextlib import contextmanager
from subprocess import CompletedProcess
from tempfile import NamedTemporaryFile
from typing import TextIO, List

__version__ = '0.2'
__all__ = [
        'ExifTool',
        'MetaTool',
        'ExifToolError'
]


class ExifTool:
    BIN_EXIF = 'exiftool'

    def __init__(
            self,
            executable: str = BIN_EXIF) -> None:
        self.executable = self.check_bin_present(executable)

    def __repr__(self) -> str:
        type_name = type(self).__name__
        arg_strings = []
        star_args = {}
        for arg in self._get_args():
            arg_strings.append(repr(arg))
        for name, value in self._get_kwargs():
            if name.isidentifier():
                arg_strings.append('%s=%r' % (name, value))
            else:
                star_args[name] = value
        if star_args:
            arg_strings.append('**%s' % repr(star_args))
        return '%s(%s)' % (type_name, ', '.join(arg_strings))

    def _get_kwargs(self) -> list:
        return list(self.__dict__.items())

    def _get_args(self) -> list:
        return []

    def check_bin_present(
            self,
            executable: str) -> str:
        '''
        Checks that the executable is present.
        '''
        bin_path = shutil.which(executable)
        self.exit_on_error(
                not bin_path,
                "Error: exiftool not installed! \n \
                To install exiftool, run \n \
                sudo apt-get install exiftool")
        return bin_path

    def execute(
            self,
            args: List[str]) -> CompletedProcess:
        '''
        Execute the command.
        '''
        return subprocess.run(
            [self.executable] + args,
            universal_newlines=True,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE)

    def exit_on_error(
            self,
            condition: bool = False,
            message: str = '') -> None:
        '''
        Cause an error to be raised when a condition is met.
        '''
        if condition:
            raise ExifToolError('Error: ' + message)

    @contextmanager
    def cd(
            self,
            new_dir: str,
            previous_dir: str = os.getcwd()) -> None:
        '''
        Change directory.
        '''
        os.chdir(os.path.expanduser(new_dir))
        try:
            yield
        finally:
            os.chdir(previous_dir)


class MetaTool(ExifTool):
    PREFIX = 'L4'
    CONFIG_FILE = 'config/xmp.config'
    FORMATS = ['json', 'yaml']
    ALLOWED_FILETYPES = ['pdf']

    def __init__(
            self,
            config: str = CONFIG_FILE,
            flags: str = '-q',
            formats: List[str] = FORMATS) -> None:
        self.config = self.get_absolute_path(config)
        self.formats = formats
        self.flags = flags
        super().__init__()

    def get_absolute_path(
            self,
            location: str,
            check_required: bool = True) -> str:
        '''
        Get the absolute path of the file.
        '''
        absolute_location = os.path.abspath(location)
        if check_required:
            self.exit_on_error(
                    not os.path.isfile(absolute_location),
                    'File not found - ' + location)
        return absolute_location

    def check_approved_filetype(
            self,
            location: str,
            allowed_filetypes: List[str] = ALLOWED_FILETYPES) -> bool:
        '''
        Check that the file is among the approved filetypes.
        '''
        name, ext = os.path.splitext(location)
        self.exit_on_error(
                ext[1:] not in allowed_filetypes,
                'Not an approved filetype - ' + ext[1:])
        return True

    def execute(
            self,
            arguments: str) -> CompletedProcess:
        '''
        Execute the command.
        '''
        arg_config = '-config ' + self.config
        arguments = arg_config \
            + ' ' + self.flags \
            + ' ' + arguments
        args = shlex.split(arguments, posix=0)
        return super().execute(args)

    def read_file(
            self,
            filename: str,
            output_format: str = 'json') -> str:
        '''
        Read metadata from a single file.
        '''
        filename = self.get_absolute_path(filename)
        self.check_approved_filetype(filename)

        command = '-j ' + filename
        process = self.execute(command)
        self.exit_on_error(
                process.returncode != 0,
                'Unable to read file!')

        output = process.stdout
        meta = self.convert_str_to_dict(output)
        return self.dump_metadata(meta, output_format)

    def write_file(
            self,
            input_file: str,
            output_file: str,
            metadata: str) -> bool:
        '''
        Write metadata to a single file.
        '''
        self.exit_on_error(
                output_file == '-',
                '\'-\' not supported at this time!')
        input_file = self.get_absolute_path(input_file)
        output_file = self.get_absolute_path(output_file, False)
        self.check_approved_filetype(input_file)
        self.check_approved_filetype(output_file)
        input_file_dir = os.path.dirname(input_file)

        raw_metadata = self.read_metadata_file(metadata)
        parsed_metadata = self.parse_metadata(raw_metadata)
        flat_metadata = self.convert_dict_to_str(parsed_metadata)

        with self.cd(input_file_dir):
            process = self.write_metadata(
                    flat_metadata,
                    input_file, output_file)
            return process.returncode == 0

    def write_metadata(
            self,
            metadata: str,
            input_file: str,
            output: str = '-',
            file_type: str = '.json') -> CompletedProcess:
        '''
        Write metadata for a single input file
        to a single output.
        '''
        with NamedTemporaryFile(mode='w+', suffix=file_type) as t:
            t.write(metadata + "\n")
            t.seek(0)
            command = '-j+=' + t.name + ' -o ' + output + ' ' + input_file
            return self.execute(command)

    def read_metadata_file(
            self,
            content: TextIO) -> str:
        '''
        Read the metadata file.
        '''
        with content as f:
            return content.read()

    def dump_metadata(
            self,
            meta: dict,
            output_format: str = 'json',
            indent: int = 4) -> str:
        '''
        Convert the metadata into a string depending on the specified
        output format. The currently accepted formats are 'json' and
        'yaml'.
        '''
        self.exit_on_error(
                output_format not in self.formats,
                'Output format should be in any of: json, yaml!')
        if output_format == 'yaml':
            return yaml.dump(meta)
        return json.dumps(meta, indent=indent)

    def parse_metadata(
            self,
            metadata: str,
            is_json=lambda s: s[0] in ['{', '[']) -> dict:
        '''
        Parse the input string

        Args:
            metadata
            is_json
        Returns:
            A dict of the metadata which has been parsed
        '''
        metadata = metadata.strip()
        if is_json(metadata):
            return json.loads(metadata)
        return yaml.safe_load(metadata)

    def convert_str_to_dict(
            self,
            meta: str) -> dict:
        '''
        Convert the stringified metadata into metadata in JSON

        Args:
            meta: The stringified metadata
        Returns:
            A dict of metadata
        '''
        try:
            meta = json.loads(meta)
            meta = meta[0][self.PREFIX]
            return json.loads(meta)
        except Exception as e:
            return {}

    def convert_dict_to_str(
            self,
            meta: dict) -> str:
        '''
        Convert the metadata in JSON into stringified metadata

        Args:
            meta: The metadata in dict
        '''
        try:
            meta = json.dumps(meta)
            meta = {self.PREFIX: meta}
            return json.dumps(meta)
        except Exception as e:
            return {}


class ExifToolError(Exception):
    pass
