from setuptools import setup, find_packages
import pathlib

here = pathlib.Path(__file__).parent.resolve()

setup(
    name = 'l4meta',
    version = '0.1.0',
    description = 'Read/Write metadata',
    packages = find_packages(),
    install_requires = [
    ],
    entry_points = {
        'console_scripts': []
    },
    author = 'Centre for Computational Law, SMU',
    license = ''
)
