from setuptools import setup
from pysearch import __version__

#long_description = open('README.md').read()

setup(name='pysearch',
      version=__version__,
      description='Python version of xsearch',
      # long_description=long_description,
      url='https://github.com/clarkcb/xsearch.git',
      author='Cary Clark',
      author_email='clarkcb@gmail.com',
      license='MIT',
      packages=['pysearch'],
      python_requires='>=3',
      install_requires=[],
      tests_require=[
          'pytest',
      ])
