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
      include_package_data=True,
      install_requires=[],
      license='MIT',
      packages=['pysearch'],
      package_data={'': ['data/*.json']},
      python_requires='>=3.9',
    #   scripts=[
    #       'bin/pysearch',
    #   ],
      tests_require=[
          'nose',
      ])
