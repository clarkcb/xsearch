from setuptools import setup
from pysearch import VERSION

#long_description = open('README.md').read()

setup(name='pysearch',
      version=VERSION,
      description='Python version of xsearch',
      # long_description=long_description,
      url='https://github.com/clarkcb/xsearch.git',
      author='Cary Clark',
      author_email='clarkcb@gmail.com',
      include_package_data=True,
      install_requires=[],
      license='MIT',
      packages=['pysearch'],
      python_requires='>=3',
      scripts=[
          'bin/pysearch',
          # 'bin/pysearch.bat'
      ],
      tests_require=[
          'nose',
      ])
