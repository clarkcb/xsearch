from setuptools import setup

#long_description = open('README.md').read()

setup(name='pysearch',
      version='0.1.0',
      description='Python version of xsearch',
      long_description=long_description,
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
