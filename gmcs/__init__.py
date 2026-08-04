import sys
if sys.version_info < (3,10):
    raise RuntimeError("Please upgrade to python>=3.10 due to dependency on PyDelphin, which requires python>=3.10.")
if sys.version_info >= (3,13):
    raise RuntimeError("Please downgrade to python<3.13 due to dependency on cgitb, which is obsolete as of python 3.13.")
