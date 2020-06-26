
# Grammar Matrix Regression Tests

This directory contains the data used for running the Grammar Matrix's
regression tests. It is organized as follows:

```
tests/regression/
├── choices                # Choices files for customizing grammars
├── grammars               # Customized grammars (temporary)
├── home
│   ├── current            # Parse results for the current run
│   └── gold               # Gold parse results
├── logs                   # Testing logs
├── regression-test-index  # Listing of available tests
├── skeletons              # The data sources for parsing
└── txt-suites             # Original sentence files used to create skeletons
```

To run the regression tests, use the `rtest.py` script from the parent
directory.
