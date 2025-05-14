# Customization webpage

To run the customization webpage locally, follow these steps:

In `matrix.py` and `matrix.cgi`, update the path on the first line to point
to your local python installation path.

> **NOTE:** For running on Macs locally, the path is likely `#!/usr/bin/env python3`.

To generate a grammar with a given choices file, create a subdirectory (of length 4)
under sessions/ called '0000' ('0001', etc), `export HTTP_COOKIE=session=0000`,
and run `python3 matrix.cgi`.

To upload the given choices file to the customization webpage, run `python3 myserver.py`
and access [matrix.cgi](127.0.0.1:9000/matrix.cgi) in the browser. Upload choices
file on the main page.

## > **NOTE:** Uploading choices via session cookie needs more work.