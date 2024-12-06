#!/bin/bash

# Following instructions from: https://github.com/delph-in/docs/wiki/AceInstall
# Choose ready-to-run ACE binary download from https://sweaglesw.org/linguistics/ace/
TAR_URL="https://sweaglesw.org/linguistics/ace/download/ace-0.9.34-x86-64.tar.gz"
sudo apt install curl
curl -L $TAR_URL -o /tmp/file.tar.gz
mkdir -p /tmp/extracted
tar -xzvf /tmp/file.tar.gz -C /tmp/extracted
chmod +x /tmp/extracted/ace-0.9.34/ace
# destination path depends on where your bin is
sudo mv /tmp/extracted/ace-0.9.34/ace /usr/local/bin/ace

exit_status=$?

if [ $exit_status -ne 0 ]; then
  echo "Error occurred: Command failed with exit status $exit_status"
  exit 1
else
  echo "Successfully installed ace"
fi