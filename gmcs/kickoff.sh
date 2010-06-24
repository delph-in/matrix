#!/bin/bash

python profiles.py -u harvester _noncoord

python profiles.py -g u_profile_noncoord _022 &
python profiles.py -g u_profile_noncoord _023 &
python profiles.py -g u_profile_noncoord _024 &
python profiles.py -g u_profile_noncoord _025 &

# Copy choices_file_022 to choices_file_022c, etc.

# python profiles.py -u coord_profile_all _coord
# python profiles.py -g u_profile_noncoord _022c &
# python profiles.py -g u_profile_noncoord _023c &
# python profiles.py -g u_profile_noncoord _024c &
# python profiles.py -g u_profile_noncoord _025c &
