#! /usr/bin/env bash
##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2024                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# Note:
#
# While this script can be run locally, it is meant to run in the Frama-C CI.
# Thus, it expects to be run from the root of the Frama-C directory and that
# some CI artifacts are available. Namely:
#   - 'release-data.json' as generated by the script Frama-C/dev/build-release.sh
#   - the 'FRAMA_CI_BOT_RELEASE_TOKEN' variable
#   - the stable branch must be available on 'pub/frama-c'
# Availability of the file is NOT checked when the script starts.

##########################################################################

TAG="$(git describe --tag)"

echo "$FRAMA_CI_BOT_SSH_PRIVATE" | base64 -d > nix/frama-c-public/id_ed25519
chmod 400 nix/frama-c-public/id_ed25519

if [[ $TAG =~ .*-beta$ ]] ; then
  git push "git@git.frama-c.com:pub/frama-c.git" "$TAG"
else
  curl \
    --header 'Content-Type: application/json' \
    --header "PRIVATE-TOKEN:$FRAMA_CI_BOT_RELEASE_TOKEN" \
    --data-binary "@release-data.json" \
    --request POST \
    "https://git.frama-c.com/api/v4/projects/780/releases"
fi