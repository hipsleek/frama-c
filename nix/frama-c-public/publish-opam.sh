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
#   - the 'opam-repository' as generated by the script Frama-C/dev/build-release.sh
# Availability of the files is NOT checked when the script starts.
# The script:
#   - clones the 'Frama-C/opam-repository' as 'pub-opam-repository'
#   - creates a remote on the 'opam/opam-repository' named 'opam'
#   - creates a branch based on 'opam/master'
#   - copies the content of 'opam-repository' to 'pub-opam-repository'
#   - commits the changes
#   - pushes the branch

##########################################################################

OCAML_OPAM_GIT="git@github.com:ocaml/opam-repository.git"
PUB_OPAM_GIT="git@github.com:Frama-C/opam-repository.git"
PUB_OPAM_DIR="pub-opam-repository"
SRC_OPAM_DIR="opam-repository"

VERSION="$(cat VERSION)"
VERSION_SAFE="$(cat VERSION | sed 's/~/-/')"
CODENAME="$(cat VERSION_CODENAME)"

BRANCH="frama-c.$VERSION_SAFE"

echo "$FRAMA_CI_BOT_SSH_PRIVATE" | base64 -d > nix/frama-c-public/id_ed25519
chmod 400 nix/frama-c-public/id_ed25519

if ! git clone $PUB_OPAM_GIT $PUB_OPAM_DIR
then
  echo "Failed to clone website directory"
  exit 128
fi

GIT="git -C $PUB_OPAM_DIR"
$GIT remote add opam $OCAML_OPAM_GIT

$GIT config user.name  "Frama-CI Bot"
$GIT config user.email "frama-ci-bot@frama-c.com"

function fetch_opam {
  $GIT fetch opam
  if [ "$?" -ne "0" ]; then
    echo "Can't fetch from ocaml opam repository"
    exit 128
  fi
}

function checkout {
  if git ls-remote --quiet --exit-code $PUB_OPAM_GIT $BRANCH
  then
    $GIT checkout $BRANCH
  else
    $GIT checkout -b $BRANCH opam/master --no-track
  fi
  if [ "$?" -ne "0" ]; then
    echo "Failed to checkout branch '$BRANCH', aborting"
    exit 128
  fi
}

function commit {
  $GIT commit -m "Frama-C: new release ($VERSION-$CODENAME)"
  if [ "$?" -ne "0" ]; then
    echo "Failed to commit on branch"
    exit 128
  fi
}

function push {
  if git ls-remote --quiet --exit-code $PUB_OPAM_GIT $BRANCH
  then
    $GIT push
  else
    $GIT push --set-upstream origin $BRANCH
  fi
  if [ "$?" -ne "0" ]; then
    echo "Failed to push branch"
    exit 128
  fi
}

fetch_opam
checkout

cp -r $SRC_OPAM_DIR/* $PUB_OPAM_DIR
$GIT add -A

commit
push
