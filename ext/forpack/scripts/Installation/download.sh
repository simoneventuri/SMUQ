#!/bin/bash
set -e

CodeName="forpack"
TopDirectory="/home/${USER}/Codes/${CodeName^^}/"
RemoteUser="${USER}"
RemoteDirectory="/home/git/${CodeName}.git"
RemoteURL="128.174.133.164"
GIT_URL="${RemoteUser}@${RemoteURL}:${RemoteDirectory}"

mkdir -p ${TopDirectory}
cd ${TopDirectory}
git clone ${GIT_URL}

