#!/bin/bash
set -e

CommitMessage="Remove hard-coded code name in Jenkinsfile"
CodeName="forpack"
TopDirectory="/home/${USER}/Codes/${CodeName^^}/${CodeName}"

cd ${TopDirectory}

git add .
# git commit -m "${CommitMessage}"

Command="git commit -m \"${CommitMessage}\""
echo ${Command}
eval ${Command}

git push