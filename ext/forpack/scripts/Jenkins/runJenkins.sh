#!/bin/sh

# This file serves as a post-commit example.
# It should be stored in '.git./hooks/post-commit'

USER="blopez"
API_TOKEN="b6fe5eb5a6d8e322c55b50b568ec8af6"    # entropy
JOB_NAME="ForPack"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
JOB_URL="http://128.174.133.164:8080/job/${JOB_NAME}/job/${BRANCH}/buildWithParameters"

function AddBuildParameter() {
  LIST_BUILD_PARAM="${LIST_BUILD_PARAM}&$1"
}

AddBuildParameter "token=mytoken"
AddBuildParameter "delay=0sec"
AddBuildParameter "git_branch=${BRANCH}"
AddBuildParameter "compiler=intel"
AddBuildParameter "compiler_version=18.0.0"
# AddBuildParameter "build_type=Debug"
# AddBuildParameter "run_tests=false"

curl --user "${USER}:${API_TOKEN}" -X POST "${JOB_URL}?${LIST_BUILD_PARAM}"