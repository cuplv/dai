#!/usr/bin/env python3
import sys
import os

# This script queries BugSwarm for suitable pass/fail program pairs and pulls their source, into directory structure of the form:
# ./_bugswarm/<image_tag>/{pass, fail}/<srcs>
# where <image_tag> is a unique identifier of each pass/fail program pair in the BugSwarm DB

if len(sys.argv) < 2:
    print("ERROR: No API token passed via command-line argument.")
    sys.exit(1)

from bugswarm.common.rest_api.database_api import DatabaseAPI

bugswarmapi = DatabaseAPI(token=sys.argv[1])

api_filter = (
'{"lang":{"$in":["Java"]},' + # JAVA source language
'"stability":"5/5",' + # non-flaky
'"classification.exceptions":["NullPointerException"],' + # NPE error type
'"metrics.changes":{"$lt":1000}}' # diff touches at most 1000 lines of code
)

artifacts = bugswarmapi.filter_artifacts(api_filter)

print("found artifacts: " + str(len(artifacts)))

if not os.path.isdir("_bugswarm/"):
    print("_bugswarm directory does not exist, attempting to create")
    os.mkdir("_bugswarm")

for a in artifacts:
    print("IMAGE TAG: " + a["image_tag"])
    print("fetching+checkouting trigger_sha's ...")
    image_dir = "_bugswarm/" + a["image_tag"]
    os.system("cd {}/pass && git fetch --depth 1 origin {} 2> /dev/null && git checkout {} && cd -".format(image_dir, a["passed_job"]["trigger_sha"], a["passed_job"]["trigger_sha"]))
    os.system("cd {}/fail && git fetch --depth 1 origin {} 2> /dev/null && git checkout {} && cd -".format(image_dir, a["failed_job"]["trigger_sha"], a["failed_job"]["trigger_sha"]))

