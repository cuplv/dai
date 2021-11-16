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
#'"classification.exceptions":["NullPointerException"],' + # NPE error type
'"classification.code":"Yes",' +
'"metrics.changes":{"$gt":0,"$lt":500}}' # diff touches at least 1 and at most 500 lines of code
)

artifacts = bugswarmapi.filter_artifacts(api_filter)

print("found artifacts: " + str(len(artifacts)))

if not os.path.isdir("_bugswarm/"):
    print("_bugswarm directory does not exist, attempting to create")
    os.mkdir("_bugswarm")

#import pprint
#pp = pprint.PrettyPrinter(indent=2,width=120)


for a in artifacts:
    print("IMAGE TAG: " + a["image_tag"] + "\n")
    print("PULLING SOURCES...")
    try:
        # underscore prefix prevents dune from searching these repos for dune config files 
        image_dir = "_bugswarm/" + a["image_tag"]
        os.mkdir(image_dir)
        # Pull a shallow (origin/master@HEAD only) copy of the repo to the `pass` directory
        os.system("git clone git@github.com:{}.git {}/pass --depth=1 2> /dev/null".format(a["repo"], image_dir))
        # Copy that to the `fail` directory
        os.system("cp -r {}/pass {}/fail".format(image_dir, image_dir))
        # Fetch a shallow copy of the commit at which the `pass` occurred in the `pass` directory
        os.system("cd {}/pass && git fetch --depth 1 origin {} 2> /dev/null && git checkout {} && cd -".format(image_dir, a["passed_job"]["trigger_sha"], a["passed_job"]["trigger_sha"]))
        # Fetch a shallow copy of the commit at which the `fail` occurred in the `fail` directory
        os.system("cd {}/fail && git fetch --depth 1 origin {} 2> /dev/null && git checkout {} && cd -".format(image_dir, a["failed_job"]["trigger_sha"], a["failed_job"]["trigger_sha"]))

        os.system("touch diagnostic/{}.stdout.log diagnostic/{}.stderr.log".format(a["image_tag"],a["image_tag"]))
        os.system("./run_diagnostic {} > diagnostic/{}.stdout.log 2>diagnostic/{}.stderr.log ".format(a["image_tag"],a["image_tag"],a["image_tag"]))
    except FileExistsError:
        print("SKIPPING IMAGE: sources already pulled")
