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

with open("experiment_inputs/query_artifacts") as f:
    progs = [prog.strip() for prog in f.readlines()]

    artifacts_of_interest = [art for art in artifacts if art['image_tag'] in progs]

    changes = [(art['image_tag'], sum(art['metrics'].values())) for art in artifacts_of_interest]

    for (prog, change) in changes:
        print(f"{prog} : {change}")

