#!/usr/bin/env python3

# Usage:
# python3 build-callgraph.py <class1.java> <class2> ... <classN>
# It is directory-independent since we translate paths to absolute paths.
# Depends on:
# - WALA-callgraph submodule
# - gradle
# - javac


import logging
import subprocess
import sys
from os import path
from typing import List

classes: List[str] = sys.argv[1:]
assert(len(classes) > 0)

subprocess.run(["javac"] + classes)

classes = [path.abspath(c) for c in classes]

subprocess.run("./gradlew compileJava", shell=True, cwd="WALA-callgraph")

def java_to_ext(java: str, ext: str) -> str:
	return path.splitext(java)[0] + "." + ext

callgraph = java_to_ext(classes[0], "callgraph")
jars = [java_to_ext(c, "jar") for c in classes]

logging.info("Building callgraph for %s", jars)
subprocess.run(["./run.py", callgraph] + jars, cwd="WALA-callgraph")
