#!/bin/sh

set -e

lein pprint --no-pretty -- :version > dev/resources/latest-version-tag
git rev-parse --short HEAD > dev/resources/latest-version-short-sha
